(* d.ml *)

(*
#use "topfind"
#require "easy_logging"
#require "domainslib"
*)

open Easy_logging
let logger = Logging.make_logger "file_logger" Debug [File ("d", Debug)]

module C = Domainslib.Chan

type 'a worker_message = Candidate of 'a | Finished of int
type 'b tally_message = Threshold of 'b 

type ('a, 'b) channels = {snd : 'a C.t; rcv : 'b C.t}

type interval = {lower: int; upper: int} 

let get_threshold c =
    let t = match C.recv c with | Threshold t -> t in
    let th = ref t in
    let rec upd () =
        match C.recv_poll c with 
        | Some (Threshold t) -> th := t; upd ()
        | None -> () in
    upd ();
    !th

let worker ch x bounds =
    fun () ->
        let rec aux i theta =
            if i >= bounds.upper then 
                C.send ch.snd (Finished (Domain.self () :> int)) |> ignore
            else begin
                let j = ref i in
                while (!j < bounds.upper - 1) && (x.(!j) >= theta) do j := !j + 1 done;
                if x.(!j) < theta then
                    let _ = C.send ch.snd (Candidate x.(!j)) in
                    aux (!j + 1) (get_threshold ch.rcv)
                else
                    aux (!j + 1) theta
            end in 
        aux bounds.lower (get_threshold ch.rcv)

let last u = u.(Array.length u - 1)

(* assume v < last u *)
let insert u v =
    let i = ref 0 in
    while v >= u.(!i) do i := !i + 1 done;
    for j = Array.length u - 1 downto !i + 1 do u.(j) <- u.(j - 1) done;
    u.(!i) <- v

let _tally u x n =
    let rec aux a bs finished =
        match C.recv a with
        | Candidate t ->
                if t < last u then 
                    insert u t;
                Array.iter (fun b -> C.send b (Threshold (last u))) bs;
                aux a bs finished
        | Finished id ->
                logger#debug "tally: finished %d" id;
                if List.length finished < Array.length bs - 1 then
                    aux a bs (id ::finished)
                else
                    () in
    let a = C.make_unbounded () and
        bs = Array.init n (fun _ -> C.make_unbounded ()) and
        p = (Array.length x) / n in
    let workers = Array.init n (fun i -> Domain.spawn(worker {snd = a; rcv = bs.(i)} x {lower = i * p; upper = (i + 1) * p})) in begin
        Array.iter (fun b -> C.send b (Threshold (last u))) bs;
        aux a bs [];
        Array.iter (fun d -> Domain.join d) workers
    end

(*
next steps: rework tally into let k_least x k = ...
and have the aux fun take just finished as its single argument
*)


(* assume (Array.length x) - k is a multiple of w, it makes the arithmetic in the setup easier! *)

let k_least k x w = 
    let a = C.make_unbounded () and
        bs = Array.init w (fun _ -> C.make_unbounded ()) and
        p = ((Array.length x) - k) / w and
        u = Array.init k (fun i -> x.(i)) in
    let _ = Array.sort Float.compare u in
    let rec aux finished =
        match C.recv a with
        | Candidate t ->
                if t < last u then 
                    insert u t
                else
                    logger#debug "worker: candidate %f too big" t;
                Array.iter (fun b -> C.send b (Threshold (last u))) bs;
                aux finished
        | Finished id ->
                logger#debug "tally: finished %d" id;
                if List.length finished < Array.length bs - 1 then
                    aux (id ::finished)
                else
                    () in
    let workers = Array.init w (fun i -> Domain.spawn(worker {snd = a; rcv = bs.(i)} x {lower = k + i * p; upper = k + (i + 1) * p})) in begin
        Array.iter (fun b -> C.send b (Threshold (last u))) bs;
        aux [];
        Array.iter (fun d -> Domain.join d) workers;
        u
    end



let main () = 
    let k = 10 and
        p = 1000 and
        w = 4 in
    let x = Array.init (k + w * p) (fun _ -> Random.float 1.0) in
    let u = k_least k x w in begin
        Array.sort Float.compare x;
        for i = 0 to k - 1 do logger#debug "%f\t%f" u.(i) x.(i) done
    end;
    logger#debug "ok";
    Printf.printf "ok\n"



let _ = main ()


(* end *)
