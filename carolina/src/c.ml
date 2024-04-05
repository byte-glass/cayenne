(* b.ml *)

(*
#use "topfind"
#require "easy_logging"
#require "domainslib"
*)

open Easy_logging
let logger = Logging.make_logger "file_logger" Debug [File ("c", Debug)]

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

let tally u x n =
    let rec aux a bs finished =
        match C.recv a with
        | Candidate t ->
                if t < last u then 
                    insert u t;
                Array.iter (fun b -> C.send b (Threshold (last u))) bs;
                aux a bs finished
        | Finished id ->
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

let main () =
    begin
        let () = Random.init 103 and
            k = 5 and
            p = 10 and 
            n = 2 in
        let z = Array.init (k + n * p) (fun _ -> Random.float 1.0) in
        let u = Array.init k (fun i -> z.(i)) in
        let _ = Array.sort Float.compare u and
            x = Array.init (n * p) (fun i -> z.(k + i)) in 
        begin
            tally u x n;
            Array.sort Float.compare z;
            Array.iter (fun i -> logger#debug "%f\t%f" u.(i) z.(i)) (Array.init k (fun i -> i));
        end;
        logger#debug "ok";
        Printf.printf "ok\n"
    end

let _ = main ()

(* end *)
