(* b.ml *)

(*
#use "topfind"
#require "easy_logging"
#require "domainslib"
*)

open Easy_logging
let logger = Logging.make_logger "file_logger" Debug [File ("b", Debug)]

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

let tally u x =
    let rec aux ch =
        match C.recv ch.rcv with
        | Candidate t ->
                if t < last u then 
                    insert u t;
                C.send ch.snd (Threshold (last u));
                aux ch 
        | Finished _id ->
                () in
    let a = C.make_unbounded () and
        b = C.make_unbounded () in 
    let worker_d = Domain.spawn(worker {snd = a; rcv = b} x {lower = 0; upper = Array.length x}) in begin
        C.send b (Threshold (last u));
        aux {snd = b; rcv = a};
        Domain.join worker_d
    end


let main () =
    begin
        let () = Random.init 103 and
            k = 10 and
            n = 1000 in
        let z = Array.init (n + k) (fun _ -> Random.float 1.0) in
        let u = Array.init k (fun i -> z.(i)) in
        let _ = Array.sort Float.compare u and
            x = Array.init n (fun i -> z.(k + i)) in 
        begin
            tally u x;
            Array.sort Float.compare z;
            Array.iter (fun i -> logger#debug "%f\t%f" u.(i) z.(i)) (Array.init k (fun i -> i));
        end;
        logger#debug "ok";
        Printf.printf "ok\n"
    end

let _ = main ()

(* end *)
