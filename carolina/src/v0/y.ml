(* y.ml -- one worker *)

open Easy_logging
let logger = Logging.make_logger "file_logger" Debug [File ("y", Debug)]

(*
open Domainslib
*)

module C = Domainslib.Chan

type 'a worker_message = Candidate of 'a | Finished of int
type 'b tally_message = Threshold of 'b 

type ('a, 'b) channels = {snd : 'a; rcv : 'b}

type interval = {lower: int; upper: int} 

(* assume u < x.(Array.length x - 1) *)
let insert x u =
    let i = ref 0 in
    while u > x.(!i) do i := !i + 1 done;
    for j = Array.length x - 1 downto !i + 1 do x.(j) <- x.(j - 1) done;
    x.(!i) <- u

let tally_fun ch u =
    fun () ->
        let tlogger = Logging.make_logger "tally-logger" Debug [File ("tally", Debug)] in
        let rec tally f u =
            (* if List.length f = List.length ch.snd then
                let _ = tlogger#info "tally: all done" in
                ()
            else *)
                match C.recv ch.rcv with
                | Candidate t ->
                        tlogger#info "tally: received candidate %f" t;
                        if t < u.(Array.length u - 1) then
                            let _ = insert u t in
                            let th = Threshold u.(Array.length u - 1) in
                            let _ = tlogger#info "tally: th is %f" u.(Array.length u - 1) in
                            List.iter (fun c -> C.send c th) ch.snd;
                        tally f u
                | Finished id ->
                        tlogger#info "tally: finished %d" id;
                        tally ((Finished id) :: f) u in
        tally [] u

let update c theta id =
    let rec upd th =
        match C.recv_poll c with 
        | Some (Threshold t) -> th := t; upd th
        | None -> () in
    let th = ref theta in
    upd th; 
    logger#info "update: %d: %f -> %f" id theta !th;
    !th


let worker_fun ch x bounds theta =
    fun () ->
        let id = string_of_int (Domain.self () :> int) in
        let wlogger = Logging.make_logger ("worker-" ^ id) Debug [File ("w-" ^ id, Debug)] in
        let rec worker i theta = 
            if i >= bounds.upper then
                let _ = C.send ch.snd (Finished (Domain.self () :> int)) in ()
            else
                let j = ref i in
                let _ = while (!j < bounds.upper - 1) && (x.(!j) >= theta) do 
                    wlogger#info "worker %d (%d, %f)" (Domain.self () :> int) !j x.(!j);
                    j := !j + 1 
                done in
                if x.(!j) < theta then
                    let _ = C.send ch.snd (Candidate x.(!j)) in
                    worker (!j + 1) (update ch.rcv theta (Domain.self() :> int))
                else 
                    worker (!j + 1) theta in
        worker bounds.lower theta


let main () =
    let _ = Random.init 103 and
        n = 10 in
    let x = Array.init (2 * n) (fun _ -> Random.float 1.0) in
    let u = Array.init n (fun i -> x.(i)) in
    let () = Array.sort Float.compare u in
    let theta = u.(Array.length u - 1) in
    let a = C.make_unbounded () and
        b = C.make_unbounded () in
    let _ = Array.iter (fun v -> logger#info "u: %f" v) u in
    let _ = Array.iter (fun v -> logger#info "x: %f" v) x in
    let domains = List.map Domain.spawn [(tally_fun {snd = [a]; rcv = b} u); 
            (worker_fun {snd = b; rcv = a} x {lower = n; upper = 2 * n} theta)] in
    List.iter Domain.join domains |> ignore;
    Array.iter (fun v -> logger#info "u: %f" v) u;
    Array.sort Float.compare x;
    Array.iter (fun v -> logger#info "x: %f" v) x;
    logger#info "ok"

(*
    logger#info "Length.list domains is %d" (List.length domains);
    logger#info "theta is %f" theta;
    logger#info "x.(0) is %f" x.(0);
    List.map (fun d -> Domain.spawn(d)) domains |> ignore;
    *)

let _ = main ()
