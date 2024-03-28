(* a.ml *)

(* usage: 

```sh
rlwrap -a ocaml -noinit 
```

```ocaml
#use "topfind"
#require "easy_logging"
#require "domainslib"

#use "src/a.ml"
```

*)

open Easy_logging
let logger = Logging.make_logger "file_logger" Debug [File ("a", Debug)]

module C = Domainslib.Chan

type 'a worker_message = Candidate of 'a | Finished of string
type 'b tally_message = Threshold of 'b 

type ('a, 'b) channels = {snd : 'a C.t; rcv : 'b C.t}

let worker_fun ch =
    fun () ->
        begin
            let id = string_of_int (Domain.self () :> int) in 
            logger#debug "worker: %s" id;
            match C.recv ch.rcv with 
            | Threshold t -> logger#debug "worker_fun: %s received threshold %f" id t;
            C.send ch.snd (Candidate (-1.0));
            C.send ch.snd (Finished id);
            ()
        end


let tally () =
    begin
        let n = 1 in
        let a = C.make_unbounded () and
            bs = Array.init n (fun _ -> C.make_unbounded ()) in
        let workers = Array.map (fun b -> Domain.spawn(worker_fun {snd = a; rcv = b;})) bs in
        Array.iter (fun b -> C.send b (Threshold 1.0)) bs;
        let finished = ref [] in
        while List.length !finished != n do
            match C.recv a with 
            | Candidate t -> logger#debug "tally: received candidate %f" t;
            | Finished id -> 
                    logger#debug "tally: received finished %s" id;
                    finished := id :: !finished
        done;
        Array.iter (fun d -> Domain.join d) workers
    end

let main () = 
    begin
        tally ();
        logger#debug "ok\n";
        Printf.printf "ok\n"
    end

let _ = main ()

(* end *)
