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

type 'a interval = {upper: 'a; lower: 'a}

let get_threshold c =
    let t = match C.recv c with | Threshold t -> t in
    let th = ref t in
    let rec upd () =
        match C.recv_poll c with 
        | Some (Threshold t) -> th := t; upd ()
        | None -> () in
    upd ();
    !th

let worker_fun ch x bounds =
    fun () ->
        begin
            let id = string_of_int (Domain.self () :> int) in 
            logger#debug "worker: %s" id;
            let th = ref x.(bounds.lower) in
            th := get_threshold ch.rcv;
            logger#debug "worker: %s threshold is %f" id !th;
            let i = ref bounds.lower in
            while !i < bounds.upper do
                if x.(!i) < !th then begin
                    C.send ch.snd (Candidate x.(!i));
                    logger#debug "worker_fun: %s send candiate %f" id x.(!i);
                    th := get_threshold ch.rcv;
                    logger#debug "worker_fun: %s threshold is %f" id !th;
                end;
                i := !i + 1
            done;
            C.send ch.snd (Finished id);
            logger#debug "worker_fun: %s send finished" id
        end

let last u = u.(Array.length u - 1)

(* assume v < last u *)
let insert u v =
    let i = ref 0 in
    while v >= u.(!i) do i := !i + 1 done;
    for j = Array.length u - 1 downto !i + 1 do u.(j) <- u.(j - 1) done;
    u.(!i) <- v

let tally u x n =
    begin
        let a = C.make_unbounded () and
            bs = Array.init n (fun _ -> C.make_unbounded ()) in
        let k = (Array.length x) / n in
        let _ = logger#debug "tally: k is %d" k in
        let workers = 
            Array.init n (fun i -> Domain.spawn(worker_fun {snd = a; rcv = bs.(i)} x {lower = i * k; upper = (i + 1) * k})) in
        Array.iter (fun b -> C.send b (Threshold (last u))) bs;
        logger#debug "tally: send threshold %f" (last u);
        let finished = ref [] in
        while List.length !finished != n do
            match C.recv a with 
            | Candidate t -> begin
                logger#debug "tally: received candidate %f" t;
                if t < last u then begin
                    insert u t;
                    logger#debug "tally: insert %f" t;
                    Array.iter (fun b -> C.send b (Threshold (last u))) bs;
                    logger#debug "tally: send threshold %f" (last u)
                end
                else 
                    logger#debug "tally: too late!! %f is above the threshold" t
            end
            | Finished id -> 
                    logger#debug "tally: received finished %s" id;
                    finished := id :: !finished
        done;
        Array.iter (fun d -> Domain.join d) workers
    end

let main () = 
    begin
        let () = Random.init 103 and
            k = 100 and
            n = 10 in
        let z = Array.init ((n + 1) * k) (fun _ -> Random.float 1.0) in
        let u = Array.init k (fun i -> z.(i)) in
        let _ = Array.sort Float.compare u and
            x = Array.init (n * k) (fun i -> z.(i + k)) in
        (* Array.iter (fun v -> logger#debug "u: %f" v) u;
        Array.iter (fun v -> logger#debug "x: %f" v) x;  *)
        tally u x n;
        Array.iter (fun v -> logger#debug "u: %f" v) u;
        Array.sort Float.compare z;
        Array.iter (fun v -> logger#debug "z: %f" v) z;
        logger#debug "ok\n";
        Printf.printf "ok\n"
    end

let _ = main ()

(* end *)
