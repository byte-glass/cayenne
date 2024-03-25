(* a.ml *)

(* usage: 

```sh
rlwrap -a ocaml -noinit 
```

```ocaml
#use "topfind"
#require "domainslib"
```

*)

open Domainslib

module C = Domainslib.Chan

type 'a message = Candidate of 'a | Finished of int

type 'a channels = {snd : 'a C.t; rcv : 'a C.t}

let tally_fun ch u =
    fun () ->
        let rec tally u =
            match C.recv ch.rcv with
            | Candidate t ->
                    Printf.printf "tally: candidate %f\n" t;
                    tally u
            | Finished id ->
                    Printf.printf "tally: finished %d\n" id;
                    () in
        tally u


(*

let a = C.make_unbounded ()
let b = C.make_unbounded ()
let ch = {snd = a; rcv = b}

Random.init 13
let n = 10
let u = Array.init n (fun _ -> Random.float 1.0)
let () = Array.sort Float.compare u

let tally_d = Domain.spawn(tally_fun ch u)

let tally_candidate = Domain.spawn(fun _ -> C.send ch.rcv (Candidate 0.1))
let tally_finished = Domain.spawn(fun _ -> C.send ch.rcv (Finished 11))

Domain.join tally_d

*)





let () = print_endline "Oh, Carolina!"


(* end *)

