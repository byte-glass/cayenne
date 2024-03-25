(* a.ml *)

(* usage: 

```sh
rlwrap -a ocaml -noinit 
```


```ocaml
#use "src/a.ml"
```

*)

#use "topfind"
#require "domainslib"

open Domainslib

module C = Domainslib.Chan

type 'a message =  Threshold of 'a | Candidate of 'a | Finished of int
(* type 'b threshold = Threshold of 'b  -- can't get this to work!! *)

type 'a channels = {snd : 'a C.t; rcv : 'a C.t}

(* assume u < x.(Array.length x - 1) *)
let insert x u =
    let i = ref 0 in
    while u > x.(!i) do i := !i + 1 done;
    for j = Array.length x - 1 downto !i + 1 do x.(j) <- x.(j - 1) done;
    x.(!i) <- u

let tally_fun ch u =
    fun () ->
        let rec tally u =
            match C.recv ch.rcv with
            | Candidate t ->
                    Printf.printf "tally: candidate %f\n" t;
                    if t < u.(Array.length u - 1) then
                        let _ = insert u t in
                        C.send ch.snd (Threshold u.(Array.length u - 1));
                    tally u
            | Finished id ->
                    Printf.printf "tally: finished %d\n" id;
                    () 
            | Threshold t -> 
                    Printf.printf "tally: threshold!??\n"; 
                    () in
        tally u


let update c theta =
    let rec upd th =
        match C.recv_poll c with 
        | Some (Threshold t) -> th := t; upd th
        | Some (Candidate t) -> upd th
        | Some (Finished n) -> upd th
        | None -> () in
    let th = ref theta in
    upd th; 
    Printf.printf "update: %f -> %f\n" theta !th;
    !th


let worker_fun ch x i theta =
    fun () ->
        let rec worker i theta = 
            if i > Array.length x - 1 then
                let _ = C.send ch.snd (Finished (Domain.self () :> int)) in ()
            else
                let j = ref i in
                let _ = while (!j < Array.length x - 1) && (x.(!j) >= theta) do j := !j + 1 done in
                if x.(!j) < theta then
                    let _ = C.send ch.snd (Candidate x.(!j)) in
                    worker (!j + 1) (update ch.rcv theta)
                else 
                    worker (!j + 1) theta in
        worker i theta

(* test worker_fun and tally_fun

let a = C.make_unbounded ()
let b = C.make_unbounded ()

Random.init 13
let k = 10
let u = Array.init k (fun _ -> Random.float 1.0)
let () = Array.sort Float.compare u
let theta = u.(Array.length u - 1)

Random.init 103
let n = 15
let x = Array.init n (fun _ -> Random.float 1.0)

let tally_d = Domain.spawn(tally_fun {snd = a; rcv = b} u)
let worker_d = Domain.spawn(worker_fun {snd = b; rcv = a} x 0 theta)

*)

(* test worker_fun

let a = C.make_unbounded ()
let b = C.make_unbounded ()

Random.init 103
let n = 15
let x = Array.init n (fun _ -> Random.float 1.0)

let worker_d = Domain.spawn(worker_fun {snd = b; rcv = a} x 0 0.5)
let sink_d = Domain.spawn(sink_fun b)

Domain.join worker_d

*)

(* test tally_fun

let a = C.make_unbounded ()
let b = C.make_unbounded ()
let ch = {snd = a; rcv = b}

Random.init 13
let k = 10
let u = Array.init k (fun _ -> Random.float 1.0)
let () = Array.sort Float.compare u

let tally_d = Domain.spawn(tally_fun {snd = a; rcv = b} u)

Domain.spawn(fun _ -> C.send b (Finished 11))

Domain.spawn(fun _ -> C.send b (Candidate 0.2))
let sink_d = Domain.spawn(sink_fun a)


Domain.join tally_d

*)


let sink_fun c =
    fun () ->
        let rec sink c = 
        match C.recv c with
            | Candidate t ->
                    Printf.printf "sink: candidate %f\n" t;
                    sink c
            | Threshold t -> 
                    Printf.printf "sink: threshold %f\n" t;
                    sink c
            | Finished id ->
                    Printf.printf "sink: finished %d\n" id;
                    () in
        sink c

(* end *)

