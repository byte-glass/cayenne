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

type 'a worker_message = Candidate of 'a | Finished of int
type 'b tally_message = Threshold of 'b 

(* type ('a, 'b) channels = {snd : 'a C.t; rcv : 'b C.t} *)
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
        let rec tally f u =
            if List.length f = List.length ch.snd then
                ()
            else
                match C.recv ch.rcv with
                | Candidate t ->
                        Printf.printf "tally: candidate %f\n%!" t;
                        if t < u.(Array.length u - 1) then
                            let _ = insert u t in
                            let th = Threshold u.(Array.length u - 1) in
                            List.iter (fun c -> C.send c th) ch.snd;
                        tally f u
                | Finished id ->
                        Printf.printf "tally: finished %d\n%!" id;
                        tally ((Finished id) :: f) u in
        tally [] u


let update c theta id =
    let rec upd th =
        match C.recv_poll c with 
        | Some (Threshold t) -> th := t; upd th
        | None -> () in
    let th = ref theta in
    upd th; 
    Printf.printf "update: %d: %f -> %f\n%!" id theta !th;
    !th


let worker_fun ch x bounds i theta =
    fun () ->
        let rec worker i theta = 
            if i >= bounds.upper then
                let _ = C.send ch.snd (Finished (Domain.self () :> int)) in ()
            else
                let j = ref i in
                let _ = while (!j < bounds.upper - 1) && (x.(!j) >= theta) do 
                    Printf.printf "%d %f\n%!" (Domain.self () :> int) x.(!j);
                    j := !j + 1 
                done in
                if x.(!j) < theta then
                    let _ = C.send ch.snd (Candidate x.(!j)) in
                    worker (!j + 1) (update ch.rcv theta (Domain.self() :> int))
                else 
                    worker (!j + 1) theta in
        worker bounds.lower theta

(* two workers!

Random.init 103
let n = 10
let x = Array.init (3 * n) (fun _ -> Random.float 1.0)

let u = Array.init n (fun i -> x.(i))
let () = Array.sort Float.compare u
let theta = u.(Array.length u - 1)

let a0 = C.make_unbounded () and
    a1 = C.make_unbounded () and
    b = C.make_unbounded () in
    let domains = [(tally_fun {snd = [a0; a1]; rcv = b} u); 
            (worker_fun {snd = b; rcv = a0} x {lower = n; upper = 2 * n} 0 theta);
            (worker_fun {snd = b; rcv = a1} x {lower = 2 * n; upper = 3 * n} n theta)] in
    List.map (fun d -> Domain.spawn(d)) domains



*)

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
let worker_d = Domain.spawn(worker_fun {snd = b; rcv = a} x {lower = 0; upper = Array.length x} 0 theta)

*)

(* test worker_fun

let a = C.make_unbounded ()
let b = C.make_unbounded ()

Random.init 103
let n = 15
let x = Array.init n (fun _ -> Random.float 1.0)

let worker_d = Domain.spawn(worker_fun {snd = b; rcv = a} x 0 0.5)
let sink_d = Domain.spawn(sink_worker_fun b)

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
let sink_d = Domain.spawn(sink_tally_fun a)


Domain.join tally_d
Domain.join sink_d

*)

let sink_tally_fun c =
    fun () ->
        let rec sink c = 
            match C.recv c with
            | Threshold t -> 
                    Printf.printf "sink: threshold %f\n%!" t;
                    sink c in
        sink c

let sink_worker_fun c =
    fun () ->
        let rec sink c = 
        match C.recv c with
            | Candidate t ->
                    Printf.printf "sink: candidate %f\n%!" t;
                    sink c
            | Finished id ->
                    Printf.printf "sink: finished %d\n%!" id;
                    () in
        sink c

(* end *)

