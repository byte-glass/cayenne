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





let () = print_endline "Oh, Carolina!"


(* end *)

