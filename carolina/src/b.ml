(* b.ml *)

(* usage: 

```sh
rlwrap -a ocaml -noinit 
```

*)

Random.init 13

let n = 10

let x = Array.init n (fun _ -> Random.float 1.0)
let () = Array.sort Float.compare x

let u = 0.5

(* if u < x.(Array.length x - 1) then ... *)

let i = ref 0

while u > x.(!i) do i := !i + 1 done

for j = Array.length x - 1 downto !i + 1 do x.(j) <- x.(j - 1) done
x.(!i) <- u


let insert x u =
    if u < x.(Array.length x - 1) then
        let i = ref 0 in
        while u > x.(!i) do i := !i + 1 done;
        for j = Array.length x - 1 downto !i + 1 do x.(j) <- x.(j - 1) done;
        x.(!i) <- u


let () = print_endline "Oh, Carolina!"

(*

for j = 3 downto 4 do 
    Printf.printf "j is %d\n" j 
done

*)

(* end *)
