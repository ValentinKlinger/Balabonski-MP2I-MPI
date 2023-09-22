(* Exercice 14 Écrire deux fonctions union : 'a list -> 'a list -> 'a list
et inter : 'a list -> 'a list -> 'a list prenant en argument deux listes
triées et calculant respectivement leur union et leur intersection. *)

let rec union l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | [], i::s -> i::s
  | a::b, [] -> a::b
  | a::b, i::s ->
    if (a < i) then a :: union b l2 else
    if (a = i) then a :: union b s else 
    i :: union l1 s

let rec inter l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | [], i::s -> []
  | a::b, [] -> []
  | a::b, i::s ->
    if (a < i) then inter b l2 else
    if (a = i) then a :: inter b s else 
    inter l1 s

(* let l1 = [1; 3; 4; 6; 7; 10]
let l2 = [2; 3; 5; 6; 7; 9; 10; 14]

let () = List.iter (fun x -> print_int x; print_string " ") (union l1 l2)
let () = print_newline ()
let () = List.iter (fun x -> print_int x; print_string " ") (inter l1 l2) *)

(* let rec union l1 l2 =
match l1, l2 with
[], _ -> l2
| _, [] -> l1
| p1 :: ll1 , p2 :: ll2 ->
if p1 < p2 then p1 :: union ll1 l2
else if p1 = p2 then p1 :: union ll1 ll2
else p2 :: union l1 ll2

let rec inter l1 l2 =
match l1, l2 with
([], _) | (_, []) -> []
| p1 :: ll1 , p2 :: ll2 ->
if p1 < p2 then inter ll1 l2
else if p1 = p2 then p1 :: inter ll1 ll2
else inter l1 ll2 *)