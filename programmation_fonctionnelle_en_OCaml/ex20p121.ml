(*Exercice 20 Écrire une fonction mex: int list -> int qui renvoie le plus petit
entier naturel qui n’apparaît pas dans la liste passée en argument. La liste contient
des entiers quelconques, y, compris négatif, et n’est pas triée.*)

let mex (l: int list) : int =
  let len_l = List.length l in
  let natural = Array.init ((len_l)+1) (fun i -> i) in
  let rec natural_without_l l =
    match l with
    | [] -> ()
    | i::s ->
      if i >= 0 && i < len_l then
        natural.(i) <- -1;
      natural_without_l s
  in 
  natural_without_l l;
  let index = ref 0 in 
  while natural.(!index) = -1 do 
    index := !index + 1;
  done;
  natural.(!index);;


  
(*let l1: int list = [-1; 0; -3; 6; 4; 43; 2; 7; 1; 8]

let () = print_int (mex l1) *)

(*
Correction proposé dans le manuel :

let mex l =
  let n = List.length l in
  let free = Array.make (n+1) true in
  List.iter (fun v -> if 0 <= v && v < n then free.(v) <- false) l;
  let rec find i = if free.(i) then i else find (i+1) in
  find 0
*)