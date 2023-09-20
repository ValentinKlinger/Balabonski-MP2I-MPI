(* Exercice 12 Écrire une fonction interval: int -> int -> int list telle que
interval i j renvoie la liste des entiers de i inclus à j exclu. La pile d’appels ne doit
pas déborder lorsque j − i est grand. *)

let interval i j =
    let rec loop i j l =
    if i >= j then 
        l 
    else    
        loop i (j - 1) ((j - 1)::l)
    in 
    loop i j []


let () = List.iter (fun x -> print_int x; print_string " ") (interval 2 1_000_000)

(* correction proposé :

On écrit une fonction avec un accumulateur et un appel
terminal.
let rec itv_aux acc i j =
if i >= j then acc else itv_aux (j-1 :: acc) i (j-1)

Elle renvoie la liste [i; i+1; ...; j-1] @ acc. Puis on en déduit la fonction
demandée.
let interval i j =
itv_aux [] i j

*)