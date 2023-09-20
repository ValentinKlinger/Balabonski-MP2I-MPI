(*Exercice 13 Étant donnée la définition de type type t = B | N | R, écrire les
fonctions suivantes (de manière récursive ou à l’aide d’un itérateur).
Étant donnée une liste l, la fonction permute : t list -> t list renvoie
une nouvelle liste dans laquelle les valeurs B de l sont remplacées par des N, les N
par des R et les R par des B.
La fonction récursive terminale compte_B : t list -> int qui compte le
nombre de B dans la liste passée en paramètre.
La fonction plus_grande_sequence : t list -> int qui renvoie la longueur
de la plus grande séquence de B dans la liste passée en paramètre. Par exemple,
l’exécution de plus_grande_sequence [B;N;N;B;B;B;R;N;N;B;B;R] renvoie 3.*)

type t = B | N | R

let rec permute l =
  match l with
  | [] -> []
  | B :: s -> N :: permute s  
  | N :: s -> R :: permute s 
  | R :: s -> B :: permute s 

let compte_B l =
  List.fold_left (fun x -> fun y -> if y = B then x + 1 else x) 0 l
  
let plus_grande_sequence l =
  let rec loop l longeur =
    match l with
    | [] -> max (fst longeur) (snd longeur)
    | i::s -> 
      let longeur = 
      if i = B then
        (fst longeur + 1, snd longeur)
      else 
        (0, max (fst longeur) (snd longeur))

      in loop s longeur

  in loop l (0, 0)


(*Correction proposé :

type t = B | N | R

let rec permute l =
match l with
| [] -> []
| B :: s -> N :: permute s
| N :: s -> R :: permute s
| R :: s -> B :: permute s

let echange x =
match x with
| B -> N
| N -> R
| R -> B

let rec permute l =
match l with
| [] -> []
| x :: s -> echange x :: permute s
let permute l = List.map echange l

let compte l =
let rec compte l acc =
match l with
| [] -> acc
| B :: s -> compte s (acc + 1)
| _ :: s -> compte s acc
in
compte l 0

let compte l =
List.fold_left (fun acc x -> (if x = B then 1 else 0) + acc) 0 l

let plus_grande_sequence l =
let rec pgs (m, p) l =
match l with
| [] -> max m p
| B :: s -> pgs (m, p + 1) s
| _ :: s -> pgs (max m p, 0) s
in
pgs (0, 0) l

let plus_grande_sequence l =
let m, p =
List.fold_left
(fun (m, p) x -> if x <> B then (max m p, 0) else (m, p + 1))
(0, 0) l in
max m p   
*)