(* 
Exercice 21 Pour résoudre le problème de l’âne rouge (voir chapitre 1), on se pro-
pose de commencer par la représentation d’une configuration et par le calcul des
déplacements valides. On se propose d’adopter les types suivants :
type pos = int * int
type block = { h: int; w: int; p: pos }
type state = block list
Une configuration de l’âne rouge (type state) est une liste de blocs. Chaque bloc
(type block) est défini par sa hauteur h, sa largeur w et sa position p sur la grille
5 × 4. Une position (type pos) est une paire (i, j) avec 0 <= i < 5 un numéro de
ligne et 0 <= j < 4 un numéro de colonne. On fixe, arbitrairement, que les lignes
sont numérotées de haut en bas et les colonnes de gauche à droite, et que la position
d’une pièce est repérée par sa case supérieure gauche. Ainsi, l’âne rouge est le bloc
{ h=2; w=2; p=(0,1) }.

1. Pour garantir l’unicité de la représentation d’une configuration, on ajoute la
contrainte supplémentaire qu’une liste du type state est triée par ordre crois-
sant pour la comparaison polymorphe d’OCaml. Donner la valeur de type
state qui correspond à la configuration initiale du jeu de l’âne rouge (voir
chapitre 1).

2. Écrire une fonction success: state -> bool qui détermine si une configu-
ration est gagnante, c’est-à-dire que l’âne rouge se trouve devant la « sortie
».

3. Écrire une fonction print: state -> unit qui affiche une configuration
sous la forme :

CDDC
CDDC
CBBC
CAAC
A..A

c’est-à-dire avec la lettre A pour les pièces 1 × 1, la lettre B pour la pièce 1 × 2,
la lettre C pour les pièces 2 × 1 et la lettre D pour l’âne rouge.
4. Écrire une fonction moves: state -> state list qui donne, pour une
configuration, tous les déplacements valides. Attention à bien garantir que
les configurations renvoyées sont triées. Indication : Commencer par remplir
une matrice 5×4 de booléens indiquant les cases occupées. Par ailleurs, on pourra
avantageusement tirer profit du fait que les dimensions des pièces ne dépassent
jamais 2 × 2.*)

(* 1. La configuration de state, qui corespond à la configuration initiale du jeu de l’âne rouge.
(Trié par ordre croissant) :
[{h = 1; w = 1; p = (3, 1) }; {h = 1; w = 1; p = (3, 2)};
{h = 1; w = 1; p = (4, 0)}; {h = 1; w = 1; p = (4, 3)};
{h = 1; w = 2; p = (2, 1)}; {h = 2; w = 1; p = (0, 0)};
{h = 2; w = 1; p = (0, 3)}; {h = 2; w = 1; p = (2, 0)};
{h = 2; w = 1; p = (2, 3)}; {h = 2; w = 2; p = (0, 1)}] *)

type pos = int * int
type block = { h: int; w: int; p: pos }
type state = block list


let compare_block block1 block2 = 
  (*List.sort compare_block state output state sorted in ascending order.*)
  compare (block1.h, block1.w, fst block1.p, snd block1.p) 
  (block2.h, block2.w, fst block2.p, snd block2.p)

let rec success state =
 match state with
 | [] -> false
 | i::s ->
     if i.h = 2 && i.w = 2 && i.p = (3,1) then 
       true
     else 
      success s

let color (h, w) =
  match h, w with
  | 1, 1 -> 'A'
  | 1, 2 -> 'B'
  | 2, 1 -> 'C'
  | 2, 2 -> 'D'
  | _, _ -> raise (Invalid_argument "Une pièce n'appartient pas au jeux de l’âne rouge")

let print state =
  let m = Array.make_matrix 5 4 '.' in

  let add c (i, j) = m.(i).(j) <- c in 

  let modif_matrix h w i j =
    for ai = 0 to h - 1 do
      for aj = 0 to w - 1 do 
        add (color (h, w)) (i + ai, j + aj) done done in 
  List.iter (fun x -> modif_matrix x.h x.w (fst x.p) (snd x.p)) state;
  for i = 0 to 4 do 
    for j = 0 to 3 do
      Printf.printf "%c" m.(i).(j)
    done;
    Printf.printf "\n";
  done

let moves state = 
  (*Representation of the red donkey game as a matrix where empty cells have the value false.*)
  let free_space = Array.make_matrix 5 4 false in 
  let fill h w i j =
  for ai = 0 to h - 1 do
    for aj = 0 to w - 1 do 
      free_space.(ai + i).(aj + j) <- true done done in
  List.iter (fun x -> fill x.h x.w (fst x.p) (snd x.p)) state;

  (*Creation of a list containing the location of empty cells.*)
  let free = ref [] in
  for i = 0 to 4 do
    for j = 0 to 3 do
      if not free_space.(i).(j) then
        free := (i, j) :: !free
    done;
  done;

  (*Function that tells us whether a cell in the matrix is free or not.*)
  let is_free i j =
    List.exists (fun (x, y) -> x = i && y = j) !free in 

  (*Functions that tell us whether different movements are possible for a given piece.*)
  let can_move_up w i j =
    i > 0 && (is_free (i-1) j) && (w=1 || (is_free (i-1) (j+1))) in     
  let can_move_down w h i j =
    (i+h) < 5 && (is_free (i+h) j) && (w=1 || (is_free (i+h) (j+1))) in
  let can_move_left h w i j =
    j > 0 && (is_free i (j-1)) && (h=1 || (is_free (i+1) (j-1))) in
  let can_move_right h w i j =
    (j+w) < 4 && (is_free i (j+w)) && (h=1 || (is_free (i+1) (j+w))) in
  
  (*This function give the new game configuration after a piece has been moved. *)
  let replace i j stat direction =
    let check i j block direction =
      let xi = fst block.p in
      let xj = snd block.p in
      if xi = i && xj = j then

        if direction = "up" then 
          {h=block.h; w=block.w; p=(i-1, j)}
        else if direction = "down" then 
          {h=block.h; w=block.w; p=(i+1, j)}
        else if direction = "left" then 
          {h=block.h; w=block.w; p=(i, j-1)}
        else
          {h=block.h; w=block.w; p=(i, j+1)}
      else 
        block
      in
    List.sort compare_block (List.map (fun block -> check i j block direction) stat)
    in

  (*Creates a list of all new configurations and return it.*)
  let out = ref [] in 
  let mouve h w i j = 
    if can_move_up w i j then 
      out := (replace i j state "up") :: !out;
    if can_move_down w h i j then
      out := (replace i j state "down") :: !out;
    if can_move_left h w i j then
      out := (replace i j state "left") :: !out;
    if can_move_right h w i j then
      out := (replace i j state "right") :: !out;
    in
  List.iter (fun x -> mouve x.h x.w (fst x.p) (snd x.p)) state;
  !out



(*let state = 
[{h = 1; w = 1; p = (3, 1) }; {h = 1; w = 1; p = (3, 2)};
{h = 1; w = 1; p = (4, 0)}; {h = 1; w = 1; p = (4, 3)};
{h = 1; w = 2; p = (2, 1)}; {h = 2; w = 1; p = (0, 0)};
{h = 2; w = 1; p = (0, 3)}; {h = 2; w = 1; p = (2, 0)};
{h = 2; w = 1; p = (2, 3)}; {h = 2; w = 2; p = (0, 1)}]

let () = Printf.printf "%b\n" (success state)

let () = print state

let () = 
let result = moves state in
Printf.printf "Valid moves:\n";
List.iter (fun etat -> print etat; print_newline ()) result

let state2 = 
  [{h = 2; w = 1; p = (0, 0) }; {h = 2; w = 1; p = (0, 1)};
  {h = 2; w = 1; p = (0, 2)}; {h = 2; w = 1; p = (0, 3)};
  {h = 1; w = 1; p = (2, 0)}; {h = 1; w = 1; p = (2, 1)};
  {h = 1; w = 2; p = (2, 2)}; {h = 2; w = 2; p = (3, 1)};
  {h = 1; w = 1; p = (3, 3)}; {h = 1; w = 1; p = (4, 3)}]

let state2_sorted = List.sort compare_block state2

let () = Printf.printf "%b\n" (success state2_sorted)

let () = print state2_sorted

let () = 
let result = moves state2_sorted in
Printf.printf "Valid moves:\n";
List.iter (fun etat -> print etat; print_newline ()) result*)

(*
Correction proposé dans le manuel :

https://www.informatique-mpi.fr/files/code/ane_rouge.ml.html   
*)