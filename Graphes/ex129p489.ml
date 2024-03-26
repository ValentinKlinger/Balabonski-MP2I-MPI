(*Exercice 129 En utilisant un parcours en profondeur, écrire une fonction
is_bipartite: graph -> bool qui détermine si un graphe est biparti. Indication :
marquer les sommets visités avec une couleur 0 ou 1.*)

open GraphModule

let is_bipartite g =
  
  let output = ref true in
  let color = Array.make (size g) (-1) in
  let next_color = ref 0 in

  let rec dfs v =
  if color.(v) = -1 then (
    color.(v) <- !next_color;
    next_color := (!next_color + 1) mod 2;
    List.iter dfs (succ g v);
  ) else 
  if color.(v) <> !next_color then (
    output := false;
  )
  in
  dfs 0;
  !output

(*Solution proposée :*)

let is_bipartite g =
  let n = size g in
  let color = Array.make n (-1) in
  let rec dfs c v = (* on vient de la couleur c *)
  if color.(v) = -1 then (
  color.(v) <- 1 - c;
  List.iter (dfs (1 - c)) (succ g v)
  ) else
  if color.(v) = c then raise Exit
  in
  try
  for i = 0 to n-1 do if color.(i) = -1 then dfs 0 i done; true
  with Exit -> false