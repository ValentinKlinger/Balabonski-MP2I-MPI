(*Exercice 126 Écrire une fonction has_cycle: digraph -> int -> bool qui
détermine s’il existe un cycle dans un graphe orienté, accessible à partir du som-
met donné. Pour cela, utiliser un parcours en profondeur en marquant les sommets
avec trois couleurs : non visité / en cours de visite / visité. Si on arrive sur un som-
met en cours de visite, c’est qu’on a découvert un cycle. Discuter ensuite le cas d’un
graphe non orienté.*)

open GraphModule

let has_cycle (g: digraph) (source: int) : bool =
  let color = Array.make (size g) 0 in
  let rec dfs v =
    if color.(v) = 1 then true
    else
    if color.(v) = 0 then (
      color.(v) <- 1;

    let has_cycle_in_succ = List.exists dfs (succ g v) in
    color.(v) <- 2;
    has_cycle_in_succ
    ) 
    else false in
    dfs source

(*Solution proposée :*)

type color = Unvisited | InProgress | Visited

let has_cycle g source : bool =
  let color = Array.make (size g) Unvisited in
  let rec dfs v = match color.(v) with
  | Visited -> ()
  | InProgress -> raise Exit
  | Unvisited ->
  color.(v) <- InProgress;
  List.iter dfs (succ g v);
  color.(v) <- Visited
  in
  try dfs source; false with Exit -> true