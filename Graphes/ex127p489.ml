(* Exercice 127 Tel que le parcours en profondeur est écrit, dans le programme 8.3
page 450, il détermine s’il existe un chemin entre le sommet source et tout autre
sommet, mais il ne renvoie pas de tel chemin lorsqu’il existe. Pour y remédier, écrire
une variante de ce programme qui renvoie un tableau donnant, pour chaque som-
met v, le sommet qui a permis de l’atteindre pendant le parcours, le cas échéant, et
la valeur −1 sinon. Pour le sommet source, on indiquera sa propre valeur. Écrire
ensuite une fonction qui reconstruit le chemin, comme une liste de sommets, entre
la source et un sommet donné. *)

open GraphModule

let dfs (g: digraph) (source: int) : int array =
  let visited = Array.make (size g) false in
  let path = Array.make (size g) (-1) in
  let rec dfs p v =
    if not visited.(v) then (
      visited.(v) <- true;
      path.(v) <- p;

      List.iter (dfs v) (succ g v);
    ) in
    dfs source source;
  path

(*Solution proposée :*)

let dfs_path (g: digraph) (source: int) : int array =
  let visited = Array.make (size g) false in
  let path = Array.make (size g) (-1) in
  let rec dfs p v =
  if not visited.(v) then (
  visited.(v) <- true;
  path.(v) <- p;
  List.iter (dfs v) (succ g v)
  ) in
  dfs source source;
  path