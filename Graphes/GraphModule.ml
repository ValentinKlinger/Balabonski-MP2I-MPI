type digraph = int list array

let create (n: int) : digraph =
  Array.make n []

let size (g: digraph) : int =
  Array.length g

let has_edge (g: digraph) (u: int) (v: int) : bool =
  List.mem v g.(u)

let add_edge (g: digraph) (u: int) (v: int) : unit =
  if not (has_edge g u v) then
    g.(u) <- v :: g.(u)

let succ (g: digraph) (u: int) : int list =
  g.(u)

let edges (g: digraph) : (int * int) list =
  let l = ref [] in
  for i = 0 to size g - 1 do
    List.iter (fun j -> l := (i, j) :: !l) g.(i)
  done;
  !l