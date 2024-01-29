(*Exercice 125 Réécrire le parcours en profondeur (programme 8.3 page 450) sans
utiliser de récursivité. Pour cela, utiliser une pile contenant des sommets.*)
open GraphModule

let dfs (g: digraph) (source: int) : bool array =
  let visited = Array.make (size g) false in
  let stack = ref [ source ] in
  while !stack <> [] do
   let v = List.hd !stack in
   stack := List.tl !stack;
   if not visited.(v) then begin
    visited.(v) <- true;
    List.iter (fun w -> if not visited.(w) then stack := w :: !stack) (succ g v)
   end 
  done;
  visited

(*Solution proposée :*)
let dfs_stack (g: digraph) (source: int) : bool array =
  let visited = Array.make (size g) false in
  let st = Stack.create () in
  let add v = if not visited.(v) then (
  visited.(v) <- true; Stack.push st v) in
  add source;
  while not (Stack.is_empty st) do
  let v = Stack.pop st in
  List.iter add (succ g v);
  done;
  visited