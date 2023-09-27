(*Exercice 22 Écrire un programme mlcat prenant en argument un nom de fichier
et affichant le contenu de ce dernier sur la sortie standard. Si aucun fichier n’est
passé sur la ligne de commande, mlcat affiche sa sortie standard (comme le ferait
cat).*)

let mlcat file =
  let rec read () =
    let str_line = input_line file in Printf.printf "%s\n" str_line;
    read ()
  in 
  try 
    read ()
  with 
  | End_of_file -> close_in file


let () =
  let file =
    if Array.length Sys.argv < 2 then
      stdin
    else 
      open_in Sys.argv.(1) in 
  mlcat file
  

(*Correction proposé dans le manuel :
   
let rec cat ic =
  let cont =
    try
      print_char (input_char ic);
      true
    with End_of_file -> false
  in if cont then cat ic

let () =
  try
    let ic =
      if Array.length Sys.argv < 2 then
        stdin
      else open_in Sys.argv.(1)
    in cat ic
  with _ -> Printf.printf "Erreur\n"*)