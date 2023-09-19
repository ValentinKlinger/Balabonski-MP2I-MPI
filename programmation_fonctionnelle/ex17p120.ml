let max_kadane l = 

    let rec loop l somme_actuelle meilleur_somme =
        match l with
        | [] -> meilleur_somme
        | i::s -> 
        let somme_actuelle = 
        if fst somme_actuelle + i <= i then
            (i, [i]) 
        else 
            (fst somme_actuelle + i, i :: snd somme_actuelle)
        in
        
        let meilleur_somme = 
        if fst meilleur_somme < fst somme_actuelle then
            somme_actuelle
        else 
            meilleur_somme
        in 

        loop s somme_actuelle meilleur_somme
    in
    loop l (0, []) (0, [])

(*
(* Test de la fonction max_kadane *)
let (max_somme, sous_liste_max) = max_kadane([-1; 2; 3; -4])

(* Affichage des résultats *)
;; print_int max_somme;  (* Affiche la somme maximale *)
print_endline "";     (* Saut de ligne *)
List.iter (fun x -> print_int x; print_string " ") sous_liste_max;  (* Affiche la sous-liste maximale *)
print_newline ();     (* Saut de ligne *) *)