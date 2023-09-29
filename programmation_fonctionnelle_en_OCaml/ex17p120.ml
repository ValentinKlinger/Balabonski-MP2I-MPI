(* Exercice 17 Le problème de la sous-liste maximale est le problème de trouver dans
une liste d’entiers (positifs ou négatifs) la sous-liste ayant la plus grande somme.
Par sous-liste, on entend ici une liste d’éléments contigus. Par exemple, dans la liste
d’entiers (de type int) [-2; 1; -3; 4; -1; 2; 1; -5; 4], une sous-liste ayant
la plus grande somme est la sous-liste [4; -1; 2; 1] avec une somme de 6.
Pour résoudre ce problème, Jay Kadane (Carnegie Mellon University) a proposé
en 1984 un algorithme efficace qui ne parcourt qu’une seule fois la liste. Le fonction-
nement de cet algorithme est décrit par récurrence de la manière suivante :
- Si l est vide, alors la plus grande somme est 0.
- Si l est de la forme [v 1 ; v 2 ; . . . ; vi−1 ; vi ; . . . ; vn ] et mi−1 est la plus grande somme
se terminant à l’indice i − 1, alors la plus grande somme de [v 1 ; v 2 ; . . . ; vi−1 ; vi ]
est max (vi , mi−1 + vi ).
Écrire une fonction max_kadane, de type int list -> int, qui implé-
mente l’algorithme de Kadane donné ci-dessus et telle que max_kadane
l renvoie uniquement la plus grande somme d’une liste l. Indication :
il peut être utile d’utiliser une seconde liste [0; m 1 ; m 2 ; . . . ; mn ] pour
mémoriser les plus grandes sommes intermédiaires, ainsi qu’une fonction
ma_liste : ('a -> 'a -> int) -> 'a list -> 'a renvoyant le plus grand
élément d’une liste non vide, selon une fonction de comparaison donnée.
Étendre la fonction ci-dessus pour écrire une fonction kadane, de type
int list -> int list, telle que kadane l renvoie une sous-liste de l de somme
maximale. *)

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
let (max_somme, sous_liste_max) = max_kadane([-1; 2; 3; -4])

;; print_int max_somme; 
print_endline "";
List.iter (fun x -> print_int x; print_string " ") sous_liste_max;  
print_newline (); *)

(* 
Correction proposé dans le manuel :
Exercice 17, page 120 On donne la fonction kadane. On remarque que si
max (vi , mi−1 + vi ) = vi , on commence une nouvelle sous-séquence.

let kadane l =
    let rec loop l l_max =
        match (l, l_max) with
            [], _ -> max_list (fun (a, _) (b, _) -> compare a b) l_max
        | v :: ll, (m, lm) :: _ ->
            let vm = v + m in
            loop ll ((if v <= vm then (vm,v::lm) else (v,[v]))::l_max)
        | _ -> assert false
    in List.rev (snd (loop l [ (0, []) ]))

*)