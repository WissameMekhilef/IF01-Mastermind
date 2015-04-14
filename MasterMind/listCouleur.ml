open List;;

(* Création d'un module avec création type couleur et les fonctions associées *)
module ListCouleur=struct
(* Création d'un type couleur *)
type couleur= Rouge | Bleu | Vert | Noir | Jaune | Orange | Violet | Blanc

let listeCouleur= [[Rouge];[Bleu];[Vert];[Noir];[Jaune];[Orange];[Violet];[Blanc]]

(* Création de la liste de coup possible avec redondance *)
let rec construire_ListR taille listC=
  let rec construire_aux list= 
    match list with (* permet de créer les différents pattern / ajout récursif à la liste de possibilité *)
    |[]->[]
    |h::t->(Rouge::h)::(Bleu::h)::(Vert::h)::(Noir::h)::(Jaune::h)::(Orange::h)::(Violet::h)::(Blanc::h)::(construire_aux t)
  in match taille with (* permet de répéter/accumuler récursivement la fonction pour obtenir la bonne taille de possibilité  *)
  |1 -> listC
  |_-> if taille>=1 then
      construire_aux (construire_ListR (taille-1) listC)
    else 
      failwith "Erreur taille trop petite"


(* Création dela fonction de comparaison *)
let rec compList list =
  match list with
  | []->true
  |h::t-> not(mem h t) && compList t 

(* Création de la liste sans redondance de couleur *)
let rec  construire_ListSR list comp =
  let rec aux l res=
    match l with 
  |[]->res
  |h::t->if not(comp h) then
      aux t res
    else
      aux t (h::res)
  in aux list []

(* Supprime la 1ere occurence de e dans l *)

let suppression e l =
  let rec aux res l2 = match l2 with
      [] -> res
    | h::t -> if e = h then res @ t else aux (res @ [h]) t
  in aux [] l

(* Compare deux combinaisons et retourne le nombre de pions communs *)

let pions_communs l1 l2 =
  let rec aux l1 l2 = match l1 with
      [] -> l2
    | h::t -> aux t (suppression h l2)
  in length l2 - length (aux l1 l2);;

(* Compare deux combinaisons et retourne le nombre de pions bien places *)

let pions_bien_places l1 l2 =
  let rec aux n l1 l2 = match l1,l2 with
    | [],_ -> n
    |_,[]->n
    | h1::t1,h2::t2 -> if h1 = h2 then aux (n + 1) t1 t2 else aux n t1 t2
  in aux 0 l1 l2

(* Compare deux combinaisons et retourne un couple d'entiers (bp, mp) *)
(* bp est le nombre de pions bien places et mp le nombre de pions mal places *)

let indications l1 l2 =
  let bp = pions_bien_places l1 l2 in
  let mp = (pions_communs l1 l2) - bp in (bp, mp)

(* comb est la combinaison proposee par l'ordinateur *)
(* ind est le couple d'indications donnee par l'utilisateur *)
(* l est la liste des solutions potentielles selon l'ordinateur *)
(* Supprime de l toutes les combinaisons qui ne peuvent etre la solution *)

let elagage comb ind l =
  let rec aux l1 l2 = match l2 with
      [] -> l1
    | h::t -> if (indications comb h) = ind
        then aux (l1 @ [h]) t
        else aux l1 t
  in aux [] l

(* Affichage des couleurs  *)

let rec print_list l =
  match l with
  | [] -> ()
  | h::t -> match h with
    |Rouge-> print_string "\027[31m Rouge \027[0m";
      print_list t
    |Bleu -> print_string "\027[34m Bleu \027[0m";
      print_list t
    |Vert->  print_string "\027[32m Vert \027[0m";
      print_list t
    |Noir->  print_string "\027[30m Noir \027[0m";
      print_list t
    |Jaune->  print_string "\027[93m Jaune \027[0m";
      print_list t
    |Orange->  print_string "\027[33m Orange \027[0m";
      print_list t
    |Violet->  print_string "\027[35m Violet \027[0m";
      print_list t
    |Blanc->  print_string "\027[37m Blanc \027[0m";
      print_list t

end
