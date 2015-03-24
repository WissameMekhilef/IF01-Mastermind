(* Création d'un type couleur *)
type couleur= Rouge | Bleu | Vert | Noir | Jaune | Orange | Violet | Blanc;;

let listeCouleur= [[Rouge];[Bleu];[Vert];[Noir];[Jaune];[Orange];[Violet];[Blanc]];;

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
      failwith "Erreur taille trop petite";;


(* Création dela fonction de comparaison *)
let rec compList list =
  match list with
  | []->true
  |h::t-> not(List.mem h t) && compList t 

(* Création de la liste sans redondance de couleur *)
let rec  construire_ListSR list comp =
  let rec aux l res=
    match l with 
  |[]->res
  |h::t->if not(comp h) then
      aux t res
    else
      aux t (h::res)
  in aux list [];;

let listeComplete= construire_ListR 5 listeCouleur;;
let listeSR= construire_ListSR listeComplete compList;;

	    
