open List;;
open String;;

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

let suppression e l =
  let rec aux res l2 = match l2 with
      [] -> res
    | h::t -> if e = h then res @ t else aux (res @ [h]) t
  in aux [] l;;

(* Compare deux combinaisons et retourne le nombre de pions communs *)

let pions_communs l1 l2 =
  let rec aux l1 l2 = match l1 with
      [] -> l2
    | h::t -> aux t (suppression h l2)
  in List.length l2 - List.length (aux l1 l2);;

(* Compare deux combinaisons et retourne le nombre de pions bien places *)

let pions_bien_places l1 l2 =
  let rec aux n l1 l2 = match l1,l2 with
    | [],_ -> n
    |_,[]->n
    | h1::t1,h2::t2 -> if h1 = h2 then aux (n + 1) t1 t2 else aux n t1 t2
  in aux 0 l1 l2;;

(* Compare deux combinaisons et retourne un couple d'entiers (bp, mp) *)
(* bp est le nombre de pions bien places et mp le nombre de pions mal places *)

let indications l1 l2 =
  let bp = pions_bien_places l1 l2 in
  let mp = (pions_communs l1 l2) - bp in (bp, mp);;

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
  in aux [] l;;

let rec print_list l =
  match l with
  | [] -> ()
  | h::t -> match h with
    |Rouge-> print_string "Rouge ";
      print_list t
    |Bleu -> print_string "Bleu ";
      print_list t
    |Vert->  print_string "Vert ";
      print_list t
    |Noir->  print_string "Noir ";
      print_list t
    |Jaune->  print_string "Jaune ";
      print_list t
    |Orange->  print_string "Orange ";
      print_list t
    |Violet->  print_string "Violet ";
      print_list t
    |Blanc->  print_string "Blanc ";
      print_list t;;

exception Tricherie;;

let jouer  nbcoup liste =
  let rec aux n l =
    if l=[] then
      raise Tricherie
    else
      let prop =hd l in
      if List.length l=1 then (
        print_list prop;
        print_string "est la bonne combinaison !\n";
        print_newline())
      else if n = nbcoup then (
        print_string "Plus d'essai: PERDU!\n";
        print_newline())
      else (
        print_string "Essai ";
        print_int n;
        print_string " : ";
        print_list prop;
	print_string "\n";
	print_string "Nombre de pion(s) bien placé(s):\n";
	let bp=read_int() in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let mp=read_int() in
        print_newline();
        aux (n + 1) (elagage prop (bp,mp) l))
  in aux 1 liste;;

let rec menu listC listSR=
  print_newline();
  print_string "Choisissez votre mode de jeu (1: Sans redondance, 2: Avec redondance, 3: avec nombre de coup limité, 0: quitter)\n";
  let mode=read_int() in
  match  mode with
  |1->begin try 
	  jouer 0 listSR;
	  menu listC listSR
    with
      Tricherie-> print_string "Tricherie! \n"; end 
   |2->begin  try 
	  jouer 0 listC;
	  menu listC listSR
      with Tricherie->print_string "Tricherie! \n"; end
  |3->begin print_string "Souhaitez vous un nombre de coups maximum? (0 pour aucun)\n";
   let nbcoup=read_int() in 
	try 
	  jouer nbcoup listC;
	  menu listC listSR
	with Tricherie->print_string "Tricherie!"; end
  |0->print_string "Merci d'avoir joué!\n"
  |_-> print_string "Erreur de choix\n";
    menu listC listSR;;

let listeComplete= construire_ListR 5 listeCouleur;;
let listeSR= construire_ListSR listeComplete compList;;
 
let main() =
  print_string "MASTERMIND \n";
  menu listeComplete listeSR;
  print_string "Au revoir! \n";;

main();;
