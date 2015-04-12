open List;;
open ListCouleur;;

exception Tricherie;;

(* fonction de jeu: affichage de la propostion *)
(* elagage de la liste en fonction de bp et mp *)
(* test de Tricherie et combinaison caché *)

let jouer  nbcoup liste =
  let rec aux n l =
    if l=[] then
      raise Tricherie
    else
      let prop =hd l in
      if length l=1 then (
        ListCouleur.print_list prop;
        print_string "est la bonne combinaison !\n";
        print_newline())
      else if n = nbcoup then (
        print_string "Plus d'essai: PERDU!\n";
        print_newline())
      else (
        print_string "Essai ";
        print_int n;
        print_string " : ";
	ListCouleur.print_list prop;
	print_string "\n";
	print_string "Nombre de pion(s) bien placé(s):\n";
	let bp=read_int() in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let mp=read_int() in
        print_newline();
        aux (n + 1) (ListCouleur.elagage prop (bp,mp) l))
  in aux 1 liste;;

(* Menu récursif pour rejouer *)
(* gestion de la Tricherie *)
(* choix du mode de jeu *)

let rec menu listC listSR=
  print_newline();
  print_string "Choisissez votre mode de jeu (1: Sans redondance, 2: Avec redondance, 3: avec nombre de coup limité, 0: quitter)\n";
  let mode=read_int() in
  match  mode with
  |1->begin try
	      jouer 0 listSR;
	      menu listC listSR
    with
      Tricherie-> print_string "Tricherie! \n";
	menu listC listSR end
   |2->begin  try 
	  jouer 0 listC;
	  menu listC listSR
      with Tricherie->print_string "Tricherie! \n";
	menu listC listSR end 
  |3->begin print_string "Souhaitez vous un nombre de coups maximum? (0 pour aucun)\n";
   let nbcoup=read_int() in 
	try 
	  jouer nbcoup listC;
	  menu listC listSR
	with Tricherie->print_string "Tricherie!";
	  menu listC listSR end
  |0->print_string "Merci d'avoir joué!\n"
  |_-> print_string "Erreur de choix\n";
    menu listC listSR;;

(* Construction des listes de combinaison*)

let listeComplete= ListCouleur.construire_ListR 5 ListCouleur.listeCouleur;;
let listeSR= ListCouleur.construire_ListSR listeComplete ListCouleur.compList;;
 
let main() =
  print_string "MASTERMIND \n";
  menu listeComplete listeSR;
  print_string "Au revoir! \n";;

main();;
