open List;;
open ListCouleur;;

exception Tricherie;;

(* fonction de jeu: affichage de la propostion *)
(* elagage de la liste en fonction de bp et mp *)
(* test de Tricherie et combinaison caché *)
let jouer  nbcoup liste  =
  let rec aux n l =
    match (n,l) with
    |_,[]-> raise Tricherie
    |x,[h]->
      let prop =hd l in
      ListCouleur.print_list prop;
      print_string "est la bonne combinaison !\n";
      print_newline();
    |x,_ when x>nbcoup && nbcoup<>0->
      print_string "Plus d'essai: PERDU!\n";
      print_newline();
    |x,_->
      let prop=hd l in
      print_string "Essai ";
      print_int x;
      print_string " : ";
      ListCouleur.print_list prop;
      print_string "\n";
      print_string "Nombre de pion(s) bien placé(s):\n";
      try
	let bp=read_int() in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let mp=read_int() in
	print_newline();
	aux (x + 1) (ListCouleur.elagage prop (bp,mp) l)
      with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;

  in aux 1 liste;;

(* Menu récursif pour rejouer *)
(* gestion de la Tricherie *)
(* choix du mode de jeu *)
let rec menu listC listSR=
  print_newline();
   print_string "Choisissez votre combinaison de 5 couleurs parmis les couleurs suivantes:\n \027[31m Rouge \027[0m \027[34m Bleu \027[0m \027[32m Vert \027[0m \027[30m Noir \027[0m \027[93m Jaune \027[0m \027[33m Orange \027[0m\027[35m Violet \027[0m \027[37m Blanc \027[0m \n";
   try
  print_string "Choisissez votre mode de jeu (1: Sans redondance, 2: Avec redondance, 3: avec nombre de coup limité, 0: quitter)\n";
    let mode=read_int() in
    match  mode with
    |1->jouer 0 listSR ;
	menu listC listSR    
    |2->jouer 0 listC ;
	menu listC listSR   
    |3->print_string "Combien de coups maximum souhaitez vous?\n";
      let nbcoup=read_int() in        
	jouer nbcoup listC ;
	menu listC listSR      
    |0->print_string "Merci d'avoir joué!\n"
    |_-> print_string "Erreur de choix\n";
      menu listC listSR
  with 
  |Failure("int_of_string")-> print_string "Erreur de saisie!\n"; menu listC listSR
  |Tricherie -> print_string "Tricherie!\n"; menu listC listSR;;

(* Construction des listes de combinaison*)
let listeComplete= ListCouleur.construire_ListR 5 ListCouleur.listeCouleur;;
let listeSR= ListCouleur.construire_ListSR listeComplete ListCouleur.compList;;
 
let main() =
  print_string "MASTERMIND \n";
  menu listeComplete listeSR;
  print_string "Au revoir! \n";;

main();;
