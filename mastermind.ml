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
	    
(* Supprime la premiere occurence de l'element e dans la liste l *)

let suppression e l =
  let rec aux l1 l2 = match l2 with
      [] -> l1
    | h::t -> if e = h then l1 @ t else aux (l1 @ [h]) t
  in aux [] l;;

(* Compare deux combinaisons et retourne le nombre de pions communs *)

let pions_communs l1 l2 =
  let rec aux l1 l2 = match l1 with
      [] -> l2
    | h::t -> aux t (suppression h l2)
  in List.length l2 - List.length (aux l1 l2);;

(* Compare deux combinaisons et retourne le nombre de pions bien places *)

let pions_bien_places l1 l2 =
  let rec aux n l1 l2 = match l1 with
      [] -> n
    | h1::t1 -> match l2 with
        h2::t2 -> if h1 = h2 then aux (n + 1) t1 t2 else aux n t1 t2
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

(* Affiche une combinaison *)

let rec print_list l =
  match l with
  | [] -> ()
  | h::t -> match h with
    |Rouge-> print_string "Rouge";
      print_string ", ";
      print_list t
    |Bleu -> print_string "Bleu";
      print_string ", ";
      print_list t
    |Vert->  print_string "Vert";
      print_string ", ";
      print_list t
    |Noir->  print_string "Noir";
      print_string ", ";
      print_list t
    |Jaune->  print_string "Jaune";
      print_string ", ";
      print_list t
    |Orange->  print_string "Orange";
      print_string ", ";
      print_list t
    |Violet->  print_string "Violet";
      print_string ", ";
      print_list t
    |Blanc->  print_string "Blanc";
      print_string ", ";
      print_list t;;

(* sol est la solution *)
(* max est le nombre d'essais maximum, ou 0 en cas d'absence de limite *)
(* l est la liste des combinaisons connues par l'ordinateur *)
(* Lance une partie de Mastermind *)

let mastermind sol max l =
  let rec aux n l =
    let prop = List.hd l in
      if prop = sol then (
        print_string "Essai ";
        print_int n;
        print_string " : ";
        print_list prop;
        print_string "gagne !";
        print_newline())
      else if n = max then (
        let ind = indications prop sol in
          match ind with
              bp, mp -> (
                print_string "Essai ";
                print_int n;
                print_string " : ";
                print_list prop;
                print_int bp;
                print_string " pion(s) bien place(s), ";
                print_int mp;
                print_string " pion(s) mal place(s), perdu...";
                print_newline()))
      else (
        let ind = indications prop sol in
          match ind with
              bp, mp -> (
                print_string "Essai ";
                print_int n;
                print_string " : ";
                print_list prop;
                print_int bp;
                print_string " pion(s) bien place(s), ";
                print_int mp;
                print_string " pion(s) mal place(s).";
                print_newline();
                aux (n + 1) (elagage prop ind l))
      )
  in aux 1 l;;

(* Retourne l'entier correspondant a la couleur donnee sous forme de string *)

let couleur s =
  match s with
  |"Rouge" -> Rouge 
  |"Bleu" -> Bleu 
  |"Vert" -> Vert 
  |"Noir" -> Noir 
  |"Jaune" -> Jaune 
  |"Orange" -> Orange 
  |"Violet" -> Violet 
  |"Blanc" -> Blanc;;

(* Lance une partie de Mastermind en ligne de commande *)

let main =
  print_string "MASTERMIND";
  print_newline();
  print_string "Entrez votre combinaison :";
  print_newline();
  let c1 = couleur (read_line()) in
  let c2 = couleur (read_line()) in
  let c3 = couleur (read_line()) in
  let c4 = couleur (read_line()) in
  let c5 = couleur (read_line()) in
  let sol = [c1; c2; c3; c4; c5] in
    print_string "Entrez la difficulte de la partie (1, 2 ou 3) :";
    print_newline();
    let diff = read_int() in
      if diff = 1 then mastermind sol 0 listeSR
      else if diff = 2 then mastermind sol 0 listeComplete
      else (
        print_string "Entrez le nombre d'essais maximum :";
        print_newline();
        let n = read_int() in mastermind sol n listeComplete);
      print_string "Au revoir.";;
