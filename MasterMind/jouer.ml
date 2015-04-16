let jouer  nbcoup liste =
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
      with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;;
