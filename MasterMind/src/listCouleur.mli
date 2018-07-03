module ListCouleur :
  sig
    type couleur =
        Rouge
      | Bleu
      | Vert
      | Noir
      | Jaune
      | Orange
      | Violet
      | Blanc
    val listeCouleur : couleur list list
    val construire_ListR : int -> couleur list list -> couleur list list
    val compList : 'a list -> bool
    val construire_ListSR : 'a list -> ('a -> bool) -> 'a list
    val suppression : 'a -> 'a list -> 'a list
    val pions_communs : 'a list -> 'a list -> int
    val pions_bien_places : 'a list -> 'a list -> int
    val indications : 'a list -> 'a list -> int * int
    val elagage : 'a list -> int * int -> 'a list list -> 'a list list
    val print_list : couleur list -> unit
  end
