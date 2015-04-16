
let rec construire_ListR taille listC=
  let rec construire_aux list= 
    match list with 
    |[]->[]
    |h::t->(Rouge::h)::(Bleu::h)::(Vert::h)::(Noir::h)::(Jaune::h)::(Orange::h)::(Violet::h)::(Blanc::h)::(construire_aux t)
  in match taille with 
  |1 -> listC
  |_-> if taille>=1 then
      construire_aux (construire_ListR (taille-1) listC)
    else 
      failwith "Erreur taille trop petite"

let rec compList list =
  match list with
  | []->true
  |h::t-> not(mem h t) && compList t 

let rec  construire_ListSR list comp =
  let rec aux l res=
    match l with 
  |[]->res
  |h::t->if not(comp h) then
      aux t res
    else
      aux t (h::res)
  in aux list []


