let suppression e l =
  let rec aux res l2 = match l2 with
      [] -> res
    | h::t -> if e = h then res @ t else aux (res @ [h]) t
  in aux [] l

let pions_communs l1 l2 =
  let rec aux l1 l2 = match l1 with
      [] -> l2
    | h::t -> aux t (suppression h l2)
  in length l2 - length (aux l1 l2);;

let pions_bien_places l1 l2 =
  let rec aux n l1 l2 = match l1,l2 with
    | [],_ -> n
    |_,[]->n
    | h1::t1,h2::t2 -> if h1 = h2 then aux (n + 1) t1 t2 else aux n t1 t2
  in aux 0 l1 l2

let indications l1 l2 =
  let bp = pions_bien_places l1 l2 in
  let mp = (pions_communs l1 l2) - bp in (bp, mp)

let elagage comb ind l =
  let rec aux l1 l2 = match l2 with
      [] -> l1
    | h::t -> if (indications comb h) = ind
        then aux (l1 @ [h]) t
        else aux l1 t
  in aux [] l
