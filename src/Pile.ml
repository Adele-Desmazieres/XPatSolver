
type 'a pile = {
  
	contenu : 'a list;
	taille : int;
  
}

let newPile =
  {contenu = []; taille = 0}
;;

(* Renvoie une paire contenant l'élément pop et la nouvelle pile sans cet élément *)
let popPile (p : 'a pile) : ('a * 'a pile) =
  match p.contenu with
  | [] -> failwith "Pile vide"
  | e::c2 -> (e, {contenu = c2; taille = p.taille-1})
;;

let pushPile (p : 'a pile) (e : 'a) =
  {contenu = e::p.contenu; taille = p.taille+1}
;;

let peekPile (p : 'a pile) =
  match p.contenu with
  | [] -> None
  | e::c2 -> Some e
;;

let equalsPile (p1 : 'a pile) (p2 : 'a pile) = p1.contenu = p2.contenu;;

let ofList (l : 'a list) : 'a pile =
  {contenu = l; taille = List.length l}
;;
