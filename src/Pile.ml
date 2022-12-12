
type 'a pile = {
  
	contenu : 'a list;
	taille : int;
  
}

let newPile =
  {contenu = []; taille = 0}
;;

(* Renvoie une paire contenant l'élément pop et la nouvelle pile sans cet élément *)
let popPile (p : 'a pile) =
  match p.contenu with
  | [] -> None
  | e::c2 -> Some (e, {contenu = c2; taille = p.taille-1})
;;

let pushPile (p : 'a pile) (e : 'a) =
  {contenu = e::p.contenu; taille = p.taille+1}
;;

let peekPile (p : 'a pile) =
  match p.contenu with
  | [] -> None
  | e::c2 -> Some e
;;