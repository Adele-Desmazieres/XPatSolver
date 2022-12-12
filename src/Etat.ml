open Format

type coup = {
  
  destination : string; (* carte : son numéro 0 à 51 / col vide : "V" / registre : "T" *)
  source : Card.card; (* carte source *)

}

type etat = {
  
	historique : coup list; (* Liste des coups joués jusqu'à l'obtention de cet état *)
  
	colonnes : (Card.card Pile.pile) list; (* Colonnes à implémenter FArray de Piles *)
	depot : Card.card list; (* Contient une liste des dernieres cartes ajoutées *)
	registre : Card.card list; (* Liste ordonnée des cartes au registre *)
	nbColMax : int;
	nbRegMax : int
  
}

let printList l =
  List.iter (
    fun c -> 
      printf "%s " (Card.to_string c)
      (*else printf "vide ") *)
  )
  l
;;

let printCardList (l : Card.card list) (name : string) =
  printf "%s : " name;
  printList l;
  printf "\n"
;;  

let rec printColonnes (l : (Card.card Pile.pile) list) (num : int) =
  match l with
  |[] -> printf "\n"
  |pile::l2 -> 
    printCardList pile.contenu ("c" ^ (string_of_int num));
    printColonnes l2 (num+1)
;; 


let printEtat (etat : etat) =
  printCardList etat.depot "depot";
  printCardList etat.registre "registre";
  printColonnes etat.colonnes 1
;;

