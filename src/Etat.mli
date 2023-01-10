type coup = {
  
	destination : string; (* carte : son numéro 0 à 51 / col vide : "V" / registre : "T" *)
	source : Card.card; (* carte source *)

}

type etat = {
  
	historique : coup list; (* Liste des coups joués jusqu'à l'obtention de cet état *)
  
	colonnes : (Card.card Pile.pile) list; (* Colonnes à implémenter FArray de Piles *)
	depot : Card.card list; (* Contient une liste des dernieres cartes ajoutées *)
	registre : Card.card list; (* Liste ordonnée des cartes au registre *)
	nbRegMax : int;
	score : int
  
}

val printEtat : etat -> unit
val printCardList : Card.card list -> string -> unit
val printColonnes : (Card.card Pile.pile) list -> int -> unit