
type 'a pile = {
  
	contenu : 'a list;
	taille : int;
  
}

val newPile : 'a pile
val popPile : 'a pile -> 'a * 'a pile
val pushPile : 'a pile -> 'a -> 'a pile
val peekPile : 'a pile -> 'a option
val equalsPile : 'a pile -> 'a pile -> bool
val ofList : 'a list -> 'a pile
