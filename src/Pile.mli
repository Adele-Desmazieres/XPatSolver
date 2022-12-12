
type 'a pile = {
  
	contenu : 'a list;
	taille : int;
  
}

val newPile : 'a pile
val popPile : 'a pile -> ('a * 'a pile) option
val pushPile : 'a pile -> 'a -> 'a pile
val peekPile : 'a pile -> 'a option