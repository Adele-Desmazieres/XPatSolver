
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bc" -> Baker
  | _ -> raise Not_found

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

type coup = {
  
  destination : string; (* carte : son numéro 0 à 51 / col vide : "V" / registre : "T" *)
  source : Card.card; (* carte source *)

}

type enchainementCouleur = Alternee | Identique | Toutes 

type regles = {
  capaciteRegistre : int;
  nbrColonnes : int;
  distributionCartes : int list; (* Comment sont distribuées initialement les cartes e.g. [7;6;7;6;7;6;7;6] pour FreeCell *)
  carteSurColonneVide : (Card.rank option) option; (* Quel est le rang de carte autorisé sur une colonne vide : None -> Aucun, Some (n) -> rang n, Some (None) -> Tous *)
  enchainement : enchainementCouleur;
}

type 'a pile = {
  
  contenu : 'a list;
  taille : int;

}

type etat = {
 
  colonnes : (Card.card pile) list; (* Colonnes à implémenter en piles (LIFO) *)
  depot : Card.card list; (* Contient une liste des dernieres cartes ajoutées *)
  registre : Card.card list; (* A implémenter en Set *)
  historique : coup list;

}

let newPile =
  {contenu = []; taille = 0}
;;

(* Renvoie une paire contenant l'élément pop et la nouvelle pile sans cet élément *)
let popPile (p : 'a pile) =
  match p.contenu with
  | [] -> raise (PileError "Pile vide")
  | e::c2 -> (e, {contenu = c2; taille = p.taille-1})
;;

let pushPile (p : 'a pile) (e : 'a) =
  {contenu = e::p.contenu; taille = p.taille+1}
;;

let peekPile (p : 'a pile) =
  match p.contenu with
  | [] -> raise (PileError "Pile vide")
  | e::c2 -> e
;;


exception PileError of string ;;

(* Faire en sorte que regles contiennent toutes les tailles de colonnes, dont les vides *)

(* Construit l'état initial de la partie : place les cartes dans les colonnes (et registres si nécéssaire)
   repositionne bien les rois si Baker's Dozen *)
let construireEtatInit (conf : config) (regles : regles) (paquet : Card.card list) =

  let construireColonnes =
    (*let paquetPile = listToPile paquet in*)
    
    let rec oneColonneInit (nbToAdd : int) (col : Card.card pile) (paquet : Card.card list) =
      if (nbToAdd = 0) then (col, paquet)
      else (* let (carte, paquet2) = popPile paquet in*)
      match paquet with 
      | [] -> failwith "Paquet vide, distribution impossible. "
      | carte::paquet2 -> oneColonneInit (nbToAdd-1) (pushPile col carte) paquet2
    in 
    
    let rec aux (cols : Card.card pile list) (colDistrib : int list) (paquet : Card.card list) =
      match colDistrib with
      | [] -> (cols, paquet)
      | nbCartes::colDistrib2 -> 
        let (col, paq2) = oneColonneInit nbCartes newPile paquet in 
        aux (col::cols) colDistrib2 paq2
      
      (*oneColonneInit regles.distributionCartes.get(x) newPile*)
    in
    aux [] regles.distributionCartes paquet
    
  in
  
  let (colonnes, paquet2) = construireColonnes in
  
  {
  colonnes = colonnes;
  depot = [];
  registre = paquet2; (* TODO : vérifier si ca passe de faire ca comme ca *)
  historique = [];
  }
;;

(* Renvoie vrai si la carte source peut être posée sur la carte dest selon les règles d'enchaînement *)
let respecteEnchainement (src : Card.card) (dest : Card.card) (enchainement : enchainementCouleur) =
  match enchainement with
  | Identique -> ((src.rank = (dest.rank - 1)) && (src.suit = dest.suit))
  | Tous -> (src.rank = (dest.rank - 1))
  | Alternee ->
    match dest.suit with
    | Trefle | Pique -> ((src.suit = Carreau || src.suit = Coeur) && (src.rank = (dest.rank - 1)))
    | Carreau | Coeur -> ((src.suit = Trefle || src.suit = Pique) && (src.rank = (dest.rank - 1)))
;;

(* Renvoie vrai si la carte représentée par dest est au sommet d'une des colonnes de l'état etat *)
let estAccessibleSurColonne (etat : etat) (dest : int) =
  let rec colSearcher colonnes dest =
    match colonnes with
    | [] -> false
    | col :: restantes ->
      let carteSurPile = try (Card.of_num (peekPile col)) with
        PileError -> colSearcher restantes dest
      in if (Card.of_num carteSurPile) = dest then true else colSearcher restantes dest
  in colSearcher etat.colonnes dest 
;;

(* Renvoie vrai si la carte src est dans le registre de l'état courant *)
let estDansLeRegistre (src : Card.card) (etat : etat) =
  let rec registreSearcher reg src =
    match reg with
    | [] -> false
    | x :: r -> if (Card.to_num src) = (Card.to_num x) then true
    else registreSearcher r src
  in registreSearcher etat.registre src
;;

(* Renvoie vrai si la carte src est accessible depuis une colonne ou le registre *)
let estAccessibleGeneral (etat : etat) (src : Card.card) =
  (estAccessibleSurColonne etat (Card.to_num src)) || (estDansLeRegistre src etat)
;;

(* Renvoie vrai si l'etat courant possede au moins une colonne vide *)
let possedeColonneVide (etat : etat) =
  let rec possedePileVide colonnes =
    match colonnes with
    | [] -> false
    | p :: restantes ->
      if p.length = 0 then true else possedePileVide restantes
  in possedePileVide etat.colonnes
;;



(* Renvoie vrai si le coup est légal par rapport aux règles et à l'état courant *)
let coupLegal (coup : coup) (regles : regles) (etat : etat) =
  (* TODO *)
  match coup.destination with
  | "V" -> (possedeColonneVide etat) && (estAccessibleGeneral coup.source) 
  | "T" -> ((List.length etat.registre) < 4) && (estAccessibleGeneral coup.source)
  | x ->
    let val = int_of_string x in
    (estAccessibleSurColonne etat val && respecteEnchainement coup.source (Card.of_num val) && (estAccessibleGeneral coup.source))
;; 

(* Normalise l'état actuel, i.e. mets les cartes qui peuvent aller au dépôt, dans le dépôt *)
let normaliser (etat : etat) =
  (*  TODO  *) ()
;;

(* Met à jour l'état actuel, i.e applique le coup à l'état actuel (à appeler seulement si le coup est légal...)*)
let miseAJourPartie (coup : coup) (etat : etat) =
  (*    TODO    *) ()
;;

(* Transforme la permutation en liste de cartes : ATTENTION inverse l'ordre*)
let rec permutToCardList (permut : int list) (ret : Card.card list) =
  match permut with
  | [] -> ret
  | x :: l ->
    let ret = (Card.of_num x) :: ret in permutToCardList l ret

(* Renvoie un type règle qui correspond au jeu actuel *)
let definirRegles (conf : config) =
  match conf.game with
  | Freecell ->
    {capaciteRegistre = 4; nbrColonnes = 8; distributionCartes = [7;6;7;6;7;6;7;6]; carteSurColonneVide = Some (None); enchainement = Alternee;}
  | Seahaven ->
    {capaciteRegistre = 4; nbrColonnes = 10; distributionCartes = [5]; carteSurColonneVide = Some (Some 13); enchainement = Identique}
  | Midnight ->
    {capaciteRegistre = 0; nbrColonnes = 18; distributionCartes = [3]; carteSurColonneVide = None; enchainement = Identique}
  | Baker ->
    {capaciteRegistre = 0; nbrColonnes = 13; distributionCartes = [4]; carteSurColonneVide = None; enchainement = Toutes}

(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "\nVoici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  print_string "\nC'est tout pour l'instant. TODO: continuer...\n";
  let regles = definirRegles conf in
  let paquet = List.rev (permutToCardList permut []) in
  exit 0

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()
