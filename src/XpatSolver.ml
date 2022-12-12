open Format (* printf pour l'affichage *)
open XpatLib.Etat
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


type enchainementCouleur = Alternee | Identique | Any 

type regles = {
  capaciteRegistre : int;
  nbrColonnes : int;
  distributionCartes : int list; (* Comment sont distribuées initialement les cartes e.g. [7;6;7;6;7;6;7;6] pour FreeCell *)
  carteSurColonneVide : (Card.rank option) option; (* Quel est le rang de carte autorisé sur une colonne vide : None -> Aucun, Some (n) -> rang n, Some (None) -> Tous *)
  enchainement : enchainementCouleur;
}


(* Faire en sorte que regles contiennent toutes les tailles de colonnes, dont les vides *)

(* Construit l'état initial de la partie : place les cartes dans les colonnes (et registres si nécéssaire)
   repositionne bien les rois si Baker's Dozen *)
let construireEtatInit (conf : config) (regles : regles) (paquet : Card.card list) =

  let construireColonnes =
    (*let paquetPile = listToPile paquet in*)
    
    let rec oneColonneInit (nbToAdd : int) (col : Card.card Pile.pile) (paquet : Card.card list) =
      if (nbToAdd = 0) then (col, paquet)
      else (* let (carte, paquet2) = popPile paquet in*)
      match paquet with 
      | [] -> failwith "Paquet vide, distribution impossible. "
      | carte::paquet2 -> oneColonneInit (nbToAdd-1) (Pile.pushPile col carte) paquet2
    in 
    
    let rec aux (cols : Card.card Pile.pile list) (colDistrib : int list) (paquet : Card.card list) =
      match colDistrib with
      | [] -> (cols, paquet)
      | nbCartes::colDistrib2 -> 
        let (col, paq2) = oneColonneInit nbCartes Pile.newPile paquet in 
        aux (col::cols) colDistrib2 paq2
      
      (*oneColonneInit regles.distributionCartes.get(x) newPile*)
    in
    aux [] regles.distributionCartes paquet
    
  in
  
  let (colonnes, paquet2) = construireColonnes in
  
  {
  colonnes = colonnes;
  depot = [(0, Coeur); (0, Carreau); (0, Trefle); (0, Pique)];
  registre = paquet2; (* TODO : vérifier si ca passe de faire ca comme ca *)
  historique = [];
  nbColMax = regles.nbrColonnes;
  nbRegMax = regles.capaciteRegistre;
  }
;;

(* Renvoie vrai si la carte source peut être posée sur la carte dest selon les règles d'enchaînement *)
let respecteEnchainement 
((rank1, suit1) : Card.card) 
((rank2, suit2) : Card.card) 
(enchainement : enchainementCouleur) =
  match enchainement with
  | Any -> (rank1 = rank2 - 1)
  | Identique -> ((rank1 = rank2 - 1) && (suit1 = suit2))
  | Alternee ->
    match suit2 with
    | Trefle | Pique -> ((suit1 = Carreau || suit1 = Coeur) && (rank1 = rank2 - 1))
    | Carreau | Coeur -> ((suit1 = Trefle || suit1 = Pique) && (rank1 = rank2 - 1))
;;


(* Renvoie vrai si la carte représentée par dest est au sommet d'une des colonnes de l'état etat *)
let estAccessibleSurColonne (etat : etat) (dest : int) =
  let rec colSearcher colonnes dest =
    match colonnes with
    | [] -> false
    | col :: restantes ->
      let carteSurPile = Pile.peekPile col
      in match carteSurPile with
      | None -> colSearcher restantes dest
      | Some x -> if (Card.to_num x) = dest then true else colSearcher restantes dest
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
      if p.Pile.taille = 0 then true else possedePileVide restantes
  in possedePileVide etat.colonnes
;;

(* Renvoie vrai si le coup est légal par rapport aux règles et à l'état courant *)
let coupLegal (coup : coup) (regles : regles) (etat : etat) =
  (* TODO *)
  match coup.destination with
  | "V" -> (possedeColonneVide etat) && (estAccessibleGeneral etat coup.source) 
  | "T" -> ((List.length etat.registre) < regles.capaciteRegistre) && (estAccessibleGeneral etat coup.source)
  | x ->
    let input = int_of_string x in
    (estAccessibleSurColonne etat input && respecteEnchainement coup.source (Card.of_num input) regles.enchainement && (estAccessibleGeneral etat coup.source))
;; 

(* Construit une liste des cartes qui sont recherchées pour être ajoutées au dépot *)
let getCartesPourDepot (depot : Card.card list) =
  List.map (fun card -> if fst(card) = 13 then card else (fst(card)+1, snd(card))) depot
;; 

(* Construit le nouveau dépot selon les cartes bougées dedans (cardsMoved) *)
let rec construireNouvDepot (depot : Card.card list) (cardsMoved : Card.card list) nouv =
  match depot with
  | [] -> nouv @ cardsMoved
  | card :: restCards ->
    if List.for_all (fun x -> snd(x) <> snd(card)) cardsMoved then construireNouvDepot restCards cardsMoved (card :: nouv)
    else construireNouvDepot restCards cardsMoved nouv
;;


(* Normalise l'état actuel, i.e. mets les cartes qui peuvent aller au dépôt, dans le dépôt *)

let normaliserColonnes (etat : etat) =
  let wanted = getCartesPourDepot etat.depot in
  printCardList wanted "wanted";
  let rec normaliseCol cols newCol cardsMoved =
    match cols with
    | [] -> (newCol, cardsMoved)
    | p :: l ->
      let pop = Pile.popPile p in
      match pop with
      | None -> normaliseCol l (p :: newCol) cardsMoved
      | Some (card, pile) ->
        if (List.mem card wanted) = true then normaliseCol l (pile :: newCol) (card :: cardsMoved)
        else normaliseCol l (p :: newCol) cardsMoved 
  in let newColsAndCardsMoved =  normaliseCol etat.colonnes [] [] in
  printCardList (snd newColsAndCardsMoved) "to move";
  printColonnes (fst newColsAndCardsMoved) 0;
  
  let newDepot = construireNouvDepot etat.depot (snd (newColsAndCardsMoved)) [] in 
  printCardList newDepot "newDepot";
  
  let newCols = fst (newColsAndCardsMoved) in 
  (* TODO supprimer les colonnes vides de la liste *)
  { depot = newDepot; colonnes = newCols; registre = etat.registre; historique = etat.historique; 
  nbColMax = etat.nbColMax;
  nbRegMax = etat.nbRegMax;
  } 
;;

let normaliserRegistre (etat : etat) =
  let wanted = getCartesPourDepot etat.depot in 
  let rec normaliseReg registre newReg cardsMoved =
    match registre with
    | [] -> (newReg, cardsMoved)
    | carte :: restant ->
        if (List.mem carte wanted) = true then normaliseReg restant newReg (carte :: cardsMoved)
        else normaliseReg restant (carte :: newReg) cardsMoved
  in let newRegistreAndCardsMoved = normaliseReg etat.registre [] [] in 
  let newDepot = construireNouvDepot etat.depot (snd (newRegistreAndCardsMoved)) [] in
  let newRegistre = fst (newRegistreAndCardsMoved) in 
  { depot = newDepot; colonnes = etat.colonnes; registre = newRegistre; historique = etat.historique;
  nbColMax = etat.nbColMax;
  nbRegMax = etat.nbRegMax;
  } 
;;

let normaliserGeneral (etat : etat) =
  let etatColonne = normaliserColonnes etat in 
  normaliserRegistre etatColonne
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
    {capaciteRegistre = 4; nbrColonnes = 10; distributionCartes = [5;5;5;5;5;5;5;5;5;5]; carteSurColonneVide = Some (Some 13); enchainement = Identique}
  | Midnight ->
    {capaciteRegistre = 0; nbrColonnes = 18; distributionCartes = [3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;1]; carteSurColonneVide = None; enchainement = Identique}
  | Baker ->
    {capaciteRegistre = 0; nbrColonnes = 13; distributionCartes = [4;4;4;4;4;4;4;4;4;4;4;4;4]; carteSurColonneVide = None; enchainement = Any}

let stringToCoup args = 
  match args with 
  | [a; b] ->
    Some { source = (Card.of_num (int_of_string a)) ; destination = b }
  | _ -> None


let lireFichier fichier regles etatInit =
  let file = open_in fichier in 
  let etat = etatInit in
  let iter = 0 in

  try
    while true do 
      let ligne = input_line file in 
      let arguments = String.split_on_char ' ' ligne in 
      let coupActuel = stringToCoup arguments in
      let iter = iter + 1 in
      match coupActuel with 
      | None -> 
        let () = print_string "ECHEC " in 
        let () = print_int iter in 
        exit 1
      | Some coupActuel -> 
      if coupLegal coupActuel regles etat then 
        let etat = miseAJourPartie coupActuel etat in ()
      else 
        let () = print_string "ECHEC " in 
        let () =print_int iter in 
        exit 1

    done
  with End_of_file ->
    close_in file
  ;;

let stringToCoup args = 
  match args with 
  | [a; b] ->
    Some { source = (Card.of_num (int_of_string a)) ; destination = b }
  | _ -> None


let lireFichier fichier regles etatInit =
  let file = open_in fichier in 
  let etat = etatInit in
  let iter = 0 in

  try
    while true do 
      let ligne = input_line file in 
      let arguments = String.split_on_char ' ' ligne in 
      let coupActuel = stringToCoup arguments in
      let iter = iter + 1 in
      match coupActuel with 
      | None -> 
        let () = print_string "ECHEC " in 
        let () = print_int iter in 
        exit 1
      | Some coupActuel -> 
      if coupLegal coupActuel regles etat then 
        let etat = miseAJourPartie coupActuel etat in ()
      else 
        let () = print_string "ECHEC " in 
        let () =print_int iter in 
        exit 1

    done
  with End_of_file ->
    close_in file
  ;;

(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  
  Printf.printf "\nVoici juste la permutation de graine %d:\n" conf.seed;
  
  (*printf "%s " (Card.to_string c)*)
  List.iter (fun n -> printf "%s " (Card.to_string (Card.of_num n))) permut;
  
  print_newline ();
  print_newline ();
  (*List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut; *)
  print_newline ();
  
  
  let regles = definirRegles conf in
  let paquet = List.rev (permutToCardList permut []) in
  let etat1 = construireEtatInit conf regles paquet in
  printEtat etat1; 
  
  let etat2 = normaliserGeneral etat1 in
  printEtat etat2;
  
  
  exit 0
;;

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
