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
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found
  
let print_conf (c : config) : (unit) =
  match config.game with
  | Freecell -> Printf.printf "FreeCell.%d\n" c.seed
  | Seahaven -> Printf.printf "Seahaven.%d\n" c.seed
  | Midnight -> Printf.printf "MidnightOil.%d\n" c.seed 
  | Baker -> Printf.printf "BakersDozen.%d\n" c.seed

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


(* Descendre les rois au fond d'une pile *)
let descendreCarte (p : Card.card Pile.pile) (rankSearch : int) : Card.card Pile.pile =
  Pile.{ 
    contenu = 
      (List.filter_map 
        (fun (rank, suit) -> if (rank <> rankSearch) then Some (rank, suit) else None) 
        p.contenu)
      @ 
      (List.filter_map 
        (fun (rank, suit) -> if (rank = rankSearch) then Some (rank, suit) else None)
        p.contenu); 
        
    taille = p.taille }
;;

  

(* Construit l'état initial de la partie : place les cartes dans les colonnes 
(et registres si nécessaire)
et repositionne bien les rois si Baker's Dozen *)
let construireEtatInit (conf : config) (regles : regles) (paquet : Card.card list) =
  
  (* Fonction auxiliaire construisant la liste des piles de cartes
  représentant l'ensemble des colonnes *)
  let construireColonnes =
    
    (* Renvoie une colonne remplie avec ses cartes initiales. 
    Prend en argument le nb de cartes à ajouter dans cette colonnes, 
    une pile accumulatrice de cartes dans la colonne, et le reste du paquet *)
    let rec oneColonneInit (nbToAdd : int) (col : Card.card Pile.pile) (paquet : Card.card list) =
      if (nbToAdd = 0) then 
        (* Descendre les rois dans les colonnes si c'est Bakers Dozen, 
        sinon renvoie la colonne prête et le reste du paquet de cartes non-distribuées *)
        if (config.game = Baker) then ( (descendreCarte col 13), paquet)
        else (col, paquet)
        
      else (* Sinon pousser successivement les cartes du paquet sur la colonne *)
      match paquet with 
      | [] -> failwith "Paquet vide, distribution impossible. "
      | carte::paquet2 -> oneColonneInit (nbToAdd-1) (Pile.pushPile col carte) paquet2
    in 
    
    (* Renvoie l'ensemble des colonnes remplies de cartes. *)
    let rec aux (cols : Card.card Pile.pile list) (colDistrib : int list) (paquet : Card.card list) =
      match colDistrib with
      | [] -> (cols, paquet)
      | nbCartes::colDistrib2 -> 
        let (col, paq2) = oneColonneInit nbCartes Pile.newPile paquet in 
        aux (col::cols) colDistrib2 paq2
      
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
  score = 0;
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


(* Renvoie une liste de toutes les cartes au sommet des colonnes *)
let getCartesSommets (etat : etat) =
  let rec colSearcher colonnes ret =
    match colonnes with
    | [] -> ret
    | col :: restantes ->
      let carteSurPile = Pile.peekPile col
      in match carteSurPile with
      | None -> colSearcher restantes ret
      | Some x -> colSearcher restantes (x :: ret)
  in colSearcher etat.colonnes []
;;

let estAccessibleSurColonne etat dest =
  List.mem (Card.of_num(dest)) (getCartesSommets etat)
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

(* Renvoie vrai si la carte src peut être placée sur une colonne vide selon les règles du jeu *)
let respecteColonneVide (src : Card.card) (regles : regles) =
  match regles.carteSurColonneVide with
  | None -> false
  | Some x ->
    match x with
    | None -> true
    | Some rang -> (rang = fst(src))

(* Renvoie vrai si le coup est légal par rapport aux règles et à l'état courant *)
let coupLegal (coup : coup) (regles : regles) (etat : etat) =
  match coup.destination with
  | "V" -> (estAccessibleGeneral etat coup.source) && (possedeColonneVide etat) && (respecteColonneVide coup.source regles)
  | "T" -> ((List.length etat.registre) < regles.capaciteRegistre) && (estAccessibleGeneral etat coup.source)
  | x ->
    let dest = int_of_string x in
    (estAccessibleSurColonne etat dest && 
    respecteEnchainement coup.source (Card.of_num dest) regles.enchainement && 
    (estAccessibleGeneral etat coup.source))
;; 

(* Renvoie vrai si la carte carte est seule sur sa colonne *)
let estSeuleSurColonne (carte : Card.card) etat =
  let rec parcoursCol cols (carte : Card.card) =
    match cols with
    | [] -> false
    | p :: rest ->
      if ((Pile.peekPile p) = Some carte) && (p.Pile.taille = 1) then true else parcoursCol rest carte
  in parcoursCol etat.colonnes carte
;;

(* Renvoie une liste de coups légaux pour une carte source, un état et des règles donnés *)
let coupLegalSrc (source : Card.card) (regles : regles) (etat : etat) =
  let rec legal_rec source dests regles etat ret =
    match dests with
    | [] -> ret
    | dest :: rest ->
      let coup = { source = source; destination = dest } in
      if coupLegal coup regles etat then legal_rec source rest regles etat (coup :: ret)
      else legal_rec source rest regles etat ret
  in let paquet = ("T" :: "V" :: ((List.map (fun x -> string_of_int (Card.to_num x)) (getCartesSommets etat))))
  in legal_rec source (List.rev paquet) regles etat []
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
  let rec normaliseCol cols newCol cardsMoved =
    match cols with
    | [] -> (newCol, cardsMoved)
    | p :: l ->
      if p.Pile.taille = 0 then normaliseCol l (p :: newCol) cardsMoved else
      let (card, pile) = Pile.popPile p in
        if (List.mem card wanted) = true then normaliseCol l (pile :: newCol) (card :: cardsMoved)
        else normaliseCol l (p :: newCol) cardsMoved 
  in let newColsAndCardsMoved =  normaliseCol etat.colonnes [] [] in
  
  let newDepot = construireNouvDepot etat.depot (snd (newColsAndCardsMoved)) [] in 
  let newScore = List.length (snd(newColsAndCardsMoved)) in
  
  let newCols = fst (newColsAndCardsMoved) in 
  { depot = newDepot; colonnes = newCols; registre = etat.registre; historique = etat.historique; 
  nbColMax = etat.nbColMax;
  nbRegMax = etat.nbRegMax;
  score = etat.score + newScore;
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
  let newScore = List.length (snd(newRegistreAndCardsMoved)) in
  let newRegistre = fst (newRegistreAndCardsMoved) in 
  { depot = newDepot; colonnes = etat.colonnes; registre = newRegistre; historique = etat.historique;
  nbColMax = etat.nbColMax;
  nbRegMax = etat.nbRegMax;
  score = etat.score + newScore;
  } 
;;

let trierColonnes (etat : etat) =
  List.sort (fun x y -> 
      if x.Pile.taille = y.Pile.taille then
        if x.Pile.taille = 0 then 0 else
          let pop1 = Pile.popPile x in let pop2 = Pile.popPile y in
          Stdlib.compare (Card.to_num (fst(pop1))) (Card.to_num (fst(pop2)))
      else Stdlib.compare x.Pile.taille y.Pile.taille
            ) etat.colonnes
;;

let normaliserGeneral (etat : etat) =
  let etatColonne = normaliserColonnes etat in 
  normaliserRegistre etatColonne
;;

let normaliser (etat : etat) =
  let rec normalRec e1 e2 =
    if e1.depot = e2.depot then e2 else normalRec e2 (normaliserGeneral e2);
  in let etatNormalise = normalRec etat (normaliserGeneral etat)
  in {
    colonnes = trierColonnes (etatNormalise);
    historique = etatNormalise.historique;
    score = etatNormalise.score;
    depot = etatNormalise.depot;
    nbRegMax = etatNormalise.nbRegMax;
    nbColMax = etatNormalise.nbColMax;
    registre = List.sort (fun x y -> Stdlib.compare (Card.to_num x) (Card.to_num y) ) etatNormalise.registre;
  } 
;; 

(* Enlève la carte card des colonnes *)
let rec enleverDeCol cols newCol (carte : Card.card) =
  match cols with
  | [] -> newCol
  | p :: l ->
    if p.Pile.taille = 0 then enleverDeCol l (p :: newCol) carte else
    let (card, pile) = Pile.popPile p in
      if card = carte then enleverDeCol l (pile :: newCol) carte
      else enleverDeCol l (p :: newCol) carte
;;

(* enlève la carte card du registre *)
let rec enleverDeRegistre reg newReg (carte : Card.card)  =
  match reg with
  | [] -> newReg
  | x :: rest -> if x = carte then enleverDeRegistre rest newReg carte else enleverDeRegistre rest (x::newReg) carte
;;

(* enlève la carte src des colonnes et la met au registre *)
let mettreAuRegistre (coup : coup) (etat : etat) =
  if (estDansLeRegistre coup.source etat) then etat else
  let newCols = enleverDeCol etat.colonnes [] coup.source in 
  let newreg = coup.source::etat.registre in 
  {depot = etat.depot; colonnes = newCols; registre = newreg; historique = (coup :: etat.historique); nbColMax = etat.nbColMax; nbRegMax = etat.nbRegMax; score = etat.score}
;;

let rec mettreDansColVideRec oldCols newCols cardToAdd =
  match oldCols with
  | [] -> newCols
  | col::colsRestantes -> match (Pile.peekPile col) with
    | None -> colsRestantes @ ((Pile.pushPile col cardToAdd)::newCols)
    | Some card -> mettreDansColVideRec colsRestantes (col::newCols) cardToAdd
;;

let rec mettreDansColRec oldCols newCols cardToAdd cardDestination =
  match oldCols with
  | [] -> newCols
  | col::colsRestantes -> match (Pile.peekPile col) with
    | None -> mettreDansColRec colsRestantes (col::newCols) cardToAdd cardDestination
    | Some card -> if card = cardDestination
      then mettreDansColRec colsRestantes ((Pile.pushPile col cardToAdd)::newCols) cardToAdd cardDestination
      else mettreDansColRec colsRestantes (col::newCols) cardToAdd cardDestination
;;

(* déplace la carte de n'importe ou vers une colonne, renvoie le nouvel état mis à jour *)
let deplacerDansCol (coup : coup) (etat : etat) (cardnum : int) =
  
  let registreMoinsCarte = if (estDansLeRegistre coup.source etat) 
  then enleverDeRegistre etat.registre [] (coup.source)
  else etat.registre in
  
  let colsMoinsCarte = if (estAccessibleSurColonne etat (Card.to_num coup.source))
  then enleverDeCol etat.colonnes [] (coup.source)
  else etat.colonnes in
  (*print_string (Card.to_string (coup.source));
  print_newline ();
  print_string (Card.to_string (Card.of_num(cardnum)));
  print_newline ();*)
    
  let newCols = if (0 <= cardnum && cardnum <= 51)
  then mettreDansColRec     colsMoinsCarte [] coup.source (Card.of_num cardnum)
  else mettreDansColVideRec colsMoinsCarte [] coup.source in
  
  {historique = (coup :: etat.historique); 
	colonnes = newCols; 
	depot = etat.depot; 
	registre = registreMoinsCarte;
	nbColMax = etat.nbColMax;
	nbRegMax = etat.nbRegMax;
  score = etat.score}
;;


(* Met à jour l'état actuel, i.e applique le coup à l'état actuel (à appeler seulement si le coup est légal...)*)
let miseAJourPartie (coup : coup) (etat : etat) =
  match coup.destination with 
  | "T" -> mettreAuRegistre coup etat
  | "V" -> deplacerDansCol coup etat (-1)
  | cardstring -> let cardnum = int_of_string cardstring in deplacerDansCol coup etat cardnum
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


let lireFichier fichier =
  let file = open_in fichier in
  let rec lecture file ret =
    try let line = input_line file in lecture file (line::ret) with
    End_of_file -> ret 
  in lecture file []
;;

let ecrireCoupsDansFichier coups fichier =
  let file = open_out fichier in 
  let rec ecriture file coups = 
    match coups with 
    |[] -> ()
    | coup :: rest -> let () = Printf.fprintf file "%s %s\n" (string_of_int (Card.to_num(coup.source))) coup.destination
      in ecriture file rest 
  in ecriture file (List.rev coups) 
;;

let stringToCoup args = 
  match args with 
  | [a; b] ->
     { source = (Card.of_num (int_of_string a)) ; destination = b }
  | _ -> failwith "Wrong coup"

let rec stringListToCoups list ret =
  match list with
  | [] -> ret
  | x :: rest -> stringListToCoups rest (stringToCoup (String.split_on_char ' ' x) :: ret)
;;

(* PARTIE 2.2 : recherche de solutions *)

let getEtatsLegaux (source : Card.card) (etatInit : etat) (regles : regles) : etat list =
  
  (* Renvoie la liste des cartes sur lequel le déplacement serait légal, et s'il y a une col vide *)
  let rec parcourirCols (cols : (Card.card Pile.pile) list) (cardColLegal : Card.card list) (hasColVide : bool) : (Card.card list * bool) = 
    match cols with
    | [] -> (cardColLegal, hasColVide)
    | pile::tail -> 
      if pile.Pile.taille = 0 then parcourirCols tail cardColLegal true
      else let dest, pileTail = Pile.popPile pile in 
      if respecteEnchainement source dest regles.enchainement
        then parcourirCols tail (dest::cardColLegal) hasColVide
        else parcourirCols tail cardColLegal hasColVide
  in
  
  let (cardColLegal, hasColVide) = parcourirCols etatInit.colonnes [] false in
  
  (* Transforme la liste de cartes destinations en liste d'états légaux normalisés *)
  let etatsLegaux = 
    List.map 
    (
      fun dest -> normaliser
        (miseAJourPartie ({source = source; destination = string_of_int (Card.to_num dest)}) etatInit)
    ) 
    cardColLegal
  in
  
  (* Renvoie l'état correspondant à la carte déplacée dans une col vide, ou None si aucune dispo *)
  let getEtatVide : etat option = 
    if not hasColVide then None else
    match regles.carteSurColonneVide with
    | None -> None
    | Some(rangOption) ->
      match rangOption with
      | None -> Some (normaliser (miseAJourPartie ({source=source; destination="V"}) etatInit))
      | Some(rangValideReel) -> 
        if (fst source) = rangValideReel  
        then Some (normaliser (miseAJourPartie ({source=source; destination="V"}) etatInit))
        else None
  in
  
  (* Renvoie l'état avec la carte déplacée dans le registre, ou None si pas la place *)
  let getEtatRegistre : etat option =
    if (List.length etatInit.registre) < regles.capaciteRegistre 
    then Some (normaliser (miseAJourPartie ({source=source; destination="T"}) etatInit))
    else None
  in
  
  let etatVide = getEtatVide in 
  let etatsLegaux = (match etatVide with
  | None -> etatsLegaux
  | Some(e) -> e::etatsLegaux) in
  
  let etatRegistre = getEtatRegistre in
  let etatsLegaux = (match etatRegistre with
  | None -> etatsLegaux
  | Some(e) -> e::etatsLegaux) in
  
  etatsLegaux
;;

let creerListeEtatsPossibles (etatInit : etat) (regles : regles) : etat list =

  let cartesAccessibles = getCartesSommets etatInit in
  let cartesAccessibles = List.rev_append etatInit.registre cartesAccessibles in
  
  let rec parcourirCartes (cartesAccessibles : Card.card list) (acc : etat list) : etat list =
    match cartesAccessibles with
    | [] -> acc
    | carte::tail -> parcourirCartes tail (List.rev_append (getEtatsLegaux carte etatInit regles) acc)
  in
  
  parcourirCartes cartesAccessibles []
;;



(* PARTIE 2.3  : ensembles d'états *)

let compRegistres etat1 etat2 =
  Stdlib.compare etat1.registre etat2.registre
;;

let compColonnes etat1 etat2 =
  Stdlib.compare etat1.colonnes etat2.colonnes
;; 

let compEtat etat1 etat2 =
  let comp1 = (compRegistres etat1 etat2) in
  if (comp1 = 0) then compColonnes etat1 etat2 else comp1
;;
  
module States = Set.Make (struct type t = etat let compare = compEtat end)

let rec bouclePrincipaleVerif etatCourant regles coupsList iter =
  (*printEtat etatCourant;*) 
  match coupsList with 
  | [] -> if etatCourant.score = 52
    then (print_string "SUCCES" ; print_newline ();  exit 0)
    else let () = print_string "ECHEC " in let () = print_int iter in exit 1
  | coup :: coupsRestants ->
    if coupLegal coup regles etatCourant
      then let newEtat = miseAJourPartie coup etatCourant in 
          (*printEtat newEtat;*)
           let newEtatNormalise = normaliser newEtat in 
           (*printEtat newEtatNormalise;*)
           bouclePrincipaleVerif newEtatNormalise regles coupsRestants (iter + 1)
      else let () = print_string "ECHEC " in let () = print_int iter in let () = print_newline () in exit 1
;;


let dfs2 
  (etatCourant : etat) 
  (etatsParcourus : States.t) 
  (regles : regles) 
  (aFiltrer : bool) 
  : (States.t * etat option) =
  
  let rec dfsRec 
    (etatsParcourus : States.t) 
    (pile : etat list) 
    (addCondition : etat -> bool) 
    (aFiltrer : bool) 
    (meilleurScore : int)
    : (etat option * States.t * etat list) =
    (* si la pile est vide, c'est qu'il n'y a pas d'état final gagnant *)
    if List.length pile = 0 then (None, etatsParcourus, pile) else
      
    (* retirer l'état de la pile *)
    let etatCourant, pile = match pile with 
      | [] -> failwith "Pile vide"
      | e::tail -> e, tail
    in
    
    (*
    let () = if States.cardinal etatsParcourus mod 10000 = 0 then
      (Printf.printf "\nDFS2 nbr états en mémoire : %d" (States.cardinal etatsParcourus);
      Printf.printf "\nnbr états dans la pile : %d" (List.length pile);
      Printf.printf "\nScore actuel : %d" etatCourant.score;
      Printf.printf "\nScore du dernier filtrage : %d" meilleurScore;
      print_newline ();)
    else () in 
    *)
    
    (* condition de victoire *)
    if etatCourant.score = 52 then (Some etatCourant, etatsParcourus, pile) else
    
    let distanceDoubli = 8 in 
      
    (* condition de recherche non exhaustive *)
    let pile, etatsParcourus, addCondition, meilleurScore = 
    if etatCourant.score > distanceDoubli && etatCourant.score > meilleurScore+1 && aFiltrer && (States.cardinal etatsParcourus) >= 20000
      then 
      (*let () = (Printf.printf "\nScore : %d\nScore max : %d\nFiltrage de la pile et la mémoire...\n" etatCourant.score meilleurScore) in
      let () = print_newline () in*)
      (List.filter (fun etatX -> etatX.score >= etatCourant.score-distanceDoubli) pile), 
      (States.filter (fun etatX -> etatX.score >= etatCourant.score-distanceDoubli) etatsParcourus),
      (fun x -> x.score >= etatCourant.score-distanceDoubli),
      etatCourant.score
    else pile, etatsParcourus, addCondition, meilleurScore
    in
    
    (* itérer sur la liste d'états atteignables depuis cet état,
       les ajouter à la pile et à l'ensemble d'état parcourus *)
    let rec parcourirEtats pile etatsParcourus etatsAtteignables addCondition =
      match etatsAtteignables with
      | [] -> pile, etatsParcourus
      | x::tail -> if (not (States.mem x etatsParcourus)) && (addCondition x)
        then parcourirEtats (x::pile) (States.add x etatsParcourus) tail addCondition
        else parcourirEtats pile etatsParcourus tail addCondition
    in
    
    let pile, etatsParcourus = 
      parcourirEtats pile etatsParcourus (creerListeEtatsPossibles etatCourant regles) addCondition in
      
    let pile = List.fast_sort (fun x y -> -1 * Stdlib.compare (x.score) (y.score)) pile in
    dfsRec etatsParcourus pile addCondition aFiltrer meilleurScore
  in
  
  let p = [etatCourant] in
  let etatFinal, etatsParcourus, pile = 
    dfsRec (States.add etatCourant etatsParcourus) p (fun x -> true) aFiltrer 0 in
  (*Printf.printf "Etats de la mémoire finale : %d\n" (States.cardinal etatsParcourus);*)
  (etatsParcourus, etatFinal)
;;


(* recherche non-exhaustive, si aucune solution alors recherche exhaustive *)
let bouclePrincipaleRecherche (etatInit : etat) regles (fichier : string) =
  let (memoire, etatTrouve) = dfs2 etatInit States.empty regles true in 
  match etatTrouve with
  | Some etatFinal -> let () = ecrireCoupsDansFichier etatFinal.historique fichier in
    printf "SUCCES"; exit 0
  | None -> let (mem, etatTrouve) = dfs2 etatInit States.empty regles false in 
            match etatTrouve with
            | None -> printf "INSOLUBLE"; exit 2
            | Some etatFinal-> let () = ecrireCoupsDansFichier etatFinal.historique fichier in
            printf "SUCCES"; exit 0



let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  (*
  print_newline ();
  print_conf conf;
  *)
  let regles = definirRegles conf in
  let paquet = List.rev (permutToCardList permut []) in
  let etat1 = normaliser (construireEtatInit conf regles paquet) in
  
  match conf.mode with
  | Search (s) -> bouclePrincipaleRecherche etat1 regles s
  | Check (s) -> 
    let listeCoups = stringListToCoups (lireFichier s) [] in
    bouclePrincipaleVerif etat1 regles listeCoups 1 
  
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
