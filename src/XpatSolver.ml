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
  (* TODO *)
  match coup.destination with
  | "V" -> (estAccessibleGeneral etat coup.source) && (possedeColonneVide etat) && (respecteColonneVide coup.source regles)
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
  
  let newDepot = construireNouvDepot etat.depot (snd (newColsAndCardsMoved)) [] in 
  
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

let normaliser (etat : etat) =
  let rec normalRec e1 e2 =
    if e1.depot = e2.depot then e2 else normalRec e2 (normaliserGeneral e2);
  in normalRec etat (normaliserGeneral etat)
;; 

(* Enlève la carte card des colonnes *)
let rec enleverDeCol cols newCol (carte : Card.card) =
  match cols with
  | [] -> newCol
  | p :: l ->
    let pop = Pile.popPile p in
    match pop with
    | None -> enleverDeCol l (p :: newCol) carte
    | Some (card, pile) ->
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
  {depot = etat.depot; colonnes = newCols; registre = newreg; historique = etat.historique; nbColMax = etat.nbColMax; nbRegMax = etat.nbRegMax}
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
  
  {historique = etat.historique; 
	colonnes = newCols; 
	depot = etat.depot; 
	registre = registreMoinsCarte;
	nbColMax = etat.nbColMax;
	nbRegMax = etat.nbRegMax}
;;


(* Met à jour l'état actuel, i.e applique le coup à l'état actuel (à appeler seulement si le coup est légal...)*)
let miseAJourPartie (coup : coup) (etat : etat) =
  match coup.destination with 
  | "T" -> mettreAuRegistre coup etat
  | "V" -> deplacerDansCol coup etat (-1)
  | cardstring -> let cardnum = int_of_string cardstring in deplacerDansCol coup etat cardnum
;;

(* renvoie vrai si le dépot est rempli de rois *)
let rec conditionDeVictoire depot =
  match depot with
  | [] -> true
  | carte :: rest ->
    if (fst(carte) < 13) then false else conditionDeVictoire rest
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

let rec bouclePrincipale etatCourant regles coupsList iter =
  (* printEtat etatCourant; *)
  match coupsList with 
  | [] -> if conditionDeVictoire etatCourant.depot
    then (print_string "SUCCES" ; print_newline ();  exit 0)
    else let () = print_string "ECHEC " in let () = print_int iter in exit 1
  | coup :: coupsRestants ->
    if coupLegal coup regles etatCourant
      then let newEtat = miseAJourPartie coup etatCourant in 
          (*printEtat newEtat;*)
           let newEtatNormalise = normaliser newEtat in 
           (*printEtat newEtatNormalise;*)
           bouclePrincipale newEtatNormalise regles coupsRestants (iter + 1)
      else let () = print_string "ECHEC " in let () = print_int iter in let () = print_newline () in exit 1
;;



(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  
  (*Printf.printf "\nVoici juste la permutation de graine %d:\n" conf.seed;*)
  
  (*printf "%s " (Card.to_string c)*)
  (*List.iter (fun n -> printf "%s " (Card.to_string (Card.of_num n))) permut;*)
  
  (*print_newline ();
  print_newline ();
  (*List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut; *)
  print_newline ();*)
  
  
  let regles = definirRegles conf in
  let paquet = List.rev (permutToCardList permut []) in
  let etat1 = construireEtatInit conf regles paquet in
  (*let () = printEtat etat1 in*)
  
  let etat2 = normaliser etat1 in
  (*printEtat etat2;*)
  
  (*printf "%b " (coupLegal {source = Card.of_num 6; destination = "T"} regles etat2);*)
  let etat3 = miseAJourPartie {source = Card.of_num 6; destination = "T"} etat2 in 
  let etat3 = normaliser etat3 in
  (*printEtat etat3;
  
  printf "Int of string de 38 %d \n" (int_of_string "38");
  printf "%b " (coupLegal {source = Card.of_num 33; destination = "38"} regles etat3);*)
  let etat4 = miseAJourPartie {source = Card.of_num 33; destination = "38"} etat3 in
  let etat4 = normaliser etat4 in 
  (*printEtat etat4*)
  match conf.mode with
  | Search (s) -> exit 0
  | Check (s) -> 
    let listeCoups = stringListToCoups (lireFichier s) [] in
    bouclePrincipale etat2 regles listeCoups 1
  
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
