open Format (* print pour debug *)

(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (between 1 and 999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).

*)

(* The numbers manipulated below will be in [0..randmax[ *)
let randmax = 1_000_000_000

(* Converting an integer n in [0..randmax[ to an integer in [0..limit[ *)
let reduce n limit =
  Int.(of_float (to_float n /. to_float randmax *. to_float limit))


(** DESCRIPTION DE L'ALGORITHME DE GENERATION DES PERMUTATIONS

a) Créer tout d'abord les 55 premières paires suivantes:
  * premières composantes : 0 pour la premiere paire,
    puis ajouter 21 modulo 55 à chaque fois
  * secondes composantes : graine, puis 1, puis les "différences"
    successives entre les deux dernières secondes composantes.
    Par "différence" entre a et b on entend
      - Ou bien (a-b) si b<=a
      - Ou bien (a-b+randmax) si a<b

b) Trier ces 55 paires par ordre croissant selon leurs premières composantes,
   puis séparer entre les 24 premières paires et les 31 suivantes.
   Pour les 31 paires, leurs secondes composantes sont à mettre dans
   une FIFO f1_init, dans cet ordre (voir `Fifo.of_list` documenté dans
   `Fifo.mli`). De même pour les 24 paires, leurs secondes composantes sont
   à mettre dans une FIFO f2_init, dans cet ordre.

c) Un *tirage* à partir de deux FIFO (f1,f2) consiste à prendre
   leurs premières valeurs respectives n1 et n2 (cf `Fifo.pop`),
   puis calculer la "différence" de n1 et n2 (comme auparavant),
   nommons-la d. Ce d est alors le résultat du tirage, associé
   à deux nouvelles FIFO constituées des restes des anciennes FIFO
   auxquelles on a rajouté respectivement n2 et d (cf `Fifo.push`).

d) On commence alors par faire 165 tirages successifs en partant
   de (f1_init,f2_init). Ces tirages servent juste à mélanger encore
   les FIFO qui nous servent d'état de notre générateur pseudo-aléatoire,
   les entiers issus de ces 165 premiers tirages ne sont pas considérés.

e) La fonction de tirage vue précédemment produit un entier dans
   {0..randmax{. Pour en déduire un entier dans {0..limit{ (ou limit est
   un entier positif quelconque), on utilisera alors la fonction `reduce`
   fournie plus haut.
   Les tirages suivants nous servent à créer la permutation voulue des
   52 cartes. On commence avec une liste des nombres successifs entre 0 et 51.
   Un tirage dans {0..52{ nous donne alors la position du dernier nombre
   à mettre dans notre permutation. On enlève alors le nombre à cette position
   dans la liste. Puis un tirage dans {0..51{ nous donne la position
   (dans la liste restante) de l'avant-dernier nombre de notre permutation.
   On continue ainsi à tirer des positions valides dans la liste résiduelle,
   puis à retirer les nombres à ces positions tirées pour les ajouter devant
   la permutation, jusqu'à épuisement de la liste. Le dernier nombre retiré
   de la liste donne donc la tête de la permutation.

   NB: /!\ la version initiale de ce commentaire donnait par erreur
   la permutation dans l'ordre inverse).

Un exemple complet de génération d'une permutation (pour la graine 1)
est maintenant donné dans le fichier XpatRandomExemple.ml, étape par étape.

*)

(* For now, we provide a shuffle function that can handle a few examples.
   This can be kept later for testing your implementation. *)


let shuffle_test = function
  | 1 ->
     [13;32;33;35;30;46;7;29;9;48;38;36;51;41;26;20;23;43;27;
      42;4;21;37;39;2;15;34;28;25;17;16;18;31;3;0;10;50;49;
      14;6;24;1;22;5;40;44;11;8;45;19;12;47]
  | 12 ->
     [44;9;28;35;8;5;3;4;11;25;43;2;27;1;24;40;17;41;47;18;
      10;34;39;7;36;29;15;19;30;37;48;45;0;21;12;46;22;13;16;
      33;31;38;23;6;14;49;26;50;20;32;42;51]
  | 123 ->
     [16;51;44;27;11;37;33;50;48;13;17;38;7;28;39;15;4;5;3;6;
      42;25;19;34;20;49;23;0;8;26;30;29;47;36;9;24;40;45;14;
      22;32;10;1;18;12;31;35;2;21;43;46;41]
  | 1234 ->
     [36;37;44;26;9;10;23;30;29;18;4;35;15;50;33;43;28;2;45;
      6;3;31;27;20;7;51;39;5;14;8;38;17;49;0;40;42;13;19;34;
      1;46;22;25;24;12;48;16;21;32;11;41;47]
  | 12345 ->
     [10;12;6;23;50;29;28;24;7;37;49;32;38;30;31;18;13;2;15;4;
      5;47;16;1;0;35;43;40;42;44;46;39;48;20;36;34;8;14;33;11;
      25;45;41;19;3;17;21;51;26;22;27;9]
  | 123456 ->
     [1;7;39;47;5;15;50;49;37;44;29;10;4;23;17;20;0;11;24;14;
      28;35;3;48;8;41;19;46;13;12;36;34;27;9;33;22;43;32;25;30;
      38;6;31;16;51;21;26;18;45;40;42;2]
  | 1234567 ->
     [19;17;31;6;4;14;9;36;35;30;39;40;50;48;42;37;12;3;25;1;
      43;27;5;20;10;51;11;44;46;38;16;22;26;23;21;28;15;7;47;
      13;18;29;32;0;49;34;8;45;24;33;2;41]
  | 22222 ->
     [43;17;21;40;42;47;0;35;23;18;11;29;41;10;45;7;15;25;13;
      51;6;12;33;24;8;34;50;2;30;28;37;3;4;39;49;31;32;14;44;
      22;46;48;9;1;36;5;27;26;38;20;16;19]
  | 222222 ->
     [42;48;16;9;22;21;45;12;40;44;29;31;24;27;33;38;14;15;49;
      37;0;26;10;1;47;4;50;34;23;8;3;2;19;32;13;43;51;6;39;35;
      18;30;11;7;46;17;20;5;41;36;25;28]
  | 2222222 ->
     [17;45;5;4;33;23;10;42;39;3;24;46;6;29;44;27;0;43;2;7;20;
      14;34;8;11;18;15;28;25;49;40;47;48;21;41;9;31;30;36;12;
      51;1;35;26;50;38;32;19;13;37;22;16]
  | 999_999_999 ->
     [22;1;0;21;20;44;23;43;38;11;4;2;19;27;36;9;49;7;18;14;
      46;10;25;35;39;48;51;40;33;13;42;16;32;50;24;47;26;6;34;
      45;5;3;41;15;12;31;17;28;8;29;30;37]
  | _ -> failwith "shuffle : unsupported number (TODO)"

(* crée une paire en suivant la suite décrire dans a) *)
let pairCreator (lastfst : int) (lastsnd : int) (last2snd : int) =
   let newSnd = 
      if (last2snd >= lastsnd) then (last2snd - lastsnd) 
      else (last2snd - lastsnd + randmax) in 
   ((lastfst + 21) mod 55, newSnd) 
;;

(* compare deux paires (a1, b1) (a2, b2), et renvoie 
   1  si a1 > a2
   -1 si a1 < a2 
   si a1 = a2 alors comparaison de b1 et b2 suivant le meme modele
*)
let comparaison paire1 paire2 =
   if (fst paire1 = fst paire2) then 
      if (snd paire1 = snd paire2) then 0
      else if (snd paire1 > snd paire2) then 1
      else -1
   else if (fst paire1 > fst paire2) then 1
   else -1
;;

(* crée une liste de paire triée qui suit les propriétés décrites dans a) et début de b) *)
let listOfPair (seed : int) =
   let ret = [(21, 1); (0, seed)] in
   
   let rec addPair l iter =
      if iter = 55 then l else
      let popLast = List.nth l 0 in
      let popLast2 = List.nth l 1 in
      let newPaire = pairCreator (fst popLast) (snd popLast) (snd popLast2) in
      (* printf "test : (%d, %d)\n%!" (fst newPaire) (snd newPaire); *)
      addPair (newPaire::l) (iter+1)
   in 
   
   List.sort comparaison (addPair ret 2) 
;;

(* debuggage affiche une liste d'entiers *)
let rec tmpPrintList l =
   match l with
   | [] -> printf "\n\n"
   | e::l2 -> printf "%d; " e; tmpPrintList l2 ;;

(* crée deux FIFO qui contiennent les deuxièmes composantes des paires de la liste l 
   en ayant une longueur de 24 et 31 respectivement *)
let queuePair (l : (int * int) list) = 
   let rec splitList (l : (int * int) list) (ret : int list * int list) (iter : int) =
      match l with
      | [] -> ret
      | pair :: l ->
         if iter < 24 then
            let ret = (snd (pair) :: fst (ret), snd (ret)) in splitList l ret (iter+1)
         else
            let ret = (fst (ret), snd (pair) :: snd (ret)) in splitList l ret (iter+1)
   in
   let splittedList = splitList l ([],[]) 0 in
   (*
   tmpPrintList (snd splittedList) ;
   tmpPrintList (fst splittedList) ; *)
   
   (* (f1_init, f2_init) *)
   (Fifo.of_list (List.rev (snd splittedList)), Fifo.of_list (List.rev (fst splittedList)))
;;

(* effectue un tirage comme décrit dans c) et renvoie les nouvelles FIFO mises à jour *)
let tirage (files : int Fifo.t * int Fifo.t) =
   let newFile1 = Fifo.pop (fst files) in
   let newFile2 = Fifo.pop (snd files) in
   let n1 : int = fst (newFile1) in
   let n2 : int = fst (newFile2) in
      
   let files = (snd (newFile1), snd (newFile2)) in
   
   let d = 
      if n2 <= n1 then n1 - n2
      else n1 - n2 + randmax
   in
      
   let tmp1 = Fifo.push n2 (fst files) in
   let tmp2 = Fifo.push d (snd files) in
   
   (* printf "n1 = %d, n2 = %d, d = %d\n" n1 n2 d; *)
   
   ((tmp1, tmp2), d)
;;

(* effectue 165 tirages (si iter est initialisé à 1) comme décrits dans la fonction ci-dessus *)
let rec tirageRepete (files : int Fifo.t * int Fifo.t) (iter : int) =
   if iter = 0 then files
      
   else let files = fst (tirage (files)) in 
   tirageRepete files (iter-1)
;;

(* enlève un élément n d'une liste s'il existe en temps linéaire, renvoie la liste sans l'élément ou celle de base sinon *)
let rec recValueRemover (n : int) (list : int list) (ret : int list) =
   match list with
   | [] -> List.rev ret
   | x::l ->
      if x = n then recValueRemover n l ret 
      else recValueRemover n l (x::ret)
;;

(* créé la permutation finale selon la paire de FIFO donnée en argument *)
let permutFinal (suiteInit : int list) (files : int Fifo.t * int Fifo.t) =
   let ret = [] in
   
   let rec construcPermut (suite : int list) (ret : int list) (files : int Fifo.t * int Fifo.t) =
      match suite with
      | [] -> ret
      | _ ->
         let dataPair = tirage files in
         let files = fst (dataPair) in
         let pos = reduce (snd (dataPair)) (List.length suite) in
         let currNumb = List.nth suite pos in
         let ret = currNumb::ret in
         let suite = recValueRemover currNumb suite [] in
         (* printf "%d; " currNumb; *)
         construcPermut suite ret files
   in
   
   construcPermut suiteInit ret files
   (* in List.rev (construcPermut suiteInit ret files) *)
;;


let shuffle n =
  (* shuffle_test n TODO: changer en une implementation complete *)

  let files = tirageRepete (queuePair (listOfPair n)) 165 in
  let suiteInit = List.init 52 (fun x -> x) in
  permutFinal suiteInit files
;;

