# Rapport de projet : Solveur de Solitaire

Programmation Fonctionelle
12/2022 - 01/2023
Université de Paris Cité
[Lien git du projet](https://gaufre.informatique.univ-paris-diderot.fr/becherra/pf5-projet-2022)

### Auteurs
- BECHERRA Mattéo, [@becherra](https://gaufre.informatique.univ-paris-diderot.fr/becherra), 22006156, L3 Informatique
- DESMAZIERES Adèle, [@desmazie](https://gaufre.informatique.univ-paris-diderot.fr/desmazie), 21979092, DL3 Biologie-Informatique


### Fonctionnalités

Ce projet est composé de 3 parties : 
1. L'implémentation des règles du solitaire, avec ses 4 variantes :
   * FreeCell
   * Seahaven
   * Midnight Oil
   * Baker's Dozen
2. La vérification d'une solution à une partie de solitaire, en soumettant un fichier de déplacements de cartes
3. La recherche automatisée de solutions à une partie

Un script bash a également été ajouté comme extension afin de tester un grand nombre de configurations pour la recherche de solutions.


### Compilation et exécution

Pour compiler et exécuter le programme, allez dans un terminal sous bash.

Lancement des tests :
```sh
dune runtest
```

Compilation :
```sh
dune build
```

Recherche d'une solution pour une partie, et enregistrement de la solution dans le fichier spécifié :
```sh
./run -search file_name FreeCell.123
```

Vérification de la correction d'un fichier solution pour une partie avec une graine spécifique :
```sh
./run -check file_name FreeCell.123
```

Les types de parties possibles sont :
- FreeCell ou fc : FreeCell
- Seahaven ou st : Seahaven
- MidnightOil ou mo : Midnight Oil
- BakersDozen ou bc : Baker's Dozen

Les graines possibles vont de 1 à 999'999'999. 

### Découpage modulaire

#### Etat.ml
Définit le type état ainsi que les méthodes associées à l'affichage de ce dernier. Un état représente l'état du tapis de jeu à un moment donné. De plus, il définit le type coup qui correspond au mouvement d'une carte. Voici comment est structuré le type état :
- les colonnes de cartes : une liste de piles de cartes (éventuellement avec des piles vides pour les colonnes vides)
- le registre : une liste de cartes (de taille variable, donc pas de registre vide)
- la capacité maximum du registre : un entier
- le dépôt : une liste de cartes qui contient seulement la dernière carte déposée de chaque couleur
- l'historique : une liste de coups qui mènent à l'état actuel
- le score : un entier qui représente le nombre de cartes mises au dépôt

Le type coup est défini ainsi :
- la source : la carte déplacée
- la destination : un String qui représente soit le numéro de la carte sur laquelle la carte source est déplacée ou alors un "T" pour un mouvement vers le registre, ou un "V" pour un mouvement vers une colonne vide

#### Pile.ml
Implémente une structure de type LIFO ainsi que la plupart des méthodes associées pour l'utiliser. Cette structure est utilisée pour représenter les colonnes de cartes. Elle contient une liste et sa taille. 

#### Autres structures
Le type règles est modélisé par :
- la capacité du registre
- le nombre de colonnes
- la distribution des cartes : une liste du nombre de cartes initial dans chaque colonne
- les cartes autorisées sur les colonnes vides : une option d'option de rang (aucune carte autorisée pour "None", les cartes de rang n autorisées pour "Some(n)", toutes les cartes autorisées pour "Some(None)" )
- l'enchainement des couleurs : indique quelle couleur de carte peut être placée sur laquelle

Le type enchainement de couleur : 
- soit Alternée : noir sur rouge ou rouge sur noir
- soit Identique : même couleur, au sens du symbole
- soit Any : peu importe


### Organisation du travail
Nous avons programmé les permutations de cartes pseudo-aléatoires du fichier [src/XpatRandom.ml](), générées à partir de l'entier appelé graine. Nous avons eu des difficultés à obtenir les permutations attendues, jusqu'à ce qu'on puisse comparer nos résultats intermédiaires avec ceux donnés pour la graine 1. Adèle a programmé l'initialisation d'une partie et la distribution des cartes dans [src/XpatSolver.ml](). 

Mattéo a implémenté les fonctions de normalisation d'état ainsi que de vérification de la légalité d'un coup donné, la gestion des entrées/sorties fichier, les ensembles d'états, ainsi qu'une fonction permettant de récupérer les états atteignables depuis un état donné selon les règles courantes.

Adèle a implémenté la fonction de mise à jour de la partie, le parcours du graphe des états qui représentent la partie. Elle a optimisé les fonctions de récupération d'états atteignables, et a créé un script en bash pour mesurer le temps de résolution de plusieurs configurations différentes. 

Nous avons tous les deux participé à l'écriture de ce rapport. 

### Divers : script de test

`./lancer_tests.exe input_file output_file timer_max` 
- Avec l'input file dans le meme dossier, contenant juste les noms des parties à tester séparées par des espaces : "st.7 mo.8 bc.100". 
- L'output file contiendra les résultats. 
- - Timer max limite le temps de chaque test. Enfin il ne faut pas avoir un fichier tmp.txt dans votre dossier, sinon il sera écrasé.

### Optimisation

#### La liste des états atteignables
Au départ nous utilisions pour la recherche les fonctions programmées pour la vérification d'une solution. Cependant nous parcourions toutes les colonnes de trop nombreuses fois, pour lister les cartes à déplacer, lister les destinations possibles, et pour chaque paire parcourir à nouveau pour vérifier la légalité du coup. 

Pour optimiser la recherche, on a diminué drastiquement le nombre de parcours des colonnes en appliquant les fonctionnalités nécessaires en un minimum de parcours : trouver la carte à déplacer, et pour chaque destination faire immédiatement toutes les vérifications nécessaires. 

#### Les états similaires
Pour réduire au maximum le nombre d'états du graphe, nous avons triés les registres (par valeur de carte) et les colonnes entre elles (par taille et valeur de la première carte). Ainsi nous évitons de traiter plusieurs fois des états identiques en terme de mécanique de jeu, mais différents en terme de permutation. Cela nous as permis de grandement accélérer la vitesse de résolution des tests. 

#### La fonction de recherche
Enfin mais pas des moidres, la fonction de recherche a été le meilleur candidat pour optimiser nos recherches. Nous avons tenté une approche en parcourant les états par Depth-First Search et par Breadth-First Search. Mais comme nous cherchons à maximiser un score, et non pas à trouver un état connu à l'avance, nous avons finalement opté pour une version du DFS ou la pile est triée à chaque étape, afin de traiter les états de meilleur score en priorité. 

Nous gardons donc à jour une "pile" d'états, modélisée par une liste et non pas par un objet pile, car elle doit être souvent retriée. Il y a également une "mémoire" de tous les états parcourus et dans la pile, sous forme de State.t état. Cet ensemble permet d'éviter de traiter plusieurs fois le même état, et donc gagner du temps. Avec ces éléments, nous passons la majorité des tests faciles et quelques tests medians. 

La dernière optimisation est l'implémentation d'un filtrage de la mémoire et la pile durant la recherche. Ce filtre ce déclenche quand on atteint un nouvel état de score supérieur au dernier filtrage. Alors les états de score inférieurs à la distance d'oubli (valeur optimale testée à 8) sont filtrés, et aucun ne pourra être ajouté ensuite, grâce à la mise en place d'une condition d'ajout d'état. Notre algorithme cherche d'abord une solution avec filtrage, puis relance la recherche sans filtrage si la recherche non-exhaustive à échouée. 

Nous passons désormais les tests faciles, médians et hards, sauf certaines configurations de BakersDozen. Cela est dû à leur grand nombre d'état atteignables, qui surpasse notre filtrage. 









