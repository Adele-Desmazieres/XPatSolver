# Rapport de projet : Solveur de Solitaire

Programmation Fonctionelle
12/2022 
Université de Paris Cité
[Lien git du projet](https://gaufre.informatique.univ-paris-diderot.fr/becherra/pf5-projet-2022)

### Auteurs
- BECHERRA Mattéo, [@becherra](https://gaufre.informatique.univ-paris-diderot.fr/becherra), 22006156, L3 Informatique
- DESMAZIERES Adèle, [@desmazie](https://gaufre.informatique.univ-paris-diderot.fr/desmazie), 21979092, DL3 Biologie-Informatique


### Fonctionnalités

Ce projet est composé de 3 parties : 
1. L'implémentation du solitaire, avec ses 4 variantes :
   * FreeCell
   * Seahaven
   * Midnight Oil
   * Baker's Dozen
2. La vérification d'une solution en soumettant un fichier de déplacements de cartes
3. La recherche automatisée de solutions du solitaire

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
- FreeCell ou fc -> FreeCell
- Seahaven ou st -> Seahaven
- MidnightOil ou mo -> Midnight Oil
- BakersDozen ou bc -> Baker's Dozen

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
Implémente une structure de type LIFO ainsi que la plupart des méthodes associées pour l'utiliser. Cette structure est utilisée pour représenter les colonnes de cartes.

#### Autres structures

Le type regles :
- la capacité du registre
- le nombre de colonnes
- la distribution des cartes : une liste du nombre de cartes dans chaque colonne
  carteSurColonneVide : (Card.rank option) option; (* Quel est le rang de carte autorisé sur une colonne vide : None -> Aucun, Some (n) -> rang n, Some (None) -> Tous *)
  enchainement : enchainementCouleur;






### Organisation du travail

Mattéo a programmé les permutations de cartes pseudo-aléatoires du fichier [src/XpatRandom.ml](), générées à partir de l'entier appelé graine. Nous avons eu des difficultés à obtenir les permutations attendues, jusqu'à ce qu'on puisse comparer nos résultats intermédiaires avec ceux donnés pour la graine 1. Adèle a programmé l'initialisation d'une partie et la distribution des cartes dans [src/XpatSolver.ml](). 

Adèle a débuté l'écriture du rapport. 

Mattéo a implémenté les fonctions de normalisation d'état ainsi que de vérification de la légalité d'un coup donné, la gestion des entrées/sorties fichier, les ensembles d'états, ainsi qu'une fonction permettant de récupérer les états atteignables depuis un état donné selon les règles courantes.

Adèle a implémenté le parcours du graphe des états qui représentent la partie, a optimisé les fonctions de récupération d'états atteignables, et a créé un script pour tester la solvabilité de différentes configurations en donnant le temps mis pour l'effectuer. 

### Divers
*TODO : Cette partie est entièrement libre : remarques, suggestions, questions...*

### Optimisation
