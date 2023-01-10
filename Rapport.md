# Rapport de projet : Solveur de Solitaire

Programmation Fonctionelle
12/2022 
Université de Paris Cité
[Lien git du projet](https://gaufre.informatique.univ-paris-diderot.fr/becherra/pf5-projet-2022)

### Auteurs
- BECHERRA Mattéo, [@becherra](https://gaufre.informatique.univ-paris-diderot.fr/becherra), 22006156, L3 Informatique
- DESMAZIERES Adèle, [@desmazie](https://gaufre.informatique.univ-paris-diderot.fr/desmazie), 21979092, DL3 Biologie-Informatique


### Fonctionnalités

*TODO : Donnez une description précise des fonctionnalités implémentées par votre rendu - sujet minimal, extensions éventuelles, éventuellement parties non réalisées ou non fonctionnelles.*

Ce projet est composé de 3 parties : 
1. l'implémentation du solitaire, avec ses 4 variantes, permettre à l'utilisateur de vérifier une solution en soumettant un fichier de déplacements de cartes
   * FreeCell
   * Seahaven
   * Midnight Oil
   * Baker's Dozen
2. la recherche automatisée de solutions du solitaire


### Compilation et exécution
*TODO : Documentez ensuite de façon précise la manière dont votre projet doit être compilé (normalement via dune) et exécuté (en donnant les options acceptées par votre programme). Pour ce projet, aucune bibliothèques externes n'est autorisé a priori. Nous contacter si cela vous semble problématique.*

Pour compiler et exécuter le programme, allez dans un terminal sous bash.

Lancement des tests :
```sh
dune runtest tests/I
```

Compilation :
```sh
dune build
```

Exécution avec les paramètres par défaut :
```sh
./run
```

Exécution avec un type de solitaire spécifique (ex : freecell) et une graine spécifique (ex : 123) :
```sh
./run freecell.123
```

Recherche d'une solution pour une partie, et enregistrement de la solution dans le fichier spécifié :
```sh
./run -search file_name freecell.123
```

Vérification de la correction d'un fichier solution pour une partie avec une graine spécifique :
```sh
./run -check file_name freecell.123
```

Les types de parties possibles sont :
- freecell ou fc -> FreeCell
- seahaven ou st -> Seahaven
- midnightoil ou mo -> Midnight Oil
- bakersdozen ou bc -> Baker's Dozen

Les graines possibles vont de 1 and 999'999'999. 

### Découpage modulaire

Etat.ml -> définit le type état ainsi que les méthodes associées à l'affichage de ce dernier. Un état représente l'état du tapis de jeu à un moment donné.

Pile.ml -> Implémente une structure de type LIFO ainsi que la plupart des méthodes associées pour l'utiliser.


###### Choix des structures à utiliser pour modéliser le solitaire
Après avoir mis en place le git du projet, nous nous sommes accordé sur la manière d'implémenter les mécanismes du solitaire :

- les colonnes de cartes : une liste de piles de cartes
- les colonnes vides : une pile de cartes vide
- les registres : une liste de cartes
- les dépôts : idem, mais celle ci contient seulement la dernière carte déposée de chaque couleur

Nous gardons à l'esprit la possibilité d'en changer si d'autres options se révélaient plus efficace. 

### Organisation du travail

Mattéo a programmé les permutations de cartes pseudo-aléatoires du fichier [src/XpatRandom.ml](), générées à partir de l'entier appelé graine. Nous avons eu des difficultés à obtenir les permutations attendues, jusqu'à ce qu'on puisse comparer nos résultats intermédiaires avec ceux donnés pour la graine 1. Adèle a programmé l'initialisation d'une partie et la distribution des cartes dans [src/XpatSolver.ml](). 

Adèle a débuté l'écriture du rapport. 

Mattéo a implémenté les fonctions de normalisation d'état ainsi que de vérification de la légalité d'un coup donné, la gestion des entrées/sorties fichier, les ensembles d'états, ainsi qu'une fonction permettant de récupérer les états atteignables depuis un état donné selon les règles courantes.

Adèle a implémenté le parcours du graphe des états qui représentent la partie, a optimisé les fonctions de récupération d'états atteignables, et a créé un script pour tester la solvabilité de différentes configurations en donnant le temps mis pour l'effectuer. 

### Divers
*TODO : Cette partie est entièrement libre : remarques, suggestions, questions...*

### Optimisation
