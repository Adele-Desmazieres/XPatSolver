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
1. l'implémentation du solitaire, avec ses 4 variantes, permettre à l'utilisateur d'y "jouer" en soumettant un fichier de déplacements de cartes
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
*TODO : Donnez une description des traitements pris en charge par chaque module (.ml) de votre projet Précisez le rôle et la nécessité de chaque module ajouté au dépôt initial.*

### Organisation du travail
*TODO : Cette partie est plus libre dans sa forme. Indiquez la manière dont les tâches ont été réparties entre les membres du groupe au cours du temps. Donnez une brève chronologie de votre travail sur ce projet au cours de ce semestre.*

#### Semaine 47

###### Choix des structures à utiliser pour modéliser le solitaire
Après avoir mis en place le git du projet, nous nous sommes accordé sur la manière d'implémenter les mécanismes du solitaire :

- les colonnes de cartes : une liste de piles de cartes
- les colonnes vides : un entier représentant leur nombre
- les registres : 
- les dépôts : 

Nous gardons à l'esprit la possibilité d'en changer si d'autres options se révélaient plus efficace. 

###### Planification temporelle

*TODO : faire genre on a planifié dans le temps*

#### Semaine 48

Mattéo a programmé les permutations de cartes pseudo-aléatoires du fichier [src/XpatRandom.ml](), générées à partir de l'entier appelé graine. Nous avons eu des difficultés à obtenir les permutations attendues, jusqu'à ce qu'on puisse comparer nos résultats intermédiaires avec ceux donnés pour la graine 1. Adèle a programmé l'initialisation d'une partie et la distribution des cartes dans [src/XpatSolver.ml](). 

#### Semaine 49

Adèle a débuté l'écriture du rapport. 

### Divers
*TODO : Cette partie est entièrement libre : remarques, suggestions, questions...*

### Optimisation
