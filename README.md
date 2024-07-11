# Machine Lambda-Calcul

## Année : L3

## Langage : OCaml

Le but de ce stage était d'implémenter une machine abstraite réalisant de la normalisation forte (i.e. jusqu'à forme normale) de lambda-termes.

Pour arriver à cet objectif, on a d'abord expérimenté les machines de réduction faible réalisant différentes stratégies de réduction (appel par nom, valeur et nécessité). Pour limiter le nombre de réductions (afin de rendre l'implémentation efficace), on est parti sur les machines à environnement.

Puis, on s'est intéressé aux méthodes permettant de réaliser de la réduction forte à partir des machines à environnement. On s'est concentré sur la machine Readback présentée en 2002 par Benjamin Grégoire et Xavier Leroy avec le papier "A Compiled Implementation of Strong Reduction" (https://dl.acm.org/doi/abs/10.1145/581478.581501). Une autre méthode qu'on aurait pu étudier davantage est celle mélangeant syntaxe et sémentique présentée dans la papier d'Août 2022 : "A Simple and Efficient Implementation of Strong Call by Need by an Abstract Machine" de Malgorzata Biernacka, Witold Charatonik & Tomasz Drab (https://dl.acm.org/doi/10.1145/3549822).

La suite envisageable serait de poursuivre le travail de formalisation de la machine de réductions fortes Call By Need afin de faire suffisamment de parallèles avec le lambda-calcul afin de prouver la correction de cette machine, c'est-à-dire de prouver que la normalisation effectuée est bien celle définie par le lambda-calcul.
