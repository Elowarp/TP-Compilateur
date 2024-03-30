# TP Compilateur Numérix

Ce dépot est une archive d'un de mes tp fait durant ma 2nde année de prépa (MPI). Le sujet porte sur la création d'un nouveau langage (Le Numérix) et la création d'un analyseur syntaxique en Ocaml qui va se charger de traduire notre nouveau langage en du C. Le but pédagogique étant de manipuler les automates et les grammaires.

Le sujet du tp est le fichier `Compilo_sujet.pdf`, il définit notamment les contraintes du langage Numérix (variables seulement entières, notation préfix, etc...)

Après compilation, l'exécution du fichier `parseur` créera un fichier `ex1.c` qui sera la traduction en C du fichier `ex1.num`, le fichier source Numérix.

Pour compiler et exécuter :

```bash
ocamlc -c lexeur.ml
ocamlc -o parseur lexeur.ml parseur.ml
./parseur
```

## Exemple de code

Ce code calcule 5!

En Numérix :

```Numerix
s := 1;
i := 1;

TantQue < i 6 Faire
s := * s i;
i := + i 1
FinTq
```

Puis en C :

```C
#include <stdlib.h>
#include <stdio.h>

int main() {
  int i;
  int s;

  s = 1;
  i = 1;
  while (i < 6){
    s = (s * i);
    i = (i + 1);
  }

  printf("i = %d\n", i);
  printf("s = %d\n", s);
  return 0;
}
```
