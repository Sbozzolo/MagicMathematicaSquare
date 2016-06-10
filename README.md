# MagicMathematicaSquare
Magic.m is a collection of tools to solve an NxN normal magic square using different stochastic tecniques: genetic algorithm and evolutionary algorithm.

The genetic approach contains:
- differents fitness functions
- a notion of distance between squares
- differents selection functions (fitness proportionate, elitism, fittest)

There is a semideterministic algorithm for small N, which doesn't converges.

The genetic approach does not converges.

Now it's in development an evolutionary approach.

MAGICMATHEMATICASQUARE NOW (almost) WORKS!!!

It can build large squares (> 10x10) in a few minutes!

Try for example xiekang[10]!


Sono stati implementati un algoritmo genetico e un algoritmo evolutivo.

L'algoritmo generico e' stato realizzato utilizzando diverse funzioni di fitness:
- Scarti quadratici delle linee dal valore ideale
- Valore assoluto delle differenze delle somme delle linee dal valore ideale
- Numero di linee corrette
Sono stati implementati diversi criteri di selezione
- Proporzionale alla fitness
- Pesato sulla distanza dall'individuo migliore
- Selezione casuale tra una certa mating pool
Diversi sono i crossover:
- Ad uno o due punti verticale o orizzontale
- Scambiando linee buone
Le mutazioni sono cinque:
- Scambio di una coppia
- Scambio di due colonne
- Scambio di due righe
- Permutazione di una riga
- Permutazione di una colonna
E' stato implementato l'elitismo, cioe' la possibilita' di preservare gli individui migliori anche alle generazioni successive.
E' stato tentato un approccio meno stocastico a partire da individui con tutte le linee perfette.
Nulla di tutto cio' ha prodotto risultati.

L'algoritmo evolutivo e' basato sull'algoritmo di Xie e Kang (con alcune piccole differenze). Questo e' un algoritmo genetico in cui non c'e' crossover ed uno stesso invidiuo viene sottoposto a diverse mutazioni. In questo caso il quadrato e' accompagnato da una matrice di deviazioni che tiene regola le mutazioni. 
Sono state implementate le mutazioni, che sono tre:
- Scambia elementi che stanno in una riga e una colonna non magiche con elementi di righe o colonne non magiche
- Scambia elementi di righe o colonne non magiche tra di loro
- Scambia elementi di righe o colonne non magiche con tutti quelli della matrice.
Una volta che gli individui diventano abbastanza buoni si rettificano, cioe' si cercano tutte quelle coppie, o coppie di coppie che rendono almeno una linea perfetta con la loro permutazione.
Quando gli individui hanno tutte le linee perfette si cambia regime di mutazione: si fanno permutazioni tra le righe o tra le colonne (in modo da non rovinare il quadrato). Quindi si fanno altre rettificazioni diagonali per sistemare le diagonali.
Questo approccio funziona.

Utilizzando la funzione xiekang[ordine del quadrato] si genera un quadrato magico, accompagnato da statistiche di esecuzione dell'algoritmo. Il quadrato e' in %[[2]].

Tempi di esecuzione:
3x3    ~ 0.01 s
7x7    ~ 10 s
10x10  ~ 1 min
20x20  ~ 30 min
30x30  ~ 4 h

Le statistiche per ordini superiori sono in elaborazione.