(*#!/usr/local/bin/math*)

(*Magic Mathematica Square*)

(*Costruisce quadrati magici di qualsiasi dimensione*)

(*Un quadrato magico è uno schieramento di numeri interi distinti in una tabella
quadrata tale che la somma dei numeri presenti in ogni riga, in ogni colonna e in
entrambe le diagonali dia sempre lo stesso numero, il numero magico*)
		 
(*Esempio di quadrato magico 3x3*)
(* 6 1 8
   7 5 3
   2 9 4 *)
   
If[$VersionNumber < 8, Print["Questo programma richiede Mathematica 8"]]

(*Messaggio di benvenuto*)
Print["Benveuto in MagicMathematicaSquare"]
Print["Ecco le funzioni che puoi utilizzare:"]
Print["runFitnessTrend[nInd_Integer, order_Integer, criterion_, pc_, pm_, limit_Integer, elitism_Integer:0]"]
		 
(*Calcola il numero magico*)
magicNumber[n_Integer] :=
	Module[{},
	       Return[ n (n n +1) / 2];
	];

(*Genera un numero che non e' nella lista data*)
(*Extreme sono gli estremi da cui pescare il numero*)
randomDiff[list_List, extreme_List] :=
	Module[{r},
	       r = Random[Integer, extreme];
	       While[Count[list,r] =!= 0, r = Random[Integer, extreme]];
	       Return[r];
	];

(*Genera un individuo, cioe' un quadrato magico*)
(*Il numero massimo che puo' apparire in un quadrato magico e' n^2,
  infatti il numero piu' grande che appare deve dare in tre somme
  diverse il numero magico, quindi signfica che la riga, la colonna e
  la diagonale devono contenere i numeri piu' piccoli ammessi, che
  in un quadrato NxN sono i numeri da 1 a 3(n-1)
 *)
  
generateSquare[n_Integer] :=
	Module[{used, ret},
	       used = Table[-1,{i,1,n},{j,1,n}];
	       ret = Table[r = randomDiff[Flatten[used], {1, n^2}];
			   used[[i,j]] = r;
			   r,{i,1,n},{j,1,n}];
	       Return[ret];
	];
		     
(*Genera una popolazione*) 
generatePop[popnum_Integer, order_Integer] :=
	Module[{},
	       If[order < 3,
		  Print["L'ordine del quadrato deve essere almeno 3"];
		  Abort[],
		  Return[Table[generateSquare[order], {popnum}]];
	       ];
	];

(*Scambia gli elementi i e j di una lista*)
swap[list_List, i_Integer, j_Integer] :=
	Module[{},
		   Return[list /. {list[[i]] -> list[[j]], list[[j]] -> list[[i]]}];
	];

(*Introduce una nozione di distanza tra quadrati come il
  numero di scambi che sono necessari per portare il secondo
  nel primo*)
permDist[square1_List, square2_List] :=
	Module[{order1, order2, counter, pos},
		   counter = 0;
	       order1 = Length[square1];
	       order2 = Length[square2];
	       flat1 = Flatten[square1];
	       flat2 = Flatten[square2];
	       If[order1 =!= order2,
		      Print["I quadrati devono essere dello stesso ordine!"];
		      Abort[];
	       ];
		   (*Controlla ogni elemento, se non combacia li scambia*)
	       For[i = 1, i < Length[flat1], i++, 
			   If[flat1[[i]] =!= flat2[[i]],
			      pos = Position[flat2, flat1[[i]]][[1,1]];
				  flat2 = swap[flat2, i, pos];
				  counter += 1;
				  ]
			];
		   Return[counter];
	];	       
(*Calcola somme righe*)
rowTotal[square_List] :=
	Module[{},
	       Return[Table[Plus @@ square[[i]], {i,1,Length[square]}]];
	];

(*Calcola somme colonne*)
columnTotal[square_List] :=
	Module[{},
	       Return[Table[Plus @@ Transpose[square][[i]], {i,1,Length[square]}]];
	];

(*Calcola somme diagonali*)
diagonalTotal[square_List] :=
	Module[{},
	       Return[{Sum[square[[i,i]],{i,1,Length[square]}],
		       Sum[square[[-i,i]],{i,1,Length[square]}]}];
	];

(*Funzione di fitness*)
(*Questa funzione di fitness e' la somma dei quadrati della differenza tra i valori
  delle linee e il numero magico, intendendo con linee le righe, le colonne e
  le diagionali*)
(*Fitness 0 significa che il quadrato e' magico*)
fitness[ind_List] :=
	Module[{order, row, column, diagonal},
	       order = Length[ind];
	       row = rowTotal[ind] - magicNumber[order];
	       column = columnTotal[ind] - magicNumber[order];
	       diagonal = diagonalTotal[ind] - magicNumber[order];
	       row = (#)^2 & /@ row;
	       column = (#)^2 & /@ column;
	       diagonal = (#)^2 & /@ diagonal;
	       row = Plus @@ row;
	       column = Plus @@ column;
	       diagonal = Plus @@ diagonal;
	       Return[row + column + diagonal]; 
	];

(*Lista di fitness*)
fitnessPop[pop_List] :=
	Module[{},
	       Return[fitness /@ pop]; (* /@ = Map, @@ = Apply *)
	];

(*Massimo fitness di una popolazione*)
maxFitnessPop[pop_List] :=
	Module[{},
	       Return[Max[fitnessPop[pop]]];
	];

(*Minimo fitness di una popolazione*)
minFitnessPop[pop_List] :=
	Module[{},
	       Return[Min[fitnessPop[pop]]];
	];
(*Restituisce l'individuo con fitness minima di una popolazione*)
whoIsTheBest[pop_List] :=
	Module[{min, l, fp},
	       fp = fitnessPop[pop];
	       Return[pop[[Position[fp, Min[fp]][[1,1]]]]];
	];

(*Fitness medio di una popolazione*)
meanFitnessPop[pop_List] :=
	Module[{},
	       Return[N[Mean[fitnessPop[pop]]]];
	];

(*Deviazione standard del fitness di una popolazione*)
devstdFitnessPop[pop_List] :=
	Module[{},
	       Return[N[StandardDeviation[fitnessPop[pop]]]];
	];

(*Fitness totale di una popolazione*)
totalFitnessPop[pop_List] :=
	Module[{},
	       Return[Apply[Plus,fitnessPop[pop]]];
	];

(*Statistiche di fitness di una popolazione*)
statPop[pop_List] :=
	Module[{},
	       Print["Fitness minimo: ", minFitnessPop[pop]];
	       Print["Fitness massimo: ", maxFitnessPop[pop]];
	       Print["Fitness medio: ", meanFitnessPop[pop]];
	       Print["Fitness totale: ", totalFitnessPop[pop]];
	       Print["Deviazione: ", devstdFitnessPop[pop]];
	];

(*Statistiche di una popolazione con il numero della generazione*)
statGen[pop_List, gen_Integer] :=
	Module[{},
	       Print["Siamo alla generazione ", gen];
	       statPop[pop];
	];

(*Ripartisci intervallo*)
(*Siccome l'individuo migliore e' quello che fitness minima si usano i reciproci*)
divideInterval[pop_List, criterion_] :=
	Module[{len, inv, inf},
	       Switch[criterion,
		   
		      fitnessProportionate, (*If criterion === FitnessProportionate*)
		      inv = N[#^(-1) & /@ fitnessPop[pop], 9]; (*N[] per performance*)
		      inf = Plus @@ inv;
		      len = inv/inf;
		      Return[Table[Sum[len[[i]],{i,1,j}],{j,1,Length[pop]}]],
			  			  
		      criterion, (*Evita che il criterio non sia definito*)
		      Print["Il criterio ", criterion, " non e' implementato"];
		      Abort[];
	       ];
	]; (*Restituisce una lista di segnaposti nell'intervallo 0,1*)
		      
(*Seleziona genitori dall'invervallo*)
chooseParents[pop_List, criterion_] :=
	Module[{r},
	       Return[Table[
		       r = Random[];
		       Length[Select[divideInterval[pop, criterion], (# < r) &]] + 1,
		       {Length[pop]}
		      ]
	       ];
	]; (*Restituisce una lista di individui*)

(*Produce un quadrato da una lista*)
listToSquare[list_List] :=
	Module[{order},
	       order = Sqrt[Length[list]];
	       Return[Table[list[[j + order (i - 1)]], {i,1,order}, {j,1,order}]];
	];

(*Controlla in modo piu' efficiente di Cases se un elemento e' in una lista*)
isThere[list_List, x_Integer] :=
	Module[{},
	       Return[Length[Select[list, (# === x) &]] =!= 0];
	];

(*Sostituisce i numeri doppi con altri numeri a caso, ma permessi*)
deleteDouble[square_List] :=
	Module[{order, numbers, insq, accept, went, this, r},
	       order = Length[square];
	       insq = Flatten[square];
	       numbers = Table[i, {i, 1, order^2}];
	       accept = DeleteCases[numbers, x_ /; isThere[insq, x]];
	       went = {};
	       ret = Table[If[isThere[went, square[[i,j]]],
			      (*Se il numero che sto considerando e' gia' passato*)
			      (*Significa che e' doppio*)
			      r = Random[Integer, {1, Length[accept]}];
			      this = accept[[r]];
			      accept = Drop[accept, {r}];
			      went = Append[went, this];
			      this,
			      (*Se il numero che sto considerando non e' passato*)
			      went = Append[went, square[[i,j]]];
			      square[[i,j]]
			   ],
			   {i,1,order},
			   {j,1,order}
		     ];
	       Return[ret];
	];

(*Produce figli a partire da due genitori*)
(*Il crossover presenta una criticita': quando ci si scambiano geni puo' accadere che
  un quadrato contenga due volte lo stesso numero, che non e' consentito dalle regole.
  Una possibile soluzione e' questa: una volta prodotti i figli si fanno passare alla
  funzione deleteDouble*)
crossoverOne[parents_List, pc_] :=
	Module[{where, c11, c12, c21, c22, p1, p2, order, j1, j2},
	       If[Random[] < pc,
		  (*Srotolo la struttura a quadrato*)
		  order = Length[parents[[1]]];
		  p1 = Flatten[parents[[1]],1];
		  p2 = Flatten[parents[[2]],1];
		  where = Random[Integer, {1,Length[p1]}];
		  c11 = Take[p1, {1, where}];
		  c12 = Take[p1, {where + 1, Length[p1]}];
		  c21 = Take[p2, {1,where}];
		  c22 = Take[p2, {where + 1, Length[p2]}];
		  j1 = deleteDouble[listToSquare[Join[c11,c22]]];
		  j2 = deleteDouble[listToSquare[Join[c21,c12]]];
  		  Return[{j1, j2}],
		  Return[parents];
	       ]
	]; (*Restituisce una lista contenente due individui mescolati*)

(*Produce figli scambiando bit tra i genitori dati in tutta la popolazione*)
crossoverAll[pop_List, pIndex_List, pc_Real] :=
	Module[{N},
	       N = Length[pop];
	       Return[Flatten[Table[
		       crossoverOne[{pop[[pIndex[[i]]]], pop[[pIndex[[N-i]]]]}, pc],
		       {i,1,N/2}
			      ], 1	
		      ]
	       ]
	]; (*Restituisce una lista contenente una popolazione di individui mescolati*)

(*Produce figli mescolati a partire dalla popolazione*)
(*choosePrantes crea la lista dei genitori scelti*)
crossoverParents[pop_List, pc_, criterion_, elitism_:0] :=
	Module[{bests, pop2, ret},
		   If[EvenQ[elitism],
		      Print["L'elitismo puo' essere solo per un numero pari di individui"];
			  Abort[]
		     ];
		   If[elitism >= Length[pop],
		      Print["Non puoi selezionare un elitismo cosi' elevato"];
			  Abort[];
		     ];	     
           If[elitism === 0,
		      Return[crossoverAll[pop, chooseParents[pop, criterion], pc]],
			  bests = {};
			  pop2 = pop;
			  For[i = 1, i <= elitism, i++,
				  bests = Append[bests, whoIsTheBest[pop]];		   
				  pop2 = Drop[pop2, Position[pop2, bests[[i]]][[1]]]; 
				 ];
			  ret = crossoverAll[pop2, chooseParents[pop2, criterion], pc];
			  ret = Append[ret, bests];
			  Return[ret];			  
			 ];			 
	];

(*Produce una mutazione su un individuo*)
(*La mutazione non produce doppioni*)
mutationOne[square_List, pm_] :=
(*	Module[{r, rr, ret, order, insq, accept, numbers, this},
	       order = Length[square];
	       insq = Flatten[square];
	       numbers = Table[i, {i, 1, magicNumber[order] - 1}];
	       accept = DeleteCases[numbers, x_ /; isThere[insq, x]];
	       ret = Table[r = Random[];
			   If[r < pm,
			      rr = Random[Integer, {1, Length[accept]}];
			      this = accept[[rr]];
			      accept = Drop[accept, {rr}];
			      accept = Append[accept, square[[i,j]]];
			      this,
			      square[[i,j]]
			   ],
			   {i, 1, order}, {j, 1, order}
		     ];
	       Return[ret];
	];
 *)
	Module[{r, order, temp, list},
	       order = Length[square];
	       If[Random[] < pm,
			  r = RandomInteger[{1, order^2}, 2];
			  list = Flatten[square];
			  temp = list[[r[[1]]]];
			  list[[r[[1]]]] = list[[r[[2]]]];
			  list[[r[[2]]]] = temp;
			  Return[listToSquare[list]],
			  Return[square]
	       ];
	];
		  
(*Produce mutazioni su una popolazione*)
mutationAll[pop_, pm_] :=
	Module[{},
	       Return[mutationOne[#, pm] & /@ pop]
	];

(*Produce una nuova popolazione a partire da una esistente*)
reproduce[pop_List, criterion_, pc_, pm_, elitism_Integer:0] :=
	Module[{},
	       Return[mutationAll[crossoverParents[pop, pc, criterion], pm]];
	]; (*Restituisce una popolazione di nuovi individui*)

(*Produce popolazioni finche' non arriva un quadrato magico con limite di gen*)
runLimited[nInd_Integer, order_Integer, criterion_, pc_, pm_, limit_Integer, elitism_Integer:0] :=
	Module[{count, popin},
	       
	       count = 1;
	       popin = generatePop[nInd, order];
	       statGen[popin, 1];
	       
	       If[inFitnessPop[popin] === 0, Return[count]];

	       While[minFitnessPop[popin] > 0 && count < limit,
		     count += 1;
		     popin = reproduce[popin, criterion, pc, pm, elitism];
		     statGen[popin, count];
	       ];
	       
	       Print["Generazione finale: ", count];
	       
	       best = whoIsTheBest[popin];
	       Print[MatrixForm[best]];
	       Print["Somme delle righe ", rowTotal[best]];
	       Print["Somme delle colonne ", columnTotal[best]];
	       Print["Somme delle diagonali ", diagonalTotal[best]];
	       	       
	       Return[whoIsTheBest[popin]];
	];

(*Produce popolazioni finche' non arriva un quadrato magico con limite di gen*)
runFitnessTrend[nInd_Integer, order_Integer, criterion_, pc_, pm_, limit_Integer, elitism_Integer:0] :=
	Module[{count, popin, fitmin, fitmax, fitmean},
	       
	       count = 1;
	       popin = generatePop[nInd, order];
	       statGen[popin, 1];
	       fitmin = {minFitnessPop[popin]};
	       fitmax = {maxFitnessPop[popin]};
	       fitmean = {meanFitnessPop[popin]};
	       
	       If[inFitnessPop[popin] === 0, Return[count]];

	       While[minFitnessPop[popin] > 0 && count < limit,
		     count += 1;
		     popin = reproduce[popin, criterion, pc, pm, elitism];
		     fitmin = Append[fitmin, minFitnessPop[popin]];
		     fitmax = Append[fitmax, maxFitnessPop[popin]];
		     fitmean = Append[fitmean, meanFitnessPop[popin]];
		     statGen[popin, count];
	       ];
	       
	       Print["Generazione finale: ", count];
	       Return[{whoIsTheBest[popin],{fitmin, fitmax, fitmean}}];
	];
