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

(*Per aggiungere una fitness modificare*)
(*fitness, whoIsTheBest, divideInterval, targetFitnessPop e nel run*)
(*anche in sortpop*)
   
If[$VersionNumber < 8, Print["Questo programma richiede Mathematica 8"]]

(*Messaggio di benvenuto*)
Print["Benveuto in MagicMathematicaSquare"]
Print["Ecco le funzioni che puoi utilizzare:"]
Print["run[nInd_Integer, order_Integer, pc_, pm_, criterion->fintessProportionate, limit->1000, elitism->0, fitnessFunction -> totalSquared]"]
Print["L'output prodotto e': {whoIsTheBest}, timeused, {fitmin, fitmax, fitmean}"];
		 
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
  in un quadrato NxN sono i numeri da 1 a N^2
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
	
(*Funzioni di fitness*)
fitness[ind_List, type_:totalSquared] := 
	Module[{order, row, column, diagonal},
	       order = Length[ind];
	       row = rowTotal[ind] - magicNumber[order];
	       column = columnTotal[ind] - magicNumber[order];
	       diagonal = diagonalTotal[ind] - magicNumber[order];
	       
	       Switch[type,
		 (*Questa funzione di fitness e' la somma dei quadrati*)
		 (*della differenza tra i valori delle linee e il numero magico,*)
		 (*intendendo con linee le righe, le colonne e le diagionali*)	
				  
		      totalSquared,
		      row = (#)^2 & /@ row;
		      column = (#)^2 & /@ column;
		      diagonal = (#)^2 & /@ diagonal;
		      row = Plus @@ row;
		      column = Plus @@ column;
		      diagonal = Plus @@ diagonal;
		      Return[row + column + diagonal],

		(*Questa funzione di fitness e' come totalSquared ma non usa
                  il quadrato ma il valore assoluto*)
		      totalAbs,
		      row = Abs[(#)] & /@ row;
		      column = Abs[(#)] & /@ column;
		      diagonal = Abs[(#)] & /@ diagonal;
		      row = Plus @@ row;
		      column = Plus @@ column;
		      diagonal = Plus @@ diagonal;
		      Return[row + column + diagonal],		      
				  
		(*Questa funzione di fitness conta il numero di linee perfette*)
	        (*Fitness 2 n + 2 significa che il quadrato e' magico*)
			 
		      correctLines,
		      Return[Count[{row, column, diagonal}, 0, Infinity]],

		(*Questa funzione di fintess conta il numero di righe perfette*)
	        (*Un individuo perfetto e' quello che ha tutte le righe perfette*)
		      correctRows,
		      Return[Count[{row}, 0, Infinity]],
		   
		      _, (*Evita che il criterio non sia definito*)
		      Print["La funzione di fitness ", type, " non e' ancora stata implementata"];
		      Abort[];	
		];   
	];

(*Lista di fitness*)
fitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       Return[fitness[#, type] & /@ pop]; 
	];

(*Massimo fitness di una popolazione*)
maxFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       Return[Max[fitnessPop[pop, type]]];
	];

(*Minimo fitness di una popolazione*)
minFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       Return[Min[fitnessPop[pop, type]]];
	];
(*Restituisce l'individuo con fitness minima di una popolazione*)
whoIsTheBest[pop_List, type_:totalSquared] :=
	Module[{min, l, fp},
	       fp = fitnessPop[pop, type];
	       Switch[type,
		      totalSquared,
		      Return[pop[[Position[fp, Min[fp]][[1,1]]]]],
		      totalAbs,
		      Return[pop[[Position[fp, Min[fp]][[1,1]]]]],
		      correctLines,
		      Return[pop[[Position[fp, Max[fp]][[1,1]]]]],
		      correctRows,
		      Return[pop[[Position[fp, Max[fp]][[1,1]]]]]
	       ];
	];

(*Fitness medio di una popolazione*)
meanFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       Return[N[Mean[fitnessPop[pop, type]]]];
	];

(*Deviazione standard del fitness di una popolazione*)
devstdFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       Return[N[StandardDeviation[fitnessPop[pop, type]]]];
	];

(*Fitness totale di una popolazione*)
totalFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       Return[Apply[Plus,fitnessPop[pop, type]]];
	];
	
(*Distanza media dal migliore*)
meanDistPop[pop_List, type_:totalSquared] :=
	Module[{},
	   Return[N[Mean[permDist[whoIsTheBest[pop, type], #] & /@ pop]]];
	];

(*Ordina una popolazione in basa alla fitness*)
sortPop[pop_List, type_:totalSquared] :=
	Module[{},
	       If[type === totalSquared,
		  Return[SortBy[pop, fitness[#, type] &]],
		  Return[SortBy[pop, 1/fitness[#, type] &]];
	       ];
	];
	
(*Restituisce la funzione di fitness migliore a seconda dei casi*)
targetFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
		   Switch[type,
		          totalSquared,
			  Return[minFitnessPop[pop, type]],
			  totalAbs,
			  Return[minFitnessPop[pop, type]],
			  correctLines,
			  Return[maxFitnessPop[pop, type]],
			  correctRows,
			  Return[maxFitnessPop[pop, type]]
		   ];
	];

(*Statistiche di fitness di una popolazione*)
statPop[pop_List, type_:totalSquared, distance_:False] :=
	Module[{},
	       Print["Fitness minimo: ", minFitnessPop[pop, type]];
	       Print["Fitness massimo: ", maxFitnessPop[pop, type]];
	       Print["Fitness medio: ", meanFitnessPop[pop, type]];
	       Print["Fitness totale: ", totalFitnessPop[pop, type]];
	       Print["Deviazione: ", devstdFitnessPop[pop, type]];
	       If[distance, Print["Distanza media: ", meanDistPop[pop, type]] ];
	];

(*Statistiche di una popolazione con il numero della generazione*)
statGen[pop_List, gen_Integer, type_:totalSquared, distance_:False] :=
	Module[{},
	       Print["Siamo alla generazione ", gen];
	       statPop[pop, type, distance];
	];

(*Ripartisci intervallo*)
(*Siccome l'individuo migliore e' quello che fitness minima si usano i reciproci*)
divideInterval[pop_List, criterion_, type_:totalSquared] :=
	Module[{len, inv, inf, best, dists, fits, upfits, distmean},
	       Switch[criterion,
		   
		      fitnessProportionate, (*If criterion === FitnessProportionate*)
		      fits = fitnessPop[pop, type];
		      (*Aggiugnere qui le nuove fitness*)
		      Switch[type,
			     totalSquared,
			     inv = N[#^(-1) & /@ fits, 9],
		             totalAbs,
		             inv = N[#^(-1) & /@ fits, 9],
	                     correctLines,
			     inv = fits,
			     correctRows,
			     inv = fits
		      ];
		      inf = Plus @@ inv;
		      len = inv/inf;
		      Return[Table[Sum[len[[i]],{i,1,j}],{j,1,Length[pop]}]],
				  
		      similarSquare,
		      best = whoIsTheBest[pop, type];
		      dists = permDist[best, #] & /@ pop;
		      distmean = Mean[dists];
		      dists = dists / distmean;
		      fits = fitnessPop[pop, type];
		      upfits = Table[fits[[i]](1 + dists[[i]]), {i,1,Length[pop]}];
		      (*Aggiungere qui le nuove fintess*)
		      Switch[type,
			     totalSquared,
			     inv = N[#^(-1) & /@ upfits, 9],
		             totalAbs,
		             inv = N[#^(-1) & /@ upfits, 9],
	                     correctLines,
			     inv = upfits,
			     correctRow,
			     inv = upfits
		      ];
		      inf = Plus @@ inv;
		      len = inv/inf;
		      Return[Table[Sum[len[[i]],{i,1,j}],{j,1,Length[pop]}]],

		      fittests, (*Gli individui scelti hanno tutti la stessa probabilita*)
		      dists = N[1/Length[pop]];
		      Return[Table[Sum[dists,{j}],{j,1,Length[pop]}]],
							  
       		      _, (*Evita che il criterio non sia definito*)
		      Print["Il criterio ", criterion, " non e' implementato"];
		      Abort[];
	       ];
	]; (*Restituisce una lista di segnaposti nell'intervallo 0,1*)
		      
(*Seleziona genitori dall'invervallo*)
chooseParents[pop_List, criterion_, type_:totalSquared] :=
	Module[{r},
	       Return[Table[
		       r = Random[];
		       Length[Select[divideInterval[pop, criterion, type], (# < r) &]] + 1,
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

(*Restitisce una lista contenente le righe corrette del quadrato*)
goodRows[square_List] :=
	Module[{order, mv},
	       order = Length[square];
	       mv = magicNumber[order];
	       Return[Table[If[(Plus @@ square[[i]]) === mv, 1, 0], {i,1,order}]];
	];

(*Restituisce una lista contente le colonne corrette del quadrato*)
goodColumns[square_List] :=
	Module[{},
	       Return[goodRows[Transpose[square]]];
	];

(*Scambia due righe tra due quadrati*)
swapRows[square1_List, square2_List, row1_Integer, row2_Integer] :=
	Module[{re1, ret2},
	       If[Length[square1] =!= Length[square2],
		  Print["Non si puo' fare questa operazione con ordini diversi!"];
		  Abort[];
	       ];
	       ret1 = square1;
	       ret1[[row1]] = square2[[row2]];
	       ret2 = square2;
	       ret2[[row2]] = square1[[row1]];
	       Return[{ret1, ret2}];
	];

(*Scambia due colonne tra due quadrati*)
swapColumns[square1_List, square2_List, row1_Integer, row2_Integer] :=
	Module[{re1, ret2},
	       Return[swapRows[Transpose[square1], Transpose[square2], row1, row2]];
	];

(*Restistuice una permutazione della riga*)
permutateRow[square_List, n_Integer] :=
	Module[{},
	       Return[Part[Permutations[square[[n]]], Random[Integer, {1, Length[square]!}]]];
	];

(*Restituisce una permutazione della colonna*)
permutateColumn[square_List, n_Integer] :=
	Module[{},
	       Return[permutateRow[Transpose[square]]];
	];

(*Produce figli a partire da due genitori*)
(*Il crossover presenta una criticita': quando ci si scambiano geni puo' accadere che
  un quadrato contenga due volte lo stesso numero, che non e' consentito dalle regole.
  Una possibile soluzione e' questa: una volta prodotti i figli si fanno passare alla
  funzione deleteDouble*)
crossoverOne[parents_List, pc_, type_:totalSquared] :=
	Module[{where, c11, c12, c21, c22, p1, p2, order, j1, j2, goodrows1, goodrow2,
	        goodcols1, goodcols2, ps1, ps2, where1, where2, ran},
	       (*If[Random[] < pc,
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
	       ]*)

	       Switch[type,
		      totalSquared,
		   
		      If[Random[] <= pc,
			 (*Crossover a due punti*)
			 (*Srotolo la struttura a quadrato*)
			 order = Length[parents[[1]]];
			 p1 = Flatten[parents[[1]],1];
			 p2 = Flatten[parents[[2]],1];
			 where1 = Random[Integer, {2,Length[p1] - 2}];
			 where2 = Random[Integer, {where1,Length[p1] - 1}];
			 c11 = Take[p1, {1, where1}];
			 c12 = Take[p1, {where1 + 1, where2}];
			 c13 = Take[p1, {where2 + 1, Length[p1]}];
			 c21 = Take[p2, {1, where1}];
			 c22 = Take[p2, {where1 + 1, where2}];
			 c23 = Take[p2, {where2 + 1, Length[p2]}];
			 j1 = deleteDouble[listToSquare[Join[c11, c22, c13]]];
			 j2 = deleteDouble[listToSquare[Join[c21, c12, c23]]];
			 Return[{j1, j2}],
			 Return[parents];
		      ],

		      correctLines,
		      
		      (*Questo crossover scambia due linee buone tra gli individui*)
		      If[Random[] <= pc,
			 goodrows1 = goodRows[parents[[1]]];
			 goodrows2 = goodRows[parents[[2]]];
			 goodcols1 = goodColumns[parents[[1]]];
			 goodcols2 = goodColumns[parents[[2]]];		     
			 c11 = Plus @@ goodrows1;
			 c12 = Plus @@ goodrows2;
			 c21 = Plus @@ goodcols1;
			 c22 = Plus @@ goodcols2;
			 (*Per avere un trattamento simmetrico tra righe e colonne*)
			 ran = Random[Integer];
			 Switch[ran,
				0,
				If[c11 c12 > 0,
				   where1 = Random[Integer, {1, c11}];
				   where2 = Random[Integer, {1, c12}];
				   ps1 = Position[goodrows1, 1][[where1, 1]];
				   ps2 = Position[goodrows2, 1][[where2, 1]];
				   j1 = swapRows[parents[[1]], parents[[2]], ps1, ps2][[1]];
				   j2 = swapRows[parents[[1]], parents[[2]], ps1, ps2][[2]];
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
				   Return[{j1, j2}];
				];
				If[c21 c22 > 0,
				   where1 = Random[Integer, {1, c21}];
				   where2 = Random[Integer, {1, c22}];
				   ps1 = Position[goodcols1, 1][[where1, 1]];
				   ps2 = Position[goodcols2, 1][[where2, 1]];
				   j1 = swapColumns[parents[[1]], parents[[2]], ps1, ps2][[1]];
				   j2 = swapColumns[parents[[1]], parents[[2]], ps1, ps2][[2]];
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
 				   Return[{j1, j2}];
				],
				1,
				If[c21 c22 > 0,
				   where1 = Random[Integer, {1, c21}];
				   where2 = Random[Integer, {1, c22}];
				   ps1 = Position[goodcols1, 1][[where1, 1]];
				   ps2 = Position[goodcols2, 1][[where2, 1]];
				   j1 = swapColumns[parents[[1]], parents[[2]], ps1, ps2][[1]];
				   j2 = swapColumns[parents[[1]], parents[[2]], ps1, ps2][[2]];
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
				   Return[{j1, j2}];
				];
				If[c11 c12 > 0,
				   where1 = Random[Integer, {1, c11}];
				   where2 = Random[Integer, {1, c12}];
				   ps1 = Position[goodrows1, 1][[where1, 1]];
				   ps2 = Position[goodrows2, 1][[where2, 1]];
				   j1 = swapRows[parents[[1]], parents[[2]], ps1, ps2][[1]];
				   j2 = swapRows[parents[[1]], parents[[2]], ps1, ps2][[2]];
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
				   Return[{j1, j2}];
				]
			 ];
			 Return[parents],
			 Return[parents];
		      ],

		      totalAbs,
		      
		      If[Random[] <= pc,
			 (*Crossover ad un punto*)
			 (*Srotolo la struttura a quadrato*)
			 order = Length[parents[[1]]];
			 p1 = Flatten[parents[[1]],1];
			 p2 = Flatten[parents[[2]],1];
			 where1 = Random[Integer, {1,Length[p1] - 1}];
			 c11 = Take[p1, {1, where1}];
			 c12 = Take[p1, {where1 + 1, Length[p1]}];
			 c21 = Take[p2, {1, where1}];
			 c22 = Take[p2, {where1 + 1, Length[p2]}];
			 j1 = deleteDouble[listToSquare[Join[c11,c22]]];
			 j2 = deleteDouble[listToSquare[Join[c21,c12]]];
			 Return[{j1, j2}],
			 Return[parents];
		      ],

		      correctRows,
		      If[Random[] <= pc,
			 (*Crossover ad un punto su righe*)
			 (*Srotolo la struttura a quadrato*)
			 order = Length[parents[[1]]];
			 p1 = parents[[1]];
			 p2 = parents[[2]];
			 where1 = Random[Integer, {2, order}];
			 c11 = Take[p1, {1, where1}];
			 c12 = Take[p1, {where1 + 1, Length[p1]}];
			 c21 = Take[p2, {1, where1}];
			 c22 = Take[p2, {where1 + 1, Length[p2]}];
			 j1 = deleteDouble[Join[c11,c22]];
			 j2 = deleteDouble[Join[c21,c12]];
			 Return[{j1, j2}],
			 Return[parents];
		      ],		      

		      _, (*Evita che il criterio non sia definito*)
		      Print["La funzione di fitness ", type, " non e' ancora stata implementata"];
		      Abort[];
	       ];

	]; (*Restituisce una lista contenente due individui mescolati*)

(*Produce figli scambiando bit tra i genitori dati in tutta la popolazione*)
crossoverAll[pop_List, pIndex_List, pc_Real, type_:totalSquared] :=
	Module[{N},
	       N = Length[pop];
	       Return[Flatten[Table[
		       crossoverOne[{pop[[pIndex[[i]]]], pop[[pIndex[[N-i]]]]}, pc, type],
		       {i,1,N/2}
			      ], 1	
		      ]
	       ]
	]; (*Restituisce una lista contenente una popolazione di individui mescolati*)

(*Produce figli mescolati a partire dalla popolazione*)
(*choosePrantes crea la lista dei genitori scelti*)
crossoverParents[pop_List, pc_, criterion_, type_:totalSquared, elitism_:0] :=
	Module[{bests, pop2, ret, popsorted},
               If[elitism === 0,
		  Return[crossoverAll[pop, chooseParents[pop, criterion, type], pc, type]],
		  popsorted = sortPop[pop, type];
		  bests = popsorted[[1;;elitism]];
		  pop2 = Drop[pop, elitism];
		  ret = crossoverAll[pop2, chooseParents[pop2, criterion, type], pc, type];
		  ret = Join[ret, bests];
		  Return[ret];			  
		 ];			 
	];

(*Produce una mutazione su un individuo*)
(*La mutazione non produce doppioni*)
mutationOne[square_List, pm_] :=
	Module[{n, r, order, temp, list},
	       order = Length[square];
	       n = Random[Integer, {1, 5}];
	       n = 4;
	       Switch[n,
		      1, (*Scambio di una coppia*)
		      If[Random[] <= pm,
			 r = RandomInteger[{1, order^2}, 2];
			 list = Flatten[square];
			 temp = list[[r[[1]]]];
			 list[[r[[1]]]] = list[[r[[2]]]];
			 list[[r[[2]]]] = temp;
			 Return[listToSquare[list]],
			 Return[square]
		      ],
		      2, (*Scambia due colonne*)
		      If[Random[] <= pm,
			 r = RandomInteger[{1, order}, 2];
			 list = Transpose[square];
			 temp = list[[r[[1]]]];
			 list[[r[[1]]]] = list[[r[[2]]]];
			 list[[r[[2]]]] = temp;
			 Return[Transpose[list]],
			 Return[square]
		      ],
		      3, (*Scambia due righe*)
		      If[Random[] <= pm,
			 r = RandomInteger[{1, order}, 2];
			 list = square;
			 temp = list[[r[[1]]]];
			 temp = list[[r[[1]]]];
			 list[[r[[1]]]] = list[[r[[2]]]];
			 list[[r[[2]]]] = temp;
			 Return[list],
			 Return[square]
		      ],
		      4, (*Permuta una riga*)
		      If[Random[] <= pm,
			 r = Random[Integer, {1, order}];
			 list = square;
			 list[[r]] = permutateRow[square, r];
			 Return[list],
			 Return[square]
		      ],
		      5, (*Permuta una colonna*)
		      If[Random[] <= pm,
			 r = Random[Integer, {1, order}];
			 list = square;
			 list[[r]] = permutateRow[square, r];
			 Return[list],
			 Return[square]
		      ]		      
	       ];
	];

(*Data un numero cerca tutti gli elementi nelle righe che
  hanno per somma questo valore*)
findPairInRow[square_List, row_Integer, sum_Integer] :=
	Module[{},
	       Return[Cases[Subsets[square[[row]], {2}],
				  x_ /; Total[x] === sum
		      ]
	       ];
	];

(*Prendi due elementi a caso da una riga,
  prende una riga a caso e cerca le coppie che hanno la
  stessa somma e fa uno scambiuo a caso, se nella riga
  considerata non c'e' nessuna coppia ne cerca un'atra*)
swapElementsFromCorrectRows[square_List] :=
	Module[{order, row, pair, randrow, pairs, ret, selectpair, sub1, sub2, rows},
	       order = Length[square];
	       row = Random[Integer, {1, order}];
	       (*Richiede Mathematica 8*)
	       pair = RandomSample[square[[row]], 2];
	       pairs = {};
	       numbers = Table[i, {i,1,order}];
	       numbers = DeleteCases[numbers, row];
	       While[Length[pairs] === 0,
		     If[Length[numbers] === 0,
			Break[];
			Break[];
		     ];
		     randrow = RandomChoice[numbers, 1];
		     randrow = randrow[[1]];
		     While[randrow === row,
			   randrow = Random[Integer, {1, order}];
		     ];
		     pairs = findPairInRow[square, randrow, Total[pair]];
		     numbers = DeleteCases[numbers, randrow];
	       ];
	       ret = square;
	       If[Length[numbers] =!= 0,
		  selectpair = RandomSample[pairs, 1];
		  selectpair = Flatten[selectpair, 1];		  
		  sub1 = {pair[[1]] -> selectpair[[2]], pair[[2]] -> selectpair[[1]]};
		  sub2 = {selectpair[[1]] -> pair[[2]], selectpair[[2]] -> pair[[1]]};
		  ret[[row]] = ret[[row]] /. sub1;
		  ret[[randrow]] = ret[[randrow]] /. sub2
	       ];
	       Return[ret];
	];

(*Applica piu' volte swapElementsFromCorrectRows*)
swapElementsFromCorrectRowsMulti[square_List, multi_Integer:1] :=
	Module[{},
	       Return[Nest[swapElementsFromCorrectRows[#] &, square, multi]];
	];

(*Applica swapElementsFromCorrectRows ad una popolazione*)
swapElementsFromCorrectRowsAll[pop_List, elitism_Integer:0, type_:correctLines, multi_Integer:1] :=
	Module[{popsorted, pop2, ret},
	       popsorted = sortPop[pop, type];
	       pop2 = Drop[popsorted, elitism];
	       ret = swapElementsFromCorrectRowsMulti[#, multi] & /@ pop2;
	       ret = Join[popsorted[[1;;elitism]], pop2];
	       Return[ret];
	];
    

(*RICHIEDE MATHEMATICA 8*)	
Options[fromRowsToMagic] = {elitism -> 0, fitnessFunction -> correctLines, limit->1000,
			    multi -> 10}

(*Prende una popolazione di individui con righe perfette e li mischia*)
fromRowsToMagic[nInd_Integer, order_Integer, OptionsPattern[]] :=
	Module[{count, t0, fintmin, fitmax, fitmean,
	        deletat, popin, popsorted, i, newpop},

	       Print["-----Inizio a scambiare le righe-----"];
	       t0 = TimeUsed[];
	       count = 1;
	       popin = ParallelTable[buildFromScratch[order], {nInd}];
	       		   
	       statGen[popin, 1, OptionValue[fitnessFunction]];
		   
	       fitmin = {minFitnessPop[popin, OptionValue[fitnessFunction]]};
	       fitmax = {maxFitnessPop[popin, OptionValue[fitnessFunction]]};
	       fitmean = {meanFitnessPop[popin, OptionValue[fitnessFunction]]};

	       (*Aggiornare qui quando si aggiunge fitness*)
	       Switch[OptionValue[fitnessFunction],
		      totalSquared,
		      target = 0,
		      correctLines,
		      target = 2 order + 2
		     ];
				 		   	       
	       If[targetFitnessPop[popin, OptionValue[fitnessFunction]] === target, Return[count]];

	       While[targetFitnessPop[popin, OptionValue[fitnessFunction]] =!= target && count < OptionValue[limit],
		     count += 1;
		     popin = swapElementsFromCorrectRowsAll[popin, OptionValue[elitism],
							    OptionValue[fitnessFunction], OptionValue[multi]];
		     popsorted = sortPop[popin, OptionValue[fitnessFunction]];
		     newpop = ParallelTable[buildFromScratch[order], {nInd/2}];
		     popin = Join[popsorted[[1;;nInd/2]], newpop];
		     fitmin = Append[fitmin, minFitnessPop[popin, OptionValue[fitnessFunction]]];
		     fitmax = Append[fitmax, maxFitnessPop[popin, OptionValue[fitnessFunction]]];
		     fitmean = Append[fitmean, meanFitnessPop[popin, OptionValue[fitnessFunction]]];
		     statGen[popin, count, OptionValue[fitnessFunction]];
	       ];

	       deltat = TimeUsed[] - t0;
	       
	       Print["Generazione finale: ", count];
	       Print["Tempo impiegato: ", deltat, " s"];
	       Return[{whoIsTheBest[popin, OptionValue[fitnessFunction]], deltat, {fitmin, fitmax, fitmean}}];
	];

(*Esegue piu' mutazioni*)
mutationMulti[square_List, pm_, multi_Integer:1] :=
	Module[{},
	       Return[Nest[mutationOne[#, pm] &, square, multi]];
	];
		  
(*Produce mutazioni su una popolazione*)
mutationAll[pop_List, pm_, type_:totalSquared, elitism_Integer:0, multi_Integer:1] :=
	Module[{pop2, popsorted, ret},
	       popsorted = sortPop[pop, type];
	       pop2 = Drop[popsorted, elitism];
	       ret = mutationMulti[#, pm, multi] & /@ pop2;
	       ret = Join[popsorted[[1;;elitism]], pop2];
	       Return[ret];
	];
	
(*Elimina dalla popolazione i doppioni*)
purge[pop_List] :=
	Module[{ret, len, order},
		len = Length[pop];
		order = Length[pop[[1]]];
		(*RICHIEDE MATHEMATICA 8*)
		ret = DeleteDuplicates[pop];
		Return[Join[generatePop[len - Length[ret], order], ret]];
	];

(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)
(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)
(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)
(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)
(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)
(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)
(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)
(*TODO INSERIRE LE OPZIONI PER REPRODUCE*)

(*Produce una nuova popolazione a partire da una esistente*)
reproduce[pop_List, criterion_, pc_, pm_, type_:totalSquared, elitism_Integer:0,
	  nfittests_Integer:0, crossover_Integer:1, multi_Integer:1] :=
	Module[{popsorted, mating, mated, new, bests},
	       If[OddQ[elitism],
		  Print["L'elitismo puo' essere solo per un numero pari di individui!"];
		  Abort[];
	       ];	       
	       If[elitism >= Length[pop],
		  Print["Non puoi selezionare un elitismo cosi' elevato!"];
		  Abort[];
	       ];
	       If[OddQ[nfittests],
		  Print["Il numero di riproduzione puo' essere solo per un numero pari di individui!"];
		  Abort[];
	       ];
	       If[nfittests >= Length[pop],
		  Print["Non puoi selezionare un numero di individui cosi' elevato da far riprodurre!"];
		  Abort[];
	       ];
	       (*Se non c'e' crossover*)
	       If[crossover === 0,	  
		  Retrun[purge[mutationAll[pop, pm, elitism]]];
	       ];
	       If[criterion === fittests,
		  popsorted = sortPop[pop, type];
		  mating = popsorted[[1;;nfittests]];
		  mated = mutationAll[crossoverParents[mating, pc, criterion, type, elitism], pm, type, elitism];
		  new = generatePop[Length[pop] - nfittests, Length[pop[[1]]]];
		  Return[purge[Join[new, mated]]];
	       ];
	       Return[purge[mutationAll[crossoverParents[pop, pc, criterion, type, elitism], pm, type, elitism, multi]]];
	]; (*Restituisce una popolazione di nuovi individui*)
	
(*RICHIEDE MATHEMATICA 8*)	
Options[run] = {elitism -> 0, fitnessFunction -> totalSquared, limit->1000,
			    nfittests -> 0, criterion -> fitnessProportionate,
			    crossover -> 1, method -> none}

(*elitism e' il numero di individui conservati di generazione in generazione
  fitnessFunction e' la funzione di fintess da usare
  limit e' il numero massimo di genrazioni
  nfittists (da settare se si usa fittests come criterion e' la dimensione della mating pool
  criterion e' il criterio per definire i pesi di rirpoduzion
  crossover 0 disabilita il crossover
  method per utilizzare un metodo articolato*)

(*Dei valori non male sono nInd = 50, pc = 0.25, pm = 0.75*)

(*Produce popolazioni finche' non arriva un quadrato magico con limite di gen*)
run[nInd_Integer, order_Integer, pc_, pm_, OptionsPattern[]] :=
	Module[{count, popin, fitmin, fitmax, fitmean,
		distance, dists, target, t0, deltat},

	       t0 = TimeUsed[];
	       count = 1;
	       popin = generatePop[nInd, order];
	       
	       If[Optionvalue[criterion] === similarSquare,
		 distance = True;
		 dists = {meanDistPop[popin, OptionValue[fitnessFunction]]},
		 distance = False
	       ];

	       If[OptionValue[criterion] === fittests && OptionValue[nfittests] === 0,
		 Print["Devi impostare un numero di individui da far riprodurre"];
		 Print["Per farlo aggiungi la flag nfittests -> numero"];
		 Abort[];
	      ];       
		   
	       statGen[popin, 1, OptionValue[fitnessFunction], distance];
		   
	       fitmin = {minFitnessPop[popin, OptionValue[fitnessFunction]]};
	       fitmax = {maxFitnessPop[popin, OptionValue[fitnessFunction]]};
	       fitmean = {meanFitnessPop[popin, OptionValue[fitnessFunction]]};

	       (*Aggiornare qui quando si aggiunge fitness*)
	       Switch[OptionValue[fitnessFunction],
		      totalSquared,
		      target = 0,
		      correctLines,
		      target = 2 order + 2,
		      correctRows,
		      target = order
		     ];
				 		   	       
	       If[targetFitnessPop[popin, OptionValue[fitnessFunction]] === target, Return[count]];

	       While[targetFitnessPop[popin, OptionValue[fitnessFunction]] =!= target && count < OptionValue[limit],
		     count += 1;
		     popin = reproduce[popin, OptionValue[criterion], pc, pm,
				       OptionValue[fitnessFunction], OptionValue[elitism],
				       OptionValue[nfittests], OptionValue[crossover]];
		     fitmin = Append[fitmin, minFitnessPop[popin, OptionValue[fitnessFunction]]];
		     fitmax = Append[fitmax, maxFitnessPop[popin, OptionValue[fitnessFunction]]];
		     fitmean = Append[fitmean, meanFitnessPop[popin, OptionValue[fitnessFunction]]];
		     If[distance,
			dists = Append[dists, meanDistPop[popin, OptionValue[fitnessFunction]]];
		     ];
		     statGen[popin, count, OptionValue[fitnessFunction], distance];
	       ];

	       deltat = TimeUsed[] - t0;
	       
	       Print["Generazione finale: ", count];
	       Print["Tempo impiegato: ", deltat, " s"];
	       If[distance,
		  Return[{whoIsTheBest[popin, OptionValue[fitnessFunction]], deltat, {fitmin, fitmax, fitmean, dists}}],
		  Return[{whoIsTheBest[popin, OptionValue[fitnessFunction]], deltat, {fitmin, fitmax, fitmean}}]
	       ]
	];

(*Controlla se una lista ha due volte uno stesso elemento*)
checkIfValidLine[row_List] :=
	Module[{order},
	       order = Length[row];
	       If[Length[row] === Length[Tally[row]],
		  If[Length[DeleteCases[row, x_ /; x > order^2]] =!= Length[row],
		     Return[False],
		     Return[True] (*Valido*)
		  ],
		  Return[False] (*Invalido*)
	       ];
	];

(*Applica una permutazione random ad una linea*)
randomPermutation[row_List] :=
	Module[{},
	       Return[RandomChoice[Permutations[row]]];
	];

(*Costruisce tutti i possibili quadrati con righe perfette*)
buildFromScratch[order_Integer] :=
	Module[{parts, mn, goods, used, n, ret, i},
	       If[order > 7,
		  Print["Attualmente questa funzione gira con al massimo ordine 7"];
		  Abort[];
	       ];
	       mn = magicNumber[order];
	       parts = IntegerPartitions[mn, {order}];
	       goods = DeleteCases[parts, x_ /; checkIfValidLine[x] === False];
	       goods = randomPermutation /@ goods;
	       n = Random[Integer, {1, Length[goods]}];
	       ret = Table[1, {order}];
	       used = {};
	       i = 1;
	       While[i <= order,
		     While[Length[goods] =!= 0,
			   n = Random[Integer, {1, Length[goods]}];
			   If[Length[Intersection[goods[[n]], used]] === 0,
			      Break[];
			      Break[],
			      goods = Drop[goods, {n}];
			   ];
		     ];
		     (*TODO: FARE QUALCOSA ANCHE QUANDO NON FUNZIONA*)
		     If[Length[goods] === 0,
			Print["La costruzione non ha funzionato"];
			Abort[];
		     ];
		     ret[[i]] = goods[[n]];
		     used = Flatten[Append[used, ret[[i]]]];
		     i += 1;
	       ];
	       Return[ret];
	];


(*
__  ___            _  __                 
\ \/ (_) ___      | |/ /__ _ _ __   __ _ 
 \  /| |/ _ \_____| ' // _` | '_ \ / _` |
 /  \| |  __/_____| . \ (_| | | | | (_| |
/_/\_\_|\___|     |_|\_\__,_|_| |_|\__, |
                                   |___/ 
    _    _                  _ _   _               
   / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  
  / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \ 
 / ___ \| | (_| | (_) | |  | | |_| | | | | | | | |
/_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|
           |___/
 *)

(*Segue l'impelementazione dell'argoritmo di Xie Kang*)
(*Maggiori informazioni disponibili al link:
http://ieeexplore.ieee.org/xpl/abstractAuthors.jsp?arnumber=1299763 *)

(*Da ora in poi l'elemento base e' un individuo, non un quadrato*)

(*Questo modulo genera e inizializa un individuo, cioe' una coppia matrice
  del quadrato + matrice delle deviazioni*)
(*La matrice delle deviazioni di default e' inizializzata con il valore 3*)
(*TODO: La funzione che genera quadrati random deve essere ottimizzata!!!*)
generateInd[order_Integer] :=
	Module[{},
	     Return[{generateSquare[order], ParallelTable[order^2, {order}, {order}]}];
	];

(*Eseguo queste funzioni in parallelo per gli individui grossi*)

(*Calcola somme delle righe di un individuo*)
(*Total e' molto piu' efficiente di Plus @@ *)
rowsTotalInd[ind_List] :=
	Module[{},
	       Return[ParallelTable[Total[ind[[1,i]]], {i,1,Length[ind[[1]]]}]];
	];

(*Calcola somme colonne di un individuo*)
columnsTotalInd[ind_List] :=
	Module[{},
	       Return[Table[Total[Transpose[ind[[1]]][[i]]], {i,1,Length[ind[[1]]]}]];
	];

(*Calcola somme diagonali di un individuo*)
diagonalsTotalInd[ind_List] :=
	Module[{},
	       Return[{ParallelSum[ind[[1,i,i]],{i,1,Length[ind[[1]]]}],
		       ParallelSum[ind[[1,-i,i]],{i,1,Length[ind[[1]]]}]}];
	];

(*Calcola somme diagonali di un quadrato*)
diagonalsTotal[square_List] :=
	Module[{},
	       Return[{ParallelSum[square[[i,i]],{i,1,Length[square]}],
		       ParallelSum[square[[-i,i]],{i,1,Length[square]}]}];
	];

(*Calcola il numero di righe non corrette in un individuo*)
incorrectRows[ind_List] :=
	Module[{},
	       (*Conto le righe che sottranendo il numero magico danno 0*)
	       Return[Length[ind[[1]]] - Count[rowsDeviation[ind], 0]];
	];

(*Calcola il numero di colonne non corrette in un individuo*)
incorrectColumns[ind_List] :=
	Module[{},
	       (*Conto le colonne che sottranendo il numero magico danno 0*)
	       Return[Length[ind[[1]]] - Count[columnsDeviation[ind], 0]];
	];

(*Calcola il numero di diagonali non corrette in un individuo*)
incorrectDiagonals[ind_List] :=
	Module[{},
	       (*Conto le diagonali che sottranendo il numero magico danno 0*)
	       Return[2 - Count[diagonalsDeviation[ind], 0]];
	];

(*Restituisce una lista contenete la differenza tra la righe e il valore magico*)
rowsDeviation[ind_List] :=
	Module[{mv},
	       mv = magicNumber[Length[ind[[1]]]];
	       Return[Abs[rowsTotalInd[ind] - mv]];
	];

(*Restituisce una lista contenete la differenza tra le colonne e il valore magico*)
columnsDeviation[ind_List] :=
	Module[{mv},
	       mv = magicNumber[Length[ind[[1]]]];
	       Return[Abs[columnsTotalInd[ind] - mv]];
	];

(*Restituisce una lista contenete la differenza tra le diagonali e il valore magico*)
diagonalsDeviation[ind_List] :=
	Module[{mv},
	       mv = magicNumber[Length[ind[[1]]]];
	       Return[Abs[diagonalsTotalInd[ind] - mv]];
	];

(*Funzione di fitness di un individuo*)
fitnessInd[ind_List] :=
	Module[{inclin},
	       inclin = incorrectRows[ind] + incorrectColumns[ind];
	       If[inclin === 0,
		  Return[Total[diagonalsDeviation[ind]]],
		  Return[Total[rowsDeviation[ind] + columnsDeviation[ind]]];
	       ];
	];

(*Funzione di fitness di una popolazione di individui*)
fitnessPopInd[pop_List] :=
	Module[{},
	       Return[fitnessInd /@ pop];
	];

(*Seleziona il figlio migliore*)
fittestChild[pop_List] :=
	Module[{fp},
	       fp = fitnessPopInd[pop];
	       Return[pop[[Position[fp, Min[fp]][[1,1]]]]];
	];

(*Calcola il valore di sigma t, che e' un parametro che serve per la nuova
  varianza dopo le mutazioni*)
sigmat[ind_List] :=
	Module[{nincorrect},
	       nincorrect = incorrectRows[ind] + incorrectColumns[ind];
	       If[nincorrect =!= 0,
		  Return[Total[rowsDeviation[ind]] + Total[columnsDeviation[ind]] /
						     nincorrect],
		  Return[Total[diagonalsDeviation[ind]]/incorrectDiagonals[ind]];
	       ];
	];

(*Esegue le mutazioni su un individuo*)
mutateInd[ind_List] :=
	Module[{order, S1, S2, mutnum, incorrectrows, incorrectcolumns, pm, i, j,
		ret, incorrectdiagonals, inclin, ran, subs, S2r, S2c, rows, cols,
		temp, sig, minlist, min, sc, sigt},
	       
	       order = Length[ind[[1]]];

	       incorrectrows = incorrectRows[ind];
	       incorrectcolumns = incorrectColumns[ind];
	       inclin = incorrectrows + incorrectcolumns;
	       (*inclin sono le linee incorrette*)
	       incorrectdiagonals = incorrectDiagonals[ind];

	       ret = ind[[1]];
	       sig = ind[[2]];

	       (*Se tutte le righe e le colonne sono ok faccio solo permutazioni
                 di linee a caso*)
	       If[inclin === 0 && incorrectdiagonals === 0, Return[{ret, ind[[2]]}]];
	       If[inclin === 0 && incorrectdiagonals =!= 0,
		  mutnum = Random[Integer, {1,2}]; (*1 scambio righe, 2 colonne*)
		  Switch[mutnum,
			 1, (*Permuto righe*)
			 ran = RandomInteger[{1, order}];
			 subs = {ret[[ran[[1]]]] -> ret[[ran[[2]]]],
				 ret[[ran[[2]]]] -> ret[[ran[[1]]]]};
			 Return[ret /. subs],
			 2, (*Permuto colonne*)
			 ran = RandomInteger[{1, order}];
			 ret = Transpose[ret];
			 subs = {ret[[ran[[1]]]] -> ret[[ran[[2]]]],
				 ret[[ran[[2]]]] -> ret[[ran[[1]]]]};
			 Return[Transpose[ret /. subs]]
		  ];
	       ];

	       (*ATTUALMENTE C'E' UNA SOLA MUTAZIONE E LE PROBABILITA'
                 SONO DA SISTEMARE*)
	       
	       If[inclin =!= 0,
	          rows = rowsDeviation[ind];
	          cols = columnsDeviation[ind];
		  S2r = ParallelTable[If[rows[[i]] =!= 0,
					 ret[[i,j]], (*Il numero se e' in S2r*)
				         0], (*0 se non e' in S2r*)
				      {i, 1, order},
				      {j, 1, order}];
		  S2r = Flatten[S2r];
		  S2r = DeleteCases[S2r, 0];
		  S2c = ParallelTable[If[cols[[j]] =!= 0,
					 ret[[i,j]], (*Il numero se e' in S2c*)
				         0], (*0 se non e' in S2c*)
				      {i, 1, order},
				      {j, 1, order}];
		  S2c = Flatten[S2c];
		  S2c = DeleteCases[S2c, 0];
		  (*set1 e' l'insieme dei numeri la cui colonna e riga non e' magica*)
		  S1 = Intersection[S2r, S2c];
		  (*set2 e' l'insieme dei numeri in righe o colonne non magiche*)
		  S2 = Union[S2r, S2c];
		  (*Seleziona una mutazione casuale*)
		  mutnum = Random[Integer, {1,3}];
		  Switch[mutnum,
			 1, (*S1 in S2*)
			 pm = N[1/(incorrectrows incorrectcolumns)];
			 For[i = 1, i <= order, i++,
			     For[j = 1, j <= order, j++,
				 If[Count[S1, ret[[i,j]]] =!= 0,
				    If[Random[] <= pm,
				       (*Trovo il nuovo valore*)
				       temp = ret[[i,j]] +
					      RandomInteger[{-sig[[i,j]], sig[[i,j]]}];
				       If[temp < 1,
					  temp = Random[Integer, {1, order}];
				       ];
				       If[temp > order^2,
					  temp = order^2 - Random[Integer, {0, order}];
				       ];
				       (*Trovo con chi scambiarlo*)
				       minlist = ParallelMap[Abs[# - temp] &, S2];
				       min = Min[minlist];
				       sc = S2[[Position[minlist, min][[1,1]]]];
				       (*Faccio lo scambio*)
				       subs = {ret[[i,j]] -> sc, sc -> ret[[i,j]]};
				       ret = Replace[ret, subs, 2];
				       (*Aggiusto le varianze*)
				       sig[[i,j]] = sig[[i,j]] + RandomInteger[{-1,1}];
				       If[sig[[i,j]] < 1 || sig[[i,j]] > order^2,
					  sigt = sigmat[{ret, ind[[2]]}];
					  sig[[i,j]] = Random[Integer, {1, sigt}];
				       ];
				       Return[{ret, sig}];
				    ];
				 ];
			     ];
			 ];
			 Return[{ret, sig}],
			 2, (*S2 in S2*)
			 pm = N[1/(incorrectrows incorrectcolumns)];
			 For[i = 1, i <= order, i++,
			     For[j = 1, j <= order, j++,
				 If[Count[S2, ret[[i,j]]] =!= 0,
				    If[Random[] <= pm,
				       (*Trovo il nuovo valore*)
				       temp = ret[[i,j]] +
					      RandomInteger[{-sig[[i,j]], sig[[i,j]]}];
				       If[temp < 1,
					  temp = Random[Integer, {1, order}];
				       ];
				       If[temp > order^2,
					  temp = order^2 - Random[Integer, {0, order}];
				       ];
				       (*Trovo con chi scambiarlo*)
				       minlist = ParallelMap[Abs[# - temp] &, S2];
				       min = Min[minlist];
				       sc = S2[[Position[minlist, min][[1,1]]]];
				       (*Faccio lo scambio*)
				       subs = {ret[[i,j]] -> sc, sc -> ret[[i,j]]};
				       ret = Replace[ret, subs, 2];
				       (*Aggiusto le varianze*)
				       sig[[i,j]] = sig[[i,j]] + RandomInteger[{-1,1}];
				       If[sig[[i,j]] < 1 || sig[[i,j]] > order^2,
					  sigt = sigmat[{ret, ind[[2]]}];
					  sig[[i,j]] = Random[Integer, {1, sigt}];
				       ];
				       Return[{ret, sig}];
				    ];
				 ];
			     ];
			 ];
			 Return[{ret, sig}],
			 3, (*S2 in tutto*)
			 pm = N[1/(incorrectrows incorrectcolumns)];
			 For[i = 1, i <= order, i++,
			     For[j = 1, j <= order, j++,
				 If[Random[] <= pm,
				    (*Trovo il nuovo valore*)
				    temp = ret[[i,j]] +
					   RandomInteger[{-sig[[i,j]], sig[[i,j]]}];
				    If[temp < 1,
				       temp = Random[Integer, {1, order}];
				    ];
				    If[temp > order^2,
				       temp = order^2 - Random[Integer, {0, order}];
				    ];
				    (*Trovo con chi scambiarlo*)
				    minlist = ParallelMap[Abs[# - temp] &, S2];
				    min = Min[minlist];
				    sc = S2[[Position[minlist, min][[1,1]]]];
				    (*Faccio lo scambio*)
				    subs = {ret[[i,j]] -> sc, sc -> ret[[i,j]]};
				    ret = Replace[ret, subs, 2];
				    (*Aggiusto le varianze*)
				    sig[[i,j]] = sig[[i,j]] + RandomInteger[{-1,1}];
				    If[sig[[i,j]] < 1 || sig[[i,j]] > order^2,
				       sigt = sigmat[{ret, ind[[2]]}];
				       sig[[i,j]] = Random[Integer, {1, sigt}];
				    ];
				    Return[{ret, sig}];
				 ];
			     ];
			 ];
			 Return[{ret, sig}];
		  ];
	       ];	       
	];

(*Rettificazione ad una coppia*)

(*Scambia due elementi che sono in righe diverse ma nella
  stessa colonna se cio' porta alla somma magica*)
rectifyRowsWithOnePair[ind_List] :=
	Module[{mv, order, rows, subs, ret, i1, i2, j1},
	       order = Length[ind[[1]]];
	       mv = magicNumber[order];
	       rows = rowsTotalInd[ind];
	       ret = ind[[1]];
	       (*QUESTO NON E' UN MODO ELEGANTE PER FARLO*)
	       (*Elemento a_{i1,J1}*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       For[i2 = i1 + 1, i2 <= order, i2++, (*Scorro la colonna*)
			   If[(rows[[i1]] - mv) === (mv - rows[[i2]]) &&
			     (ret[[i1, j1]] - ret[[i2, j1]] === rows[[i1]] - mv),
			      subs = {ret[[i1, j1]] -> ret[[i2, j1]],
				      ret[[i2, j1]] -> ret[[i1, j1]]};
			      ret = ret /. subs;
			      (*Bisogna aggiornare il valore delle somme*)
			      rows[[i1]] = Total[ret[[i1]]];
			      rows[[i2]] = Total[ret[[i2]]];
			   ];
		       ];
		   ];   
	       ];
	       Return[{ret, ind[[2]]}];
	];

(*Scambia due elementi che sono in colonne diverse ma nella
  stessa riga se cio' porta alla somma magica*)
rectifyColumnsWithOnePair[ind_List] :=
	Module[{trind, rect, ret},
	       trind = {Transpose[ind[[1]]], ind[[2]]};
	       rect = rectifyRowsWithOnePair[trind];
	       ret = {Transpose[rect[[1]]], ind[[2]]};
	       Return[ret];
	];


(*Rettificazioni a due coppie*)

(*Scambia due coppie che sono in righe diverse e in
  colonne diverse se cio' porta alla somma magica*)
rectifyRowsWithTwoPairs[ind_List] :=
	Module[{mv, order, rows, subs, ret, i1, i2, j1, j2},
	       order = Length[ind[[1]]];
	       mv = magicNumber[order];
	       rows = rowsTotalInd[ind];
	       ret = ind[[1]];
	       (*QUESTO NON E' UN MODO ELEGANTE PER FARLO*)
	       (*Elemento a_{i1,J1}*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       For[i2 = i1 + 1, i2 <= order, i2++, (*Scorro la colonna*)
			   For[j2 = j1 + 1, j2 <= order, j2++,
			       If[(rows[[i1]] - mv) === (mv - rows[[i2]]) &&
				  (ret[[i1, j1]] + ret[[i1, j2]]
				   - ret[[i2, j2]] - ret[[i2, j1]]
                                   === rows[[i1]] - mv),
				  subs = {ret[[i1, j1]] -> ret[[i2, j2]],
					  ret[[i2, j2]] -> ret[[i1, j1]],
					  ret[[i1, j2]] -> ret[[i2, j1]],
					  ret[[i2, j1]] -> ret[[i1, j2]]};
				  ret = ret /. subs;
				  rows[[i1]] = Total[ret[[i1]]];
				  rows[[i2]] = Total[ret[[i2]]];
			       ];
			   ];
		       ];
		   ];
	       ];
	       Return[{ret, ind[[2]]}];
	];

(*Scambia due coppie che sono in colonne diverse e in
  righe diverse se cio' porta alla somma magica*)
rectifyColumnsWithTwoPairs[ind_List] :=
	Module[{trind, rect, ret},
	       trind = {Transpose[ind[[1]]], ind[[2]]};
	       rect = rectifyRowsWithTwoPairs[trind];
	       ret = {Transpose[rect[[1]]], ind[[2]]};
	       Return[ret];
	];

(*Sistema le diagonali con scambi intelligenti*)
rectifyDiagonals[ind_List] :=
	Module[{order, ret, j1, j2, diag1, diag2, temp},
	       order = Length[ind[[1]]];
	       mv = magicNumber[order];
	       {diag1, diag2} = diagonalsTotalInd[ind];
	       ret = ind[[1]];
	       (*QUESTO NON E' UN MODO ELEGANTE PER FARLO*)
	       (*Prima rettificazione*)
	       (*Elemento a_{i1,J1}*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       If[i1 =!= j1, 		       
			  If[(ret[[i1, i1]] + ret[[i1, j1]] ===
                              ret[[j1, j1]] + ret[[j1, i1]]) &&
			     (ret[[i1, i1]] + ret[[j1, j1]] -
			      ret[[i1, j1]] - ret[[j1, i1]] === diag1 - mv),
			     subs = {ret[[i1, i1]] -> ret[[j1, i1]],
				     ret[[j1, i1]] -> ret[[i1, i1]],
				     ret[[i1, j1]] -> ret[[j1, j1]],
				     ret[[j1, j1]] -> ret[[i1, j1]]};
			     ret = ret /. subs;
			     {diag1, diag2} = diagonalsTotal[ret];
			  ];
		       ];
		   ];   
	       ];
	       (*Seconda rettificazione*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       If[i1 =!= j1, 		       
			  If[(ret[[i1, j1]] +
			      ret[[i1, order - i1 + 1]]
                              === ret[[order - j1 + 1, j1]] +
			      ret[[order - j1 + 1 , order - i1 + 1]]) &&
			     (ret[[i1, order - i1 + 1]] +
			      ret[[order - j1 +1, j1]] -
			      ret[[i1, j1]] -
			      ret[[order - j1 + 1, order - i1 +1]]
                              === diag2 - mv),
			     subs = {ret[[i1, j1]] ->
				     ret[[order - j1 +1, j1]],
				     ret[[order - j1 +1, j1]] ->
				     ret[[i1, j1]],
				     ret[[i1, order - i1 + 1]] ->
				     ret[[order - j1 + 1, order - i1 +1]],
				     ret[[order - j1 + 1, order - i1 +1]] ->
			             ret[[i1, order - i1 + 1]]};
			     ret = ret /. subs;
			     {diag1, diag2} = diagonalsTotal[ret];
			  ];
		       ];
		   ];   
	       ];

	       (*NON TESTATA!!!!*)
	       (*Terza rettificazione*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       If[i1 =!= j1, 
			  If[(ret[[i1, i1]] + ret[[j1, j1]] -
			      ret[[i1, j1]] - ret[[j1, i1]]
                              === diag1 - mv) &&
			     (ret[[i1, order - i1 + 1]] +
			      ret[[j1, order - j1 + 1]] -
			      ret[[i1, order - j1 + 1]] -
			      ret[[j1, order - i1 + 1]]
                              === diag2 - mv),
			     subs = {ret[[i1]] -> ret[[j1]],
				     ret[[j1]] -> ret[[i1]]};
			     ret = ret /. subs;
			     {diag1, diag2} = diagonalsTotal[ret];
			     If[incorrectDiagonals[ret] === 0,
				Print["TERZA RETTIFICAZIONE!!!"];
			     ];
			  ];
		       ];
		   ];   
	       ];

	       (*NON TESTATA!!!!*)
	       (*Quarta rettificazione*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       If[i1 =!= j1, 
			  If[(ret[[i1, i1]] + ret[[j1, j1]] -
			      ret[[i1, j1]] - ret[[j1, i1]]
                              === diag1 - mv) &&
			     (ret[[order - i1 + 1, i1]] +
			      ret[[order - j1 + 1, j1]] -
			      ret[[order - j1 + 1, i1]] -
			      ret[[order - i1 + 1, j1]]
                              === diag2 - mv),
			     temp = Transpose[ret];
			     subs = {temp[[i1]] -> temp[[j1]],
				     temp[[j1]] -> temp[[i1]]};
			     temp = temp /. subs;
			     ret = Transpose[temp];
			     {diag1, diag2} = diagonalsTotal[ret];
			     If[incorrectDiagonals[ret] === 0,
				Print["QUARTA RETTIFICAZIONE!!!"];
			     ];
			  ];
		       ];
		   ];   
	       ];

	       (*Quinta rettificazione*)
	       (*NON TESTATA!!!*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       If[i1 =!= j1, 
			  If[(ret[[i1, i1]] +
			      ret[[order - i1 + 1, order - i1 + 1]] -
			      ret[[i1, order - i1 + 1]] -
			      ret[[order -i1 + 1, i1]]
                              === diag1 - mv) &&
			     (diag1 - mv  === mv - diag2),
			     subs = {ret[[i1]] -> ret[[order - i1 + 1]],
				     ret[[order -i1 + 1]] -> ret[[i1]]};
			     ret = ret /. subs;
			     {diag1, diag2} = diagonalsTotal[ret];
			     If[incorrectDiagonals[ret] === 0,
				Print["QUINTA RETTIFICAZIONE!!!"];
			     ];
			  ];
		       ];
		   ];   
	       ];	       
	       
	       Return[{ret, ind[[2]]}];
	];


xiekang[order_Integer] :=
	Module[{father, offspring, gen, fitbest, timings, fittest},

	       (*timings contiene gli intervalli temporali*)
	       
	       Print["Stai eseguendo l'algoritmo Xie-Kang per costruire un quadrato
                      di ordine ", order];
	       
	       Print["Genero il capostipide di tutti i quadrati"];
	       timings[0] = TimeUsed[];
	       father = generateInd[order];
	       timings[1] = TimeUsed[];
	       Print["Quadrato generato in ", timings[1] - timings[0], " s"];

	       If[fitnessInd[father] === 0,
		  Print["Ho costruito un quadrato magico in ",
			TimeUsed[] - timings[1], " s"];
		  Return[father[[1]]];
	       ];

	       (*Inizializzo una figliata di 10 elementi uguali al padre*)
	       gen = 1;
	       fitbest = fitnessInd[father];

	       Print["Il padre ha fitness ", fitbest];
	       
	       While[fitbest > 0,
		     offspring = ParallelTable[father, {10}];
		     Print["Siamo alla generazione ", gen];
		     Print["Effettuo mutazioni"];
		     timings[2] = TimeUsed[];
		     offspring = mutateInd /@  offspring;
		     Print["Mutazioni effettuate in ", TimeUsed[] - timings[2], " s"];
		     fittest = fittestChild[offspring];
		     fitbest = fitnessInd[fittest];
		     Print["Il miglior figlio ha fitness ", fitbest];
		     If[fitbest < 50 order,
			Print["Il miglior figlio e' promettente, lo rettifico"];
			timings[3] = TimeUsed[];
			fittest = rectifyRowsWithOnePair[fittest];
			fittest = rectifyRowsWithTwoPairs[fittest];
			fittest = rectifyColumnsWithOnePair[fittest];
			fittest = rectifyColumnsWithTwoPairs[fittest];	
			Print["Rettificazione effettuata in ", TimeUsed[] - timings[3],
			      " s"];
			fitbest = fitnessInd[fittest];
			Print["Ora il miglior figlio ha fitness ", fitbest];
		     ];
		     If[fitbest < 100,
			Print["Il miglior figlio e' estremamente buono, lo rettifico"];
			timings[4] = TimeUsed[];
			fittest = rectifyDiagonals[fittest];
			Print["Rettificazione effettuata in ", TimeUsed[] - timings[4],
			      " s"];
			fitbest = fitnessInd[fittest];
			Print["Ora il miglior figlio ha fitness ", fitbest];
		     ];
		     If[incorrectRows[fittest] + incorrectColumns[fittest] =!= 0,
			If[fitbest > 50 order,
			   father = fittestChild[Append[offspring, father]],
			   father = fittest;
			],
			(*Se le linee sono a posto*)
			If[incorrectDiagonals[fittest] === 0,
			   (*Se le diagonali sono a posto*)
			   Print["TROVATO!"];
			   Print["Ho costruito un quadrato magico in ",
				 TimeUsed[] - timings[1], " s"];
			   Return[fittest[[1]]],
			   (*Se non sono a posto*)
			   If[fitbest > 100,
			      father = fittest,
			      father = fittestChild[Append[offspring, father]];
			   ];
			]			
		     ];
		     gen += 1;
	       ];
	];
