(*#!/usr/local/bin/math*)

(*Magic Mathematica Square*)

(*Costruisce quadrati magici di quasi qualsiasi dimensione*)

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

(*Si richiede Mathematica 8 per via di alcuni metodi di parallelizzazione
  utilizzati e di alcuni funzioni random*)
If[$VersionNumber < 8, Print["Questo programma richiede Mathematica 8"]];

(*
  ____                 _   _      
 / ___| ___ _ __   ___| |_(_) ___ 
| |  _ / _ \ '_ \ / _ \ __| |/ __|
| |_| |  __/ | | |  __/ |_| | (__ 
 \____|\___|_| |_|\___|\__|_|\___|
                                  
    _    _                  _ _   _               
   / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  
  / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \ 
 / ___ \| | (_| | (_) | |  | | |_| | | | | | | | |
/_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|
           |___/                                  

 *)

(*Messaggio di benvenuto*)
(*Print["Benveuto in MagicMathematicaSquare"]*)
(*Print["Ecco le funzioni che puoi utilizzare:"]
Print["run[nInd_Integer, order_Integer, pc_, pm_,
criterion->fintessProportionate, limit->1000, elitism->0, fitnessFunction -> totalSquared]"]
Print["L'output prodotto e': {whoIsTheBest}, timeused, {fitmin, fitmax, fitmean}"];*)
		 
(*Calcola il numero magico*)
magicNumber[n_Integer] :=
	Module[{},
	       Return[n (n n +1) / 2];
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
(*type e' il tipo di fintess da usare*)
(*Di default viene usata totalSquared*)
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
	        (*Serve per generare risultati intermedi*)    
		      correctRows,
		      Return[Count[{row}, 0, Infinity]],
		   
		      _, (*Evita che il criterio non sia definito*)
		      Print["La funzione di fitness ", type,
			    " non e' ancora stata implementata"];
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

(*Restituisce l'individuo con fitness migliore di una popolazione*)
(*Dipenda dalla fitness adottata*)
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

(*Fitness media di una popolazione*)
meanFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       (*N per performance*)
	       Return[N[Mean[fitnessPop[pop, type]]]];
	];

(*Deviazione standard del fitness di una popolazione*)
devstdFitnessPop[pop_List, type_:totalSquared] :=
	Module[{},
	       (*N per performance*)
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
	       If[type === totalSquared || type == totalAbs,
		  Return[SortBy[pop, fitness[#, type] &]],
		  (*correctLines e' ordinata al contrario
                    in quanto il migliore e' quello con piu'
                    linee a posto*)
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
		      (*In questi due metodi il migliore
                        ha fitness maggiore*)
		      correctLines,
		      Return[maxFitnessPop[pop, type]],
		      correctRows,
		      Return[maxFitnessPop[pop, type]]
	       ];
	];

(*Statistiche di fitness di una popolazione*)
(*distance e' un bool che dipende da se si sta
  usando la nozione di distanza oppure no*)
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
(*Metodo usato solo per comodita'*)
statGen[pop_List, gen_Integer, type_:totalSquared, distance_:False] :=
	Module[{},
	       Print["Siamo alla generazione ", gen];
	       statPop[pop, type, distance];
	];

(*Ripartisci intervallo*)
divideInterval[pop_List, criterion_, type_:totalSquared] :=
	Module[{len, inv, inf, best, dists, fits, upfits, distmean},
	       Switch[criterion,
		   
		      fitnessProportionate, (*If criterion === FitnessProportionate*)
		      fits = fitnessPop[pop, type];
		      (*Aggiugnere qui le nuove fitness*)
		      Switch[type,
			     (*Dove l'individuo migliore e' quello
                              che fitness minima si usano i reciproci*)
			     totalSquared,
			     (*N per performance*)
			     inv = N[#^(-1) & /@ fits, 9],
		             totalAbs,
		             inv = N[#^(-1) & /@ fits, 9],
	                     correctLines,
			     inv = fits,
			     correctRows,
			     inv = fits
		      ];
		      (*Lunghezza dell'intervallo*)
		      inf = Plus @@ inv;
		      (*Lunghezza di ogni intervallino associato ad un individuo*)
		      len = inv/inf;
		      Return[Table[Sum[len[[i]],{i,1,j}],{j,1,Length[pop]}]],

		      (*Si premiano individui vicini*)
		      similarSquare,
		      best = whoIsTheBest[pop, type];
		      dists = permDist[best, #] & /@ pop;
		      distmean = Mean[dists];
		      dists = dists / distmean;
		      fits = fitnessPop[pop, type];
		      (*Le fitness sono pesate anche dalla distanza dal migliore*)
		      upfits = Table[fits[[i]](1 + dists[[i]]), {i,1,Length[pop]}];
		      (*Aggiungere qui le nuove fintess*)
		      Switch[type,
			     (*Dove l'individuo migliore e' quello
                              che fitness minima si usano i reciproci*)
			     totalSquared,
			     inv = N[#^(-1) & /@ upfits, 9],
		             totalAbs,
		             inv = N[#^(-1) & /@ upfits, 9],
	                     correctLines,
			     inv = upfits,
			     correctRow,
			     inv = upfits
		      ];
		      (*Lunghezza dell'intervallo*)
		      inf = Plus @@ inv;
		      (*Lunghezza di ogni intervallino associato ad un individuo*)
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
		       (*Estrae casualmente*)
		       Length[Select[divideInterval[pop, criterion, type],
				     (# < r) &]] + 1,
		       {Length[pop]}
		      ]
	       ];
	]; (*Restituisce una lista di individui*)

(*Produce un quadrato da una lista*)
(*Utile quando si srotola il quadrato*)
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
	       (*Quali numeri ci sono gia'?*)
	       accept = DeleteCases[numbers, x_ /; isThere[insq, x]];
	       (*Quali ho gia' provato?*)
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
	       Return[Part[Permutations[square[[n]]],
			   Random[Integer, {1, Length[square]!}]]];
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
			 (*Punti di taglio*)
			 where1 = Random[Integer, {2,Length[p1] - 2}];
			 where2 = Random[Integer, {where1,Length[p1] - 1}];
			 (*Genero i figli*)
			 c11 = Take[p1, {1, where1}];
			 c12 = Take[p1, {where1 + 1, where2}];
			 c13 = Take[p1, {where2 + 1, Length[p1]}];
			 c21 = Take[p2, {1, where1}];
			 c22 = Take[p2, {where1 + 1, where2}];
			 c23 = Take[p2, {where2 + 1, Length[p2]}];
			 (*Controllo che non ci siano invalidi*)
			 j1 = deleteDouble[listToSquare[Join[c11, c22, c13]]];
			 j2 = deleteDouble[listToSquare[Join[c21, c12, c23]]];
			 Return[{j1, j2}],
			 (*Se non muto*)
			 Return[parents];
		      ],

		      correctLines,
		      
		      (*Questo crossover scambia due linee buone tra gli individui*)
		      If[Random[] <= pc,
			 (*Trovo linee buone*)
			 goodrows1 = goodRows[parents[[1]]];
			 goodrows2 = goodRows[parents[[2]]];
			 goodcols1 = goodColumns[parents[[1]]];
			 goodcols2 = goodColumns[parents[[2]]];
			 (*Numero di righe e colonne magiche*)
			 c11 = Plus @@ goodrows1;
			 c12 = Plus @@ goodrows2;
			 c21 = Plus @@ goodcols1;
			 c22 = Plus @@ goodcols2;
			 (*Per avere un trattamento simmetrico tra righe e colonne*)
			 (*Scelgo o di scambiare righe o di scambiare colonne*)
			 ran = Random[Integer];
			 Switch[ran,
				0,
				(*Se ci sono righe buone in entrambi*)
				If[c11 c12 > 0,
				   (*Quali scambio?*)
				   where1 = Random[Integer, {1, c11}];
				   where2 = Random[Integer, {1, c12}];
				   ps1 = Position[goodrows1, 1][[where1, 1]];
				   ps2 = Position[goodrows2, 1][[where2, 1]];
				   (*Produco figli*)
				   j1 = swapRows[parents[[1]],
						 parents[[2]], ps1, ps2][[1]];
				   j2 = swapRows[parents[[1]],
						 parents[[2]], ps1, ps2][[2]];
				   (*Controllo che non ci siano invalidi*)
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
				   Return[{j1, j2}];
				];
				(*Se ci sono colonne buone in entrambi*)
				If[c21 c22 > 0,
				   (*Quali scambio?*)
				   where1 = Random[Integer, {1, c21}];
				   where2 = Random[Integer, {1, c22}];
				   ps1 = Position[goodcols1, 1][[where1, 1]];
				   ps2 = Position[goodcols2, 1][[where2, 1]];
				   (*Produco figli*)
				   j1 = swapColumns[parents[[1]],
						    parents[[2]], ps1, ps2][[1]];
				   j2 = swapColumns[parents[[1]],
						    parents[[2]], ps1, ps2][[2]];
				   (*Controllo che non ci siano invalidi*)
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
 				   Return[{j1, j2}];
				],
				1,				
				(*Se ci sono righe buone in entrambi*)
				If[c21 c22 > 0,
				   (*Quali scambio?*)
				   where1 = Random[Integer, {1, c21}];
				   where2 = Random[Integer, {1, c22}];
				   ps1 = Position[goodcols1, 1][[where1, 1]];
				   ps2 = Position[goodcols2, 1][[where2, 1]];
				   (*Produco figli*)
				   j1 = swapColumns[parents[[1]],
						    parents[[2]], ps1, ps2][[1]];
				   j2 = swapColumns[parents[[1]],
						    parents[[2]], ps1, ps2][[2]];
				   (*Controllo che non ci siano invalidi*)
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
				   Return[{j1, j2}];
				];
				(*Se ci sono colonne buone in entrambi*)
				If[c11 c12 > 0,
				   (*Quali scambio?*)
				   where1 = Random[Integer, {1, c11}];
				   where2 = Random[Integer, {1, c12}];
				   ps1 = Position[goodrows1, 1][[where1, 1]];
				   ps2 = Position[goodrows2, 1][[where2, 1]];
				   (*Produco figli*)
				   j1 = swapRows[parents[[1]],
						 parents[[2]], ps1, ps2][[1]];
				   j2 = swapRows[parents[[1]],
						 parents[[2]], ps1, ps2][[2]];
				   (*Controllo che non ci siano invalidi*)
				   j1 = deleteDouble[j1];
				   j2 = deleteDouble[j2];
				   Return[{j1, j2}];
				]
			 ];
			 Return[parents],
			 (*Se non muto*)
			 Return[parents];
		      ],

		      totalAbs,
		      
		      If[Random[] <= pc,
			 (*Crossover ad un punto*)
			 (*Srotolo la struttura a quadrato*)
			 order = Length[parents[[1]]];
			 p1 = Flatten[parents[[1]],1];
			 p2 = Flatten[parents[[2]],1];
			 (*Trovo punto di taglio*)
			 where1 = Random[Integer, {1,Length[p1] - 1}];
		         (*Produco figli*)
			 c11 = Take[p1, {1, where1}];
			 c12 = Take[p1, {where1 + 1, Length[p1]}];
			 c21 = Take[p2, {1, where1}];
			 c22 = Take[p2, {where1 + 1, Length[p2]}];
			 (*Controllo che non ci siano invalidi*)
			 j1 = deleteDouble[listToSquare[Join[c11,c22]]];
			 j2 = deleteDouble[listToSquare[Join[c21,c12]]];
			 Return[{j1, j2}],
			 (*Se non muto*)
			 Return[parents];
		      ],

		      correctRows,
		      If[Random[] <= pc,
			 (*Crossover ad un punto su righe*)
			 (*Srotolo la struttura a quadrato*)
			 order = Length[parents[[1]]];
			 p1 = parents[[1]];
			 p2 = parents[[2]];
			 (*Trovo punto di taglio*)
			 where1 = Random[Integer, {2, order}];
			 (*Produco figli*)
			 c11 = Take[p1, {1, where1}];
			 c12 = Take[p1, {where1 + 1, Length[p1]}];
			 c21 = Take[p2, {1, where1}];
			 c22 = Take[p2, {where1 + 1, Length[p2]}];
			 (*Controllo che non ci siano invalidi*)
			 j1 = deleteDouble[Join[c11,c22]];
			 j2 = deleteDouble[Join[c21,c12]];
			 Return[{j1, j2}],
			 (*Se non muto*)
			 Return[parents];
		      ],		      

		      _, (*Evita che il criterio non sia definito*)
		      Print["La funzione di fitness ", type,
			    " non e' ancora stata implementata"];
		      Abort[];
	       ];

	]; (*Restituisce una lista contenente due individui mescolati*)

(*Produce figli scambiando bit tra i genitori dati in tutta la popolazione*)
crossoverAll[pop_List, pIndex_List, pc_Real, type_:totalSquared] :=
	Module[{N},
	       N = Length[pop];
	       Return[Flatten[Table[
		       crossoverOne[{pop[[pIndex[[i]]]],
				     pop[[pIndex[[N-i]]]]}, pc, type],
		       {i,1,N/2}
			      ], 1	
		      ]
	       ]
	]; (*Restituisce una lista contenente una popolazione di individui mescolati*)

(*Produce figli mescolati a partire dalla popolazione*)
(*choosePrantes crea la lista dei genitori scelti*)
(*elitism impedisce che alcuni individui vengano modificati*)
crossoverParents[pop_List, pc_, criterion_, type_:totalSquared, elitism_:0] :=
	Module[{bests, pop2, ret, popsorted},
               If[elitism === 0,
		  Return[crossoverAll[pop, chooseParents[pop,
							 criterion, type], pc, type]],
		  (*Tolgo i migliori dalla lista dei crossover*)
		  popsorted = sortPop[pop, type];
		  bests = popsorted[[1;;elitism]];
		  (*Li ordino ed elimino i primi*)
		  pop2 = Drop[pop, elitism];
		  ret = crossoverAll[pop2, chooseParents[pop2,
							 criterion, type], pc, type];
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
			 (*Non muto*)
			 Return[square];
		      ],
		      2, (*Scambia due colonne*)
		      If[Random[] <= pm,
			 r = RandomInteger[{1, order}, 2];
			 list = Transpose[square];
			 temp = list[[r[[1]]]];
			 list[[r[[1]]]] = list[[r[[2]]]];
			 list[[r[[2]]]] = temp;
			 Return[Transpose[list]],
			 (*Non muto*)
			 Return[square];
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
			 (*Non muto*)
			 Return[square];
		      ],
		      4, (*Permuta una riga*)
		      If[Random[] <= pm,
			 r = Random[Integer, {1, order}];
			 list = square;
			 list[[r]] = permutateRow[square, r];
			 Return[list],
			 (*Non muto*)
			 Return[square];
		      ],
		      5, (*Permuta una colonna*)
		      If[Random[] <= pm,
			 r = Random[Integer, {1, order}];
			 list = square;
			 list[[r]] = permutateRow[square, r];
			 Return[list],
			 (*Non muto*)
			 Return[square];
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
(*E' un tentaivo per fare un approccio piu' deterministico*)
(*Non converge*)
swapElementsFromCorrectRows[square_List] :=
	Module[{order, row, pair, randrow, pairs, ret, selectpair, sub1, sub2, rows},
	       order = Length[square];
	       row = Random[Integer, {1, order}];
	       (*Richiede Mathematica 8*)
	       pair = RandomSample[square[[row]], 2];
	       pairs = {};
	       numbers = Table[i, {i,1,order}];
	       numbers = DeleteCases[numbers, row];
	       (*Finche' ci sono coppie*)
	       While[Length[pairs] === 0,
		     (*Se non posso piu'*)
		     If[Length[numbers] === 0,
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
		  (*Scelgo una possibilita' e faccio lo scambio*)
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
swapElementsFromCorrectRowsAll[pop_List, elitism_Integer:0,
			       type_:correctLines, multi_Integer:1] :=
	Module[{popsorted, pop2, ret},
	       (*Toglo i milgiori dalla mutazione*)
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
(*Cercando di utilizzare la funzione semideterministica di sopra per
  generare l'individuo perfetto*)
(*Non converge*)
fromRowsToMagic[nInd_Integer, order_Integer, OptionsPattern[]] :=
	Module[{count, t0, fintmin, fitmax, fitmean,
	        deletat, popin, popsorted, i, newpop},

	       Print["-----Inizio a scambiare le righe-----"];
	       t0 = TimeUsed[];
	       count = 1;
	       (*Costruisco individui con le righe perfette*)
	       popin = ParallelTable[buildFromScratch[order], {nInd}];
	       		   
	       statGen[popin, 1, OptionValue[fitnessFunction]];

	       (*Statistiche*)
	       fitmin = {minFitnessPop[popin, OptionValue[fitnessFunction]]};
	       fitmax = {maxFitnessPop[popin, OptionValue[fitnessFunction]]};
	       fitmean = {meanFitnessPop[popin, OptionValue[fitnessFunction]]};

	       (*Aggiornare qui quando si aggiunge fitness*)
	       (*Trovo le fintess obiettivo*)
	       Switch[OptionValue[fitnessFunction],
		      totalSquared,
		      target = 0,
		      correctLines,
		      target = 2 order + 2
		     ];

	       (*Se ho gia' il quadrato perfetto*)
	       If[targetFitnessPop[popin, OptionValue[fitnessFunction]] === target,
		  Return[count]];

	       While[targetFitnessPop[popin, OptionValue[fitnessFunction]] =!=
				     target && count < OptionValue[limit],
		     count += 1;
		     popin = swapElementsFromCorrectRowsAll[popin,
			      OptionValue[elitism],
			      OptionValue[fitnessFunction], OptionValue[multi]];
		     popsorted = sortPop[popin, OptionValue[fitnessFunction]];
		     newpop = ParallelTable[buildFromScratch[order], {nInd/2}];
		     popin = Join[popsorted[[1;;nInd/2]], newpop];
		     fitmin = Append[fitmin, minFitnessPop[popin,
		                     OptionValue[fitnessFunction]]];
		     fitmax = Append[fitmax, maxFitnessPop[popin,
		                     OptionValue[fitnessFunction]]];
		     fitmean = Append[fitmean, meanFitnessPop[popin,
				     OptionValue[fitnessFunction]]];
		     statGen[popin, count, OptionValue[fitnessFunction]];
	       ];

	       deltat = TimeUsed[] - t0;
	       
	       Print["Generazione finale: ", count];
	       Print["Tempo impiegato: ", deltat, " s"];
	       Return[{whoIsTheBest[popin, OptionValue[fitnessFunction]],
		       deltat, {fitmin, fitmax, fitmean}}];
	];

(*Esegue piu' mutazioni*)
mutationMulti[square_List, pm_, multi_Integer:1] :=
	Module[{},
	       (*Nesta piu' mutazioni*)
	       Return[Nest[mutationOne[#, pm] &, square, multi]];
	];
		  
(*Produce mutazioni su una popolazione*)
mutationAll[pop_List, pm_, type_:totalSquared, elitism_Integer:0, multi_Integer:1] :=
	Module[{pop2, popsorted, ret},
	       (*Non muta i migliori*)
	       popsorted = sortPop[pop, type];
	       pop2 = Drop[popsorted, elitism];
	       ret = mutationMulti[#, pm, multi] & /@ pop2;
	       (*Li tolgo e li riattacco*)
	       ret = Join[popsorted[[1;;elitism]], pop2];
	       Return[ret];
	];
	
(*Elimina dalla popolazione i doppioni*)
purge[pop_List] :=
	Module[{ret, len, order},
		len = Length[pop];
		order = Length[pop[[1]]];
		(*RICHIEDE MATHEMATICA 8*)
		(*Tolgo i doppioni*)
		ret = DeleteDuplicates[pop];
		(*Rigenero quadrati nuovi per ogni doppione tolto*)
		Return[Join[generatePop[len - Length[ret], order], ret]];
	];

(*Produce una nuova popolazione a partire da una esistente*)
reproduce[pop_List, criterion_, pc_, pm_, type_:totalSquared, elitism_Integer:0,
	  nfittests_Integer:0, crossover_Integer:1, multi_Integer:1] :=
	Module[{popsorted, mating, mated, new, bests},
	       (*Controlli su elitismo*)
	       If[OddQ[elitism],
		  Print["L'elitismo puo' essere solo per un numero pari di individui!"];
		  Abort[];
	       ];	       
	       If[elitism >= Length[pop],
		  Print["Non puoi selezionare un elitismo cosi' elevato!"];
		  Abort[];
	       ];
	       (*Controlli su individui da fare riprodurre con nfittests*)
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
	       (*Se il criterio e' fittests muto solo un certo numero, li altri li scarto*)
	       If[criterion === fittests,
		  popsorted = sortPop[pop, type];
		  mating = popsorted[[1;;nfittests]];
		  mated = mutationAll[crossoverParents[mating,
			  pc, criterion, type, elitism], pm, type, elitism];
		  new = generatePop[Length[pop] - nfittests, Length[pop[[1]]]];
		  Return[purge[Join[new, mated]]];
	       ];
	       (*Se il criterio non e' fittest passo attraverso crossoverParesnts*)
	       Return[purge[mutationAll[crossoverParents[pop,
			pc, criterion, type, elitism], pm, type, elitism, multi]]];
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

	       (*E' l'unica opzioni che usa la distanza*)
	       If[Optionvalue[criterion] === similarSquare,
		 distance = True;
		 dists = {meanDistPop[popin, OptionValue[fitnessFunction]]},
		 distance = False
	       ];

	       (*Controlli per l'opzione fittests*)
	       If[OptionValue[criterion] === fittests && OptionValue[nfittests] === 0,
		 Print["Devi impostare un numero di individui da far riprodurre"];
		 Print["Per farlo aggiungi la flag nfittests -> numero"];
		 Abort[];
	      ];       
		   
	       statGen[popin, 1, OptionValue[fitnessFunction], distance];

	       (*Statistiche*)
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

	       (*Controllo se c'e' gia' il quadrato magico*)
	       If[targetFitnessPop[popin, OptionValue[fitnessFunction]] === target,
		  Return[count]];

	       (*Cicla fino a quando non trova il quadrato o raggiunge il limte*)
	       While[targetFitnessPop[popin, OptionValue[fitnessFunction]] =!= target
		     && count < OptionValue[limit],
		     count += 1;
		     (*Nuova popolazione*)
		     popin = reproduce[popin, OptionValue[criterion], pc, pm,
				       OptionValue[fitnessFunction],
				       OptionValue[elitism],
				       OptionValue[nfittests], OptionValue[crossover]];
		     (*Statistiche*)
		     fitmin = Append[fitmin, minFitnessPop[popin,
						OptionValue[fitnessFunction]]];
		     fitmax = Append[fitmax, maxFitnessPop[popin,
							OptionValue[fitnessFunction]]];
		     fitmean = Append[fitmean, meanFitnessPop[popin,
							OptionValue[fitnessFunction]]];
		     If[distance,
			dists = Append[dists, meanDistPop[popin,
						        OptionValue[fitnessFunction]]];
		     ];
		     statGen[popin, count, OptionValue[fitnessFunction], distance];
	       ];

	       deltat = TimeUsed[] - t0;
	       
	       Print["Generazione finale: ", count];
	       Print["Tempo impiegato: ", deltat, " s"];
	       If[distance,
		  Return[{whoIsTheBest[popin, OptionValue[fitnessFunction]],
			  deltat, {fitmin, fitmax, fitmean, dists}}],
		  Return[{whoIsTheBest[popin, OptionValue[fitnessFunction]],
			  deltat, {fitmin, fitmax, fitmean}}]
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
(*Funziona, ma inutile*)
buildFromScratch[order_Integer] :=
	Module[{parts, mn, goods, used, n, ret, i},
	       If[order > 7,
		  Print["Attualmente questa funzione funziona con al massimo ordine 7"];
		  Abort[];
	       ];
	       mn = magicNumber[order];
	       (*Genero tutte le partizioni della somma magica*)
	       parts = IntegerPartitions[mn, {order}];
	       (*Tolgo quelle con numeri doppi*)
	       goods = DeleteCases[parts, x_ /; checkIfValidLine[x] === False];
	       (*Prendo una permutazione casuale*)
	       goods = randomPermutation /@ goods;
	       n = Random[Integer, {1, Length[goods]}];
	       ret = Table[1, {order}];
	       (*Numeri gia' usati*)
	       used = {};
	       i = 1;
	       While[i <= order,
		     While[Length[goods] =!= 0,
			   n = Random[Integer, {1, Length[goods]}];
			   (*Evito che ci siano numeri doppi*)
			   If[Length[Intersection[goods[[n]], used]] === 0,
			      Break[];
			      Break[],
			      (*Tolgo dai numeri buoni*)
			      goods = Drop[goods, {n}];
			   ];
		     ];
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

(*Funziona!*)
(*Bug noti:
  - Non sempre converge con quadrati di ordine dal 3 al 6
 *)

(*Segue l'impelementazione dell'argoritmo di Xie Kang*)
(*Maggiori informazioni disponibili al link:
http://ieeexplore.ieee.org/xpl/abstractAuthors.jsp?arnumber=1299763 *)

(*Da ora in poi l'elemento base e' un individuo, non un quadrato*)

(*Versioni compilate per essere piu' veloci*)
(*Per questo c'e' l'avviso di caricamento*)
(*Compilo per C, usando tutti i thread possibili, e togliendo i
  controlli sull'overflow degli interi, che tanto non accade con
  questi numeri*)
(*La compilazione rende la funzione 1000 volte piu' veloce*)
(*Questo si sente su quadrati 100x100*)
Print["Caricamento... Attendere prego"];

Print["0 %"];

(*Calcola il numero magico*)
magicNumber = Compile[{{n, _Integer}},
		      Module[{},
			     Return[Quotient[n (n n +1), 2]]
		      ], CompilationTarget->"C", Parallelization->True,
		      RuntimeOptions->"Speed"
	      ];

Print["25 %"];

(*Genera un numero che non e' nella lista data*)
(*Extreme sono gli estremi da cui pescare il numero*)
randomDiff = Compile[{{list, _Integer, 1}, {extreme, _Integer, 1}},
		     Module[{r},
			    r = Random[Integer, extreme];
			    While[Count[list,r] =!= 0, r = Random[Integer, extreme]];
			    Return[r]
		     ], CompilationTarget->"C", Parallelization->True,
		     RuntimeOptions->"Speed"
	     ];

Print["50 %"]

(*Genera un quadrato con tutti i numeri diversi, da 1 a n^2*)
generateSquare = Compile[{{n, _Integer}},
			 Module[{used, ret},
				used = Table[-1, {i,1,n}, {j,1,n}];
				ret = Table[r = randomDiff[Flatten[used], {1, n^2}];
					    used[[i,j]] = r;
					    r,{i,1,n},{j,1,n}];
				Return[ret]
			 ], {{randomDiff[_], _Integer}},
			 CompilationTarget->"C", Parallelization->True,
			 RuntimeOptions->"Speed"
		 ];

Print["75 %"];

(*Questo modulo genera e inizializa un individuo, cioe' una coppia matrice
  del quadrato + matrice delle deviazioni*)
(*La matrice delle deviazioni di default e' inizializzata con il valore order*)
generateInd = Compile[{{order, _Integer}},
		      Module[{},
			     Return[{generateSquare[order],
				     Table[order^2, {order}, {order}]}]
		      ], {{generateSquare[_], _Integer, 2}},
		      CompilationTarget->"C", Parallelization->True,
		      CompilationOptions -> {"InlineExternalDefinitions" -> True},
		      RuntimeOptions->"Speed"
	      ];

Print["100 %"];

(*Questo modulo genera e inizializa un individuo, cioe' una coppia matrice
  del quadrato + matrice delle deviazioni*)
(*La matrice delle deviazioni di default e' inizializzata con il valore 3*)
(*generateInd[order_Integer] :=
	Module[{},
	     Return[{generateSquare[order], Table[order^2, {order}, {order}]}];
	];*)

(*Calcola somme delle righe di un individuo*)
(*Total e' molto piu' efficiente di Plus @@ *)
rowsTotalInd[ind_List] :=
	Module[{},
	       Return[Table[Total[ind[[1,i]]], {i,1,Length[ind[[1]]]}]];
	];

(*Calcola somme colonne di un individuo*)
columnsTotalInd[ind_List] :=
	Module[{},
	       Return[Table[Total[Transpose[ind[[1]]][[i]]], {i,1,Length[ind[[1]]]}]];
	];

(*Calcola somme diagonali di un individuo*)
diagonalsTotalInd[ind_List] :=
	Module[{},
	       Return[{(*Parallel*)Sum[ind[[1,i,i]],{i,1,Length[ind[[1]]]}],
		       (*Parallel*)Sum[ind[[1,-i,i]],{i,1,Length[ind[[1]]]}]}];
	];

(*Calcola somme diagonali di un quadrato*)
diagonalsTotal[square_List] :=
	Module[{},
	       Return[{(*Parallel*)Sum[square[[i,i]],{i,1,Length[square]}],
	               (*Parallel*)Sum[square[[-i,i]],{i,1,Length[square]}]}];
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

(*Restituisce il numero di linee non corrette di un individuo*)
incorrectLines[ind_List] :=
	Module[{},
	       Return[incorrectRows[ind] + incorrectColumns[ind]];
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
		  (*La fitness e' negativa quando il quadrato e' semimagico*)
		  (*Questo mi permette di renderli preferiti ai quadrati generici*)
		  Return[-Total[diagonalsDeviation[ind]]],
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
	Module[{fp, index},
	       fp = fitnessPopInd[pop];
	       (*Controllo se c'e' l'individuo perfetto*)
	       If[Length[Position[fp, 0]] =!= 0,
		  Return[pop[[Position[fp, 0][[1,1]]]]];
	       ];
	       (*Io voglio l'individuo piu' vicino a zero, ma voglio
                 anche privilegiare chi ha fitness negativa*)
	       If[Min[fp] < 0,
		  fp = (#)^(-1) & /@ fp
	       ];
	       (*Posizione del migliore*)
	       index = Random[Integer, {1, Length[Position[fp, Min[fp]]]}];
	       Return[pop[[Position[fp, Min[fp]][[index ,1]]]]];
	];

(*Calcola il valore di sigma t, che e' un parametro che serve per la nuova
  varianza dopo le mutazioni*)
sigmat[ind_List] :=
	Module[{nincorrect},
	       nincorrect = incorrectLines[ind];
	       If[nincorrect =!= 0,
		  Return[Total[rowsDeviation[ind]] + Total[columnsDeviation[ind]] /
						     nincorrect],
		  Return[Total[diagonalsDeviation[ind]]/incorrectDiagonals[ind]];
	       ];
	];

(*Costruisce gli insiemi da cui prendere gli elementi di mutazione*)
(*Ci sono tre tipi di mutazioni, la prima muta solo gli elementi che stanno in S1,
  la seconda solo quell in S2, e la terza solo quelli in S2 scambiandoli con tutti
  gli altri del quadrato*)
generateMutationSets[ind_List] :=
	Module[{rows, cols, S2r, S2c, S2, order, ret},
	       ret = ind[[1]];
	       order = Length[ret];
	       rows = rowsDeviation[ind];
	       cols = columnsDeviation[ind];
	       (*Passo in rassegna tutto il quadrato, se non e' in S2r metto 0*)
	       S2r = (*Parallel*)Table[If[rows[[i]] =!= 0,
					  ret[[i,j]], (*Il numero se e' in S2r*)
				          0], (*0 se non e' in S2r*)
				       {i, 1, order},
				       {j, 1, order}];
	       S2r = Flatten[S2r];
	       S2r = DeleteCases[S2r, 0];
	       (*Passo in rassegna tutto il quadrato, se non e' in S2r metto 0*)
	       S2c = (*Parallel*)Table[If[cols[[j]] =!= 0,
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
	       Return[{S1, S2r, S2c, S2}];
	];
	       
(*Esegue le mutazioni su un individuo*)
mutateInd[ind_List] :=
	Module[{order, S1, S2, mutnum, incorrectrows, incorrectcolumns, pm, i, j,
		ret, incorrectdiagonals, inclin, ran, subs, S2r, S2c, rows, cols,
		temp, sig, minlist, min, sc, sigt, reti, num},
	       
	       order = Length[ind[[1]]];

	       incorrectrows = incorrectRows[ind];
	       incorrectcolumns = incorrectColumns[ind];
	       inclin = incorrectLines[ind];
	       incorrectdiagonals = incorrectDiagonals[ind];

	       ret = ind[[1]];
	       sig = ind[[2]];

	      
	       (*Se il quadrato e' perfetto*)
	       If[inclin === 0 && incorrectdiagonals === 0, Return[{ret, ind[[2]]}]];
	       (*Se tutte le righe e le colonne sono ok faccio solo permutazioni
                 di linee a caso*)
	       If[inclin === 0 && incorrectdiagonals =!= 0,
		  num = Random[Integer, {1, order}];
		  For[i = 0, i < num, i++,
		      mutnum = Random[Integer, {1,2}]; (*1 scambio righe, 2 colonne*)
		      Switch[mutnum,
			     1, (*Permuto righe*)
			     ran = RandomInteger[{1, order}, 2];
			     subs = {ret[[ran[[1]]]] -> ret[[ran[[2]]]],
				     ret[[ran[[2]]]] -> ret[[ran[[1]]]]};
			     ret = ret /. subs,			     
			     2, (*Permuto colonne*)
			     ran = RandomInteger[{1, order}, 2];
			     ret = Transpose[ret];
			     subs = {ret[[ran[[1]]]] -> ret[[ran[[2]]]],
				     ret[[ran[[2]]]] -> ret[[ran[[1]]]]};
			     ret = Transpose[ret /. subs];
		      ];
		  ];
		  Return[{ret, ind[[2]]}];
	       ];

	       (*Se il quadrato non e' semimagico*)
	       If[inclin =!= 0,
		  {S1, S2r, S2c, S2} = generateMutationSets[ind];
		  (*Seleziona una mutazione casuale*)
		  (*Se S1 e' vuoto non faccio la mutazione 1*)
		  If[S1 =!= {},
		     mutnum = Random[Integer, {1,3}],
		     mutnum = Random[Integer, {2,3}]
		  ];
		  (*pm = N[1/order];*)
		  pm = 1;
		  (*Sara' il mio output, ed e' quello che modifico*)
		  reti = {ret, sig};
		  Switch[mutnum,
			 1, (*S1 in S2*)
			 For[i = 1, i <= order, i++,
			     For[j = 1, j <= order, j++,
				 If[Count[S1, ret[[i,j]]] =!= 0,
				    pm = N[1/(incorrectrows incorrectcolumns)];
				    If[Random[] <= pm,
				       (*Trovo il nuovo valore*)
				       temp = ret[[i,j]] +
					      RandomInteger[{-sig[[i,j]], sig[[i,j]]}];
				       (*Se non e' ammissibile*)
				       If[temp < 1,
					  temp = Random[Integer, {1, order}];
				       ];
				       If[temp > order^2,
					  temp = order^2 - Random[Integer, {0, order}];
				       ];
				       (*Trovo con chi scambiarlo*)
				       minlist = (*Parallel*)Map[Abs[# - temp] &, S2];
				       min = Min[minlist];
				       sc = S2[[Position[minlist, min][[1,1]]]];
				       (*Faccio lo scambio*)
				       subs = {ret[[i,j]] -> sc, sc -> ret[[i,j]]};
				       ret = Replace[ret, subs, 2];
				       (*Aggiusto le varianze*)
				       sig[[i,j]] = sig[[i,j]] + RandomInteger[{-1,1}];
				       If[sig[[i,j]] < 1 ||
					  sig[[i,j]] > sigmat[{ret, ind[[2]]}],
					  sigt = sigmat[{ret, ind[[2]]}];
					  sig[[i,j]] = Random[Integer, {1, sigt}];
				       ];
				       reti = {ret, sig};
				       (*Aggiorni valori*)
				       incorrectrows = incorrectRows[reti];
				       incorrectcolumns = incorrectColumns[reti];
				       {S1, S2r, S2c, S2} = generateMutationSets[reti];
				       (*Per evitare che gli insiemi si svuotino*)
				       If[S1 === {} || S2 === {}, Return[reti]];
				    ];
				 ];
			     ];
			 ],
			 2, (*S2 in S2*)
			 For[i = 1, i <= order, i++,
			     For[j = 1, j <= order, j++,
				 If[Count[S2, ret[[i,j]]] =!= 0,
				    pm = 1;
				    (*Aggiusto la probabilita' di mutazione*)
				    If[Count[S2r, ret[[i,j]]] =!= 0,
				       pm *= N[1/incorrectrows]
				    ];
				    If[Count[S2c, ret[[i,j]]] =!= 0,
				       pm *= N[1/incorrectcolumns]
				    ];				       
				    If[Random[] <= pm,
				       (*Trovo il nuovo valore*)
				       temp = ret[[i,j]] +
					      RandomInteger[{-sig[[i,j]], sig[[i,j]]}];
				       (*Se non e' ammissibile*)
				       If[temp < 1,
					  temp = Random[Integer, {1, order}];
				       ];
				       If[temp > order^2,
					  temp = order^2 - Random[Integer, {0, order}];
				       ];
				       (*Trovo con chi scambiarlo*)
				       minlist = (*Parallel*)Map[Abs[# - temp] &, S2];
				       min = Min[minlist];
				       sc = S2[[Position[minlist, min][[1,1]]]];
				       (*Faccio lo scambio*)
				       subs = {ret[[i,j]] -> sc, sc -> ret[[i,j]]};
				       ret = Replace[ret, subs, 2];
				       (*Aggiusto le varianze*)
				       sig[[i,j]] = sig[[i,j]] + RandomInteger[{-1,1}];
				       If[sig[[i,j]] < 1 ||
					  sig[[i,j]] > sigmat[{ret, ind[[2]]}],
					  sigt = sigmat[{ret, ind[[2]]}];
					  sig[[i,j]] = Random[Integer, {1, sigt}];
				       ];
				       reti = {ret, sig};
				       (*Aggiorno valori*)
				       incorrectrows = incorrectRows[reti];
				       incorrectcolumns = incorrectColumns[reti];
				       {S1, S2r, S2c, S2} = generateMutationSets[reti];
				       (*Per evitare che gli insiemi si svuotino*)
				       If[S1 === {} || S2 === {}, Return[reti]];
				    ];
				 ];
			     ];
			 ],
			 3, (*S2 in tutto*)
			 For[i = 1, i <= order, i++,
			     For[j = 1, j <= order, j++,
				 If[Count[S2, ret[[i,j]]] =!= 0,
				    pm = 1;
				    (*Aggiusto probabilita' di mutazione*)
				    If[Count[S2r, ret[[i,j]]] =!= 0,
				       pm *= N[1/incorrectrows]
				    ];
				    If[Count[S2c, ret[[i,j]]] =!= 0,
				       pm *= N[1/incorrectcolumns]
				    ];				       
				    If[Random[] <= pm,
				       (*Trovo il nuovo valore*)
				       temp = ret[[i,j]] +
					      RandomInteger[{-sig[[i,j]], sig[[i,j]]}];
				       (*Se non e' ammissibile*)
				       If[temp < 1,
					  temp = Random[Integer, {1, order}];
				       ];
				       If[temp > order^2,
					  temp = order^2 - Random[Integer, {0, order}];
				       ];
				       (*Faccio lo scambio*)
				       subs = {ret[[i,j]] -> temp, temp -> ret[[i,j]]};
				       ret = Replace[ret, subs, 2];
				       (*Aggiusto le varianze*)
				       sig[[i,j]] = sig[[i,j]] + RandomInteger[{-1,1}];
				       If[sig[[i,j]] < 1 ||
					  sig[[i,j]] > sigmat[{ret, ind[[2]]}],
					  sigt = sigmat[{ret, ind[[2]]}];
					  sig[[i,j]] = Random[Integer, {1, sigt}];
				       ];
				       reti = {ret, sig};
				       (*Aggiorno valori*)
				       incorrectrows = incorrectRows[reti];
				       incorrectcolumns = incorrectColumns[reti];
				       {S1, S2r, S2c, S2} = generateMutationSets[reti];
				       (*Per evitare che gli insiemi si svuotino*)
				       If[S1 === {} || S2 === {}, Return[reti]];
				    ];
				 ];
			     ];
			 ]
		  ];
		  Return[reti];
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
	Module[{mv, order, cols, subs, ret, i1, i2, j1},
	       order = Length[ind[[1]]];
	       mv = magicNumber[order];
	       cols = columnsTotalInd[ind];
	       ret = ind[[1]];
	       (*QUESTO NON E' UN MODO ELEGANTE PER FARLO*)
	       (*Elemento a_{i1,J1}*)
	       For[i1 = 1, i1 <= order, i1++,  (*Riga 1*)
		   For[j1 = 1, j1 <= order, j1++, (*Colonna 1*)
		       For[j2 = j1 + 1, j2 <= order, j2++, (*Scorro la colonna*)
			   If[(cols[[j1]] - mv) === (mv - cols[[j2]]) &&
			     (ret[[j1, i1]] - ret[[j1, j2]] === cols[[j1]] - mv),
			      subs = {ret[[j1, i1]] -> ret[[j1, j2]],
				      ret[[j1, j2]] -> ret[[j1, i1]]};
			      ret = ret /. subs;
			      (*Bisogna aggiornare il valore delle somme*)
			      cols[[j1]] = Total[Transpose[ret][[j1]]];
			      cols[[j2]] = Total[Transpose[ret][[j2]]];
			   ];
		       ];
		   ];   
	       ];
	       Return[{ret, ind[[2]]}];
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
			   For[j2 = 1, j2 <= order, j2++, (*Scrorro la riga*)
			       (*Workaround per fare percorrere tutto
                                 il quadrato correttamente*)
			       If[i2 === i1 && j2 === 1, j2 = j1 +1];
			       If[j2 > order, Break[]];
			       If[j2 === j1, Break[]];
			       If[(rows[[i1]] - mv) === (mv - rows[[i2]]) &&
				  (ret[[i1, j1]] + ret[[i1, j2]]
				   - ret[[i2, j2]] - ret[[i2, j1]]
                                   === rows[[i1]] - mv),
				  subs = {ret[[i1, j1]] -> ret[[i2, j1]],
					  ret[[i2, j1]] -> ret[[i1, j1]],
					  ret[[i1, j2]] -> ret[[i2, j2]],
					  ret[[i2, j2]] -> ret[[i1, j2]]};
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
			  ];
		       ];
		   ];   
	       ];
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
			  ];
		       ];
		   ];   
	       ];

	       (*Quinta rettificazione*)
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
			  ];
		       ];
		   ];   
	       ];	       	       
	       Return[{ret, ind[[2]]}];
	];

(*Rettifica le linee di un individuo se questo ha fitness migliore di 50 order*)
(*Fai tutte le rettificazioni*)
rectifyLinesInd[ind_List] :=
	Module[{order, ret(*, timezero*)},
	       order = Length[ind[[1]]];
	       If[fitnessInd[ind] < 50 order &&
		  incorrectRows[ind] + incorrectColumns[ind] =!= 0,
		  (*  timezero = TimeUsed[];*)
		  ret = rectifyRowsWithOnePair[ind]; 
		 (* TimeUsed[] - timezero >>> "reclin.dat";
		  timezero = TimeUsed[];*)
		  ret = rectifyRowsWithTwoPairs[ret];
		 (* TimeUsed[] - timezero >>> "reclin.dat";
		  timezero = TimeUsed[];*)
		  ret = rectifyColumnsWithOnePair[ret];
		 (* TimeUsed[] - timezero >>> "reclin.dat";
		  timezero = TimeUsed[];*)
		  ret = rectifyColumnsWithTwoPairs[ret];
		 (* TimeUsed[] - timezero >>> "reclin.dat";
		  timezero = TimeUsed[];*)
		  Return[ret];
	       ];
	       Return[ind];
	];

(*Refficila le linee di una popolazione*)
rectifyLinesPop[pop_List] :=
	Module[{},
	       Return[rectifyLinesInd /@ pop];
	];

(*Rettifica le diagonali di un individuo se questo ha fitness migliore di 100*)
rectifyDiagonalsInd[ind_List] :=
	Module[{order},
	       order = Length[ind[[1]]];
	       If[fitnessInd[ind] < 100 order &&
		  incorrectLines[ind] === 0,
		  Return[rectifyDiagonals[ind]];
	       ];
	       Return[ind];
	];

(*Rettifica le linee di una popolazione*)
rectifyDiagonalsPop[pop_List] :=
	Module[{},
	       Return[rectifyDiagonalsInd /@ pop];
	];

(*Per evitare di utilizzare Append su liste molto lunghe assegno il valore
  che voglio appendere ad una funzione, e alla fine genero una lista a partire
  dai valori della funzione. Questo e' piu' efficiente*)
fromValuesToList[values_, n_Integer] :=
	Module[{},
	       Return[ParallelTable[values[i], {i,1,n}]];
	];

(*Istruzioni*)
Print["Per costruire un quadrato utilizza il comando xiekang[n_Integer]"];
Print["Il risultato e' una lista contenente i seguenti elementi"];
Print["1: tempo di esecuzione dell'algoritmo in secondi"];
Print["2: quarato magico"];
Print["3: fitness dell'individuo migliore in funzione della generazione"];
Print["4: righe non corrette dell'individuo migliore in funzione della generazione"];

(*Esegue l'algoritmo Xie-Kang*)
(*Utilizzando tutti i metodi sopra definiti*)
xiekang[order_Integer] :=
	Module[{father, offspring, gen, fitbest, timings, fittest, fr, fc, fd,
		inclin, fit, fitbestindex, times, timmut, timreclin, timrecdiag,
		timsel},

	       If[order < 3,
		  Print["L'ordine del quadrato deve essere almeno 3"];
		  Abort[];
	       ];

	       If[order < 7,
		  Print["Attenzione, potrebbe non convergere!"];
		  Pause[2];
	       ];

	       (*timings contiene i tempi*)
	       (*times i delta tempi*)
	       times = {0};
	       (*1 = generazione di un individuo*)
	       (*2 = mutazioni*)
	       (*3 = rettifilcazioni linee*)
	       (*4 = rettificazioni diagonali*)
	       (*5 = selezioni*)
	       	       
	       Print["Costruisco un quadrato di ordine: ", order];
	       
	       Print["Genero il capostipide di tutti i quadrati"];
	       timings[0] = TimeUsed[];
	       father = generateInd[order];
	       timings[1] = TimeUsed[];
	       (*Print["Quadrato generato in ", timings[1] - timings[0], " s"];*)
	       
	       times[[1]] = timings[1] - timings[0];

	       (*Se trovo gia' un quadrato magico*)
	       If[fitnessInd[father] === 0,
		  Print["Ho costruito un quadrato magico in ",
			TimeUsed[] - timings[1], " s"];
		  Return[father[[1]]];
	       ];

	       (*Inizializzo una figliata di 25 elementi uguali al padre*)
	       gen = 1;
	       fitbest = fitnessInd[father];

	       Print["Il padre ha fitness ", fitbest];
	       
	       While[fitbest =!= 0,
		     (*Print[father];*)
		     offspring = ParallelTable[father, {25}];
		     Print["Siamo alla generazione: ", gen];
		     Print["Effettuo mutazioni"];
		     timings[2] = TimeUsed[];
		     offspring = ParallelMap[mutateInd, offspring];
		     (*Print[fitnessPopInd[offspring]];*)
		  (*Print["Mutazioni effettuate in ", TimeUsed[] - timings[2], " s"];*)
		     timmut[gen] = TimeUsed[] - timings[2];
		     timings[5] = TimeUsed[];
		     fittest = fittestChild[Append[offspring, father]];
		     timsel[gen] = TimeUsed[] - timings[5];
		     fitbest = fitnessInd[fittest];
(*		     Print["Il miglior figlio ha fitness ", fitbest];
		     fr = incorrectRows[fittest];
		     fc = incorrectColumns[fittest];
		     fd = incorrectDiagonals[fittest];
		     Print["Righe sbagliate: ", fr];
		     Print["Colonne sbagliate: ", fc];
		     Print["Diagonali sbagliate: ", fd];
		     Print["Linee sbagliate: ", fr + fc + fd];*)
		     timreclin[gen] = -9999;
		     timrecdiag[gen] = -9999;		     
		     If[fitbest === 0,
			Print["TROVATO!"];
			Print["Ho costruito un quadrato magico in ",
			      TimeUsed[] - timings[1], " s"];
			Return[fittest[[1]]]
		     ];
		     If[fitbest < 50 order &&
			incorrectLines[fittest] =!= 0,
			Print["Rettifico righe"];
			(*Print[fittest];*)
			timings[3] = TimeUsed[];
			offspring = rectifyLinesPop[offspring];
   (*		Print["Rettificazione effettuata in ", TimeUsed[] - timings[3],
			      " s"];*)
			timreclin[gen] = TimeUsed[] - timings[3];
			fittest = fittestChild[Append[offspring, father]];
			fitbest = fitnessInd[fittest];
			(*Print["Ora il miglior figlio ha fitness ", fitbest];
			fr = incorrectRows[fittest];
			fc = incorrectColumns[fittest];
			fd = incorrectDiagonals[fittest];
			Print["Righe sbagliate: ", fr];
			Print["Colonne sbagliate: ", fc];
			Print["Diagonali sbagliate: ", fd];
			Print["Linee sbagliate: ", fr + fc + fd];*)
		     ];
		     If[incorrectLines[fittest] === 0 &&
			fitbest < 100,
			Print["Rettifico diagonali"];
			(*Print[fittest];*)
			timings[4] = TimeUsed[];
			offspring = rectifyDiagonalsPop[offspring];
			timrecdiag[gen] = TimeUsed[] - timings[4];
		(*Print["Rettificazione effettuata in ", TimeUsed[] - timings[4],
			      " s"];*)
			fittest = fittestChild[Append[offspring, father]];
			fitbest = fitnessInd[fittest];
		      (*Print["Ora il miglior figlio ha fitness ", fitbest];
			fr = incorrectRows[fittest];
			fc = incorrectColumns[fittest];
			fd = incorrectDiagonals[fittest];
			Print["Righe sbagliate: ", fr];
			Print["Colonne sbagliate: ", fc];
			Print["Diagonali sbagliate: ", fd];
			Print["Linee sbagliate: ", fr + fc + fd];*)
		     ];
		     If[incorrectLines[fittest] =!= 0,
			If[fitbest > 50 order,
			   father = fittestChild[offspring],
			   father = fittestChild[Append[offspring, father]];
			   (*father = fittestChild[offspring];*)
			],
			(*Se le linee sono a posto*)
			If[incorrectDiagonals[fittest] === 0,
			   (*Se le diagonali sono a posto*)
			   Parallelize[
				   fr = incorrectRows[fittest];
				   fc = incorrectColumns[fittest];
				   fd = incorrectDiagonals[fittest];
				   fitbest = fitnessInd[fittest];
				   inclin[gen] = fr + fc + fd;
				   fit[gen] = fitbest;
				   (*inclin = Append[inclin, fr + fc + fd];
				   fit = Append[fit, fitbest];*)
			   ];
			   Print["TROVATO!"];
			   Print["Ho costruito un quadrato magico in ",
				 TimeUsed[] - timings[1], " s"];
			   (*times = {times[[1]],
				    fromValuesToList[timmut, gen],
				    fromValuesToList[timreclin, gen],
				    fromValuesToList[timrecdiag, gen],
				    fromValuesToList[timsel, gen]};*)
			   Return[{TimeUsed[] - timings[1], fittest[[1]],
				   fromValuesToList[fit, gen],
				   fromValuesToList[inclin, gen](*, times*)}],
			   (*Se non sono a posto*)
			   If[Abs[fitbest] > 100,
			      father = fittestChild[offspring],
			      father = fittestChild[Append[offspring, father]];
			   ];
			];		
		     ];
		     Parallelize[
			     fr = incorrectRows[father];
			     fc = incorrectColumns[father];
			     fd = incorrectDiagonals[father];
			     fitbest = fitnessInd[father];
			     inclin[gen] =  fr + fc + fd;
			     fit[gen] = fitbest;
			     (*inclin = Append[inclin, fr + fc + fd];
			     fit = Append[fit, fitbest];*)
		     ];
		     Print["Il nuovo padre ha fitness: ", fitbest];
		     (*Print["Righe sbagliate: ", fr];
		     Print["Colonne sbagliate: ", fc];
		     Print["Diagonali sbagliate: ", fd];*)
		     Print["Linee sbagliate: ", fr + fc + fd];
		     gen += 1;
	       ];
	       Return[False];
	];

(*Produce un po' di quadrati e salva i risultati in data, per confrontare con XieKang*)
(*Di ordini da 10 a n con ordini variabili da 5 a 5*)
workout[n_Integer] :=
	Module[{a, k, t0},
	       t0 = TimeUsed[];
	       For[k = 10, k <= n, k = k + 5,
		   For[i = 1, i <= 10, i++,
		       Print["Ordine ", k];
		       Print["Tentativo ", i];
		       a = xiekang[k];
		       Save["data/ordine_" <>
			     ToString[k] <>
			     "_tentativo_" <> ToString[i], a];
		   ];
	       ];
	       Print["FINE!!!!!!!!!!!!!!!!!!"];
	       Print["Tempo impiegato: ", TimeUsed[] - to];
	];
