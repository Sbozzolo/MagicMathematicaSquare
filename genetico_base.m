(* ALGORITMO GENERICO PER TROVARE UN INDIVIDUO COMPOSTO DA BIT 1 *)

(*Genera un individuo*)
generateOne[n_Integer] :=
	Module[{},
		 Return[Table[Random[Integer],{n}]];
	 ];

(*Genera una popolazione*)
generatePop[nInd_Integer, nBit_Integer] :=
	Module[{},
	       Return[Table[generateOne[nBit],{nInd}]];
	];
					  
(*Funzione di fitness*)
fitness[ind_List] :=
	Module[{},
	       Return[Apply[Plus,ind]];
	];

(*Lista di fitness*)
fitnessPop[pop_List] :=
	Module[{},
	       Return[Map[fitness,pop]];
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

(*Ripartisci intervallo*)
divideInterval[pop_List, criterion_] :=
	Module[{len},
	       Switch[criterion,
		      fitnessProportionate, (*If criterion === FitnessProportionate*)
		      len = fitnessPop[pop]/totalFitnessPop[pop];
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
		       Length[Select[divideInterval[pop,criterion], (# < r) &]] + 1,
		       {Length[pop]}
		      ]
	       ];
	]; (*Restituisce una lista di individui*)

(*Produce figli scambiando bit tra i genitori dati*)
crossoverOne[parents_List, pc_] :=
	Module[{where, c11, c12, c21, c22},
	       If[Random[] < pc,
		  where = Random[Integer, {1,Length[parents[[1]]]}];
		  c11 = Take[parents[[1]], {1, where}];
		  c12 = Take[parents[[1]], {where + 1, Length[parents[[1]]]}];
		  c21 = Take[parents[[2]], {1,where}];
		  c22 = Take[parents[[2]], {where + 1, Length[parents[[2]]]}];		  		   Return[{Join[c11,c22], Join[c21,c12]}],
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
crossoverParents[pop_List, pc_, criterion_] :=
	Module[{},
	       Return[crossoverAll[pop, chooseParents[pop, criterion], pc]];
	];

(*Produce una mutazione su un individuo*)
mutationOne[ind_List, pm_] :=
	Module[{r},
	       Return[Map[ (r = Random[]; If[r < pm, 1 - #, #]) &, ind]];
	];

(*Produce mutazioni su una popolazione*)
mutationAll[pop_, pm_] :=
	Module[{},
	       Return[mutationOne[#, pm] & /@ pop]
	];

(*Produce una nuova popolazione a partire da una esistente*)
reproduce[pop_List, criterion_, pc_, pm_] :=
	Module[{},
	       Return[mutationAll[crossoverParents[pop, pc, criterion], pm]];
	]; (*Restituisce una popolazione di nuovi individui*)
		       
(*Produce popolazioni finche' non arriva un individuo perfetto*)
run[nInd_Integer, nBit_Integer, criterion_, pc_, pm_] :=
	Module[{count, popin},
	       
	       count = 1;
	       popin = generatePop[nInd, nBit];
	       
	       If[maxFitnessPop[popin] === nBit, Return[count]];

	       While[maxFitnessPop[popin] < nBit,
		     count += 1;
		     popin = reproduce[popin, criterion, pc, pm];
	       ];

	       Return[count];
	];

	       
