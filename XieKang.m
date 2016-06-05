(*Magic Mathematica Square*)

(*Costruisce quadrati magici di qualsiasi dimensione*)

(*Un quadrato magico è uno schieramento di numeri interi distinti in una tabella
quadrata tale che la somma dei numeri presenti in ogni riga, in ogni colonna e in
entrambe le diagonali dia sempre lo stesso numero, il numero magico*)
		 
(*Esempio di quadrato magico 3x3*)
(* 6 1 8
   7 5 3
   2 9 4 *)

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

(*Richiede Mathematica 8 per via delle parallelizzazioni*)
If[$VersionNumber < 8, Print["Questo programma richiede Mathematica 8"]];

Print["Ci potrebbero volere alcuni istanti a caricare il programma"];
Print["Attendere prego"];

(*In questa implementazione si cerca di compilare il possibile*)

(*Calcola il numero magico*)
magicNumber = Compile[{{n, _Integer}},
		      Module[{},
			     Return[Quotient[n (n n +1), 2]]
		      ], CompilationTarget->"C", Parallelization->True,
		      RuntimeOptions->"Speed"
	      ];

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
(*Calcola somme delle righe di un individuo*)
(*Total e' molto piu' efficiente di Plus @@ *)
rowsTotalInd = Compile[{{ind, _Integer, 3}},
		       Module[{},
			      Return[Table[Total[ind[[1,i]]],
					   {i,1,Length[ind[[1]]]}]]
		       ], CompilationTarget->"C", Parallelization->True,
		       RuntimeOptions->"Speed"
	       ];

(*Calcola somme colonne di un individuo*)
columnsTotalInd = Compile[{{ind, _Integer, 3}},
			  Module[{},
				 Return[Table[Total[Transpose[ind[[1]]][[i]]],
					      {i,1,Length[ind[[1]]]}]]
			  ], CompilationTarget->"C", Parallelization->True,
			  RuntimeOptions->"Speed"
		  ];

(*Calcola somme diagonali di un individuo*)
diagonalsTotalInd = Compile[{{ind, _Integer, 3}},
			    Module[{},
				   Return[{Sum[ind[[1,i,i]],{i,1,Length[ind[[1]]]}],
					    Sum[ind[[1,-i,i]],{i,1,Length[ind[[1]]]}]}]
			    ], CompilationTarget->"C", Parallelization->True,
			    RuntimeOptions->"Speed"
		    ];

(*Calcola somme diagonali di un quadrato*)
diagonalsTotal = Compile[{{square, _Integer, 2}},
			 Module[{},
				Return[{Sum[square[[i,i]],{i,1,Length[square]}],
					Sum[square[[-i,i]],{i,1,Length[square]}]}]
			 ], CompilationTarget->"C", Parallelization->True,
			 RuntimeOptions->"Speed"
		 ];

(*Calcola il numero di righe non corrette in un individuo*)
incorrectRows = Compile[{{ind, _Integer, 3}}, 
			   Module[{},
				  (*Conto le colonne che sottranendo
                                    il numero magico danno 0*)
				  Return[Length[ind[[1]]] -
					 Count[rowsDeviation[ind], 0]]
			   ], {{rowsDeviation[_], _Integer, 1}},
			   CompilationTarget -> "C", Parallelization -> True,
			   CompilationOptions -> {"InlineExternalDefinitions" -> True},
			   RuntimeOptions->"Speed"
		   ];

(*Calcola il numero di colonne non corrette in un individuo*)
incorrectColumns = Compile[{{ind, _Integer, 3}}, 
			   Module[{},
				  (*Conto le colonne che sottranendo
                                    il numero magico danno 0*)
				  Return[Length[ind[[1]]] -
					 Count[columnsDeviation[ind], 0]]
			   ], {{columnsDeviation[_], _Integer, 1}},
			   CompilationTarget -> "C", Parallelization -> True,
			   CompilationOptions -> {"InlineExternalDefinitions" -> True},
			   RuntimeOptions->"Speed"
		   ];

(*Restituisce il numero di linee non corrette di un individuo*)
incorrectLines = Compile[{{ind, _Integer, 3}}, 
			 Module[{},
				Return[incorrectRows[ind] + incorrectColumns[ind]]
			 ], {{incorrectRows[_], _Integer, 1},
			     {incorrectColumns[_], _Integer, 1}},
			 CompilationTarget -> "C", Parallelization -> True,
			 CompilationOptions -> {"InlineExternalDefinitions" -> True},
			 RuntimeOptions->"Speed"
		 ];

(*Calcola il numero di diagonali non corrette in un individuo*)
incorrectDiagonals = Compile[{{ind, _Integer, 3}}, 
			     Module[{},
				    (*Conto le diagonali che sottranendo
                                      il numero magico danno 0*)
				    Return[2 - Count[diagonalsDeviation[ind], 0]]
			     ], {{diagonalsDeviation[_], _Integer, 1}},
			     CompilationTarget -> "C", Parallelization -> True,
			     CompilationOptions -> {"InlineExternalDefinitions"->True},
			     RuntimeOptions->"Speed"
		     ];

(*Restituisce una lista contenete la differenza tra la righe e il valore magico*)
rowsDeviation = Compile[{{ind, _Integer, 3}}, 
			Module[{mv},
			       mv = magicNumber[Length[ind[[1]]]];
			       Return[Abs[rowsTotalInd[ind] - mv]]
			],{{rowsTotalInd[_], _Integer, 1},
			   {magicNumber[_], _Integer}},
			CompilationTarget -> "C", Parallelization -> True,
			CompilationOptions -> {"InlineExternalDefinitions"->True},
			RuntimeOptions->"Speed"
		];

(*Restituisce una lista contenete la differenza tra le colonne e il valore magico*)
columnsDeviation = Compile[{{ind, _Integer, 3}},
			   Module[{mv},
				  mv = magicNumber[Length[ind[[1]]]];
				  Return[Abs[columnsTotalInd[ind] - mv]]
			   ], {{columnsTotalInd[_], _Integer, 1}},
			   CompilationTarget->"C", Parallelization->True,
			   CompilationOptions -> {"InlineExternalDefinitions"->True},
			   RuntimeOptions->"Speed"
		   ];

(*Restituisce una lista contenete la differenza tra le diagonali e il valore magico*)
diagonalsDeviation = Compile[{{ind, _Integer, 3}}, 
			     Module[{mv},
				    mv = magicNumber[Length[ind[[1]]]];
				    Return[Abs[diagonalsTotalInd[ind] - mv]]
			     ], {{diagonalsTotalInd[_], _Integer, 1}},
			     CompilationTarget->"C", Parallelization->True,
			     CompilationOptions -> {"InlineExternalDefinitions"->True},
			     RuntimeOptions->"Speed"
		     ];

(*Calcola il numero di righe non corrette in un individuo*)
incorrectRows = Compile[{{ind, _Integer, 3}},
			Module[{},
			       (*Conto le righe che sottranendo
                                 il numero magico danno 0*)
			       Return[Length[ind[[1]]] - Count[rowsDeviation[ind], 0]]
			], {{rowsDeviation[_], _Integer, 1}},
			CompilationTarget->"C", Parallelization->True,
			CompilationOptions -> {"InlineExternalDefinitions"->True},
			RuntimeOptions->"Speed"
		];

(*Funzione di fitness di un individuo*)
fitnessInd = Compile[{{ind, _Integer, 3}},
		     Module[{inclin},
			    inclin = incorrectLines[ind];
			    If[inclin === 0,
			       Return[-Total[diagonalsDeviation[ind]]],
			       Return[Total[rowsDeviation[ind]
					    + columnsDeviation[ind]]]
			    ]
		     ], {{incorrectLines[_], _Integer, 1},
			 {diagonalsDeviation[_], _Integer, 1},
			 {rowsDeviation[_], _Integer, 1},
			 {columnsDeviation[_], _Integer, 1}},
		     CompilationTarget->"C", Parallelization->True,
		     CompilationOptions -> {"InlineExternalDefinitions"->True},
		     RuntimeOptions->"Speed"
	     ];

(*Funzione di fitness di una popolazione di individui*)
fitnessPopInd = Compile[{{pop, _Integer, 4}},
			Module[{},
			       Return[fitnessInd /@ pop]
			], {{fitnessInd[_], _Integer}},
			CompilationTarget->"C", Parallelization->True,
			CompilationOptions -> {"InlineExternalDefinitions"->True},
			RuntimeOptions->"Speed"
		];

(*Seleziona il figlio migliore*)
fittestChild = Compile[{{pop, _Integer, 4}}, 
		       Module[{fp, index},
			      fp = fitnessPopInd[pop];
			      (*Controllo se c'e' l'individuo perfetto*)
			      If[Length[Position[fp, 0]] =!= 0,
				 Return[pop[[Position[fp, 0][[1,1]]]]]
			      ];
			      (*Io voglio l'individuo piu' vicino a zero, ma voglio
                                anche privilegiare chi ha fitness negativa*)
			      If[Min[fp] < 0,
				 fp = Quotient[1, #] & /@ fp;
			      ];
			      index = Random[Integer,
					     {1, Length[Position[fp, Min[fp]]]}];
			      Return[pop[[Position[fp, Min[fp]][[index ,1]]]]]
		       ], {{fitnessPopInd[_], _Integer, 1}},
		       CompilationTarget->"C", Parallelization->True,
		       CompilationOptions -> {"InlineExternalDefinitions"->True},
		       RuntimeOptions->"Speed"
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
generateMutationSets[ind_List] :=
	Module[{rows, cols, S2r, S2c, S2, order, ret},
	       ret = ind[[1]];
	       order = Length[ret];
	       rows = rowsDeviation[ind];
	       cols = columnsDeviation[ind];
	       S2r = (*Parallel*)Table[If[rows[[i]] =!= 0,
					  ret[[i,j]], (*Il numero se e' in S2r*)
				          0], (*0 se non e' in S2r*)
				       {i, 1, order},
				       {j, 1, order}];
	       S2r = Flatten[S2r];
	       S2r = DeleteCases[S2r, 0];
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

	       (*Se tutte le righe e le colonne sono ok faccio solo permutazioni
                 di linee a caso*)
	       If[inclin === 0 && incorrectdiagonals === 0, Return[{ret, ind[[2]]}]];
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
				       incorrectrows = incorrectRows[reti];
				       incorrectcolumns = incorrectColumns[reti];
				       {S1, S2r, S2c, S2} = generateMutationSets[reti];
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
				       incorrectrows = incorrectRows[reti];
				       incorrectcolumns = incorrectColumns[reti];
				       {S1, S2r, S2c, S2} = generateMutationSets[reti];
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
			   For[j2 = 1, j2 <= order, j2++,
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

(*RISCRIVERLA NATIVA!!!!*)

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
rectifyLinesInd[ind_List] :=
	Module[{order, ret},
	       order = Length[ind[[1]]];
	       If[fitnessInd[ind] < 50 order &&
		  incorrectRows[ind] + incorrectColumns[ind] =!= 0,
		  ret = rectifyRowsWithOnePair[ind];
		  ret = rectifyRowsWithTwoPairs[ret];
		  ret = rectifyColumnsWithOnePair[ret];
		  ret = rectifyColumnsWithTwoPairs[ret];
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

(*Esegue l'algoritmo Xie-Kang*)
xiekang[order_Integer] :=
	Module[{father, offspring, gen, fitbest, timings, fittest, fr, fc, fd,
	       inclin, fit, fitbestindex},

	       (*timings contiene gli intervalli temporali*)
	       
	       Print["Costruisco un quadrato di ordine: ", order];
	       
	       Print["Genero il capostipide di tutti i quadrati"];
	       timings[0] = TimeUsed[];
	       father = generateInd[order];
	       timings[1] = TimeUsed[];
	       (*Print["Quadrato generato in ", timings[1] - timings[0], " s"];*)

	       If[fitnessInd[father] === 0,
		  Print["Ho costruito un quadrato magico in ",
			TimeUsed[] - timings[1], " s"];
		  Return[father[[1]]];
	       ];

	       (*Inizializzo una figliata di 10 elementi uguali al padre*)
	       gen = 1;
	       fitbest = fitnessInd[father];

	       Print["Il padre ha fitness ", fitbest];
	       
	       While[fitbest =!= 0 && gen < 30000,
		     (*     Print[father];*)
		     offspring = ParallelTable[father, {25}];
		     Print["Siamo alla generazione: ", gen];
		     Print["Effettuo mutazioni"];
		     timings[2] = TimeUsed[];
		     offspring = ParallelMap[mutateInd, offspring];
		     (*	     Print[fitnessPopInd[offspring]];*)
 (*	     Print["Mutazioni effettuate in ", TimeUsed[] - timings[2], " s"];*)
		     fittest = fittestChild[Append[offspring, father]];
		     fitbest = fitnessInd[fittest];
(*		     Print["Il miglior figlio ha fitness ", fitbest];
		     fr = incorrectRows[fittest];
		     fc = incorrectColumns[fittest];
		     fd = incorrectDiagonals[fittest];
		     Print["Righe sbagliate: ", fr];
		     Print["Colonne sbagliate: ", fc];
		     Print["Diagonali sbagliate: ", fd];
		     Print["Linee sbagliate: ", fr + fc + fd];*)
		     If[fitbest === 0,
			Print["TROVATO!"];
			Print["Ho costruito un quadrato magico in ",
			      TimeUsed[] - timings[1], " s"];
			Return[fittest[[1]]]
		     ];
		     If[fitbest < 50 order &&
			incorrectLines[fittest] =!= 0,
			Print["Rettifico righe"];
			(*			Print[fittest];*)
			timings[3] = TimeUsed[];
			offspring = rectifyLinesPop[offspring];
   (*		Print["Rettificazione effettuata in ", TimeUsed[] - timings[3],
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
		     If[incorrectLines[fittest] === 0 &&
			fitbest < 100,
			Print["Rettifico diagonali"];
			(*Print[fittest];*)
			timings[4] = TimeUsed[];
			offspring = rectifyDiagonalsPop[offspring];
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
			   Return[{TimeUsed[] - timings[1], gen, fittest[[1]],
				   fromValuesToList[fit, gen],
				   fromValuesToList[inclin, gen]}],
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


workout[n_Integer] :=
	Module[{a, k},
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
	];








(*
           _              _ _                                  
 _ __ ___ (_)___  ___ ___| | | __ _ _ __   ___  ___  _   _ ___ 
| '_ ` _ \| / __|/ __/ _ \ | |/ _` | '_ \ / _ \/ _ \| | | / __|
| | | | | | \__ \ (_|  __/ | | (_| | | | |  __/ (_) | |_| \__ \
|_| |_| |_|_|___/\___\___|_|_|\__,_|_| |_|\___|\___/ \__,_|___/
                                                               
 *)






(*
(*Calcola somme delle righe di un individuo*)
(*Total e' molto piu' efficiente di Plus @@ *)
rowsTotalInd = Compile[{{ind, _Integer, 3}},
		       Module[{},
			      Return[Table[Total[ind[[1,i]]],
					   {i,1,Length[ind[[1]]]}]]
		       ], CompilationTarget->"C", Parallelization->True,
		       RuntimeOptions->"Speed"
	       ];

(*Calcola somme colonne di un individuo*)
columnsTotalInd = Compile[{{ind, _Integer, 3}},
			  Module[{},
				 Return[Table[Total[Transpose[ind[[1]]][[i]]],
					      {i,1,Length[ind[[1]]]}]]
			  ], CompilationTarget->"C", Parallelization->True,
			  RuntimeOptions->"Speed"
		  ];

(*Calcola somme diagonali di un individuo*)
diagonalsTotalInd = Compile[{{ind, _Integer, 3}},
			    Module[{},
				   Return[{Sum[ind[[1,i,i]],{i,1,Length[ind[[1]]]}],
					    Sum[ind[[1,-i,i]],{i,1,Length[ind[[1]]]}]}]
			    ], CompilationTarget->"C", Parallelization->True,
			    RuntimeOptions->"Speed"
		    ];

(*Calcola somme diagonali di un quadrato*)
diagonalsTotal = Compile[{{square, _Integer, 2}},
			 Module[{},
				Return[{Sum[square[[i,i]],{i,1,Length[square]}],
					Sum[square[[-i,i]],{i,1,Length[square]}]}]
			 ], CompilationTarget->"C", Parallelization->True,
			 RuntimeOptions->"Speed"
		 ];

(*Calcola il numero di righe non corrette in un individuo*)
incorrectRows = Compile[{{ind, _Integer, 3}}, 
			   Module[{},
				  (*Conto le colonne che sottranendo
                                    il numero magico danno 0*)
				  Return[Length[ind[[1]]] -
					 Count[rowsDeviation[ind], 0]]
			   ], {{rowsDeviation[_], _Integer, 1}},
			   CompilationTarget -> "C", Parallelization -> True,
			   CompilationOptions -> {"InlineExternalDefinitions" -> True},
			   RuntimeOptions->"Speed"
		   ];

(*Calcola il numero di colonne non corrette in un individuo*)
incorrectColumns = Compile[{{ind, _Integer, 3}}, 
			   Module[{},
				  (*Conto le colonne che sottranendo
                                    il numero magico danno 0*)
				  Return[Length[ind[[1]]] -
					 Count[columnsDeviation[ind], 0]]
			   ], {{columnsDeviation[_], _Integer, 1}},
			   CompilationTarget -> "C", Parallelization -> True,
			   CompilationOptions -> {"InlineExternalDefinitions" -> True},
			   RuntimeOptions->"Speed"
		   ];

(*Restituisce il numero di linee non corrette di un individuo*)
incorrectLines = Compile[{{ind, _Integer, 3}}, 
			 Module[{},
				Return[incorrectRows[ind] + incorrectColumns[ind]]
			 ], {{incorrectRows[_], _Integer, 1},
			     {incorrectColumns[_], _Integer, 1}},
			 CompilationTarget -> "C", Parallelization -> True,
			 CompilationOptions -> {"InlineExternalDefinitions" -> True},
			 RuntimeOptions->"Speed"
		 ];

(*Calcola il numero di diagonali non corrette in un individuo*)
incorrectDiagonals = Compile[{{ind, _Integer, 3}}, 
			     Module[{},
				    (*Conto le diagonali che sottranendo
                                      il numero magico danno 0*)
				    Return[2 - Count[diagonalsDeviation[ind], 0]]
			     ], {{diagonalsDeviation[_], _Integer, 1}},
			     CompilationTarget -> "C", Parallelization -> True,
			     CompilationOptions -> {"InlineExternalDefinitions"->True},
			     RuntimeOptions->"Speed"
		     ];

(*Restituisce una lista contenete la differenza tra la righe e il valore magico*)
rowsDeviation = Compile[{{ind, _Integer, 3}}, 
			Module[{mv},
			       mv = magicNumber[Length[ind[[1]]]];
			       Return[Abs[rowsTotalInd[ind] - mv]]
			],{{rowsTotalInd[_], _Integer, 1},
			   {magicNumber[_], _Integer}},
			CompilationTarget -> "C", Parallelization -> True,
			CompilationOptions -> {"InlineExternalDefinitions"->True},
			RuntimeOptions->"Speed"
		];

(*Restituisce una lista contenete la differenza tra le colonne e il valore magico*)
columnsDeviation = Compile[{{ind, _Integer, 3}},
			   Module[{mv},
				  mv = magicNumber[Length[ind[[1]]]];
				  Return[Abs[columnsTotalInd[ind] - mv]]
			   ], {{columnsTotalInd[_], _Integer, 1}},
			   CompilationTarget->"C", Parallelization->True,
			   CompilationOptions -> {"InlineExternalDefinitions"->True},
			   RuntimeOptions->"Speed"
		   ];

(*Restituisce una lista contenete la differenza tra le diagonali e il valore magico*)
diagonalsDeviation = Compile[{{ind, _Integer, 3}}, 
			     Module[{mv},
				    mv = magicNumber[Length[ind[[1]]]];
				    Return[Abs[diagonalsTotalInd[ind] - mv]]
			     ], {{diagonalsTotalInd[_], _Integer, 1}},
			     CompilationTarget->"C", Parallelization->True,
			     CompilationOptions -> {"InlineExternalDefinitions"->True},
			     RuntimeOptions->"Speed"
		     ];

(*Calcola il numero di righe non corrette in un individuo*)
incorrectRows = Compile[{{ind, _Integer, 3}},
			Module[{},
			       (*Conto le righe che sottranendo
                                 il numero magico danno 0*)
			       Return[Length[ind[[1]]] - Count[rowsDeviation[ind], 0]]
			], {{rowsDeviation[_], _Integer, 1}},
			CompilationTarget->"C", Parallelization->True,
			CompilationOptions -> {"InlineExternalDefinitions"->True},
			RuntimeOptions->"Speed"
		];

(*Funzione di fitness di un individuo*)
fitnessInd[ind_List] :=
	Module[{inclin},
	       inclin = incorrectRows[ind] + incorrectColumns[ind];
	       If[inclin === 0,
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

(*
(*Funzione di fitness di un individuo*)
fitnessInd = Compile[{{ind, _Integer, 3}},
		     Module[{inclin},
			    inclin = incorrectLines[ind];
			    If[inclin === 0,
			       Return[-Total[diagonalsDeviation[ind]]],
			       Return[Total[rowsDeviation[ind]
					    + columnsDeviation[ind]]]
			    ]
		     ], {{incorrectLines[_], _Integer, 1},
			 {diagonalsDeviation[_], _Integer, 1},
			 {rowsDeviation[_], _Integer, 1},
			 {columnsDeviation[_], _Integer, 1}},
		     CompilationTarget->"C", Parallelization->True,
		     CompilationOptions -> {"InlineExternalDefinitions"->True},
		     RuntimeOptions->"Speed"
	     ];

(*Funzione di fitness di una popolazione di individui*)
fitnessPopInd = Compile[{{pop, _Integer, 4}},
			Module[{},
			       Return[fitnessInd /@ pop]
			], {{fitnessInd[_], _Integer}},
			CompilationTarget->"C", Parallelization->True,
			CompilationOptions -> {"InlineExternalDefinitions"->True},
			RuntimeOptions->"Speed"
		];

(*Seleziona il figlio migliore*)
fittestChild = Compile[{{pop, _Integer, 4}}, 
		       Module[{fp, index},
			      fp = fitnessPopInd[pop];
			      (*Controllo se c'e' l'individuo perfetto*)
			      If[Length[Position[fp, 0]] =!= 0,
				 Return[pop[[Position[fp, 0][[1,1]]]]]
			      ];
			      (*Io voglio l'individuo piu' vicino a zero, ma voglio
                                anche privilegiare chi ha fitness negativa*)
			      If[Min[fp] < 0,
				 fp = (#)^(-1) & /@ fp;
			      ];
			      index = Random[Integer,
					     {1, Length[Position[fp, Min[fp]]]}];
			      Return[pop[[Position[fp, Min[fp]][[index ,1]]]]]
		       ], {{fitnessPopInd[_], _Integer, 1}},
		       CompilationTarget->"C", Parallelization->True,
		       CompilationOptions -> {"InlineExternalDefinitions"->True},
		       RuntimeOptions->"Speed"
	       ];

(*Calcola il valore di sigma t, che e' un parametro che serve per la nuova
  varianza dopo le mutazioni*)
sigmat = Compile[{{ind, _Integer, 3}}, 
		 Module[{nincorrect, a, b, ret},
			nincorrect = incorrectLines[ind];
			If[nincorrect =!= 0,
			   ret = Total[rowsDeviation[ind]] +
				 Total[columnsDeviation[ind]]/nincorrect,
			   ret = Total[diagonalsDeviation[ind]]/incorrectDiagonals[ind]
		 	];
			Return[ret]
		 ], {{incorrectLines[_], _Integer},
		     {rowsDeviation[_], _Integer, 1},
		     {columnsDeviation[_], _Integer, 1},
		     {diagonalsDeviation[_], _Integer, 1},
		     {incorrectDiagonals[_], _Integer}},
		 CompilationTarget->"C", Parallelization->True,
		 CompilationOptions -> {"InlineExternalDefinitions"->True},
		 RuntimeOptions->"Speed"
	 ];*)

(*Costruisce gli insiemi da cui prendere gli elementi di mutazione*)
generateMutationSets[ind_List] :=
	Module[{rows, cols, S2r, S2c, S2, order, ret},
	       ret = ind[[1]];
	       order = Length[ret];
	       rows = rowsDeviation[ind];
	       cols = columnsDeviation[ind];
	       S2r = (*Parallel*)Table[If[rows[[i]] =!= 0,
					  ret[[i,j]], (*Il numero se e' in S2r*)
				          0], (*0 se non e' in S2r*)
				       {i, 1, order},
				       {j, 1, order}];
	       S2r = Flatten[S2r];
	       S2r = DeleteCases[S2r, 0];
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

	       (*Se tutte le righe e le colonne sono ok faccio solo permutazioni
                 di linee a caso*)
	       If[inclin === 0 && incorrectdiagonals === 0, Return[{ret, ind[[2]]}]];
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
				       incorrectrows = incorrectRows[reti];
				       incorrectcolumns = incorrectColumns[reti];
				       {S1, S2r, S2c, S2} = generateMutationSets[reti];
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
				       incorrectrows = incorrectRows[reti];
				       incorrectcolumns = incorrectColumns[reti];
				       {S1, S2r, S2c, S2} = generateMutationSets[reti];
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
			   For[j2 = 1, j2 <= order, j2++,
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

(*RISCRIVERLA NATIVA!!!!*)

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
rectifyLinesInd[ind_List] :=
	Module[{order, ret},
	       order = Length[ind[[1]]];
	       If[fitnessInd[ind] < 50 order &&
		  incorrectRows[ind] + incorrectColumns[ind] =!= 0,
		  ret = rectifyRowsWithOnePair[ind];
		  ret = rectifyRowsWithTwoPairs[ret];
		  ret = rectifyColumnsWithOnePair[ret];
		  ret = rectifyColumnsWithTwoPairs[ret];
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

(*Esegue l'algoritmo Xie-Kang*)
xiekang[order_Integer] :=
	Module[{father, offspring, gen, fitbest, timings, fittest, fr, fc, fd,
	       inclin, fit, fitbestindex},

	       (*timings contiene gli intervalli temporali*)
	       
	       Print["Costruisco un quadrato di ordine: ", order];
	       
	       Print["Genero il capostipide di tutti i quadrati"];
	       timings[0] = TimeUsed[];
	       father = generateInd[order];
	       timings[1] = TimeUsed[];
	       (*Print["Quadrato generato in ", timings[1] - timings[0], " s"];*)

	       If[fitnessInd[father] === 0,
		  Print["Ho costruito un quadrato magico in ",
			TimeUsed[] - timings[1], " s"];
		  Return[father[[1]]];
	       ];

	       (*Inizializzo una figliata di 10 elementi uguali al padre*)
	       gen = 1;
	       fitbest = fitnessInd[father];

	       Print["Il padre ha fitness ", fitbest];
	       
	       While[fitbest =!= 0,
		     (*     Print[father];*)
		     offspring = ParallelTable[father, {25}];
		     Print["Siamo alla generazione: ", gen];
		     Print["Effettuo mutazioni"];
		     timings[2] = TimeUsed[];
		     offspring = ParallelMap[mutateInd, offspring];
		     (*	     Print[fitnessPopInd[offspring]];*)
 (*	     Print["Mutazioni effettuate in ", TimeUsed[] - timings[2], " s"];*)
		     fittest = fittestChild[Append[offspring, father]];
		     fitbest = fitnessInd[fittest];
(*		     Print["Il miglior figlio ha fitness ", fitbest];
		     fr = incorrectRows[fittest];
		     fc = incorrectColumns[fittest];
		     fd = incorrectDiagonals[fittest];
		     Print["Righe sbagliate: ", fr];
		     Print["Colonne sbagliate: ", fc];
		     Print["Diagonali sbagliate: ", fd];
		     Print["Linee sbagliate: ", fr + fc + fd];*)
		     If[fitbest === 0,
			Print["TROVATO!"];
			Print["Ho costruito un quadrato magico in ",
			      TimeUsed[] - timings[1], " s"];
			Return[fittest[[1]]]
		     ];
		     If[fitbest < 50 order &&
			incorrectLines[fittest] =!= 0,
			Print["Rettifico righe"];
			(*			Print[fittest];*)
			timings[3] = TimeUsed[];
			offspring = rectifyLinesPop[offspring];
   (*		Print["Rettificazione effettuata in ", TimeUsed[] - timings[3],
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
		     If[incorrectLines[fittest] === 0 &&
			fitbest < 100,
			Print["Rettifico diagonali"];
			(*Print[fittest];*)
			timings[4] = TimeUsed[];
			offspring = rectifyDiagonalsPop[offspring];
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
			   Print["Prendo anche il padre"];
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
			   Return[{TimeUsed[] - timings[1], fittest[[1]],
				   fromValuesToList[fit, gen],
				   fromValuesToList[inclin, gen]}],
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
	];
 *)
