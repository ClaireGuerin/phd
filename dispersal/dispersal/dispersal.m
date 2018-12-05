(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Dec 4, 2018 *)

BeginPackage["dispersal`"]
(* Exported symbols added here with SymbolName::usage *) 

proceduraldispersal::usage="proceduraldispersal[pop,mig] disperses individuals at rate {mig} between patches in a spatially structured population {pop} following a procedural programming strategy"
functionaldispersal::usage="functionaldispersal[pop,mig] disperses individuals at rate {mig} between patches in a spatially structured population {pop} following a functional programming strategy"

Begin["`Private`"]
(* Implementation of the package *)

proceduraldispersal[pop_, mig_] := 
 Block[{nG, numberIndivPerIsland, disp, destination, 
   popAfterDispersal, individualCounter},
  nG = Length[pop];
  numberIndivPerIsland = Length /@ pop;
  disp = RandomChoice[{1 - mig, mig} -> {0, 1}, 
    Total[numberIndivPerIsland]]; (* Do you disperse (1) or not (0)?, 
  for each individual in Ntotal*)
  
  destination = 
   RandomInteger[{1, nG}, Total[numberIndivPerIsland]] ; (* 
  destination for each individual in population toward any group in \
nG*)
  popAfterDispersal = Table[{}, {i, nG}];
  individualCounter = 0;
        Table[
               individualCounter++;
               Which[ disp[[individualCounter]] == 0,  
    AppendTo[popAfterDispersal[[i]] , pop[[i, j]]],
                      disp[[individualCounter]] == 1 ,  
    AppendTo[popAfterDispersal[[destination[[individualCounter]]]] , 
     pop[[i, j]]] ]
               , {i, nG}, {j, 1, numberIndivPerIsland[[i]]}];
  Return[popAfterDispersal]
  ]
  
moveonegrouptrait[pop_, disp_, grp_, trait_: 0] := If[trait > 0, 
	pop[[#1[[1]], trait, #1[[2]]]] & /@ Position[disp, grp],
	pop[[#1[[1]], #1[[2]]]] & /@ Position[disp, grp]];
moveallgrouptraits[pop_, disp_, grp_, ntraits_] := If[ntraits > 1,
	moveonegrouptrait[pop, disp, grp, #] & /@ Range[ntraits],
	moveonegrouptrait[pop, disp, grp]];
functionaldispersal[pop_, mig_] := Block[{nG, nT, psizes, transitions, dispersal},
	nG = Length[pop]; (* number of groups in the population *);
	nT = If[Head[pop[[1]][[1]]] == Real,
		1, Length[pop[[1]]]]; (*number of traits in the population *);
	transitions = ReplacePart[(mig/(nG-1)) & /@ Range[nG], # -> 1 - mig] & /@ Range[nG]; (*create a "matrix of transitions" between every group, including group of origin*)
	psizes = If[nT > 1, Length[pop[[#]][[1]]] & /@ Range[nG],
		Length[pop[[#]]] & /@ Range[nG]];
	dispersal = RandomChoice[transitions[[#]] -> Range[nG], psizes[[#]]] & /@ Range[nG]; (*assign each individual in every group to a "new" group. It may be the same as the group of origin if the individual is resident*)
	Return[moveallgrouptraits[pop, dispersal, #, nT] & /@ Range[nG]]];

End[]

EndPackage[]

