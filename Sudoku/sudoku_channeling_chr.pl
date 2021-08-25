:- use_module(library(chr)).
:- use_module(library(lists)).
:- include(sudex_toledo).
:- chr_constraint cell_alternative/5, cell_classical_options/6, cell_classical/5, cell_alternative_options/6, board_size/1, fillone/1, print/2, clear/0, domainSize/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU CHANNELING SOLVER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * These rules comes strait out of the sudoku_classical_chr file so for more information we refer to that file.
**/
fillone(N), cell_classical_options(BR,BC,C,R,N2,L)#Id <=> N2=N | member(V,L), cell_classical(BR,BC,C,R,V), fillone(1) pragma passive(Id).
fillone(N), domainSize(D)#Id <=> N < D | N1 is N+1, domainSize(D), fillone(N1) pragma passive(Id).
fillone(_) <=> true.

/**
 * This predicate puts the constraint fillone(1) into the constraint store which will be the beginning of filling the sudoku with binary arrays.
**/
solve :- fillone(1).

/**
 * These 2 rules implement the channeling between the two viewpoint. The first rule channels information from the alternative viewpoint to the classical viewpoint.
 * The second rule the other way around. When a solution is found in one of the two viewpoints the same cell in the other viewpoint is updated to have the same value.
**/
cell_alternative(BR,BC,R,C,V) \ cell_classical_options(BR,BC,R,C,_,_)#Id <=> nth1(Idx,V,1) | cell_classical(BR,BC,R,C,Idx) pragma passive(Id).
cell_classical(BR,BC,R,C,V), domainSize(D)#Id2 \ cell_alternative_options(BR,BC,R,C,_,_)#Id <=> D1 is D+1, create_bin_list(1,D1,V,[],Result) | cell_alternative(BR,BC,R,C,Result) pragma passive(Id), passive(Id2).

/**
 * These 2 rules filter out duplicate constraints that can be occur because of the channeling.
**/
cell_classical(BR,BC,R,C,V) \ cell_classical(BR,BC,R,C,V) <=> true.
cell_alternative(BR,BC,R,C,V) \cell_alternative(BR,BC,R,C,V) <=> true.

/**
 * These eight rules come strait from the files sudoku_classical_chr and sudoku_alternative_chr. So for more information we refer to those files.
**/
cell_classical(BR,BC,R,C,_) \ cell_classical_options(BR,BC,R,C,_,_)#Id <=> true pragma passive(Id).

cell_classical(_,BC,_,C,V) \ cell_classical_options(BR,BC,R,C,N,L)#Id <=> select(V,L,LL) | N1 is N-1, N1>0, cell_classical_options(BR,BC,R,C,N1,LL) pragma passive(Id).
cell_classical(BR,_,R,_,V) \ cell_classical_options(BR,BC,R,C,N,L)#Id <=> select(V,L,LL) | N1 is N-1, N1>0, cell_classical_options(BR,BC,R,C,N1,LL) pragma passive(Id).
cell_classical(BR,BC,_,_,V) \ cell_classical_options(BR,BC,R,C,N,L)#Id <=> select(V,L,LL) | N1 is N-1, N1>0, cell_classical_options(BR,BC,R,C,N1,LL) pragma passive(Id).

cell_alternative(BR,BC,R,C,_) \ cell_alternative_options(BR,BC,R,C,_,_)#Id <=> true pragma passive(Id).

cell_alternative(_,BC,_,C,V) \ cell_alternative_options(BR,BC,R,C,_,L)#Id <=> subtract(L,[V],LL) | length(LL,N1), N1>0, cell_alternative_options(BR,BC,R,C,N1,LL) pragma passive(Id).
cell_alternative(BR,_,R,_,V) \ cell_alternative_options(BR,BC,R,C,_,L)#Id <=> subtract(L,[V],LL) | length(LL,N1), N1>0, cell_alternative_options(BR,BC,R,C,N1,LL) pragma passive(Id).
cell_alternative(BR,BC,_,_,V) \ cell_alternative_options(BR,BC,R,C,_,L)#Id <=> subtract(L,[V],LL) | length(LL,N1), N1>0, cell_alternative_options(BR,BC,R,C,N1,LL) pragma passive(Id).

/**
 * This set of predicates and rules comes strait out of the sudoku_classical_chr file so for more information we refer to that file.
**/
printsolution :- printRow(1), nl, printRow(2), nl, printRow(3), nl, nl, printRow(4), nl, printRow(5), nl, printRow(6), nl, nl, printRow(7), nl, printRow(8), nl, printRow(9), nl.
printRow(R) :- printCol(R,1), printCol(R,2), printCol(R,3), write('  '), printCol(R,4), printCol(R,5), printCol(R,6), write('  '), printCol(R,7), printCol(R,8), printCol(R,9).
printCol(R,C) :- print(R,C).
print(R,C), cell_classical(_,_,R,C,Val) <=> write(Val).
print(_,_) <=> write('.').

/**
 * This rule is only here for cleaning the constraint store. Such that in the end only the printed solution is visible and not some constraints that are still there.
**/
clear \ domainSize(_) <=> true.
clear \ cell_alternative(_,_,_,_,_) <=> true.
clear <=> true.

/**
 * This predicate is the main predicate that will be called whenever you want to solve a puzzle. You give the name of the puzzle you want to solve and as a result
 * a solution of the puzzle will be printed out to the screen.
**/
solve_channeling(Name) :-
	puzzles(Puzzle,Name),
	length(Puzzle,N),
	domainSize(N),
	BlockSize is truncate(sqrt(N)),
	UpperLimit is N + 1,
	init_board(UpperLimit,BlockSize,N),
	loop_row(Puzzle,1,1,BlockSize,UpperLimit),
	solve,
	printsolution,
	nl,
	clear.

/**
 * These predicates are helper predicates. And are copied from sudoku_classical_chr and sudoku_alternative_chr. So for more information we refer to those files.
**/
init_board(UpperLimit,BlockSize,N) :-
	init_row(1,1,1,UpperLimit,BlockSize,N).

init_row(_,UpperLimit,_,UpperLimit,_,_).
init_row(BlockRow,R,C,UpperLimit,BlockSize,N) :-
	init_col(BlockRow,1,R,1,UpperLimit,BlockSize,N),
	NewR is R + 1,
	NewBlockRow is div(R,BlockSize) + 1,
	init_row(NewBlockRow,NewR,C,UpperLimit,BlockSize,N).

init_col(_,_,_,UpperLimit,UpperLimit,_,_).
init_col(BlockRow,BlockColumn,R,C,UpperLimit,BlockSize,N) :-
	numlist(1,N,InitalList),
	cell_classical_options(BlockRow,BlockColumn,R,C,N,InitalList),
	inital_list(UpperLimit,1,[],L),
	cell_alternative_options(BlockRow,BlockColumn,R,C,N,L),
	NewC is C + 1,
	NewBlockColumn is div(C,BlockSize) + 1,
	init_col(BlockRow,NewBlockColumn,R,NewC,UpperLimit,BlockSize,N).

loop_row([],_,_,_,_).
loop_row([H|T],Row,BlockRow,BlockSize,UpperLimit) :-
	loop_col(H,1,Row,BlockRow,1,BlockSize,UpperLimit),
	NewRow is Row + 1,
	NewBlockRow is div(Row,BlockSize) + 1,
	loop_row(T,NewRow,NewBlockRow,BlockSize,UpperLimit).

loop_col([],_,_,_,_,_,_).
loop_col([H|T],Col,Row,BlockRow,BlockColumn,BlockSize,UpperLimit) :-
	(nonvar(H) ->
		cell_classical(BlockRow,BlockColumn,Row,Col,H),
		create_bin_list(1,UpperLimit,H,[],List),
		cell_alternative(BlockRow,BlockColumn,Row,Col,List)
		;
		true
	),
	NewCol is Col + 1,
	NewBlockColumn is div(Col,BlockSize) + 1,
	loop_col(T,NewCol,Row,BlockRow,NewBlockColumn,BlockSize,UpperLimit).


inital_list(UpperLimit,UpperLimit,L,L).
inital_list(UpperLimit,I,L,List) :-
	create_bin_list(1,UpperLimit,I,[],BinList),
	append(L,[BinList],NewL),
	NewI is I + 1,
	inital_list(UpperLimit,NewI,NewL,List).

create_bin_list(UpperLimit,UpperLimit,_,L,L).
create_bin_list(Start,UpperLimit,H,L,List) :-	
	(Start == H -> 	
		append(L,[1],NewL)
	;
		append(L,[0],NewL)
	),
	NewStart is Start + 1,
	create_bin_list(NewStart,UpperLimit,H,NewL,List).

/**
 * This predicate is purely for testing. It solves all the sudokus from the benchmark and prints out the time it took to solve them all.
**/
test :-
	statistics(runtime, [_ | [_]]),
	solve_channeling(lambda),
	solve_channeling(hard17),
	solve_channeling(eastermonster),
	solve_channeling(tarek_052),
	solve_channeling(goldennugget),
	solve_channeling(coloin),
	solve_channeling(extra2),
	solve_channeling(extra3),
	solve_channeling(extra4),
	solve_channeling(inkara2012),
	solve_channeling(clue18),
	solve_channeling(clue17),
	solve_channeling(sudowiki_nb28),
	solve_channeling(sudowiki_nb49),
	statistics(runtime, [_ | [ExecutionTime]]),
	write('Execution took '), write(ExecutionTime), write(' ms.'), nl.