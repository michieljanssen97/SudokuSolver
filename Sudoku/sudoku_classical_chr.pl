:- use_module(library(chr)).
:- use_module(library(lists)).
:- include(sudex_toledo).
:- chr_constraint cell_classical/5, cell_classical_options/6, fillone/1, print/2, domainSize/1, clear/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU CLASSICAL SOLVER
% This solution is inspired by the solution found in the Constraint Handling Rules book of T. Frühwirth on page 218
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * These rules are simplification rules. In the first two rules the head also contains a passive constraint. 
 * This means that when e.g. cell_classical_options constraint becomes active it will not be able to call/see this rule. 
 * The first rule will search for a cell, that has still no number assigned to it, that has the same amount of options as N (N2=N).
 * When such a cell has been found we take the first element out of its option list and with this number a new constraint, cell_classical, is been made.
 * This constraint will probably call/activate other rules. Once this is done the constraint fillone(1) is created which will also call/activate one of these three
 * rules. When no cell has been found we increase the number N. This makes sure that we always try the cell with the least amount of options.
**/
fillone(N), cell_classical_options(BR,BC,C,R,N2,L)#Id <=> N2=N | member(V,L), cell_classical(BR,BC,C,R,V), fillone(1) pragma passive(Id).
fillone(N), domainSize(D)#Id <=> N < D | N1 is N+1, domainSize(D), fillone(N1) pragma passive(Id).
fillone(_) <=> true.

/**
 * This predicate puts the constraint fillone(1) into the constraint store which will be the beginning of filling the sudoku with numbers.
**/
solve :- fillone(1).

/**
 * This rule is a simpagation rule. This rule exists because of the way we create our initial constraints.
 * First we create for every cell in the sudoku a cell_classical_options constraint. Afterwards we read out the information from the given board.
 * Creating a cell_classical constraint for the cells that already have a value in the given board. Because there are now two constraints for some cells we have 
 * to delete the cell_classical_options constraints for those cells because they don't have options any more. Which is done in this rule.
**/
cell_classical(BR,BC,R,C,_) \ cell_classical_options(BR,BC,R,C,_,_)#Id <=> true pragma passive(Id).

/**
 * These rules are all simpagation rules. These rules implement the sudoku constraints. That is in the first rule we make sure that cells that are in the same column 
 * have all different values. In the second rule this is made sure for cells that are in the same row and in the last rule it holds for cells in the same block.
 * Cells that do not have a value assigned to it are represented by the cell_classical_options that holds a list of all possible values for that cell. These rules will
 * update that cell whenever a new cell has a value assigned to it (cell_classical) and if that value is present in there list. If so that value will be taken out
 * of that list and a new cell_classical_options will be created for that corresponding cell with the updated list.
**/
cell_classical(_,BC,_,C,V) \ cell_classical_options(BR,BC,R,C,N,L)#Id <=> select(V,L,LL) | N1 is N-1, N1>0, cell_classical_options(BR,BC,R,C,N1,LL) pragma passive(Id).
cell_classical(BR,_,R,_,V) \ cell_classical_options(BR,BC,R,C,N,L)#Id <=> select(V,L,LL) | N1 is N-1, N1>0, cell_classical_options(BR,BC,R,C,N1,LL) pragma passive(Id).
cell_classical(BR,BC,_,_,V) \ cell_classical_options(BR,BC,R,C,N,L)#Id <=> select(V,L,LL) | N1 is N-1, N1>0, cell_classical_options(BR,BC,R,C,N1,LL) pragma passive(Id).

/**
 * This set of predicates and rules lets us print out the sudoku. Row per row and column per column. The rules are simplification rules.
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
clear <=> true.

/**
 * This predicate is the main predicate that will be called whenever you want to solve a puzzle. You give the name of the puzzle you want to solve and as a result
 * a solution of the puzzle will be printed out to the screen.
**/
solve_classical(Name) :-
	puzzles(Puzzle,Name),
	length(Puzzle,N),
	domainSize(N),
	BlockSize is truncate(sqrt(N)),
	UpperLimit is N + 1,
	init_board(UpperLimit,BlockSize,N),
	loop_row(Puzzle,1,1,BlockSize),
	solve,
	printsolution,
	nl,
	clear.

/**
 * These predicates are helper predicates. The first three predicates will result in creating a cell_classical_option constraint for every cell.
 * The last two predicates will read out the information from the given puzzle. And for the cells that already have a value assigned to it it will 
 * create a cell_classical constraint.
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
	NewC is C + 1,
	NewBlockColumn is div(C,BlockSize) + 1,
	init_col(BlockRow,NewBlockColumn,R,NewC,UpperLimit,BlockSize,N).

loop_row([],_,_,_).
loop_row([H|T],Row,BlockRow,BlockSize) :-
	loop_col(H,1,Row,BlockRow,1,BlockSize),
	NewRow is Row + 1,
	NewBlockRow is div(Row,BlockSize) + 1,
	loop_row(T,NewRow,NewBlockRow,BlockSize).

loop_col([],_,_,_,_,_).
loop_col([H|T],Col,Row,BlockRow,BlockColumn,BlockSize) :-
	(nonvar(H) ->
		cell_classical(BlockRow,BlockColumn,Row,Col,H)
		;
		true
	),
	NewCol is Col + 1,
	NewBlockColumn is div(Col,BlockSize) + 1,
	loop_col(T,NewCol,Row,BlockRow,NewBlockColumn,BlockSize).

/**
 * This predicate is purely for testing. It solves all the sudokus from the benchmark and prints out the time it took to solve them all.
**/
test :-
	statistics(runtime, [_ | [_]]),
	solve_classical(lambda),
	solve_classical(hard17),
	solve_classical(eastermonster),
	solve_classical(tarek_052),
	solve_classical(goldennugget),
	solve_classical(coloin),
	solve_classical(extra2),
	solve_classical(extra3),
	solve_classical(extra4),
	solve_classical(inkara2012),
	solve_classical(clue18),
	solve_classical(clue17),
	solve_classical(sudowiki_nb28),
	solve_classical(sudowiki_nb49),
	statistics(runtime, [_ | [ExecutionTime]]),
	write('Execution took '), write(ExecutionTime), write(' ms.'), nl.
