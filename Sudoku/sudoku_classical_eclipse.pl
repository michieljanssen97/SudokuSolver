:- lib(ic).
:- compile(sudex_toledo).
:- compile(sudoku_helper_eclipse).
:- import alldifferent/1 from ic_global.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU CLASSICAL SOLVER
% This solution is inspired by the solution given by J. Schimpf found on the ECLiPSe website
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * This predicate solves a given sudoku with the classical viewpoint.
 * param: SudokuName = the given sudoku name that needs to be solved.
 * The result of this predicate will be the printed solution of the given sudoku.
**/
solve_classical(SudokuName) :-
	puzzles(Matrix,SudokuName),
	board_to_array(Matrix, Array),
	writeln("Given board:"),
	print_board(Array),
	sudoku_classical(Array),
	statistics(runtime, [_ | [_]]),
	search(naive, Array, _),
	statistics(runtime, [_ | [ExecutionTimeMS1]]),
	ExTimeS1 is ExecutionTimeMS1 / 1000,
	writeln(ExTimeS1),
	writeln("Solution:"),
	print_board(Array).

/**
 * This predicate solves a given sudoku with the classical viewpoint.
 * param: SudokuName = the given sudoku name that needs to be solved.
 * The result of this predicate will be the printed solution of the given sudoku 
 * and the amount of backtracks will be calculated.
**/
solve_classical(SudokuName,Backtracks) :-
    puzzles(Matrix,SudokuName),
	board_to_array(Matrix, Array),
	writeln("Given board:"),
	print_board(Array),
	sudoku_classical(Array),
	search(naive, Array, Backtracks),
	writeln("Solution:"),
	print_board(Array),
	writeln("Amount of backtracks: "), 
	writeln(Backtracks).


/**
 * This predicate put constraints on the given sudoku with the classical viewpoint.
 * param: Board = the given sudoku board which consists of integer values or variables.
**/
sudoku_classical(Board) :-
	dim(Board, [N,N]),
	Board :: 1..N,
	% row and column constraint 
	( for(I,1,N), param(Board,N) do
	    alldifferent(Board[I,1..N]),
	    alldifferent(Board[1..N,I])
	),
	% block constraint
	N2 is integer(sqrt(N)),
	( multifor([I,J],1,N,N2), param(Board,N2) do
		SubBoard is Board[I..I+N2-1, J..J+N2-1],
		flatten(SubBoard, SubBoardFlat),
		alldifferent(SubBoardFlat)
	).