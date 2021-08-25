:- lib(ic).
:- compile(sudex_toledo).
:- compile(sudoku_alternative_eclipse).
:- compile(sudoku_classical_eclipse).
:- import bool_channeling/3 from ic_global.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLVER WITH CHANNELING CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * This predicate solves a given sudoku using the channeling constraints.
 * param: SudokuName = the given sudoku name that needs to be solved.
 * The result of this predicate will be the printed solution of the given sudoku.
**/
solve_channeling(SudokuName) :-
	puzzles(Matrix,SudokuName),
	board_to_array(Matrix, Array),
	board_to_binary_array(Matrix,BinaryBoard),
	writeln("Given board:"),
	print_board(Array),
	sudoku_classical(Array),
	sudoku_alternative(BinaryBoard),
	channeling(Array,BinaryBoard),
	statistics(hr_time, Start),
	search(naive, BinaryBoard, _),
	statistics(hr_time, End),
	Time is End - Start,
	writeln(Time),
	writeln("Solution:"),
	print_board(Array).
/**
 * This predicate solves a given sudoku using the channeling constraints.
 * param: SudokuName = the given sudoku name that needs to be solved.
 * The result of this predicate will be the printed solution of the given sudoku 
 * and the amount of backtracks will be calculated.
**/
solve_channeling(SudokuName,Backtracks) :-
	puzzles(Matrix,SudokuName),
	board_to_array(Matrix, Array),
	board_to_binary_array(Matrix,BinaryBoard),
	writeln("Given board:"),
	print_board(Array),
	sudoku_classical(Array),
	sudoku_alternative(BinaryBoard),
	channeling(Array,BinaryBoard),
	search(naive, BinaryBoard, Backtracks),
	writeln("Solution:"),
	print_board(Array),
	writeln("Amount of backtracks: "), 
	writeln(Backtracks).

/**
 * This predicate defines the channeling constraints.
 * param: Array = the given sudoku array that contains the integere values or variables of the board.
 * param: BinaryBoard = the given sudoku board which consists of binary boolean arrays.
**/
channeling(Array,BinaryBoard) :-
	dim(Array, [N,N]),
	( multifor([I,J],1,N), param(Array,BinaryBoard,N) do
  		Value is Array[I,J], 
  		Binary is BinaryBoard[I,J,1..N], 
  		bool_channeling(Value,Binary,1) 
	).