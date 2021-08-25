:- compile(sudoku_helper_eclipse).
:- import nth1/3 from listut.
:- import sumlist/2 from ic_global.
:- include(sudex_toledo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU ALTERNATIVE SOLVER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * This predicate solves a given sudoku with the alternative viewpoint.
 * param: SudokuName = the given sudoku name that needs to be solved.
 * The result of this predicate will be the printed solution of the given sudoku.
**/
solve_alternative(SudokuName) :-
	puzzles(Matrix,SudokuName),
	board_to_array(Matrix, Array),
	writeln("Given board:"),
	print_board(Array),
	board_to_binary_array(Matrix,BinaryBoard),
	writeln("Binary board:"),
	print_binary_board(BinaryBoard),
	sudoku_alternative(BinaryBoard),
	statistics(hr_time, Start),
	search(naive, BinaryBoard, _),
	statistics(hr_time, End),
	Time is End - Start,
	writeln(Time),
	writeln("Solution:"),
	print_binary_board(BinaryBoard).

/**
 * This predicate solves a given sudoku with the classical viewpoint.
 * param: SudokuName = the given sudoku name that needs to be solved.
 * The result of this predicate will be the printed solution of the given sudoku 
 * and the amount of backtracks will be calculated.
**/
solve_alternative(SudokuName,Backtracks) :-
    puzzles(Matrix,SudokuName),
	board_to_array(Matrix, Array),
	writeln("Given board:"),
	print_board(Array),
	board_to_binary_array(Matrix,BinaryBoard),
	writeln("Binary board:"),
	print_binary_board(BinaryBoard),
	sudoku_alternative(BinaryBoard),
	search(naive, BinaryBoard, Backtracks),
	writeln("Solution:"),
	print_binary_board(BinaryBoard),
	writeln("Amount of backtracks: "), 
	writeln(Backtracks).

/**
 * This predicate put constraints on the given sudoku with the alternative viewpoint.
 * param: Board = the given sudoku board which consists of binary boolean arrays.
**/
sudoku_alternative(Board) :-
	dim(Board, [N,N,N]),
	Board :: 0..1,
	% row and column constraint 
	( for(I,1,N), param(Board,N) do
		Row is Board[I,1..N],
		Column is Board[1..N,I],
	    sum_check(Row),
	    sum_check(Column)
	),
	%block constraint
	N2 is integer(sqrt(N)),
	( multifor([I,J],1,N,N2), param(Board,N2) do
		SubBoard is Board[I..I+N2-1, J..J+N2-1],
		flatten(SubBoard, SubBoardFlat),
		sum_check(SubBoardFlat)
	).

/** This predicate checks the following 2 constraints:
* 1: The binary array of size N (equivalent to one sudoku cell) has a sum equal to 1
* 2: The entire row/column/block does not contain the same value more than once.
*	 This is done by making sure that, for each binary array in the same row/column/block, 
*    the sum of all the binary values on the same position in the binary arrays is equal to 1.
* param: BinaryArray = a binary boolean array on which the constraints are checked.
**/
sum_check(BinaryArray) :-
	length(BinaryArray,N),
	( for(I,1,N), param(BinaryArray,N) do
		dim(CheckArray,[N]),
		( for(J,1,N), param(CheckArray,BinaryArray,I) do
			nth1(J,BinaryArray,BinaryArrayElem),
			array_list(BinaryArrayElem,BinaryArrayElemList),
			sumlist(BinaryArrayElemList,SumBinaryArrayElemList),
			SumBinaryArrayElemList #= 1,
			SumBinaryArrayElem is BinaryArrayElem[I],
			CheckArray[J] #= SumBinaryArrayElem
		),
		array_list(CheckArray,CheckList),
		sumlist(CheckList,CheckSum),
		CheckSum #=1
	).

