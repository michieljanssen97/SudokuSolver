:- lib(listut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES:
% Procedures that are common over all the eclipse files for solving sudokus
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * This predicate prints the given sudoku board.
 * param: Board = the given sudoku board which consists of integer values or variables.
**/
print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.

/**
 * This predicate prints the given binary sudoku board.
 * param: Board = the given sudoku board which consists of binary boolean arrays.
**/
print_binary_board(Board) :-
	dim(Board, [N,N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I,N) do
	    	( for(K,1,N), param(Board,I,J) do
	    		X is Board[I,J,K],
				print(X)
	    		),
	    	print("   ")
	    ), nl
	), nl.

/**
 * This predicate changes the input matrix to an array of arrays.
 * param: Matrix = the given sudoku matrix.
 * output: Array = the array of arrays that consist of integer values or variables (denoted by "_").
**/
board_to_array(Matrix, Array):-
    length(Matrix, N),
    dim(Array, [N, N]),
    ( multifor([I, J], 1, N), param(Matrix, Array) do
        nth1(I, Matrix, Row),
        nth1(J, Row, Val),
        Array[I, J] #= Val
    ).

/**
 * This predicate transforms the input matrix to a binary representation array of arrays.
 * param: Matrix = the given sudoku matrix.
 * output: BinaryBoard = the binary representation of a given number or an array of variables (denoted by "_").
**/
board_to_binary_array(Matrix,BinaryBoard) :-
	length(Matrix, N),
	dim(BinaryBoard, [N,N]),
	( multifor([I,J], 1, N), param(Matrix, BinaryBoard,N) do
		nth1(I, Matrix, Row),
		nth1(J, Row, Val),
		(nonvar(Val) -> create_bin_array(Val,N,BinArray),
			array_flat(1,BinArray,Flatten), 
			L is BinaryBoard[I,J],
			array_concat([],Flatten,L) 
			;
			dim(ValArray, [N]), 
			L is BinaryBoard[I,J],
			array_concat([],ValArray,L)
		)
	).
/**
 * This predicate creates binary representation for a given integer.
 * e.g. 1 becomes [1,0,0,0,0,0,0,0,0] for a 9x9 sudoku
 * param: Val = the given number to transform.
 * param: N = the size of the sudoku puzzle.
 * output: BinArray = the binary representation of the given number.
**/
create_bin_array(Val,N,BinArray) :-
	dim(BinArray, [N]),
	( for(I, 1, N), param(Val, BinArray) do
		(Val == I , BinArray[I] #= 1)
		;
		(Val \== I , BinArray[I] #= 0)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOME SEARCH STRATEGIES TAKEN FROM THE COURSE MATERIAL (Lecture Active Constraints slide 57)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(naive, List, Backtracks) :-
    search(List, 0, input_order, indomain, complete, [backtrack(Backtracks)]).

search(middle_out, List, Backtracks) :-
    middle_out(List, MOList),
    search(MOList, 0, input_order, indomain, complete, [backtrack(Backtracks)]).

search(first_fail, List, Backtracks) :-
    search(List, 0, first_fail, indomain, complete, [backtrack(Backtracks)]).

search(moff, List, Backtracks) :-
    middle_out(List, MOList),
    search(MOList, 0, first_fail, indomain, complete, [backtrack(Backtracks)]).

search(moffmo, List, Backtracks) :-
    middle_out(List, MOList),
    search(MOList, 0, first_fail,  indomain_middle, complete, [backtrack(Backtracks)]).