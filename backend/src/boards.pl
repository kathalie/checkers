:- module(boards, [
        initial_board/1, 
        board2/1, 
        board_w_wins/1, 
        board_eat/1, 
        board_eat2/1, 
        board_cannot_eat/1, 
        board_cannot_eat2/1,
        board_q_eat/1,
        board_eating/1,
        board_q_move/1,
        print_board/1
    ]).

:- use_module(utils).


print_board(Board) :-
    normalize_board(Board, NormalizedBoard),
    print_normalized_board(NormalizedBoard).

print_normalized_board([]).
print_normalized_board([Row | RestOfRows]) :-
    print_row(Row), nl,
    print_normalized_board(RestOfRows).

print_row([]).
print_row([Cell | RestOfCells]) :-
    write(Cell), write(' '),
    print_row(RestOfCells).


normalize_board(Board, NormalizedBoard) :-
    Initial = [    
        ['-', ' ', '-', ' ', '-', ' ', '-', ' '],
        [' ', '-', ' ', '-', ' ', '-', ' ', '-'],
        ['-', ' ', '-', ' ', '-', ' ', '-', ' '],
        [' ', '-', ' ', '-', ' ', '-', ' ', '-'],
        ['-', ' ', '-', ' ', '-', ' ', '-', ' '],
        [' ', '-', ' ', '-', ' ', '-', ' ', '-'],
        ['-', ' ', '-', ' ', '-', ' ', '-', ' '],
        [' ', '-', ' ', '-', ' ', '-', ' ', '-']
    ],
    normalize_board(Board, Initial, NormalizedBoard).


normalize_board([], NormalizedBoard, NormalizedBoard).

normalize_board([cell(_, Ch, R, C) | RestOfBoard], Initial, NormalizedBoard) :- 
    nth0(R, Initial, Row),
    replace_nth(C, Row, Ch, NewRow),
    replace_nth(R, Initial, NewRow, IntermediaryBoard),
    normalize_board(RestOfBoard, IntermediaryBoard, NormalizedBoard).


initial_board([
    cell(1, b, 0, 0), cell(2, b, 0, 2), cell(3, b, 0, 4), cell(4, b, 0, 6),
    cell(5, b, 1, 1), cell(6, b, 1, 3), cell(7, b, 1, 5), cell(8, b, 1, 7),
    cell(9, b, 2, 0), cell(10, b, 2, 2), cell(11, b, 2, 4), cell(12, b, 2, 6),
    cell(13, w, 5, 1), cell(14, w, 5, 3), cell(15, w, 5, 5), cell(16, w, 5, 7),
    cell(17, w, 6, 0), cell(18, w, 6, 2), cell(19, w, 6, 4), cell(20, w, 6, 6),
    cell(21, w, 7, 1), cell(22, w, 7, 3), cell(23, w, 7, 5), cell(24, w, 7, 7)
]).

board2([
    cell(1, b, 0, 0), cell(2, b, 0, 2), cell(3, b, 0, 4), cell(4, b, 0, 6),
    cell(5, b, 1, 1), cell(6, b, 1, 3), cell(7, b, 1, 5), cell(8, b, 1, 7),
    cell(9, b, 2, 0), cell(10, b, 2, 2), cell(11, b, 2, 4), cell(12, b, 2, 6),
    cell(13, w, 5, 1), cell(14, w, 5, 3), cell(15, w, 5, 5), cell(16, w, 5, 7),
    cell(17, w, 6, 0), cell(18, w, 6, 2), cell(19, w, 6, 4), cell(20, w, 6, 6)
]).

board_w_wins([
    cell(13, w, 5, 1), cell(14, w, 5, 3), cell(15, w, 5, 5), cell(16, w, 5, 7),
    cell(17, w, 6, 0), cell(18, w, 6, 2), cell(19, w, 6, 4), cell(20, w, 6, 6)
]).

board_eat([
    cell(21, w, 2, 5), cell(22, b, 1, 4)
]).

board_eat2([
    cell(21, b, 2, 5), cell(22, w, 1, 4)
]).

board_cannot_eat([
    cell(21, w, 1, 4), cell(22, b, 0, 3)
]).

board_cannot_eat2([
    cell(21, w, 2, 5), cell(22, w, 1, 4)
]).

board_q_eat([
    cell(21, wq, 7, 1), cell(22, b, 4, 4)
]).

board_eating([
    cell(21, wq, 7, 1), cell(22, w, 2, 2), cell(23, w, 2, 6), cell(24, w, 4, 0),
    cell(25, b, 3, 1), cell(26, b, 4, 4), cell(27, b, 3, 7)
]).

board_q_move([
    cell(21, w, 7, 1), cell(22, w, 2, 2), cell(23, w, 2, 6),
    cell(25, wq, 4, 0),
    cell(26, b, 3, 1), cell(27, b, 4, 4), cell(28, b, 3, 7)
]).