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

normalize_board([cell(Ch, R, C) | RestOfBoard], Initial, NormalizedBoard) :- 
    nth0(R, Initial, Row),
    replace_nth(C, Row, Ch, NewRow),
    replace_nth(R, Initial, NewRow, IntermediaryBoard),
    normalize_board(RestOfBoard, IntermediaryBoard, NormalizedBoard).


initial_board([
    cell(b, 0, 0), cell(b, 0, 2), cell(b, 0, 4), cell(b, 0, 6),
    cell(b, 1, 1), cell(b, 1, 3), cell(b, 1, 5), cell(b, 1, 7),
    cell(b, 2, 0), cell(b, 2, 2), cell(b, 2, 4), cell(b, 2, 6),
    cell(w, 5, 1), cell(w, 5, 3), cell(w, 5, 5), cell(w, 5, 7),
    cell(w, 6, 0), cell(w, 6, 2), cell(w, 6, 4), cell(w, 6, 6),
    cell(w, 7, 1), cell(w, 7, 3), cell(w, 7, 5), cell(w, 7, 7)
]).

board2([
    cell(b, 0, 0), cell(b, 0, 2), cell(b, 0, 4), cell(b, 0, 6),
    cell(b, 1, 1), cell(b, 1, 3), cell(b, 1, 5), cell(b, 1, 7),
    cell(b, 2, 0), cell(b, 2, 2), cell(b, 2, 4), cell(b, 2, 6),
    cell(w, 5, 1), cell(w, 5, 3), cell(w, 5, 5), cell(w, 5, 7),
    cell(w, 6, 0), cell(w, 6, 2), cell(w, 6, 4), cell(w, 6, 6)
]).

board_w_wins([
    cell(w, 5, 1), cell(w, 5, 3), cell(w, 5, 5), cell(w, 5, 7),
    cell(w, 6, 0), cell(w, 6, 2), cell(w, 6, 4), cell(w, 6, 6)
]).

board_eat([
    cell(w, 2, 5), cell(b, 1, 4)
]).

board_eat2([
    cell(b, 2, 5), cell(w, 1, 4)
]).

board_cannot_eat([
    cell(w, 1, 4), cell(b, 0, 3)
]).

board_cannot_eat2([
    cell(w, 2, 5), cell(w, 1, 4)
]).

board_q_eat([
    cell(wq, 7, 1), cell(b, 4, 4)
]).

board_eating([
    cell(wq, 7, 1), cell(w, 2, 2), cell(w, 2, 6), cell(w, 4, 0),
    cell(b, 3, 1), cell(b, 4, 4), cell(b, 3, 7)
]).

board_q_move([
    cell(wq, 7, 1), cell(w, 2, 2), cell(w, 2, 6), cell(w, 4, 0),
    cell(wq, 4, 0),
    cell(b, 3, 1), cell(b, 4, 4), cell(b, 3, 7)
]).