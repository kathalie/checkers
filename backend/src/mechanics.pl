:- module(mechanics, [
    possible_actions/3,
    game_over/1,
    wins/2,
    player/2,
    opponent/2,
    checker/5
]).

:- use_module(boards).
:- use_module(utils).
:- use_module(evaluation).


% Checks is column or row exists on a board.
% is_valid(+CR).
is_valid(CR) :-
    CR >= 0, CR =<7, !.

% Define two types of players and their possible pieces in the game.
player(black, b).
player(black, bq).
player(white, w).
player(white, wq).

% Define which pieces are queens.
queen(wq).
queen(bq).

% A cheker becomes a queen when reaches the opposite edge of the board. Otherwise it is a still a simple checker.
% promote(+R, +Ch, -CheckerOrQueen).
promote(0, w, wq) :- !.
promote(7, b, bq) :- !.
promote(_, Ch, Ch) :- !.


% Define what type of checker is located in the cell(K, Ch, R, C).
% checker(+Board, ?R, ?C, ?Ch)
checker([cell(K, Ch, R, C) | _], K, R, C, Ch).
checker([_ | RestOfBoard], K, R, C, Ch) :-
    checker(RestOfBoard, K, R, C, Ch).


% Checks if Ch1 is opponent of Ch2.
% opponent(?Ch1, ?Ch2)
opponent(Ch1, Ch2) :-
    (player(black, Ch1), player(white, Ch2));
    (player(white, Ch1), player(black, Ch2)).

% opponent(?Player1, ?Player2)
opponent(white, black).
opponent(black, white).

% Checks if there is a winner.
% wins(+Board, ?Ch).
wins(Board, black) :-
    \+ member(cell(_, w, _, _), Board),
    \+ member(cell(_, wq, _, _), Board).

% wins(+Board, ?Ch).
wins(Board, white) :-
    \+ member(cell(_, b, _, _), Board),
    \+ member(cell(_, bq, _, _), Board).


game_over(Board) :- wins(Board, _).


% Move is possible and the board is updated
% put(+Cell, +RNew, +CNew, +Board, -NewBoard).
put(Cell, RNew, CNew, Board, NewBoard) :-
    Cell = cell(K, Ch, _, _),
    is_valid(RNew), is_valid(CNew),
    \+ checker(Board, _, RNew, CNew, _), % cell with coords (RNew, CNew) is not occupied.
    promote(RNew, Ch, ChNew), % checker on teh new position is promoted to queen if possible.
    replace(Cell, Board, cell(K, ChNew, RNew, CNew), NewBoard), 
    !.


% Checks if checkers in cells (R1, C1) and (R2, C2) respectively have a corner in common.
% are_neighbours(+R1, +C1, +R2, +C2).
are_neighbours(R1, C1, R2, C2) :-
    RDiff = R1 - R2, 
    abs(RDiff) =:= 1,
    CDiff = C1 - C2,
    abs(CDiff) =:= 1, 
    !.


% Checks if checkers in cells (R1, C1) and (R2, C2) respectively are on one diagonal.
% are_in_diagonal(+R1, +C1, +R2, +C2).
are_in_diagonal(R1, C1, R2, C2) :-
    vector(R1, R2, C1, C2, X, Y),
    is_diagonal_vector(X, Y),
    !.


% Checks if Ch can eat ChToEat.
% eatable(+Ch, +ChToEat, +Board).
eatable(cell(_, ChW, RW, CW), cell(_, ChB, RB, CB)) :-
    opponent(ChW, ChB),
    \+ queen(ChW),
    are_neighbours(RW, CW, RB, CB).

% eatable(+Ch, +ChToEat, +Board).
eatable(cell(_, ChW, RW, CW), cell(_, ChB, RB, CB)) :-
    opponent(ChW, ChB),
    queen(ChW),
    are_in_diagonal(RW, CW, RB, CB).


% Checker Ch eats checker ChToEat if possible and new board is generated. 
% eat(+Ch, +ChToEat, +Board, -NewBoard).
eat(Cell, CellToEat, Board, NewBoard) :-
    Cell = cell(_, _, RW, CW),
    CellToEat = cell(_, _, RB, CB),
    eatable(Cell, CellToEat),
    % Define direction to move the checker.
    vector(RW, RB, CW, CB, RVector, CVector),
    RDirection is sign(RVector), 
    CDirection is sign(CVector),
    RNew is RW + RDirection * (abs(RVector) + 1),
    CNew is CW + CDirection * (abs(CVector) + 1),
    % Move the checker to a new position.
    put(Cell, RNew, CNew, Board, IntermediaryBoard),
    % Eat the checker and remove it from board.
    select(CellToEat, IntermediaryBoard, NewBoard), 
    !. 


% Generates all possible moves for a checker in the Cell.
% w moves from 7 to 0.
% b moves from 0 to 7.
% move_forward(+Cell, +Board, -NewBoards).
move_forward(Cell, Board, NewBoards) :-
    Cell = cell(_, Ch, R, C),
    \+ queen(Ch),
    (
        (player(white, Ch), RNew is R - 1);
        (player(black, Ch), RNew is R + 1)
    ),
    (
        CNew is C + 1;
        CNew is C - 1
    ), 
    put(Cell, RNew, CNew, Board, NewBoards).

move_forward(Cell, Board, NewBoards) :-
    Cell = cell(_, Ch, _, _),
    queen(Ch),
    (
        move_in_direction(1, 1, 1, Cell, Board, NewBoards);
        move_in_direction(-1, 1, 1, Cell, Board, NewBoards);
        move_in_direction(1, -1, 1, Cell, Board, NewBoards);
        move_in_direction(-1, -1, 1, Cell, Board, NewBoards)
    ).

move_in_direction(X, Y, Dist, Cell, Board, NewBoards) :-
    Cell = cell(_, Ch, R, C),
    queen(Ch),
    (
        % If there is space for a queen to move further, move.
        (
            RAfterNew is R + X * (Dist + 1),
            CAfterNew is C + Y * (Dist + 1),
            put(Cell, RAfterNew, CAfterNew, Board, _),
            NewDist is Dist + 1,
            move_in_direction(X, Y, NewDist, Cell, Board, NewBoards)
        );
        % Otherwise, put it on the new position if possible.
        (
            RNew is R + X * Dist,
            CNew is C + Y * Dist,
            put(Cell, RNew, CNew, Board, NewBoards)
        )
    ).


% If Player (white or black) has to eat, a checker is eaten 
% and all such updated boards are generated.
% must_eat(+Player, +Board, -NewBoards).
must_eat(Player, Board, NewBoards) :-
    player(Player, ChW),
    checker(Board, KW, RW, CW, ChW),
    checker(Board, K, R, C, Ch),
    eat(cell(KW, ChW, RW, CW), cell(K, Ch, R, C), Board, NewBoards).

% Finds all the possible forward moves without eating checkers
% for Player (white, black).
% possible_forward_moves(+Player, +Board, -NewBoards).
possible_forward_moves(Player, Board, NewBoards) :-
    player(Player, Ch),
    checker(Board, K, R, C, Ch),
    move_forward(cell(K, Ch, R, C), Board, NewBoards).

% Finds all the possible actions moves for Player (white, black).
% If Player can eat opponent's checker, it becomes obligatory and all the variants 
% of eating opponent's checkers are generated.
% Otherwise, possible moves for simple checkers are generated.
% possible_forward_moves(+Player, +Board, -NewBoards).
possible_actions(Player, Board, NewBoards) :-
    \+ game_over(Board),
    must_eat(Player, Board, NewBoards).

possible_actions(Player, Board, NewBoards) :-
    \+ game_over(Board),
    \+ must_eat(Player, Board, _),
    possible_forward_moves(Player, Board, NewBoards).


/** <examples>
?- boards:board_eating(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
possible_actions(black, Board, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl.


?- boards:board_q_move(Board), 
write("Board:"), nl, boards:print_board(Board), nl,
possible_actions(white, Board, NewBoard), 
write("New Board: "), nl, boards:print_board(NewBoard), nl.

*/