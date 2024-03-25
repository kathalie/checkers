:- module(mechanics, [
    possible_actions/3,
    game_over/1,
    wins/2,
    player/2,
    opponent/2
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


% Define what type of checker is located in the cell(Ch, R, C).
% checker(+Board, ?R, ?C, ?Ch)
checker([cell(Ch, R, C) | _], R, C, Ch).
checker([_ | RestOfBoard], R, C, Ch) :-
    checker(RestOfBoard, R, C, Ch).


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
    \+ member(cell(w, _, _), Board),
    \+ member(cell(wq, _, _), Board).

% wins(+Board, ?Ch).
wins(Board, white) :-
    \+ member(cell(b, _, _), Board),
    \+ member(cell(bq, _, _), Board).


game_over(Board) :- wins(Board, _).


% Move is possible and the board is updated
% put(+Cell, +RNew, +CNew, +Board, -NewBoard).
put(cell(Ch, R, C), RNew, CNew, Board, NewBoard) :-
    is_valid(RNew), is_valid(CNew),
    \+ checker(Board, RNew, CNew, _),
    promote(RNew, Ch, ChNew),
    replace(cell(Ch, R, C), Board, cell(ChNew, RNew, CNew), NewBoard), !.


% Checks if checkers in cells (R1, C1) and (R2, C2) respectively have a corner in common.
% are_neighbours(+R1, +C1, +R2, +C2).
are_neighbours(R1, C1, R2, C2) :-
    RDiff = R1 - R2, 
    abs(RDiff) =:= 1,
    CDiff = C1 - C2,
    abs(CDiff) =:= 1, !.


% Checks if checkers in cells (R1, C1) and (R2, C2) respectively are on one diagonal.
% are_in_diagonal(+R1, +C1, +R2, +C2).
are_in_diagonal(R1, C1, R2, C2) :-
    vector(R1, R2, C1, C2, X, Y),
    is_diagonal_vector(X, Y).


% Checks if Ch can eat ChToEat.
% eatable(+Ch, +ChToEat, +Board).
eatable(cell(ChW, RW, CW), cell(ChB, RB, CB)) :-
    opponent(ChW, ChB),
    \+ queen(ChW),
    are_neighbours(RW, CW, RB, CB).

% eatable(+Ch, +ChToEat, +Board).
eatable(cell(ChW, RW, CW), cell(ChB, RB, CB)) :-
    opponent(ChW, ChB),
    queen(ChW),
    are_in_diagonal(RW, CW, RB, CB).


% Checker Ch eats checker ChToEat if possible and new board is generated. 
% eat(+Ch, +ChToEat, +Board, -NewBoard).
eat(cell(ChW, RW, CW), cell(ChB, RB, CB), Board, NewBoard) :-
    eatable(cell(ChW, RW, CW), cell(ChB, RB, CB)),
    % Define direction to move the checker.
    vector(RW, RB, CW, CB, RVector, CVector),
    RDirection is sign(RVector), 
    CDirection is sign(CVector),
    RNew is RW + RDirection * (abs(RVector) + 1),
    CNew is CW + CDirection * (abs(CVector) + 1),
    % Move the checker to a new position.
    put(cell(ChW, RW, CW), RNew, CNew, Board, IntermediaryBoard),
    % Eat the checker and remove it from board.
    select(cell(ChB, RB, CB), IntermediaryBoard, NewBoard), 
    !. 


% Generates all possible moves for a checker in the Cell.
% w moves from 7 to 0.
% b moves from 0 to 7.
% move_forward(+Cell, +Board, -NewBoards).
move_forward(cell(Ch, R, C), Board, NewBoards) :-
    \+ queen(Ch),
    (
        (player(white, Ch), RNew is R - 1);
        (player(black, Ch), RNew is R + 1)
    ),
    (
        CNew is C + 1;
        CNew is C - 1
    ), 
    put(cell(Ch, R, C), RNew, CNew, Board, NewBoards).

move_forward(cell(Ch, R, C), Board, NewBoards) :-
    queen(Ch),
    (
        move_in_direction(1, 1, 1, cell(Ch, R, C), Board, NewBoards);
        move_in_direction(-1, 1, 1, cell(Ch, R, C), Board, NewBoards);
        move_in_direction(1, -1, 1, cell(Ch, R, C), Board, NewBoards);
        move_in_direction(-1, -1, 1, cell(Ch, R, C), Board, NewBoards)
    ).

move_in_direction(X, Y, Dist, cell(Ch, R, C), Board, NewBoards) :-
    (
        % If there is space for a queen to move further, move.
        (
            RAfterNew is R + X * (Dist + 1),
            CAfterNew is C + Y * (Dist + 1),
            put(cell(Ch, R, C), RAfterNew, CAfterNew, Board, _),
            NewDist is Dist + 1,
            move_in_direction(X, Y, NewDist, cell(Ch, R, C), Board, NewBoards)
        );
        % Otherwise, put it on the new position if possible.
        (
            RNew is R + X * Dist,
            CNew is C + Y * Dist,
            put(cell(Ch, R, C), RNew, CNew, Board, NewBoards)
        )
    ).


% If Player (white or black) has to eat, a checker is eaten 
% and all such updated boards are generated.
% must_eat(+Player, +Board, -NewBoards).
must_eat(Player, Board, NewBoards) :-
    player(Player, ChW),
    checker(Board, RW, CW, ChW),
    checker(Board, R, C, Ch),
    eat(cell(ChW, RW, CW), cell(Ch, R, C), Board, NewBoards).

% Finds all the possible forward moves without eating checkers
% for Player (white, black).
% possible_forward_moves(+Player, +Board, -NewBoards).
possible_forward_moves(Player, Board, NewBoards) :-
    player(Player, Ch),
    checker(Board, R, C, Ch),
    move_forward(cell(Ch, R, C), Board, NewBoards).

% Finds all the possible actions moves for Player (white, black).
% If Player can eat opponent's checker, it becomes obligatory and all the variants 
% of eating opponent's checkers are generated.
% Otherwise, possible moves for simple checkers are generated.
% possible_forward_moves(+Player, +Board, -NewBoards).
possible_actions(Player, Board, NewBoards) :-
    \+ game_over(Board),
    (
        must_eat(Player, Board, NewBoards);
        (
            \+ must_eat(Player, Board, _),
            possible_forward_moves(Player, Board, NewBoards)
        )
    ).
