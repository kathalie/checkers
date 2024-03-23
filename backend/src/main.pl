% test_boards.pl
:- use_module(boards).
:- use_module(evaluation).
:- use_module(utils).

% Define two types of players and their possible pieces in the game.
player(black, b).
player(black, bq).
player(white, w).
player(white, wq).

% Define which pieces are queens.
queen(wq).
queen(bq).

% Checks if there is a winner.
% wins(+Board, ?Ch).
wins(Board, b) :-
    \+ member(cell(w, _, _), Board),
    \+ member(cell(wq, _, _), Board).
    
wins(Board, w) :-
    \+ member(cell(b, _, _), Board),
    \+ member(cell(bq, _, _), Board).

% Checks if Ch1 is opponent of Ch2.
% opponent(?Ch1, ?Ch2)
opponent(Ch1, Ch2) :-
    (player(black, Ch1), player(white, Ch2));
    (player(white, Ch1), player(black, Ch1)).

% A cheker becomes a queen when reaches the opposite edge of the board. Otherwise it is a still a simple checker.
% promote(+R, +Ch, -CheckerOrQueen).
promote(1, w, wq) :- !.
promote(8, b, bq) :- !.
promote(_, Ch, Ch) :- !.

% Define what type of checker is located in the cell(Ch, R, C).
% checker(+Board, +R, +C, -Ch)
checker([cell(Ch, R, C) | _], R, C, Ch).
checker([_ | RestOfBoard], R, C, Ch) :-
    checker(RestOfBoard, R, C, Ch).

% Checks if checkers in cells (R1, C1) and (R2, C2) respectively have a corner in common.
% are_neighbours(+R1, +C1, +R2, +C2)
are_neighbours(R1, C1, R2, C2) :-
    RDiff = R1 - R2, 
    abs(RDiff) =:= 1,
    CDiff = C1 - C2,
    abs(CDiff) =:= 1, !.


% move(+Cell, +NewRow, +NewCol, +Board, -NewBoard) 
% Move is impossible and the board is the same
move(_, NewRow, NewCol, Board, Board) :-
    (
        \+ is_valid(NewRow); 
        \+ is_valid(NewCol);
        checker(Board, NewRow, NewCol, _) % the cell is occupied!!!
    ), !.

% Move is possible and the board is updated
move(cell(Ch, R, C), NewRow, NewCol, Board, NewBoard) :-
    is_valid(NewRow), is_valid(NewCol),
    promote(NewRow, Ch, NewChecker),
    replace(cell(Ch, R, C), Board, cell(NewChecker, NewRow, NewCol), NewBoard), !.
    

% is_valid(+ColOrRow).
% Checks is column or row exists on a board.
is_valid(ColOrRow) :-
    ColOrRow >= 0, ColOrRow =<7, !.



% eat(+Ch, +CheckerToEat, +Board, -NewBoard)
eat(cell(CheckerCurr, RowCurr, ColCurr), cell(CheckerEnemy, RowEnemy, ColEnemy), Board, NewBoard) :-
    are_neighbours(RowCurr, ColCurr, RowEnemy, ColEnemy),
    % checker(Board, RowCurr, ColCurr, CheckerCurr),
    % checker(Board, RowEnemy, ColEnemy, CheckerEnemy),
    opponent(CheckerCurr, CheckerEnemy),
    % Define direction to move the checker.
    HVector is RowEnemy - RowCurr,
    VVector is ColEnemy - ColCurr,
    NewRow is RowCurr + 2 * HVector, is_valid(NewRow),
    NewCol is ColCurr + 2 * VVector, is_valid(NewCol),
    % Move the checker to a new position.
    move(cell(CheckerCurr, RowCurr, ColCurr), NewRow, NewCol, Board, IntermediaryBoard),
    % Eat the checker and remove it from board.
    select(cell(CheckerEnemy, RowEnemy, ColEnemy), IntermediaryBoard, NewBoard), 
    !. 

% eat(_, _, Board, Board) :- !.


% can_be_eaten(Board, )

% b moves from 1 to 8
% w moves from 8 to 1