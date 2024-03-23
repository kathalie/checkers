% Define initial board configuration
initial_board([
    cell(enemy, 1, 1), cell(enemy, 1, 3), cell(enemy, 1, 5), cell(enemy, 1, 7),
    cell(enemy, 2, 2), cell(enemy, 2, 4), cell(enemy, 2, 6), cell(enemy, 2, 8),
    cell(enemy, 3, 1), cell(enemy, 3, 3), cell(enemy, 3, 5), cell(enemy, 3, 7),
    cell(curr_player, 6, 2), cell(curr_player, 6, 4), cell(curr_player, 6, 6), cell(curr_player, 6, 8),
    cell(curr_player, 7, 1), cell(curr_player, 7, 3), cell(curr_player, 7, 5), cell(curr_player, 7, 7),
    cell(curr_player, 8, 2), cell(curr_player, 8, 4), cell(curr_player, 8, 6), cell(curr_player, 8, 8)
]).

board2([
    cell(enemy, 1, 1), cell(enemy, 1, 3), cell(enemy, 1, 5), cell(enemy, 1, 7),
    cell(enemy, 2, 2), cell(enemy, 2, 4), cell(enemy, 2, 6), cell(enemy, 2, 8),
    cell(enemy, 3, 1), cell(enemy, 3, 3), cell(enemy, 3, 5), cell(enemy, 3, 7),
    cell(curr_player, 6, 2), cell(curr_player, 6, 4), cell(curr_player, 6, 6), cell(curr_player, 6, 8),
    cell(curr_player, 7, 1), cell(curr_player, 7, 3), cell(curr_player, 7, 5), cell(curr_player, 7, 7)
]).

board_curr_wins([
    cell(curr_player, 6, 2), cell(curr_player, 6, 4), cell(curr_player, 6, 6), cell(curr_player, 6, 8),
    cell(curr_player, 7, 1), cell(curr_player, 7, 3), cell(curr_player, 7, 5), cell(curr_player, 7, 7)
]).

board_eat([
    cell(curr_player, 3, 6), cell(enemy, 2, 5)
]).

board_eat2([
    cell(enemy, 3, 6), cell(curr_player, 2, 5)
]).

board_cannot_eat([
    cell(curr_player, 2, 5), cell(enemy, 1, 4)
]).

board_cannot_eat2([
    cell(curr_player, 3, 6), cell(curr_player, 2, 5)
]).

material_value(enemy, 1).
material_value(curr_player, -1).
material_value(enemy_king, 2).
material_value(curr_player_king, -2).

% % positional_value(+Checker, +Row, -Value)
% positional_value(Checker, Row, Value) :-
%     Coef = 

wins(Board, enemy) :-
    \+ member(cell(curr_player, _, _), Board),
    \+ member(cell(curr_player_king, _, _), Board).
    
wins(Board, curr_player) :-
    \+ member(cell(enemy, _, _), Board),
    \+ member(cell(enemy_king, _, _), Board).

opponent(enemy, curr_player).
opponent(curr_player, enemy).

% Cheker becomes a king when reaches the opposite row of the board. Otherwise it is a still a checker.
% promote(+Row, +Checker, -CheckerOrKing).
promote(1, curr_player, curr_player_king) :- !.
promote(8, enemy, enemy_king) :- !.
promote(_, Checker, Checker) :- !.


% evaluate_board(+Board, -Score)
evaluate_board(Board, -1000) :-
    wins(Board, enemy).

evaluate_board(Board, 1000) :-
    wins(Board, curr_player).

evaluate_board(Board, Score) :-
    evaluate_board(Board, 0, Score).

evaluate_board([], Score, Score).

evaluate_board([cell(Checker, Row, Col) | RestOfBoard], CurrentScore, Score) :-
    material_value(Checker, AddToScore),
    NewScore is CurrentScore + AddToScore,
    evaluate_board(RestOfBoard, NewScore, Score).


% Define what type of checker is located in the cell (Row, Col).
% checker(+Board, +Row, +Col, -Checker)
checker([cell(Checker, Row, Col) | _], Row, Col, Checker).
checker([_ | RestOfBoard], Row, Col, Checker) :-
    checker(RestOfBoard, Row, Col, Checker).


% are_in_diagonal(+Row1, +Col1, +Row2, +Col2)
are_in_diagonal(Row1, Col1, Row2, Col2) :-
    RowDiff = Row1 - Row2, 
    abs(RowDiff) =:= 1,
    ColDiff = Col1 - Col2,
    abs(ColDiff) =:= 1, !.


% replace(+Element, +List, +NewElement, -NewList)
replace(Element, List, NewElement, NewList) :-
    append(Start, [Element | End], List),
    append(Start, [NewElement | End], NewList), !.


% move(+Cell, +NewRow, +NewCol, +Board, -NewBoard) 
% Move is impossible and the board is the same
move(_, NewRow, NewCol, Board, Board) :-
    (
        \+ is_valid(NewRow); 
        \+ is_valid(NewCol);
        checker(Board, NewRow, NewCol, _) % the cell is occupied!!!
    ), !.

% Move is possible and the board is updated
move(cell(Checker, Row, Col), NewRow, NewCol, Board, NewBoard) :-
    is_valid(NewRow), is_valid(NewCol),
    promote(NewRow, Checker, NewChecker),
    replace(cell(Checker, Row, Col), Board, cell(NewChecker, NewRow, NewCol), NewBoard), !.
    

% is_valid(+ColOrRow).
% Checks is column or row exists on a board.
is_valid(ColOrRow) :-
    ColOrRow > 0, ColOrRow < 9, !.


% enemy goes from 1 to 8
% curr_player goes from 8 to 1
% eat_checker(+Checker, +CheckerToEat, +Board, -NewBoard)
eat_checker(cell(CheckerCurr, RowCurr, ColCurr), cell(CheckerEnemy, RowEnemy, ColEnemy), Board, NewBoard) :-
    are_in_diagonal(RowCurr, ColCurr, RowEnemy, ColEnemy),
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


% eat_checker(_, _, Board, Board) :- !.






alphabeta(Board, 0, _, _, Score) :-
    evaluate_board(Board, Score).

%alphabeta(Board, Depth, Alpha, Beta, Score) :- 




% make_a_move(Board, NewBoards) :-


