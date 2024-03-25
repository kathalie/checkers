:- module(alphabeta, [
    alphabeta/4
]).

:- use_module(mechanics).
:- use_module(evaluation).

next_player(white, black).
next_player(black, white).

maximizing(white).
minimizing(black).

% function minimax(position, depth, alpha, beta, maximizingPlayer)
% 	if depth == 0 or game over in position
% 		return static evaluation of position

% minimax(Board, Depth, _, _, _, Eval) :-  
%     (
%         game_over(Board); 
%         Depth =:= 0
%     ),
%     evaluate_board(Board, Eval).

% minimax(Board, Depth, Alpha, Beta, Player, Eval) :- 
%     minimax_helper(Board, NewBoards, Depth, NewDepth, Player, NextPlayer),
%     minimizing(Player),
%     minEval(NewBoards, NewDepth, Alpha, Beta, NextPlayer, 9999, Eval).

% minimax(Board, Depth, Alpha, Beta, Player, Eval) :- 
%     minimax_helper(Board, NewBoards, Depth, NewDepth, Player, NextPlayer),
%     maximizing(Player),
%     maxEval(NewBoards, NewDepth, Alpha, Beta, NextPlayer, -9999, Eval).

% minimax_helper(Board, NewBoards, Depth, NewDepth, Player, NextPlayer) :-
%     Depth > 0,
%     \+ game_over(Board),
%     possible_actions(Player, Board, NewBoards),
%     next_player(Player, NextPlayer),
%     NewDepth is Depth - 1.

% % 	if maximizingPlayer
% % 		maxEval = -infinity
% % 		for each child of position
% % 			eval = minimax(child, depth - 1, alpha, beta false)
% % 			maxEval = max(maxEval, eval)
% % 			alpha = max(alpha, eval)
% % 			if beta <= alpha
% % 				break
% % 		return maxEval
% maxEval([], _, _, _, _, MaxEval, MaxEval).

% maxEval([NewBoard | Rest], Depth, Alpha, Beta, NextPlayer, TempMaxEval, MaxEval) :-
%     minimax(NewBoard, Depth, Alpha, Beta, NextPlayer, Eval),
%     NewMaxEval is max(TempMaxEval, Eval),
%     NewAlpha is max(Alpha, Eval),
%     (
%         Beta =< NewAlpha, % If Beta is less than or equal to NewAlpha
%         MaxEval = NewMaxEval % Set MaxEval to NewMaxEval
%     ;
%         Beta > NewAlpha, % Otherwise
%         maxEval(Rest, Depth, NewAlpha, Beta, NextPlayer, NewMaxEval, MaxEval) % Continue with recursion
%     ).

% % 	else
% % 		minEval = +infinity
% % 		for each child of position
% % 			eval = minimax(child, depth - 1, alpha, beta true)
% % 			minEval = min(minEval, eval)
% % 			beta = min(beta, eval)
% % 			if beta <= alpha
% % 				break
% % 		return minEval
% minEval([], _, _, _, _, MinEval, MinEval).

% minEval([NewBoard | Rest], Depth, Alpha, Beta, NextPlayer, TempMinEval, MinEval) :-
%     minimax(NewBoard, Depth, Alpha, Beta, NextPlayer, Eval),
%     NewMinEval is min(TempMinEval, Eval),
%     NewBeta is min(Beta, Eval),
%     (
%         NewBeta =< Alpha, % If Beta is less than or equal to Alpha
%         MinEval = NewMinEval % Set MinEval to NewMinEval
%     ;
%         NewBeta > Alpha, % Otherwise
%         minEval(Rest, Depth, Alpha, NewBeta, NextPlayer, NewMinEval, MinEval) % Continue with recursion
%     ).



% best_action(Depth, Board, BestBoard) :-
%     findall(
%         NewBoard-Eval, 
%         (
%             minimax(Board, Depth, -1000, 1000, white, Eval), 
%             NewBoard
%         ), 
%         Boards
%     ),
%     max_member(BestBoard-Eval, Boards).


alphabeta(Player, Board, Depth, BestBoard) :- 
    alphabeta(Player, Board, Depth, -1000, 1000, BestBoard, _).

alphabeta(Player, Board, Depth, Alpha, Beta, GoodBoard, Eval) :-
    Depth > 0, \+game_over(Board),
    possible_actions(Player, Board, NewBoards), !,
    boundedbest(Player, NewBoards, Depth, Alpha, Beta, GoodBoard, Eval);
    evaluate_board(Board, Eval).        % Static value of Board

boundedbest(Player, [Board|OtherBoards], Depth, Alpha, Beta, GoodBoard, GoodEval) :-
    NewDepth is Depth - 1,
    alphabeta(Player, Board, NewDepth, Alpha, Beta, _, Eval),
    goodenough(Player, OtherBoards, Depth, Alpha, Beta, Board, Eval, GoodBoard, GoodEval).

goodenough(_, [], _, _, _, Board, Eval, Board, Eval) :- !.     % No other candidate

goodenough(Player, _, _, Alpha, Beta, Board, Eval, Board, Eval) :-
    minimizing(Player), Eval > Beta, !;       % Maximizer attained upper bound
    maximizing(Player), Eval < Alpha, !.      % Minimizer attained lower bound

goodenough(Player, NewBoards, Depth, Alpha, Beta, Board, Eval, GoodBoard, GoodEval) :-
    newbounds(Player, Alpha, Beta, Eval, NewAlpha, NewBeta),        % Refine bounds
    boundedbest(Player, NewBoards, Depth, NewAlpha, NewBeta, GoodBoard, GoodEval),
    betterof(Player, Board, Eval, GoodBoard, GoodEval, GoodBoard, GoodEval).

newbounds(Player, Alpha, Beta, Eval, Eval, Beta) :-
    minimizing(Player), Eval > Alpha, !.        % Maximizer increased lower bound

newbounds(Player, Alpha, Beta, Eval, Alpha, Eval) :-
    maximizing(Player), Eval < Beta, !.         % Minimizer decreased upper bound

newbounds(_, Alpha, Beta, _, Alpha, Beta).          % Otherwise bounds unchanged

betterof(Player, Board, Eval, _, GoodEval, Board, Eval) :-         % Board better then GoodBoard
    minimizing(Player), Eval > GoodEval, !;
    maximizing(Player), Eval < GoodEval, !.

betterof(_, _, _, GoodBoard, GoodEval, GoodBoard, GoodEval).             % Otherwise GoodBoard better



%TODO minimax!!!!