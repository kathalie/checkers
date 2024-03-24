:- module(utils, [replace/4, vector/6, is_diagonal_vector/2]).

% replace(+Element, +List, +NewElement, -NewList)
replace(Element, List, NewElement, NewList) :-
    append(Start, [Element | End], List),
    append(Start, [NewElement | End], NewList), !.

% vector(+XStart, +XEnd, +YStart, +YEnd, -X, -Y).
vector(XStart, XEnd, YStart, YEnd, X, Y) :-
    X is XEnd - XStart,
    Y is YEnd - YStart, !.

% X, Y - vector coordinates.
% is_diagonal_vector(+X, +Y)
is_diagonal_vector(X, Y) :-
    abs(X) =:= abs(Y).