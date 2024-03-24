:- module(utils, [
        replace/4,
        replace_nth/4, 
        vector/6,
        is_diagonal_vector/2
    ]).

% replace(+Element, +List, +NewElement, -NewList)
replace(Element, List, NewElement, NewList) :-
    append(Start, [Element | End], List),
    append(Start, [NewElement | End], NewList), !.

% replace_nth(+Index, +List, +NewElement, -NewList).
replace_nth(Index, List, NewElement, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewElement, Rest).

% vector(+XStart, +XEnd, +YStart, +YEnd, -X, -Y).
vector(XStart, XEnd, YStart, YEnd, X, Y) :-
    X is XEnd - XStart,
    Y is YEnd - YStart, !.

% X, Y - vector coordinates.
% is_diagonal_vector(+X, +Y)
is_diagonal_vector(X, Y) :-
    abs(X) =:= abs(Y).