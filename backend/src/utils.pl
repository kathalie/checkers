:- module(utils, [replace/4, vector/6]).

% replace(+Element, +List, +NewElement, -NewList)
replace(Element, List, NewElement, NewList) :-
    append(Start, [Element | End], List),
    append(Start, [NewElement | End], NewList), !.

% vector(+XStart, +XEnd, +YStart, +YEnd, -X, -Y).
vector(XStart, XEnd, YStart, YEnd, X, Y) :-
    X is XEnd - XStart,
    Y is YEnd - YStart, !.