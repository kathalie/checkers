:- module(utils, [replace/4]).

% replace(+Element, +List, +NewElement, -NewList)
replace(Element, List, NewElement, NewList) :-
    append(Start, [Element | End], List),
    append(Start, [NewElement | End], NewList), !.