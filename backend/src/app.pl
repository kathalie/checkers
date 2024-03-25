:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(http/json_convert)).

:- use_module(main).

parse_cell(_{key: K, type: Ch, row: R, col: C}, cell(K, ChAtom, R, C)) :- 
    atom_string(ChAtom, Ch).

% parse_depth_and_board(+Json, -Depth, -Board).
parse_depth_and_board(_{depth: Depth, board: CellsList}, Depth, Board) :-
    maplist(parse_cell, CellsList, Board).


% Request:
% {
%     depth: 3,
%     board: [
%         {key: 0, type: 'b', row: 0, col: 0}, 
%         ...
%     ]
% }
handle_best_move_request(Request) :-
    http_read_json_dict(Request, Json),
    parse_depth_and_board(Json, Depth, Board),
    best_move(Depth, Board, RFrom, CFrom, RTo, CTo),
    reply_json_dict(json([
        rFrom=RFrom,
        cFrom=CFrom,
        rTo=RTo,
        cTo=CTo
    ])).

handle_default(_Request) :-
    reply_json_dict(200).

server(IP, Port) :-
    write(IP),
    % handlers
    http_handler('/best-move', handle_best_move_request, [methods([post])]),
    http_handler('/', handle_default, []),
    % start server
    http_server(http_dispatch, [port(IP:Port)]).