:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(fd_parser).
:- use_module(fd).

:- http_handler(root(nf), httpNF, []).
:- http_handler(root(fmin), httpFMin, []).

% http://localhost:5000/?r=abcdef&f=ab-%3Ec,%20cd-%3Ee,%20f-%3Ea

start :-
    server(5000).

stop :-
    http_stop_server(5000, []).

restart :-
    stop, [ws], start.

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

reply(Text) :-
        format('Content-type: text/plain~n~n'),
        format(Text).

httpNF(Request) :-
	http_parameters(Request,
          [
            r(R, []),
            f(F, [])
          ]),
        parse_fds(F, FD),
        nf(R, FD, N),
        reply(N).

httpFMin(Request) :-
	http_parameters(Request,
          [
            f(F, [])
          ]),
        parse_fds(F, FD),
        fmin(FD, FMin),
        fds_to_string(FMin, FMinS),
        reply(FMinS).

