:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(fd_parser).
:- use_module(fd).

:- http_handler(root(.),      list_modules, []).
% :- http_handler(root(mmodule), list_module, []).

% http://localhost:5000/?r=abcdef&f=ab-%3Ec,%20cd-%3Ee,%20f-%3Ea

start :-
    server(5000).

stop :-
    http_stop_server(5000, []).

restart :-
    stop, [ws], start.

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

test(R, F, N) :-
        parse_fds(F, FD),
        FD = N.       
        %nf(R, FD, N).

list_modules(Request) :-
	http_parameters(Request,
          [
            r(R, []),
            f(F, [])
          ]),
        parse_fds(F, FD),
        nf(R, FD, N),
        reply_html_page(title('Functional Dependency Calculator'),
          [
            %h1('FD calculator'),
            %'R(', R, '), F={', F, '}',
            %div('normal form:', N)
            N
          ]).



