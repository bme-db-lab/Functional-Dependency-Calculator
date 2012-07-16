:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(fd_parser).
:- use_module(fd).

:- http_handler(root(.), http_reply_file('index.html', []), []).
:- http_handler(root(nf), httpNF, []).
:- http_handler(root(fmin), httpFMin, []).
:- http_handler(root(keys), httpKeys, []).
:- http_handler(root(primary), httpPrimaryAttributes, []).
:- http_handler(root(secondary), httpSecondaryAttributes, []).
:- http_handler(root(bcnfs), httpBCNFs, []).
:- http_handler(root(d3nfs), http3NFs, []).

port(5000).

start :-
    port(Port),
    server(Port).

stop :-
    port(Port),
    http_stop_server(Port, []).

restart :-
    stop, [ws], start.

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

null_to_empty([], '') :- !.
null_to_empty(PrimaryAttributes, PrimaryAttributes).

reply(Text) :-
    format('Content-type: text/plain~n~n'),
    format(Text).

% nf
httpNF(Request) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    nf(R, F0, N),
    reply(N).

% fmin
httpFMin(Request) :-
    http_parameters(Request,
      [
        f(F, [])
      ]),
    parse_fds(F, F0),
    fmins(F0, FMins),
    reply(FMins).

% keys
httpKeys(Request) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    keys(R, F0, Keys),
    atomic_list_concat(Keys, ', ', S),
    reply(S).

% primary attributes
httpPrimaryAttributes(Request) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    primaryattributes(R, F0, PrimaryAttributes),
    null_to_empty(PrimaryAttributes, PrimaryAttributes0),
    reply(PrimaryAttributes0).

% secondary attributes
httpSecondaryAttributes(Request) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    secondaryattributes(R, F0, SecondaryAttributes),
    null_to_empty(SecondaryAttributes, SecondaryAttributes0),
    reply(SecondaryAttributes0).

% BCNF decompositions
httpBCNFs(Request) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    bcnfs(R, F0, Rhos),
    reply(Rhos).

% 3NF decompositions
http3NFs(Request) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    d3nfs(R, F0, Rhos),
    reply(Rhos).

 