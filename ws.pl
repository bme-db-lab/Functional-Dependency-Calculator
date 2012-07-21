:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(fd_parser).
:- use_module(fd).
:- use_module(functional).
:- use_module(timeout).

:- http_handler(root(.),         httpIndex,                      []).
:- http_handler(root(nf),        httpNFTimeout,                  []).
:- http_handler(root(keys),      httpKeysTimeout,                []).
:- http_handler(root(primary),   httpPrimaryAttributesTimeout,   []).
:- http_handler(root(secondary), httpSecondaryAttributesTimeout, []).
:- http_handler(root(fmin),      httpFMinTimeout,                []).
:- http_handler(root(bcnfs),     httpBCNFsTimeout,               []).
:- http_handler(root(d3nfs),     http3NFsTimeout,                []).

httpIndex(Request)                      :- http_reply_file('index.html', [], Request).
httpNFTimeout(Request)                  :- reply_with_timeout(Request, httpNF).
httpKeysTimeout(Request)                :- reply_with_timeout(Request, httpKeys).
httpPrimaryAttributesTimeout(Request)   :- reply_with_timeout(Request, httpPrimaryAttributes).
httpSecondaryAttributesTimeout(Request) :- reply_with_timeout(Request, httpSecondaryAttributes).
httpFMinTimeout(Request)                :- reply_with_timeout(Request, httpFMin).
httpBCNFsTimeout(Request)               :- reply_with_timeout(Request, httpBCNFs).
http3NFsTimeout(Request)                :- reply_with_timeout(Request, http3NFs).

% port number
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
httpNF(Request, Reply) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    nf(R, F0, N),
    Reply = N.

% fmin
httpFMin(Request, Reply) :-
    http_parameters(Request,
      [
        f(F, [])
      ]),
    parse_fds(F, F0),
    fmins(F0, FMins),
    Reply = FMins.

% list all fmins with timeout
fmins(F, FMins) :-
  call_with_timeout(F, FMins, fmins_all, fmin, fd_parser:fds_to_string).

% keys
httpKeys(Request, Reply) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    keys(R, F0, Keys),
    atomic_list_concat(Keys, ', ', S),
    Reply = S.

% primary attributes
httpPrimaryAttributes(Request, Reply) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    primaryattributes(R, F0, PrimaryAttributes),
    null_to_empty(PrimaryAttributes, PrimaryAttributes0),
    Reply = PrimaryAttributes0.

% secondary attributes
httpSecondaryAttributes(Request, Reply) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    secondaryattributes(R, F0, SecondaryAttributes),
    null_to_empty(SecondaryAttributes, SecondaryAttributes0),
    Reply = SecondaryAttributes0.

% BCNF decompositions
httpBCNFs(Request, Reply) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    bcnfs(R, F0, Rhos),
    Reply = Rhos.

% list all BCNF decompositions with timeout
bcnfs(R, F, Rhos) :-
  call_with_timeout(R, F, Rhos, bcnfs_all, bcnf, fd:decomposition_to_text).

% 3NF decompositions
http3NFs(Request, Reply) :-
    http_parameters(Request,
      [
        r(R, []),
        f(F, [])
      ]),
    parse_fds(F, F0),
    d3nfs(R, F0, Rhos),
    Reply = Rhos.

% list all 3NF decompositions with timeout
d3nfs(R, F, Rhos) :-
  call_with_timeout(R, F, Rhos, d3nfs_all, d3nf, fd:decomposition_to_text).

