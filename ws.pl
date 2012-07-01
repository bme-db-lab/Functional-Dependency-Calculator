:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(fd_parser).
:- use_module(fd).

:- http_handler(root(.), httpRoot, []).
:- http_handler(root(nf), httpNF, []).
:- http_handler(root(fmin), httpFMin, []).
:- http_handler(root(keys), httpKeys, []).
:- http_handler(root(primaryattributes), httpPrimaryAttributes, []).
:- http_handler(root(secondaryattributes), httpSecondaryAttributes, []).
:- http_handler(root(fclose), httpFClose, []).

start :-
    server(5000).

stop :-
    http_stop_server(5000, []).

restart :-
    stop, [ws], start.

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

null_to_empty([], '') :- !.
null_to_empty(PrimaryAttributes, PrimaryAttributes).

reply(Text) :-
    format('Content-type: text/plain~n~n'),
    format(Text).

httpRoot(_) :-
    format('Content-type: text/html~n~n'),
    format('<!DOCTYPE html>~n'),
    format('<html>~n'),
    format('<head>~n'),
    format('  <title>Functional Dependency Calculator Web Service</title>~n'),
    format('</head>~n'),
    format('<body>~n'),
    format('  <h1>Functional Dependency Calculator Web Service</h1>~n'),
    format('  <div>Examples:</div>~n'),
    format('  <ul>~n'),
    format('    <li><a href="nf?r=abcdef&f=a->b,b->c,c->a,d->e,e->f,f->d">nf</a></li>~n'),
    format('    <li><a href="fmin?f=a->b,b->d,a->d">fmin</a></li>~n'),
    format('    <li><a href="keys?r=abcdef&f=a->b,b->c">keys</a></li>~n'),
    format('    <li><a href="primaryattributes?r=abcdef&f=a->b,b->c">primaryattributes</a></li>~n'),
    format('    <li><a href="secondaryattributes?r=abcdef&f=a->b,b->c">secondaryattributes</a></li>~n'),
    format('  </ul>~n'),
    format('</body>~n'),
    format('</html>~n').

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
    fmin(F0, FMin),
    fds_to_string(FMin, FMinS),
    reply(FMinS).

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

