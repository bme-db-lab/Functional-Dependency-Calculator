:- use_module(fd).
:- use_module(library(plunit)).
:- begin_tests(lists).

% normal forms
test(nf01) :- nf(abc, [a->b], nf1NF).
test(nf02) :- nf(lmno, [m->o, lm->ln, n->m, no->m], nf1NF).
test(nf03) :- nf(abcd, [c->b, b->d, ab->ac, cd->b], nf1NF).
test(nf04) :- nf(abcd, [ab->c, c->d], nf2NF).
test(nf05) :- nf(vui, [vu->i, i->v], nf3NF).
test(nf06) :- nf(abcdef, [a->b, b->c, c->a, d->e, e->f, f->d], nf3NF).
test(nf07) :- nf(abc, [a->b, b->c, c->a], nfBCNF).
test(nf08) :- nf(abcdef, [a->d, b->e, c->f, d->b, e->c, f->a], nfBCNF).

% fmin
test(fmin01) :-
  fmins_all(
    [a->b, ab->d, b->a, d->a],
    [[ (a->b), (b->d), (d->a)], [ (a->b), (a->d), (b->a), (d->a)]]
  ).
test(fmin02) :- 
  fmins_all(
    [cd->e, ab->cd, d->a, a->b, b->ac],
    [[ (a->b), (b->c), (b->d), (d->a), (d->e)],
     [ (a->b), (a->d), (b->a), (b->c), (d->a), (d->e)],
     [ (a->b), (a->c), (b->d), (d->a), (d->e)],
     [ (a->b), (a->c), (a->d), (b->a), (d->a), (d->e)]]
  ).
test(fmin03) :- fmins_all(
    [abcd->e, e->d, a->b, ac->d],
    [[(a->b), (ac->e), (e->d)]] 
  ).

:- end_tests(lists).
