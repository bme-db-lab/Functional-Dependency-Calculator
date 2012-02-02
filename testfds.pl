:- use_module(fdp).

writeList([]).
writeList([H|T]) :-
  write(H), write('\n'),
  writeList(T).

testNF(R, F) :-
  write('nf('), write(R), write(', '), write(F), write(', N).\n'),
  nf(R, F, N),
  write(N), write('\n').

testFmin(F) :-
  write('fmin('), write(F), write(', FMin).\n'),
  findall(FMin, fmin(F, FMin), FMins),
  writeList(FMins),
  write('\n').

testfds :-
  write('Determining the highest normal form of a relational schema\n'),
  write('===========================================================\n'),
  
  write('\n1NF\n'),
  write('-------------------------------------------\n'),
  testNF(abc, [a->b]),
  testNF(lmno, [m->o, lm->ln, n->m, no->m]),
  testNF(abcd, [c->b, b->d, ab->ac, cd->b]),
  
  write('\n2NF\n'),
  write('-------------------------------------------\n'),  
  testNF(abcd, [ab->c, c->d]),
  
  write('\n3NF\n'),
  write('-------------------------------------------\n'),
  testNF(vui, [vu->i, i->v]),
  testNF(abcdef, [a->b, b->c, c->a, d->e, e->f, f->d]),

  write('\nBCNF\n'),
  write('-------------------------------------------\n'),
  testNF(abc, [a->b, b->c, c->a]),
  testNF(abcdef, [a->d, b->e, c->f, d->b, e->c, f->a]),
  
  write('\nCalculating minimal cover(s)\n'),
  write('===========================================================\n\n'),
  
  testFmin([cd->e,ab->cd,d->a,a->b,b->ac]),
  testFmin([no->p, l->m, lm->no, m->ln, o->l]),
  testFmin([a->b, ab->cd, b->ac, cd->e, d->a]),
  
  true.