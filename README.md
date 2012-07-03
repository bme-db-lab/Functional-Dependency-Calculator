Functional Dependency Calculator
================================
Gabor Szarnyas, 2012.

Little Prolog tool for performing simple algorithms on functional dependency sets.
Currently supported:
 - minimal cover of an FD set
 - highest normal form of a relational schema
 
Compatible with SWI-Prolog (http://www.swi-prolog.org/) and SICStus Prolog.

Usage (SWI-Prolog):
```
1 ?- [fd].
%  ...
true.

2 ?- fmin([cd->e,ab->cd,d->a,a->b,b->ac], FMin).
FMin = [ (a->b), (b->c), (b->d), (d->a), (d->e)] ;
FMin = [ (a->b), (a->d), (b->a), (b->c), (d->a), (d->e)] ;
FMin = [ (a->b), (a->c), (b->d), (d->a), (d->e)] ;
FMin = [ (a->b), (a->c), (a->d), (b->a), (d->a), (d->e)] ;
false.

3 ?- nf(abcdef, [a->b, b->c, c->a, d->e, e->f, f->d], NF).
NF = nf3NF.
```

To run the test compile ```testfds.pl``` and run type ```testfds.```

```
1 ?- [testfds].
%  ...
true.

2 ?- testfds.
...
```

Architecture
-------------
```
┌───────────────────────┐      ┌────────────────────┐
│ web frontend          │      │  Prolog console    │
│ [e.g. HTML+AJAX page] │      │ [e.g. SWI─Prolog]  │
└───────────┬───────────┘      └─────────┬──────────┘
            │                            │           
┌───────────┴───────────┐                │           
│ web service (ws.pl)   │                │           
│    [SWI-Prolog]       │                │           
└───────────┬───────────┘                │           
            │                            │           
┌───────────┴────────────────────────────┴──────────┐
│ Functional Dependency Calculator frontend (fd.pl) │
└───────────────────────┬───────────────────────────┘
                        │
┌───────────────────────┴───────────────────────────┐
│   Functional Dependency Calculator core (fdc.pl)  │
└───────────────────────────────────────────────────┘
```

Starting the web service from command line
------------------------------------------

Start the service with the following command:
```
swipl -f ws.pl -g start
```

Visit http://localhost:5000/ and try the examples.
