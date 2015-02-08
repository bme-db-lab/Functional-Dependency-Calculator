Functional Dependency Calculator
================================
Gabor Szarnyas, Adam Lippai, 2012--2015.

Little Prolog tool for performing simple algorithms on functional dependency sets.

The application (in Hungarian) is available as a live web service at <https://www.db.bme.hu/fd-calculator>.

The web service (in English) is also available at <http://alippai.github.io/fdcalc-frontend/>.

Given a relational scheme and a set of functional dependencies the program can
 - determinte the highest normal form of a relational schema,
 - enumerate minimal covers of the FD set,
 - enumerate the keys of the relational schema,
 - determine the primary and secondary attributes of the relational schema,
 - enumerate lossless and dependency preserving 3NF or lossless BCNF decompositions of the schema.
 
Compatible and tested with SWI-Prolog (http://www.swi-prolog.org/). SWI-Prolog was chosen because of it's module concept, it's ability to run a HTTP server, exchange data in AJAX format and it's unit testing framework.

Architecture
------------
```
┌───────────────────────┐   ┌────────────────────────┐
│     web frontend      │   │    Prolog console      │
│ [e.g. HTML+AJAX page] │   │   [e.g. SWI─Prolog]    │
└───────────┬───────────┘   └────────────┬───────────┘
            │                            │            
┌───────────┴───────────┐                │            
│  web service (ws.pl)  │                │            
│     [SWI-Prolog]      │                │            
└───────────┬───────────┘                │            
            │                            │            
┌───────────┴────────────────────────────┴───────────┐
│ Functional Dependency Calculator frontend (fd.pl)  │
└─────────────────────────┬──────────────────────────┘
                          │                           
┌─────────────────────────┴──────────────────────────┐
│   Functional Dependency Calculator core (fdc.pl)   │
└────────────────────────────────────────────────────┘
```

Each layer uses only lower layers so the web service, the frontend and the core layer may run without the higher ones.

Usage
-----
In the SWI-Prolog console compile ```fd.pl``` (type ```[fd].```).

Functional dependencies are formatted like ab->cd. Prolog atoms must begin with a small letter so you should use small letters for each attribute.

Enumerate minimal covers of a FD set (note that the relational schema is irrelevant in this case).
```
?- fmin([cd->e, ab->cd, d->a, a->b, b->ac], FMin).
FMin = [ (a->b), (b->c), (b->d), (d->a), (d->e)] ;
FMin = [ (a->b), (a->d), (b->a), (b->c), (d->a), (d->e)] ;
FMin = [ (a->b), (a->c), (b->d), (d->a), (d->e)] ;
FMin = [ (a->b), (a->c), (a->d), (b->a), (d->a), (d->e)] ;
false.
```

Determinte the highest normal form of a relational schema:
```
?- nf(abcdef, [a->b, b->c, c->a, d->e, e->f, f->d], NF).
NF = nf3NF.
```
Enumerate the keys of a relational schema:
```
?- keys(abcdef, [a->b, b->c, c->a, d->e, e->f, f->d], Keys).
Keys = [af, ae, ad, bf, be, bd, cf, ce, cd].
```

Determine the primary and secondary attributes of a relational schema:
```
?- primaryattributes(abcd, [a->b, bc->ad], Primary).
Primary = [a, b, c].

?- secondaryattributes(abcd, [a->b, bc->ad], Secondary).
Secondary = [d].
```

Enumerate lossless and dependency preserving 3NF...
```
?- d3nf(abcde, [ab->b, b->c, c->bd], Rho).
Rho = [ace, bc, cd] ;
Rho = [abe, bc, cd] ;
false.
```

...or lossless BCNF decompositions of a schema:
```
?- bcnf(abcde, [ab->b, b->c, c->bd], Rho).
Rho = [ab, abe, bc, bd] ;
Rho = [abe, bc, bd] ;
Rho = [abe, bc, bd, be] ;
Rho = [ac, ace, bc, cd] ;
Rho = [ace, bc, cd] ;
Rho = [ace, bc, cd, ce] ;
Rho = [ab, ace, bc, cd] ;
Rho = [ab, abe, bc, cd] ;
Rho = [abe, ac, bc, cd] ;
Rho = [abe, bc, cd] ;
Rho = [abe, bc, be, cd] ;
Rho = [abe, bc, cd, ce] ;
Rho = [ace, bc, be, cd] ;
false.
```
The BCNF decomposition algoritm may produce a _lot_ of possible decompositions even for schemas with only a few attributes.

Starting the web service
------------------------
 - From SWI-Prolog: compile ```ws.pl``` then run ```start.```.
 - From command line: start the web service with the ```start.sh``` or ```start.bat``` script. Visit http://localhost:5000/ and try the examples. You may change the port by editing the ```port``` clause in ```ws.pl```.

Running the unit tests
----------------------
To run the unit tests, run ```test.sh```. Note that SWI-Prolog does not include ```plunit``` by default, see ```plunit_install.txt``` for details.
