Functional Dependency Calculator
================================
Gabor Szarnyas, 2012.

Little Prolog tool for performing simple algorithms on functional dependency sets.
Given a relational scheme and a set of functional dependencies the program can
 - determinte the highest normal form of a relational schema,
 - enumerate minimal covers of the FD set,
 - enumerate the keys of the relational schema,
 - determine the primary and secondary attributes of the relational schema,
 - enumerate lossless and dependency preserving 3NF or lossless BCNF decompositions of the schema.
 
Compatible and tested with SWI-Prolog (http://www.swi-prolog.org/).

Architecture
-------------
```
┌───────────────────────┐   ┌────────────────────────┐
│ web frontend          │   │  Prolog console        │
│ [e.g. HTML+AJAX page] │   │ [e.g. SWI─Prolog]      │
└───────────┬───────────┘   └────────────┬───────────┘
            │                            │            
┌───────────┴───────────┐                │            
│ web service (ws.pl)   │                │            
│    [SWI-Prolog]       │                │            
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

Starting the web service from command line
------------------------------------------

Start the web service with the ```start.sh``` or ```start.bat``` script. Visit http://localhost:5000/ and try the examples. You may change the port in ```ws.pl``` by editing the ```port``` clause.

