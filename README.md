Semantic Properties of Extension Sets
===========================

This repository contains the code developed as a term project for the course
"Abstract Argumentation" at TU Vienna. The course was held by Uwe Egly,
Thomas Linsbichler, and Stefan Woltran.  A goal of the project was to
empirically evaluate certain properties of the extensions for a set of
argumentation frameworks.

The tools are written in Haskell and can be compiled with Stack. Furthermore,
the clasp Answer Set Solver is required.  See the `doc/report/report.tex` file for an
extensive documentation.

Installing the ASPARTIX framework
=========================

The ASPARTIX encoding for various semantics are required for the tools to
work correctly. They can be downloaded from
[the ASPARTIX page](http://www.dbai.tuwien.ac.at/research/project/argumentation/systempage/) and
should be saved into the `statistics/APX` directory and appropriate soft
links can be added to simplify usage. During development they were set up as
follows:

* adm.dl
* prefex_gringo.lp
* semi_stable_gringo.lp
* stable.dl
* stage_gringo.lp
* adm -> adm.dl
* prf -> prefex_gringo.lp
* sem -> semi_stable_gringo.lp
* stb -> stable.dl
* stg -> stage_gringo.lp
