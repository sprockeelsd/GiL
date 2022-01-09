# GiLv2.0

GiL (Gecode interface Lisp) is a wrapper that allows to use Gecode functions in Lisp program. It is far for complete but still provides enough tools to generate interesting CSPs in Common Lisp. It is the continuation of Baptiste Lapière's work (https://github.com/blapiere/GiL) and any contribution is welcomed.

## How to use
**Disclaimer : GiL currently only works onMacOS.**
Install Gecode, a Common Lisp implementation and CFFI. Depending on what MacOS version you are using, you might run into a problem with the dynamic library not finding Gecode due to a relative path problem. If this is the case, there is a script **script.sh** in the c++ folder that should solve the problem. You should update the value for the relative and full path in the script, then run it. The problem should be gone after that.


You are ready to load the library and use it! A small example problem is shown in the **Gil** pdf. The exact same problem is given in C++ and in Lisp.

## How to improve GiL
The **Gil** pdf provides explanation on how to add features to GiL. Here is a (non-exhaustive) list of ideas to add : 

- Making the branch-and-bound searchengine work
- Adding the Limited Discrepancy search engine
- Adding support for other variables than integer and boolean
- Adding branching strategies
- Adding other constraints

Basically the long term goal is to support all Gecode features in GiL.

