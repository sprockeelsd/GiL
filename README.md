# GiLv2.0

GiL (Gecode interface Lisp) is a wrapper that allows to use Gecode in Lisp program.

## How to use
Install Gecode, a Common Lisp implementation and CFFI. Depending on what MacOS version you are using, you might need to remove the execution of script.sh from the makefile, or to change the path in the script. You are ready to load the library and use it! A small example with the SEND+MORE=MONEY problem is shown in the **Gil introduction** pdf from my master's thesis. The example is first gievn in Lisp, then in C++.

## How to improve GiL
GiL does not provide full wrapping of Gecode. The **Gil introduction** pdf explains what is missing and suggests ways to improve it.
