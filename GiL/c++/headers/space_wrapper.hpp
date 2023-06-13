#ifndef space_wrapper_hpp
#define space_wrapper_hpp

#include <vector>
#include <iostream>
#include <stdlib.h>
#include <exception>

#include "gecode/kernel.hh"
#include "gecode/int.hh"
#include "gecode/search.hh"
#include "gecode/minimodel.hh"
#include "gecode/set.hh"

using namespace Gecode;
using namespace Gecode::Int;
using namespace Gecode::Set;
using namespace std;
using namespace Gecode::Search;


/*************************
 * Problem class methods *
 *************************/

/**
 * Problem class. This class represents a problem
 */
class Problem: public Space {
protected:
    IntVarArray vars;
    int size;

public:

    Problem(int size);

    int getSize();

    int* return_solution();

    Problem(Problem &s);

    virtual Space *copy(void);

    string toString();

};


/*************************
 * Search engine methods *
 *************************/

DFS<Problem>* make_dfs(Problem* sp);

Problem* get_next_solution_space(DFS<Problem>* dfs);


/***********************
 * Auxiliary functions *
 ***********************/

void writeToLogFile(const char* message);

#endif
