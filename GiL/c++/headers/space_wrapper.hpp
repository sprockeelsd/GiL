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


void writeToLogFile(const char* message);

class WSpace: public Space {
protected:
    IntVarArray vars;
    int size;

public:

    WSpace(int size);

    int getSize();

    int* return_solution();

    WSpace(WSpace &s);

    virtual Space *copy(void);

    string toString();

};


/*
 * Search engine
 */
DFS<WSpace>* make_dfs(WSpace* sp);

WSpace* get_next_solution_space(DFS<WSpace>* dfs);

//int* all_steps_at_once(WSpace* sp);

//int* get_sol(WSpace* sp);

#endif
