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

class WSpace: public Space {
protected:
    IntVarArray vars;

public:
    /**
     Default constructor
     */
    WSpace();

    WSpace(WSpace &s);

    virtual Space *copy(void);

};


//=================
//= Search engine =
//=================

class WdfsEngine {
protected:
    DFS<WSpace>* dfs;
public:
    WdfsEngine(WSpace* sp, Options opts);
    ~WdfsEngine();

    /**
     Search the next solution for this search engine.
     */
    WSpace* next();
    int stopped();
};

#endif
