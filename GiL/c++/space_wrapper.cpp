#include "headers/space_wrapper.hpp"
#include <iostream>
#include <fstream>

using namespace Gecode;
using namespace Gecode::Int;
using namespace Gecode::Set;
using namespace std;

/*
To Print value to a file :
    ofstream myfile;
    myfile.open ("/home/amdiels/Bureau/example.txt", ios::app);
    myfile << value << endl;
    myfile.close();
*/

/**
 Default constructor
 */
WSpace::WSpace() {
}

//==================
//= Search support =
//==================

/**
 Define which variable, denoted by vid, will be considered as the cost.
 */

WSpace::WSpace(WSpace& s): Space(s){
    //IntVars update
    vars.update(*this, s.vars);
}

Space* WSpace::copy(void) {
    return new WSpace(*this);
}

//=================
//= Search engine =
//=================

/*
 Depth-first search
*/
WdfsEngine::WdfsEngine(WSpace* sp, Options opts) {
    dfs = new DFS<WSpace>(sp, opts);
}

WdfsEngine::~WdfsEngine() {
    delete dfs;
}

/**
 Search the next solution for this search engine.
 */
WSpace* WdfsEngine::next() {
    return dfs->next();
}

/**
 Returns true if the search has been stopped by a search object
 */
int WdfsEngine::stopped(){
    return dfs->stopped();
}
