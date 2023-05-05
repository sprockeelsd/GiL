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
WSpace::WSpace() : vars(*this, 3,1,4){
    distinct(*this, vars);
    branch(*this, vars, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
}

int WSpace::getSize(){
    return size;
}

int* WSpace::return_solution(){
    int* solution = new int[size];
    for(int i = 0; i < size; i++){
        solution[i] = vars[i].val();
    }
    return solution;
}

WSpace::WSpace(WSpace& s): Space(s){
    //IntVars update
    vars.update(*this, s.vars);
}

Space* WSpace::copy(void) {
    return new WSpace(*this);
}

/**=================
   = Search engine =
   =================*/

WdfsEngine::WdfsEngine(WSpace* sp) {
    dfs = new DFS<WSpace>(sp);
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
