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


/*****************
 * Problem class *
 *****************/
 // This class represents a constraint problem to be solved
class Problem: public Space {
protected:
    // solution related attributes
    IntVarArray vars; // The variables of interest
    int size; // The size of the variable array of interest
    // @todo Add here any additional attributes you need to represent your problem (domain bounds, ...)

public:
    /**
     * Constructor
     * @todo Modify this constructor depending on your problem. This constructor is where the problem is defined
     * @todo (variables, constraints, branching, ...)
     * @param size
     */
    Problem(int size);

    /**
     * Copy constructor
     * @param s an instance of the Problem class
     * @todo modify this copy constructor to also copy any additional attributes you add to the class
     */
    Problem(Problem &s);

    /**
     * Returns the size of the problem
     * @return an integer representing the size of the vars array
     */
    int getSize();

    /**
     * Returns the values taken by the variables vars in a solution
     * @todo Modify this to return the solution for your problem. This function uses @param size to generate an array of integers
     * @return an array of integers representing the values of the variables in a solution
     */
    int* return_solution();

    /**
     * Copy method
     * @return a copy of the current instance of the Problem class. Calls the copy constructor
     */
    virtual Space *copy(void);

    /**
     * toString method
     * @return a string representation of the current instance of the Problem class.
     * Right now, it returns a string "Problem object. size = <size>"
     * @todo modify this method to also print any additional attributes you add to the class
     */
    string toString();

};


/*************************
 * Search engine methods *
 *************************/

/**
 * Creates a DFS engine for the given problem
 * @todo Modify this function to add search options etc
 * @param pb an instance of the Problem class representing a given problem
 * @return a DFS engine for the given problem
 */
DFS<Problem>* make_dfs(Problem* pb);

/**
 * Returns the next solution space for the problem
 * @param dfs a DFS solver for the problem
 * @return an instance of the Problem class representing the next solution to the problem
 */
Problem* get_next_solution_space(DFS<Problem>* dfs);


/***********************
 * Auxiliary functions *
 ***********************/

/**
 * Write a text into a log file
 * @param message the text to write
 */
void writeToLogFile(const char* message);

#endif
