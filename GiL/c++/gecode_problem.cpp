#include "headers/gecode_problem.hpp"

/*************************
 * Problem class methods *
 *************************/

/**
 * Constructor
 * @todo Modify this constructor depending on your problem. This constructor is where the problem is defined
 * @todo (variables, constraints, branching, ...)
 * @param size
 */
Problem::Problem(int s) {
    string message = "WSpace object created. ";
    size = s;
    message += "size = " + to_string(size) + ".\n";

    // variable initialization
    vars = IntVarArray(*this, size, 1, 4);

    //constraints
    distinct(*this, vars);

    //branching
    branch(*this, vars, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    writeToLogFile(message.c_str());
}

/**
 * Copy constructor
 * @param s an instance of the Problem class
 * @todo modify this copy constructor to also copy any additional attributes you add to the class
 */
Problem::Problem(Problem& s): Space(s){
    //IntVars update
    size = s.size;
    vars.update(*this, s.vars);
}

/**
 * Returns the size of the problem
 * @return an integer representing the size of the vars array
 */
int Problem::getSize(){
    string message = "getSize function called. size = " + to_string(size) + "\n";
    writeToLogFile(message.c_str());
    return size;
}

/**
 * Returns the values taken by the variables vars in a solution
 * @todo Modify this to return the solution for your problem. This function uses @param size to generate an array of integers
 * @return an array of integers representing the values of the variables in a solution
 */
int* Problem::return_solution(){
    string message = "return_solution method. Solution : [";
    int* solution = new int[size];
    for(int i = 0; i < size; i++){
        solution[i] = vars[i].val();
        message += to_string(solution[i]) + " ";
    }
    message += "]\n";
    writeToLogFile(message.c_str());
    return solution;
}

/**
 * Copy method
 * @return a copy of the current instance of the Problem class. Calls the copy constructor
 */
Space* Problem::copy(void) {
    return new Problem(*this);
}

/**
 * toString method
 * @return a string representation of the current instance of the Problem class.
 * Right now, it returns a string "Problem object. size = <size>"
 * @todo modify this method to also print any additional attributes you add to the class
 */
string Problem::toString(){
    string message = "Problem object. \n";
    message += "size = " + to_string(size) + "\n";
    writeToLogFile(message.c_str());
    return message;
}

/*************************
 * Search engine methods *
 *************************/

/**
 * Creates a DFS engine for the given problem
 * @todo Modify this function to add search options etc
 * @param pb an instance of the Problem class representing a given problem
 * @return a DFS engine for the given problem
 */
DFS<Problem>* make_dfs(Problem* pb){
    writeToLogFile("make_dfs function called.\n");
    return new DFS<Problem>(pb);
}

/**
 * Returns the next solution space for the problem
 * @param dfs a DFS solver for the problem
 * @return an instance of the Problem class representing the next solution to the problem
 */
Problem* get_next_solution_space(DFS<Problem>* dfs){
    string message = "get_next_solution_space function called.\n";
    Problem* sol_space = dfs->next();
    message += sol_space->toString();
    int* sol = sol_space->return_solution();
    writeToLogFile(message.c_str());
    return sol_space; // WSpace* pointer
}

/***********************
 * Auxiliary functions *
 ***********************/

/**
 * Write a text into a log file
 * @param message the text to write
 */
void writeToLogFile(const char* message){
    std::time_t currentTime = std::time(nullptr); // Get the current time
    std::string timeString = std::asctime(std::localtime(&currentTime)); // Convert to string

    const char* homeDir = std::getenv("HOME"); // Get the user's home directory
    if (homeDir) {
        std::string filePath(homeDir);
        filePath += "/log.txt"; // Specify the desired file path, such as $HOME/log.txt

        std::ofstream myfile(filePath, std::ios::app); // append mode
        if (myfile.is_open()) {
            myfile <<timeString<< endl << message << endl;
            myfile.close();
        }
    }
}
