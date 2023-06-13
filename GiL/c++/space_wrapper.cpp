#include "headers/space_wrapper.hpp"
#include <iostream>
#include <fstream>
#include <string>
#include <ctime>

using namespace Gecode;
using namespace Gecode::Int;
using namespace Gecode::Set;
using namespace std;

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

/************************
 * WSpace class methods *
 ************************/

WSpace::WSpace(int s) {
    string message = "WSpace object created. ";
    size = s;
    message += "size = " + to_string(size) + ".\n";
    vars = IntVarArray(*this, size, 1, 4);
    distinct(*this, vars);
    branch(*this, vars, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    writeToLogFile(message.c_str());
}

int WSpace::getSize(){
    string message = "getSize function called. size = " + to_string(size) + "\n";
    writeToLogFile(message.c_str());
    return size;
}

int* WSpace::return_solution(){
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

string WSpace::toString(){
    string message = "toString method. \n";
    message += "size = " + to_string(size) + "\n";
    writeToLogFile(message.c_str());
    return message;
}

WSpace::WSpace(WSpace& s): Space(s){
    //IntVars update
    size = s.size;
    vars.update(*this, s.vars);
}

Space* WSpace::copy(void) {
    return new WSpace(*this);
}

/*************************
 * Search engine methods *
 *************************/

DFS<WSpace>* make_dfs(WSpace* sp){
    writeToLogFile("make_dfs function called.\n");
    return new DFS<WSpace>(sp);
}

WSpace* get_next_solution_space(DFS<WSpace>* dfs){
    string message = "get_next_solution_space function called.\n";
    WSpace* sol_space = dfs->next();
    message += sol_space->toString();
    int* sol = sol_space->return_solution();
    writeToLogFile(message.c_str());
    return sol_space; // WSpace* pointer
}

///**
// * @todo There is a problem here when trying to get the size of a solution space.
// * First try to generate one response that works, then divide it into different functions
// */
//
//int* all_steps_at_once(WSpace* sp){
//    DFS<WSpace>* dfs = make_dfs(sp);
//     WSpace* sol_space = dfs->next();
//     return sol_space->return_solution();
//}

//int* get_sol(WSpace* sp){
//    return sp->return_solution();
//}
//
