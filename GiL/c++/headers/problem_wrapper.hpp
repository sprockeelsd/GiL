#ifndef gecode_wrapper_hpp
#define gecode_wrapper_hpp

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    BOT_AND,
    BOT_OR,
};

/**
 * Wraps the Problem constructor.
 * @todo modify this to include any parameters your Problem constructor requires
 * @param size an integer representing the size of the problem
 * @return A pointer to a Problem object casted as a void*
 */
void* create_space(int size);

/**
 * returns the size of the problem
 * @param sp a void* pointer to a Problem object
 * @return an integer representing the size of the problem
 */
int get_size(void* sp);

/**
 * returns the values of the variables for a solution
 * @param sp a void* pointer to a Problem object
 * @return an int* pointer representing the values of the variables
 */
int* return_solution(void* sp);

/**
 * creates a dfs search engine for Problem objects
 * @param sp a void* pointer to a Problem object
 * @return a void* cast of a DFS<Problem>* pointer
 */
void* create_dfs(void* sp);

/**
 * returns the next solution space, it should be bound. If not, it will return NULL.
 * @param dfs a void* pointer to a DFS<Problem>* pointer for the search engine of the problem
 * @return a void* cast of a Problem* pointer
 */
void* return_next_solution_space(void* dfs);

#ifdef __cplusplus
};
#endif
#endif
