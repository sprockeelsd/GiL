#ifndef gecode_wrapper_hpp
#define gecode_wrapper_hpp

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    IRT_EQ,
    IRT_NQ,
    IRT_LQ,
    IRT_LE,
    IRT_GQ,
    IRT_GR
};

enum {
    BOT_AND,
    BOT_OR,
    BOT_IMP,
    BOT_EQV,
    BOT_XOR
};

/**
 * Wraps the WSpace constructor.
 * @param size an integer representing the size of the problem
 * @return A pointer to a WSpace object casted as a void*
 */
void* create_space(int size);

/**
 * returns the size of the problem
 * @param sp a void* pointer to a WSpace object
 * @return an integer representing the size of the problem
 */
int get_size(void* sp);

/**
 * returns the values of the variables for a solution
 * @param sp a void* pointer to a WSpace object
 * @return a int* pointer to an int* pointer representing the values of the variables
 */
int* return_solution(void* sp);

/**
 * creates a dfs search engine for WSpace objects
 * @param sp a void* pointer to a WSpace object
 * @return a void* pointer to a DFS<WSpace>* pointer
 */
void* create_dfs(void* sp);

/**
 * returns the space of the next solution, it should be bound. If not, it will return NULL.
 * @param dfs a void* pointer to a DFS<WSpace>* pointer for the search engine of the problem
 * @return a void* pointer to a WSpace object
 */
void* return_next_solution_space(void* dfs);
//
//void* return_sol(void* sp);
//
//void* return_all(void*sp);


#ifdef __cplusplus
};
#endif
#endif
