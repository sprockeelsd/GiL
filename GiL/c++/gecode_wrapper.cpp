#include "headers/gecode_wrapper.hpp"
#include "headers/space_wrapper.hpp"

/**
 Wraps the WSpace constructor.
 */
void* computation_space() {
    return (void*) new WSpace();
}

int get_size(void* sp){
    return static_cast<WSpace*>(sp)->getSize();
}

int* return_solution(void* sp){
    return static_cast<WSpace*>(sp)->return_solution();
}

/**
 Wraps the WdfsEngine constructor.
 */
void* new_dfs_engine(void* sp){
    return (void*) new WdfsEngine(static_cast<WSpace*>(sp));
}

/**
 Wraps the WdfsEngine next method.
 */
void* dfs_next(void* se){
    return (void*) static_cast<WdfsEngine*>(se)->next();
}

/**
 Wraps the WSpace destructor.
 */
void release(void* sp) {
    delete static_cast<WSpace*>(sp);
}

