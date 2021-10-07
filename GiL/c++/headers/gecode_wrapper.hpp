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
 Wraps the WSpace constructor.
 */
void* computation_space();

/**
 Wraps the WSpace add_intVar method.
 */
int add_intVar(void* sp, int min, int max);

/**
 Wraps the WSpace add_intVarWithDom method.
 */
int add_intVarWithDom(void* sp, int s, int* dom);

/**
 Wraps the WSpace add_intVarArray method.
 */
int* add_intVarArray(void* sp, int n, int min, int max);

/**
 Wraps the WSpace add_intVarArrayWithDom method.
 */
int* add_intVarArrayWithDom(void* sp, int n, int s, int* dom);

/**
 Wraps the WSpace nvars method.
 */
int nvars(void* sp);

/**
 Wraps the WSpace add_boolVar method.
 */
int add_boolVar(void* sp, int min, int max);

/**
 Wraps the WSpace add_boolVar_expr_val method.
 */
int add_boolVar_expr_val(void* sp, int vid, int rel_type, int val);

/**
 Wraps the WSpace add_boolVar_expr_var method.
 */
int add_boolVar_expr_var(void* sp, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace cst_val_rel method.
 */
void val_rel(void* sp, int vid, int rel_type, int val);

/**
 Wraps the WSpace cst_var_relr method.
 */
void var_rel(void* sp, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace cst_arr_val_rel method.
 */
void arr_val_rel(void* sp, int n, int* vids, int rel_type, int val);

/**
 Wraps the WSpace cst_arr_var_rel method.
 */
void arr_var_rel(void* sp, int n, int* vids, int rel_type, int vid);

/**
 Wraps the WSpace cst_arr_rel method.
 */
void arr_rel(void* sp, int n, int* vids, int rel_type);

/**
 Wraps the WSpace cst_arr_arr_rel method.
 */
void arr_arr_rel(void* sp, int n1, int* vids1, int rel_type, int n2, int* vids2);

/**
 Wraps the WSpace cst_distinct method.
 */
void distinct(void* sp, int n, int* vids);

/**
 Wraps the WSpace cst_val_linear method.
 */
void val_linear(void* sp, int n, int* c, int* vids, int rel_type, int value);

/**
 Wraps the WSpace cst_var_linear method.
 */
void var_linear(void* sp, int n, int* c, int* vids, int rel_type, int vid);

/**
 Wraps the WSpace cst_abs method.
 */
void arithmetics_abs(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace acst_div method.
 */
void arithmetics_div(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_var_mod method.
 */
void arithmetics_mod(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_divmod method.
 */
void arithmetics_divmod(void* sp, int vid1, int vid2, int vid3, int vid4);

/**
 Wraps the WSpace cst_min method.
 */
void arithmetics_min(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_arr_min method.
 */
void arithmetics_arr_min(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_argmin method.
 */
void arithmetics_argmin(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_max method.
 */
void arithmetics_max(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_arr_max method.
 */
void arithmetics_arr_max(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_argmax method.
 */
void arithmetics_argmax(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_mult method.
 */
void arithmetics_mult(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_sqr method.
 */
void arithmetics_sqr(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace cst_sqrt method.
 */
void arithmetics_sqrt(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace cst_pow method.
 */
void arithmetics_pow(void* sp, int vid1, int n, int vid2);

/**
 Wraps the WSpace cst_nroot method.
 */
void arithmetics_nroot(void* sp, int vid1, int n, int vid2);

/**
 Wraps the WSpace cst_dom method.
 */
void set_dom(void* sp, int vid, int n, int* d);

/**
 Wraps the WSpace cst_member method.
 */
void set_member(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_sum method.
 */
void rel_sum(void* sp, int vid, int n, int* vids);

/**
 Wraps the WSpace cst_count_val_val method.
 */
void count_val_val(void* sp, int n, int* vids, int val1, int rel_type, int val2);

/**
 Wraps the WSpace cst_count_val_var method.
 */
void count_val_var(void* sp, int n, int* vids, int val, int rel_type, int vid);

/**
 Wraps the WSpace cst_count_var_val method.
 */
void count_var_val(void* sp, int n, int* vids, int vid, int rel_type, int val);

/**
 Wraps the WSpace cst_count_var_var method.
 */
void count_var_var(void* sp, int n, int* vids, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace cst_nvalues method.
 */
void nvalues(void* sp, int n, int* vids, int rel_type, int vid);

/**
 Wraps the WSpace cst_circuit method.
 */
void circuit(void* sp, int n, int* c, int* vids1, int* vids2, int vid);

/**
 Wraps the WSpace cst_precede method 
*/
void precede(void* sp, int n, int* vids, int s, int u);

/**
 Wraps the WSpace cst_boolop_val method.
 */
void val_boolop(void* sp, int vid1, int bool_op, int vid2, int val);

/**
 Wraps the WSpace cst_boolop_var method.
 */
void var_boolop(void* sp, int vid1, int bool_op, int vid2, int vid3);

/**
 Wraps the WSpace cst_boolrel_val method.
 */
void val_boolrel(void* sp, int vid, int rel_type, int val);

/**
 Wraps the WSpace cst_boolrel_var method.
 */
void var_boolrel(void* sp, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace branch method.
 */
void branch(void* sp, int n, int* vids, int var_strategy, int val_strategy);

/**
 Wraps the WSpace branch_b method.
 */
void branch_b(void* sp, int n, int* vids, int var_strategy, int val_strategy);

/**
 Wraps the WSpace cost method.
 */
void cost(void* sp, int vid);

/**
 Wraps the WbabEngine constructor.
 */
void* new_bab_engine(void* sp);

/**
 Wraps the WbabEngine next method.
 */
void* bab_next(void* se);

/**
 Wraps the WdfsEngine constructor.
 */
void* new_dfs_engine(void* sp);

/**
 Wraps the WdfsEngine next method.
 */
void* dfs_next(void* se);

/**
 Wraps the WSpace destructor.
 */
void release(void* sp);

/**
 Wraps the WSpace value method.
 */
int get_value(void* sp, int vid);

/**
 Wraps the WSpace values method.
 */
int* get_values(void* sp, int n, int* vids);

/**
 Wraps the WSpace print method.
 */
void print_vars(void* sp, int n, int* vids);

#ifdef __cplusplus
};
#endif
#endif
