#include "headers/gecode_wrapper.hpp"
#include "headers/space_wrapper.hpp"

/**
 Wraps the WSpace constructor.
 */
void* computation_space() {
    return (void*) new WSpace();
}

/**
 Wraps the WSpace add_intVar method.
 */
int add_intVar(void* sp, int min, int max) {
    return static_cast<WSpace*>(sp)->add_intVar(min, max);
}

/**
 Wraps the WSpace add_intVarWithDom method.
 */
int add_intVarWithDom(void* sp, int s, int* dom) {
    return static_cast<WSpace*>(sp)->add_intVarWithDom(s, dom);
}

/**
 Wraps the WSpace add_intVarArray method.
 */
int* add_intVarArray(void* sp, int n, int min, int max) {
    return static_cast<WSpace*>(sp)->add_intVarArray(n, min, max);
}

/**
 Wraps the WSpace add_intVarArrayWithDom method.
 */
int* add_intVarArrayWithDom(void* sp, int n, int s, int* dom) {
    return static_cast<WSpace*>(sp)->add_intVarArrayWithDom(n, s, dom);
}

/**
 Wraps the WSpace set_as_solution_variables method.
 */
int* set_solution_vars(void* sp, int n, int* vids){
    return static_cast<WSpace*>(sp)->set_as_solution_variables(n, vids);
}

/**
 Wraps the WSpace nvars method.
 */
int nvars(void* sp) {
    return static_cast<WSpace*>(sp)->nvars();
}

/**
 Wraps the WSpace add_boolVar method.
 */
int add_boolVar(void* sp, int min, int max) {
    return static_cast<WSpace*>(sp)->add_boolVar(min, max);
}

/**
 Wraps the WSpace add_boolVar_expr_val method.
 */
int add_boolVar_expr_val(void* sp, int vid, int rel_type, int val) {
    return static_cast<WSpace*>(sp)->add_boolVar_expr_val(vid, rel_type, val);
}

/**
 Wraps the WSpace add_boolVar_expr_var method.
 */
int add_boolVar_expr_var(void* sp, int vid1, int rel_type, int vid2) {
    return static_cast<WSpace*>(sp)->add_boolVar_expr_var(vid1, rel_type, vid2);
}

/**
 Wraps the WSpace cst_val_rel method.
 */
void val_rel(void* sp, int vid, int rel_type, int val) {
    return static_cast<WSpace*>(sp)->cst_val_rel(vid, rel_type, val);
}

/**
 Wraps the WSpace cst_var_relr method.
 */
void var_rel(void* sp, int vid1, int rel_type, int vid2) {
    return static_cast<WSpace*>(sp)->cst_var_rel(vid1, rel_type, vid2);
}

/**
 Wraps the WSpace cst_arr_val_rel method.
 */
void arr_val_rel(void* sp, int n, int* vids, int rel_type, int val) {
    return static_cast<WSpace*>(sp)->cst_arr_val_rel(n, vids, rel_type, val);
}

/**
 Wraps the WSpace cst_arr_var_rel method.
 */
void arr_var_rel(void* sp, int n, int* vids, int rel_type, int vid) {
    return static_cast<WSpace*>(sp)->cst_arr_var_rel(n, vids, rel_type, vid);
}

/**
 Wraps the WSpace cst_arr_rel method.
 */
void arr_rel(void* sp, int n, int* vids, int rel_type) {
    return static_cast<WSpace*>(sp)->cst_arr_rel(n, vids, rel_type);
}

/**
 Wraps the WSpace cst_arr_arr_rel method.
 */
void arr_arr_rel(void* sp, int n1, int* vids1, int rel_type, int n2, int* vids2) {
    return static_cast<WSpace*>(sp)->cst_arr_arr_rel(n1, vids1, rel_type, n2, vids2);
}

/**
 Wraps the WSpace cst_distinct method.
 */
void distinct(void* sp, int n, int* vids) {
    return static_cast<WSpace*>(sp)->cst_distinct(n, vids);
}

/**
 Wraps the WSpace cst_val_linear method.
 */
void val_linear(void* sp, int n, int* c, int* vids, int rel_type, int value) {
    return static_cast<WSpace*>(sp)->cst_val_linear(n, c, vids, rel_type, value);
}

/**
 Wraps the WSpace cst_var_linear method.
 */
void var_linear(void* sp, int n, int* c, int* vids, int rel_type, int vid) {
    return static_cast<WSpace*>(sp)->cst_var_linear(n, c, vids, rel_type, vid);
}

/**
 Wraps the WSpace cst_abs method.
 */
void arithmetics_abs(void* sp, int vid1, int vid2) {
    return static_cast<WSpace*>(sp)->cst_abs(vid1, vid2);
}

/**
 Wraps the WSpace acst_div method.
 */
void arithmetics_div(void* sp, int vid1, int vid2, int vid3) {
    return static_cast<WSpace*>(sp)->cst_div(vid1, vid2, vid3);
}

/**
 Wraps the WSpace cst_mod method.
 */
void arithmetics_mod(void* sp, int vid1, int vid2, int vid3) {
    return static_cast<WSpace*>(sp)->cst_mod(vid1, vid2, vid3);
}

/**
 Wraps the WSpace cst_divmod method.
 */
void arithmetics_divmod(void* sp, int vid1, int vid2, int vid3, int vid4) {
    return static_cast<WSpace*>(sp)->cst_divmod(vid1, vid2, vid3, vid4);
}

/**
 Wraps the WSpace cst_min method.
 */
void arithmetics_min(void* sp, int vid1, int vid2, int vid3) {
    return static_cast<WSpace*>(sp)->cst_min(vid1, vid2, vid3);
}

/**
 Wraps the WSpace cst_arr_min method.
 */
void arithmetics_arr_min(void* sp, int n, int* vids, int vid) {
    return static_cast<WSpace*>(sp)->cst_arr_min(n, vids, vid);
}

/**
 Wraps the WSpace cst_argmin method.
 */
void arithmetics_argmin(void* sp, int n, int* vids, int vid) {
    return static_cast<WSpace*>(sp)->cst_argmin(n, vids, vid);
}

/**
 Wraps the WSpace cst_max method.
 */
void arithmetics_max(void* sp, int vid1, int vid2, int vid3) {
    return static_cast<WSpace*>(sp)->cst_max(vid1, vid2, vid3);
}

/**
 Wraps the WSpace cst_arr_max method.
 */
void arithmetics_arr_max(void* sp, int n, int* vids, int vid) {
    return static_cast<WSpace*>(sp)->cst_arr_max(n, vids, vid);
}

/**
 Wraps the WSpace cst_argmax method.
 */
void arithmetics_argmax(void* sp, int n, int* vids, int vid) {
    return static_cast<WSpace*>(sp)->cst_argmax(n, vids, vid);
}

/**
 Wraps the WSpace cst_mult method.
 */
void arithmetics_mult(void* sp, int vid1, int vid2, int vid3) {
    return static_cast<WSpace*>(sp)->cst_mult(vid1, vid2, vid3);
}

/**
 Wraps the WSpace cst_sqr method.
 */
void arithmetics_sqr(void* sp, int vid1, int vid2) {
    return static_cast<WSpace*>(sp)->cst_sqr(vid1, vid2);
}

/**
 Wraps the WSpace cst_sqrt method.
 */
void arithmetics_sqrt(void* sp, int vid1, int vid2) {
    return static_cast<WSpace*>(sp)->cst_sqrt(vid1, vid2);
}

/**
 Wraps the WSpace cst_pow method.
 */
void arithmetics_pow(void* sp, int vid1, int n, int vid2) {
    return static_cast<WSpace*>(sp)->cst_pow(vid1, n, vid2);
}

/**
 Wraps the WSpace cst_nroot method.
 */
void arithmetics_nroot(void* sp, int vid1, int n, int vid2) {
    return static_cast<WSpace*>(sp)->cst_nroot(vid1, n, vid2);
}

/**
 Wraps the WSpace cst_dom method.
 */
void set_dom(void* sp, int vid, int n, int* d) {
    return static_cast<WSpace*>(sp)->cst_dom(vid, n, d);
}

/**
 Wraps the WSpace cst_member method.
 */
void set_member(void* sp, int n, int* vids, int vid) {
    return static_cast<WSpace*>(sp)->cst_member(n, vids, vid);
}

/**
 Wraps the WSpace cst_sum method.
 */
void rel_sum(void* sp, int vid, int n, int* vids) {
    return static_cast<WSpace*>(sp)->cst_sum(vid, n, vids);
}

/**
 Wraps the WSpace cst_count_val_val method.
 */
void count_val_val(void* sp, int n, int* vids, int val1, int rel_type, int val2) {
    return static_cast<WSpace*>(sp)->cst_count_val_val(n, vids, val1, rel_type, val2);
}

/**
 Wraps the WSpace cst_count_val_var method.
 */
void count_val_var(void* sp, int n, int* vids, int val, int rel_type, int vid) {
    return static_cast<WSpace*>(sp)->cst_count_val_var(n, vids, val, rel_type, vid);
}

/**
 Wraps the WSpace cst_count_var_val method.
 */
void count_var_val(void* sp, int n, int* vids, int vid, int rel_type, int val) {
    return static_cast<WSpace*>(sp)->cst_count_var_val(n, vids, vid, rel_type, val);
}

/**
 Wraps the WSpace cst_count_var_var method.
 */
void count_var_var(void* sp, int n, int* vids, int vid1, int rel_type, int vid2) {
    return static_cast<WSpace*>(sp)->cst_count_var_var(n, vids, vid1, rel_type, vid2);
}

/**
 Wraps the WSpace cst_count_var_set_val method.
 */
void count_var_set_val(void*sp, int n, int* vids, int s, int* set, int rel_type, int val){
    return static_cast<WSpace*>(sp)->cst_count_var_set_val(n, vids, s, set, rel_type, val);
}

/**
 Wraps the WSpace cst_nvalues method.
 */
void nvalues(void* sp, int n, int* vids, int rel_type, int vid) {
    return static_cast<WSpace*>(sp)->cst_nvalues(n, vids, rel_type, vid);
}

/**
 Wraps the WSpace cst_circuit method.
 */
void circuit(void* sp, int n, int* c, int* vids1, int* vids2, int vid) {
    return static_cast<WSpace*>(sp)->cst_circuit(n, c, vids1, vids2, vid);
}

/**
 Wraps the WSpace cst_precede method 
*/
void precede(void* sp, int n, int* vids, int s, int u){
    return static_cast<WSpace*>(sp)->cst_precede(n, vids, s, u);
}

/**
 Wraps the WSpace cst_boolop_val method.
 */
void val_boolop(void* sp, int vid1, int bool_op, int vid2, int val) {
    return static_cast<WSpace*>(sp)->cst_boolop_val(vid1, bool_op, vid2, val);
}

/**
 Wraps the WSpace cst_boolop_var method.
 */
void var_boolop(void* sp, int vid1, int bool_op, int vid2, int vid3) {
    return static_cast<WSpace*>(sp)->cst_boolop_var(vid1, bool_op, vid2, vid3);
}

/**
 Wraps the WSpace cst_boolrel_val method.
 */
void val_boolrel(void* sp, int vid, int rel_type, int val) {
    return static_cast<WSpace*>(sp)->cst_boolrel_val(vid, rel_type, val);
}

/**
 Wraps the WSpace cst_boolrel_var method.
 */
void var_boolrel(void* sp, int vid1, int rel_type, int vid2) {
    return static_cast<WSpace*>(sp)->cst_boolrel_var(vid1, rel_type, vid2);
}

/**
 Wraps the WSpace branch method.
 */
void branch(void* sp, int n, int* vids, int var_strategy, int val_strategy) {
    return static_cast<WSpace*>(sp)->branch(n, vids, var_strategy, val_strategy);
}

/**
 Wraps the WSpace branch_b method.
 */
void branch_b(void* sp, int n, int* vids, int var_strategy, int val_strategy) {
    return static_cast<WSpace*>(sp)->branch_b(n, vids, var_strategy, val_strategy);
}

/**
 Wraps the WSpace cost method.
 */
void cost(void* sp, int vid) {
    return static_cast<WSpace*>(sp)->cost(vid);
}

/**
 Wraps the WTimeStop constructor
 */
void* new_time_stop(int maxTime){
    return (void*) new WTimeStop(maxTime);
}

/**
 Wraps the WTimeStop reset method
 */
void reset_time_stop(void* tStop){
    WTimeStop* _tStop = static_cast<WTimeStop*>(tStop);
    _tStop->reset();
}

/**
 Wraps the WSearchOptions constructor.
 */
void* new_search_options(){
    return (void*) new WSearchOptions();
}

/**
 Wraps the WSearchOptions setNbThreads method.
 */
int set_nb_threads(void* sOpts, int nThreads){
    return static_cast<WSearchOptions*>(sOpts)->setNbThreads(nThreads);
}

/**
 Wraps the WSearchOptions setTimeStop method.
 Returns the options object passed as an argument as a void pointer
 */
void* set_time_stop(void* sOpts, void* tStop){
    WTimeStop* _tStop = static_cast<WTimeStop*>(tStop);
    WSearchOptions* _sOpts = static_cast<WSearchOptions*>(sOpts);
    _sOpts->setTimeStop(_tStop);
    return (void*) _sOpts;
}

//new version
/**
 Wraps the WbabEngine constructor.
 */
void* new_bab_engine(void* sp, void* opts) {
    WSpace* _sp = static_cast<WSpace*>(sp);
    WSearchOptions* _opts = static_cast<WSearchOptions*>(opts);
    return (void*) new WbabEngine(_sp, _opts->getOpts());
}

/**
 Wraps the WbabEngine next method.
 */
void* bab_next(void* se) {
    return (void*) static_cast<WbabEngine*>(se)->next();
}

/**
 Wraps the WdfsEngine stopped method.
 */
int bab_stopped(void* se){
    return static_cast<WbabEngine*>(se)->stopped();
}

/**
 Wraps the WdfsEngine constructor.
 */
void* new_dfs_engine(void* sp, void* opts) {
    WSpace* _sp = static_cast<WSpace*>(sp);
    WSearchOptions* _opts = static_cast<WSearchOptions*>(opts);
    return (void*) new WdfsEngine(_sp, _opts->getOpts());
}

/**
 Wraps the WdfsEngine next method.
 */
void* dfs_next(void* se) {
    return (void*) static_cast<WdfsEngine*>(se)->next();
}

/**
 Wraps the WdfsEngine stopped method.
 */
int dfs_stopped(void* se){
    return static_cast<WdfsEngine*>(se)->stopped();
}

/**
 Wraps the WSpace destructor.
 */
void release(void* sp) {
    delete static_cast<WSpace*>(sp);
}

/**
 Wraps the WSpace value method.
 */
int get_value(void* sp, int vid) {
    return static_cast<WSpace*>(sp)->value(vid);
}

/**
 Wraps the WSpace values method.
 */
int* get_values(void* sp, int n, int* vids) {
    return static_cast<WSpace*>(sp)->values(n, vids);
}

/**
 Wraps the WSpace print method.
 */
void print_vars(void* sp, int n, int* vids) {
    return static_cast<WSpace*>(sp)->print(n, vids);
}
