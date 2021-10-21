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
using namespace std;
using namespace Gecode::Search;

class WSpace: public IntMinimizeSpace {
protected:
    vector<IntVar> int_vars;
    vector<BoolVar> bool_vars;
    int i_size;
    int b_size;
    int cost_id;
    
    //======================
    //= Variables from idx =
    //======================
    
    /**
     Return the IntVar contained in int_vars at index vid.
     */
    IntVar get_int_var(int vid);

    /**
     Return the BoolVar contained in bool_vars at index vid.
     */
    BoolVar get_bool_var(int vid);

    
    //====================
    //= Args for methods =
    //====================
    
    /**
     Return an IntVarArgs of size n, containing the n IntVars contained in
     int_vars at indices vids.
     */
    IntVarArgs int_var_args(int n, int* vids);

    /**
     Return a BoolVarArgs of size n, containing the n BoolVars contained in
     bool_vars at indices vids.
     */
    BoolVarArgs bool_var_args(int n, int* vids);
    
    /**
     Return an IntArgs of size n, containing the n values in vals
    */
    IntArgs int_args(int n, int* vals);

    /**
     Return the expression int_rel(vid, val)
    */
    BoolVar bool_expr_val(int vid, int int_rel, int val);

    /**
     Return the expression int_rel(vid1, vid2)
    */    
    BoolVar bool_expr_var(int vid1, int int_rel, int vid2);
    
public:
    /**
     Default constructor
     */
    WSpace();
    
    //=========================
    //= Variables and domains =
    //=========================
    
    /**
     Add an IntVar to the WSpace ranging from min to max.
     In practice, push a new IntVar at the end of the vector int_vars.
     Return the index of the IntVar in int_vars
     */
    int add_intVar(int min, int max);
    
    /**
     Add an IntVar to the WSpace with domain dom of size s.
     In practice, push a new IntVar at the end of the vector int_vars.
     Return the index of the IntVar in int_vars
     */
    int add_intVarWithDom(int s, int* dom);
    
    /**
     Add n IntVars to the WSpace ranging from min to max.
     In practice, push n new IntVars at the end of the vector int_vars.
     Return the indices of the IntVars in int_vars.
     */
    int* add_intVarArray(int n, int min, int max);
    
    /**
     Add n IntVars to the WSpace with domain dom of size s.
     In practice, push n new IntVars at the end of the vector int_vars.
     Return the indices of the IntVars in int_vars.
     */
    int* add_intVarArrayWithDom(int n, int s, int* dom);
    
    /**
     Return the number of IntVars in the space.
     */
    int nvars();

    enum {
        //Relations for BoolExpr
        B_EQ,
        B_NQ,
        B_LE,
        B_LQ,
        B_GQ,
        B_GR
    };

    /**
     Add a BoolVar to the WSpace ranging from min to max.
     In practice, push a new BoolVar at the end of the vector bool_vars.
     Return the index of the BoolVar in bool_vars
     */
    int add_boolVar(int min, int max);

    /**
     Add a BoolVar to the WSpace corresponding to the evaluation of int_rel(vid, val).
     In practice, push a new BoolVar at the end of the vector bool_vars.
     Return the index of the BoolVar in bool_vars
     */
    int add_boolVar_expr_val(int vid, int int_rel, int val);

    /**
     Add a BoolVar to the WSpace corresponding to the evaluation of int_rel(vid1, vid2).
     In practice, push a new BoolVar at the end of the vector bool_vars.
     Return the index of the BoolVar in bool_vars
     */
    int add_boolVar_expr_var(int vid1, int int_rel, int vid2);
    
    //=======================
    //= Posting constraints =
    //=======================
    
    //=== INTVARS ===

    /**
     Post a relation constraint between the IntVar denoted by vid and the val.
     */
    void cst_val_rel(int vid, int rel_type, int val);
    
    /**
     Post a relation constraint between the IntVars denoted by vid1 and vid2.
    */
    void cst_var_rel(int vid1, int rel_type, int vid2);

    /**
     Post a relation constraint between the n IntVars denoted by vids and the val.
    */
    void cst_arr_val_rel(int n, int* vids, int rel_type, int val);

    /**
     Post a relation constraint between the n IntVars denoted by vids and the the IntVar vid.
    */
    void cst_arr_var_rel(int n, int* vids, int rel_type, int vid);

    /**
     Post a relation constraint between the n IntVars denoted by vids.
    */
    void cst_arr_rel(int n, int* vids, int rel_type);
    
    /**
     Post a lexicographic relation constraint between the n1 IntVars denoted by vids1 and
     the n2 IntVars denoted by vids2.
    */
    void cst_arr_arr_rel(int n1, int* vids1, int rel_type, int n2, int* vids2);

    /**
     Post the constraint that the n IntVars denoted by vids are distinct
     */
    void cst_distinct(int n, int* vids);
    
    /**
     Post the linear constraint [c]*[vids] rel val.
     */
    void cst_val_linear(int n, int* c, int* vids, int rel_type, int val);
    
    /**
     Post the linear constraint [c]*[vids] rel_type vid.
    */
    void cst_var_linear(int n, int* c, int* vids, int rel_type, int vid);
    
    /**
     Post the constraint that |vid1| = vid2.
     */
    void cst_abs(int vid1, int vid2);
    
    /**
     Post the constraint that dom(vid) = d, where d is a set of size n.
     */
    void cst_dom(int vid, int n, int* d);

    /**
     Post the constraint that vid is included in {vids[0], ..., vids[n-1]}
     */
    void cst_member(int n, int* vids, int vid);
    
    /**
     Post the constraint that vid1 / vid2 = vid3.
     */
    void cst_div(int vid1, int vid2, int vid3); 
    
    /**
     Post the constraint that vid1 % vid2 = vid3.
     */
    void cst_mod(int vid1, int vid2, int vid3);

    /**
     Post the constraint that vid1 / vid2 = vid3 
     and vid1 % vid2 = div4
     */
    void cst_divmod(int vid1, int vid2, int vid3, int vid4);

    /**
     Post the constraint that min(vid1, vid2) = vid3.
     */
    void cst_min(int vid1, int vid2, int vid3);  

    /**
     Post the constraint that vid = min(vids).
     */
    void cst_arr_min(int n, int* vids, int vid);

    /**
     Post the constraint that vid = argmin(vids).
     */
    void cst_argmin(int n, int* vids, int vid);

    /**
     Post the constraint that max(vid1, vid2) = vid3.
     */
    void cst_max(int vid1, int vid2, int vid3); 

    /**
     Post the constraint that vid = max(vids).
     */
    void cst_arr_max(int n, int* vids, int vid);

    /**
     Post the constraint that vid = argmax(vids).
     */
    void cst_argmax(int n, int* vids, int vid); 

    /**
     Post the constraint that vid1 * vid2 = vid3.
     */
    void cst_mult(int vid1, int vid2, int vid3);

    /**
     Post the constraint that sqr(vid1) = vid2.
     */
    void cst_sqr(int vid1, int vid2);
    
    /**
     Post the constraint that sqrt(vid1) = vid2.
     */
    void cst_sqrt(int vid1, int vid2);

    /**
     Post the constraint that pow(vid1, n) = vid2.
     */
    void cst_pow(int vid1, int n, int vid2);   

    /**
     Post the constraint that nroot(vid1, n) = vid2.
     */
    void cst_nroot(int vid1, int n, int vid2);    
    
    /**
     Post the constraint that vid = sum(vids).
     */
    void cst_sum(int vid, int n, int* vids);

    /**
     Post the constraint that the number of variables in vids equal to val1 has relation rel_type 
     with val2.
     */
    void cst_count_val_val(int n, int* vids, int val1, int rel_type, int val2);

    /**
     Post the constraint that the number of variables in vids equal to val has relation rel_type 
     with vid.
     */
    void cst_count_val_var(int n, int* vids, int val, int rel_type, int vid);

    /**
     Post the constraint that the number of variables in vids equal to vid has relation rel_type 
     with val.
     */
    void cst_count_var_val(int n, int* vids, int vid, int rel_type, int val);

    /**
     Post the constraint that the number of variables in vids equal to vid1 has relation rel_type 
     with vid2.
     */
    void cst_count_var_var(int n, int* vids, int vid1, int rel_type, int vid2);

    /**
     Post the constraint the number of distinct values in the n variables denoted by vids
     has the given rel_type relation with the variable vid.
     */
    void cst_nvalues(int n, int* vids, int rel_type, int vid);

    /**
     Post the constraint that values of vids1 are the edges of an hamiltonian circuit in 
     the graph formed by the n variables in vids1, vids2 are the costs of these edges described
     by c, and vid is the total cost of the circuit, i.e. sum(vids2).
     */
    void cst_circuit(int n, int* c, int* vids1, int* vids2, int vid);

    /**
     Post the constraint that if there exists j (0 ≤ j < |x|) such thatx[j] = t, 
     then there must exist i with i < j such that x[i] = s
    */ 
    void cst_precede(int n, int* vids, int t, int u);

    //=== BOOLVARS ===

    /**
     Post the constraint that vid1 bool_op vid2 = val.
     */
    void cst_boolop_val(int vid1, int bool_op, int vid2, int val);

    /**
     Post the constraint that vid1 bool_op vid2 = vid3.
     */
    void cst_boolop_var(int vid1, int bool_op, int vid2, int vid3);

    /**
     Post a relation constraint between vid and val.
     */
    void cst_boolrel_val(int vid, int rel_type, int val);

    /**
     Post a relation constraint between vid1 and vid2.
     */
    void cst_boolrel_var(int vid1, int rel_type, int vid2);
    
    //==========================
    //= Exploration strategies =
    //==========================
    
    /**
     Post a branching strategy on the n IntVars in vids, with strategies denoted by var_strategy and
     val_strategy.
     */
    void branch(int n, int* vids, int var_strategy, int val_strategy);

    /**
     Post a branching strategy on the n BoolVars in vids, with strategies denoted by var_strategy and
     val_strategy.
     */
    void branch_b(int n, int* vids, int var_strategy, int val_strategy);   
    
    //==================
    //= Search support =
    //==================
    
    void cost(int vid);
    
    virtual IntVar cost(void) const;
    
    WSpace(WSpace& s);
    
    virtual Space* copy(void);

    //=====================
    //= Getting solutions =
    //=====================
    
    /**
     Return the current values of the variable denoted by vid.
     */
    int value(int vid);

    /**
     Return the current values of the n variables denoted by vids.
     */
    int* values(int n, int* vids);
    
    //======================
    //= Printing solutions =
    //======================
    
    /**
     Print the n variables denoted by vids.
     */
    void print(int n, int* vids);
};

//==================
//= Search options =
//==================

class WTimeStop {
    protected:
    Gecode::Search::TimeStop stop;
    Gecode::Search::TimeStop* stop_ptr;

    public:
    WTimeStop(int maxTime);
    ~WTimeStop();

    void reset();
    TimeStop getStop();
    TimeStop* getStopPtr();
};

class WSearchOptions {
    protected:
        Gecode::Search::Options opts;

    public:
    WSearchOptions();
    ~WSearchOptions();

    /**
     getter for the opts field
    */
    Options getOpts();

    /**
     Different functions to add options
     */

    /**
     set the number of threads to use for parallel search
     */
    int setNbThreads(int nThreads);

    /**
    Set the time stopping mechanism that is to be used during the search to a certain duration in ms
    */ 
    void setTimeStop(WTimeStop* timestop);
};



//=================
//= Search engine =
//=================

class WbabEngine {
protected:
    BAB<WSpace>* bab;
public:
    WbabEngine(WSpace* sp, Options opts);
    ~WbabEngine();
    
    /**
     Search the next solution for this search engine.
     */
    WSpace* next();
};

class WdfsEngine {
protected:
    DFS<WSpace>* dfs;
public:
    WdfsEngine(WSpace* sp, Options opts);
    ~WdfsEngine();

    /**
     Search the next solution for this search engine.
     */
    WSpace* next();
};

#endif
