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
    myfile << set_vars.size() << endl;
    myfile.close();
*/

/**
 Default constructor
 */
WSpace::WSpace() {
    i_size = 0;
    b_size = 0;
    s_size = 0;
}

//======================
//= Variables from idx =
//======================

/**
 Return the IntVar contained in int_vars at index vid
 */
IntVar WSpace::get_int_var(int vid) {
    return int_vars.at(vid);
}

/**
 Return the BoolVar contained in bool_vars at index vid
 */
BoolVar WSpace::get_bool_var(int vid) {
    return bool_vars.at(vid);
}

/**
Return the SetVar contained in set_vars at index vid.
*/
SetVar WSpace::get_set_var(int vid){
    return set_vars.at(vid);
}

//====================
//= Args for methods =
//====================

/**
 Return an IntVarArgs of size n, containing the n IntVars contained in
 int_vars at indices vids.
 */
IntVarArgs WSpace::int_var_args(int n, int* vids) {
    IntVarArgs x(n);
    for(int i = 0; i < n; i++)
        x[i] = get_int_var(vids[i]);
    return x;
}

/**
 Return an BoolVarArgs of size n, containing the n BoolVars contained in
 bool_vars at indices vids.
 */
BoolVarArgs WSpace::bool_var_args(int n, int* vids) {
    BoolVarArgs x(n);
    for(int i = 0; i < n; i++)
        x[i] = get_bool_var(vids[i]);
    return x;
}

/**
 Return a SetVarArgs of size n, containing the n SetVars contained in
 set_vars at indices vids.
 */
SetVarArgs WSpace::set_var_args(int n, int* vids) {
    SetVarArgs x(n);
    for(int i = 0; i < n; i++)
        x[i] = get_set_var(vids[i]);
    return x;
}

/**
 Return an IntArgs of size n, containing the n values in vals
*/
IntArgs WSpace::int_args(int n, int* vals) {
    IntArgs c(n);
    for(int i = 0; i < n; i++)
        c[i] = vals[i];
    return c;
}

/**
 Return the expression int_rel(vid, val)
*/
BoolVar WSpace::bool_expr_val(int vid, int int_rel, int val) {
    switch(int_rel) {
        case B_EQ: return expr(*this, get_int_var(vid) == val);
        case B_NQ: return expr(*this, get_int_var(vid) != val);
        case B_LE: return expr(*this, get_int_var(vid) < val);
        case B_LQ: return expr(*this, get_int_var(vid) <= val);
        case B_GQ: return expr(*this, get_int_var(vid) >= val);
        case B_GR: return expr(*this, get_int_var(vid) > val);
        default: 
            cout << "Wrong expression type in BoolVar creation." << endl;
            return BoolVar();
    }
}

/**
 Return the expression int_rel(vid1, vid2)
*/    
BoolVar WSpace::bool_expr_var(int vid1, int int_rel, int vid2) {
    switch(int_rel) {
        case B_EQ: return expr(*this, get_int_var(vid1) == get_int_var(vid2));
        case B_NQ: return expr(*this, get_int_var(vid1) != get_int_var(vid2));
        case B_LE: return expr(*this, get_int_var(vid1) < get_int_var(vid2));
        case B_LQ: return expr(*this, get_int_var(vid1) <= get_int_var(vid2));
        case B_GQ: return expr(*this, get_int_var(vid1) >= get_int_var(vid2));
        case B_GR: return expr(*this, get_int_var(vid1) > get_int_var(vid2));
        default: 
            cout << "Wrong expression type in BoolVar creation." << endl;
            return BoolVar();
    }
}


//=========================
//= Variables and domains =
//=========================

/**
 Add an IntVar to the WSpace ranging from min to max.
 In practice, push a new IntVar at the end of the vector int_vars.
 Return the index of the IntVar in int_vars
 */
int WSpace::add_intVar(int min, int max) {
    int_vars.push_back(IntVar(*this, min, max));
    return i_size++;
}

/**
 Add an IntVar to the WSpace with domain dom of size s.
 In practice, push a new IntVar at the end of the vector int_vars.
 Return the index of the IntVar in int_vars
 */
int WSpace::add_intVarWithDom(int s, int* dom) {
    int_vars.push_back(IntVar(*this, IntSet(dom, s)));
    return i_size++;
}

/**
 Add n IntVars to the WSpace ranging from min to max.
 In practice, push n new IntVars at the end of the vector int_vars.
 Return the indices of the IntVars in int_vars.
 */
int* WSpace::add_intVarArray(int n, int min, int max) {
    int* vids = new int[n];
    for(int i = 0; i < n; i++)
        vids[i] = this->add_intVar(min, max);
    return vids;
}

/**
 Add n IntVars to the WSpace with domain dom of size s.
 In practice, push n new IntVars at the end of the vector int_vars.
 Return the indices of the IntVars in int_vars.
 */
int* WSpace::add_intVarArrayWithDom(int n, int s, int* dom) {
    int* vids = new int[n];
    for(int i = 0; i < n; i++)
        vids[i] = this->add_intVarWithDom(s, dom);
    return vids;
}

/**
 Define which variables are to be the solution so they can be accessed to add a constraint with bab
 */
int* WSpace::set_as_solution_variables(int n, int* vids){
    solution_variable_indexes = vids;
    return solution_variable_indexes;
}

/**
 Return the number of IntVars in the space.
 */
int WSpace::nvars() {
    return i_size;
}

/**
 Add a BoolVar to the WSpace ranging from min to max.
 In practice, push a new BoolVar at the end of the vector bool_vars.
 Return the index of the BoolVar in bool_vars
 */
int WSpace::add_boolVar(int min, int max) {
    bool_vars.push_back(BoolVar(*this, min, max));
    return b_size++;
}

/**
 Add a BoolVar to the WSpace corresponding to the evaluation of int_rel(vid, val).
 In practice, push a new BoolVar at the end of the vector bool_vars.
 Return the index of the BoolVar in bool_vars
 */
int WSpace::add_boolVar_expr_val(int vid, int int_rel, int val) {
    bool_vars.push_back(bool_expr_val(vid, int_rel, val));
    return b_size++;
}

/**
 Add a BoolVar to the WSpace corresponding to the evaluation of int_rel(vid1, vid2).
 In practice, push a new BoolVar at the end of the vector bool_vars.
 Return the index of the BoolVar in bool_vars
 */
int WSpace::add_boolVar_expr_var(int vid1, int int_rel, int vid2) {
    bool_vars.push_back(bool_expr_var(vid1, int_rel, vid2));
    return b_size++;
}

/**
Add a SetVar to the WSpace initialized with n integer from array r.
In practice, push a new SetVar at the end of the vector set_vars.
Return the index of the SetVar in set_vars.
*/
int WSpace::add_setVar(int card_min, int card_max) {
    set_vars.push_back(SetVar(*this,IntSet::empty, IntSet(0, 127), card_min, card_max));
    return s_size++;
}

/**
 Add n SetVars to the WSpace ranging with cardinality card_min to card_max.
 In practice, push n new SetVars at the end of the vector set_vars.
 Return the indices of the SetVars in set_vars.
 */
int* WSpace::add_setVarArray(int n, int card_min, int card_max) {
    int* vids = new int[n];
    for(int i = 0; i < n; i++)
        vids[i] = this->add_setVar(card_min, card_max);
    return vids;
}


//=======================
//= Posting constraints =
//=======================

//=== INTVAR ===

/**
 Post a relation constraint between the IntVar denoted by vid and the val.
 */
void WSpace::cst_val_rel(int vid, int rel_type, int val) {
    rel(*this, get_int_var(vid), (IntRelType) rel_type, val);
}

/**
 Post a relation constraint between the IntVars denoted by vid1 and vid2.
*/
void WSpace::cst_var_rel(int vid1, int rel_type, int vid2) {
    rel(*this, get_int_var(vid1), (IntRelType) rel_type, get_int_var(vid2));
}

/**
 Post a relation constraint between the n IntVars denoted by vids and the val.
*/
void WSpace::cst_arr_val_rel(int n, int* vids, int rel_type, int val) {
    rel(*this, int_var_args(n, vids), (IntRelType) rel_type, val);
}

/**
 Post a relation constraint between the n IntVars denoted by vids and the the IntVar vid.
*/
void WSpace::cst_arr_var_rel(int n, int* vids, int rel_type, int vid) {
    rel(*this, int_var_args(n, vids), (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post a relation constraint between the n IntVars denoted by vids.
*/
void WSpace::cst_arr_rel(int n, int* vids, int rel_type) {
    rel(*this, int_var_args(n, vids), (IntRelType) rel_type);
}

/**
 Post a lexicographic relation constraint between the n1 IntVars denoted by vids1 and
 the n2 IntVars denoted by vids2.
*/
void WSpace::cst_arr_arr_rel(int n1, int* vids1, int rel_type, int n2, int* vids2) {
    rel(*this, int_var_args(n1, vids1), (IntRelType) rel_type, int_var_args(n2, vids2));
}

/**
 Post the constraint that all IntVars denoted by vids are distinct
 */
void WSpace::cst_distinct(int n, int* vids) {
    distinct(*this, int_var_args(n, vids));
}

/**
 Post the linear constraint [c]*[vids] rel_type val.
 */
void WSpace::cst_val_linear(int n, int* c, int* vids, int rel_type, int val) {
    linear(*this, int_args(n, c), int_var_args(n, vids), (IntRelType) rel_type, val);
}

/**
 Post the linear constraint [c]*[vids] rel_type vid.
*/
void WSpace::cst_var_linear(int n, int* c, int* vids, int rel_type, int vid) {
    linear(*this, int_args(n, c), int_var_args(n, vids), (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post the constraint that |vid1| = vid2.
 */
void WSpace::cst_abs(int vid1, int vid2) {
    abs(*this, get_int_var(vid1), get_int_var(vid2));
}

/**
 Post the constaraint that dom(vid) = d.
 */
void WSpace::cst_dom(int vid, int n, int* d) {
    dom(*this, get_int_var(vid), IntSet(d, n));
}

/**
 Post the constraint that vid is included in {vids[0], ..., vids[n-1]}
*/
void WSpace::cst_member(int n, int* vids, int vid) {
    member(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid1 / vid2 = vid3.
 */
void WSpace::cst_div(int vid1, int vid2, int vid3) {
    div(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid1 % vid2 = vid3.
 */
void WSpace::cst_mod(int vid1, int vid2, int vid3) {
    mod(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid1 / vid2 = vid3 
 and vid1 % vid2 = div4
 */
void WSpace::cst_divmod(int vid1, int vid2, int vid3, int vid4) {
    divmod(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3), get_int_var(vid4));
}

/**
 Post the constraint that min(vid1, vid2) = vid3.
 */
void WSpace::cst_min(int vid1, int vid2, int vid3) {
    Gecode::min(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid = min(vids).
 */
void WSpace::cst_arr_min(int n, int* vids, int vid) {
    Gecode::min(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid = argmin(vids).
 */
void WSpace::cst_argmin(int n, int* vids, int vid) {
    Gecode::argmin(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that max(vid1, vid2) = vid3.
 */
void WSpace::cst_max(int vid1, int vid2, int vid3) {
    Gecode::max(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid = max(vids).
 */
void WSpace::cst_arr_max(int n, int* vids, int vid) {// corrigé
    Gecode::max(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid = argmax(vids).
 */
void WSpace::cst_argmax(int n, int* vids, int vid) {
    Gecode::argmax(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid1 * vid2 = vid3.
 */
void WSpace::cst_mult(int vid1, int vid2, int vid3) {
    mult(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that sqr(vid1) = vid2.
 */
void WSpace::cst_sqr(int vid1, int vid2) {
    sqr(*this, get_int_var(vid1), get_int_var(vid2));
}

/**
 Post the constraint that sqrt(vid1) = vid2.
 */
void WSpace::cst_sqrt(int vid1, int vid2) {
    Gecode::sqrt(*this, get_int_var(vid1), get_int_var(vid2));
}

/**
 Post the constraint that pow(vid1, n) = vid2.
 */
void WSpace::cst_pow(int vid1, int n, int vid2) {
    Gecode::pow(*this, get_int_var(vid1), n, get_int_var(vid2));
}   

/**
 Post the constraint that nroot(vid1, n) = vid2.
 */
void WSpace::cst_nroot(int vid1, int n, int vid2) {
    nroot(*this, get_int_var(vid1), n, get_int_var(vid2));
}

/**
 Post the constraint that vid = sum(vids).
 */
void WSpace::cst_sum(int vid, int n, int* vids) {
    rel(*this, get_int_var(vid), IRT_EQ, expr(*this, sum(int_var_args(n, vids))));
}

/**
 Post the constraint that the number of variables in vids equal to val1 has relation rel_type 
 with val2.
 */
void WSpace::cst_count_val_val(int n, int* vids, int val1, int rel_type, int val2) {
    count(*this, int_var_args(n, vids), val1, (IntRelType) rel_type, val2);
}

/**
 Post the constraint that the number of variables in vids equal to val has relation rel_type 
 with vid.
 */
void WSpace::cst_count_val_var(int n, int* vids, int val, int rel_type, int vid) {
    count(*this, int_var_args(n, vids), val, (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post the constraint that the number of variables in vids equal to vid has relation rel_type 
 with val.
 */
void WSpace::cst_count_var_val(int n, int* vids, int vid, int rel_type, int val) {// corrigé
    count(*this, int_var_args(n, vids), get_int_var(vid), (IntRelType) rel_type, val);
}

/**
 Post the constraint that the number of variables in vids equal to vid1 has relation rel_type 
 with vid2.
 */
void WSpace::cst_count_var_var(int n, int* vids, int vid1, int rel_type, int vid2) {
    count(*this, int_var_args(n, vids), vid1, (IntRelType) rel_type, get_int_var(vid2));
}

/**
 Post the constraint that the number of variables in vids in the set set has relation rel_type with val
 */
void WSpace::cst_count_var_set_val(int n, int*vids, int s, int* set, int rel_type, int val){// ajouté
    count(*this, int_var_args(n, vids), IntSet(set, s), (IntRelType) rel_type, val);
}

/** 
 Post the constraint that the number of variables in vids where vars[i] = c[i] and c is an array of integers has rel_type to val
 */
void WSpace::cst_count_array_val(int n, int*vids, int* c, int rel_type, int val){
    count(*this, int_var_args(n, vids), int_args(n, c), (IntRelType) rel_type, val);
}

/**
 Post the constraint that the number of distinct values in the n variables denoted by vids
 has the given rel_type relation with the variable vid.
 */
void WSpace::cst_nvalues(int n, int* vids, int rel_type, int vid) {
    nvalues(*this, int_var_args(n, vids), (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post the constraint that values of vids1 are the edges of an hamiltonian circuit in 
 the graph formed by the n variables in vids1, vids2 are the costs of these edges described
 by c, and vid is the total cost of the circuit, i.e. sum(vids2).
 */
void WSpace::cst_circuit(int n, int* c, int* vids1, int* vids2, int vid) {
    circuit(*this, int_args(n*n, c), int_var_args(n, vids1), int_var_args(n, vids2), get_int_var(vid));
}

/**
 Post the constraint that if there exists j (0 ≤ j < |x|) such that x[j] = u, 
 then there must exist i with i < j such that x[i] = s
*/ 
void WSpace::cst_precede(int n, int* vids, int s, int u){
    precede(*this, int_var_args(n, vids), s, u);
}


//=== BOOLVAR ===

/**
 Post the constraint that vid1 bool_op vid2 = val.
 */
void WSpace::cst_boolop_val(int vid1, int bool_op, int vid2, int val) {
    rel(*this, get_bool_var(vid1), (BoolOpType) bool_op, get_bool_var(vid2), val);
}

/**
 Post the constraint that vid1 bool_op vid2 = vid3.
 */
void WSpace::cst_boolop_var(int vid1, int bool_op, int vid2, int vid3) {
    rel(*this, get_bool_var(vid1), (BoolOpType) bool_op, get_bool_var(vid2), get_bool_var(vid3));
}

/**
 Post a relation constraint between vid and val.
 */
void WSpace::cst_boolrel_val(int vid, int rel_type, int val) {
    rel(*this, get_bool_var(vid), (IntRelType) rel_type, val);
}

/**
 Post a relation constraint between vid1 and vid2.
 */
void WSpace::cst_boolrel_var(int vid1, int rel_type, int vid2) {
    rel(*this, get_bool_var(vid1), (IntRelType) rel_type, get_bool_var(vid2));
}

//=== SETVAR ===

/**
 Post the constraint that vid1 set_op vid2 = vid3.
 */
void WSpace::cst_setop_var(int vid1, int set_op, int vid2, int set_rel, int vid3) {
    rel(*this, get_set_var(vid1), (SetOpType) set_op, get_set_var(vid2), (SetRelType) set_rel, get_set_var(vid3));
}

/**
 Post a relation constraint between vid1 and vid2.
 */
void WSpace::cst_setrel_var(int vid1, int rel_type, int vid2) {
    rel(*this, get_set_var(vid1), (SetRelType) rel_type, get_set_var(vid2));
}

/**
 Post a relation constraint between vid1 and domain dom.
 */
void WSpace::cst_setrel_val(int vid1, int rel_type, int* domain, int s) {
    dom(*this, get_set_var(vid1), (SetRelType) rel_type, IntSet(domain, s));
}

/**
 Post a constraint that SetVar vid1 has to be empty
 */
void WSpace::cst_set_empty(int vid1) {
    dom(*this, get_set_var(vid1), (SetRelType) 0, IntSet::empty);
}

/**
 Post a dom constraint between vid1 and dom {i,..., j}.
 */
void WSpace::cst_setdom_ints(int vid1, int rel_type, int i, int j) {
    dom(*this, get_set_var(vid1), (SetRelType) rel_type, i, j);
}

/**
 Post a cardinality constraint on vid1
 */
void WSpace::cst_card_var(int n, int* vids, int min_card, int max_card) {
    cardinality(*this, set_var_args(n, vids), min_card, max_card);
}

//======================================
//Branch and bound constraint function =
//======================================

/*
 Constrain method for BAB search
 This is called everytime the solver finds a solution
 This is a virtual method as declared in space_wrapper.h
*/
void WSpace::constrain(const Space& _b) {
    // right now, do nothing
    //const WSpace& b = static_cast<const WSpace&>(_b);// cast the space
    //IntVarArgs vars(int_var_args(var_sol_size, solution_variable_indexes));
    //for(int i = 0; i < var_sol_size; ++i){
    //    rel(*this, vars[i], IRT_NQ, 2);
    //    b.vars[i].val()
    //}
}

//==========================
//= Exploration strategies =
//==========================

/**
 Post a branching strategy on the variables in vids, with strategies denoted by var_strategy and
 val_strategy.
 var_strategy:
    - 0 : INT_VAR_SIZE_MIN()
    - 1 : INT_VAR_RND(r)
    - 2 : INT_VAR_DEGREE_MAX()
 val_strategy:
    - 0 : INT_VAL_MIN()
    - 1 : INT_VAL_RND(r)
    - 2 : INT_VAL_SPLIT_MIN()
    - 3 : INT_VAL_SPLIT_MAX()
    - 4 : INT_VAL_MED()
 */
void WSpace::branch(int n, int* vids, int var_strategy, int val_strategy) {
    IntVarBranch var_strat;
    IntValBranch val_strat;

    Rnd r1(1U);
    Rnd r2(3U);

    //determine the variable strategy
    if(var_strategy == 0){//INT_VAR_SIZE_MIN()
        var_strat = INT_VAR_SIZE_MIN();
    }
    else if(var_strategy == 1){//INT_VAR_RND(r1)
        var_strat = INT_VAR_RND(r1);
    }
    else if(var_strategy == 2){//INT_VAR_DEGREE_MAX()
        var_strat = INT_VAR_DEGREE_MAX();
    }

    //determine the value strategy
    if(val_strategy == 0){//INT_VAL_MIN()
        val_strat = INT_VAL_MIN();
    }
    else if(val_strategy == 1){//INT_VAL_RND(r2)
        val_strat = INT_VAL_RND(r2);
    }
    else if(val_strategy == 2){//INT_VAL_SPLIT_MIN()
        val_strat = INT_VAL_SPLIT_MIN();
    }
    else if(val_strategy == 3){//INT_VAL_SPLIT_MAX()
        val_strat = INT_VAL_SPLIT_MAX();
    }
    else if(val_strategy == 4){//INT_VAL_MED()
        val_strat = INT_VAL_MED();
    }

    Gecode::branch(*this, int_var_args(n, vids), var_strat, val_strat);
}


/**
 Post a branching strategy on the n BoolVars in vids, with strategies denoted by var_strategy and
 val_strategy.
 */
void WSpace::branch_b(int n, int* vids, int var_strategy, int val_strategy) {
    Gecode::branch(*this, bool_var_args(n, vids), BOOL_VAR_NONE(), BOOL_VAL_MIN()); //default for now
} 

/**
 Post a branching strategy on the n SetVars in vids.
 */
void WSpace::branch_set(int n, int* vids) {
    Rnd r2(3U);
    Gecode::branch(*this, set_var_args(n, vids), SET_VAL_RND_INC(r2)); //default for now
} 

//==================
//= Search support =
//==================

/**
 Define which variable, denoted by vid, will be considered as the cost.
 */
void WSpace::cost(int vid) {
    cost_id = vid;
}

IntVar WSpace::cost(void) const {
    return int_vars.at(cost_id);
}

WSpace::WSpace(WSpace& s): IntMinimizeSpace(s), int_vars(s.i_size), bool_vars(s.b_size), set_vars(s.s_size), i_size(s.i_size), b_size(s.b_size), s_size(s.s_size), cost_id(s.cost_id) {
    //IntVars update
    vector<IntVar>::iterator itd, its;
    for(itd = int_vars.begin(), its = s.int_vars.begin(); itd != int_vars.end(); ++itd, ++its)
        itd->update(*this, *its);
    
    //BoolVars update
    vector<BoolVar>::iterator btd, bts;
    for(btd = bool_vars.begin(), bts = s.bool_vars.begin(); btd != bool_vars.end(); ++btd, ++bts)
        btd->update(*this, *bts);
    
    //SetVars update
    vector<SetVar>::iterator std, sts;
    for(std = set_vars.begin(), sts = s.set_vars.begin(); std != set_vars.end(); ++std, ++sts)
        std->update(*this, *sts);
}

Space* WSpace::copy(void) {
    return new WSpace(*this);
}

//=====================
//= Getting solutions =
//=====================

/**
 Return the current values of the variable denoted by vid.
 */
int WSpace::value(int vid) {
    return get_int_var(vid).val();
}

/**
 Return the current values of the variable denoted by vid.
 */
int* WSpace::value_set(int vid, int n) {
    SetVar sv = get_set_var(vid);
    int* vals = new int[n] ;
    int i = 0 ;
    for (SetVarGlbValues d(sv);d();++d){
        vals[i] = d.val() ;
        i++ ;
    }
    return vals;
}

int WSpace::value_size(int vid) {
    return get_set_var(vid).glbSize() ;
}

/**
 Return the current values of the n variables denoted by vids.
 */
int* WSpace::values(int n, int* vids) {
    int* vals = new int[n];
    for(int i = 0; i < n; i++)
        vals[i] = get_int_var(vids[i]).val();
    return vals;
}

//======================
//= Printing solutions =
//======================

void WSpace::print(int n, int* vids) {
    std::cout << "{";
    for(int i = 0; i < n; i++) {
        std::cout << get_int_var(vids[i]);
        if(i < n - 1) std::cout << ", ";
    }
    std::cout << "}" << std::endl;
}

//============================
//= Search options managment =
//============================

// === TIME STOP OBJECT ===

/**
 Default constructor
 */
WTimeStop::WTimeStop(int maxTime) : stop(Gecode::Search::TimeStop(maxTime)) {
    stop_ptr = &stop;
}

WTimeStop::~WTimeStop(){

}

TimeStop WTimeStop::getStop(){
    return stop;
}

TimeStop* WTimeStop::getStopPtr(){
    return stop_ptr;
}

/**
 Reset the time value of the time stop object
 */
void WTimeStop::reset(){
    stop.reset();
}

// === OPTIONS OBJECT ===

/**
 Default constructor
 */
WSearchOptions::WSearchOptions(){
    
}

WSearchOptions::~WSearchOptions(){

}

/**
 getter for the opts field
 */
Options WSearchOptions::getOpts(){
    return opts;
}
/**
 set the number of threads to use for parallel search
 */
int WSearchOptions::setNbThreads(int nThreads){
    opts.threads = nThreads;
    return opts.threads;
}

/**
 Set the time stopping mechanism that is to be used during the search to a certain duration in ms
 Takes a WTimeStop object as argument, and sets the WSearchOptions object's opts.stop field to the TimeStop pointer of the WTimeStop object
 */
void WSearchOptions::setTimeStop(WTimeStop* timestop){
    opts.stop = timestop->getStopPtr();
}

//=================
//= Search engine =
//=================

/*
 Branch and bound
 */
WbabEngine::WbabEngine(WSpace* sp, Options opts) {
    bab = new BAB<WSpace>(sp, opts);
}

WbabEngine::~WbabEngine() {
    delete bab;
}

/**
 Search the next solution for this search engine.
 */
WSpace* WbabEngine::next() {
    return bab->next();
}

/**
 Returns true if the search has been stopped by a search object
 */
int WbabEngine::stopped(){
    return bab->stopped();
}

/*
 Depth-first search
*/
WdfsEngine::WdfsEngine(WSpace* sp, Options opts) {
    dfs = new DFS<WSpace>(sp, opts);
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



