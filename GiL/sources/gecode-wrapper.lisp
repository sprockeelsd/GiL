(cl:defpackage "gil"
  (:nicknames "GIL")
   (:use common-lisp :cl-user :cl :cffi))

(in-package :gil)

(cffi::defcfun ("computation_space" new-space) :pointer
    "Create a new computation space."
)

(cffi::defcfun ("add_intVar" add-int-var-low) :int
    "Add an IntVar ranging from min to max to the specified space. Return the reference of this variable for this space."
    (sp :pointer)
    (min :int)
    (max :int)
)

(cffi::defcfun ("add_intVarWithDom" add-int-var-dom-aux) :int
    "Add an IntVar with domain dom of size s to the specified space. Return the reference of this variable for this space."
    (sp :pointer)
    (s :int)
    (dom :pointer)
)

(defun add-int-var-dom-low (sp dom)
    "Add an IntVar with domain dom to the specified space. Return the reference of this variable for this space."
    (let ((x (cffi::foreign-alloc :int :initial-contents dom)))
        (add-int-var-dom-aux sp (length dom) x))
)

(cffi::defcfun ("add_intVarArray" add-int-var-array-aux) :pointer
    "Add n IntVar ranging from min to max to the specified space."
    (sp :pointer)
    (n :int)
    (min :int)
    (max :int)
)

(defun add-int-var-array-low (sp n min max)
    "Add n IntVar ranging from min to max to the specified space. Return the references of those variables for this space"
    (let ((p (add-int-var-array-aux sp n min max)))
        (loop for i from 0 below n 
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("add_intVarArrayWithDom" add-int-var-array-dom-aux) :pointer
    "Add n IntVar with domain dom of size s to the specified space."
    (sp :pointer)
    (n :int)
    (s :int)
    (dom :pointer)
)

(defun add-int-var-array-dom-low (sp n dom)
    "Add n IntVar with domain dom to the specified space. Return the references of those variables for this space"
    (let ((x (cffi::foreign-alloc :int :initial-contents dom))
           p)
        (setq p (add-int-var-array-dom-aux sp n (length dom) x))
        (loop for i from 0 below n 
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("nvars" nvars) :int
    "Return the number of variables in the space."
    (sp :pointer)
)

;IntVar relation flags
(defparameter gil::IRT_EQ 0)    ; equality relation
(defparameter gil::IRT_NQ 1)    ; inequality
(defparameter gil::IRT_LQ 2)    ; Less or equal
(defparameter gil::IRT_LE 3)    ; Strictly lower 
(defparameter gil::IRT_GQ 4)    ; Greater or equal
(defparameter gil::IRT_GR 5)    ; Strictly greater

(cffi::defcfun ("add_boolVar" add-bool-var-range) :int
    "Add a BoolVar ranging from l to h. Return the index to this BoolVar."
    (sp :pointer)
    (l :int)
    (h :int)
)

(cffi::defcfun ("add_boolVar_expr_val" add-bool-var-expr-val) :int
    "Add a BoolVar corresponding to the evalueation of rel-type(vid, val)."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(cffi::defcfun ("add_boolVar_expr_var" add-bool-var-expr-var) :int
    "Add a BoolVar corresponding to the evalueation of rel-type(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(cffi::defcfun ("val_rel" val-rel) :void
    "Post a variable/value rel constraint."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(cffi::defcfun ("var_rel" var-rel) :void
    "Post a variable/variable rel constraint."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(cffi::defcfun ("arr_val_rel" arr-val-rel-aux) :void
    "Post a variable-array/value rel constraint."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
    (val :int)
)

(defun arr-val-rel (sp vids rel-type val)
    "Post a variable-array/value rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (arr-val-rel-aux sp (length vids) x rel-type val))
)

(cffi::defcfun ("arr_var_rel" arr-var-rel-aux) :void
    "Post a variable-array/variable rel constraint."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
    (vid :int)
)

(defun arr-var-rel (sp vids rel-type vid)
    "Post a variable-array/variable rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (arr-var-rel-aux sp (length vids) x rel-type vid))
)

(cffi::defcfun ("arr_rel" arr-rel-aux) :void
    "Post a variable-array rel constraint."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
)

(defun arr-rel (sp vids rel-type)
    "Post a variable-array rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (arr-rel-aux sp (length vids) x rel-type))
)

(cffi::defcfun ("arr_arr_rel" arr-arr-rel-aux) :void
    "Post a variable-array/variable-array rel constraint."
    (sp :pointer)
    (n1 :int)
    (vids1 :pointer)
    (rel-type :int)
    (n2 :int)
    (vids2 :pointer)
)

(defun arr-arr-rel (sp vids1 rel-type vids2)
    "Post a variable-array/variable-array rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids1))
          (y (cffi::foreign-alloc :int :initial-contents vids2)))
        (arr-arr-rel-aux sp (length vids1) x rel-type (length vids2) y))
)

(cffi::defcfun ("distinct" distinct-aux) :void
    "Post a distinct constraint on the n variables denoted in vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
)

(defun distinct (sp vids)
    "Post a distinct constraint on the variables denoted in vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (distinct-aux sp (length vids) x))
)

(cffi::defcfun ("val_linear" val-linear-aux) :void
    "Post a linear equation constraint."
    (sp :pointer)
    (n :int)
    (c :pointer)
    (vids :pointer)
    (rel-type :int)
    (val :int)
)

(defun val-linear (sp coeffs vars rel-type value)
    "Post a linear equation constraint. coeffs and vars must have the same number of elements."
    (let ((c (cffi::foreign-alloc :int :initial-contents coeffs))
          (x (cffi::foreign-alloc :int :initial-contents vars)))
        (val-linear-aux sp (length coeffs) c x rel-type value))
)

(cffi::defcfun ("var_linear" var-linear-aux) :void
    "Post a linear equation constraint."
    (sp :pointer)
    (n :int)
    (c :pointer)
    (vids :pointer)
    (rel-type :int)
    (vid :int)
)

(defun var-linear (sp coeffs vars rel-type vid)
    "Post a linear equation constraint. coeffs and vars must have the same number of elements."
    (let ((c (cffi::foreign-alloc :int :initial-contents coeffs))
          (x (cffi::foreign-alloc :int :initial-contents vars)))
        (var-linear-aux sp (length coeffs) c x rel-type vid))
)

(cffi::defcfun ("arithmetics_abs" ge-abs) :void
    "Post the constraint that |vid1| = vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi::defcfun ("arithmetics_div" ge-div) :void
    "Post the constraint that vid3 = vid1/vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi::defcfun ("arithmetics_mod" var-mod) :void
    "Post the constraint that vid1 % vid2 = vid3."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi::defcfun ("arithmetics_divmod" ge-divmod) :void
    "Post the constraint that vid3 = vid1/vid2 and vid4 = vid1 % vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
    (vid4 :int)
)

(cffi:defcfun ("arithmetics_min" ge-min) :void
    "Post the constraint that vid3 = min(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi:defcfun ("arithmetics_arr_min" ge-arr-min-aux) :void
    "Post the constraint that vid = min(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-arr-min (sp vid vids)
    "Post the constraint that vid = min(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-arr-min-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_argmin" ge-argmin-aux) :void
    "Post the constraint that vid = argmin(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-argmin (sp vids vid)
    "Post the constraint that vid = argmin(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-argmin-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_max" ge-max) :void
    "Post the constraint that vid3 = max(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi:defcfun ("arithmetics_arr_max" ge-arr-max-aux) :void
    "Post the constraint that vid = max(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-arr-max (sp vids vid)
    "Post the constraint that vid = max(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-arr-max-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_argmax" ge-argmax-aux) :void
    "Post the constraint that vid = argmax(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-argmax (sp vids vid)
    "Post the constraint that vid = argmax(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-argmax-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_mult" ge-mult) :void
    "Post the constraint that vid3 = vid1 * vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi:defcfun ("arithmetics_sqr" ge-sqr) :void
    "Post the constraint that vid2 = vid1^2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi:defcfun ("arithmetics_sqrt" ge-sqrt) :void
    "Post the constraint that vid2 = vid1^(1/2)."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi:defcfun ("arithmetics_pow" ge-pow) :void
    "Post the constraint that vid2 = vid1^n."
    (sp :pointer)
    (vid1 :int)
    (n :int)
    (vid2 :int)
)

(cffi:defcfun ("arithmetics_nroot" ge-nroot) :void
    "Post the constraint that vid2 = vid1^(1/n)."
    (sp :pointer)
    (vid1 :int)
    (n :int)
    (vid2 :int)
)

(cffi::defcfun ("set_dom" set-dom-aux) :void
    "Post the constraint that dom(vid) = domain of size n."
    (sp :pointer)
    (vid :int)
    (n :int)
    (domain :pointer)
)

(defun set-dom (sp vid domain)
    "Post the constraint that dom(vid) = domain."
    (let ((x (cffi::foreign-alloc :int :initial-contents domain)))
        (set-dom-aux sp vid (length domain) x))
)

(cffi::defcfun ("set_member" set-member-aux) :void
    "Post the constraint that vid is a member vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun set-member (sp vids vid)
    "Post the constraint that vid is a member vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (set-member-aux sp (length vids) x vid))
)

(cffi::defcfun ("rel_sum" rel-sum-aux) :void
    "Post the constraint that vid = sum(vids). n is the number of vars in vids."
    (sp :pointer)
    (vid :int)
    (n :int)
    (vids :pointer)
)

(defun rel-sum (sp vid vids)
    "Post the constraint that vid = sum(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (rel-sum-aux sp vid (length vids) x))
)

(cffi::defcfun ("count_val_val" count-val-val-aux) :void
    "Post the constraint that the number of variables in vids equal to val1 has relation
    rel-type with val2."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (val1 :int)
    (rel-type :int)
    (val2 :int)
)

(defun count-val-val (sp vids val1 rel-type val2)
    "Post the constraint that the number of variables in vids equal to val1 has relation
    rel-type with val2."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-val-val-aux sp (length vids) x val1 rel-type val2))
)

(cffi::defcfun ("count_val_var" count-val-var-aux) :void
    "Post the constraint that the number of variables in vids equal to val1 has relation
    rel-type with vid."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (val :int)
    (rel-type :int)
    (vid :int)
)

(defun count-val-var (sp vids val rel-type vid)
    "Post the constraint that the number of variables in vids equal to val has relation
    rel-type with vid."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-val-var-aux sp (length vids) x val rel-type vid))
)

(cffi::defcfun ("count_var_val" count-var-val-aux) :void
    "Post the constraint that the number of variables in vids equal to vid has relation
    rel-type with val."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(defun count-var-val (sp vids vid rel-type val)
    "Post the constraint that the number of variables in vids equal to vid has relation
    rel-type with val."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-var-val-aux sp (length vids) x vid rel-type val))
)

(cffi::defcfun ("count_var_var" count-var-var-aux) :void
    "Post the constraint that the number of variables in vids equal to vid1 has relation
    rel-type with vid2."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(defun count-var-var (sp vids vid1 rel-type vid2)
    "Post the constraint that the number of variables in vids equal to vid1 has relation
    rel-type with vid2."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-var-var-aux sp (length vids) x vid1 rel-type vid2))
)

(cffi::defcfun ("nvalues" nvalues-aux) :void
    "Post the constraint the number of distinct values in the n variables denoted by vids
     has the given rel-type relation with the variable vid."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
    (vid :int)
)

(defun nvalues (sp vids rel-type vid)
    "Post the constraint the number of distinct values in the n variables denoted by vids
    has the given rel-type relation with the variable vid."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (nvalues-aux sp (length vids) x rel-type vid))
)

(cffi::defcfun ("circuit" hcircuit-aux) :void
    "Post the constraint that values of vids1 are the edges of an hamiltonian circuit in 
    the graph formed by the n variables in vids1, vids2 are the costs of these edges described
    by c, and vid is the total cost of the circuit, i.e. sum(vids2)."
    (sp :pointer)
    (n :int)
    (c :pointer)
    (vids1 :pointer)
    (vids2 :pointer)
    (vid :int)    
)

(defun hcircuit (sp c vids1 vids2 vid)
    "Post the constraint that values of vids1 are the edges of an hamiltonian circuit in 
    the graph formed by the variables in vids1, vids2 are the costs of these edges described
    by c, and vid is the total cost of the circuit, i.e. sum(vids2)."
    (let ((costs (cffi::foreign-alloc :int :initial-contents c))
          (x (cffi::foreign-alloc :int :initial-contents vids1))
          (y (cffi::foreign-alloc :int :initial-contents vids2)))
        (hcircuit-aux sp (length vids1) costs x y vid))
)

;BoolVar operation flags
(defparameter gil::BOT_AND 0)    ; logical and
(defparameter gil::BOT_OR 1)     ; logical or
(defparameter gil::BOT_IMP 2)    ; logical implication
(defparameter gil::BOT_EQV 3)    ; logical equivalence
(defparameter gil::BOT_XOR 4)    ; logical exclusive or

(cffi::defcfun ("val_boolop" val-bool-op) :void
    "Post the constraint that val = bool-op(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (bool-op :int)
    (vid2 :int)
    (val :int)
)

(cffi::defcfun ("var_boolop" var-bool-op) :void
    "Post the constraint that vid3 = bool-op(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (bool-op :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi::defcfun ("val_boolrel" val-bool-rel) :void
    "Post boolean rel constraint."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(cffi::defcfun ("var_boolrel" var-bool-rel) :void
    "Post boolean rel constraint."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(cffi::defcfun ("branch" branch-aux) :void
    "Post branching on the n IntVars denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (var-strat :int)
    (val-strat :int)
)

(defun branch (sp vids var-strat val-strat)
    "Post branching on the IntVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (branch-aux sp (length vids) x var-strat val-strat))
)

(cffi::defcfun ("branch_b" branch-b-aux) :void
    "Post branching on the n BoolVars denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (var-strat :int)
    (val-strat :int)
)

(defun branch-b (sp vids var-strat val-strat)
    "Post branching on the BoolVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (branch-b-aux sp (length vids) x var-strat val-strat))
)

(cffi::defcfun ("cost" set-cost) :void
    "Define which variable is to be the cost."
    (sp :pointer)
    (vid :int)
)

(cffi::defcfun ("new_bab_engine" bab-engine-low) :pointer
    "Create a new branch and bound search-engine."
    (sp :pointer)
)

(cffi::defcfun ("bab_next" bab-next) :pointer
    "Find the next solution for the search-engine se."
    (se :pointer)
)

(cffi::defcfun ("new_dfs_engine" dfs-engine-low) :pointer
    "Create a new depth-first search search-engine."
    (sp :pointer)
)

(cffi::defcfun ("dfs_next" dfs-next) :pointer
    "Find the next solution for the search-engine se."
    (se :pointer)
)

(cffi::defcfun ("get_value" get-value) :int
    "Get the value of the variable denoted by vid."
    (sp :pointer)
    (vid :int)
)

(cffi::defcfun ("get_values" get-values-aux) :pointer
    "Get the values of the n variables denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
)

(defun get-values (sp vids)
    "Print the values of the variables denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids))
           p)
        (setq p (get-values-aux sp (length vids) x))
        (loop for i from 0 below (length vids) 
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("print_vars" print-vars-aux) :void
    "Print the values of the n variables denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
)

(defun print-vars (sp vids)
    "Print the values of the variables denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (print-vars-aux sp (length vids) x))
)