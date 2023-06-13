(cl:defpackage "gil"
  (:nicknames "GIL")
   (:use common-lisp :cl-user :cl :cffi))

(in-package :gil)

(print "Loading gecode-wrapper...")

;;;;;;;;;;;;;;;;;;;;
;; WSpace methods ;;
;;;;;;;;;;;;;;;;;;;;

(cffi::defcfun ("create_space" create-space) :pointer
    "Creates a new computation space. Returns a void* pointer to a WSpace object."
    (size :int) ; an integer representing the size
)

(cffi::defcfun ("get_size" get-size) :int
    "Returns the size of the space."
    (sp :pointer) ; a void* pointer to a WSpace object
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search engine methods ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi::defcfun ("create_dfs" create-dfs) :pointer
    "Creates a DFS<WSpace> object. Returns a void* pointer to a DFS<WSpace> object."
    (sp :pointer) ; a void* pointer to a WSpace object
)

(cffi::defcfun ("return_next_solution_space" return-next-solution-space) :pointer
    "Returns a pointer to the next solution of the problem. Returns a void* pointer to a WSpace* pointer."
    (dfs :pointer) ; a void* pointer to a DFS<WSpace>* pointer
)




; (cffi::defcfun ("return_solution_space" return-solution-space) :pointer
;     "returns a pointer to a WSpace object"
;     (dfs :pointer) ; a pointer to a DFS object
; )

; (cffi::defcfun ("return_sol" return-sol) :pointer
;     "returns the next solution of the problem in the form of a pointer to a int*"
;     (sp :pointer) ; a pointer to a WSpace object
; )

; (cffi::defcfun ("return_all" return-sol-directly) :pointer
;     (sp :pointer)
; )

; ;solution exist?
; ; (defun sol? (sol)
; ;     "Existence predicate for a solution"
; ;     (and (not (cffi::null-pointer-p sol)) sol))

; ; (defmethod find-sol (dfs-engine)
; ;     (sol? (dfs-next dfs-engine)))

; (defun get-value (sp)
;     (let* (
;             (size (get-size sp))
;             (ptr (return-sol-directly sp))
;             (x (cffi::foreign-alloc :int :initianl-contents ptr))
;         )
;         (print size)
;         (loop for i from 0 below size
;             collect (cffi::mem-aref ptr :int i)
;         )
;     )
; )