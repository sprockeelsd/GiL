(cl:defpackage "gil"
  (:nicknames "GIL")
   (:use common-lisp :cl-user :cl :cffi))

(in-package :gil)

(print "Loading gecode-wrapper...")

(cffi::defcfun ("computation_space" new-space) :pointer
    "Create a new computation space."
)

(cffi::defcfun ("get_size" get-size) :int
    "returns the size of the space"
    (sp :pointer)
)

(cffi::defcfun ("new_dfs_engine" dfs-engine) :pointer
    "Create a new depth-first search search-engine."
    (sp :pointer)
)

;solution exist?
(defun sol? (sol)
    "Existence predicate for a solution"
    (and (not (cffi::null-pointer-p sol)) sol))

(defmethod find-sol (dfs-engine)
    (sol? (dfs-next dfs-engine)))

(defun get-value (sp)
    (let* (
            (size (get-size sp))
            (ptr (return-solution sp))
        )
        (loop for i from 0 below size
            collect (cffi::mem-aref ptr :int i)
        )
    )
)

(cffi::defcfun ("return_solution" return-solution) :pointer
    "returns a pointer to an integer array where the first element is the size and the rest are the values of the solution"
    (sp :pointer)
)


(cffi::defcfun ("dfs_next" dfs-next) :pointer
    "Find the next solution for the search-engine se."
    (se :pointer)
)

(cffi::defcfun ("dfs_stopped" dfs-stopped) :int
    "returns t if the search engine has been stopped, nil otherwise"
    (se :pointer)
)






(defun get-values (sp vids)
    "Print the values of the variables denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids))
           p)
        (setq p (get-values-aux sp (length vids) x))
        (loop for i from 0 below (length vids)
            collect (cffi::mem-aref p :int i)))
)