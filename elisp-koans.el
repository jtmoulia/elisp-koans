;; -*- lexical-binding: t; -*-

(require 'cl)

(defconst elisp-koans--blanks '(__ ___ ____))
(defconst elisp-koans--groups-directory (expand-file-name "koans/")
  "Directory where the elisp koan groups are stored")
(defvar elisp-koans-groups
  '(
    asserts
    nil-false-empty
    evaluation
    atoms-vs-lists
    special-forms
    lists
    vectors
    equality-distinctions
    hash-tables
    functions
    strings
    control-statements
    iteration
    mapcar-and-reduce
    format
    ;; cl-koans
    cl-loops
    cl-multiple-values
    cl-structures
    ;; projects
    triangle-project
    scoring-project
    )
  "List of elisp-koans which are run by default.")


;; helper functions

(defun elisp-koans//is-blank (elt)
  "Return whether ELT is a blank."
  (member elt elisp-koans--blanks))


(defun elisp-koans//load-koan-group (group)
  "Load a koan group from `elisp-koans--groups-directory'."
  (let* ((file-name (concat (symbol-name group) ".el"))
         (file-path (concat (file-name-as-directory elisp-koans--groups-directory) file-name)))
    (load-file file-path)))


(defun elisp-koans//replace (test substitution lst)
  (if (null lst)
      '()
    (let* ((head (car lst))
           (tail (cdr lst))
           (next-head (if (listp head) (elisp-koans//replace test substitution head)
                        (if (funcall test head) substitution head))))
      (cons next-head (elisp-koans//replace test substitution tail)))))


;; main interface

(defmacro elisp-koans/deftest (name args &rest form)
  "Define a test called NAME with DESCRIPTION and a body of FORM.

This wraps `ert-deftest' with a check for blanks."
  `(ert-deftest ,name ,args
     ,@(elisp-koans//replace
        #'elisp-koans//is-blank
        '(should-not "Fill in the blank to expand your awareness.")
        form)))


(defun elisp-koans/load-koan-groups (&optional koans)
  "Load KOANS from the groups specified in `elisp-koans-groups'."
  (interactive)
  (dolist (koan-group elisp-koans-groups)
    (elisp-koans//load-koan-group koan-group)))

(provide 'elisp-koans)

;;; elisp-koans.el ends here
