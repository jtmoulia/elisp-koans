;;; elisp-koans.el --- Koans for learning elisp  -*- lexical-binding: t; -*-
;;
;; Author: Thomas Moulia <jtmoulia@pocketknife.io>
;; Version: 0.6.1
;; Keywords: elisp, educational
;; URL: https://github.com/jtmoulia/elisp-koans

;;; Commentary:
;;
;; Elisp-koans provides a set of a set of tests which you can fix as a tour of
;; elisp.

(require 'cl-lib)
(require 'dash)
(require 'ert)

(defgroup elisp-koans nil
  "Learn Emacs Lisp by fixing failing tests."
  :group 'lisp
  :prefix "elisp-koans-")

(defconst elisp-koans--blanks '(__ ___ ____))

(defconst elisp-koans--source-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing the elisp-koans source tree.")

(defcustom elisp-koans-directory
  (expand-file-name "koans/" elisp-koans--source-directory)
  "Directory the koan groups are loaded from.

Point this at `cheatsheet/' to load the filled-in solutions instead
of the koans, e.g. to check your answers.  See
`elisp-koans/load-cheatsheet'."
  :type 'directory
  :group 'elisp-koans)

(defcustom elisp-koans-cheatsheet-directory
  (expand-file-name "cheatsheet/" elisp-koans--source-directory)
  "Directory holding the filled-in solutions to the koans."
  :type 'directory
  :group 'elisp-koans)

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
    lexical-bindings
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


(defun elisp-koans//group-file (group &optional directory)
  "Return the absolute path of koan GROUP's file.

DIRECTORY defaults to `elisp-koans-directory'."
  (expand-file-name (concat (symbol-name group) ".el")
                    (or directory elisp-koans-directory)))


(defun elisp-koans//load-koan-group (group &optional directory)
  "Load a koan GROUP from DIRECTORY, defaulting to `elisp-koans-directory'."
  (load-file (elisp-koans//group-file group directory)))


(defun elisp-koans//replace (test substitution lst)
  "Where TEST is true use splice SUBSTITUTION into LST."
  (if (null lst)
      '()
    (let* ((head (car lst))
           (tail (cdr lst))
           (next-head (if (listp head) (elisp-koans//replace test substitution head)
                        (if (funcall test head) substitution head))))
      (cons next-head (elisp-koans//replace test substitution tail)))))


(defun elisp-koans//select-group (prefix-arg)
  "Select a koan group from `elisp-koans-groups' if PREFIX-ARG."
  (if prefix-arg
      (list (completing-read "Select koan group: "
                             (mapcar #'symbol-name elisp-koans-groups)))
    '()))


(defun elisp-koans//boundp (symbol)
  "Return non-nil if SYMBOL names an `elisp-koans' test."
  (and (ert-test-boundp symbol)
       (string-prefix-p "elisp-koans/" (symbol-name symbol))))


;; main interface

;;;###autoload
(defmacro elisp-koans/deftest (name args &rest form)
  "Define a test called NAME accepting ARGS with a body of FORM.

This wraps `ert-deftest' with a check for blanks."
  `(ert-deftest ,name ,args
     ,@(elisp-koans//replace
        #'elisp-koans//is-blank
        '(should-not "Fill in the blank to expand your awareness.")
        form)))


;;;###autoload
(defun elisp-koans/load-groups (&optional koans directory)
  "Load KOANS from the groups specified in `elisp-koans-groups'.

DIRECTORY defaults to `elisp-koans-directory'; pass
`elisp-koans-cheatsheet-directory' to load the solutions instead.

If called as an interactive function with a prefix argument the
caller is asked for the koan group to load."
  (interactive (list (elisp-koans//select-group current-prefix-arg)))
  (dolist (koan-group elisp-koans-groups)
    (elisp-koans//load-koan-group koan-group directory)))


;;;###autoload
(defun elisp-koans/load-cheatsheet ()
  "Load the filled-in solutions from `elisp-koans-cheatsheet-directory'.

This replaces any koans already loaded, so the next
`elisp-koans/run-tests' reports on the solutions rather than on
your own answers.  Call `elisp-koans/load-groups' to go back."
  (interactive)
  (elisp-koans/load-groups nil elisp-koans-cheatsheet-directory))


(defun elisp-koans//group-tests (&optional directory)
  "Return every koan test loaded from DIRECTORY, in koan-group order."
  (-flatten
   (mapcar
    (lambda (group)
      (let ((file-path (elisp-koans//group-file group directory)))
        (reverse (cdadr (assoc 'define-symbol-props
                               (cdr (assoc file-path load-history)))))))
    elisp-koans-groups)))


;;;###autoload
(defun elisp-koans/run-tests (&optional koans directory)
  "Run the tests in KOANS. If no koans are provided all of the
`elisp-koan' tests are run.

DIRECTORY defaults to `elisp-koans-directory' and selects which
copy of the koans the test list is drawn from.

If called as an interactive function with a prefix argument the
caller is asked for the koan group to test."
  (interactive (list (elisp-koans//select-group current-prefix-arg)))
  (ert-run-tests-interactively
   `(member ,@(elisp-koans//group-tests directory))))


;;;###autoload
(defun elisp-koans/run-tests-batch (&optional directory)
  "Load and run every koan from DIRECTORY in batch mode.

Returns non-nil when every koan passes.  This is what CI runs
against `elisp-koans-cheatsheet-directory'."
  (elisp-koans/load-groups nil directory)
  (let ((stats (ert-run-tests-batch
                `(member ,@(elisp-koans//group-tests directory)))))
    (zerop (ert-stats-completed-unexpected stats))))


;;;###autoload
(defun elisp-koans/run-test (test)
  "Run the elisp koan TEST."
  (interactive (list (completing-read "Run koan: " obarray #'elisp-koans//boundp)))
  (ert-run-tests-interactively test))

;;;###autoload
(defun elisp-koans/contemplate ()
  "Run koans in order and jump to next failure or blank."
  (interactive)
  (elisp-koans/load-groups)
  (elisp-koans/run-tests)
  ;; Note: This doesn't check for when the koan tests are "Aborted." rather than "Finished."
  (let ((retry-for 3.0)
        (retry-interval 0.2)
        (retries 3))
    (while (not (re-search-forward "^Finished\.$" nil t))
      (if (>= 0 retries)
          (error "Elisp-koans did not succcessfully complete!"))
      (sit-for retry-interval)
      (setf retries (- retry-for retry-interval))))
  (forward-button 3)
  (let ((failure (button-label (button-at (point)))))
    (push-button)
    (re-search-forward "___?_?")
    (backward-to-word)
    (forward-char)
    (message "%s has damaged your karma" failure)))

;;;###autoload
(defalias 'elisp-koans 'elisp-koans/contemplate)

(provide 'elisp-koans)

;;; elisp-koans.el ends here
