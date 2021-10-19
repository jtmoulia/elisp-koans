;;; elisp-koans.el --- Koans for learning elisp
;;
;; Author: Thomas Moulia <jtmoulia@pocketknife.io>
;; Version: 0.6.0
;; Keywords: elisp, educational
;; URL: https://github.com/jtmoulia/elisp-koans

;;; Commentary:
;;
;; Elisp-koans provides a set of a set of tests which you can fix as a tour of
;; elisp.

;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)

(defconst elisp-koans--blanks '(__ ___ ____))
(defconst elisp-koans--groups-directory (expand-file-name "koans/")
  "Directory where the elisp koan groups are stored.")
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
  "Load a koan GROUP from `elisp-koans--groups-directory'."
  (let* ((file-name (concat (symbol-name group) ".el"))
         (file-path (concat (file-name-as-directory elisp-koans--groups-directory) file-name)))
    (load-file file-path)))


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
(defun elisp-koans/load-groups (&optional koans)
  "Load KOANS from the groups specified in `elisp-koans-groups'.

If called as an interactive function with a prefix argument the
caller is asked for the koan group to load."
  (interactive (list (elisp-koans//select-group current-prefix-arg)))
  (dolist (koan-group elisp-koans-groups)
    (elisp-koans//load-koan-group koan-group)))


;;;###autoload
(defun elisp-koans/run-tests (&optional koans)
  "Run the tests in KOANS. If no koans are provided all of the
`elisp-koan' tests are run.

If called as an interactive function with a prefix argument the
caller is asked for the koan group to test."
  (interactive (list (elisp-koans//select-group current-prefix-arg)))
  (ert-run-tests-interactively
   `(member
     ,@(-flatten
        (mapcar
         (lambda (group)
           (let* ((file-name (concat (symbol-name group) ".el"))
                  (file-path
                   (concat (file-name-as-directory elisp-koans--groups-directory) file-name)))
             (reverse (cdadr (assoc 'define-symbol-props (cdr (assoc file-path load-history)))))))
         elisp-koans-groups)))))


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
