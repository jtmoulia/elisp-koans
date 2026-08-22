;;; test-cheatsheet.el --- Keep the solutions in step with the koans  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The cheatsheet used to live on its own branch and quietly fell twelve
;; commits behind `master'.  Now that it is a directory these tests keep it
;; honest: every koan group must exist in both places, define the same things
;; in the same order, and the solutions must have no blanks left in them.
;;
;; Whether the solutions are *correct* is settled by running them --- see
;; `elisp-koans/run-tests-batch'.

;;; Code:

(require 'ert)
(require 'elisp-koans)

(defun elisp-koans-test//read-forms (file)
  "Return every top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (condition-case nil
                            (read (current-buffer))
                          (end-of-file nil)))
        (push form forms))
      (nreverse forms))))


(defun elisp-koans-test//definitions (file)
  "Return the (TYPE . NAME) of each definition in FILE, in order."
  (delq nil
        (mapcar (lambda (form)
                  (when (and (consp form)
                             (memq (car form)
                                   '(elisp-koans/deftest ert-deftest defun
                                     defmacro defvar defconst define-error
                                     cl-defstruct)))
                    (cons (car form) (cadr form))))
                (elisp-koans-test//read-forms file))))


(defun elisp-koans-test//blanks-in (form)
  "Return non-nil if FORM contains a koan blank anywhere within it."
  (cond ((elisp-koans//is-blank form) t)
        ((consp form) (or (elisp-koans-test//blanks-in (car form))
                          (elisp-koans-test//blanks-in (cdr form))))
        (t nil)))


(ert-deftest test-elisp-koans/cheatsheet-covers-every-group ()
  "Every koan group has a matching file in the cheatsheet."
  (dolist (group elisp-koans-groups)
    (let ((koan (elisp-koans//group-file group elisp-koans-directory))
          (solution (elisp-koans//group-file group elisp-koans-cheatsheet-directory)))
      (should (file-exists-p koan))
      (should (file-exists-p solution)))))


(defun elisp-koans-test//subsequence-p (needles haystack)
  "Return non-nil if NEEDLES appear in HAYSTACK in order, gaps allowed."
  (let ((rest haystack))
    (catch 'missing
      (dolist (needle needles t)
        (let ((found (member needle rest)))
          (unless found (throw 'missing nil))
          (setq rest (cdr found)))))))


(ert-deftest test-elisp-koans/cheatsheet-defines-the-same-koans ()
  "Every koan appears in its solution file, in the same order.

Solutions may define extra helper functions -- writing helpers is part
of the exercise -- but may not drop, rename or reorder a koan.  This is
what catches the cheatsheet falling behind the koans."
  (dolist (group elisp-koans-groups)
    (let ((koan (elisp-koans-test//definitions
                 (elisp-koans//group-file group elisp-koans-directory)))
          (solution (elisp-koans-test//definitions
                     (elisp-koans//group-file group elisp-koans-cheatsheet-directory))))
      (should (elisp-koans-test//subsequence-p koan solution)))))


(ert-deftest test-elisp-koans/cheatsheet-has-no-blanks-left ()
  "No solution still contains a blank -- the cheatsheet is meant to be complete."
  (dolist (group elisp-koans-groups)
    (let ((solution (elisp-koans//group-file
                     group elisp-koans-cheatsheet-directory)))
      (dolist (form (elisp-koans-test//read-forms solution))
        (should-not (elisp-koans-test//blanks-in form))))))


(ert-deftest test-elisp-koans/koans-are-not-the-cheatsheet ()
  "Each koan group differs from its solution.

Guards against the cheatsheet being copied over the koans by mistake.
Note that not every group has blanks -- in the project koans the work
is writing a function body, not filling in a blank."
  (dolist (group elisp-koans-groups)
    (let ((koan (elisp-koans-test//read-forms
                 (elisp-koans//group-file group elisp-koans-directory)))
          (solution (elisp-koans-test//read-forms
                     (elisp-koans//group-file group elisp-koans-cheatsheet-directory))))
      (should-not (equal koan solution)))))

;;; test-cheatsheet.el ends here
