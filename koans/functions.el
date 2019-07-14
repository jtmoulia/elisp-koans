;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.
;;
;;   borrows from about_methods.py
;;   Adapted from google/lisp-koans:koans/functions.lisp

;; Relevant emacs info page: `(info "(elisp)Functions")'


(defun elisp-koans/add (a b)
  (+ a b))


(elisp-koans/define-test test-call-a-function
    "DEFUN defines global functions"
  (elisp-koans/assert-equal ___ (elisp-koans/add 7 11)))


(elisp-koans/define-test test-shadow-a-function
    "Local functions are defined with FLET or LABELS.  One major difference
     between the two is that local functions defined with LABELS may refer
     to themselves, whereas local functions defined with FLET may not."
   (elisp-koans/assert-eq 18 (elisp-koans/add 7 11))
   "`cl-flet' binds a function to a name within a lexical environment"
   (cl-flet ((elisp-koans/add (a b) (* a b)))
     (elisp-koans/assert-equal ___ (elisp-koans/add 7 11)))
   (elisp-koans/assert-equal ___  (elisp-koans/add 7 11)))


; borrowed from Common Lisp The Language chapter 5.2.2
(cl-defun func-with-opt-params (&optional (a 2) (b 3) )
  ; each optional parameter has a form like (var default-val)
  (list a b))

(elisp-koans/define-test test-optional-parameters
    "Optional parameters are filled in with their default value."
   (elisp-koans/assert-equal (func-with-opt-params :test-1 :test-2) ___)
   (elisp-koans/assert-equal (func-with-opt-params :test-1) ___)
   (elisp-koans/assert-equal (func-with-opt-params) ___))


;; ----


(cl-defun func-with-opt-params-and-indication (&optional (a 2 a?) (b 3 b?))
  (list a a? b b?))

(elisp-koans/define-test test-optional-parameters-with-indication
   "Common Lisp optional params may bind a symbol which indicate whether the
    value was provided or defaulted.  Each optional parameter binding has the
    form (var default-form supplied-p)."
   (elisp-koans/assert-equal (func-with-opt-params-and-indication :test-1 :test-2) ___)
   (elisp-koans/assert-equal (func-with-opt-params-and-indication :test-1) ___)
   (elisp-koans/assert-equal (func-with-opt-params-and-indication) ___))


;; ----


(defun func-with-rest-params (&rest x)
  x)

(elisp-koans/define-test test-func-with-rest-params
  "With &rest, the remaining params, are handed in as a list.  Remaining
   arguments (possibly none) are collected into a list."
  (elisp-koans/assert-equal (func-with-rest-params) ___)
  (elisp-koans/assert-equal (func-with-rest-params 1) ___)
   (elisp-koans/assert-equal (func-with-rest-params 1 :two 333) ___))


;; ----


(cl-defun cl-defun-with-key-params (&key a b)
  "Return keyword arguments A and B as a list."
  (list a b))

(elisp-koans/define-test test-key-params ()
  "Key params allow the user to specify params in any order"
   (elisp-koans/assert-equal (cl-defun-with-key-params) ___)
   (elisp-koans/assert-equal (cl-defun-with-key-params :a 11 :b 22) ___)
   ; it is not necessary to specify all key parameters
   (elisp-koans/assert-equal (cl-defun-with-key-params :b 22) ___)
   ; order is not important
   (elisp-koans/assert-equal (cl-defun-with-key-params :b 22 :a 0) ___))


(cl-defun cl-defun-key-params-can-have-defaults (&key  (a 3) (b 4))
  "Return keyword arguments A and B as a list."
  (list a b))

(elisp-koans/define-test test-key-params-can-have-defaults
    "key parameters can have defaults also"
   (elisp-koans/assert-equal (cl-defun-key-params-can-have-defaults) ____)
   (elisp-koans/assert-equal (cl-defun-key-params-can-have-defaults :a 3 :b 4) ___)
   (elisp-koans/assert-equal (cl-defun-key-params-can-have-defaults :a 11 :b 22) ___)
   (elisp-koans/assert-equal (cl-defun-key-params-can-have-defaults :b 22) ___)
   ; order is not important
   (elisp-koans/assert-equal (cl-defun-key-params-can-have-defaults :b 22 :a 0) ___))


;; ----


;; borrowed from common lisp the language 5.2.2
(cl-defun cl-defun-with-funky-parameters (a &rest x &key b (c a))
   (list a b c x))

(elisp-koans/define-test test-many-kinds-params
    "CL provides the programmer with more than enough rope to hang himself."
   (elisp-koans/assert-equal (cl-defun-with-funky-parameters 1) ___)
   (elisp-koans/assert-equal (cl-defun-with-funky-parameters 1 :b 2) ___)
   (elisp-koans/assert-equal (cl-defun-with-funky-parameters 1 :b 2 :c 3) ___)
   (elisp-koans/assert-equal (cl-defun-with-funky-parameters 1 :c 3 :b 2) ___))


;; Note that &rest parameters have to come before &key parameters.
;; This is an error: (defun f (&key a &rest x) () )
;; But this is ok:   (defun f (&rest x &key a) () )


(elisp-koans/define-test test-lambdas-are-nameless-functions
    "A lambda form defines a function, but with no name.  It is possible
     to execute that function immediately, or put it somewhere for later use."
   (elisp-koans/assert-equal 19 ((lambda (a b) (+ a b)) 10 9))
  (let ((my-function))
    (setf my-function (lambda (a b) (* a b)))
    (elisp-koans/assert-equal ___ (funcall my-function 11 9)))
  (let ((list-of-functions nil))
    (push (lambda (a b) (+ a b)) list-of-functions)
    (push (lambda (a b) (* a b)) list-of-functions)
    (push (lambda (a b) (- a b)) list-of-functions)
    (elisp-koans/assert-equal ___ (funcall (second list-of-functions) 2 33))))


; returns sign x
;; (defun sign-of (x)
;;   (if (< x 0) (return-from sign-of -1))
;;   (if (eq x 0) (return-from sign-of 0))
;;   1)

;; (elisp-koans/define-test test-return-from-function-early
;;    (elisp-koans/assert-equal (sign-of -5.5) ___)
;;    (elisp-koans/assert-equal (sign-of 0) ___)
;;    (elisp-koans/assert-equal (sign-of ___) 1))


;; ----




(elisp-koans/define-test test-apply-function-with-apply
  "APPLY calls the function parameter on a list of all the remaining
   parameters"
  (let (f1 f2 f3)
    (setq f1 '+)
    (setq f2 '-)
    (setq f3 'max)

    (elisp-koans/assert-equal ___ (apply f1 '(1 2)))
    (elisp-koans/assert-equal ___ (apply f2 '(1 2)))

    ; after the function name, the parameters are consed onto the front
    ; of the very last parameter
    (elisp-koans/assert-equal ___ (apply f1 1 2 '(3)))
    (elisp-koans/assert-equal ___ (apply f3 1 2 3 4 '()))))


(elisp-koans/define-test test-apply-function-with-funcall
  "FUNCALL calls the function parameter on a list of all the remaining
   parameters.  Remaining params do not expect a final list."
  (let (f1 f2 f3)
    (setq f1 '+)
    (setq f2 '-)
    (setq f3 'max)
    (elisp-koans/assert-equal ___ (funcall f1 1 2))
    (elisp-koans/assert-equal ___ (funcall f2 1 2))
    (elisp-koans/assert-equal ___ (funcall f1 1 2 3))
    (elisp-koans/assert-equal ___ (funcall f3 1 2 3 4))))


;; clean up the example function
(makunbound 'elisp-koans/add)

;;; functions.el ends here
