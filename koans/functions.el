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


(elisp-koans/deftest
 elisp-koans/functions-call ()
 "`defun' defines global functions"
 (should (eq ___ (elisp-koans/add 7 11))))


(elisp-koans/deftest
 elisp-koans/functions-shadow ()
 "Local functions are defined with `cl-flet' or `cl-labels'. One
major difference between the two is that local functions defined
with `cl-labels' may refer to themselves, whereas local functions
defined with `cl-flet' may not."
 (cl-flet ((elisp-koans/add (a b) (* a b)))
   "`cl-flet' binds a function to a name within a lexical environment"
   (should (equal ___ (elisp-koans/add 7 11))))
 (should (equal ___  (elisp-koans/add 7 11))))


;; borrowed from Common Lisp The Language chapter 5.2.2
(cl-defun elisp-koans/func-with-opt-params (&optional (a 2) (b 3) )
  ;; each optional parameter has a form like (var default-val)
  (list a b))

(elisp-koans/deftest
 elisp-koans/functions-optional-parameters ()
 "Optional parameters are filled in with their default value."
 (should (equal ___ (elisp-koans/func-with-opt-params :test-1 :test-2)))
 (should (equal ___ (elisp-koans/func-with-opt-params :test-1)))
 (should (equal ___ (elisp-koans/func-with-opt-params))))

;; ----


(cl-defun elisp-koans/func-with-opt-params-and-indication (&optional (a 2 a?) (b 3 b?))
  (list a a? b b?))

(elisp-koans/deftest
 elisp-koans/functions-parameters-with-indication ()
 "Common Lisp optional params may bind a symbol which indicate
whether the value was provided or defaulted. Each optional
parameter binding has the form (var default-form supplied-p)."
   (should (equal ___ (elisp-koans/func-with-opt-params-and-indication :test-1 :test-2)))
   (should (equal ___ (elisp-koans/func-with-opt-params-and-indication :test-1)))
   (should (equal ___ (elisp-koans/func-with-opt-params-and-indication))))

;; ----


(defun elisp-koans/func-with-rest-params (&rest x)
  x)

(elisp-koans/deftest
 elisp-koans/functions-with-rest-params ()
 "With &rest, the remaining params, are handed in as a list.  Remaining
arguments (possibly none) are collected into a list."
 (should (equal ___ (elisp-koans/func-with-rest-params)))
 (should (equal ___ (elisp-koans/func-with-rest-params 1)))
 (should (equal ___ (elisp-koans/func-with-rest-params 1 :two 333))))

;; ----


(cl-defun elisp-koans/cl-defun-with-key-params (&key a b)
  "Return keyword arguments A and B as a list."
  (list a b))

(elisp-koans/deftest
 elisp-koans/functions-key-params ()
 "Key params allow the user to specify params in any order."
 (should (equal ___ (elisp-koans/cl-defun-with-key-params)))
 (should (equal ___ (elisp-koans/cl-defun-with-key-params :a 11 :b 22)))
 ;; it is not necessary to specify all key parameters
 (should (equal ___ (elisp-koans/cl-defun-with-key-params :b 22)))
 ;; order is not important
 (should (equal ___ (elisp-koans/cl-defun-with-key-params :b 22 :a 0))))


(cl-defun elisp-koans/cl-defun-key-params-can-have-defaults (&key  (a 3) (b 4))
  "Return keyword arguments A and B as a list."
  (list a b))

(elisp-koans/deftest
 elisp-koans/functions-key-params-can-have-defaults ()
 "key parameters can have defaults also"
 (should (equal ___ (elisp-koans/cl-defun-key-params-can-have-defaults)))
 (should (equal ___ (elisp-koans/cl-defun-key-params-can-have-defaults :a 3 :b 4)))
 (should (equal ___ (elisp-koans/cl-defun-key-params-can-have-defaults :a 11 :b 22)))
 (should (equal ___ (elisp-koans/cl-defun-key-params-can-have-defaults :b 22)))
 ;; order is not important
 (should (equal ___ (elisp-koans/cl-defun-key-params-can-have-defaults :b 22 :a 0))))


;; ----


;; borrowed from common lisp the language 5.2.2
(cl-defun elisp-koans/cl-defun-with-funky-parameters (a &rest x &key b (c a))
   (list a b c x))

(elisp-koans/deftest
 elisp-koans/functions-many-kinds-params ()
 "CL (and elisp!) provides the programmer with more than enough rope to hang themself."
 (should (equal ___ (elisp-koans/cl-defun-with-funky-parameters 1)))
 (should (equal ___ (elisp-koans/cl-defun-with-funky-parameters 1 :b 2)))
 (should (equal ___ (elisp-koans/cl-defun-with-funky-parameters 1 :b 2 :c 3)))
 (should (equal ___ (elisp-koans/cl-defun-with-funky-parameters 1 :c 3 :b 2))))


;; Note that &rest parameters have to come before &key parameters.
;; This is an error: (defun f (&key a &rest x) () )
;; But this is ok:   (defun f (&rest x &key a) () )


(elisp-koans/deftest
 elisp-koans/functions-lambdas-are-nameless-functions ()
 "A `lambda' form defines a function, but with no name.  It is possible
to execute that function immediately, or put it somewhere for later use."
 (should (eq ___ ((lambda (a b)) (+ a b)) 10 9))
 (let ((my-function))
   (setf my-function (lambda (a b) (* a b)))
   (should (eq ___ (funcall my-function 11 9))))
 (let ((list-of-functions nil))
   (push (lambda (a b) (+ a b)) list-of-functions)
   (push (lambda (a b) (* a b)) list-of-functions)
   (push (lambda (a b) (- a b)) list-of-functions)
   (should (equal ___ (funcall (second list-of-functions)) 2 33))))


(elisp-koans/deftest
 elisp-koans/functions-apply-with-apply ()
 "`apply' calls the function parameter on a list of all the remaining
parameters"
 (let (f1 f2 f3)
   (setq f1 '+)
   (setq f2 '-)
   (setq f3 'max)

   (should (equal ___ (apply f1 '(1 2))))
   (should (equal ___ (apply f2 '(1 2))))

   ;; after the function name, the parameters are consed onto the front
   ;; of the very last parameter
   (should (equal ___ (apply f1 1 2 '(3))))
   (should (equal ___ (apply f3 1 2 3 4 '())))))


(elisp-koans/deftest
 elisp-koans/functions-apply-with-funcall ()
 "`funcall' calls the function parameter on a list of all the
remaining parameters. Remaining params do not expect a final
list."
 (let (f1 f2 f3)
   (setq f1 '+)
   (setq f2 '-)
   (setq f3 'max)
   (should (equal ___ (funcall f1 1 2)))
   (should (equal ___ (funcall f2 1 2)))
   (should (equal ___ (funcall f1 1 2 3)))
   (should (equal ___ (funcall f3 1 2 3 4)))))

;;; functions.el ends here
