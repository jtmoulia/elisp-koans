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
;;   Adapted from google/lisp-koans:koans/evaluation.lisp


(elisp-koans/deftest
 elisp-koans/test-function-name-is-first-argument ()
 "In most imperative languages, the syntax of a function call has
the function name succeeded by a list of arguments.  In lisp,
the function name and arguments are all part of the same list,
with the function name the first element of that list."

 "in these examples, the function names are +, -, and *"
 (should (equal 5 (+ 2 3)))
 (should (equal -2 (- 1 3)))
 (should (equal 28 (* 7 4)))

 "'>' and '=' are the boolean functions (predicates) 'greater-than' and
   'equal to'"
 (should (equal t (> 100 4)))
 (should (equal t (= 3 3)))

 "'NUMBERP' is a predicate which returns true if the argument is a number"
 (should (equal t (numberp 5)))
 (should (equal nil (numberp "five"))))


(elisp-koans/deftest
 elisp-koans/evaluation-test-evaluation-order ()
 "Arguments to functions are evaluated before the function"
 (should (equal 9 (* (+ 1 2) (- 13 10)))))


(elisp-koans/deftest
 elisp-koans/test-quoting-behavior ()
 "Preceding a list with a quote (') will tell lisp not to evaluate a list.
The quote special form suppresses normal evaluation, and instead returns
the literal list.
Evaluating the form (+ 1 2) returns the number 3,
but evaluating the form '(+ 1 2) returns the list (+ 1 2)"
  (should (equal 3 (+ 1 2)))
  (should (equal '(+ 1 2) '(+ 1 2)))
  "'LISTP' is a predicate which returns true if the argument is a list"
  " the '(CONTENTS) form defines a list literal containing CONTENTS"
  (should (equal t (listp '(1 2 3))))
  (should (equal nil (listp 100)))
  (should (equal nil (listp "Word to your moms I came to drop bombs")))
  (should (equal t (listp nil)))
  (should (equal nil (listp (+ 1 2))))
  (should (equal t (listp '(+ 1 2))))
  "equalp is an equality predicate"
  (should (equal t (equalp 3 (+ 1 2))))
  "the '(xyz ghi) syntax is syntactic sugar for the (QUOTE (xyz ghi)) function."
  (should (eq t (equalp '(/ 4 0) (quote (/ 4 0))))))

;;; evalation.el ends here
