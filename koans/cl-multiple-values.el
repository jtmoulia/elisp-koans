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
;;   Adapted from google/lisp-koans:koans/multiple-values.lsp


(elisp-koans/deftest
 elisp-koans/multiple-values-floor ()
 "In Common Lisp, it is possible for a function to return more than one value.
This is distinct from returning a list or structure of values."
 (let ((x)
       (y))
   "`floor' returns a single value"
   (setf x (floor 1.5))
   (should (eq x 1))
   "`floor*' returns multiple values"
   (setf x (multiple-value-list (floor* 1.5)))
   (should (equal x '(1 0.5))))
 (should (equal '(24 0.75) (multiple-value-list (floor* (/ 99.0 4))))))


(defun next-fib (a b)
  (values b (+ a b)))

(elisp-koans/deftest
 elisp-koans/multiple-values-bind ()
 "multiple-value-bind binds the variables in the first form to
the outputs of the second form. And then returns the output of
the third form using those bindings"
 (let ((x)
       (y))
   (setf x (next-fib 2 3))
   (should (equal '(3 5) x))
   (setf x (multiple-value-list (next-fib 2 3)))
   (should (equal '(3 5) x))
   (setf y (multiple-value-bind (b c) (next-fib 3 5) (* b c)))
   (should (equal 40 y))
   "multiple-value-setq is like setf, but can set multiple variables"
   (multiple-value-setq (x y) (values :v1 :v2))
   (should (equal '(:v1 :v2) (list x y)))
   (multiple-value-setq (x y) (next-fib 5 8))
   (should (equal '(8 13) (list x y)))))

;;; multiple-values.el ends here
