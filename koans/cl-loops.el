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
;;   Adapted from google/lisp-koans:koans/loops.lsp

;; see http://www.gigamonkeys.com/book/loop-for-black-belts.html
;; "Loop for blackbelts" for more on the cl-loop macro.


(elisp-koans/deftest
 elisp-koans/cl-loops-basic ()
 "Use `cl-loop' to loop through and collect a list."
 (let* ((letters '(:a :b :c :d))
        (loop-result
         (cl-loop for letter in letters
                  collect letter)))
   (should (equal '(:a :b :c :d) loop-result))))


(elisp-koans/deftest
 elisp-koans/cl-loops-compound ()
 "With multiple `for' clauses, `cl-loop' ends when the first is exhausted"
 (let* ((letters '(:a :b :c :d))
        (loop-result
         (cl-loop for letter in letters
                  for i from 1 to 1000
                  collect (list i letter))))
   (should (equal '((1 :a) (2 :b) (3 :c) (4 :d)) loop-result))))


(elisp-koans/deftest
 elisp-koans/cl-loops-counting-skip-by-syntax ()
 "The `by' clause specifies the step interval for `cl-loop'."
 (let* ((letters '(:a :b :c :d))
        (loop-result
         (cl-loop for letter in letters
                  for i from 0 to 1000 by 5
                  collect (list i letter))))
   (should (equal '((0 :a) (5 :b) (10 :c) (15 :d)) loop-result))))


(elisp-koans/deftest
 elisp-koans/cl-loops-counting-backwards ()
 "The `by' clause can specify a negative step interval `downto' a target."
 (let ((loop-result
        (cl-loop for i from 10 downto -10 by 5
                 collect i)))
   (should (equal '(10 5 0 -5 -10) loop-result))))


(elisp-koans/deftest
 elisp-koans/cl-loops-in-vs-on ()
 "Use `on' to iterate across the list `cdr' as opposed to the `car'."
 (let* ((letters '(:a :b :c))
        (loop-result-in
         (cl-loop for letter in letters collect letter))
        (loop-result-on
         (cl-loop for letter on letters collect letter)))
   (should (equal '(:a :b :c) loop-result-in))
   (should (equal '((:a :b :c) (:b :c) (:c)) loop-result-on))))


(elisp-koans/deftest
 elisp-koans/cl-loops-in-skip-by ()
 "Use `by' to specify a custom step function for the list."
 (let* ((letters '(:a :b :c :d :e :f))
        (loop-result-in
         (cl-loop for letter in letters collect letter))
        (loop-result-in-cdr
         (cl-loop for letter in letters by #'cdr collect letter))
        (loop-result-in-cddr
         (cl-loop for letter in letters by #'cddr collect letter))
        (loop-result-in-cdddr
         (cl-loop for letter in letters by #'cdddr collect letter)))
   (should (equal '(:a :b :c :d :e :f) loop-result-in))
   (should (equal '(:a :b :c :d :e :f) loop-result-in-cdr))
   (should (equal '(:a :c :e) loop-result-in-cddr))
   (should (equal '(:a :d) loop-result-in-cdddr))))


(elisp-koans/deftest
 elisp-koans/cl-loops-across-vector ()
 "`cl-loop' works across vectors."
 (let* ((my-vector (vector 0 1 2 3 4))
        (loop-result
         (cl-loop for val across my-vector collect val)))
   (should (equal '(0 1 2 3 4) loop-result))))


(elisp-koans/deftest
  elisp-koans/cl-loops-over-hash-tables ()
  "`cl-loop' iterates over keys when passed a hash table.
`using' allow you to specify the loop's access function."
  (let ((books-to-heroes (make-hash-table :test 'equal)))
    (setf (gethash "The Hobbit" books-to-heroes) "Bilbo")
    (setf (gethash "Where The Wild Things Are" books-to-heroes) "Max")
    (setf (gethash "The Wizard Of Oz" books-to-heroes) "Dorothy")
    (setf (gethash "The Great Gatsby" books-to-heroes) "James Gatz")
    (let* ((pairs-in-table
            (cl-loop for k being the hash-keys in books-to-heroes
                     using (hash-value v)
                     collect (list k v))))
      (should (eq 4 (length pairs-in-table)))
      (should (equal '("The Hobbit" "Bilbo") (find '("The Hobbit" "Bilbo") pairs-in-table :test #'equal)))
      )))


(elisp-koans/deftest
 elisp-koans/cl-loops-accumulation-forms ()
 "`into' specifies an accumulator function."
 (let ((loop-1
        (cl-loop for x in '(1 2 4 8 16)
                 collect x into collected
                 count x into counted
                 sum x into summed
                 maximize x into maximized
                 minimize x into minimized
                 finally (return (list collected counted summed maximized minimized)))))
   (cl-destructuring-bind (col count sum max min) loop-1
     (should (equal '(1 2 4 8 16) col))
     (should (eq 5 count))
     (should (eq 31 sum))
     (should (eq 16 max))
     (should (eq 1 min)))))


(elisp-koans/deftest
 elisp-koans/cl-loops-destructuring-bind ()
 "`for' supports a `cl-destructuring-bind'-like assignment."
 (let* ((count 0)
        (result
         (cl-loop for (a b) in '((1 9) (2 8) (3 7) (4 6))
                  do (setf count (+ 1 count))
                  collect (+ a b))))
   (should (equal 4 count))
   (should (equal '(10 10 10 10) result))))


(elisp-koans/deftest
 elisp-koans/cl-loops-conditional ()
 "`cl-loop' uses `when' for conditional evaluation."
 (let ((loop-return
        (cl-loop for x in '(1 1 2 3 5 8 13)
                 when (evenp x) sum x)))
   (should (equal 10 loop-return))))


(defun greater-than-10-p (x)
  (> x 10))

(elisp-koans/deftest
 elisp-koans/cl-loops-conditional-with-defun ()
 "the conditional `when' can accept any function."
 (let ((loop-return
        (cl-loop for x in '(1 1 2 3 5 8 13)
                 when (greater-than-10-p x) sum x)))
   (should (equal 13 loop-return))))


(elisp-koans/deftest
 elisp-koans/cl-loops-conditional-with-lambda ()
 "the conditional `when' can also accept a lambda surrounded in parens."
 (let ((loop-return
        (cl-loop for x in '(1 1 2 3 5 8 13)
                 when ((lambda (z) (equal 1 (mod z 3))) x) sum x)))
   (should (equal 15 loop-return))))

;; cl-loops.el ends here
