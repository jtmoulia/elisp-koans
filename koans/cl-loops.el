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

(elisp-koans/define-test
 test-basic-loop
 (let* ((letters '(:a :b :c :d))
        (loop-result
         (cl-loop for letter in letters
                  collect letter)))
   (elisp-koans/assert-equal loop-result ____)))


(elisp-koans/define-test
 test-compound-loop
 (let* ((letters '(:a :b :c :d))
        (loop-result
         (cl-loop for letter in letters
                  for i from 1 to 1000
                  collect (list i letter))))
   (elisp-koans/assert-equal loop-result ____)))


(elisp-koans/define-test
 test-counting-loop-skip-by-syntax
 "with multiple 'for' clauses, cl-loop ends when the first is exhausted"
 (let* ((letters '(:a :b :c :d))
        (loop-result
         (cl-loop for letter in letters
                  for i from 0 to 1000 by 5
                  collect (list i letter))))
   (elisp-koans/assert-equal loop-result ____ )))


(elisp-koans/define-test
 test-counting-backwards
 (let ((loop-result
        (cl-loop for i from 10 downto -10 by 5
                 collect i )))
   (elisp-koans/assert-equal loop-result ____ )))


(elisp-koans/define-test
 test-loop-in-vs-loop-on
 (let* ((letters '(:a :b :c))
        (loop-result-in
         (cl-loop for letter in letters collect letter))
        (loop-result-on
         (cl-loop for letter on letters collect letter)))
   (elisp-koans/assert-equal loop-result-in ____)
   (elisp-koans/assert-equal loop-result-on ____ )))


(elisp-koans/define-test
 test-loop-in-skip-by
 (let* ((letters '(:a :b :c :d :e :f))
        (loop-result-in
         (cl-loop for letter in letters collect letter))
        (loop-result-in-cdr
         (cl-loop for letter in letters by #'cdr collect letter))
        (loop-result-in-cddr
         (cl-loop for letter in letters by #'cddr collect letter))
        (loop-result-in-cdddr
         (cl-loop for letter in letters by #'cdddr collect letter)))
   (elisp-koans/assert-equal loop-result-in ____)
   (elisp-koans/assert-equal loop-result-in-cdr ____)
   (elisp-koans/assert-equal loop-result-in-cddr ____)
   (elisp-koans/assert-equal loop-result-in-cdddr ____)))


(elisp-koans/define-test
 test-loop-across-vector
 "=cl-loop= works across vectors"
 (let* ((my-vector (vector 0 1 2 3 4))
        (loop-result
         (cl-loop for val across my-vector collect val)))
   (elisp-koans/assert-equal ____ loop-result)))


(defvar books-to-heros)
(setf books-to-heros (make-hash-table :test 'equal))
(setf (gethash "The Hobbit" books-to-heros) "Bilbo")
(setf (gethash "Where The Wild Things Are" books-to-heros) "Max")
(setf (gethash "The Wizard Of Oz" books-to-heros) "Dorothy")
(setf (gethash "The Great Gatsby" books-to-heros) "James Gatz")


(elisp-koans/define-test
 test-loop-over-hash-tables
 (let* ((pairs-in-table
         (cl-loop for k being the hash-keys in books-to-heros
                  using (hash-value v)
                  collect (list k v))))
   (elisp-koans/assert-equal ____ (length pairs-in-table))
   (true-or-false? ____ (find '("The Hobbit" "Bilbo") pairs-in-table :test #'equal))))


(elisp-koans/define-test
 test-value-accumulation-forms
 (let ((loop-1
        (cl-loop for x in '(1 2 4 8 16)
                 collect x into collected
                 count x into counted
                 sum x into summed
                 maximize x into maximized
                 minimize x into minimized
                 finally (return (list collected counted summed maximized minimized)))))
   (destructuring-bind (col count sum max min) loop-1
     (elisp-koans/assert-equal col ____)
     (elisp-koans/assert-equal count ____)
     (elisp-koans/assert-equal sum ____)
     (elisp-koans/assert-equal max ____)
     (elisp-koans/assert-equal min ____))))


(elisp-koans/define-test
 test-destructuring-bind
 (let* ((count 0)
        (result (cl-loop for (a b) in '((1 9) (2 8) (3 7) (4 6))
                         do (setf count (+ 1 count))
                         collect (+ a b))))
   (elisp-koans/assert-equal ____ count)
   (elisp-koans/assert-equal ____ result)))


(elisp-koans/define-test
 test-conditional-execution
 (let ((loop-return
        (cl-loop for x in '(1 1 2 3 5 8 13)
                 when (evenp x) sum x)))
   (elisp-koans/assert-equal loop-return ____)))


(defun greater-than-10-p (x)
  (> x 10))

(elisp-koans/define-test
 test-conditional-with-defun
 (let ((loop-return
        (cl-loop for x in '(1 1 2 3 5 8 13)
                 when (greater-than-10-p x) sum x)))
   (elisp-koans/assert-equal loop-return ____)))


(elisp-koans/define-test
 test-conditional-with-lambda
 (let ((loop-return
        (cl-loop for x in '(1 1 2 3 5 8 13)
                 when ((lambda (z) (equal 1 (mod z 3))) x) sum x)))
   (elisp-koans/assert-equal loop-return ____)))

;; cl-loops.el ends here
