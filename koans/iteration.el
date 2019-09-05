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
;;   google/lisp-koans:koans/iteration.lsp


;; There are many options for iteration in lisp.
;; This set of koans will introduce a few of the most common ones


;; Dolist evaluates a form for every element of a list.

(elisp-koans/deftest
 elisp-koans/iteration-dolist ()
 "`dolist' iterates over values in a list, binding each value to a lexical
variable in turn"
 (let* ((some-primes '(10301 11311 19991 999565999))
        (how-many-in-list 0)
        (biggest-in-list (first some-primes)))
   "this dolist loops over some-primes, defined above"
   (dolist (one-prime some-primes)
     (if (> one-prime biggest-in-list)
         (setf biggest-in-list one-prime))
     (incf how-many-in-list))
   (should (equal ___ how-many-in-list))
   (should (equal ___ biggest-in-list)))
 (let ((sum 0))
   "write your own dolist here to calculate the sum of some-primes"
   "you may be interested in investigating the 'incf' function"
   ;; (dolist ... )
   (should (eq 999607602 sum))))


(elisp-koans/deftest
 elisp-koans/iteration-dolist-with-return ()
 "`dolist' can accept a return variable, which will be the return value
upon completion of the iteration."
 (let ((my-list '(1 2 3 4))
       (my-return))
   (should (equal '(16 9 4 1)
                  (dolist (x my-list my-return)
                    (push (* x x) my-return))))))


(elisp-koans/deftest
 elisp-koans/iteration-dotimes ()
 "`dotimes' iterates over the integers from 0 to (limit - 1),
binding them in order to your selected symbol."
 (let ((out-list nil))
   (dotimes (y 3) (push y out-list))
   (should (equal ___ out-list))))


(elisp-koans/deftest
 elisp-koans/iteration-dotimes-binding ()
 "`dotimes' establishes a local lexical binding which may shadow
a global value."
 (let ((outer-var "outer"))
   (dotimes (outer-var 4)
     (should (eq ___ (equal "outer" outer-var))))
   (should (eq ___ (equal "outer" outer-var)))))


(elisp-koans/deftest
 elisp-koans/iteration-loop-until-return ()
 "`loop' loops forever, unless some return condition is executed.
Note that the loop macro includes many additional options, which
will be covered in a future koan."
 (let ((loop-counter 0))
   (loop
    (incf loop-counter)
    (if (>= loop-counter 100) (return loop-counter)))
   (should (equal ___ loop-counter))))


(elisp-koans/deftest
 elisp-koans/iteration-mapcar ()
 "`mapcar' takes a list and a function.  It returns a new list
with the function applied to each element of the input"
  (let ((mc-result (mapcar #'evenp '(1 2 3 4 5))))
    (should (equal ___ mc-result))))

;; ----


(defun vowelp (c)
  "Return true if `c' is a vowel."
  (find c "AEIOUaeiou"))

(defun vowels-to-xs (my-string)
  "Convert all vowels in a string to the character `x'."
  (coerce
   (loop for c across my-string
         with new-c
         do (setf new-c (if (vowelp c) ?x c))
         collect new-c)
   'string))

(elisp-koans/deftest
 elisp-koans/iteration-mapcar-with-defun ()
 "`mapcar' is a convenient way to apply a function to a collection"
 (should (equal (vowels-to-xs "Astronomy") "xstrxnxmy"))
 (let* ((subjects '("Astronomy" "Biology" "Chemistry" "Linguistics"))
        (mc-result (mapcar #'vowels-to-xs subjects)))
   (should (equal ___ mc-result))))

;; ----


(elisp-koans/deftest
 elisp-koans/iteration-mapcar-with-lambda ()
 "`mapcar' can apply a `lambda' to a collection"
 (let ((mc-result (mapcar (lambda (x) (mod x 10)) '(21 152 403 14))))
   (should (equal ___ mc-result))))

;; iteration.el ends here
