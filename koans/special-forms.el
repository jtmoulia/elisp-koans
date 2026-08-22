;;; special-forms.el --- Special form koans  -*- lexical-binding: t; -*-

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
;;   Adapted from google/lisp-koans:koans/special-forms.lsp

;; Special forms are evaluatable lisp forms (lists) which are
;; neither functions nor macros.  Here is an introduction to a
;; few of them.

;; based on http://psg.com/~dlamkins/sl/chapter03-03.html

(defvar elisp-koans-my-name)
(defvar elisp-koans-my-clones-name)
(defvar elisp-koans-a)
(defvar elisp-koans-b)
(defvar elisp-koans-c 0)

;; Relevant emacs info page: `(info "(elisp)Setting Generalized Variables")'

(elisp-koans/deftest
 elisp-koans/special-forms-setf ()
 "`setf' is used to assign values to symbols. These symbols may refer to
variables with lexical or dynamic scope."
  (setf elisp-koans-my-name "David")
  (should (equal ___ elisp-koans-my-name))
  (setf elisp-koans-my-clones-name elisp-koans-my-name)
  (should (equal ___ "David"))
  (setf elisp-koans-a 5)
  (setf elisp-koans-b 10)
  (setf elisp-koans-c ___)
  (should (equal 50 elisp-koans-c)))

;; Relevant emacs info page: `(info "(elisp)Local Variables")'

(elisp-koans/deftest
 elisp-koans/special-forms-let ()
  "The `let' form binds symbols to values for the extent of its body.
Once the body is done the previous value, if there was one, is
visible again."
  (setf elisp-koans-a 10)
  (setf elisp-koans-b 20)
  (should (equal elisp-koans-a ___))
  (should (equal elisp-koans-b ___))
  (let ((elisp-koans-a 1111)
        (elisp-koans-b 2222))
    (should (equal elisp-koans-a ___))
    (should (equal elisp-koans-b ___)))
  (should (equal elisp-koans-a ___))
  (should (equal elisp-koans-b ___)))


(defun elisp-koans//peek-at-a ()
  "Return whatever `elisp-koans-a' is bound to right now."
  elisp-koans-a)

(elisp-koans/deftest
 elisp-koans/special-forms-let-lexical-or-dynamic ()
 "Whether `let' binds lexically or dynamically is a property of the
symbol, not of the `let'.  This file sets `lexical-binding' to t, so a
plain local is lexical -- it is visible only to code written inside the
body.  But `elisp-koans-a' was declared with `defvar' near the top of
this file, which makes it special, and `let' binds a special symbol
dynamically: every function called from the body sees the new value
too."
 (setf elisp-koans-a :global)
 (let ((elisp-koans-a :rebound)
       (plain-local :lexical))
   "`elisp-koans//peek-at-a' is defined outside this `let', yet it sees
the rebound value"
   (should (eq ___ (elisp-koans//peek-at-a)))
   (should (eq ___ plain-local)))
 "and once the `let' is done the global value is back"
 (should (eq ___ (elisp-koans//peek-at-a))))


(elisp-koans/deftest
 elisp-koans/special-forms-let-default-value ()
 "`let' vars are bound to a default value."
 (let ((x))
   (should (equal ___ x))))


(elisp-koans/deftest
 elisp-koans/special-forms-let-bindings-are-parallel ()
 "When defining the bindings in the let form, later bindings may not depend
on earlier ones."
 (setf elisp-koans-a 100)
 (let ((elisp-koans-a 5)
       (elisp-koans-b (* 10 elisp-koans-a)))
   (should (equal elisp-koans-b ___))))


(elisp-koans/deftest
 elisp-koans/special-forms-let*-bindings-are-series ()
 "let* is like let, but successive bindings may use values of previous ones"
 (let ((x 100))
   (let* ((x 5)
          (y (* 10 x)))
     (should (equal y ___)))
   (should (equal x ___))))


(elisp-koans/deftest
 elisp-koans/special-forms-write-your-own-let-statement ()
 "fix the let statement to get the tests to pass"
 (setf elisp-koans-a 100)
 (setf elisp-koans-b 23)
 (setf elisp-koans-c 456)
 (let ((elisp-koans-a ___)
       (elisp-koans-b ___)
       (elisp-koans-c ___))
   (should (eq elisp-koans-a 100))
   (should (eq elisp-koans-b 200))
   (should (equal elisp-koans-c "Jellyfish")))
 (let* ((elisp-koans-a ___)
        ;; add more bindings here
        )
   (should (eq elisp-koans-a 121))
   (should (eq elisp-koans-b 200))
   (should (equal elisp-koans-c (+ elisp-koans-a (/ elisp-koans-b elisp-koans-a))))))

;; Relevant emacs info page: `(info "(cl)Conditionals")'
;; In cl-lib, the conditional form is `cl-case'.

(elisp-koans/deftest
 elisp-koans/special-forms-case ()
 "the `cl-case' form is like the `switch' statement: it compares an
input with a set of values and evaluates an expression once a
match is found"
 (setf elisp-koans-a 4)
 (setf elisp-koans-b
       (cl-case elisp-koans-a
         (4 :four)
         (5 :five)
         ;; t specifies default behavior
         (t :unknown)))
 (should (equal ___ elisp-koans-b))
 "case can also check if a list of values contains
   the input"
 (setf elisp-koans-c
       (cl-case elisp-koans-a
         (5 :five)
         ((3 4) :three-or-four)))
 (should (equal ___ elisp-koans-c)))


(defun elisp-koans/cartoon-dads (input)
  "you should be able to complete this case statement"
  (cl-case input
    (:this-one-doesnt-happen :fancy-cat)
    (t :unknown)))

(elisp-koans/deftest
 elisp-koans/special-forms-your-own-case-statement ()
 "fix this by completing the 'cartoon-dads' function above"
 (should (equal :homer (elisp-koans/cartoon-dads :bart)))
 (should (equal :peter (elisp-koans/cartoon-dads :stewie)))
 (should (equal :randy (elisp-koans/cartoon-dads :stan)))
 (should (equal :unknown (elisp-koans/cartoon-dads :space-ghost))))

(elisp-koans/deftest
 elisp-koans/special-forms-limits-of-case ()
 "case is not suitable for all kinds of values, because
it uses the function eql for comparisons. We will explore
the implications of this in the equality-distinctions lesson"
 (let* ((name "John")
        (lastname (cl-case name
                    ("John" "Doe")
                    ("Max" "Mustermann")
                    (t "Anonymous"))))
   (should (equal ___ lastname))))

(elisp-koans/deftest
 elisp-koans/special-forms-cond ()
 "cond is the general purpose form for checking multiple
conditions, until a condition is met"
 (let* ((a 4)
        (c (cond ((> a 0) :positive)
                 ((< a 0) :negative)
                 (t :zero))))
   (should (equal ___ c))))

;;; special-forms.el ends here
