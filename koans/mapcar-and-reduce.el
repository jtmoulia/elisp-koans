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
;;   google/lisp-koans:koans/mapcar-and-reduce.lsp

(elisp-koans/deftest
 elisp-koans/mapcar-basics ()
 "apply a function to each member of a list using `mapcar'"
 (defun times-two (x) (* x 2))
 (should (equal ____ (mapcar #'times-two '(1 2 3))))
 (should (equal ____ (mapcar #'first '((3 2 1)
                                       ("little" "small" "tiny")
                                       ("pigs" "hogs" "swine"))))))


;; TODO: fix for 2-arg elisp mapcar
;; (elisp-koans/define-test
;;  test-transpose-using-mapcar
;;  "Replace the usage of WRONG-FUNCTION in 'transpose' with the
;; correct lisp function (don't forget the #')."
;;  (defun WRONG-FUNCTION-1 (&rest rest) '())
;;  (defun transpose (L) (apply #'mapcar (cons #'WRONG-FUNCTION-1 L)))
;;  (elisp-koans/assert-equal '((1 4 7)
;;                              (2 5 8)
;;                              (3 6 9))
;;                            (transpose '((1 2 3)
;;                                         (4 5 6)
;;                                         (7 8 9))))
;;  (elisp-koans/assert-equal '(("these" "pretzels" "are")
;;                              ("making" "me" "thirsty"))
;;                            (transpose '(("these" "making")
;;                                         ("pretzels" "me")
;;                                         ("are" "thirsty")))))


(elisp-koans/deftest
 elisp-koans/reduce-basics ()
 "The `reduce 'function combines the elements of a list, from left to right,
by applying a binary function to the list elements."
  (should (equal ___  (reduce #'+ '(1 2 3 4))))
  (should (equal ___ (reduce #'expt '(2 3 2)))))


(elisp-koans/deftest
 elisp-koans/reduce-right-to-left ()
 "The keyword :from-end allows us to apply reduce from right to left."
 (elisp-koans/assert-equal ___ (reduce #'+ '(1 2 3 4) :from-end t))
 (elisp-koans/assert-equal ___ (reduce #'expt '(2 3 2) :from-end t)))


(elisp-koans/deftest
 elisp-koans/reduce-with-initial-value ()
 "`reduce' accepts an optional initial value to reduce"
 (elisp-koans/assert-equal ___ (reduce #'expt '(10 21 34 43) :initial-value 1))
 (elisp-koans/assert-equal ___ (reduce #'expt '(10 21 34 43) :initial-value 0)))


;; TODO: fix for 2-arg elisp mapcar
;; (defun WRONG-FUNCTION-2 (a b) (a))
;; (defun WRONG-FUNCTION-3 (a b) (a))

;; (elisp-koans/define-test
;;  test-mapcar-and-reduce
;;  "mapcar and reduce are a powerful combination.
;; insert the correct function names, instead of WRONG-FUNCTION-X
;; to define an inner product."
;;   (defun inner (x y)
;;     (reduce #'WRONG-FUNCTION-2 (mapcar #'WRONG-FUNCTION-3 x y)))
;;   (elisp-koans/assert-equal 32 (inner '(1 2 3) '(4 5 6)))
;;   (elisp-koans/assert-equal 310 (inner '(10 20 30) '(4 3 7))))

;; mapcar-and-reduce.el ends here
