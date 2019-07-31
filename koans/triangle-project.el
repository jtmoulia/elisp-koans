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
;;   Adapted from google/lisp-koans:koans/triangle-project.lsp


;; TODO: you need to write the triangle method
(define-error 'triangle-error "triangle improperly formed")

(defun elisp-koans/triangle (a b c)
  "Return the type of triangle given side lengths A, B, and C. Possible triangle
types are `:equilateral', `:isosceles' and `:scalene'. Throw `triangle-error' if
the provided side lengths are invalid."
  (cond
   ((or (<= a 0) (<= b 0) (<= c 0)) (signal 'triangle-error nil))
   ((or (<= (+ a b) c) (<= (+ a c) b) (<= (+ b c) a)) (signal 'triangle-error nil))
   ((= a b c) :equilateral)
   ((or (= a b) (= a c) (= b c)) :isosceles)
   (:scalene)))


(elisp-koans/deftest
 elisp-koans/triangles-equilateral ()
 "An equilateral triangle has three sides of equal length."
 (should (eq :equilateral (elisp-koans/triangle 2 2 2)))
 (should (eq :equilateral (elisp-koans/triangle 10 10 10))))


(elisp-koans/deftest
 elisp-koans/triangles-isosceles ()
 "An isosceles triangle has two sides of equal length."
 (should (eq :isosceles (elisp-koans/triangle 3 4 4)))
 (should (eq :isosceles (elisp-koans/triangle 4 3 4)))
 (should (eq :isosceles (elisp-koans/triangle 4 4 3)))
 (should (eq :isosceles (elisp-koans/triangle 10 10 2))))


(elisp-koans/deftest
 elisp-koans/triangles-scalene ()
 "An scalene triangle has no sides of equal length."
 (should (eq :scalene (elisp-koans/triangle 3 4 5)))
 (should (eq :scalene (elisp-koans/triangle 10 11 12)))
 (should (eq :scalene (elisp-koans/triangle 5 4 2))))


(elisp-koans/deftest
 elisp-koans/triangles-illegal-throw-exceptions ()
 "A triangle can't have any sides of length zero."
 (should-error (elisp-koans/triangle 0 0 0) :type 'triangle-error)
 "A triangle can't have any sides of negative length."
 (should-error (elisp-koans/triangle 3 4 -5) :type 'triangle-error)
 "One side can't be longer than the sum of the other two sides."
 (should-error (elisp-koans/triangle 1 1 3) :type 'triangle-error)
 (should-error (elisp-koans/triangle 2 4 2) :type 'triangle-error))

;; triangle-project.el ends here
