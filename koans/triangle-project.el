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

(defun triangle (a b c)
  "Return the type of triangle given side lengths A, B, and C. Possible triangle
types are `:equilateral', `:isosceles' and `:scalene'. Throw `triangle-error' if
the provided side lengths are invalid."
  :write-me)


(elisp-koans/deftest
 test-equilateral-triangles-have-equal-sides ()
 "An equilateral triangle has three sides of equal length."
 (should (eq :equilateral (triangle 2 2 2)))
 (should (eq :equilateral (triangle 10 10 10))))


(elisp-koans/deftest
 test-isosceles-triangles-have-two-equal-sides ()
 "An isosceles triangle has two sides of equal length."
 (should (eq :isosceles (triangle 3 4 4)))
 (should (eq :isosceles (triangle 4 3 4)))
 (should (eq :isosceles (triangle 4 4 3)))
 (should (eq :isosceles (triangle 10 10 2))))


(elisp-koans/deftest
 test-scalene-triangles-have-no-equal-sides ()
 "An scalene triangle has no sides of equal length."
 (should (eq :scalene (triangle 3 4 5)))
 (should (eq :scalene (triangle 10 11 12)))
 (should (eq :scalene (triangle 5 4 2))))


(elisp-koans/deftest
 test-illegal-triangles-throw-exceptions ()
 "A triangle can't have any sides of length zero."
 (should-error (triangle 0 0 0) :type 'triangle-error)
 "A triangle can't have any sides of negative length."
 (should-error (triangle 3 4 -5) :type 'triangle-error)
 "One side can't be longer than the sum of the other two sides."
 (should-error (triangle 1 1 3) :type 'triangle-error)
 (should-error (triangle 2 4 2) :type 'triangle-error))

;; triangle-project.el ends here
