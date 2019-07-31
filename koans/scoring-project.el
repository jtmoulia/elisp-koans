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


;;;;;;;;;;;;;;
;; GREED !! ;;
;;;;;;;;;;;;;;


;; Modified from Ruby Koans: about_scoring_project.rb

;; *Greed* is a dice game where you roll up to five dice to accumulate
;; points.  The following "score" function will be used to calculate the
;; score of a single roll of the dice.
;;
;; A greed roll is scored as follows:
;;
;; * A set of three ones is 1000 points
;;
;; * A set of three numbers (other than ones) is worth 100 times the
;;   number. (e.g. three fives is 500 points).
;;
;; * A one (that is not part of a set of three) is worth 100 points.
;;
;; * A five (that is not part of a set of three) is worth 50 points.
;;
;; * Everything else is worth 0 points.
;;
;;
;; Examples:
;;
;; (elisp-koans/score '(1 1 1 5 1)) => 1150 points
;; (elisp-koans/score '(2 3 4 6 2)) => 0 points
;; (elisp-koans/score '(3 4 5 3 3)) => 350 points
;; (elisp-koans/score '(1 5 1 2 4)) => 250 points
;;
;; More scoring examples are given in the tests below:
;;
;; Your goal is to write the score method.


(defun elisp-koans/count-in (lst target)
  (length (seq-filter (lambda (elt) (equalp elt target)) lst)))

(defun elisp-koans/score (dice)
  "Score the DICE rolls."
  (let* ((ones-count (elisp-koans/count-in dice 1))
         (ones-score (+ (if (>= ones-count 3) 1000 0) (* (mod ones-count 3) 100)))
         (fives-score (* (mod (elisp-koans/count-in dice 5) 3) 50))
         (triples-score
          (apply '+ (mapcar
                     (lambda (x) (if (>= (elisp-koans/count-in dice x) 3) (* x 100) 0))
                     (number-sequence 2 6)))))
    (+ ones-score fives-score triples-score)))


(elisp-koans/deftest
 elisp-koans/test-score-of-an-empty-list-is-zero ()
 "The score of an empty list is 0."
 (should (eq 0 (elisp-koans/score nil))))


(elisp-koans/deftest
 elisp-koans/test-score-of-a-single-roll-of-5-is-50 ()
 "The score of a single five is 50."
 (should (eq 50 (elisp-koans/score '(5)))))


(elisp-koans/deftest
 elisp-koans/test-score-of-a-single-roll-of-1-is-100 ()
 "The score of a single role of one is 100."
 (should (eq 100 (elisp-koans/score '(1)))))


(elisp-koans/deftest
 elisp-koans/test-score-of-multiple-1s-and-5s-is-the-sum-of-individual-scores ()
 "The score of multiple ones and fives is the sum of the individual scores."
 (should (eq 300 (elisp-koans/score '(1 5 5 1)))))


(elisp-koans/deftest
 elisp-koans/test-score-of-single-2s-3s-4s-and-6s-is-zero ()
 "The score of single twos, threes, fours, and sixes is zero"
 (should (eq 0 (elisp-koans/score '(2 3 4 6)))))


(elisp-koans/deftest
 elisp-koans/test-score-of-a-triple-1-is-1000 ()
 "The score of a triple one is 1000."
 (should (eq 1000  (elisp-koans/score '(1 1 1)))))


(elisp-koans/deftest
 elisp-koans/test-score-of-other-triples-is-100x ()
 "The score of three of the same is 100x"
 (should (equal 200  (elisp-koans/score '(2 2 2))))
 (should (equal 300  (elisp-koans/score '(3 3 3))))
 (should (equal 400  (elisp-koans/score '(4 4 4))))
 (should (equal 500  (elisp-koans/score '(5 5 5))))
 (should (equal 600  (elisp-koans/score '(6 6 6)))))


(elisp-koans/deftest
 elisp-koans/test-score-of-mixed-is-sum ()
 "Sum the score of a mixture to get the total."
 (should (equal 250  (elisp-koans/score '(2 5 2 2 3))))
 (should (equal 550  (elisp-koans/score '(5 5 5 5)))))

;; scoring-project.el ends here
