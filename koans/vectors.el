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
;;   Adapted from google/lisp-koans:koans/vectors.lisp

;; Relevant emacs info page: `(info "(elisp)Vectors")'

(elisp-koans/define-test test-vector-types
  "`[x y z]' defines a vector literal containing x y z"
  (elisp-koans/true-or-false? ___ (typep [1 11 111] 'vector))
  (elisp-koans/assert-equal ___ (aref [1 11 111] 1)))


(elisp-koans/define-test test-length-works-on-vectors
  (elisp-koans/assert-equal (length [1 2 3]) ___))


(elisp-koans/define-test test-bool-vector
  "`#*0011' defines a bool vector literal with four elements, 0, 0, 1 and 1"
  (elisp-koans/assert-equal #&4"\f" (bool-vector nil t t))
  (elisp-koans/true-or-false? ____ (typep #&4"	" 'bool-vector))
  (elisp-koans/assert-equal ____ (aref #&4"	" 1)))


(elisp-koans/define-test test-bool-vector-bitwise-operations
  (elisp-koans/assert-equal ___ (bool-vector-intersection (bool-vector t t nil nil) (bool-vector t nil t nil)))
  (elisp-koans/assert-equal ___ (bool-vector-union (bool-vector t t nil nil) (bool-vector t nil t nil)))
  (elisp-koans/assert-equal ___ (bool-vector-exclusive-or (bool-vector t t nil nil) (bool-vector t nil t nil))))


(defun list-to-bool-vector (my-list)
  nil)

(elisp-koans/define-test test-list-to-bool-vector
  "you must complete `list-to-bool-vector'"
  (elisp-koans/assert-true (bool-vector-p (list-to-bool-vector '(nil nil t t nil))))
  (elisp-koans/assert-equal (aref (list-to-bool-vector '(nil)) 0) nil)
  (elisp-koans/assert-equal (aref (list-to-bool-vector '(nil t)) 1) t)
  (elisp-koans/assert-equal (length (list-to-bool-vector '(nil nil t t nil nil t t))) 8))

;;; vectors.el ends here
