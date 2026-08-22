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
;;   Adapted from google/lisp-koans:koans/vectors.lsp

;; Relevant emacs info page: `(info "(elisp)Vectors")'

(elisp-koans/deftest
 elisp-koans/vectors-types ()
 "`[x y z]' defines a vector literal containing x y z"
 (should (eq ___ (cl-typep [1 11 111] 'vector)))
 (should (eq ___ (aref [1 11 111] 1))))


(elisp-koans/deftest
 elisp-koans/vectors-length ()
 "`length' works on vectors"
 (should (eq ___ (length [1 2 3]))))


(elisp-koans/deftest
 elisp-koans/vectors-bool ()
 "`bool-vector' builds a vector of booleans, which `vconcat' expands
into an ordinary vector."
 (should (equal ___ (vconcat (bool-vector nil t t))))
 "bool vectors have a read syntax too: #&LENGTH\"BYTES\", where the bits
of each byte are read low bit first.  A tab is ASCII 9, or 1001, so the
literal below is four booleans -- but you would have to work that out
every time you read it, which is why `bool-vector' is the better way to
write one down."
 (should (eq ___ (equal #&4"	" (bool-vector t nil nil t))))
 (should (eq ___ (cl-typep (bool-vector t nil nil t) 'bool-vector)))
 (should (eq ___ (aref (bool-vector t nil nil t) 1))))


(elisp-koans/deftest
 elisp-koans/vectors-bool-operations ()
 "bool vectors can be compared using operations"
 (should (equal ___
                (vconcat (bool-vector-intersection (bool-vector t t nil nil)
                                                   (bool-vector t nil t nil)))))
 (should (equal ___
                (vconcat (bool-vector-union (bool-vector t t nil nil)
                                            (bool-vector t nil t nil)))))
 (should (equal ___
                (vconcat (bool-vector-exclusive-or (bool-vector t t nil nil)
                                                   (bool-vector t nil t nil))))))


(defun list-to-bool-vector (my-list)
  "Return a `bool-vector' created from the elements of MY-LIST."
  nil)

(elisp-koans/deftest
 elisp-koans/vectors-list-to-bool-vector ()
 "you must complete `list-to-bool-vector'"
 (should (bool-vector-p (list-to-bool-vector '(nil nil t t nil))))
 (should-not (aref (list-to-bool-vector '(nil)) 0))
 (should (aref (list-to-bool-vector '(nil t)) 1))
 (should (equal 8 (length (list-to-bool-vector '(nil nil t t nil nil t t))))))

;;; vectors.el ends here
