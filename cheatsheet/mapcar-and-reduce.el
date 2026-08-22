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
 (should (equal '(2 4 6) (mapcar #'times-two '(1 2 3))))
 (should (equal '(3 "little" "pigs") (mapcar #'cl-first '((3 2 1)
                                         ("little" "small" "tiny")
                                         ("pigs" "hogs" "swine"))))))


(elisp-koans/deftest
 elisp-koans/mapcar-over-several-lists ()
 "Common Lisp's `mapcar' walks any number of lists at once.  Emacs Lisp's
takes exactly one, so reach for `cl-mapcar' when you need to walk
several of them in step."
 (should (equal '(2 3 4) (mapcar #'1+ '(1 2 3))))
 (should-error (mapcar #'+ '(1 2 3) '(10 20 30)) :type 'wrong-number-of-arguments)
 (should (equal '(11 22 33) (cl-mapcar #'+ '(1 2 3) '(10 20 30))))
 "`seq-mapn' is the built-in equivalent, and works on any sequence"
 (should (equal '(11 22 33) (seq-mapn #'+ '(1 2 3) '(10 20 30)))))


(elisp-koans/deftest
 elisp-koans/reduce-basics ()
 "The `cl-reduce' function combines the elements of a list, from left to right,
by applying a binary function to the list elements."
  (should (equal 10  (cl-reduce #'+ '(1 2 3 4))))
  (should (equal 64 (cl-reduce #'expt '(2 3 2)))))


(elisp-koans/deftest
 elisp-koans/reduce-right-to-left ()
 "The keyword :from-end allows us to apply `cl-reduce' from right to left."
 (should (equal 10 (cl-reduce #'+ '(1 2 3 4) :from-end t)))
 (should (equal 512 (cl-reduce #'expt '(2 3 2) :from-end t))))


(elisp-koans/deftest
 elisp-koans/reduce-with-initial-value ()
 "`cl-reduce' accepts an optional initial value to reduce"
 (should (equal 1 (cl-reduce #'expt '(10 21 34 43) :initial-value 1)))
 (should (equal 0 (cl-reduce #'expt '(10 21 34 43) :initial-value 0))))

;; mapcar-and-reduce.el ends here
