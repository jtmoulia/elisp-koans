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
;;   Adapted from google/lisp-koans:koans/atoms-vs-lists.lisp

;; Relevant emacs info page: `(info "(elisp)Cons Cell Type")'

(elisp-koans/deftest
 elisp-koans/atoms-vs-lists-list-or-atom ()
 "Lists in lisp are forms beginning and ending with rounded parentheses.
Atoms are symbols, numbers, or other forms usually separated by
white-space or parentheses.  The function 'listp' will return true if
the input is a list.  The function 'atom' will return true if the
input is an atom."
 (should (eq t (listp '(1 2 3))))
 (should (eq nil (atom '(1 2 3))))

 (should (eq t (listp '("heres" "some" "strings"))))
 (should (eq nil (atom '("heres" "some" "strings"))))

 (should (eq nil (listp "a string")))
 (should (eq t (atom "a string")))

 (should (eq nil (listp 2)))
 (should (eq t (atom 2)))

 (should (eq t (listp '(("first" "list") ("second" "list")))))
 (should (eq nil (atom '(("first" "list") ("second" "list"))))))


(elisp-koans/deftest
 elisp-koans/atoms-vs-lists-test-empty-list-is-both-list-and-atom ()
 "the empty list, nil, is unique in that it is both a list and an atom"
 (should (eq t (listp nil)))
 (should (eq t (atom nil))))


(elisp-koans/deftest
 elisp-koans/atoms-vs-lists-test-keywords ()
 "symbols like :hello or :like-this are treated differently in lisp.
Called keywords, they are symbols that evaluate to themselves."
 (should (eq t (eq :this-is-a-keyword :this-is-a-keyword)))
 (should (eq t (eq :this-is-a-keyword ':this-is-a-keyword)))
 (should (eq nil (eq :this-is-a-keyword 'this-is-a-keyword))))

;;; atoms-vs-lists.el ends here
