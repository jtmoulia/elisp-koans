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

(elisp-koans/define-test test-list-or-atom
  "Lists in lisp are forms beginning and ending with rounded parentheses.
   Atoms are symbols, numbers, or other forms usually separated by
   white-space or parentheses.  The function 'listp' will return true if
   the input is a list.  The function 'atom' will return true if the
   input is an atom."
  (elisp-koans/true-or-false? ___ (listp '(1 2 3)))
  (elisp-koans/true-or-false? ___ (atom '(1 2 3)))

  (elisp-koans/true-or-false? ___ (listp '("heres" "some" "strings")))
  (elisp-koans/true-or-false? ___ (atom '("heres" "some" "strings")))

  (elisp-koans/true-or-false? ___ (listp "a string"))
  (elisp-koans/true-or-false? ___ (atom "a string"))

  (elisp-koans/true-or-false? ___ (listp 2))
  (elisp-koans/true-or-false? ___ (atom 2))

  (elisp-koans/true-or-false? ___ (listp '(("first" "list") ("second" "list"))))
  (elisp-koans/true-or-false? ___ (atom '(("first" "list") ("second" "list")))))


(elisp-koans/define-test test-empty-list-is-both-list-and-atom
  "the empty list, nil, is unique in that it is both a list and an atom"
  (elisp-koans/true-or-false? ___ (listp nil))
  (elisp-koans/true-or-false? ___ (atom nil)))


(elisp-koans/define-test test-keywords
  "symbols like :hello or :like-this are treated differently in lisp.
   Called keywords, they are symbols that evaluate to themselves."
  (elisp-koans/true-or-false? ___ (equal :this-is-a-keyword :this-is-a-keyword))
  (elisp-koans/true-or-false? ___ (equal :this-is-a-keyword ':this-is-a-keyword)))

;;; atoms-vs-lists.el ends here
