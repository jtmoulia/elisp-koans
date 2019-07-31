
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
;;   based on python koans 'about_lists.py'
;;   based also on "Lisp 3rd Edition" ch. 17. "List storage, surgery and reclamation"
;;
;;   Adapted from google/lisp-koans:koans/lists.lsp

;; Relevant emacs info page: `(info "(elisp)Lists")'

(elisp-koans/deftest
 elisp-koans/lists-creating ()
 "lists can be created using the quote form, or the `list' function"
 (let ((fruits nil)
       (some-evens nil))
   (setf fruits '(orange pomello clementine))
   (setf some-evens (list (* 2 1) (* 2 2) (* 2 3)))
   (should (equal ___ fruits))
   (should (eq ___ (length some-evens)))))


(elisp-koans/deftest
 elisp-koans/lists-cons ()
 "`cons' CONStructs new lists, by prefixing some list with a new
element like `(cons new-element some-list)'"
 (let (nums)
   (setf nums (cons :one nums))
   (should (equal nums '(:one)))

   (setf nums (cons :two nums))
   (should (equal ___ nums))

   "lists can contain anything, even mixtures of different things"
   (setf nums (cons 333 nums))
   (should (equal ___ nums))

   "lists can of course contain lists"
   (setf nums (cons '("the" "rest") nums))
   (should (equal ___ nums))))


(elisp-koans/deftest
 elisp-koans/lists-push-pop ()
 "`push' adds an element to the beginning of a list referred to by some symbol.
`pop' is the opposite of push: it removes and returns the first
element of a list."
 (let ((stack '(10 20 30 40))
       (firstval nil))
   (push "last" stack)
   (should (equal stack '("last" 10 20 30 40)))

   (setf firstval (pop stack))
   (should (equal "last" firstval))
   (should (equal stack '(10 20 30 40)))

   (setf firstval (pop stack))
   (should (equal ___ firstval))
   (should (equal ___ stack))))


(elisp-koans/deftest
 elisp-koans/lists-append ()
 "append attaches one list to the end of another."
 (should (equal (append '(:a :b) '(:c)) '(:a :b :c)))

 (let ((abc '(:a :b :c))
       (xyz '(:x :y :z))
       (abcxyz nil))
   (setf abcxyz (append abc xyz))
   (should (equal ___ abc))
   (should (equal ___ xyz))
   (should (equal ___ abcxyz))))


(elisp-koans/deftest
 elisp-koans/lists-accessing-elements ()
 "`last' returns a singleton list of the final element.
`elt' is similar to `nth', with the arguments reversed."
 (let ((noms '("peanut" "butter" "and" "jelly")))
   (should (equal "peanut" (first noms)))
   (should (equal ___ (second noms)))
   (should (equal ___ (fourth noms)))
   (should (equal ___ (last noms)))
   (should (equal "butter" (nth 1 noms))) ; k 1
   (should (equal ___ (nth 0 noms)))
   (should (equal ___ (nth 2 noms)))
   (should (equal ___ (elt noms 2)))))


(elisp-koans/deftest
 elisp-koans/lists-slicing ()
 "Use `subseq' to slice a list."
 (let ((noms '("peanut" "butter" "and" "jelly")))
   (should (equal ___ (subseq noms 0 1)))
   (should (equal ___ (subseq noms 0 2)))
   (should (equal ___ (subseq noms 2 2)))
   (should (equal ___ (subseq noms 2)))))


(elisp-koans/deftest
 elisp-koans/list-breakdown ()
 "`car' (aka. 'first') returns the first value in a list"
 (should (equal ___ (car '(1 2 3))))
 (should (equal ___ (car nil)))
 "cdr (aka. 'rest') refers to the remainder of the list,
     after the first element"
 (should (equal ___ (cdr '(1 2 3))))
 (should (equal ___ (cdr nil))))

;;; lists.el ends here
