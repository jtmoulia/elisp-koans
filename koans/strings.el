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
;;   Adapted from google/lisp-koans:koans/strings.lsp

;; Relevant emacs info page: `(info "(elisp)Strings and Characters")'


(elisp-koans/deftest
 elisp-koans/strings-double-quoted-strings ()
 "double quoted strings are strings"
 (let ((my-string "do or do not"))
   (should (eq ___ (cl-typep my-string 'string)))
   "use `stringp' to check whether a value is a string"
   (should (eq ___ (stringp my-string)))
   (should (eq ___ (stringp '(a list))))
   "strings are the same thing as vectors of characters"
   (should (eq (aref "meat" 2) (aref "fiesta" 5)))))


(elisp-koans/deftest
 elisp-koans/strings-multi-line-strings ()
 "multi-line strings are strings"
 (let ((my-string "this is
                      a multi
                      line string"))
   (should (eq ___ (stringp my-string)))))


(elisp-koans/deftest
 elisp-koans/strings-creating ()
 "use `make-string' to create a string of repeated characters"
 (should (equal ___ (make-string 5 ?x)))
 (should (equal ___ (make-string 0 ?y)))
 "use `string' to create a string out of individual characters"
 (should (equal ___ (string ?e ?l ?i ?s ?p ?!))))


(elisp-koans/deftest
 elisp-koans/strings-escape-quotes ()
 "use backslashes to escape double quotes"
 (let ((my-string "this string has one of these \" in it"))
   (should (eq ___ (stringp my-string)))))


(elisp-koans/deftest
 elisp-koans/strings-substrings ()
 "use `substring' to get part of a string"
 (let ((groucho "Groucho Marx"))
   (should (equal "Marx" (substring groucho 8)))
   (should (equal ___ (substring groucho 0 6)))
   (should (equal ___ (substring groucho -4 -1)))
   "since strings are sequences, you may also use `cl-subseq'"
   (should (equal ___ (cl-subseq groucho 0 7)))
   (should (equal ___ (cl-subseq groucho 1 5)))))


(elisp-koans/deftest
 elisp-koans/strings-accessing-individual-characters ()
 "char literals are the character preceded by a questionmark"
 (should (eq ___ (cl-typep ?a 'character)))
 (should (eq ___ (cl-typep "A" 'character)))
 (should (eq ___ (cl-typep ?a 'string))))


(elisp-koans/deftest
 elisp-koans/strings-concat ()
 "use `concat' to join strings"
 (let ((a "this")
       (b "is")
       (c "unwieldy")
       (d "less"))
   "concatenating strings with `cl-concatenate' is a little cumbersome"
   (should (equal ___ (cl-concatenate 'string a " " b " " c)))
   (should (equal ___ (concat a " " b " " d " " c)))))


(elisp-koans/deftest
 elisp-koans/strings-searching-for-characters ()
 "`cl-position' returns the index of an element in a sequence"
 (should (eq ___ (cl-position ?b "abc")))
 (should (eq ___ (cl-position ?c "abc")))
 "`seq-position' is the built-in equivalent, and works on any sequence"
 (should (eq ___ (seq-position "abc" ?b)))
 "`cl-find' returns the element itself rather than its index, and nil
when the element isn't there"
 (should (eq ___ (cl-find ?b "abc")))
 (should (eq ___ (cl-find ?d "abc"))))


(elisp-koans/deftest
 elisp-koans/strings-finding-substrings ()
 "`cl-search' finds subsequences"
 (let ((title "A supposedly fun thing I'll never do again"))
   (should (eq 2 (cl-search "supposedly" title)))
   (should (eq 13 (cl-search "CHANGETHISWORD" title)))))

;;; strings.el ends here
