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
;;   Adapted from google/lisp-koans:koans/strings.lisp

;; Relevant emacs info page: `(info "(elisp)Strings and Characters")'


(elisp-koans/define-test test-double-quoted-strings-are-strings
    (let ((my-string "do or do not"))
      (elisp-koans/true-or-false? ___ (typep my-string 'string))
      "use `stringp' to check whether a value is a string"
      (elisp-koans/true-or-false? ___ (stringp my-string))
      (elisp-koans/true-or-false? ___ (stringp '(a list)))
      "strings are the same thing as vectors of characters"
      (elisp-koans/assert-equal (aref "meat" 2) (aref "fiesta" 5))))


(elisp-koans/define-test test-multi-line-strings-are-strings
    (let ((my-string "this is
                      a multi
                      line string"))
      (elisp-koans/true-or-false? ___ (stringp my-string))))


(elisp-koans/define-test test-escape-quotes
    (let ((my-string "this string has one of these \" in it"))
      (elisp-koans/true-or-false? ___ (stringp my-string))))


(elisp-koans/define-test test-substrings
  "since strings are sequences, you may use `subseq', aliased from `cl-subseq'"
  (let ((my-string "Groucho Marx"))
    (elisp-koans/assert-equal "Marx" (subseq my-string 8))
    (elisp-koans/assert-equal (subseq my-string 0 7) ____)
    (elisp-koans/assert-equal (subseq my-string 1 5) ____)))


(elisp-koans/define-test test-accessing-individual-characters
  "char literals look like this"
  (elisp-koans/true-or-false? ___ (typep ?a 'character))
  (elisp-koans/true-or-false? ___ (typep "A" 'character))
  (elisp-koans/true-or-false? ___ (typep ?a 'string))
  "char is used to access individual characters"
  (let ((my-string "Cookie Monster"))
    (elisp-koans/assert-equal (char my-string 0) ?C)
    (elisp-koans/assert-equal (char my-string 3) ?k)
    (elisp-koans/assert-equal (char my-string 7) ___)))


(elisp-koans/define-test test-concatenating-strings
  (let ((a "this")
        (b "is")
        (c "unwieldy")
        (d "less"))
    "concatenating strings in lisp's `concatenate', aliased from `cl-concatenate'
is a little cumbersome"
    (elisp-koans/assert-equal ___ (concatenate 'string a " " b " " c))
    "the elisp `concat' is a bit more straightforward"
    (elisp-koans/assert-equal ___ (concat a " " b " " d " " c))))


(elisp-koans/define-test test-searching-for-characters
  "you can use `position', aliased from `cl-position', to detect
the position of characters in strings (or elements in sequences)"
  (elisp-koans/assert-equal ___ (position ?b "abc"))
  (elisp-koans/assert-equal ___ (position ?c "abc"))
  "you can use `find', aliased from `cl-find', to detect
the position of characters in strings (or elements in sequences)"
  (elisp-koans/assert-equal ___ (find ?d "abc")))


(elisp-koans/define-test test-finding-substrings
  "search finds subsequences"
  (let ((title "A supposedly fun thing I'll never do again"))
    (elisp-koans/assert-equal 2 (search "supposedly" title))
    (elisp-koans/assert-equal 12 (search "CHANGETHISWORD" title))))

;;; strings.el ends here
