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
;;   Adapted from google/lisp-koans:koans/equality-distinctions.lisp

;; Relevant emacs info page: `(info "(elisp)Equality Predicates")'

;; the most common equality predicates are eq, eql, equal and equalp
;; eq is similar to comparing c pointers

(elisp-koans/deftest
 elisp-koans/equality-distinctions-test-eq ()
 "(eq x y) is true if and only if x and y are the same identical object
eq is like comparing pointers in c.  If the values are EQ, any non-nil
value may be returned."
 (should (eq ___ (eq 'a 'a)))
 (should (eq ___ (eq 3 3)))
 (should (eq ___ (eq 3 3.0)))
 (should (eq ___ (eq '(1 2) '(1 2))))
 (should (eq ___ (eq "Foo" "Foo")))
 (should (eq ___ (eq "Foo" (copy-seq "Foo"))))
 (should (eq ___ (eq "FOO" "Foo"))))


(elisp-koans/deftest
 elisp-koans/equality-distinctions-test-eql ()
 "(eql x y) is true if (eq x y) also it is true if x and y are
numeric of the same type and represent the same number.
(eql x y) also if x and y are the same characters."
 (should (eq ___ (eql 'a 'a)))
 (should (eq ___ (eql 3 3)))
 (should (eq ___ (eql 3 3.0)))
 (should (eq ___ (eql '(1 2) '(1 2))))
 (should (eq ___ (eql '(a b) '(a b))))
 (should (eq ___ (eql ?S ?S)))
 (should (eq ___ (eql "Foo" "Foo")))
 (should (eq ___ (eql "Foo" (copy-seq "Foo"))))
 (should (eq ___ (eql "FOO" "Foo"))))


(elisp-koans/deftest
 elisp-koans/equality-distinctions-test-equal ()
  "(equal x y) is true if (eql x y), or
x and y are lists with equal elements, or
x and y character or bit arrays with equal elements"
  (should (eq ___ (equal 'a 'a)))
  (should (eq ___ (equal 3 3)))
  (should (eq ___ (equal 3 3.0)))
  (should (eq ___ (equal '(1 2) '(1 2))))
  (should (eq ___ (equal '(a b) '(a b))))
  (should (eq ___ (equal '(a b) '(a doesnt-match))))
  (should (eq ___ (equal ?S ?S)))
  (should (eq ___ (equal "Foo" "Foo")))
  (should (eq ___ (equal "Foo" (copy-seq "Foo"))))
  (should (eq ___ (equal "FOO" "Foo"))))


(elisp-koans/deftest
 elisp-koans/equality-distinctions-test-equalp ()
 "(equalp x y) if (equal x y) or
if x and y are strings with the same characters (case independent).
if x and y are arrays with the same dimensions and equal elements
if x and y are numeric of different types but one may be upgraded to
the other type without loss and still exhibit equality."
 (should (eq ___ (equalp 'a 'a)))
 (should (eq ___ (equalp 3 3)))
 (should (eq ___ (equalp 3 3.0)))
 (should (eq ___ (equalp '(1 2) '(1 2))))
 (should (eq ___ (equalp  '(a b) '(a b))))
 (should (eq ___ (equalp  '(a b) '(a doesnt-match))))
 (should (eq ___ (equalp ?S ?S)))
 (should (eq ___ (equalp "Foo" "Foo")))
 (should (eq ___ (equalp "Foo" (copy-seq "Foo"))))
 (should (eq ___ (equalp "FOO" "Foo"))))


(elisp-koans/deftest
 elisp-koans/equality-distinctions-test-numeric-equal ()
 "(= x y) is only for numerics
and can take multiple arguments
if x or y is not numeric there will be a compiler error."
 (should (eq ___ (= 99.0 99 99.000)))
 (should (eq ___ (= 0 1 -1)))
 (should (eq ___ (= (/ 2 3) (/ 6 9) (/ 86 129)))))


;; EQ, EQL, EQUAL, and EQUALP are general equality predicates.
;; Additionally, Lisp also provides the type-specific predicates.
;; For example, STRING= and STRING-EQUAL are predicates for strings.

(elisp-koans/deftest
 elisp-koans/equality-distinctions-test-string-equal ()
 "string-equal is just like string= except that differences in case are ignored."
 (should (eq ___ (string= "Foo" "Foo")))
 (should (eq ___ (string= "Foo" "FOO")))
 (should (eq ___ (string= "together" "frog" :start1 1 :end1 3 :start2 2)))
 (should (eq ___ (string-equal "Foo" "FOO"))))

;;; equality-distinctions.el ends here
