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
;;   based on python koans: about_dictionaries.py
;;
;;   Adapted from google/lisp-koans:koans/hash-tables.lisp

;; Relevant emacs info page: `(info "(elisp)Hash Tables")'

(elisp-koans/define-test
 test-create-hash-table
 "make hash table with make-hash-table"
 (let ((my-hash-table))
   (setf my-hash-table (make-hash-table))
   (elisp-koans/true-or-false? ___ (typep my-hash-table 'hash-table))
   (elisp-koans/true-or-false? ___  (hash-table-p my-hash-table))
   (elisp-koans/true-or-false? ___  (hash-table-p '(3 3 3)))
   (elisp-koans/assert-equal ___ (hash-table-count my-hash-table))))


(elisp-koans/define-test
 test-hash-table-access
 "gethash is for accessing hash tables"
 (let ((table-of-cube-roots (make-hash-table)))
   "assign the key-value pair 1->'uno'"
   (setf (gethash 1 table-of-cube-roots) "uno")
   (elisp-koans/assert-equal "uno" (gethash 1 table-of-cube-roots))
   (elisp-koans/assert-equal 1 (hash-table-count table-of-cube-roots))

   (setf (gethash 8 table-of-cube-roots) 2)
   (setf (gethash -3 table-of-cube-roots) -27)
   (elisp-koans/assert-equal ___ (gethash -3 table-of-cube-roots))
   (elisp-koans/assert-equal ___ (hash-table-count table-of-cube-roots))

   "accessing unset keys returns nil"
   (elisp-koans/assert-equal ___ (gethash 125 table-of-cube-roots))))


(elisp-koans/define-test test-hash-key-equality
  "hash tables need to know how to tell if two keys are equivalent.
  The programmer must be careful to know which equality predicate is right."
  (let ((hash-table-eq nil)
        (hash-table-equal nil)
        (hash-table-default nil))

    "define three hash tables, with different equality tests"
    (setf hash-table-eq (make-hash-table :test #'eq))
    (setf hash-table-equal (make-hash-table :test #'equal))
    (setf hash-table-default (make-hash-table))

    "add the same string twice, to each"
    (setf (gethash "one" hash-table-eq) "uno")
    (setf (gethash "one" hash-table-eq) "uno")

    (setf (gethash "one" hash-table-equal) "uno")
    (setf (gethash "one" hash-table-equal) "uno")

    (setf (gethash "one" hash-table-default) "uno")
    (setf (gethash "one" hash-table-default) "uno")

    "count how many unique key-value pairs in each"
    (elisp-koans/assert-equal ___ (hash-table-count hash-table-eq))
    (elisp-koans/assert-equal ___ (hash-table-count hash-table-equal))
    (elisp-koans/assert-equal ___ (hash-table-count hash-table-default))))


(elisp-koans/define-test test-hash-table-equality
  (let ((h1 (make-hash-table :test #'equal))
        (h2 (make-hash-table :test #'equal)))
    (setf (gethash "one" h1) "yat")
    (setf (gethash "one" h2) "yat")
    (setf (gethash "two" h1) "yi")
    (setf (gethash "two" h2) "yi")
    (elisp-koans/true-or-false? ___ (eq h1 h2))
    (elisp-koans/true-or-false? ___ (equal h1 h2))
    (elisp-koans/true-or-false? ___ (equalp h1 h2))))


(elisp-koans/define-test test-changing-hash-tables
  (let ((babel-fish (make-hash-table :test #'equal))
        (expected (make-hash-table :test #'equal)))
    (setf (gethash "one" babel-fish) "uno")
    (setf (gethash "two" babel-fish) "dos")
    (setf (gethash "one" expected) "eins")
    (setf (gethash "two" expected) "zwei")

    (setf (gethash "one" babel-fish) "eins")
    (setf (gethash "two" babel-fish) ____)

    (elisp-koans/assert-true (equalp babel-fish expected))))


(elisp-koans/define-test test-hash-key-membership
  "hash tables use multiple value return to tell you if the key exists"
  (let ((prev-pres (make-hash-table :test #'equal))
        (value-and-exists nil))
    (setf (gethash "Obama" prev-pres) "Bush")
    (setf (gethash "Lincoln" prev-pres) "Buchanan")
    (setf (gethash "Washington" prev-pres) nil)

    (setf value-and-exists (multiple-value-list (gethash "Obama" prev-pres)))
    (elisp-koans/assert-equal value-and-exists '("Bush" t))
    (setf value-and-exists (multiple-value-list (gethash "Lincoln" prev-pres)))
    (elisp-koans/assert-equal value-and-exists ____)
    (setf value-and-exists (multiple-value-list (gethash "Washington" prev-pres)))
    (elisp-koans/assert-equal value-and-exists ____)
    (setf value-and-exists (multiple-value-list (gethash "Franklin" prev-pres)))
    (elisp-koans/assert-equal value-and-exists ____)))


(elisp-koans/define-test test-make-your-own-hash-table
  "make a hash table that meets the following conditions"
  (let ((colors (make-hash-table))
        values)
    (elisp-koans/assert-equal (hash-table-count colors) 4)
    (setf values (list (gethash "blue" colors)
                       (gethash "green" colors)
                       (gethash "red" colors)))
    (elisp-koans/assert-equal values '((0 0 1) (0 1 0) (1 0 0)))))

;;; hash-tables.el ends here
