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
;;   Adapted from google/lisp-koans:koans/hash-tables.lsp

;; Relevant emacs info page: `(info "(elisp)Hash Tables")'

(elisp-koans/deftest
 elisp-koans/hash-tables-make ()
 "make a new hash table with `make-hash-table'"
 (let ((my-hash-table))
   (setf my-hash-table (make-hash-table))
   (should (eq ___ (typep my-hash-table 'hash-table)))
   (should (eq ___  (hash-table-p my-hash-table)))
   (should (eq ___  (hash-table-p '(3 3 3))))
   (should (eq ___ (hash-table-count my-hash-table)))))


(elisp-koans/deftest
 elisp-koans/hash-tables-access ()
 "`gethash' is for accessing hash tables"
 (let ((table-of-cube-roots (make-hash-table)))
   "assign the key-value pair 1->'uno'"
   (setf (gethash 1 table-of-cube-roots) "uno")
   (should (equal "uno" (gethash 1 table-of-cube-roots)))
   (should (equal 1 (hash-table-count table-of-cube-roots)))

   (setf (gethash 8 table-of-cube-roots) 2)
   (setf (gethash -3 table-of-cube-roots) -27)
   (should (equal ___ (gethash -3 table-of-cube-roots)))
   (should (equal ___ (hash-table-count table-of-cube-roots)))

   "accessing unset keys returns nil"
   (should (equal ___ (gethash 125 table-of-cube-roots)))))


(elisp-koans/deftest
 elisp-koans/hash-tables-key-equality ()
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
   (should (eq ___ (hash-table-count hash-table-eq)))
   (should (eq ___ (hash-table-count hash-table-equal)))
   (should (eq ___ (hash-table-count hash-table-default)))))


(elisp-koans/deftest
 elisp-koans/hash-tables-equality ()
 "let's see how equality predicates work with hash tables"
 (let ((h1 (make-hash-table :test #'equal))
       (h2 (make-hash-table :test #'equal)))
   (setf (gethash "one" h1) "yat")
   (setf (gethash "one" h2) "yat")
   (setf (gethash "two" h1) "yi")
   (setf (gethash "two" h2) "yi")
   (should (eq ___ (eq h1 h2)))
   (should (eq ___ (equal h1 h2)))
   (should (eq ___ (equalp h1 h2)))))


(elisp-koans/deftest
 elisp-koans/hash-tables-changing ()
 "hash tables can be modified using `setf'"
 (let ((babel-fish (make-hash-table :test #'equal))
       (expected (make-hash-table :test #'equal)))
   (setf (gethash "one" babel-fish) "uno")
   (setf (gethash "two" babel-fish) "dos")
   (setf (gethash "one" expected) "eins")
   (setf (gethash "two" expected) "zwei")

   (setf (gethash "one" babel-fish) "eins")
   (setf (gethash "two" babel-fish) ____)

   (should (equalp babel-fish expected))))


(elisp-koans/deftest
 elisp-koans/hash-tables-key-membership ()
 "hash tables use multiple value return to tell you if the key exists"
 (let ((prev-pres (make-hash-table :test #'equal))
       (value-and-exists nil))
   (setf (gethash "Obama" prev-pres) "Bush")
   (setf (gethash "Lincoln" prev-pres) "Buchanan")
   (setf (gethash "Washington" prev-pres) nil)

   (setf value-and-exists (multiple-value-list (gethash "Obama" prev-pres)))
   (should (equal value-and-exists '("Bush" t)))
   (setf value-and-exists (multiple-value-list (gethash "Lincoln" prev-pres)))
   (should (equal value-and-exists ____))
   (setf value-and-exists (multiple-value-list (gethash "Washington" prev-pres)))
   (should (equal value-and-exists ____))
   (setf value-and-exists (multiple-value-list (gethash "Franklin" prev-pres)))
   (should (equal value-and-exists ____))))


(elisp-koans/deftest
 elisp-koans/hash-tables-make-your-own ()
 "make a hash table that meets the following conditions"
 (let ((colors (make-hash-table))
       values)
   (should (equal (hash-table-count colors) 4))
   (setf values (list (gethash "blue" colors)
                      (gethash "green" colors)
                      (gethash "red" colors)))
   (should (equal values '((0 0 1) (0 1 0) (1 0 0))))))

;;; hash-tables.el ends here
