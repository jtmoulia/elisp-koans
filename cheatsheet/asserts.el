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
;;   Adapted from google/lisp-koans:koans/asserts.lsp

(elisp-koans/deftest
 elisp-koans/asserts-true ()
 "`t' is true. Replace the blank with `t' to assert it is true"
 (should t))


(elisp-koans/deftest
 elisp-koans/asserts-false ()
 "`nil' is false. Replace the blank with `nil' to assert it is not true"
 (should-not nil))


(elisp-koans/deftest
 elisp-koans/asserts-fill-in-the-blank ()
 "sometimes you will need to fill in the blank of an expression"
 (should (eq 2 2)))


(elisp-koans/deftest
 elisp-koans/asserts-eq-true-or-false ()
 "a test might contain multiple statements"
 (should (eq t (equal 34 34)))
 (should (eq nil (equal 19 78))))


(elisp-koans/deftest
 elisp-koans/asserts-should-error ()
 "try filling out the type of error, in this case an `\'arith-error'"
 (should-error (/ 1 0) :type 'arith-error))

;;; asserts.el ends here
