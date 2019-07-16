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

(elisp-koans/define-test
 assert-true
 "t is true.  Replace the blank with a t"
 (elisp-koans/assert-true ___))


(elisp-koans/define-test
 assert-false
 "nil is false"
 (elisp-koans/assert-false ___))


(elisp-koans/define-test
 fill-in-the-blank
 "sometimes you will need to fill the blank to complete"
 (elisp-koans/assert-equal ___ 2))


(elisp-koans/define-test
 fill-in-the-blank-string
 "you might be asked if the blank is `equal' to another argument"
 (elisp-koans/assert-equal ___ "hello world"))


(elisp-koans/define-test
 test-true-or-false
 "sometimes you will be asked to evaluate whether statements
are true (t) or false (nil)"
 (elisp-koans/true-or-false? ___ (equal 34 34))
 (elisp-koans/true-or-false? ___ (equal 19 78)))

;;; asserts.el ends here
