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
;;   Adapted from google/lisp-koans:koans/functions.lisp

;; The closure gives the returned function access to the bindings, not just the
;; values.  This means that two functions which close over the same variables
;; will always see the same values of those variables if one does a setq.

;; -*- lexical-binding: t; -*-

;; As shown in the line above, `lexical-binding' is set as a buffer local
;; variable. When set to `t' lexical bindings are enabled in the current buffer,
;; as opposed to dynamic bindings.
;;
;; Relevant emacs info page: `(info "(elisp)Lexical Binding")'
;;
;; Unless you have a particular reason not to, I'd recommend using lexical
;; bindings with new elisp source files. Lexical bindings are typical in modern
;; languages and match a common mental model. Dynamic bindings, while powerful,
;; can allow variable bindings to interact in surpising ways.

(defun elisp-koans/two-funs (x)
  "Returns a list of two functions.
The first takes no parameters and returns x.
The second takes one parameter, y, and resets x to the value of y."
  (list (function (lambda () x))
        (function (lambda (y) (setq x y)))))

(elisp-koans/deftest
 elisp-koans/lexical-bindings-closure-interactions ()
 "An illustration of how lexical closures may interact."
  (let ((tangled-funs-1 (elisp-koans/two-funs 1))
        (tangled-funs-2 (elisp-koans/two-funs 2)))
     (should (equal ___ (funcall (first tangled-funs-1))))
     (funcall (second tangled-funs-1) 0)
     (should (equal ___ (funcall (first tangled-funs-1))))

     (should (equal ___ (funcall (first tangled-funs-2))))
     (funcall (second tangled-funs-2) 100)
     (should (equal ___ (funcall (first tangled-funs-2))))))

;;; lexical-bindings.el ends here
