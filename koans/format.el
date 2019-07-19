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
;;   Adapted from google/lisp-koans:koans/format.lsp


;; FORMAT is lisp's counterpart to the c function printf. Refer to
;; http://www.gigamonkeys.com/book/a-few-format-recipes.html for more
;; on this topic.


;; FORMAT takes two fixed parameters. The first one specifies an
;; output stream that the result goes to, and if left as nil, FORMAT
;; will return the output as a string instead. The second parameter
;; specifies the format, where format specifier will be replaced by
;; formatting the rest of the parameters.

(elisp-koans/deftest
 elisp-koans/format-with-plain-text ()
 "If there is no format specifier, FORMAT just returns the string itself."
 (should (equal ___ (format "this is plain text."))))


(elisp-koans/deftest
 elisp-koans/format-with-general-specifier ()
 "%s is a general specifier that translates to the print form of a
parameter."
 (should (equal ___ (format "%s" 42)))
 (should (equal ___ (format "%s" ?C)))
 (should (equal ___ (format "%s %s" "multiple" 'arguments)))
 (should (equal ___ (format "%s" "galaxy far far away")))
 ;; %s can also translate to list
 ;; and parameters to FORMAT are passed by value
 (should (equal ___ (format "%s evaluates to %s"
                            '(/ 8 (- 3 (/ 8 3)))
                            (/ 8 (- 3 (/ 8 3)))))))


(elisp-koans/deftest
 elisp-koans/format-numeric-control-sequences ()
 "=format= accepts other numeric control sequences"
 "=%d= specifies a decimal number"
 (should (equal ___ (format "%d" 42)))
 (should (equal ___ (format "%d" #o52)))
 (should (equal ___ (format "%d" #x2A)))
 "=%o= specifies a decimal number"
 (should (equal ___ (format "%o" 42)))
 "=%x= specifies a hex number with capital characters"
 (should (equal ___ (format "%x" 42)))
 "=%X= specifies a hex number with capital characters"
 (should (equal ___ (format "%X" 42)))
 "=%f= specifies a decimal decimal point number"
 (should (equal ___ (format "%f" 42))))

;; format.el ends here
