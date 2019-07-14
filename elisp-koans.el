(defconst elisp-koans--blank :BLANK)
(defconst elisp-koans--__ elisp-koans--blank)
(defconst elisp-koans--___ elisp-koans--blank)
(defconst elisp-koans--____ elisp-koans--blank)
(defconst elisp-koans--blanks (list elisp-koans--__ elisp-koans--___ elisp-koans--____))

;; TODO: should be using the above vars instead of global defs of _ and friends
(defconst __ elisp-koans--blank)
(defconst ___ elisp-koans--blank)
(defconst ____ elisp-koans--blank)
(defconst elisp-koans--groups-directory (expand-file-name "koans/")
  "Directory where the elisp koan groups are stored")
(defvar elisp-koans--koan-groups (with-current-buffer (find-file-noselect ".koans")
                                   (goto-char (point-min))
                                   (read (current-buffer)))
  "List of koan groups")

;; helpers

(defun elisp-koans//string-strip-newlines (string)
  (replace-regexp-in-string "[\n ]+" " " string))


(defun elisp-koans//replace (target substitution lst)
  (if (null lst)
      '()
    (let* ((head (car lst))
           (tail (cdr lst))
           (next-head (if (listp head) (elisp-koans//replace target substitution head)
                        (if (equal target head) substitution head))))
        (cons next-head (elisp-koans//replace target substitution tail)))))


;; Testing helpers
(if nil
    (elisp-koans//replace '$ 10 '(20 + $ * ( 3 + $ + 5 * (sqrt ($ - 5)))))
  )

;; assertion definitions

(defun elisp-koans/assert-true (arg)
  "Assert ARG is true, raising an error if not."
  (if (eq arg elisp-koans--blank)
      (throw 'assertion "Replace `___' with the correct value."))
  (unless arg
    (throw 'assertion "Fix the assertion!")))


(defun elisp-koans/assert-false (arg)
  "Assert ARG is false, raising an error if not."
  (elisp-koans/assert-true (not arg)))


(defun elisp-koans/assert-equal (left right)
  "Assert LEFT equals RIGHT. Uses `equal' for comparison."
  (elisp-koans/assert-true (equal left right)))


(defun elisp-koans/assert-eq (left right)
  "Assert LEFT equals RIGHT. Uses `eq' for comparison."
  (elisp-koans/assert-true (eq left right)))


(defun elisp-koans/true-or-false? (left right)
  "Assert LEFT equals RIGHT. Uses `equal' for comparison."
  (if left
      (elisp-koans/assert-true right)
    (elisp-koans/assert-false right)))


(defmacro elisp-koans/define-test (name &rest form)
  "Define a test called NAME with DESCRIPTION a body of FORM.."
  (let* ((body (seq-filter (lambda (elt) (not (stringp elt))) form))
         (descriptors (seq-map #'elisp-koans//string-strip-newlines (seq-filter #'stringp form)))
         (description (car descriptors))
         (test-name (symbol-name name))
         (test-descriptor (if description (format "%s - %s" test-name description)
                            test-name)))
    `(if-let ((result (catch 'assertion ,@body)))
         (message (format "FAILED: %s\n\t%s" ,test-descriptor result))
       (message (format "SUCCESS! %s" ,test-descriptor)))))


(defun elisp-koans//load-koan-group (group)
  "Load a koan group from `elisp-koans--groups-directory'."
  (let* ((file-name (concat (symbol-name group) ".el"))
         (file-path (concat (file-name-as-directory elisp-koans--groups-directory) file-name)))
    (load-file file-path)))


(defun elisp-koans/run ()
  "Run the koans."
  (interactive)
  (dolist (koan-group elisp-koans--koan-groups)
    (elisp-koans//load-koan-group koan-group)))


(provide 'elisp-koans)

;;; elisp-koans.el ends here
