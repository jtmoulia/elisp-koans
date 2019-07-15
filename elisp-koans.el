;; -*- lexical-binding: t; -*-

(require 'cl)


(defconst elisp-koans--blank :BLANK)
(defconst elisp-koans--blanks '(__ ___ ____))

;; TODO: should be using the above vars instead of global defs of _ and friends
(defconst __ elisp-koans--blank)
(defconst ___ elisp-koans--blank)
(defconst ____ elisp-koans--blank)
(defconst elisp-koans--groups-directory (expand-file-name "koans/")
  "Directory where the elisp koan groups are stored")
(defvar elisp-koans-result-buffer-name "*elisp-koans-results*"
  "Name of the elisp-koans result buffer")

;; helpers

(defun elisp-koans//get-koan-groups ()
  (with-current-buffer (find-file-noselect ".koans")
    (goto-char (point-min))
    (read (current-buffer))))

(defun elisp-koans//format-buffer ()
  (org-indent-region (point-min) (point-max))
  (fill-region (point-min) (point-max)))


(defun elisp-koans//delete-whole-line ()
  (interactive)
  (beginning-of-line)
  (delete-region (point) (min (point-max) (+ 1 (line-end-position)))))


(defun elisp-koans//newline-if-not-beginning ()
  "If the cursor isn't at the beginning of the line insert a newline."
  (move-end-of-line nil)
  (if (not (equal (point) (line-beginning-position)))
      (insert "\n\n")))


(cl-defun elisp-koans//org-replace-or-insert-section (name title &key prefix body (level 1))
  "Replace the section with the provided section."
  (if (elisp-koans//org-find-section name)
      (progn
        (elisp-koans//org-delete-section)
        (end-of-buffer)
        (elisp-koans//newline-if-not-beginning)))
  (elisp-koans//org-insert-section title :prefix prefix :body body :level level))


(cl-defun elisp-koans//org-insert-section (title &key prefix body (level 1))
  "Insert a header with TITLE at the provided LEVEL."
  (let ((prefix (if prefix (concat " " prefix) "")))
    (insert (make-string level ?*) prefix " " title "\n\n")
    (if body
        (insert body "\n\n"))))


(defun elisp-koans//org-find-section (name)
  (let* ((header-regex "\\*\\*\\* ")
         (target (concat header-regex ".*" name " ")))
    (or (re-search-forward target nil t)
        (re-search-backward target nil t))))


(defun elisp-koans//org-delete-section ()
  "Delete the section which the cursor is currently in. See the bound
`header-*' arguments for how a section's headers are defined."
  (let* ((header-prefix "*** ")
         (header-regex "\\*\\*\\* ")
         (is-header (lambda () (equal (buffer-substring
                                       (point) (min (point-max) (+ (length header-prefix) (point))))
                                      header-prefix))))
    ;; search backwards to the previous header
    (beginning-of-line)
    (if (not (funcall is-header))
        (progn
          (re-search-backward header-regex)))
    (elisp-koans//delete-whole-line)
    (while (and (< (point) (point-max)) (not (funcall is-header)))
      (elisp-koans//delete-whole-line)
      (beginning-of-line))))


(defun elisp-koans//switch-to-results-buffer (&optional buffer)
  "Switch to the results BUFFER and ensure `org-mode' is enabled."
  (let ((buffer (or buffer (get-buffer-create elisp-koans-result-buffer-name))))
    (switch-to-buffer buffer)
    (unless (eq major-mode 'org-mode)
      (org-mode))))


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

;; Testing
;; TODO refactor out
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
         (description (concat "Advice: " (car descriptors)))
         (test-name (symbol-name name))
         (level 3))
    `(progn
       (elisp-koans//switch-to-results-buffer)
       ;; TODO can we swap out the specific test entry?
       (if-let ((result (catch 'assertion ,@body)))
           (elisp-koans//org-replace-or-insert-section
            ,test-name (concat ,test-name " has damaged your karma")
            :prefix "TODO" :body (concat  ,description "\n\n" result) :level ,level)
         (elisp-koans//org-replace-or-insert-section
          ,test-name (concat ,test-name " has expanded your awareness")
          :body ,description :prefix "DONE" :level ,level)))))


(defun elisp-koans//load-koan-group (group)
  "Load a koan group from `elisp-koans--groups-directory'."
  (let* ((file-name (concat (symbol-name group) ".el"))
         (file-path (concat (file-name-as-directory elisp-koans--groups-directory) file-name)))
    (load-file file-path)))


(defun elisp-koans/run (&optional koans)
  "Run the koans, clear the results buffer and insert results."
  (interactive)
  (elisp-koans//switch-to-results-buffer)
  (erase-buffer)
  (elisp-koans//org-insert-section "Emacs Lisp Koans")
  (dolist (koan-group (elisp-koans//get-koan-groups))
    ;; TODO add section headers
    (elisp-koans//org-insert-section (concat (symbol-name koan-group) " koan group") :level 2)
    (elisp-koans//load-koan-group koan-group)
    (with-current-buffer (find-file-noselect ".koans")
      (goto-char (point-min))
      (read (current-buffer)))
    (end-of-buffer))
  (elisp-koans//format-buffer))


(provide 'elisp-koans)

;;; elisp-koans.el ends here
