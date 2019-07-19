(ert-deftest test-elisp-koans//replace ()
  "Test replacing in a nested list based on a condition."
  (should (equal '(odd 2 (odd)) (elisp-koans//replace #'oddp 'odd '(1 2 (3))))))


;; test-elisp-koans.el ends here
