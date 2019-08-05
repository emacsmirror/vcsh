(require 'vcsh)

(ert-deftest vcsh-repo-p ()
  "Test `vcsh-repo-p'."
  (should-not (vcsh-repo-p "~"))
  (should-not (vcsh-repo-p (vcsh-repo-d)))
  (let ((repos (directory-files (vcsh-repo-d) t "^[^.]")))
    (dolist (repo repos)
      (should (vcsh-repo-p repo))
      (should (vcsh-repo-p (file-name-nondirectory repo))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
