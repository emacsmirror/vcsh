;; -*- lexical-binding: t -*-
(require 'vcsh)
;; (require 'magit-tests)

(defun vcsh-tests-log (fmt &rest args)
  "Log a message specified by FMT and ARGS.
The arguments are passed to `format'."
  (let ((warning-prefix-function (lambda (&rest _) '(:warning ""))))
    (display-warning nil (apply #'format fmt args))))

;; (defun vcsh-tests-dump-envvars (&optional comment)
;;   "Dump values of $HOME, $XDG_CONFIG_HOME and $VCSH_REPO_D,
;; as well as return value of `vcsh-repo-d'."
;;   (when comment (vcsh-tests-log comment))
;;   (loop for var in '("HOME" "XDG_CONFIG_HOME" "VCSH_REPO_D")
;;         do (vcsh-tests-log (concat "$" var ": " (getenv var))))
;;   (vcsh-tests-log "\nvcsh-repo-d returns: %s" (vcsh-repo-d)))

(defmacro vcsh-tests-letenv (specs &rest body)
  "Execute BODY with each env VAR bound to VAL.

\(fn ((VAR VAL)...) BODY...)"
  (declare (indent 1) (debug ((&rest form form) body)))
  `(let ((process-environment
          (append
           (list ,@(mapcar (lambda (spec) (macroexp-let2 nil cadr (cadr spec)
                                       `(concat ,(car spec)
                                                (when ,cadr "=")
                                                ,cadr)))
                           specs))
           process-environment)))
     ,@body))

;; ;;; Test of `vcsh-tests-letenv' macro sanity:
;; (let* ((var "HOME")
;;        (before (getenv var))
;;        inside-before
;;        inside)
;;   (vcsh-tests-letenv ((var (getenv var))
;;                       ("VCSH_REPO_D" "J"))
;;     (setq inside-before (getenv var)
;;           inside (setenv var "SET")))
;;   (message "Before: %s; Inside before: %s; Inside: %s; After: %s; %s%s"
;;            before inside-before inside (getenv var) "VCSH_REPO_D: "
;;            (getenv "VCSH_REPO_D")))

(ert-deftest vcsh-basic-sanity ()
  (should (equal (expand-file-name "~")
                 (substitute-env-in-file-name "$HOME")))
  (vcsh-tests-letenv (("HOME" "/stuff"))
    (should (equal (expand-file-name "~")
                   (substitute-env-in-file-name "$HOME")))))

(ert-deftest vcsh-repo-d ()
  (should (equal (vcsh-tests-letenv (("VCSH_REPO_D" "VRD")
                                     ("XDG_CONFIG_HOME" "XCH")
                                     ("HOME" "H"))
                   (vcsh-repo-d))
                 "VRD"))
  (should (equal (vcsh-tests-letenv (("VCSH_REPO_D")
                                     ("XDG_CONFIG_HOME" "XCH")
                                     ("HOME" "H"))
                   (vcsh-repo-d))
                 "XCH/vcsh/repo.d"))
  (should (equal (vcsh-tests-letenv (("VCSH_REPO_D")
                                     ("XDG_CONFIG_HOME")
                                     ("HOME" "H"))
                   (vcsh-repo-d))
                 "H/.config/vcsh/repo.d")))

(ert-deftest vcsh-repo-p ()
  (let (tmpdir repos)
    (vcsh-tests-letenv (("VCSH_REPO_D" (getenv "VCSH_REPO_D")))
      (when (file-accessible-directory-p (vcsh-repo-d))
        (setq repos (directory-files (vcsh-repo-d) t "^[^.]" t)))
      (unwind-protect
          (progn
            (unless repos
              (setq tmpdir (make-temp-file "vcsh-tests-repo-d" t))
              (vcsh-tests-log "No vcsh repos found, %s%s"
                              "using temporary test dir: " tmpdir)
              (setenv "VCSH_REPO_D" tmpdir)
              (make-directory (expand-file-name "fake.git" tmpdir))
              (unless (setq repos (directory-files (vcsh-repo-d) t "^[^.]" t))
                (error "This should never happen, giving up")))
            (should-not (vcsh-repo-p "~"))
            (should-not (vcsh-repo-p (vcsh-repo-d)))
            (dolist (repo repos)
              (should (vcsh-repo-p repo))
              (should (vcsh-repo-p (file-name-as-directory repo)))
              (should (vcsh-repo-p (file-name-nondirectory repo)))))
        (when tmpdir (delete-directory tmpdir t nil))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
