;;; vcsh.el --- vcsh integration -*- lexical-binding: t -*-

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Created: 2019-07-15 00:46:28 Monday +0200
;; URL: https://gitlab.com/stepnem/vcsh-el
;; Keywords: vc files
;; License: public domain
;; Version: 0.3
;; Tested-with: GNU Emacs 27
;; Package-Requires: ((magit "2.90.1") (emacs "25"))

;;; Commentary:

;; Original idea by Jonas Bernoulli (see the first two links below).

;; Other than basic "enter" functionality (`vcsh-link', `vcsh-unlink')
;; and some convenience commands (`vcsh-new' to init a repo and add
;; files to it, `vcsh-write-gitignore'), this library provides a
;; global minor mode `vcsh-hack-magit-mode' that advises Magit
;; functions so that `magit-list-repositories' and `magit-status' work
;; with vcsh repos. `vcsh-magit-status' works even without enabling
;; the minor mode.

;; Please note that this library works by creating a regular file
;; named ".git" inside $VCSH_BASE directory (typically $HOME) and does
;; not remove this file automatically, so don't be surprised if your
;; shell suddenly behaves as after "vcsh enter" when inside that
;; directory.  You can use `vcsh-unlink' or simply remove the file to
;; get rid of it.

;; Cf. also:

;; https://github.com/magit/magit/issues/2939
;; https://github.com/magit/magit/issues/460
;; https://github.com/vanicat/magit/blob/t/vcsh/magit-vcsh.el

;; Corrections and constructive feedback appreciated.

;;; Code:

(require 'magit)
(eval-when-compile (require 'subr-x))   ; string-join, when-let

;; (defgroup vcsh () "Vcsh integration."
;;   :group 'magit)

(defun vcsh-absolute-p ()
  "Return non-nil if absolute file paths should be set.
Otherwise use relative paths."
  (not (string= (getenv "VCSH_WORKTREE") "relative")))

(defun vcsh-base ()
  "Return name of vcsh work tree directory."
  (or (getenv "VCSH_BASE") (getenv "HOME")))

(defun vcsh-repo-d ()
  "Return name of directory where vcsh repos are located."
  (or (getenv "VCSH_REPO_D")
      (substitute-env-in-file-name "$XDG_CONFIG_HOME/vcsh/repo.d")))

(defun vcsh-repo-p (dir)
  "Return non-nil if DIR is a vcsh repository."
  (let ((default-directory (file-name-as-directory (vcsh-repo-d))))
    (setq dir (file-truename dir))
    (and (file-accessible-directory-p dir)
         (file-equal-p (file-name-directory (directory-file-name dir))
                       default-directory))))

(defun vcsh-repos ()
  "Return list of vcsh repo names."
  (mapcar #'file-name-base (directory-files (vcsh-repo-d) nil "^[^.]")))

(defun vcsh-read-repo ()
  "Read vcsh repo directory name interactively."
  (completing-read "vcsh repository: " (vcsh-repos) nil t))

;;;###autoload
(defun vcsh-link (repo)
  "Make REPO become the .git directory for vcsh base directory.
This is similar to vcsh \"enter\" command."
  (interactive (list (vcsh-read-repo)))
  (let ((worktree (vcsh-base))
        (repo-path (expand-file-name (concat repo ".git") (vcsh-repo-d))))
    (with-temp-file (expand-file-name ".git" worktree)
      (insert "gitdir: " (if (vcsh-absolute-p) repo-path
                           (file-relative-name repo-path worktree)) "\n"))))

;;;###autoload
(defun vcsh-unlink ()
  "Undo the effect of `vcsh-link' (vcsh \"enter\" command)."
  (interactive)
  (delete-file (expand-file-name ".git" (vcsh-base))))

;;;###autoload
(defun vcsh-magit-status (repo)
  "Make vcsh REPO current (cf. `vcsh-link') and run `magit-status' in it."
  (interactive (list (vcsh-read-repo)))
  (vcsh-link repo)
  (magit-status-setup-buffer (expand-file-name repo (vcsh-repo-d))))

(defun vcsh-command (cmd &rest args)
  "Run vcsh command CMD with ARGS and display the output, if any."
  (when-let ((output (apply #'process-lines "vcsh" cmd args)))
    (display-message-or-buffer
     (concat "\"vcsh " cmd " " (string-join args " ") "\": "
             (string-join output "\n")))))

;;;###autoload
(defun vcsh-new (name files)
  "Init a new vcsh repo and add files to it.
NAME is the repository name, FILES is a list of file names.
This command also calls `vcsh-write-gitignore' for the new repo and,
unless run in `noninteractive' mode, displays its Magit status buffer."
  (interactive (list (read-string "Repo name: ")
                     (let (fls done)
                       (while (not done)
                         (push (read-file-name "File: " (vcsh-base)) fls)
                         (unless (y-or-n-p (format "%s\n\n%s"
                                                   (string-join fls "\n")
                                                   "Add more files? "))
                           (setq done t)))
                       fls)))
  (vcsh-command "init" name)
  (apply #'vcsh-command "run" name "git" "add" files)
  (vcsh-write-gitignore name)
  (unless noninteractive (vcsh-magit-status name)))

;;;###autoload
(defun vcsh-write-gitignore (&optional repo)
  "Run \"vcsh write-gitignore\" for REPO.
With a prefix argument or if REPO is nil, run the command for all vcsh
repositories."
  (interactive (unless current-prefix-arg (list (vcsh-read-repo))))
  (let ((write (apply-partially #'vcsh-command "write-gitignore")))
    (if repo (funcall write repo) (mapc write (vcsh-repos)))))

;;; Hack some Magit functions to work with vcsh repos
(defun vcsh--magit-list-repos-1 (orig &rest args)
  "Make `magit-list-repos-1' consider vcsh git directories.
Checkdoc: can you guess what ORIG and ARGS mean?"
  (let ((dir (car args)))
    (if (and (string-match-p "[^/]\\.git$" dir)
             (vcsh-repo-p dir))
        (list (file-name-as-directory dir))
      (apply orig args))))

(defun vcsh--magit-status-setup-buffer (&optional dir)
  "Make `magit-status-setup-buffer' handle vcsh repositories.
Checkdoc: can you guess what DIR means?"
  (unless dir (setq dir default-directory))
  (when (vcsh-repo-p dir) (vcsh-link dir)))

;;;###autoload
(define-minor-mode vcsh-hack-magit-mode
  "Advise Magit functions to work with vcsh repositories.
In particular, when this mode is enabled, `magit-status' and
`magit-list-repositories' should work as expected."
  :global t
  (if vcsh-hack-magit-mode
      (progn
        (advice-add 'magit-list-repos-1 :around #'vcsh--magit-list-repos-1)
        (advice-add 'magit-status-setup-buffer :before
                    #'vcsh--magit-status-setup-buffer))
    (advice-remove 'magit-list-repos-1 #'vcsh--magit-list-repos-1)
    (advice-remove 'magit-status-setup-buffer
                   #'vcsh--magit-status-setup-buffer)))

(defun vcsh-unload-function ()
  "Clean up after the vcsh library."
  (vcsh-hack-magit-mode -1)
  nil)

(defun vcsh-reload ()
  "Reload the vcsh library."
  (interactive)
  (unload-feature 'vcsh t)
  (require 'vcsh))

(provide 'vcsh)
;;; vcsh.el ends here
