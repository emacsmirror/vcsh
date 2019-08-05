;;; vcsh.el --- vcsh integration -*- lexical-binding: t -*-

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Created: 2019-07-15 00:46:28 Monday +0200
;; Keywords: vc files
;; Licence: public domain
;; Tested-with: GNU Emacs 27

;;; Commentary:

;; Original idea by Jonas Bernoulli (see the first two links below).

;; Other than basic "enter" functionality (`vcsh-link', `vcsh-unlink'),
;; this library provides a minor mode, `vcsh-hack-magit-mode', that
;; advises Magit functions so that `magit-list-repositories' and
;; `magit-status' work with vcsh repos.  `vcsh-magit-status' works
;; even without enabling the minor mode.

;; Please note that this library works by creating a regular file
;; named ".git" inside $VCSH_BASE directory (typically $HOME) and does
;; not remove this file automatically, so don't be surprised if your
;; shell suddenly behaves as after "vcsh enter" when inside that
;; directory.  You can use `vcsh-unlink' or simply remove the file to
;; get rid of it.

;; Cf. also:

;; https://github.com/magit/magit/issues/2939
;; https://github.com/magit/magit/issues/460
;; https://github.com/vanicat/magit/blob/t/vcsh/vcsh.el

;; Corrections and constructive feedback appreciated.

;;; Code:

(require 'magit)

(defconst vcsh-version "0.2"
  "Currently loaded version of the `vcsh' library.")

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
    (setq dir (file-truename dir)))
  (and (file-accessible-directory-p dir)
       (file-equal-p (file-name-directory (directory-file-name dir))
                     (vcsh-repo-d))))

(defun vcsh-read-repo ()
  "Read vcsh repo directory name interactively."
  (completing-read "vcsh repository: "
                   (directory-files (vcsh-repo-d) nil "^[^.]")
                   nil t))

;;;###autoload
(defun vcsh-link (repo)
  "Make REPO become the .git directory for vcsh base directory.
This is similar to vcsh \"enter\" command."
  (interactive (list (vcsh-read-repo)))
  (let ((worktree (vcsh-base))
        (repo-path (expand-file-name repo (vcsh-repo-d))))
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
  "Cleanup after the vcsh library."
  (vcsh-hack-magit-mode -1)
  nil)

(defun vcsh-reload ()
  "Reload the `vcsh' library."
  (interactive)
  (unload-feature 'vcsh t)
  (require 'vcsh))

(provide 'vcsh)
;;; vcsh.el ends here
