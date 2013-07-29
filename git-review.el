;; git-review.el --- A user interface for git-review

;; Copyright (C) 2013 Alexey Zasimov <zasimov@gmail.com<

;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This file contains an interface for the git review tool.
;;
;; To install: put this file on the load-path and place the following
;; in your .emacs file:
;;
;;    (require 'git-review)
;;
;; To start: `M-x git-review'

(require 'widget)

(defvar git-review-selected-changeset)

(defun skip-bash-escape (s)
  (if (snull s)
      s
      (if (string-equal (scar s) "m")
	  (scdr s)
	  (skip-bash-escape (scdr s)))
      )
)

(defun remove-bash-escapes (s)
  (if (snull s)
      s
      (if (string-equal (scar s) "")
	  (if (and (>= (length s) 2)
	           (string-equal (substring s 0 2) "["))
	      (remove-bash-escapes (skip-bash-escape (scdr (scdr s))))
	      (scons (scar s) (remove-bash-escapes (scdr s))))
	  (scons (scar s) (remove-bash-escapes (scdr s))))
      )
)

(defun scons (a b)
  (concat a b)
)

(defun scar (s)
    (substring s 0 1)
)

(defun snull (s)
  (or (null s) (string-equal s ""))
)

(defun scdr (s)
  (if (snull s)
      nil
      (substring s 1 (length s)))
)

(defun git-review-split (s)
  (if (string-match "^\\([0-9]+\\)\\(.*\\)" s)
      (list (substring s 0 (match-beginning 2))
            (substring s (match-beginning 2) (match-end 2))))
)

(defun run-git-review-l ()
  (split-string (shell-command-to-string "git review -l") "\n")
)

(defun git-review-available-list ()
  (delq nil (mapcar (lambda (x) (git-review-split (remove-bash-escapes x)))
		    (run-git-review-l)))
)

(defun git-review-add-reviews (rb reviews)
  (if (not (null reviews))
      (dolist (review reviews)
	(let ( (value (car review))
  	       (caption (car (cdr review))) )
	  (widget-radio-add-item rb
            (list 'item :format (concat "%v " caption "\n") :value value))
	))
      )
)

(defun git-review-create-menu (git-reviews)
  (let ( (selected (car (car git-reviews)))
         (rb (widget-create 'radio-button-choice
		 :value "One"
		 :notify (lambda (widget &rest ignore)
			   (message "You selected %s changeset"
				    (widget-value widget))
			   (setq git-review-selected-changeset (widget-value widget))
			   ))) )
      (git-review-add-reviews rb git-reviews)
      )
)

(defun git-review-select-changeset (changeset)
  (shell-command (concat "git review -d " changeset) "git-review-result")
)

(defun git-review-buffer ()
  (interactive)
  (switch-to-buffer "*git-review*")
  (kill-all-local-variables)
  (setq git-review-selected-changeset nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Select changeset:\n\n")
  (git-review-create-menu (git-review-available-list))
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (if (null git-review-selected-changeset)
			     (error "Please select changeset")
			     (git-review-select-changeset git-review-selected-changeset)
			     (switch-to-buffer "git-review-result")
			     (kill-buffer "*git-review*")
			     ))
		 "Go to changeset branch")
  (use-local-map widget-keymap)
  (widget-setup)
)

(defun git-review-find-git-repo (dir)
  (if (string= "/" dir)
      nil
      (if (file-exists-p (expand-file-name ".git/" dir))
	  dir
          (git-review-find-git-repo (expand-file-name "../" dir)))))

(defun git-has-unstaged-changes ()
  (/= 0 (shell-command "git diff-files --quiet --ignore-submodules --"))
)

(defun git-has-uncommited-changes ()
  (/= 0 (shell-command "git diff-index --cached --quiet HEAD --ignore-submodules --"))
)

(defun git-has-local-changes ()
  (shell-command "git update-index -q --ignore-submodules --refresh")  
  (or (git-has-unstaged-changes)
      (git-has-uncommited-changes))
)

(defun git-review-safe ()
  (if (git-has-local-changes)
      (error "Current branch has local changes. Please commit or stash them.")
      (git-review-buffer))
)

(defun git-review ()
  (interactive)
  (let ( (gitdir (git-review-find-git-repo (pwd))) )
    (if (not (null gitdir))
	(git-review-safe)
      (error "Not a git repository."))))

(provide 'git-review)
;;; git-review.el ends here