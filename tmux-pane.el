;;; tmux-pane.el --- Provide integration between emacs window and tmux pane  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; URL: https://github.com/laishulu/emacs-tmux-pane
;; Created: November 1, 2018
;; Keywords: convenience, terminals, tmux, window, pane, navigation, integration
;; Package-Requires: ((names "0.5") (emacs "24") (s "0"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; This package provide integration between Emacs window and tmux pane.
;;; For more information see the README in the github repo.

;;; Code:

;; `define-namespace' is autoloaded, so there's no need to require
;; `names'. However, requiring it here means it will also work for
;; people who don't install through package.el.
(eval-when-compile (require 'names))
(require 'subr-x)

(define-namespace tmux-pane-

(defcustom vertical-percent 25
  "Horizontal percent of the vertical pane."
  :type 'integer
  :group 'tmux-pane)

(defcustom horizontal-percent 25
  "Horizontal percent of the horizontal pane."
  :type 'integer
  :group 'tmux-pane)

:autoload
(defun -windmove(dir tmux-cmd)
  "Move focus to window according to DIR and TMUX-CMD."
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                       ; Moving within emacs
    (shell-command tmux-cmd)))  ; At edges, send command to tmux

:autoload
(defun open-vertical ()
  "Open a vertical pane."
  (interactive)
  (shell-command (format "tmux split-window -h -p %s" vertical-percent)))

:autoload
(defun open-horizontal ()
  "Open a horizontal pane."
  (interactive)
  (shell-command (format "tmux split-window -v -p %s" horizontal-percent)))

:autoload
(defun close ()
  "Close last pane."
  (interactive)
  (shell-command "tmux kill-pane -t {last}"))

:autoload
(defun rerun ()
  "Rerun command in the last pane."
  (interactive)
  (shell-command "tmux send-keys -t {last} C-c")
  (shell-command "tmux send-keys -t {last} Up Enter"))

:autoload
(defun toggle-vertical()
  "Toggle vertical pane."
  (interactive)
  ;; have more than one pane
  (if (< 1 (length
            (split-string (string-trim (shell-command-to-string "tmux list-panes")) "\n")))
      (close)
    (open-vertical)))

:autoload
(defun toggle-horizontal()
  "Toggle horizontal pane."
  (interactive)
  ;; have more than one pane
  (if (< 1 (length
            (split-string (string-trim (shell-command-to-string "tmux list-panes")) "\n")))
      (close)
(open-horizontal)))

;; end of namespace
)

(defun windmove-last ()
  "Focus to the last Emacs window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun tmux-pane-window-last ()
  "Switch to the last window of Emacs or tmux"
  (interactive)
  (tmux-pane--windmove "last"  "tmux select-pane -l"))

(defun tmux-pane-window-up ()
  "Switch to the up window of Emacs or tmux"
  (interactive)
  (tmux-pane--windmove "up"  "tmux select-pane -U"))

(defun tmux-pane-window-down ()
  "Switch to the down window of Emacs or tmux"
  (interactive)
  (tmux-pane--windmove "down"  "tmux select-pane -D"))

(defun tmux-pane-window-left ()
  "Switch to the left window of Emacs or tmux"
  (interactive)
  (tmux-pane--windmove "left"  "tmux select-pane -L"))

(defun tmux-pane-window-right ()
  "Switch to the right window of Emacs or tmux"
  (interactive)
  (tmux-pane--windmove "right"  "tmux select-pane -R"))

(define-minor-mode tmux-pane-mode
  "Seamlessly navigate between tmux pane and emacs window"
  :init-value nil
  :global t
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-\\") #'tmux-pane-window-last)
    (define-key map (kbd "C-k") #'tmux-pane-window-up)
    (define-key map (kbd "C-j") #'tmux-pane-window-down)
    (define-key map (kbd "C-h") #'tmux-pane-window-left)
    (define-key map (kbd "C-l") #'tmux-pane-window-right)
    map))

(provide 'tmux-pane)
;;; tmux-pane.el ends here
