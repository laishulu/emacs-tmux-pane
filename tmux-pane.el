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

;;;###autoload
(define-namespace tmux-pane-

(defcustom vertical-percent 25
  "Horizontal percent of the vertical pane."
  :type 'integer
  :group 'tmux-pane)

(defcustom horizontal-percent 25
  "Horizontal percent of the horizontal pane."
  :type 'integer
  :group 'tmux-pane)

(defcustom map-evil-insert-state t
  "Map keybindings in evil insert state"
  :type 'bool
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

:autoload
(defun omni-window-last ()
  "Switch to the last window of Emacs or tmux"
  (interactive)
  (-windmove "last"  "tmux select-pane -l"))

:autoload
(defun omni-window-up ()
  "Switch to the up window of Emacs or tmux"
  (interactive)
  (-windmove "up"  "tmux select-pane -U"))

:autoload
(defun omni-window-down ()
  "Switch to the down window of Emacs or tmux"
  (interactive)
  (-windmove "down"  "tmux select-pane -D"))

:autoload
(defun omni-window-left ()
  "Switch to the left window of Emacs or tmux"
  (interactive)
  (-windmove "left"  "tmux select-pane -L"))

:autoload
(defun omni-window-right ()
  "Switch to the right window of Emacs or tmux"
  (interactive)
  (-windmove "right"  "tmux select-pane -R"))

;; keymap defined by evil 
(defvar ::evil-insert-state-map)

(defvar -saved-evil-insert-state-map
  (make-sparse-keymap)
  "Keymap to keep bindings in original `evil-insert-state-map'")

(defvar -evil-insert-state-map-overrided nil
  "Already overrided `evil-insert-state-map'")

:autoload
(defun override-evil-insert-state-map ()
  "Override `evil-insert-state-map' after saving original bindings."
  (unless (and -evil-insert-state-map-overrided
               (keymapp 'evil-insert-state-map))
    (define-key -saved-evil-insert-state-map (kbd "C-\\")
      (lookup-key evil-insert-state-map (kbd "C-\\")))
    (define-key evil-insert-state-map (kbd "C-\\") #'omni-window-last)

    (define-key -saved-evil-insert-state-map (kbd "C-k")
      (lookup-key evil-insert-state-map (kbd "C-k")))
    (define-key evil-insert-state-map (kbd "C-k") #'omni-window-up)

    (define-key -saved-evil-insert-state-map (kbd "C-j")
      (lookup-key evil-insert-state-map (kbd "C-j")))
    (define-key evil-insert-state-map (kbd "C-j") #'omni-window-down)

    (define-key -saved-evil-insert-state-map (kbd "C-h")
      (lookup-key evil-insert-state-map (kbd "C-h")))
    (define-key evil-insert-state-map (kbd "C-h") #'omni-window-left)

    (define-key -saved-evil-insert-state-map (kbd "C-l")
      (lookup-key evil-insert-state-map (kbd "C-l")))
    (define-key evil-insert-state-map (kbd "C-l") #'omni-window-right)

    (setq -evil-insert-state-map-overrided t)))

:autoload
(defun restore-evil-insert-state-map ()
  "Restore original `evil-insert-state-map'."
  (when (and -evil-insert-state-map-overrided
             (keymapp 'evil-insert-state-map))
    (define-key evil-insert-state-map (kbd "C-\\")
      (lookup-key -saved-evil-insert-state-map (kbd "C-\\")))
    (define-key evil-insert-state-map (kbd "C-k")
      (lookup-key -saved-evil-insert-state-map (kbd "C-k")))
    (define-key evil-insert-state-map (kbd "C-j")
      (lookup-key -saved-evil-insert-state-map (kbd "C-j")))
    (define-key evil-insert-state-map (kbd "C-h")
      (lookup-key -saved-evil-insert-state-map (kbd "C-h")))
    (define-key evil-insert-state-map (kbd "C-l")
      (lookup-key -saved-evil-insert-state-map (kbd "C-l")))
    (setq -evil-insert-state-map-overrided nil)))

:autoload
(define-minor-mode mode
  "Seamlessly navigate between tmux pane and emacs window"
  :init-value nil
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-\\") #'omni-window-last)
            (define-key map (kbd "C-k") #'omni-window-up)
            (define-key map (kbd "C-j") #'omni-window-down)
            (define-key map (kbd "C-h") #'omni-window-left)
            (define-key map (kbd "C-l") #'omni-window-right)
            map)
  (cond
   ((and mode map-evil-insert-state)
    (override-evil-insert-state-map)
    (add-hook 'after-init-hook #'override-evil-insert-state-map))
   ((not mode)
    (when (keymapp 'evil-insert-state-map)
      (restore-evil-insert-state-map)))))

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

(provide 'tmux-pane)
;;; tmux-pane.el ends here
