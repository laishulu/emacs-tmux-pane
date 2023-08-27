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

(require 'subr-x)

(defcustom tmux-pane-vertical-percent 25
  "Horizontal percent of the vertical pane."
  :type 'integer
  :group 'tmux-pane)

(defcustom tmux-pane-horizontal-percent 25
  "Horizontal percent of the horizontal pane."
  :type 'integer
  :group 'tmux-pane)

(defvar tmux-pane-before-leave-hook nil
  "Hook to run before leaving emacs to tmux.")

(defvar tmux-pane-after-leave-hook nil
  "Hook to run after leaving emacs to tmux.")

(defmacro tmux-pane--ensure-dir (&rest body)
  "Ensure BODY runs in home directory."
  `(let ((default-directory "~"))
     ,@body))

:autoload
(defun tmux-pane--windmove(dir flag)
  "Move focus to window according to DIR and TMUX-CMD."
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                       ; Moving within emacs
    ;; At edges, send command to tmux
    (run-hooks 'tmux-pane-before-leave-hook)
    (tmux-pane--ensure-dir (call-process "tmux" nil nil nil "select-pane" flag))
    (run-hooks 'tmux-pane-after-leave-hook)))

:autoload
(defun tmux-pane-open-vertical ()
  "Open a vertical pane."
  (interactive)
  (tmux-pane--ensure-dir
   (call-process "tmux" nil nil nil
                 "split-window" "-h" "-p" (format "%s" tmux-pane-vertical-percent))))

:autoload
(defun tmux-pane-open-horizontal ()
  "Open a horizontal pane."
  (interactive)
  (tmux-pane--ensure-dir
   (call-process "tmux" nil nil nil
                 "split-window" "-v" "-p" (format "%s" tmux-pane-vertical-percent))))

:autoload
(defun tmux-pane-close ()
  "Close last pane."
  (interactive)
  (tmux-pane--ensure-dir
   (call-process "tmux" nil nil nil "kill-pane" "-t" "{last}")))

:autoload
(defun tmux-pane-rerun ()
  "Rerun command in the last pane."
  (interactive)
  (tmux-pane--ensure-dir
   (call-process "tmux" nil nil nil "send-keys" "-t" "{last}" "C-c")
   (call-process "tmux" nil nil nil "send-keys" "-t" "{last}" "Up" "Enter")))

:autoload
(defun tmux-pane-toggle-vertical()
  "Toggle vertical pane."
  (interactive)
  ;; have more than one pane
  (if (< 1 (length
            (split-string
             (string-trim
              (tmux-pane--ensure-dir
               (shell-command-to-string "tmux list-panes")) "\n"))))
      (tmux-pane-close)
    (tmux-pane-open-vertical)))

:autoload
(defun tmux-pane-toggle-horizontal()
  "Toggle horizontal pane."
  (interactive)
  ;; have more than one pane
  (if (< 1 (length
            (split-string
             (string-trim
              (tmux-pane--ensure-dir
               (shell-command-to-string "tmux list-panes")) "\n"))))
      (tmux-pane-close)))
(tmux-pane-open-horizontal)

:autoload
(defun tmux-pane-omni-window-last ()
  "Switch to the last window of Emacs or tmux."
  (interactive)
  (tmux-pane--windmove "last" "-l"))

:autoload
(defun tmux-pane-omni-window-up ()
  "Switch to the up window of Emacs or tmux."
  (interactive)
  (tmux-pane--windmove "up" "-U"))

:autoload
(defun tmux-pane-omni-window-down ()
  "Switch to the down window of Emacs or tmux."
  (interactive)
  (tmux-pane--windmove "down" "-D"))

:autoload
(defun tmux-pane-omni-window-left ()
  "Switch to the left window of Emacs or tmux."
  (interactive)
  (tmux-pane--windmove "left" "-L"))

:autoload
(defun tmux-pane-omni-window-right ()
  "Switch to the right window of Emacs or tmux."
  (interactive)
  (tmux-pane--windmove "right" "-R"))

(defvar tmux-pane--override-map-enable nil
  "Enabe the override keymap.")

(defvar tmux-pane--override-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-\\") #'tmux-pane-omni-window-last)
    (define-key map (kbd "C-k") #'tmux-pane-omni-window-up)
    (define-key map (kbd "C-j") #'tmux-pane-omni-window-down)
    (define-key map (kbd "C-h") #'tmux-pane-omni-window-left)
    (define-key map (kbd "C-l") #'tmux-pane-omni-window-right)
    map)
  "keymap overriding existing ones.")

(defvar tmux-pane--override-map-alist
  `((tmux-pane--override-map-enable
     .
     ,tmux-pane--override-keymap))
  "Map alist for override.")

(defvar tmux-pane--override-map-alist-order 0
  "Order of map alist in `emulation-mode-map-alists'.")

(defun tmux-pane-in-tmux-p ()
  "Predicate on Emacs run in tmux."
  (string=
   "Emacs"
   (string-trim
    (tmux-pane--ensure-dir
     (shell-command-to-string
      "tmux display-message -p '#{pane_current_command}'")) "\n")))

:autoload
(define-minor-mode tmux-pane-mode
  "Seamlessly navigate between tmux pane and emacs window."
  :init-value nil
  :global t
  (cond
   (tmux-pane-mode
    (add-to-ordered-list
     'emulation-mode-map-alists
     'tmux-pane--override-map-alist
     tmux-pane--override-map-alist-order)
    (setq tmux-pane--override-map-enable t))
   ((not tmux-pane-mode)
    (setq tmux-pane--override-map-enable nil))))

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
