;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they

;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "JetBrains Mono" :size 13.0))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq catppuccin-flavor 'macchiato)
(setq doom-theme 'catppuccin)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over

;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; NAVIGATION
;; I like to navigate windows with CTRL+{hjkl} regardless of the current context
(defun my/save-and-evil-window-left ()
  (interactive)
  (unless (derived-mode-p 'term-mode 'vterm-mode)
    (save-buffer))
  (evil-window-left 1))

(defun my/save-and-evil-window-right ()
  (interactive)
  (unless (derived-mode-p 'term-mode 'vterm-mode)
    (save-buffer))
  (evil-window-right 1))

(defun my/save-and-evil-window-up ()
  (interactive)
  (unless (derived-mode-p 'term-mode 'vterm-mode)
    (save-buffer))
  (evil-window-up 1))

(defun my/save-and-evil-window-down ()
  (interactive)
  (unless (derived-mode-p 'term-mode 'vterm-mode)
    (save-buffer))
  (evil-window-down 1))

(map! :map 'override
      "C-h" #'my/save-and-evil-window-left
      "C-j" #'my/save-and-evil-window-down
      "C-k" #'my/save-and-evil-window-up
      "C-l" #'my/save-and-evil-window-right)

(map! :map evil-insert-state-map
      "C-h" #'my/save-and-evil-window-left
      "C-j" #'my/save-and-evil-window-down
      "C-k" #'my/save-and-evil-window-up
      "C-l" #'my/save-and-evil-window-right)

;; vterm will capture these commands in insert mode
(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm-send-C-c
      :ni "C-h" #'my/save-and-evil-window-left
      :ni "C-j" #'my/save-and-evil-window-down
      :ni "C-k" #'my/save-and-evil-window-up
      :ni "C-l" #'my/save-and-evil-window-right)

;; Quick switch workspace
(map! :after persp-mode
      :leader
      "j d" #'+workspace/display
      ;; "j 0" #'+workspace/switch-to-0
      "j 1" #'+workspace/switch-to-0
      "j 2" #'+workspace/switch-to-1
      "j 3" #'+workspace/switch-to-2
      "j 4" #'+workspace/switch-to-3
      "j 5" #'+workspace/switch-to-4
      )


;; open in main workspace
(setq persp-emacsclient-init-frame-behaviour-override "main")


;; Stop the debugger from opening when pressing <ESC> to quit during a file find
(setq debug-on-quit nil)

(defun my-refresh-pdf-buffers ()
  "Refresh all PDF buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eq major-mode 'pdf-view-mode) (buffer-file-name))
        (revert-buffer t t t)
        ))))

(defun my-start-pdf-refresh-timer ()
  "Start a timer to refresh all PDF buffers every 5 seconds."
  (run-at-time "0 sec" 5 'my-refresh-pdf-buffers))

(defun my-stop-pdf-refresh-timer ()
  "Stop the PDF refresh timer."
  (cancel-function-timers 'my-refresh-pdf-buffers))

;; Start the PDF refresh timer
(my-start-pdf-refresh-timer)

;; Visual delimiter from elliott
(defun add-window-divider ()
  (set-face-background 'vertical-border "#555555")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))
  (setq window-divider-default-places t
        window-divider-default-bottom-width 8
        window-divider-default-right-width 8)
  (window-divider-mode))
(add-hook 'after-init-hook 'add-window-divider)
(add-hook 'server-after-make-frame-hook 'add-window-divider)

(require 'treesit)
(add-hook 'python-ts-mode-hook #'treesitter-context-mode)

(add-hook 'python-mode-hook
          (defun which-function-header-line-enable ()
            (require 'which-func)
            (setq header-line-format
                  '((:eval (which-function))))))

;; (which-function-mode)
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
            ;; We remove Which Function Mode from the mode line, because it's mostly
            ;; invisible here anyway.
            (assq-delete-all 'which-func-mode mode-line-misc-info))

(load! "work.el")
