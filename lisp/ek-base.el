;;; ek-base.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;;;###autoload
(defun eg-base--setup-ui ()
  "Setup desired UI settings.
Disable startup messages and set desired splash screen look."
  ;; Disable scroll, menu and tool bars.
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; Inhibit startup message and splash screen.
  (setq inhibit-startup-message t)
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-screen t)
  (setq frame-title-format '("%b %& Emacs"))
  ;; Set typing behaviour.
  (setq ring-bell-function 'ignore
        echo-keystrokes 0.25)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Set performace settings
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 1024 1024))
  (message "Basic settings initialized"))

(add-hook 'after-init-hook 'eg-base--setup-ui)

;;;###autoload
(defun eg-base--setup-file-handling ()
  "Setup file handling behaviour.
Disable backups and autosaves, deduce symlinks by default."
  (setq make-backup-files nil
        auto-save-default nil)
  ;; Set symlink deducing behaviour.
  (setq find-file-visit-truename t
        vc-follow-symlinks t))

(add-hook 'emacs-startup-hook 'eg-base--setup-file-handling)

;;;###autoload
(defun eg-base--set-utf8-coding-system ()
  "Set utf-8 coding system everywhere."
  (setq locale-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt)
                                   'utf-16-le
                                 'utf-8))
  (prefer-coding-system 'utf-8))

(add-hook 'emacs-startup-hook 'eg-base--set-utf8-coding-system)

;;;###autoload
(defun eg-base--enable-window-divider-mode ()
  "Use build in window-divider-mode to draw vertical window borders differently."
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  (add-hook 'after-init-hook #'window-divider-mode))

(add-hook 'emacs-startup-hook 'eg-base--enable-window-divider-mode)

;;;###autoload
(defun eg-base--start-emacs-maximized ()
  "Start emacs maximized."
  (custom-set-variables
   '(initial-frame-alist (quote ((fullscreen . maximized))))))

(add-hook 'emacs-startup-hook 'eg-base--start-emacs-maximized)

;;;###autoload
(defun eg-base--set-text-editing-preferences ()
  "Text editing settings.
* Tab indent or complete;
* Tabs are 4 spaces;
* Use bar coursor;
* Global visual mode;
* Display line numbers in prog mode and text mode;
* Default fill column is 80."
  ;; (delete-selection-mode 1) ;; delete selected text by typing
  (setq-default tab-always-indent 'complete)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default cursor-type '(bar . 2))
  (setq default-fill-column 80
        sentence-end-double-space nil
        save-interprogram-paste-before-kill t)
  (global-visual-line-mode t)
  (show-paren-mode 1)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook 'display-line-numbers-mode))

(add-hook 'emacs-startup-hook 'eg-base--set-text-editing-preferences)

;;;###autoload
(defun eg-base--setup-mouse-behaviour ()
  "Setup ideal mouse behaviour."
  ;; Scroll settings.
  (pixel-scroll-mode 1)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position t)
  ;; Mouse settings
  (setq mouse-drag-copy-region t)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  ;; Activate modes.
  (line-number-mode t)
  (column-number-mode t)
  (mouse-wheel-mode 1))

(add-hook 'emacs-startup-hook 'eg-base--setup-mouse-behaviour)

;;;###autoload
(defun eg-base--set-font ()
  "Set font if available."
  (defvar ek/font "DejaVu Sans Mono")
  (cond
   ((find-font (font-spec :name "Iosevka"))
    (setq ek/font "Iosevka"))
   ((find-font (font-spec :name "DejaVu Sans Mono"))
    (setq ek/font "DejaVu Sans Mono"))
   ((find-font (font-spec :name "Consolas"))
    (setq ek/font "Consolas")))

  (setq ek/font (concat ek/font "-11"))
  (message "Using font: `%s'..." ek/font)

  (set-face-font 'default ek/font)
  (set-face-font 'fixed-pitch ek/font)
  (set-face-font 'variable-pitch "Liberation Serif-13"))

(add-hook 'emacs-startup-hook 'eg-base--set-font)

;;;###autoload
(defun eg-base--setup-window-behaviour ()
  "Set default window behaviour."
  (setq window-combination-resize t
        fit-window-to-buffer-horizontally t
        even-window-sizes 'height-only
        window-sides-vertical nil
        window-resize-pixelwise t)

  (defvar ek/win-param
    '(window-parameters . ((no-other-window . t)
                           (no-delete-other-windows . t))))

  (setq
   display-buffer-alist
   `(("\\*Buffer List\\*" display-buffer-in-side-window
      (side . top) (slot . 0) (window-width . 0.25)
      (preserve-size . (nil . t)) ,ek/win-param)
     ("\\*Tags List\\*" display-buffer-in-side-window
      (side . right) (slot . 0) (window-width . fit-window-to-buffer)
      (preserve-size . (t . nil)) ,ek/win-param)
     ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
      display-buffer-in-side-window
      (side . bottom) (slot . -1) (preserve-size . (nil . t))
      ,ek/win-param)
     ("\\*\\(?:Eshell\\|Compilation\\)\\*" display-buffer-in-side-window
      (side . bottom) (slot . 1) (window-height . 0.25)
      ,ek/win-param)))
  
  (global-set-key (kbd "<f4>") #'window-toggle-side-windows))

(add-hook 'emacs-startup-hook 'eg-base--setup-window-behaviour)

(provide 'ek-base)

;;; ek-base.el ends here
