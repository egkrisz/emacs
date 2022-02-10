;;; init.el --- the heart of the beast -*- lexical-binding: t; -*-

;;; Commentary:

;; This file sets up my personal Emacs configuration.
;;
;; It is structured in an org-mode like manner using code folding
;; enabled by outline-mode in a combination with outshine-mode.
;; A first level heading is declared with ";;;" followed by a space and the name of the heading.
;; Every extra delimiter will increase heading levels accordingly.
;; <Tab> can be used to toggle between hiding and showing a given heading block.
;; S-<Tab> hides or shows all heading levels.

;;; Code:
;;;; Bootstrap straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Package management

;; Macro for loading builting packages with related configuration.
(defmacro ek-req (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'ek-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

;; Macro for loading packages from Melpa via straight.el with related configuration.
(defmacro ek-pkg (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (straight-use-package ,package)
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'ek-emacs (format "Loading `%s' failed" ,package) :warning))))

;; Using general.el for keybinding management.
(ek-pkg 'general
  (general-auto-unbind-keys)
  (general-override-mode t))

;;;; Load the auxiliary configuration files

;; Auxiliary lisp files related to this config are stored in ~/.emacs.d/lisp which is added to the load-path.
(add-to-list 'load-path "~/.emacs.d/lisp")

;; This package contains the basic UI, input, display and window behaviour configurations.
(require 'ek-base-settings)

;; Each configuration group from the included file has to be loaded
;; by executing the corresponding helper functions:
;; eg-base--setup-ui                      -- hides menubar and scrollbar
;; eg-base--setup-file-handling           -- disables autosaving and backups
;; eg-base--set-utf8-coding-system        -- sets up UTF-8 charachter handling
;; eg-base--enable-window-divider-mode    -- enables window divider mode
;; eg-base--start-emacs-maximized         -- starts emacs maximized
;; eg-base--set-text-editing-preferences  -- basic editor settings
;; eg-base--setup-mouse-behaviour         -- sets desired mouse behaviour
;; eg-base--set-font                      -- sets up the default font based on availablility 
;; eg-base--setup-window-behaviour        -- sets up side-window behaviour

;; The commands are added to the after-init-hook and emacs-startup-hook.
(add-hook 'after-init-hook    'eg-base--setup-ui)
(add-hook 'emacs-startup-hook 'eg-base--setup-file-handling)
(add-hook 'emacs-startup-hook 'eg-base--set-utf8-coding-system)
(add-hook 'emacs-startup-hook 'eg-base--enable-window-divider-mode)
(add-hook 'emacs-startup-hook 'eg-base--start-emacs-maximized)
(add-hook 'emacs-startup-hook 'eg-base--set-text-editing-preferences)
(add-hook 'emacs-startup-hook 'eg-base--setup-mouse-behaviour)
(add-hook 'emacs-startup-hook 'eg-base--set-font)
(add-hook 'emacs-startup-hook 'eg-base--setup-window-behaviour)

;; This package contains personal helper functions for text and workflow management.
(require 'ek-functions)

;; The functions loaded from the included file are assigned to custom keybindings.
(general-define-key
  "C-j"         #'ek/newline-below
  "C-o"         #'ek/newline-above    
  "M-n"         #'ek/forward-ten
  "M-p"         #'ek/backward-ten        
  "C-S-j"       #'ek/yank-next-line
  "C-S-o"       #'ek/yank-prev-line
  "C-S-w"       #'ek/kill-word-at-point
  "C-M-w"       #'ek/copy-current-line
  "C-/"         #'ek/comment-or-uncomment-line-or-region
  "C-z"         #'undo
  ;; buffer and window navigation
  "<mouse-9>"   #'next-buffer
  "<mouse-8>"   #'previous-buffer
  "C-S-<up>"    #'ek/move-line-up
  "C-S-<down>"  #'ek/move-line-down
  ;; font size
  "C-+"         #'text-scale-increase
  "C--"         #'text-scale-decrease
  "C-<mouse-4>" #'text-scale-increase
  "C-<mouse-5>" #'text-scale-decrease
  )

;; The 'M-c' prefix is "reserved" for personal keybindings.
(general-define-key
 :prefix "M-c"
 "C"          #'ek/open-config-in-dired
 "c"          #'ek/open-init-el
 "w"          #'ek/toggle-whitespace
 "l"          #'ek/toggle-line-numbers
 "C-e"        #'eval-buffer
 "e"          #'eshell
 "m"          #'magit
 "d"          #'dired
 "f"          #'counsel-fzf
 )

;;;; Load third party packages
;;;;; Navigation related packages

;; Which-key shows dynamic keybinding hints.
(ek-pkg 'which-key
  (setq which-key-idle-delay 0.2)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-add-column-padding 1)
  (which-key-mode))

;; Provides visual representation of the undo tree and more sane redo.
(ek-pkg 'undo-tree
  (global-undo-tree-mode)
  (general-define-key
   :keymaps 'undo-tree-map
   "C-z"    #'undo-tree-undo
   "C-S-z"  #'undo-tree-redo))

;; Automatically expands selection.
(ek-pkg 'expand-region
  (general-define-key
   "C-q" #'er/expand-region))

;; If no text is selected, copy and cut operations are executed on the current line.
(ek-pkg 'whole-line-or-region
  (whole-line-or-region-global-mode))

(ek-pkg 'beginend
  (beginend-global-mode 1))

(ek-pkg 'goto-last-change
  (general-define-key
   "C-_" #'goto-last-change))

(ek-pkg 'ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (general-define-key
   "M-o" #'ace-window))

(ek-pkg 'region-bindings-mode
  (region-bindings-mode-enable))
	 
(ek-pkg 'ivy
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height-alist '((t . 6)))
  (setq ivy-wrap t)
  (ivy-mode 1)
  (general-define-key
   "C-x b"   #'ivy-switch-buffer
   "C-c v"   #'ivy-push-view
   "C-c V"   #'ivy-pop-view))

(ek-pkg 'counsel
  (setq counsel-yank-pop-preselect-last t)
  (setq counsel-yank-pop-separator "\n—————————\n")
  (general-define-key
   "M-x"      #'counsel-M-x
   "C-x C-f"  #'counsel-find-file
   "C-x C-r"  #'counsel-recentf
   "M-y"      #'counsel-yank-pop
   "<f1> f"   #'counsel-describe-function
   "<f1> v"   #'counsel-describe-variable
   "<f1> l"   #'counsel-find-library
   "<f2> i"   #'counsel-info-lookup-symbol
   "<f2> u"   #'counsel-unicode-char
   "<f2> j"   #'counsel-set-variable
   "M-c s"    #'counsel-ag
   "M-c g"    #'counsel-git-grep))

(ek-pkg 'swiper
  (general-define-key
   "C-S-s" #'swiper-isearch))

(ek-pkg 'ivy-xref
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Outshine-mode improves on outline-mode and enables an org-mode like folding behaviour in other major modes.
;; It might be included in emacs-28 by default!!!
(ek-pkg 'outshine
  (add-hook 'emacs-lisp-mode-hook 'outshine-mode)
  )

;;;;; Editing related packages

;; Improved editing.
(ek-pkg 'crux
  (general-define-key
   "C-o" #'crux-smart-open-line-above
   "C-j" #'crux-smart-open-line)
  (general-define-key
   :prefix "C-c"
   "i" #'crux-cleanup-buffer-or-region
   "d" #'crux-duplicate-current-line-or-region
   "r" #'crux-rename-file-and-buffer))

(ek-pkg 'electric
  (setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (electric-indent-mode 1)
  (electric-pair-mode t))

(ek-pkg 'multiple-cursors
  (general-define-key
   "C-S-c C-S-c"   #'mc/edit-lines
   "C-S-c C-S-p"   #'mc/mark-previous-like-this-word
   "C-S-c C-S-n"   #'mc/mark-next-like-this-word
   "C-<"           #'mc/mark-previous-like-this
   "C->"           #'mc/mark-next-like-this
   "C-S-c C-S-a"   #'mc/mark-all-like-this
   "C-M-<mouse-1>" #'mc/add-cursor-on-click
   "C-S-SPC"       #'rectangle-mark-mode)
  (general-define-key
   :keymaps 'region-bindings-mode-map
   "a"             #'mc/mark-all-like-this
   "p"             #'mc/mark-previous-like-this
   "n"             #'mc/mark-next-like-this
   "e"             #'mc/edit-lines
   "m"             #'mc/mark-more-like-this-extended))

;;;;; Appearance related packages

;; Zoomer icons.
(ek-pkg 'all-the-icons)

;; Setup light and dark theme based on the current time of day.
(ek-pkg 'doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  
  (defconst ek/dark-theme 'doom-material)
  (defconst ek/light-theme 'doom-tomorrow-day)
  (defvar   ek/current-theme ek/dark-theme)

  (defun ek/set-theme (theme)
    (interactive)
    (progn (load-theme theme t))
    (setq ek/current-theme theme))

  (defun ek/sync-theme ()
    (interactive)
    (setq hour (string-to-number
                (substring (current-time-string) 11 13)))
    (if (member hour (number-sequence 9 18))
        (ek/set-theme ek/light-theme)
      (ek/set-theme ek/dark-theme)))

  (ek/sync-theme)

  (defun ek/toggle-theme ()
    (interactive)
    (cond ((eq ek/current-theme ek/dark-theme)  (ek/set-theme ek/light-theme))
          ((eq ek/current-theme ek/light-theme) (ek/set-theme ek/dark-theme))))

  (general-define-key
   "M-c t" #'ek/toggle-theme))

;; Modeline improvements.
(ek-req 'time
  :init
  (setq display-time-24hr-format t
        display-time-interval 60
        display-time-mail-directory nil
        display-time-default-load-average nil)
  :config
  (display-time-mode 1))

(ek-req 'battery
  (setq battery-mode-line-format " [%b%p%%]"
        battery-mode-line-limit 95
        battery-update-interval 180
        battery-load-low 20
        battery-load-critical 10)
  (display-battery-mode))

(ek-pkg 'minions
  (setq minions-mode-line-lighter "@")
  (setq minions-direct (list 'defining-kbd-macro
                             'flymake-mode
                             'prot-simple-monocle))
  (minions-mode 1))

(ek-pkg 'org-bullets
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
  (setq org-bullets-bullet-list '("◉" "○" "✸" "▷")))

;;;;; Programming related packages

(ek-pkg 'company
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-clang-executable 'clang++)
  (setq lsp-prefer-capf t)
  (add-hook 'after-init-hook 'global-company-mode))

(ek-pkg 'treemacs
  (setq treemacs-width 30)
  (general-define-key
   "M-0"       #'treemacs-select-window
   "C-x t 1"   #'treemacs-delete-other-windows
   "C-x t t"   #'treemacs
   "C-x t B"   #'treemacs-bookmark
   "C-x t C-t" #'treemacs-find-file
   "C-x t M-t" #'treemacs-find-tag))

(ek-pkg 'treemacs-projectile)

;; C/C++ IDE.
(ek-pkg 'lsp-mode
  (add-hook 'c++-mode-hook    'lsp)
  (add-hook 'c-mode-hook      'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'lsp-mode-hook    'lsp-enable-which-key-integration)
  (defun project-root (project) (car (project-roots project)))
  (setq lsp-keymap-prefix "C-,"                ;; keymap prefix
        lsp-enable-on-type-formatting nil      ;; smth
        lsp-completion-provider :capf          ;; uses capf automatically
        lsp-enable-symbol-highlighting nil     ;; select all instances of a symbol
        lsp-headerline-breadcrumb-enable t     ;; displays ugly headline
        lsp-modeline-code-actions-enable t     ;; modeline actions
        lsp-signature-auto-activate t          ;; show function signatures
        lsp-signature-render-documentation nil ;; give function docs
        lsp-completion-show-detail nil         ;; detailed completion
        lsp-completion-show-kind nil           ;; show item type in compl
        lsp-idle-delay 0.1)                    ;; update intervals
  (general-define-key
   :keymaps '(prog-mode-map c++-mode-map c-mode-map python-mode-map)
   "C-c C-c" #'ek/comment-or-uncomment-line-or-region)
  (general-define-key
   :keymaps '(prog-mode-map)
   "C-," '(:keymap lsp-command-map)))

(ek-pkg 'lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-update-mode 'point)
  (lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references))

(ek-pkg 'lsp-ivy)

(ek-pkg 'lsp-treemacs
  (lsp-treemacs-sync-mode 1))

(ek-pkg 'cmake-mode)
(ek-pkg 'glsl-mode)
(ek-pkg 'jenkinsfile-mode)

;; (ek-pkg 'ccls
;;   (add-hook 'c++-mode-hook '(lambda () (require 'ccls) (lsp)))
;;   (add-hook 'c-mode-hook '(lambda () (require 'ccls) (lsp)))
;; )

(ek-pkg 'company-c-headers
  (add-to-list 'company-backends 'company-c-headers)
  
  (defun maybe-add-newline-at-buf-start ()
    (if (and (char-equal (char-after (point-min)) ?\n)
             (char-equal (char-after (1+ (point-min))) ?\n))
        ""
      "\n"))
  (defun maybe-add-newline-at-buf-end ()
    (if (and (char-equal (char-before (point-max)) ?\n)
             (char-equal (char-before (1- (point-max))) ?\n))
        ""
      "\n")))

(ek-pkg 'modern-cpp-font-lock
  (add-hook 'c++-mode 'modern-c++-font-lock-mode))

(ek-pkg 'ppindent)

(ek-pkg 'projectile
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-.") 'projectile-command-map)
  (general-define-key
   "<f5>"    #'projectile-compile-project
   "<f10>"   #'projectile-run-project
   "S-<f10>" #'kill-compilation))

(ek-pkg 'magit)

(ek-req 'prog-mode
  (add-hook 'c-mode-hook   '(lambda () (c-toggle-comment-style 1)))
  (add-hook 'c-mode-hook   '(lambda () (require 'ppindent)))
  (add-hook 'c++-mode-hook '(lambda () (electric-indent-mode t)))
  (setq c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (general-define-key
   "C-<tab>" #'ff-find-other-file)
  (general-define-key
   :prefix "C-,"
   "s"   #'lsp-treemacs-symbols
   "e"   #'lsp-treemacs-error-list
   "n"   #'flymake-goto-next-error
   "p"   #'flymake-goto-prev-error
   "r d" #'ek/define-cpp-function-in-other-file))

;;;;;

