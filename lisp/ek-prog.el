;;; ek-prog.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

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
  
(ek-pkg 'ccls
  (add-hook 'c++-mode-hook '(lambda () (require 'ccls) (lsp)))
  (add-hook 'c-mode-hook '(lambda () (require 'ccls) (lsp)))
)

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


(provide 'ek-prog)

;;; ek-prog.el ends here
