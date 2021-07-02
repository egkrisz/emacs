;;; ek-prog.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

(use-package electric
  :init
  (setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  :config
  (electric-indent-mode 1)
  (electric-pair-mode t))

(use-package company
  :straight t
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-clang-executable 'clang++)
  (setq lsp-prefer-capf t)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package treemacs
  :straight t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :straight t
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))
  
(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((c++-mode . lsp)
         (c-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
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
  :bind*
  (:map c++-mode-map ("C-c C-c" . ek/comment-or-uncomment-line-or-region)))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-update-mode 'point)
  (lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)
  
(use-package ccls
  :disabled
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package company-c-headers
  :straight t
  :config
  (add-to-list 'company-backends 'company-c-headers))
    (defun maybe-add-newline-at-buf-start ()
    (if (and (char-equal (char-after (point-min)) ?\n)
             (char-equal (char-after (1+ (point-min))) ?\n))
        ""
      "\n"))
  (defun maybe-add-newline-at-buf-end ()
    (if (and (char-equal (char-before (point-max)) ?\n)
             (char-equal (char-before (1- (point-max))) ?\n))
        ""
      "\n"))

(use-package modern-cpp-font-lock
  :straight t
  :hook
  (c++-mode . modern-c++-font-lock-mode))
  
(use-package ppindent
  :straight t)

(use-package projectile
  :straight t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-.") 'projectile-command-map))

(use-package magit
  :straight t)

(use-package prog-mode
  :hook
  (c-mode-hook   . (lambda () (c-toggle-comment-style 1)))
  (c-mode-hook   . (lambda () (require 'ppindent)))
  (c++-mode-hook . (lambda () (electric-indent-mode t)))
  :config
  (setq c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (c-set-offset 'case-label '+)
  :bind (("C-<tab>" . ff-find-other-file)
         ("C-, s"   . lsp-treemacs-symbols)
         ("C-, e"   . lsp-treemacs-error-list)
         ("C-c C-c" . ek/comment-or-uncomment-line-or-region)))

(provide 'ek-prog)

;;; ek-prog.el ends here
