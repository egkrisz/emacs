;;; ek-packages.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

(use-package which-key
  :straight t
  :init
  (setq which-key-idle-delay 0.2)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-add-column-padding 1)
  :config
  (which-key-mode))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

(use-package expand-region
  :straight t
  :bind ("C-q" . er/expand-region))

(use-package whole-line-or-region
  :straight t
  :config
  (whole-line-or-region-global-mode))

(use-package beginend
  :straight t
  :config
  (beginend-global-mode 1))

(use-package goto-last-change
  :straight t
  :bind ("C-z" . goto-last-change))

(use-package region-bindings-mode
  :straight t
  :config
  (region-bindings-mode-enable))

(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C-S-c C-S-p"   . mc/mark-previous-like-this-word)
         ("C-S-c C-S-n"   . mc/mark-next-like-this-word)
         ("C-<"           . mc/mark-previous-like-this)
         ("C->"           . mc/mark-next-like-this)
         ("C-S-c C-S-a"   . mc/mark-all-like-this)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)
         :map region-bindings-mode-map
         ("a"             . mc/mark-all-like-this)
         ("p"             . mc/mark-previous-like-this)
         ("n"             . mc/mark-next-like-this)
         ("e"             . mc/edit-lines)
         ("m"             . mc/mark-more-like-this-extended)))
	 
(use-package ivy
  :straight t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height-alist '((t . 6)))
  (setq ivy-wrap t)
  :config
  (ivy-mode 1)
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c v"   . ivy-push-view)
         ("C-c V"   . ivy-pop-view)))

(use-package counsel
  :straight t
  :after ivy
  :config
  (setq counsel-yank-pop-preselect-last t)
  (setq counsel-yank-pop-separator "\n—————————\n")
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("M-y"     . counsel-yank-pop)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("<f2> j"  . counsel-set-variable)))

(use-package swiper
  :straight t
  :bind (("C-S-s" . swiper-isearch)))

(use-package ivy-xref
  :straight t
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Theme setup
(use-package modus-themes
  :straight t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region 'no-extend)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; Modeline
(use-package time
  :init
  (setq display-time-24hr-format t
        display-time-interval 60
        display-time-mail-directory nil
        display-time-default-load-average nil)
  :config
  (display-time-mode 1))

(use-package battery
  :init
  (setq battery-mode-line-format " [%b%p%%]"
        battery-mode-line-limit 95
        battery-update-interval 180
        battery-load-low 20
        battery-load-critical 10)
  :config
  (display-battery-mode))

(use-package minions
  :straight t
  :init
  (setq minions-mode-line-lighter "@")
  (setq minions-direct (list 'defining-kbd-macro
                             'flymake-mode
                             'prot-simple-monocle))
  :config
  (minions-mode 1))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
  (setq org-bullets-bullet-list '("◉" "○" "✸" "▷")))

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode)))
  (setq org-superstar-item-bullet-alist '((42 . 8226) (43 . 43) (45 . 8211)))
  (setq org-superstar-prettify-item-bullets t
        org-superstar-remove-leading-stars t
        org-superstar-headline-bullets-list '("◈" "◇" "◉" "○")))

;; TODO:
;; + AMX-mode

(provide 'ek-packages)

