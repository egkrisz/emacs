;;; ek-packages.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;; Navigation related packages
(ek-pkg 'which-key
  (setq which-key-idle-delay 0.2)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-add-column-padding 1)
  (which-key-mode))

(ek-pkg 'undo-tree
  (global-undo-tree-mode)
  (general-define-key
   :keymaps 'undo-tree-map
   "C-z"    #'undo-tree-undo
   "C-S-z"  #'undo-tree-redo))

(ek-pkg 'expand-region
  (general-define-key
   "C-q" #'er/expand-region))

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

(ek-pkg 'multiple-cursors
  (general-define-key
   "C-S-c C-S-c"   #' mc/edit-lines
   "C-S-c C-S-p"   #'mc/mark-previous-like-this-word
   "C-S-c C-S-n"   #'mc/mark-next-like-this-word
   "C-<"           #'mc/mark-previous-like-this
   "C->"           #'mc/mark-next-like-this
   "C-S-c C-S-a"   #'mc/mark-all-like-this
   "C-M-<mouse-1>" #'mc/add-cursor-on-click
   "C-S-SPC"       #'rectangle-mark-mode
   :keymaps 'region-bindings-mode-map
   "a"             #'mc/mark-all-like-this
   "p"             #'mc/mark-previous-like-this
   "n"             #'mc/mark-next-like-this
   "e"             #'mc/edit-lines
   "m"             #'mc/mark-more-like-this-extended))
	 
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
   "<f2> j"   #'counsel-set-variable))

(ek-pkg 'swiper
  (general-define-key
   "C-S-s" #'swiper-isearch))

(ek-pkg 'ivy-xref
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Appearance related packages
;; Theme setup
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

;; Modeline
(ek-req 'time
  :init
  (setq display-time-24hr-format t
        display-time-interval 60
        display-time-mail-directory nil
        display-time-default-load-average nil)
  :config
  (display-time-mode 1))

;; (ek-req 'battery
;;   (setq battery-mode-line-format " [%b%p%%]"
;;         battery-mode-line-limit 95
;;         battery-update-interval 180
;;         battery-load-low 20
;;         battery-load-critical 10)
;;   (display-battery-mode))

(ek-pkg 'minions
  (setq minions-mode-line-lighter "@")
  (setq minions-direct (list 'defining-kbd-macro
                             'flymake-mode
                             'prot-simple-monocle))
  (minions-mode 1))

(ek-pkg 'org-bullets
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
  (setq org-bullets-bullet-list '("◉" "○" "✸" "▷")))

(provide 'ek-packages)

;;; ek-packages.el ends here
