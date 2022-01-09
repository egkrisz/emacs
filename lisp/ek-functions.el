;;; ek-functions.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

(defun ek/toggle-line-numbers ()
  "Toggle line numbers."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode)))

(defun ek/toggle-whitespace ()
  "Toggle whitespace."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(defun ek/newline-below ()
  "Insert a newline from anywhere within prev line, then indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun ek/newline-above ()
  "Insert a newline above from anywhere within line, then indent."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1))

(defun ek/copy-current-line ()
  "Copy line on point from anywhere within the line."
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol))
  (message "Current line copied"))

(defun ek/yank-next-line ()
  "Yank killed text to next line."
  (interactive)
  (ek/newline-below)
  (yank)
  (indent-for-tab-command))

(defun ek/yank-prev-line ()
  "Yank killed text to prev line."
  (interactive)
  (ek/newline-above)
  (yank)
  (indent-for-tab-command))

(defun ek/forward-ten ()
  "Go forward 10 lines."
  (interactive)
  (next-line 10))

(defun ek/backward-ten ()
  "Go back 10 lines."
  (interactive)
  (previous-line 10))

(defun ek/comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun ek/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ek/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun ek/open-config-in-dired ()
  "Opens the folder where config files are located."
  (interactive)
  (dired "~/.emacs.d/lisp/"))

(defun ek/open-init-el ()
  "Opens the init.el main configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun ek/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun ek/kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (ek/kill-thing-at-point 'word))

(defun ek/kill-symbol-at-point ()
  "Kill the symbol at point."
  (interactive)
  (ek/kill-thing-at-point 'symbol))

(defun ek/copy-proto-to-header-file ()
  (interactive)
  (save-excursion
    ;; c-mode's `beginning-of-defun' should be robust enough.
    (beginning-of-defun)
    (let ((l (point)))
      (search-forward-regexp " *{")
      (let ((proto (buffer-substring l (match-beginning 0))))
        (ff-find-other-file)
        ;; If other file is already open, we don't want to move point.
        (save-excursion
          (goto-char (point-max))
          ;; Do some more movement here if you want.
          (insert "\n" proto ";"))))))

(defun ek/get-class-name ()
  "Get the name of the current class."
  (save-excursion
    (call-interactively 'move-end-of-line)
    (search-backward-regexp "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?class\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)")
    (let ((ret (match-string 1)))
      ret)))

(defun ek/find-pattern-in-string (sig pattern)
  "Gets the type of a function."
  (defvar match "")
  (save-match-data
    (and (string-match pattern sig)
         (setq match (match-string 0 sig))))
  match)

(defun ek/define-cpp-function-in-other-file ()
  (interactive)
  (let* ((sig (thing-at-point 'line t))
         (type (ek/find-pattern-in-string sig "\\(?:const auto&\\|const auto\\|auto&\\|auto\\|void\\)"))
         (class (ek/get-class-name))
         (name (ek/find-pattern-in-string sig "\\(?:\\w*(.*)\\)"))
         (ret (ek/find-pattern-in-string sig "-> [^;]"))
         (final-sig (concat type " " class "::" name))
         (skeleton (concat final-sig "\n{\n}"))
         )
    (ff-find-other-file)
    (end-of-buffer)
    (beginning-of-defun)
    (message skeleton)
    (forward-list)
    (forward-list)
    (insert "\n\n" skeleton)
    ))

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
  "C-S-<down>"  #'ek/move-line-down)

  (general-define-key
   :prefix "M-c"
   "c"          #'ek/open-config-in-dired
   "w"          #'ek/toggle-whitespace
   "l"          #'ek/toggle-line-numbers)

(provide 'ek-functions)

;;; ek-functions.el ends here
