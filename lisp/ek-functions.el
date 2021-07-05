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

(global-set-key [(control shift up)]   'ek/move-line-up)
(global-set-key [(control shift down)] 'ek/move-line-down)
    
(general-define-key
  "C-j"         #'ek/newline-below
  "C-o"         #'ek/newline-above    
  "M-n"         #'ek/forward-ten
  "M-p"         #'ek/backward-ten        
  "C-S-j"       #'ek/yank-next-line
  "C-S-o"       #'ek/yank-prev-line
  "C-S-w"       #'kill-whole-line
  "C-M-w"       #'ek/copy-current-line
  "C-/"         #'ek/comment-or-uncomment-line-or-region
  "C-z"         #'undo
  ;; buffer and window navigation
  "<mouse-9>"   #'next-buffer
  "<mouse-8>"   #'previous-buffer)

  (general-define-key
   :prefix "M-c"
   "c"          #'ek/open-config-in-dired
   "w"          #'ek/toggle-whitespace
   "l"          #'ek/toggle-line-numbers)

(provide 'ek-functions)

;;; ek-functions.el ends here
