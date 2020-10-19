;;; cfg-mode.el --- sample major mode for editing LSL. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020, by you

;; Author: Krisztián Egervári ( egkrisz@gmail.com )
;; Version: 0.1
;; Created: 30 May 2020
;; Keywords: cfg
;; Homepage: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

(setq cfg-highlights
      '(("#\\(?:.\\|\n\\||\\)*?$" . font-lock-comment-face)
        ("\|\\|->" . font-lock-keyword-face)
        ("^\\(\\w+\\)" . font-lock-variable-name-face)
        ("'\\([^']+\\)'" . font-lock-string-face)))

;;;###autoload
(define-derived-mode cfg-mode fundamental-mode "cfg mode"
  "Major mode for editing CFGs (Context Free Grammars)…"

  ;; code for syntax highlighting
  (setq font-lock-defaults '(cfg-highlights)))

;; add the mode to the `features' list
(provide 'cfg-mode)

;;; cfg-mode.el ends here
