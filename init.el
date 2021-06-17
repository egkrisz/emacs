;;; init.el --- the heart of the beast -*- lexical-binding: t; -*-

;;; Code:

;;; Bootstrap straight.el

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

;;; Setup straight.el

(straight-use-package 'use-package)
(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;

(require 'ek-base)
(require 'ek-packages)
(require 'ek-functions)
(require 'ek-prog)

;;;;;

