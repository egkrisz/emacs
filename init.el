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

;; Setup custom package management
(defmacro ek-req (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'ek-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

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

;; Setup general.el tno deal with keybindings
(ek-pkg 'general
  (general-auto-unbind-keys)
  (general-override-mode t))

;; (straight-use-package 'use-package)
(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;

(require 'ek-base)
(require 'ek-packages)
(require 'ek-functions)
(require 'ek-prog)

;;;;;

