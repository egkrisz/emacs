;;; qt-mode.el --- mode for C++ Qt programs

;; Modified version of code presented at
;; https://www.emacswiki.org/emacs/QtMode


(require 'cc-mode)


;;; QtMode
(c-add-style "qt-gnu"
 	     '("gnu"
 	       (c-basic-offset . 3)))

;; And here is a slightly more complicated stuff:
;; A long time ago (in the 1.x series) Arndt Gulbrandsen put some macros on
;; some ftp server. Over time not using them for quite a
;; while -- they evolved into the following. Not perfect, but a nice start. Let
;; me know if you improve things:

;; syntax-highlighting for Qt
;; (based on work by Arndt Gulbrandsen, Troll Tech)

;; make new font for rest of qt keywords

(defface qt-macros-face
  '((((class color) (background dark))
     :foreground "DarkKhaki")
    (((class color) (background light))
     :foreground "khaki")
    (t :inverse-video t))
  "Basic face for Qt macro names.")

(defface qt-keywords-face
  '((((class color) (background dark))
     :foreground "NavajoWhite")
    (((class color) (background light))
     :foreground "NavajoWhite")
    (t :inverse-video t))
  "Basic face for Qt keywords.")

(defun jk/c-mode-common-hook ()
  "Set up c-mode and related modes.
 
Includes support for Qt code (macros name and qDebug)."
 
  ;; base-style
  ;; (c-set-style "stroustrup")
  (c-set-style "qt-gnu")
  ;; set auto cr mode
  (c-toggle-auto-hungry-state 1)

  ;; Do not check for old-style (K&R) function declarations;
  ;; this speeds up indenting a lot.
  (setq c-recognize-knr-p nil)
 
  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq indent-tabs-mode nil)
  ;; highlight Qt macros, keywords
  (font-lock-add-keywords
   'c++-mode
   '(("\\<\\(QT??_[A-Z_]+\\|SIGNAL\\|SLOT\\)\\>" . 'qt-macros-face)
     ("\\<\\(qDebug\\|qInfo\\|qWarning\\|qCritical\\|qFatal\\|tr\\)\\>" . 'qt-keywords-face)
     )))


(add-hook 'c-mode-common-hook 'jk/c-mode-common-hook)


(provide 'qt-mode)


;;; qt-mode.el ends here
