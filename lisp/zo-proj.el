;;; zo-proj.el --- Project utilities stolen from Mr Zo -*- lexical-binding: t -*-

(defun zo/proj--get-project-root ()
  "Get root of current project."
  (locate-dominating-file default-directory ".git"))

(defun zo/proj--select-item (prompt cand-func &optional initial-input)
  "Select item from CAND-FUNC's result in current project, prompting the user with PROMPT."
  (let ((dir (zo/proj--get-project-root)))
    (if dir
        (progn
          (setq dir (expand-file-name dir))
          (message dir)
          (let* ((cands (funcall cand-func dir))
                 (file (ivy-read prompt cands :initial-input initial-input)))
            (when file
              (find-file (concat dir file)))))
      (message "Not in a project."))))

(defun zo/proj--get-items-in-directory (type dir)
  (split-string
   (shell-command-to-string
    (concat "find " dir " -type " type " | sort | sed s@" dir "@@" " | sed /.git/d")) "\n"))

(defun zo/proj--get-files-in-directory (dir)
  (zo/proj--get-items-in-directory "f" dir))

(defun zo/call-with-region-text (func &rest args)
  (let ((str))
    (when mark-active
      (deactivate-mark)
      (setq str (buffer-substring-no-properties (mark) (point))))
    (if (> (length args) 0)
        (apply func (append (list str) args))
      (funcall func str))))

(defun zo/query-replace-with-region-text ()
  (interactive)
  (let ((to-string))
    (if mark-active
        (zo/call-with-region-text 'query-replace (read-from-minibuffer "Query replace with: "))
      (call-interactively #'query-replace))))

(defun zo/proj-find-file (&optional initial-input)
  "Find file in the current project."
  (zo/proj--select-item "Find file in project: " #'zo/proj--get-files-in-directory initial-input))

(defun zo/proj-find-file-with-region-text ()
  (interactive)
  (zo/call-with-region-text #'zo/proj-find-file))

(defun zo/proj-ag-with-region-text ()
  "Run ag in current project root."
  (interactive)
  (let ((dir (zo/proj--get-project-root)))
    (if dir
        (zo/call-with-region-text 'counsel-ag dir)
      (message "Not in a project."))))

(defun zo/swiper-isearch-with-region-text ()
    (interactive)
    (zo/call-with-region-text 'swiper-isearch))

(defun zo/swiper-all-with-region-text ()
    (interactive)
    (zo/call-with-region-text 'swiper-all))

(defun zo/counsel-fzf-with-region-text ()
  (interactive)
  (zo/call-with-region-text 'counsel-fzf))

(provide 'zo-proj)

;;; zo-proj.el ends here
