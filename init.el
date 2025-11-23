(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 安装use-package
(straight-use-package 'use-package)

;; straight使用ssh 协议下载
;; (setq straight-vc-git-default-protocol 'ssh)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-base)

(provide 'init)
;;; init.el ends here
