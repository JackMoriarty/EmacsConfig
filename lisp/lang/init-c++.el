;; 安装 google-c-style 包
(use-package google-c-style
  :straight t
  :hook
  (c++-mode . google-set-c-style))

(provide 'init-c++)
;;; init-c++.el ends here
