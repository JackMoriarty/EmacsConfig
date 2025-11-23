;; ansi颜色日志支持
(define-derived-mode log-ansi-mode fundamental-mode "Log (ANSI)"
  "Major mode for viewing ANSI-colored log files."
  (auto-revert-mode 1) ;; 启用自动重载
  (setq auto-revert-verbose nil)
  (setq buffer-read-only t)

  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max)))

  ;; 后续追加内容也高亮（同样需要 inhibit-read-only）
  (add-hook 'after-change-functions
            (lambda (beg end _len)
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region beg end)))
            nil t))

(provide 'log-ansi-mode)
;;; log-ansi-mode.el ends here
