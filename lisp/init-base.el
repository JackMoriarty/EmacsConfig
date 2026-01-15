;; 快捷打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

;; 显示当前路径到minibuffer, 并将完整路径复制到剪贴板
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(global-auto-revert-mode t)                    ;; 外部修改文件时，刷新 Buffer
(delete-selection-mode t)                      ;; 选中后输入进行文本替换
(setq make-backup-files nil)                   ;; 关闭文件自动备份
(global-set-key (kbd "M-/") 'hippie-expand)    ;; 文本展开
(savehist-mode 1)                              ;; 打开 Buffer 历史记录保存
(setq use-short-answers t)                     ;; 使用简短的确认方式
(setq confirm-kill-emacs 'yes-or-no-p)         ;; emacs退出前确认

;;设置窗口redo和undo
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "C-x 4 r") 'winner-redo)
  (global-set-key (kbd "C-x 4 u") 'winner-undo))

;; 剪切板
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save) ;; 复制到系统剪贴板
(global-set-key (kbd "C-S-v") 'clipboard-yank)           ;; 从系统剪贴板粘贴
(global-set-key (kbd "C-S-x") 'clipboard-kill-region)    ;; 剪切到系统剪贴板

(put 'upcase-region 'disabled nil)   ;; 开启选区字符转大写快捷键'C-x C-u'
(put 'downcase-region 'disabled nil) ;; 开启选区字符转大写快捷键'C-x C-l'

(add-to-list 'load-path (expand-file-name "lang" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "mode" (file-name-directory load-file-name)))

(require 'init-ui)
(require 'init-core)
(require 'init-lang)
(require 'init-mode)
(require 'init-prog)
(require 'init-project)
(require 'init-os)

(provide 'init-base)
;;; init-base.el ends here
