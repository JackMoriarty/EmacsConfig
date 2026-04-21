;; 语言服务
(use-package eglot
  :straight t
  :defer t
  :custom
  ;; 忽略所有格式化能力
  (eglot-ignored-server-capabilities '(:documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider
                                       :documentHighlightProvider))
  :hook
  (prog-mode . eglot-ensure))

;; eglot加速器，需要安装emacs-lsp-booster二进制程序
(use-package eglot-booster
  :straight (eglot-booster
	     :type git
	     :host nil
	     :repo "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config
  (setq eglot-booster-io-only t)
  (eglot-booster-mode))

;; 代码补全
(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)                    ;; enable auto completion
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)         ;; Preselect
  (corfu-on-exact-match 'insert)    ;; Configure handling of exact matches
  (corfu-auto-prefix 1)
  )

;; corfu终端支持
(use-package corfu-terminal
  :straight t
  ;; 仅在终端图形界面未启用时，才启用 corfu-terminal-mode
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode +1))

;; corfu中显示图标
(use-package nerd-icons-corfu
  :straight t
  :after (corfu nerd-icons) ;; 确保在 corfu 和 nerd-icons 之后加载
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

;; 管理多个补全源
(use-package cape
  :straight t
  :after corfu
  :init
  ;; 在这里添加你想要的补全后端
  (add-hook 'completion-at-point-functions #'cape-file)    ;; 文件路径补全
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; 缓冲区词补全
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Elisp 代码块
  )

;; Yasnippet配置
(use-package yasnippet
  :straight t
  :defer t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :defer t
  :after yasnippet)

(use-package yasnippet-capf
  :straight t
  :defer t
  :after cape)

;; use the `orderless' completion style.
(use-package orderless
  :straight t
  :defer t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; 面包屑导航
(use-package breadcrumb
  :straight t
  :config
  (breadcrumb-mode 1))

;; 注释当前行
(defun comment-line-improve (&optional arg)
  (interactive)
  (if (not (region-active-p))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "M-;") 'comment-line-improve)

(add-hook 'prog-mode-hook #'electric-pair-mode)                 ;; 自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode)                    ;; 高亮配对括号
(add-hook 'prog-mode-hook #'display-line-numbers-mode)          ;; 显示行号
(setq display-line-numbers-width-start t) ;; 减少行号计算开销
;; (add-hook 'prog-mode-hook (lambda () (indent-tabs-mode -1)))    ;; 关闭tab缩进
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; 增加ruler
(add-hook 'prog-mode-hook #'whitespace-mode)                    ;; 显示多余空格
(setq whitespace-style
      '(face
	trailing         ; 尾随空格（最重要！）
        tabs             ; 制表符
        spaces           ; 所有空格（可能太吵）
        empty            ; 空行的空格
        lines-tail       ; 超出列限制的部分
        space-before-tab ; 制表符前的空格
        space-after-tab  ; 制表符后的空格
        indentation      ; 缩进不一致
        tab-mark))       ; 显示制表符字符
;; (setq show-paren-style 'mixed)                                  ;; 匹配括号高亮模式
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))  ;; 关闭折行

(provide 'init-prog)
;;; init-code.el ends here
