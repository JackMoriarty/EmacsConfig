(use-package lsp-mode
  :defer t
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands
  (lsp lsp-deferred)
  :config
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-links nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (setq lsp-headerline-breadcrumb-enable-diagnostics t)
  (setq lsp-enable-on-type-formatting nil)
  ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-completion-provider :none)
  ;; lsp-server configuration.
  (setq lsp-copilot-enabled nil)
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "--header-insertion=never"))
  ;; improve performance.
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :bind
  ("C-c l s" . lsp-ivy-workspace-symbol))

(use-package lsp-ui
  :straight t
  :defer t
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :straight t
  :defer t
  :after (lsp-mode))


;; 代码补全
(use-package company
  :straight t
  :defer t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)                     ;; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)                              ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ;; 根据选择的频率进行排序

(use-package company-box
  :straight t
  :defer t
  :if window-system
  :hook
  (company-mode . company-box-mode))

;; 代码补全片段模板
(use-package yasnippet
  :straight t
  :defer t
  :config
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (unbind-key "TAB" yas-minor-mode-map)
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight t
  :defer t
  :after yasnippet)

;; 代码检查
(use-package flycheck
  :straight t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

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
;; (add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))  ;; 关闭折行

(provide 'init-prog)
;;; init-code.el ends here
