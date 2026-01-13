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

;; AI 补全
(use-package minuet
  :straight (:host github :repo "milanglacier/minuet-ai.el")
  :defer t
  :bind
  (("M-o" . #'minuet-show-suggestion) ;; use overlay for completion

   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion)     ;; invoke completion or cycle to previous completion
   ("TAB" . #'minuet-accept-suggestion)   ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("C-g" . #'minuet-dismiss-suggestion))

  ;; :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  (setq minuet-n-completions 1)                 ;; recommended for Local LLM for resource saving
  ;; I recommend you start with a small context window firstly, and gradually increase it based on your local computing power.
  ;; (setq minuet-context-window 512)
  (setq minuet-provider 'openai-fim-compatible) ;; or openai-compatible
  ;; llm completion interface
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")
  ;; (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 256)
  (minuet-set-optional-options minuet-openai-fim-compatible-options :top_p 0.9)

  ;; llm chat completion interface
  ;; (plist-put minuet-openai-compatible-options :end-point "https://api.groq.com/openai/v1/chat/completions")
  ;; (plist-put minuet-openai-compatible-options :name "Groq")
  ;; (plist-put minuet-openai-compatible-options :api-key (get-passwd "api.groq.com" "apikey"))
  ;; (plist-put minuet-openai-compatible-options :model "qwen-2.5-32b")
  ;; (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
  (setq minuet-request-timeout 5))

;; 代码检查
(use-package flycheck
  :straight t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; tree-sitter
;; (use-package treesit-auto
;;   :straight t
;;   :init
;;   (setq treesit-font-lock-level 4)
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

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
(add-hook 'prog-mode-hook #'whitespace-mode)                    ;; 显示多余空格
;; (add-hook 'prog-mode-hook (lambda () (indent-tabs-mode -1)))    ;; 关闭tab缩进
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; 增加ruler
(setq whitespace-style '(face tabs spaces trailing lines-tail newline empty
                              indentation indentation::tab indentation::space
                              big-indent space-after-tab space-after-tab::tab
                              space-after-tab::space space-before-tab
                              space-before-tab::tab space-before-tab::space
                              help-newline))
;; (setq show-paren-style 'mixed)                                  ;; 匹配括号高亮模式
;; (add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))  ;; 关闭折行

(provide 'init-prog)
;;; init-code.el ends here
