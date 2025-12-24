;; 文件管理器
(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :bind ("C-x d" . dirvish)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group"))

;; 终端
(use-package vterm-toggle
  :straight t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-buffer-name "*vterm-toggle*")
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 ;; 当前窗口
                 ;; (display-buffer-reuse-window display-buffer-same-window)
                 ;; 位于底部
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.35)
                 ;; 位于右侧
                 ;; (display-buffer-reuse-window display-buffer-in-side-window)
                 ;; (side . right)
                 ;; (window-width . 0.35)
                 (reusable-frames . visible)))
  (unbind-key "M-0" vterm-mode-map)
  :bind ("C-c t t" . 'vterm-toggle))

(use-package counsel
  :straight t)

(use-package ivy
  :straight t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-selectable-prompt t)
  (setq counsel-find-file-ignore-regexp "\\(?:\\`\\|[/\\]\\)\\(?:[#.]\\)")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ;; 在某些终端上 C-x C-SPC 会被映射为 C-x C-@, 比如在 macOS 上, 所以要手动设置
   ("C-x C-@" . 'counsel-mark-ring)
   ("C-x C-SPC" . 'counsel-mark-ring)
   ("C-c g" . 'counsel-git)
   ("C-c j" . 'counsel-git-grep)
   ("C-c i" . 'counsel-imenu)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package nerd-icons-ivy-rich
  :straight t
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))

;; 命令历史按频率排序
(use-package amx
  :straight t
  :init (amx-mode))

;; 窗口切换增强
(use-package ace-window
  :straight t
  :bind (("C-x o" . 'ace-window)))

;; 单行光标移动增强，行首/文字开头，行尾/注释前行尾
(use-package mwim
  :straight t
  :defer t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; 多行光标
(use-package multiple-cursors
  :straight t
  :defer t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m a" . mc/mark-all-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; undo-tree
(use-package undo-tree
  :straight t
  :defer t
  :init (global-undo-tree-mode))

;; 命令提示
(use-package which-key
  :straight t
  :init (which-key-mode))

;; 单词跳转
(use-package avy
  :straight t
  :bind
  (("M-j" . avy-goto-char-timer)))

;; minibuffer 选项增加注解
(use-package marginalia
  :straight t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; 高亮光标下相同词语
(use-package symbol-overlay
  :straight t
  :bind
  ("M-i" . symbol-overlay-put)
  ("M-n" . symbol-overlay-switch-forward)
  ("M-p" . symbol-overlay-switch-backward)
  ("<f7>" . symbol-overlay-mode)
  ("<f8>" . symbol-overlay-remove-all))

;; 句子翻译
(use-package go-translate
  :straight t
  :defer t
  :config
  ;; (setq gt-debug-p t)
  (setq gt-langs '(en zh))
  ;; LLM
  (setq gt-chatgpt-temperature 0.7)
  ;; Ollama
  (setq gt-chatgpt-host "localhost:11434")
  (setq gt-chatgpt-model "qwen2.5:3b")
  ;; Online
  ;; (setq gt-chatgpt-host "https://api.groq.com/openai")
  ;; (setq gt-chatgpt-model "qwen-2.5-32b")
  ;; (setq gt-chatgpt-key (get-passwd "api.groq.com" "apikey"))
  (setq gt-default-translator
        (gt-translator
         :taker
         (gt-taker :text 'buffer :pick 'paragraph :prompt t)
         :engines
         (list
          (gt-chatgpt-engine)
          (gt-google-engine)
          (gt-bing-engine))
         :render
         (gt-buffer-render
          :buffer-name "abc"
          ;; :window-config '((display-buffer-at-bottom))
          :then (lambda (_) (pop-to-buffer "abc"))))))

;; 变量名翻译插件, 需要安装crow-translate或者ollama
(use-package insert-translated-name
  :straight (:host github :repo "manateelazycat/insert-translated-name"
                   :files ("*.py" "*.el"))
  :after (llm)
  :config
  ;; 默认使用crow后端，本地llm设置为ollama, 在线llm则设置为llm
  (setq insert-translated-name-program "ollama")
  ;; 本地模型名称
  (setq insert-translated-name-ollama-model-name "qwen2.5:3b")
  ;; 如果使用在线模型，则需要设置provider
  ;; (require 'llm-openai)
  ;; (setq insert-translated-name-llm-provider
  ;;       (make-llm-openai-compatible
  ;;        :url "https://api.groq.com/openai/v1"
  ;;        :chat-model "qwen-2.5-32b"
  ;;        :key (get-passwd "api.groq.com" "apikey")))
  (setq llm-warn-on-nonfree nil))

;; 词典
(use-package bing-dict
  :straight t
  :defer t
  :config
  (setq bing-dict-add-to-kill-ring t)
  (setq bing-dict-show-thesaurus 'both)
  :bind
  ("C-c d" . bing-dict-brief))

;; ananconda 环境管理
(use-package pyvenv
  :straight t
  :config
  (setenv "WORKON_HOME" "~/.conda/envs")
  (setq python-shell-interpreter "python3")
  ;; (pyvenv-workon "py312") ;; 设置pyvenv默认虚拟环境名
  (pyvenv-mode t))

;; 自动保存
(use-package auto-save
  :straight (:host github :repo "manateelazycat/auto-save")
  :config
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace nil)  ; automatically delete spaces at the end of the line when saving)
  (auto-save-enable))

;; 删除修改行的行尾空格
(use-package ws-butler
  :straight t
  :defer t
  :hook
  (prog-mode . ws-butler-mode))

;; 记录光标位置
(use-package dogears
  :straight t
  :hook (after-init . dogears-mode)
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (setq dogears-idle 1
        dogears-limit 200
        dogears-position-delta 20)
  (setq dogears-functions '(find-file recenter-top-bottom
                                      other-window switch-to-buffer
                                      aw-select toggle-window-split
                                      windmove-do-window-select
                                      pager-page-down pager-page-up
                                      tab-bar-select-tab
                                      pop-to-mark-command
                                      pop-global-mark
                                      goto-last-change
                                      xref-go-back
                                      xref-find-definitions
                                      xref-find-references)))

;; 辅助选择单词, 句子等
(use-package expand-region
  :straight t
  :bind ("M-=" . er/expand-region))

;; 调整窗格大小
(use-package windresize
  :straight t
  :defer t)

;; 表格中英文对齐
(use-package valign
  :straight t
  :defer t
  :hook
  (org-mode . valign-mode)
  (markdown-mode . valign-mode))

;; 大文件
(use-package vlf
  :straight t
  :custom
  (vlf-application 'dont-ask))

;; AI Chat Client.
(use-package gptel
  :straight t
  :defer t
  :config
  (setq gptel-default-mode 'org-mode)
  ;; provider注册
  (setq provider_ollama
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '(qwen2.5:3b)))
  ;; (setq provider_groq
  ;;       (gptel-make-openai "Groq"
  ;;         :host "api.groq.com"
  ;;         :endpoint "/openai/v1/chat/completions"
  ;;         :stream t
  ;;         :key (get-passwd "api.groq.com" "apikey")
  ;;         :models '(qwen-2.5-32b)))
  (setq gptel-model 'qwen2.5:3b)
  (setq gptel-backend provider_ollama)
  ;; 光标自动移动到下一个prompt
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package llm
  :straight t)

;; 性能benchmark
(use-package benchmark-init
  :straight t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'init-core)
;;; init-core.el ends here
