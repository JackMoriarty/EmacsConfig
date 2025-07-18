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

;; 插件配置
;; straight使用ssh 协议下载
;; (setq straight-vc-git-default-protocol 'ssh)

;; 用于获取指定机器上指定用户的密码
(defun get-passwd (host user)
  (plist-get (car (auth-source-search :host host :user user :require '(:secret))) :secret))

;; 安装use-package
(straight-use-package 'use-package)

;; 启动性能benchmark
(use-package benchmark-init
  :straight t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package llm
  :straight t)

;; 安装icon, 运行M-x nerd-icons-install-fonts安装字体
(use-package nerd-icons
  :straight t)

;; ibuffer 显示图标
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

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
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
)

;; 安装doom主题
;; 终端下配置环境变量`export COLORTERM=truecolor`
(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
        doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  ;; (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; 便于区分真实缓冲区
(use-package solaire-mode
  :straight t
  :after (doom-themes)
  :config
  (solaire-global-mode +1))

;; 安装主题定时切换
(use-package circadian
  :straight t
  :after (doom-themes)
  :config
  ;; 基于时间
  ;; (setq circadian-themes '(("8:00" . doom-solarized-light)
  ;;                          ("19:30" . doom-solarized-dark)))
  ;; 基于经纬度日出日落
  (setq calendar-latitude 39.9)
  (setq calendar-longitude 116.4)
  (setq circadian-themes '((:sunrise . doom-one-light)
                           (:sunset  . doom-one)))
  (circadian-setup))

;; 安装doom modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

;; 安装cuda-mode
(use-package cuda-mode
  :defer t
  :straight t)

;; 安装dockerfile-mode
(use-package dockerfile-mode
  :defer t
  :straight t)

;; 安装go-mode
(use-package go-mode
  :defer t
  :straight t)

;; 安装protobuf-mode
(use-package protobuf-mode
  :defer t
  :straight t)

;; 安装yaml-mode
(use-package yaml-mode
  :defer t
  :straight t)

;; 安装treesit
(use-package treesit-auto
  :straight t
  :init
  (setq treesit-font-lock-level 4)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; 缩进线
(use-package highlight-indent-guides
  :straight t
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-responsive 'top)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; 安装bazel
(use-package bazel
  :defer t
  :straight t)

;; 安装go-translate翻译插件
(use-package go-translate
  :straight t
  :defer t
  :config
  ;; (setq gt-debug-p t)
  (setq gt-langs '(en zh))
  ;; Ollama
  (setq gt-chatgpt-host "localhost:11434")
  (setq gt-chatgpt-model "qwen2.5:3b")
  ;; Online
  ;; (setq gt-chatgpt-host "https://api.groq.com/openai")
  ;; (setq gt-chatgpt-model "qwen-2.5-32b")
  ;; (setq gt-chatgpt-key (get-passwd "api.groq.com" "apikey"))
  (setq gt-chatgpt-temperature 0.7)
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

;; 安装insert-translated-name插件, 需要安装crow-translate或者ollama
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

;; 安装vterm-toggle
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
                 ;; side 窗口
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 ;; 位于底部
                 ;; (side . bottom)
                 ;; (window-height . 0.35)
                 ;; 位于右侧
                 (side . right)
                 (window-width . 0.35)
                 (reusable-frames . visible)
                 ))
  (unbind-key "M-0" vterm-mode-map)
  :bind ("C-c t" . 'vterm-toggle))

;; 安装ivy
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

;; ivy 显示图标
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

;; undo-tree
(use-package undo-tree
  :straight t
  :defer t
  :init (global-undo-tree-mode))

;; 命令提示
(use-package which-key
  :straight t
  :init (which-key-mode))

;; avy
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

;; dashboard 更新
(use-package dashboard
  :straight t
  :init
  (setq dashboard-display-icons-p t)                     ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons)                 ;; use `nerd-icons' package
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名
  (setq dashboard-projects-backend 'projectile)          ;; 支持projectile
  (setq dashboard-startup-banner 'logo)                  ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 10)                ;; 显示多少个最近文件
                          (bookmarks . 10)               ;; 显示多少个最近书签
                          (projects . 5)))               ;; 显示多少个最近项目
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)                      ;; Content is not centered by default.
  (setq dashboard-vertically-center-content nil)         ;; vertically center content
  (setq dashboard-navigation-cycle t)                    ;; enable cycle navigation between each section
  (add-to-list 'dashboard-items '(agenda) t)
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-setup-startup-hook))

;; 高亮光标下相同词语
(use-package symbol-overlay
  :straight t
  :bind
  ("M-i" . symbol-overlay-put)
  ("M-n" . symbol-overlay-switch-forward)
  ("M-p" . symbol-overlay-switch-backward)
  ("<f7>" . symbol-overlay-mode)
  ("<f8>" . symbol-overlay-remove-all))

;; 括号彩虹高亮
(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 补全插件
(use-package company
  :straight t
  :defer t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序

(use-package company-box
  :straight t
  :defer t
  :if window-system
  :hook
  (company-mode . company-box-mode))

;; yasnippet 代码片段模板
(use-package yasnippet
  :straight t
  :defer t
  :config
  (yas-reload-all)
  ;; unbind <TAB> completion
  (unbind-key "TAB" yas-minor-mode-map)
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight t
  :defer t
  :after yasnippet)

;; AI 补全插件， 需要本地部署ollama及代码补全模型
(use-package minuet
  :straight (:host github :repo "milanglacier/minuet-ai.el")
  :defer t
  :bind
  (("M-o" . #'minuet-show-suggestion) ;; use overlay for completion

   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("TAB" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("C-g" . #'minuet-dismiss-suggestion))

  ;; :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
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

;; 代码分析
;;; for python
(use-package lsp-pyright
  :straight t
  :defer t)

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
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-enable-on-type-formatting nil)
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
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-position 'top)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :straight t
  :defer t
  :after (lsp-mode))

;; ananconda 环境管理
(use-package pyvenv
  :straight t
  :config
  (setenv "WORKON_HOME" "~/.conda/envs")
  (setq python-shell-interpreter "python3")
  ;; (pyvenv-workon "py312") ;; 设置pyvenv默认虚拟环境名
  (pyvenv-mode t))

;; 项目管理
(use-package projectile
  :straight t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :straight t
  :after (projectile)
  :init (counsel-projectile-mode))

;; 工作区管理
(use-package treemacs
  :straight t
  :defer t
  :config
  ;; (treemacs-tag-follow-mode)
  (treemacs-follow-mode)
  ;; (setq treemacs-project-follow-cleanup t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-nerd-icons
  :straight t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

;; git
(use-package magit
  :defer t
  :straight t)

(use-package treemacs-magit
  :straight t
  :defer t
  :after (treemacs magit))

;; blamer
(use-package blamer
  :straight (:host github :repo "Artawower/blamer.el")
  :custom
  ;; (blamer-author-formatter "  ✎ %s ")
  ;; (blamer-commit-formatter " ● %s")
  ;; (blamer-datetime-formatter "[%s]")
  (blamer-entire-formatter "      %s")
  (blamer-idle-time 0.5)
  (blamer-max-commit-message-length 50)
  (blamer-min-offset 6)
  (blamer-self-author-name "You")
  (blamer-type 'both)
  ;; (blamer-smart-background-p nil)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :italic t)))
  ;; :hook
  ;; (prog-mode . blamer-mode)
  :bind
  ("<f3>" . blamer-mode))

;; 高亮当前列
(use-package vline
  :straight t
  :defer t
  :config
  :bind ("<f1>" . vline-mode))

;; 高亮注释(TODO, FIXME等)
(use-package comment-tags
  :straight t
  :defer t
  :hook
  (prog-mode . comment-tags-mode))

;; 高亮doxygen
(use-package highlight-doxygen
  :straight t
  :defer t
  :hook
  (prog-mode . highlight-doxygen-mode))

;; 自动保存
(use-package auto-save
  :straight (:host github :repo "manateelazycat/auto-save")
  :config
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace nil)  ; automatically delete spaces at the end of the line when saving)
  (auto-save-enable))

;; 连体字支持
(use-package ligature
  :straight t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; 词典
(use-package bing-dict
  :straight t
  :defer t
  :config
  (setq bing-dict-add-to-kill-ring t)
  (setq bing-dict-show-thesaurus 'both)
  :bind
  ("C-c d" . bing-dict-brief))

;; 删除修改行的行尾空格
(use-package ws-butler
  :straight t
  :defer t
  :hook
  (prog-mode . ws-butler-mode))

;; 大文件
(use-package vlf
  :straight t
  :custom
  (vlf-application 'dont-ask))

;; 调整窗格大小
(use-package windresize
  :straight t
  :defer t)

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

;; 代码折叠
(use-package yafolding
  :straight t
  :hook
  (prog-mode . (lambda () (yafolding-mode))))

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

;; 表格中英文对齐
(use-package valign
  :straight t
  :defer t
  :hook
  (org-mode . valign-mode)
  (markdown-mode . valign-mode))

;; 辅助选择单词, 句子等
(use-package expand-region
  :straight t
  :bind ("M-=" . er/expand-region))

;; 语言相关配置
(add-hook 'prog-mode-hook #'electric-pair-mode);; 编程模式下自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode)   ;; 编程模式下, 光标在括号上时高亮另一个括号
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; 编程模式下, 显示行号
(add-hook 'prog-mode-hook #'whitespace-mode)   ;; 编程模式下显示多余空格
(add-hook 'prog-mode-hook (lambda () (indent-tabs-mode -1))) ;; 编程模式下关闭tab缩进
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t))) ;; 编程模式下关闭折行
(setq-default display-fill-column-indicator-column 80)
;; (c-set-offset 'innamespace 0)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; 增加ruler

;; 编辑器配置
;; (setq show-paren-style 'mixed)                 ;; 匹配括号高亮模式
(setq whitespace-style '(face trailing tabs spaces newline missing-newline-at-eof
                              empty space-after-tab space-before-tab tab-mark))
;; (add-hook 'before-save-hook 'whitespace-cleanup) ;; 保存前删除行尾空格
(column-number-mode t)                         ;; 在 Mode line 上显示列号
(global-auto-revert-mode t) ;; 当另一程序修改了文件时, 让 Emacs 及时刷新 Buffer
(delete-selection-mode t)   ;; 选中文本后输入文本会替换文本(更符合我们习惯了的其它编辑器的逻辑)
(setq inhibit-startup-message t)               ;; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil)                   ;; 关闭文件自动备份
(tool-bar-mode -1)                             ;; 关闭 Tool bar
(global-set-key (kbd "M-/") 'hippie-expand)    ;; 文本展开
(savehist-mode 1)                              ;; 打开 Buffer 历史记录保存
(setq use-short-answers t)                     ;; 使用简短的确认方式
(setq confirm-kill-emacs 'yes-or-no-p)         ;; emacs退出前确认

;; 在编程模式下高亮当前行，并在选中区域时关闭高亮当前行
(add-hook 'prog-mode-hook #'hl-line-mode)
(defun change-hl-line-mode (val)
  (if (derived-mode-p 'prog-mode)
      (hl-line-mode val)))
(add-hook 'activate-mark-hook (lambda () (change-hl-line-mode -1)))
(add-hook 'deactivate-mark-hook (lambda () (change-hl-line-mode 1)))

;; 注释当前行
(defun comment-line-improve (&optional arg)
  (interactive)
  (if (not (region-active-p))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "M-;") 'comment-line-improve) ;;注释快捷键

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)  ;; 快捷打开配置文件

;; 设置透明效果
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list))) ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))
(global-set-key (kbd "<f4>") 'loop-alpha)

;;设置窗口redo和undo
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "C-x 4 r") 'winner-redo)
  (global-set-key (kbd "C-x 4 u") 'winner-undo))

(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save) ;; 复制到系统剪贴板
(global-set-key (kbd "C-S-v") 'clipboard-yank)           ;; 从系统剪贴板粘贴
(global-set-key (kbd "C-S-x") 'clipboard-kill-region)    ;; 剪切到系统剪贴板
(put 'upcase-region 'disabled nil)                       ;; 开启选区字符转大写快捷键'C-x C-u'
(put 'downcase-region 'disabled nil)                     ;; 开启选区字符转大写快捷键'C-x C-l'

;; 显示当前路径到minibuffer, 并将完整路径复制到剪贴板
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(if (display-graphic-p)
    (progn
      ;; GUI模式下的特殊设置
      (set-face-attribute 'default nil :family "Fira Code" :weight 'normal) ;; 设置默认字体
      (set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Source Han Sans CN" :weight 'normal)) ;; 设置中文字体
      (toggle-scroll-bar -1) ;; 图形界面时关闭滚动条
      ))

;; 在mac系统上交换command和option
(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))

(provide 'init)
;;; init.el ends here
