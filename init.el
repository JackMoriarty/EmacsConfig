(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-box highlight-indent-guides dracula-theme disable-mouse ligature all-the-icons doom-themes doom-modeline fanyi super-save highlight-doxygen comment-tags go-translate docker-tramp vline yaml-mode protobuf-mode bazel magit yasnippet yasnippet-snippets vterm-toggle cuda-mode pyvenv vterm undo-tree lsp-treemacs treemacs-projectile treemacs lsp-pyright counsel-projectile projectile lsp-ivy lsp-ui lsp-mode flycheck company rainbow-delimiters highlight-symbol dashboard marginalia which-key good-scroll mwim ace-window amx counsel use-package)))

;;; emacs 配置文件
;; 配置镜像
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; 插件配置
;; 安装use-package
(package-install 'use-package)
(eval-when-compile
  (require 'use-package))

;; 安装icon, 运行M-x all-the-icons-install-fonts安装字体
(use-package all-the-icons
  :if (display-graphic-p))

;; 安装dracula主题
;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'dracula t))

;; 安装doom主题
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t                         ;; if nil, bold is universally disabled
        doom-themes-enable-italic t)                      ;; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; for treemacs users
  (if (display-graphic-p)
      (progn (setq doom-themes-treemacs-theme "doom-colors") ;; use "doom-colors" for less minimal icon theme
      (doom-themes-treemacs-config)))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; 安装doom modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; 安装cuda-mode
(use-package cuda-mode
  :ensure t)

;; 安装yaml-mode
(use-package yaml-mode
  :ensure t)

;; 安装protobuf-mode
(use-package protobuf-mode
  :ensure t)

;; 安装markdown-mode
(use-package markdown-mode
  :ensure t)

;; 安装bazel 
(use-package bazel
  :ensure t)

;; 安装docker-tramp
(use-package docker-tramp
  :ensure t)

;; 安装go-translate翻译插件
(use-package go-translate
  :ensure t
  :bind(("C-t" . 'gts-do-translate))
  :config
  (setq gts-buffer-window-config
	'((display-buffer-reuse-window display-buffer-in-side-window)
	  (side . bottom)))
  (setq gts-buffer-follow-p t)
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
       (gts-translator
        :picker
	;; (gts-prompt-picker)
	(gts-noprompt-picker)
        :engines
	(list
	 ;; (gts-google-engine)
	 (gts-google-rpc-engine)
	 (gts-bing-engine))
        :render
	;; (gts-posframe-pop-render)
	(gts-buffer-render))))

;; 安装vterm
(use-package vterm
  :ensure t)

;; 安装vterm-toggle
(use-package vterm-toggle
  :ensure t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                ;;(direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))
  :bind(("C-c t" . 'vterm-toggle)))

;; 安装ivy
(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-selectable-prompt t)
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring) ;; 在某些终端上 C-x C-SPC 会被映射为 C-x C-@, 比如在 macOS 上, 所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   ("C-c g" . 'counsel-git)
   ("C-c j" . 'counsel-git-grep)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;; 命令历史按频率排序
(use-package amx
  :ensure t
  :init (amx-mode))

;; 窗口切换增强
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

;; 单行光标移动增强，行首/文字开头，行尾/注释前行尾
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; undo-tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; 滚动平滑
(use-package good-scroll
  :ensure t
  :init (good-scroll-mode))

;; 命令提示
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; avy
(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)))

;; minibuffer 选项增加注解
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle)))

;; dashboard 更新
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名
  (setq dashboard-projects-backend 'projectile)          ;; 支持projectile
  (setq dashboard-startup-banner 'logo)                  ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 10)                ;; 显示多少个最近文件
			  (bookmarks . 10)               ;; 显示多少个最近书签
			  (projects . 5)))               ;; 显示多少个最近项目
  (dashboard-setup-startup-hook))

;; 高亮光标下相同词语
(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号

;; 括号彩虹高亮
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; company 自动补全
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)                     ;; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)                              ;; 给选项编号(按快捷键M-1, M-2等等来进行选择)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ;; 根据选择的频率进行排序

;; company 图标
(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

;; 语法检查
(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ;; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; 代码分析 LSP 前端
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration) ;; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none)          ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t))

(use-package lsp-ui
  :ensure t
  :config
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  ;; lsp-ui-peek
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; lsp-ui-doc
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

;; 代码分析 LSP C/C++后端，需要安装llvm
(use-package c++-mode
  :functions 			;; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state))

;; 代码分析 LSP Python后端，需要pip安装pyright
(use-package lsp-pyright
  :ensure t
  :config
  :hook
  (python-mode . (lambda ()
		  (require 'lsp-pyright)
		  (lsp-deferred))))

;; yasnippet 代码片段模板
(use-package yasnippet
  :ensure t
  :init
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; ananconda 环境管理
(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.conda/envs")
  (setq python-shell-interpreter "python3")
  (pyvenv-mode t))

;; 项目管理
(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

;; 工作区管理
(use-package treemacs
  :ensure t
  :defer t
  ;; :config
  ;; (treemacs-tag-follow-mode)
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

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

;; git
(use-package magit
  :ensure t)

;; 高亮当前列
(use-package vline
  :ensure t
  :config
  (set-face-attribute 'vline nil :background "dimgray")
  :bind ("<f1>" . vline-mode))

;; 高亮注释(TODO, FIXME等)
(use-package comment-tags
  :ensure t
  :hook
  (prog-mode . comment-tags-mode))

;; 高亮doxygen
(use-package highlight-doxygen
  :ensure t
  :hook
  (prog-mode . highlight-doxygen-mode))

;; 自动保存
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; 连体字支持
(use-package ligature
  :ensure t
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
(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

;; 关闭鼠标
(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode t))

;; 缩进对齐线
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; 编辑器配置
(add-hook 'prog-mode-hook #'electric-pair-mode)                       ;; 编程模式下自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode)                          ;; 编程模式下, 光标在括号上时高亮另一个括号
(column-number-mode t)                                                ;; 在 Mode line 上显示列号
(global-auto-revert-mode t)                                           ;; 当另一程序修改了文件时, 让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                                             ;; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(setq inhibit-startup-message t)                                      ;; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil)                                          ;; 关闭文件自动备份
(add-hook 'prog-mode-hook #'hs-minor-mode)                            ;; 编程模式下, 可以折叠代码块
(add-hook 'prog-mode-hook #'linum-mode)                               ;; 编程模式下, 显示行号
(tool-bar-mode -1)                                                    ;; 关闭 Tool bar
(when (display-graphic-p) (toggle-scroll-bar -1))                     ;; 图形界面时关闭滚动条
(global-set-key (kbd "M-/") 'hippie-expand)                           ;; 文本展开
(savehist-mode 1)                                                     ;; 打开 Buffer 历史记录保存
(setq-default display-fill-column-indicator-column 80) 
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)       ;; 增加ruler
(setq display-time-24hr-format t)                                     ;; 使用24小时制
(setq display-time-day-and-date t)                                    ;; 显示日期
(display-time-mode t)                                                 ;; 显示时间及CPU负载
(display-battery-mode t)                                              ;; 显示电池电量
(global-hl-line-mode t)                                               ;; 高亮当前行
(setq show-paren-style 'mixed)                                        ;; 匹配括号高亮模式
(setq confirm-kill-emacs 'yes-or-no-p)                                ;; emacs退出前确认
(defun comment-line-improve (&optional arg)
  (interactive)
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "M-;") 'comment-line-improve)                    ;;注释快捷键

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)                         ;; 快捷打开配置文件

(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)              ;; 复制到系统剪贴板
(global-set-key (kbd "C-S-v") 'clipboard-yank)                        ;; 从系统剪贴板粘贴
(global-set-key (kbd "C-S-x") 'clipboard-kill-region)                 ;; 剪切到系统剪贴板

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(hl-line ((t (:extend t :background "#444759"))))
 ;; '(ivy-current-match ((t (:extend t :background "#444759"))))
 ;; '(region ((t (:extend t :background "black"))))
 '(default ((t (:family "FiraCode Nerd Font Mono" :foundry "CTDB" :slant normal :weight normal :height 90 :width normal)))))
