;; nerd-icon支持, 运行M-x nerd-icons-install-fonts安装字体
(use-package nerd-icons
  :straight t)

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

;; ibuffer 显示图标
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; doom主题
;; 终端下配置环境变量`export COLORTERM=truecolor`
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t)   ;; if nil, bold is universally disabled
  (setq doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  (doom-themes-visual-bell-config)   ;; Enable flashing mode-line on errors
  (doom-themes-org-config))          ;; Corrects (and improves) org-mode's native fontification.

;; doom modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

;; 主题定时切换
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
  (setq circadian-themes '((:sunrise . doom-solarized-light)
                           (:sunset  . doom-solarized-dark)))
  (circadian-setup))

;; 高亮工作区buffer
(use-package solaire-mode
  :straight t
  :after (doom-themes)
  :config
  (solaire-global-mode +1))

;; 缩进线
(use-package highlight-indent-guides
  :straight t
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-responsive 'top)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; 括号彩虹高亮
(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package holo-layer
  :straight (:host github :repo "manateelazycat/holo-layer"
                   :files ("*.py" "*.el"))
  :config
  (setq holo-layer-enable-cursor-animation t)
  (holo-layer-enable))

(column-number-mode t)                         ;; 在 Mode line 上显示列号

;; GUI 配置
(if (display-graphic-p)
    (progn
      (setq-default cursor-type 'bar) ;;光标类型
      (toggle-scroll-bar -1) ;; 关闭滚动条
      (set-face-attribute 'default nil :family "Fira Code" :weight 'normal) ;; 设置默认字体
      (set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Source Han Sans CN" :weight 'normal)))) ;; 设置中文字体

(provide 'init-ui)
;;; init-ui.el ends here
