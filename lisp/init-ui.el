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

;; 主题随系统主题更换
(use-package auto-dark
  :straight t
  :custom
  (custom-safe-themes t)
  (auto-dark-themes '((doom-gruvbox) (doom-gruvbox-light)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :init
  (auto-dark-mode))

;; 高亮工作区buffer
(use-package solaire-mode
  :straight t
  :after (doom-themes)
  :config
  (solaire-global-mode +1))

;; 缩进线
(use-package indent-bars
  :straight t
  ;; :custom
  ;; (indent-bars-prefer-character t)
  :hook
  (prog-mode . indent-bars-mode))

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
  ;; Enable all Maple Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode '("::" ":::" "?:" ":?" ":?>" "<:" ":>" ":<" "<:<" ">:>" "__" "#{"
		"#[" "#(" "#?" "#!" "#:" "#=" "#_" "#__" "#_(" "]#" "#######"
		"<<" "<<<" ">>" ">>>" "{{" "}}" "{|" "|}" "{{--" "{{!--" "--}}"
		"[|" "|]" "!!" "||" "??" "???" "&&" "&&&" "//" "///" "/*" "/**"
		"*/" "++" "+++" ";;" ";;;" ".." "..." ".?" "?." "..<" ".=" "<~"
		"~>" "~~" "<~>" "<~~" "~~>" "-~" "~-" "~@" "~~~~~~~"
		"0xA12 0x56 1920x1080" "<>" "</" "/>" "</>" "<+" "+>" "<+>" "<*"
		"*>" "<*>" ">=" "<=" "<=<" ">=>" "==" "===" "!=" "!==" "=/="
		"=!=" "|=" "<=>" "<==>" "<==" "==>" "=>" "<=|" "|=>" "=<=" "=>="
		"=======" ">=<" ":=" "=:" ":=:" "=:=" "\\ \' \." "--" "---"
		"<!--" "<#--" "<!---->" "<->" "<-->" "->" "<-" "-->" "<--" ">->"
		"<-<" "|->" "<-|" "-------" ">--" "--<" "<|||" "|||>" "<||"
		"||>" "<|" "|>" "<|>" "_|_" "[TRACE]" "[DEBUG]" "[INFO]"
		"[WARN]" "[ERROR]" "[FATAL]" "[TODO]" "[FIXME]" "[NOTE]"
		"[HACK]" "[MARK]" "[EROR]" "[WARNING]" "todo))" "fixme))" "Cl"
		"al" "cl" "el" "il" "tl" "ul" "xl" "ff" "tt" "all" "ell" "ill"
		"ull" "ll"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; GUI 配置
(if (display-graphic-p)
    (progn
      (setq-default cursor-type 'bar) ;;光标类型
      (toggle-scroll-bar -1) ;; 关闭滚动条
      ;; 设置默认字体
      (set-face-attribute
       'default nil :family "Maple Mono NF CN" :weight 'normal)))

(setq inhibit-startup-message t)               ;; 关闭启动 Emacs 时的欢迎界面
;; (menu-bar-mode -1)                             ;; 关闭菜单栏
(tool-bar-mode -1)                             ;; 关闭 Tool bar
(column-number-mode t)                         ;; 在 Mode line 上显示列号

(provide 'init-ui)
;;; init-ui.el ends here
