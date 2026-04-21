(setq gc-cons-threshold most-positive-fixnum) ; 临时禁用GC
(setq package-enable-at-startup nil)

;; 临时禁用文件操作钩子，加速文件读取
(defvar old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; 直接信任 .elc 文件是最新的，避免无谓的时间戳检查
(setq load-prefer-newer nil)

;; 绕过启动闪屏
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)

;; 跳过窗口重绘
(setq frame-inhibit-implied-resize t)
;; 通信buffer
(setq read-process-output-max (* 1024 1024))  ;; 1MB
