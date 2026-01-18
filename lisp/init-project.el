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

(use-package magit
  :defer t
  :straight t)

(use-package treemacs-magit
  :straight t
  :defer t
  :after (treemacs magit))

(use-package diff-hl
  :straight t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode t)
  (global-diff-hl-mode t))

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
  (blamer-type 'visual)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :italic t)))
  ;; :hook
  ;; (prog-mode . blamer-mode)
  :bind
  ("<f3>" . blamer-mode))

;; 远程开发
(use-package ppcompile
  :straight t
  :config
  (setq ppcompile-rsync-exclude-list
        '("*.o" ".git*" ".svn*" "*.~undo-tree~" ".dir-locals.el")))

(provide 'init-project)
;;; init-project.el ends here
