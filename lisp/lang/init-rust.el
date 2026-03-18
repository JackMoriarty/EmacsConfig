(use-package rust-mode
  :straight t
  :defer t
  :config
  (setq rust-format-on-save t)
  (setq rust-format-show-buffer nil)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  (rust-mode . (lambda () (prettify-symbols-mode))))

(use-package cargo-mode
  :straight t
  :defer t
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

(provide 'init-rust)
;;; init-rust.el ends here
