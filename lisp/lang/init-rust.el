(use-package rust-mode
  :defer t
  :straight t
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  (rust-mode . (lambda () (prettify-symbols-mode))))

(provide 'init-rust)
;;; init-rust.el ends here
