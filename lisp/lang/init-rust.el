(use-package rust-mode
  :straight t
  :defer t
  :config
  (setq rust-format-on-save t)
  ;; lsp config
  (setq lsp-rust-analyzer-binding-mode-hints t)
  (setq lsp-rust-analyzer-closure-capture-hints t)
  (setq lsp-rust-analyzer-closure-return-type-hints "with_block")
  (setq lsp-rust-analyzer-discriminants-hints "fieldless")
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-reborrow-hints "mutable")
  (setq lsp-rust-analyzer-expression-adjustment-hide-unsafe t)
  (setq lsp-rust-analyzer-expression-adjustment-hints "reborrow")
  (setq lsp-rust-analyzer-hide-closure-initialization t)
  (setq lsp-rust-analyzer-implicit-drops t)
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
