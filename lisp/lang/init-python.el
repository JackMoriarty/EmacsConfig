(use-package lsp-pyright
  :straight t
  :defer t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

(provide 'init-python)
;;; init-python.el ends here
