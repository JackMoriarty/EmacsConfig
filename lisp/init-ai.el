(use-package llm
  :straight t)

;; 句子翻译
(use-package gt
  :straight t
  :defer t
  :config
  ;; (setq gt-debug-p t)
  (setq gt-default-translator
        (gt-translator
         :taker
         (gt-taker
	  :langs '(en zh)
	  :text 'word
	  :pick 'paragraph)
         :engines
         (list
	  ;; (gt-chatgpt-engine
	  ;;  :host "[RemoteHost]"
	  ;;  :path "/chat/completions"
	  ;;  :model "[RemoteModel]")
          (gt-google-engine)
	  (gt-bing-engine))
         :render
         (gt-buffer-render))))

;; 输入翻译, 需要安装crow-translate或者ollama
(use-package insert-translated-name
  :straight (:host github :repo "manateelazycat/insert-translated-name"
                   :files ("*.py" "*.el"))
  :after (llm)
  :config
  ;; 默认使用crow后端，本地llm设置为ollama, 在线llm则设置为llm
  ;; (setq insert-translated-name-program "ollama")
  ;; Ollama模型
  ;; (setq insert-translated-name-ollama-model-name "[OllamaModel]")
  ;; 在线模型，则需要设置provider
  ;; (require 'llm-openai)
  ;; (setq llm-warn-on-nonfree nil)
  ;; (setq insert-translated-name-llm-provider
  ;;       (make-llm-openai-compatible
  ;;        :url "[RemoteHostURL]" ;; for example "https://api.ai.com/v1/"
  ;;        :key (auth-source-pick-first-password
  ;; 	       :host "[RemoteHost]"
  ;; 	       :user "apikey")
  ;; 	 :chat-model "[RemoteModel]"))
  )

;; 模型chat
(use-package gptel
  :straight t
  :bind
  ("C-c e" . gptel-menu)
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model '[ChatModel])
  (setq gptel-backend (gptel-make-openai "RemoteService"
			:host "[RemoteHost]"
			:endpoint "/[RemoteEndPoint]/chat/completions"
			:stream t
			:key (auth-source-pick-first-password
			      :host "[RemoteHost]"
			      :user "apikey")
			:models '([ChatrModel])))

  ;; 关于更多provider配置，详见https://github.com/karthink/gptel
  )

;; ai代码补全
(use-package minuet
  :straight t
  :bind
  (:map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  (add-to-list 'minuet-auto-suggestion-block-predicates
	       '(lambda () (< (point) (line-end-position))))
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  (setq minuet-provider 'openai-fim-compatible)

  ;; 关于模型更多的配置选项，详见https://github.com/milanglacier/minuet-ai.el
  ;; openai-fim-compatible模型
  ;; (plist-put minuet-openai-fim-compatible-options
  ;; 	     :end-point "[RemoteHostURL]/completions")
  ;; (plist-put minuet-openai-fim-compatible-options
  ;; 	     :api-key (lambda () (auth-source-pick-first-password
  ;; 				  :host "[RemoteHost]"
  ;; 				  :user "apikey")))
  ;; (plist-put minuet-openai-fim-compatible-options :model "[FIMModel]")

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

(provide 'init-ai)
;;; init-ai.el ends here
