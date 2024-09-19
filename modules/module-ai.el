(use-package chatgpt-shell :straight t
  :config (setq
           chatgpt-shell-chatgpt-model-version "gpt-4"
           chatgpt-shell-openai-key '(lambda () (auth-source-pass-get 'secret "justwatch/openai-key"))))

(use-package dall-e-shell :straight t)

(provide 'module-ai)
