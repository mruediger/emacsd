(use-package chatgpt-shell
  :config (setq
           chatgpt-shell-chatgpt-model-version "gpt-4"
           chatgpt-shell-openai-key '(lambda () (auth-source-pass-get 'secret "justwatch/openai-key"))))

;(use-package dall-e-shell)


(use-package gptel
  :defer t
  :config
  (setq gptel-default-mode 'org-mode
        gptel-api-key (auth-source-pass-get 'secret "justwatch/openai-key"))

  (setq gptel-backend-gemini (gptel-make-gemini "Gemini"
                               :key (auth-source-pass-get 'secret "cloud/gemini-n96")
                               :stream t))

  (setq gptel-backend-claude (gptel-make-anthropic "Claude"
                               :key (auth-source-pass-get 'secret "provider/anthropic")
                               :stream t))

  (setq gptel-backend gptel-backend-claude
        gptel-model 'claude-3-5-sonnet-20241022)

  :bind (("C-c C-<return>" . gptel-send)))

(use-package aider
;;  :vc (:fetcher  github :repo tninja/aider.el)
  :vc (aider :url "https://github.com/tninja/aider.el" :files ("aider.el"))
  :defer t
  :config (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY" (auth-source-pass-get 'secret "provider/anthropic"))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;(use-package magit-magit-gptcommit
;;  :after magit
;;  :bind (:map git-commit-mode-map
;;              ("C-c C-g" . magit-gptcommit-commit-accept))

(use-package elysium
  :defer t
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

(provide 'module-ai)
