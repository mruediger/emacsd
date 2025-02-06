(use-package chatgpt-shell :straight t
  :config (setq
           chatgpt-shell-chatgpt-model-version "gpt-4"
           chatgpt-shell-openai-key '(lambda () (auth-source-pass-get 'secret "justwatch/openai-key"))))

(use-package dall-e-shell :straight t)


(use-package gptel :straight t
  :config
  (setq gptel-default-mode 'org-mode
        gptel-api-key (auth-source-pass-get 'secret "justwatch/openai-key"))

  (setq
   gptel-backend-gemini (gptel-make-gemini "Gemini"
                          :key (auth-source-pass-get 'secret "cloud/gemini-n96")
                          :stream t))

  (setq gptel-backend gptel-backend-gemini
        gptel-model 'gemini-2.0-flash-exp)

  :bind (("C-c RET" . gptel-send)))

;;(use-package magit-magit-gptcommit :straight t
;;  :after magit
;;  :bind (:map git-commit-mode-map
;;              ("C-c C-g" . magit-gptcommit-commit-accept))

(provide 'module-ai)
