(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :defer t
  :custom-face
  ;; do not highlight buffer when adding to context
  (gptel-context-highlight-face
   ((t (:background unspecified :foreground unspecified :inherit unspecified))))
  :config
  (setq gptel-default-mode 'org-mode)

  (setq gptel-backend-gemini-rennsport (gptel-make-gemini "Rennsport-Gemini"
                               :key (auth-source-pass-get 'secret "rennsport/gemini-api-key")
                               :stream t))

  (setq gptel-backend-gemini (gptel-make-gemini "Gemini"
                               :key (auth-source-pass-get 'secret "cloud/gemini-n96")
                               :stream t))

  (setq gptel-backend-claude (gptel-make-anthropic "Claude"
                               :key (auth-source-pass-get 'secret "provider/anthropic")
                               :stream t))

  (setq gptel-backend-ollama
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '((qwen3:14b :capabilities (tool-use))
    		    (deepseek-r1:14b))
          :request-params '(:options (:num_ctx 32768))))

  (setq gptel-backend gptel-backend-gemini-rennsport
        gptel-model 'gemini-flash-latest)

  (gptel-make-preset 'tool-session
    :description "Chat session wtih tools and MCPs"
    :pre (lambda () (gptel-mcp-connect nil 'sync))
    :tools '(:append ("mcp-nixos")))

  (gptel-make-preset 'websearch
    :description  "Web search capability."
    :tools        '("WebSearch" "WebFetch"))

  (setq gptel-use-tools t
        gptel-log-level 'info
        gptel--set-buffer-locally t)

  :bind
  (("C-c C-<return>" . gptel-send))
  (("C-x a r" . gptel-rewrite))
  (("C-x a b" . gptel)))

;; collection of tools and prompts to use gptel “agentically”
(use-package gptel-agent
  :straight (:host github :repo "karthink/gptel-agent")
  :after gptel
  :config (gptel-agent-update))

(use-package gptel-preset-collection
  :straight (:host github :repo "karthink/gptel-preset-collection")
  :after gptel)

;; view LLM responses as buffer annotations
(use-package gptel-annotate
  :straight (:host github :repo "karthink/gptel-annotate")
  :after gptel)

(use-package mcp
  :straight t
  :after gptel
  :config (require 'mcp-hub)
  :custom
  (mcp-hub-servers
   `(("nixos" . (:command "nix" :args ("run" "github:utensils/mcp-nixos" "--"))))))

(use-package gptel-integrations
   :after (gptel mcp))

(provide 'module-ai)
