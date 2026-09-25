(use-package gptel
  :straight t
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

  (setq gptel-backend gptel-backend-gemini-rennsport
        gptel-model 'gemini-pro-latest)

  (setq gptel-use-tools t
        gptel-log-level 'info
        gptel--set-buffer-locally t)

  :bind
  (("C-c C-<return>" . gptel-send))
  (("C-x a r" . gptel-rewrite))
  (("C-x a b" . gptel)))

;; collection of tools and prompts to use gptel “agentically”
(use-package gptel-agent
  :straight t
  :after gptel
  :config (gptel-agent-update))

(use-package gptel-preset-collection
  :vc (:url "https://github.com/karthink/gptel-preset-collection"
       :rev :newest)
  :after gptel)

;; view LLM responses as buffer annotations
(use-package gptel-annotate
  :vc (:url "https://github.com/karthink/gptel-annotate"
       :rev :newest)
  :after gptel)

(use-package mcp
  :after gptel
  :config (require 'mcp-hub)
  :custom
  (mcp-hub-servers
   `(("fetch" . (:command "uvx" :args ("mcp-server-fetch"))))))

(use-package gptel-integrations
   :after (gptel mcp))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :defer t
  :config (setq aider-args '("--model" "anthropic/claude-3-7-sonnet-20250219"))
  (setenv "ANTHROPIC_API_KEY" (auth-source-pass-get 'secret "provider/anthropic"))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(use-package elysium
  :defer t
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

(use-package monet
  :straight (monet :type git :host github :repo "stevemolitor/monet"))

(use-package claude-code :ensure t
  :straight (:host github :repo "stevemolitor/claude-code.el")
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(provide 'module-ai)
