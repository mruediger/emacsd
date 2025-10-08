(use-package gptel
  :defer t
  :config
  (setq gptel-default-mode 'org-mode)

  (setq gptel-backend-gemini (gptel-make-gemini "Gemini"
                               :key (auth-source-pass-get 'secret "cloud/gemini-n96")
                               :stream t))

  (setq gptel-backend-claude (gptel-make-anthropic "Claude"
                               :key (auth-source-pass-get 'secret "provider/anthropic")
                               :stream t))

  (setq gptel-backend gptel-backend-claude
        gptel-model 'claude-3-5-sonnet-20241022)

  (gptel-make-tool
   :name "create_file"
   :function (lambda (path filename content)   ; the function that runs
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :description "Create a new file with the specified content"
   :args (list '(:name "path"             ; a list of argument specifications
	               :type string
               '(:name "filename"
	               :type string
	               :description "The name of the file to create")
               '(:name "content"
	               :type string
	               :description "The content to write to the file"))
   :category "filesystem"))
  :bind (("C-c C-<return>" . gptel-send)))

(use-package mcp
  :after gptel
  :config (require 'mcp-hub)
  :custom
  (mcp-hub-servers
   `(("fetch" . (:command "uvx" :args ("mcp-server-fetch"))))))

(use-package gptel-integrations
   :after (gptel mcp))

(use-package aider
  :vc (:url "git@github.com:tninja/aider.el.git")
  :defer t
  :config (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY" (auth-source-pass-get 'secret "provider/anthropic"))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(use-package elysium
  :defer t
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

(provide 'module-ai)
