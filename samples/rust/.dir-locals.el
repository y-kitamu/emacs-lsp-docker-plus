;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((rust-mode
  . ((lsp-docker+-server-id . rust-analyzer)
     (lsp-docker+-docker-server-id . rust-analyzer-lsp-docker)
     (lsp-docker+-server-command . "rust-analyzer")
     (lsp-docker+-docker-options . "-u ${USER}")
     (lsp-docker+-image-id . "rust_language-server")
     (lsp-docker+-container-name . "rust-lsp-docker")
     (lsp-docker+-path-mappings
      . (("${HOME}/work/emacs-lsp-docker-plus/samples/rust" . "/project/"))))))
