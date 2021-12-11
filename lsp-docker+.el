;;; lsp-docker+.el --- Convenient wrapper for lsp-docker  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; URL: https://github.com/y-kitamu/emacs-lsp-docker-plus
;; Keywords: convenience, language server
;; Package-Requires: ((emacs "25.1") (dash "2.18.1") (lsp-mode "7.0.1") (lsp-docker "0.0"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package wrap lsp-docker (https://github.com/emacs-lsp/lsp-docker) to make settings easier.
;; This package make it easier to switch language server via settings in .dir-locals.el.
;; You can easily use an actual development environment (docker image) as language server.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'lsp-mode)
(require 'lsp-docker)

(defgroup lsp-docker+ nil
  "Convenient wrapper for lsp-docker"
  :group 'convenience)

(defcustom lsp-docker+-server-id nil
  "Server id of the lsp mode client.
The client's configuration is replicated to create new clients."
  :type 'symbol
  :safe #'symbolp)

(defcustom lsp-docker+-docker-server-id nil
  "Server id of the created lsp mode client."
  :type 'symbol
  :safe #'symbolp)

(defcustom lsp-docker+-server-command nil
  "Server command of the created lsp mode client."
  :type 'string
  :safe #'stringp)

(defcustom lsp-docker+-image-id "emacslsp/lsp-docker-langservers"
  "ID of the language server docker image."
  :type 'string
  :safe #'stringp)

(defcustom lsp-docker+-docker-options ""
  "Additional docker command options.
Environment variables can be used.
The options would be added just before docker image id.
docker run --name <container name> --rm \\
 -i <path-mappings> <additional options> <image id> <command>"
  :type 'string
  :safe #'stringp)

(defcustom lsp-docker+-container-name "lsp-docker"
  "Name of created language server docker container."
  :type 'string
  :safe #'stringp)

(defcustom lsp-docker+-path-mappings nil
  "List of docker path mappings.
Environment variables can be used."
  :type 'list
  :safe #'listp)

(defcustom lsp-docker+-priority 10
  "Priority of the created lsp client."
  :type 'integer
  :safe #'integerp)

(defcustom lsp-docker+-server-cmd-fn #'lsp-docker+-launch-new-container
  "Function to launch language server."
  :type 'function
  :safe #'functionp)

(defcustom lsp-docker+-client-configs nil
  "List of client settings."
  :type 'list
  :safe #'listp)

(defun lsp-docker+-format (format-str &rest rest)
  "Wrapper function of `format' for logging.
FORMAT-STR is format string (first argument of `format').
REST is list of arguments for format string."
  (concat "lsp-docker :: " (apply 'format format-str rest)))

(defun lsp-docker+-replace-env-vars (path-mappings)
  "Replace environment variables in `PATH-MAPPINGS'."
  (-map (-lambda ((path . docker-path))
		(cons (substitute-in-file-name path) (substitute-in-file-name docker-path)))
      path-mappings))

(defun lsp-docker+-before-lsp (&rest rest)
  "Advice function of `lsp'.
This function register docker lsp client
using variables defined in .dir-locals.el.
Argument REST is arguments of original function (`lsp')
and not used in this function."
  (ignore rest)
  (hack-dir-local-variables-non-file-buffer)
  (cond ((-any? 'null (list lsp-docker+-server-id
                            lsp-docker+-docker-server-id
                            lsp-docker+-server-command
                            lsp-docker+-path-mappings))
         (message (lsp-docker+-format "Skip registering lsp-docker client. Some args are nil.")))
        (t
         (lsp-docker+-init-clients
            :path-mappings lsp-docker+-path-mappings
            :default-docker-image-id lsp-docker+-image-id
            :default-docker-container-name lsp-docker+-container-name
            :priority lsp-docker+-priority
            :client-configs (list
                             (list :server-id lsp-docker+-server-id
                                   :docker-server-id lsp-docker+-docker-server-id
                                   :server-command lsp-docker+-server-command))))))

(defun lsp-docker+-register-before-lsp (&rest rest)
  "Add lsp client.  REST is not used."
  (hack-dir-local-variables-non-file-buffer)
  (if (not (null lsp-docker+-client-configs))
      (lsp-docker+-init-clients
       :path-mappings lsp-docker+-path-mappings
       :default-docker-image-id lsp-docker+-image-id
       :default-docker-container-name lsp-docker+-container-name
       :priority lsp-docker+-priority
       :client-configs lsp-docker+-client-configs)))


(defun lsp-docker+-launch-new-container (&rest _)
  "Return the docker command to be executed on host.
This function is advice function of `lsp-docker-launch-new-container'."
  (cl-incf lsp-docker-container-name-suffix)
  (let ((command
         (remove "" (split-string
                     (format "%s run --name %s-%d --rm -i %s %s %s %s"
		                     lsp-docker-command
                             lsp-docker+-container-name
		                     lsp-docker-container-name-suffix
		                     (->> lsp-docker+-path-mappings
			                   (-map (-lambda ((path . docker-path))
				                       (substitute-in-file-name
                                        (format "-v %s:%s" path docker-path))))
			                   (s-join " "))
                             (substitute-in-file-name lsp-docker+-docker-options)
		                     lsp-docker+-image-id
		                     lsp-docker+-server-command)
                     " "))))
    (message (lsp-docker+-format "docker command = %s" command))
    command))

(defun lsp-docker+-exec-in-container (docker-container-name server-command)
  "Return the docker command to run language server in existing container.
DOCKER-CONTAINER-NAME and SERVER-COMMAND is used."
  (let ((command
         (remove "" (split-string
                     (format "docker exec %s -i %s %s"
                             (substitute-in-file-name lsp-docker+-docker-options)
                             docker-container-name
                             server-command)))))
    (message (lsp-docker+-format "docker command = %s" command))
    command))

(cl-defun lsp-docker+-register-client (&key docker-container-name server-command path-mappings)
  "Advice function of `lsp-docker-register-client'.
Original function is not called in this function.
All the arguments used in original function is replaced by custom variables.
Correspondence is as follows.
server-id             -> `lsp-docker+-server-id'
docker-server-id      -> `lsp-docker+-docker-server-id'
PATH-MAPPINGS
docker-image-id       -> `lsp-docker+-image-id'
DOCKER-CONTAINER-NAME
priority              -> `lsp-docker+-priority'
SERVER-COMMAND
launch-server-cmd-fn  -> `lsp-docker+-server-cmd-fn'"
  (message (lsp-docker+-format
            "Start register lsp-docker client.
  docker-container-name = %s, docker-image-id = %s
  server-id = %s, docker-server-id = %s, server-command = %s"
            lsp-docker+-container-name lsp-docker+-image-id
            lsp-docker+-server-id lsp-docker+-docker-server-id lsp-docker+-server-command))
  (if-let ((client (gethash lsp-docker+-server-id lsp-clients)))
      (progn
        (lsp-register-client
         (make-lsp-client
          :language-id (lsp--client-language-id client)
          :add-on? (lsp--client-add-on? client)
          :new-connection (plist-put
                           (lsp-stdio-connection
                            (lambda ()
                              (funcall lsp-docker+-server-cmd-fn docker-container-name server-command)))
                           :test? (lambda (&rest _)
                                    (-any? (-lambda ((dir)) (f-ancestor-of? dir (buffer-file-name)))
                                           (lsp-docker+-replace-env-vars path-mappings))))
          :ignore-regexps (lsp--client-ignore-regexps client)
          :ignore-messages (lsp--client-ignore-messages client)
          :notification-handlers (lsp--client-notification-handlers client)
          :request-handlers (lsp--client-request-handlers client)
          :response-handlers (lsp--client-response-handlers client)
          :prefix-function (lsp--client-prefix-function client)
          :uri-handlers (lsp--client-uri-handlers client)
          :action-handlers (lsp--client-action-handlers client)
          :major-modes (lsp--client-major-modes client)
          :activation-fn (lsp--client-activation-fn client)
          :priority (or lsp-docker+-priority (lsp--client-priority client))
          :server-id lsp-docker+-docker-server-id
          :multi-root (lsp--client-multi-root client)
          :initialization-options (lsp--client-initialization-options client)
          :semantic-tokens-faces-overrides (lsp--client-semantic-tokens-faces-overrides client)
          :custom-capabilities (lsp--client-custom-capabilities client)
          :library-folders-fn (lsp--client-library-folders-fn client)
          :before-file-open-fn (lsp--client-before-file-open-fn client)
          :initialized-fn (lsp--client-initialized-fn client)
          :remote? (lsp--client-remote? client)
          :completion-in-comments? (lsp--client-completion-in-comments? client)
          :path->uri-fn (-partial #'lsp-docker--path->uri (lsp-docker+-replace-env-vars
                                                           path-mappings))
          :uri->path-fn (-partial #'lsp-docker--uri->path
                                  (lsp-docker+-replace-env-vars path-mappings)
                                  lsp-docker+-container-name)
          :environment-fn (lsp--client-environment-fn client)
          :after-open-fn (lsp--client-after-open-fn client)
          :async-request-handlers (lsp--client-async-request-handlers client)
          :download-server-fn (lsp--client-download-server-fn client)
          :download-in-progress? (lsp--client-download-in-progress? client)
          :buffers (lsp--client-buffers client)))
        (message (lsp-docker+-format "Finish register lsp docker client : server-id = %s"
                                     lsp-docker+-docker-server-id)))
    (user-error (lsp-docker+-format "No such client %s" lsp-docker+-server-id))))

(cl-defun lsp-docker+-init-clients
    (&key (path-mappings lsp-docker+-path-mappings)
          (default-docker-image-id lsp-docker+-image-id)
		  (default-docker-container-name lsp-docker+-container-name)
		  (priority lsp-docker+-priority)
		  (client-packages lsp-docker-default-client-packages)
		  (client-configs lsp-docker-default-client-configs))
  "This function works the same as `lsp-docker-init-clients'.
PATH-MAPPINGS is an alist of mountpoints (<host path> . <docker path>).
DEFAULT-DOCKER-IMAGE-ID is the identifier for the docker image.
This variable is used when DOCKER-IMAGE-ID is not defined in CLIENT-CONFIGS.
DEFAULT-DOCKER-CONTAINER-NAME is the name to use for the container.
This variable is used when DOCKER-CONTAINER-NAME is not defined
in CLIENT-CONFIGS.
PRIORITY is the priority of the lsp mode client.
CLIENT-PACKAGES is a list of dependent packages.
CLIENT-CONFIGS is a list of configurations for the clients to be registered."
  (seq-do (lambda (package) (require package nil t)) client-packages)
  (seq-do (-lambda ((&plist :server-id :docker-server-id :docker-image-id
                            :docker-container-name :server-command))
            (let ((lsp-docker+-server-id server-id)
                  (lsp-docker+-docker-server-id docker-server-id)
                  (lsp-docker+-image-id (or docker-image-id default-docker-image-id))
                  (lsp-docker+-container-name (or docker-container-name default-docker-container-name))
                  (lsp-docker+-server-command server-command)
                  (lsp-docker+-path-mappings path-mappings)
                  (lsp-docker+-server-cmd-fn #'lsp-docker-launch-new-container)
                  (lsp-docker+-priority priority))
              (lsp-docker+-register-client :docker-container-name lsp-docker+-container-name
                                           :server-command lsp-docker+-server-command
                                           :path-mappings path-mappings)))
   client-configs))

;;;###autoload
(defun lsp-docker+-enable ()
  "Enable lsp-docker+.
This function add advice function `lsp-docker+-before-lsp' to `lsp'"
  (interactive)
  (advice-add #'lsp :before #'lsp-docker+-before-lsp)
  (advice-add #'lsp :before #'lsp-docker+-register-before-lsp))

;;;###autoload
(defun lsp-docker+-disable ()
  "Disable lsp-docker+.
This function remove advice function `lsp-docker+-before-lsp' from `lsp'"
  (interactive)
  (advice-remove #'lsp #'lsp-docker+-before-lsp)
  (advice-remove #'lsp #'lsp-docker+-register-before-lsp))

(provide 'lsp-docker+)
;;; lsp-docker+.el ends here
