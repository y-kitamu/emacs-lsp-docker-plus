;;; test-lsp-docker+.el --- Unit test for lsp-docker+  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; Keywords:

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

;;

;;; Code:
(require 'cl-lib)
(require 'ert)
(require 'lsp-docker+)
(require 'ccls)


(defun lsp-docker+-run-on-c++-config (func &rest args)
  "Run FUNC with lsp-docker+ c++ configuration.
ARGS is arguments of FUNC."
  (let ((ldp-foo (getenv "LDP_FOO"))
        (ldp-bar (getenv "LDP_BAR"))
        (lsp-docker-command "docker")
        (lsp-docker-container-name-suffix 0)
        (lsp-docker+-server-id 'ccls)
        (lsp-docker+-docker-server-id 'test-docker-lsp)
        (lsp-docker+-server-command "ccls")
        (lsp-docker+-image-id "emacslsp/lsp-docker-langservers")
        (lsp-docker+-docker-options "-u test_user")
        (lsp-docker+-container-name "lsp-docker")
        (lsp-docker+-path-mappings (list (cons "${LDP_FOO}/test" "${LDP_BAR}/test")))
        (lsp-docker+-priority 5)
        (lsp-docker+-server-cmd-fn #'lsp-docker+-launch-new-container))
    (unwind-protect
        (progn
          (setenv "LDP_FOO" "/home/user")
          (setenv "LDP_BAR" "/project")
          (if (null args)
              (funcall func)
            (funcall func args)))
      (setenv "LDP_FOO" ldp-foo)
      (setenv "LDP_BAR" ldp-bar))))


(ert-deftest lsp-docker+-unittest-lsp-docker+-register-client ()
  "Unit test of `lsp-docker+-register-client'."
  (let ((lsp-docker+-server-id 'ccls)
        (lsp-docker+-docker-server-id 'test-docker-lsp)
        (lsp-docker+-priority 20))
    (lsp-docker+-register-client)
    (let ((client (gethash 'test-docker-lsp lsp-clients)))
      (should (equal 20 (lsp--client-priority client)))
      (should (equal 'test-docker-lsp (lsp--client-server-id client)))
      )))

(ert-deftest lsp-docker+-unittest-lsp-docker+-launch-new-container ()
  "Unit test of `lsp-docker+-launch-new-container.'"
  (lsp-docker+-run-on-c++-config
   (lambda ()
     (let ((command (lsp-docker+-launch-new-container))
           (expect '("docker" "run" "--name" "lsp-docker-1" "--rm" "-i" "-v"
                     "/home/user/test:/project/test"  "-u" "test_user"
                     "emacslsp/lsp-docker-langservers" "ccls")))
       (cl-mapcar (lambda (x y) (should (equal x y))) command expect)))))

(ert-deftest lsp-docker+-unittest-lsp-docker+-exec-in-container ()
  "Unit test of `lsp-docker+-exec-in-container'."
  (lsp-docker+-run-on-c++-config
   (lambda ()
     (let ((command (lsp-docker+-exec-in-container))
           (expect '("docker" "exec" "-u" "test_user" "-i" "lsp-docker" "ccls")))
       (cl-mapcar (lambda (x y) (should (equal x y))) command expect)))))

;;; test-lsp-docker+.el ends here
