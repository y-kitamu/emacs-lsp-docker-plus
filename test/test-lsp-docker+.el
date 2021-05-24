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

(require 'ert)

(require 'lsp-docker+)
(require 'ccls)

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

;;; test-lsp-docker+.el ends here
