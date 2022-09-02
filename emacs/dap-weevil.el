;;; dap-weevil.el --- Debug Adapter Protocol mode for WEEVIL      -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Simon Parry

;; Author: Simon Parry <simon.parry@karoshibee.com>
;; Keywords: languages

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

;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0"))
;; Version: 0.2

;;; Commentary:
;; Adapter for https://github.com/wyn/weevil

;;; Code:

(require 'dap-mode)

(defun dap-tezos-weevil--populate-start-tcp-args (conf)
  "Populate CONF with the required arguments."
  (let ((conf (-> conf
                  (dap--put-if-absent :host "localhost")
                  (dap--put-if-absent :debugServer 9000)
                  (dap--put-if-absent :request "attach")
                  (dap--put-if-absent :name "Tezos-Weevil::Connected"))))
    conf))

(dap-register-debug-provider "tezos-weevil-tcp" #'dap-tezos-weevil--populate-start-tcp-args)

(dap-register-debug-template "Tezos Weevil Attach (Console)"
                             (list :type "tezos-weevil-tcp"
                                   :request "attach"
                                   :mode "attach"
                                   :name "Tezos-Weevil::Attach"))

(provide 'dap-weevil)
;;; dap-weevil.el ends here
