;;; dap-weevil.el --- Debug Adapter Protocol mode for Michelson -*- lexical-binding: t; -*-
;; Copyright (C) 2022 KaroshiBee

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


;;; Commentary:
;; dap-mode adapter for https://github.com/wyn/weevil

;;; Code:

(require 'dap-mode)

(defun dap-weevil--populate-start-tcp-args (conf)
  "Populate CONF with the required arguments."
  (let ((conf (-> conf
                  (dap--put-if-absent :host "localhost")
                  (dap--put-if-absent :debugServer 9000)
                  (dap--put-if-absent :request "launch")
                  ;(dap--put-if-absent :name "Tezos-Weevil::Connected")
                  (dap--put-if-absent :script_filename "TODO")
                  (dap--put-if-absent :storage "Unit")
                  (dap--put-if-absent :parameter "Unit")
                  (dap--put-if-absent :entrypoint "default"))))
    conf))

(dap-register-debug-provider "tezos-weevil-tcp" #'dap-weevil--populate-start-tcp-args)

(dap-register-debug-template "Tezos Weevil Launch (Console)"
                             (list :type "tezos-weevil-tcp"
                                   :request "launch"
                                   :mode "launch"
                                   :name "Tezos-Weevil::Launch"))

(provide 'dap-weevil)
;;; dap-weevil.el ends here
