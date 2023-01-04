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

(defun dap-weevil--full-example-filename (filename)
  "Utility to make the fully qualified FILENAME of the examples/*.tz ."
  (f-join (f-dirname (f-this-file)) ".." "examples" filename))

(defvar dap-weevil--script-filename1 (dap-weevil--full-example-filename "open_tezos_example_looping.tz"))
(defvar dap-weevil--script-filename2 (dap-weevil--full-example-filename "open_tezos_example2_iter.tz"))
(defvar dap-weevil--script-filename3 (dap-weevil--full-example-filename "open_tezos_example3_lambda.tz"))
(defvar dap-weevil--script-filename4 (dap-weevil--full-example-filename "open_tezos_example4_loop_left.tz"))
(defvar dap-weevil--script-filename5 (dap-weevil--full-example-filename "open_tezos_example5_factorial.tz"))

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

(dap-register-debug-template "Tezos Weevil Launch (Open Tezos - example 1)"
                             (list :type "tezos-weevil-tcp"
                                   :request "launch"
                                   :mode "launch"
                                   :script_filename dap-weevil--script-filename1
                                   :storage "0"
                                   :parameter "(Pair 7 5)"
                                   :name "Tezos-Weevil::Launch1"))

(dap-register-debug-template "Tezos Weevil Launch (Open Tezos - example 2)"
                             (list :type "tezos-weevil-tcp"
                                   :request "launch"
                                   :mode "launch"
                                   :script_filename dap-weevil--script-filename2
                                   :storage "None"
                                   :parameter "{1;2;5;3;7;2;15;4}"
                                   :name "Tezos-Weevil::Launch2"))

(dap-register-debug-template "Tezos Weevil Launch (Open Tezos - example 3)"
                             (list :type "tezos-weevil-tcp"
                                   :request "launch"
                                   :mode "launch"
                                   :script_filename dap-weevil--script-filename3
                                   :storage "5"
                                   :parameter "1"
                                   :name "Tezos-Weevil::Launch3"))

(dap-register-debug-template "Tezos Weevil Launch (Open Tezos - example 4)"
                             (list :type "tezos-weevil-tcp"
                                   :request "launch"
                                   :mode "launch"
                                   :script_filename dap-weevil--script-filename4
                                   :storage "100"
                                   :parameter "5"
                                   :name "Tezos-Weevil::Launch4"))

(dap-register-debug-template "Tezos Weevil Launch (Open Tezos - example 5)"
                             (list :type "tezos-weevil-tcp"
                                   :request "launch"
                                   :mode "launch"
                                   :script_filename dap-weevil--script-filename5
                                   :storage "1"
                                   :parameter "5"
                                   :name "Tezos-Weevil::Launch5"))

(provide 'dap-weevil)
;;; dap-weevil.el ends here
