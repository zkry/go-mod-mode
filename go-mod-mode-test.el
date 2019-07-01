;;; go-mod-mode-test.el --- Test main functionality of go-mod-mode

;; Copyright (C) 2019 Zachary Romero

;; Author: Zachary Romero <zacromero@posteo.net>
;; Url: http://github.com/zkry/

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'go-mod-mode)

(ert-deftest go-mod-module-name-match-test ()
  "Test the regex for module name matching."
  (let ((full-regexp (format "^%s$" go-mod--source-regexp)))
	(should (string-match full-regexp "github.com/travelaudience/bidder-bid-price"))
	(should (string-match full-regexp "github.com/stretchr/testify"))
	(should (string-match full-regexp "github.com/travelaudience/cmt-advertiserserv"))
	(should (string-match full-regexp "github.com/travelaudience/go-event"))
	(should (string-match full-regexp "github.com/travelaudience/go-event.x"))
	(should (string-match full-regexp "github.com"))
	(should-not (string-match full-regexp "v1.2.3"))))

(ert-deftest go-mod-module-version-match-test ()
  "Test the regex for version string matching."
  (let ((full-regexp (format "^%s$" go-mod--version-regexp)))
	(should (string-match full-regexp "v1.4.0"))
	(should (string-match full-regexp "v0.0.0-20160906093959-a57dc5b54cd2"))
	(should (string-match full-regexp "v3.14.0+incompatible"))
	(should (string-match full-regexp "v1.1.17"))
	(should (string-match full-regexp "v0.0.0-20190621222207-cc06ce4a13d4"))
	(should-not (string-match full-regexp "github.com/travelaudience/bidder-bidder"))))

(ert-deftest go-mod-generate-graph-for-mod-test ()
  "Test the graph generating algorithm via a table test."
  
  ;; The table data is in the form `(mod-name ((edge-from . edge-to)*) expected-result)+'
  (let ((data `(;; simple from case
				("mod.a" (("mod.a" . "mod.b") ("mod.x" . "mod.y")) "\"mod.a\" -> \"mod.b\"\n")

				;; simple to case
				("mod.b" (("mod.a" . "mod.b")) "\"mod.a\" -> \"mod.b\"\n")

				;; two connectors
				("mod.a" (("mod.a" . "mod.b")
						  ("mod.a" . "mod.c"))
				 "\"mod.a\" -> \"mod.b\"\n\"mod.a\" -> \"mod.c\"\n")

				;; complex DAG
				("mod.a" (("mod.a" . "mod.b")
						  ("mod.z" . "mod.a")
						  ("mod.y" . "mod.z")
						  ("mod.a" . "mod.c")
						  ("mod.b" . "mod.c")
						  ("bom.a" . "bom.b"))
				 ,(concat "\"mod.a\" -> \"mod.b\"\n"
						 "\"mod.a\" -> \"mod.c\"\n"
						 "\"mod.b\" -> \"mod.c\"\n"
						 "\"mod.z\" -> \"mod.a\"\n"
						 "\"mod.y\" -> \"mod.z\"\n"))

				;; three node cycle
				("mod.a" (("mod.a" . "mod.b")
						  ("mod.b" . "mod.c")
						  ("mod.c" . "mod.a"))
				 ,(concat "\"mod.a\" -> \"mod.b\"\n"
						  "\"mod.b\" -> \"mod.c\"\n"
						  "\"mod.c\" -> \"mod.a\"\n"))

				;; two node cycle
				("mod.a" (("mod.a" . "mod.b") ("mod.b" . "mod.a"))
				 "\"mod.a\" -> \"mod.b\"\n\"mod.b\" -> \"mod.a\"\n"))))
	
	(dolist (test-case data)
	  (let* ((module (car test-case))
			(edges (cadr test-case))
			(want (format "\"%s\" [fillcolor=\"#00ADDB\", style=filled]\n%s"
						  module
						  (caddr test-case))))
	(with-temp-buffer
		  (erase-buffer)
		  (go-mod--generate-graph-for-mod module edges)
		  (should (equal want (buffer-substring-no-properties (point-min) (point-max)))))))))
;;; go-mod-mode-test.el ends here
