;;; go-mod-mode.el --- Some conveniences when working with Go modules. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Zachary Romero

;; Author: Zachary Romero <zacromero@posteo.net>
;; Url: http://github.com/zkry/go-org-mode
;; Version: 0.1-pre
;; Package-Requires: ((s "1.6.0") (flycheck "31") (f "0.6.1"))

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

;; `go-mod-mode' adds supprot for working with go.mod files.  This
;; includes standard syntax highlighting for go.mod and go.sum files
;; as well as functions for managing a Go module project such as
;; upgrading a module or creating a local replica of a module.
;; Lastly, this package contains integrations with various other Emacs
;; plugins such as eldoc and flycheck.

;;; Code:

(require 'flycheck)

(defconst go-mod--source-regexp
  "\\<[a-z]+\\(?:\\.[a-z]+\\)+\\(/\\(?:[[:alnum:]-_]+/\\)*[[:alnum:]-_]+\\(?:\\.[[:alnum:]-_]+\\)?\\(?:\\.v[0-9][0-9]?\\)?\\)?"
  "Regexp for finding source names such as github.com/lib/pq.")

(defconst go-mod--version-regexp
  "\\<v[0-9]\\.[0-9]+\\.[0-9]+\\(?:-0.[0-9a-f]+\\)?\\(?:-[0-9a-f]+\\)*\\(?:+incompatible\\)?\\(?:/go.mod\\)?\\>"
  "Regexp for finding version strings.")

(defvar go-mod-graph-output "pdf"
  "The output type for the dependency graph.

This should be a valid output type that Graphviz can produce such
as png, gif, pdf, svg.")

;;; initialize the go mod mode hook.
(defvar go-mod-mode-hook nil)
(defvar go-mod-sum-mode-hook nil)

;;; Variables to cache requests of go.mod files. (local copy is always made)
(defvar go-mod--version-store nil
  "Hashmap to store list of modules with corresponding versions.")
(defvar go-mod--replacement-store nil
  "Alist to store list of modules with corresponding replacement.")

;;; Define keymaps
(let ((m (define-prefix-command 'go-mod-map)))
  (define-key m "l" #'go-mod-create-local)
  (define-key m "r" #'go-mod-undo-local)
  (define-key m "u" #'go-mod-upgrade)
  (define-key m "U" #'go-mod-upgrade-all)
  (define-key m "g" #'go-mod-get)
  (define-key m "a" #'go-mod-graph))

(defvar go-mod-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "\C-c\C-o" 'go-mod-map)
	map)
  "The keymap used when `go-mod-minor-mode' or `go-mod-mode' is active.")

;;; go mod font lock keywords
(defconst go-mod-font-lock-keywords-1
  (list
   '("\\<\\(\\(?:modul\\|re\\(?:plac\\|quir\\)\\)e\\)\\>" . font-lock-builtin-face)
   '("\\<go 1\\.[1-9][0-9]\\>" . font-lock-builtin-face)
   `((,@go-mod--version-regexp) . font-lock-constant-face)
   `((,@go-mod--source-regexp) . font-lock-string-face)))

(defconst go-mod-sum-font-lock-keywords-1
  (list
   `((,@go-mod--version-regexp) . font-lock-constant-face)
   `((,@go-mod--source-regexp) . font-lock-string-face)))

(makunbound 'go-mod-sum-font-lock-keywords)
(defvar go-mod-font-lock-keywords go-mod-font-lock-keywords-1
  "Default highlighting expressions for go mod mode.")

(defvar go-mod-sum-font-lock-keywords go-mod-sum-font-lock-keywords-1
  "Default highlighting expressions for go mod mode.")

;;; go mod indentation rules
(defun go-mod-indent-line ()
  "Indent current line as go.mod file."
  (interactive)
  (beginning-of-line)
  (if (bobp)
	  (indent-line-to 0)
	(let ((not-indented t)
		  cur-indent)
	  (if (looking-at "^[ \t]*)")
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) tab-width))
			  (if (< cur-indent 0)
                  (setq cur-indent 0))))
		(save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*)") ; Check for rule 3
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
										; Check for rule 4
			  (if (looking-at "^require (")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))
	  (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;;; go mod syntax table
(defvar go-mod-mode-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?/ ". 124b" st)
	(modify-syntax-entry ?\n "> b" st)
	st)
  "Syntax table for go-mod-mode.")

;;; go mod entry point
;;;###autoload
(defun go-mod-mode ()
  "Major mode for editing go mod files."
  (interactive)
  (when (not (executable-find "go"))
	(error "Could not find go tool.  Please see https://golang.org/doc/install to install Go"))
  (when (not (string-match "go1\\.1[1-9]" (shell-command-to-string "go version")))
	(error "Current version of Go does not support modules.  Please upgrade to at least version 1.11"))

  (kill-all-local-variables)
  (set-syntax-table go-mod-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(go-mod-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'go-mod-indent-line)

  ;; Set comment syntax
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")

  ;; If GO111MODULE is off or auto, turn on.
  (when (equal "off" (getenv "GO111MODULE"))
	(setenv "GO111MODULE" "on"))
  ;; If GO111MODULE is auto and go version is 1.12, turn on.
  (when (and (equal "auto" (getenv "GO111MODULE"))
			 (string-match "go1.12" (shell-command-to-string "go version")))
	(setenv "GO111MODULE" "on"))

  (setq major-mode 'go-mod-mode)
  (set (make-local-variable 'eldoc-documentation-function)
	   'go-mod-eldoc-function)
  (go-mod--initialize-version-cache)
  (set (make-local-variable 'go-mod--replacement-store) '())
  (add-hook 'after-save-hook 'go-mod--get-modules t t)
  (use-local-map go-mod-mode-map)
  (run-hooks 'go-mod-mode-hook))

(defun go-mod-sum-mode ()
  "Major mode for viewing go sum files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table (make-syntax-table))
  (set (make-local-variable 'font-lock-defaults) '(go-mod-sum-font-lock-keywords))
  (setq major-mode 'go-mod-sum-mode)
  (use-local-map go-mod-mode-map)
  (run-hooks 'go-mod-sum-mode-hook))

(define-minor-mode go-mod-minor-mode
  "Minor mode to add commadns to work with Go modules.

\\{go-mod-mode-map}"
  :init-value nil
  :keymap go-mod-mode-map
  :lighter nil)


;;; eldoc supporting functions
(defun go-mod-eldoc-function ()
  "Return a doc string relating to a Go module."
  (save-excursion
	(back-to-indentation)
	(let* ((mod-name (thing-at-point 'filename))
		   ;; Only proceed if 1) there is filename at point and 2) filename looks like source ID.
		   (is-mod-name (and (stringp mod-name) (string-match go-mod--source-regexp mod-name)))
		   (hash-entry (and is-mod-name (gethash mod-name go-mod--version-store))))
	  (cond
	   ((not is-mod-name) "") ; don't display anything if not mod entry on line.
	   (hash-entry (progn
					 (message (format "===cache hit: %s===" mod-name))
					 (go-mod--eldoc-format-version-list mod-name))) ; display from cache
	   (t (progn
			(message "===cache miss===")
			""))))))

(defun go-mod--eldoc-format-version-list (mod-name)
  "Return a string for displaying MOD-NAME information."
  (let ((versions (gethash mod-name go-mod--version-store))
		(replacement (cdr (assoc mod-name go-mod--replacement-store))))
	(concat
	 (nth 0 versions) ; original version
	 (if (nth 1 versions) (concat " ↑" (nth 1 versions)) "")
	 (if replacement (format " =>\"%s\"" replacement) ""))))


;;; flycheck syntax checker
(flycheck-define-checker go-mod
  "A syntax checker for go.mod files."
  :command ("go" "list" "-m")
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes go-mod-mode)

;;;###autoload
(defun flycheck-go-mod-setup ()
  "Setup Go-mod support for Flycheck.
Add `golangci-lint' to `flycheck-checkers'."
  (interactive)
  (add-hook 'flycheck-checkers 'go-mod))


;;; version-cache
(defun go-mod--add-to-version-cache (module version &optional upgrade)
  "Add MODULE, VERSION, UPGRADE version to cache."
  (message (format "%s>%s>%s" module version upgrade))
  (when (not go-mod--version-store) (error "Go Mod version store cache not initialized"))
  (puthash module `((,@version) (,@upgrade)) go-mod--version-store))

(defun go-mod--initialize-version-cache ()
  "Initialize the version cache."
  (set (make-local-variable 'go-mod--version-store)
	   (make-hash-table
		:test 'equal)))

(defun go-mod--reset-version-cache ()
  "Reset the version cache."
  (clrhash go-mod--version-store))


;;; data extraction code
(defun go-mod--get-curent-module ()
  "Return the current module string."
  (string-trim (shell-command-to-string "go list -m")))

(defun go-mod--get-modules ()
  "Get all current modules in a list."
  (when (not (go-mod--mod-enabled)) (error "Go modules not turned on"))
  (ignore-errors
	(mapcar 'split-string (process-lines "go" "list" "-m" "all"))))

(defun go-mod--get-hack-replacements ()
  "Get all modules which have been copied locally via gohack."
  (when (not (go-mod--mod-enabled)) (error "Go modules not turned on"))
  ;; Get the output from the `go list -m all' command, having a
  ;; replacement (ie `=>''), and containing the directory `gohack'.
  (mapcar (function (lambda (line) (split-string line " => " t)))
		  (seq-filter (function (lambda (str) (string-match-p "=>.*/gohack/" str)))
												(process-lines "go" "list" "-m" "all"))))

(defun go-mod--get-replacements ()
  "Get all of the replacements in form of alist from current go mod buffer."
  (save-excursion
	(let ((data '()))
	  (goto-char (point-min))
	  (while (re-search-forward (format "^replace \\(%s\\) => \\(%1$s\\)" go-mod--source-regexp) nil t)
		(let ((line (thing-at-point 'line t))
			  (regexp (format "^replace \\(%s\\) => \\(%1$s.*\\)$" go-mod--source-regexp)))
          (string-match regexp line)
		  (setq data (cons `(,(match-string 1 line) . ,(match-string 2 line)) data))))
	  (setq go-mod--replacement-store data))))

(defun go-mod--get-module-information ()
  "Get all upgradable modules."
  (when (not (go-mod--mod-enabled)) (error "Go modules not turned on"))
  (with-current-buffer "*go-mod-upgradable*"
	(erase-buffer))
  (make-process :name "go-mod-upgradable"
				:buffer "*go-mod-upgradable*"
				:stderr "*go-mod-upgradable*"
				:command '("go" "list" "-u" "-m" "all")
				:connection-type 'pipe
				:sentinel #'go-mod--get-module-information-sentinel))

(defun go-mod--get-module-information-sentinel (proc _msg)
  "Process output from the PROC that lists all upgradable go modules when _MSG is exit."
  (when (and (eq (process-status proc) 'exit)
			 (zerop (process-exit-status proc))
			 (buffer-live-p (process-buffer proc)))
	(message "response retrieved")
	(let ((go-mod-buffer (current-buffer)))
	  (with-current-buffer (process-buffer proc)
		(mapc
		 ;; Line processing code:
		 (function
		  (lambda (line)
			(when (string-match
				   go-mod--source-regexp
				   line)
			  (let* ((fields (split-string line " "))
					 (pkg-name (nth 0 fields))
					 (current-version (nth 1 fields))
					 (upgradable-version (nth 2 fields)))
				(with-current-buffer go-mod-buffer
				  (go-mod--add-to-version-cache pkg-name current-version upgradable-version))))))
		 (split-string (buffer-string) "\n")) ; Process each line of buffer.
		(kill-buffer)))))

(defun go-mod--get-module-upgrade (mod-name)
  "Return the version that MOD-NAME can upgrade to."
  (when (not (go-mod--mod-enabled)) (error "Go modules not turned on"))
  (message mod-name)
  (let* ((command (format "go list -m -u %s" (shell-quote-argument mod-name)))
		(output (shell-command-to-string command)))
	(and (string-match "\\[\\(.*\\)\\]" output)
		 (match-string 1 output))))

(defun go-mod--get-module-versions (mod-name)
  "Return a list of the versions for a particular MOD-NAME."
  (cdr (split-string (string-trim (shell-command-to-string (format "go list -m -versions %s" (shell-quote-argument mod-name)))) nil t)))
;;; end data extraction code

(defun go-mod--module-on-line ()
  "Return the module string that the pointer is on."
  (if (not (or (equal "go.mod" (file-name-nondirectory (buffer-file-name (current-buffer))))
			   (equal "go.sum" (file-name-nondirectory (buffer-file-name (current-buffer))))))
	  nil
	(let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	  (if (not (string-match go-mod--source-regexp line))
		  nil
		(progn
		  (string-match go-mod--source-regexp line)
		  (match-string 0 line))))))

(defun go-mod--get-dep-graph ()
  "Return an an a list of each dependency for current module."
  (when (get-buffer "*dep-edges*") (kill-buffer "*dep-edges*"))
  (call-process "go" nil "*dep-edges*" t "mod" "graph")
  (with-current-buffer "*dep-edges*"
	(goto-char (point-min))
	(let ((edges '()))
	  (while (looking-at "[a-zA-Z]")
		(let* ((fields (split-string (string-trim (thing-at-point 'line))))
			  (from (car fields))
			  (to (cadr fields)))
		  (setq edges (cons (cons from to) edges)))
		(forward-line))
	  edges)))


;;; general commands
(defun go-mod--mod-enabled ()
  "Return if go-mod is supported."
  (and (equal "on" (getenv "GO111MODULE"))
	   (not (equal "command-line-arguments\n" (shell-command-to-string "go list -m")))))

(defun go-mod-create-local ()
  "Clones the selected module to directory specified."
  (interactive "")
  (when (not (executable-find "gohack"))
	(error "You must have gohack installed to use this command"))
  (when (not (go-mod--mod-enabled))
	(error "Go modules not enabled"))

  (let* ((mod-name (or (go-mod--module-on-line) (go-mod--prompt-non-hack-modules)))
		 (command (and mod-name (format "gohack get %s" (shell-quote-argument mod-name)))))
	(when (not mod-name) (error "No module on current line"))
	(message (shell-command-to-string command))))

(defun go-mod-undo-local ()
  "Revert the usage of local module by calling `gohack undo ...'."
  (interactive "")
  (when (not (executable-find "gohack"))
	(error "You must have gohack installed to use this command"))
  (when (not (go-mod--mod-enabled))
	(error "Go modules not enabled"))

  (let* ((mod-name (or (go-mod--module-on-line) (go-mod--prompt-hack-replacements)))
		 (command (and mod-name (format "gohack undo %s" (shell-quote-argument mod-name)))))
	(message (shell-command-to-string command))))

(defun go-mod-upgrade ()
  "Upgrade the selected module."
  (interactive "")
  (when (not (go-mod--mod-enabled))
	(error "Go modules not enabled"))

  (let* ((mod-name (or (go-mod--module-on-line) (go-mod--prompt-all-modules)))
		 (command (and mod-name (format "go get %s@latest" (shell-quote-argument mod-name))))
		 (upgrade-to (go-mod--get-module-upgrade mod-name)))
	(if (not upgrade-to)
		(message "Module is already at latest version")
	  (when (y-or-n-p (format "Do you want to upgrade %s to %s? " mod-name upgrade-to))
		(message "Please wait while module is being updated.")
		(message (shell-command-to-string command))))))

(defun go-mod-upgrade-all (arg)
  "Upgrade all modules in go.mod.

You can modify this command with a prefix ARG by pressing \\[universal-argument]
which will only patch-upgrade available modules."
  (interactive "P")
  (if (= arg 4)
	  (compilation-start "go get -u=patch -m all")
	(compilation-start "go get -u -m all")))

(defun go-mod-get ()
  "List all of the available versions of module and select version to set it at."
  (interactive "")
  (when (not (go-mod--mod-enabled))
	(error "Go modules not enabled"))

  (let* ((mod-name (or (go-mod--module-on-line) (go-mod--prompt-all-modules)))
		 (versions (go-mod--get-module-versions mod-name)))
	;; fail if no versions for the module are available
	(if (= 0 (length versions))
		(message (format "Module %s has no other available versions." mod-name))
	  (let* ((selected-version (ido-completing-read "Select version: " versions))
			 (command (format "go get %s@%s" mod-name (shell-quote-argument selected-version))))
		(message (shell-command-to-string command))))))

(defun go-mod-graph ()
  "Display all dependencies in graph form to show inter-dependencies."
  (interactive "")
  (when (not (executable-find "twopi"))
	(error "Graphviz not installed.  Please install Graphviz to use this command"))
  (when (not (go-mod--mod-enabled))
	(error "Go modules not enabled"))

  (let ((mod-name (or (go-mod--module-on-line) (go-mod--prompt-all-modules)))
		(dep-edges (go-mod--get-dep-graph)))
	(with-current-buffer (generate-new-buffer "*dep-graph*")
	  (insert "strict digraph {\nrankdir=LR;\n")
	  (go-mod--generate-graph-for-mod mod-name dep-edges)
	  (insert "}")
	  (call-process-region (point-min) (point-max) "twopi" nil t nil "-Kdot" (concat "-T" go-mod-graph-output) "-o" (concat "mod-graph." go-mod-graph-output))
	  (kill-buffer (current-buffer)))
	;; TODO: kill buffer
	(find-file-other-window "mod-graph.pdf")))

(defun go-mod--generate-graph-for-mod (mod edges)
  "Write the graph syntax in buffer for transitive dependencies of MOD in graph EDGES."
  (insert (format "\"%s\" [fillcolor=\"#00ADDB\", style=filled]\n" mod))

  (let ((added-edges (make-hash-table :test 'equal)))
	
	;; Go from the MOD to all leaves.
	(let ((from-nodes (cons mod nil)))
	  (while (< 0 (length from-nodes))
		(dolist (a->b edges)
		  (when (and (string-prefix-p (car from-nodes) (car a->b))
					 (not (gethash a->b added-edges)))
			(insert (format "\"%s\" -> \"%s\"\n"
							(nth 0 (split-string (car a->b) "@"))
							(nth 0 (split-string (cdr a->b) "@"))))
			(puthash a->b t added-edges)
			(setq from-nodes (append from-nodes (cons (nth 0 (split-string (cdr a->b))) nil)))))
		(setq from-nodes (cdr from-nodes))))

	;; Go from MOD to the root.
	(let ((to-nodes (cons mod nil)))
	  (while (< 0 (length to-nodes))
		(dolist (a->b edges)
		  (when (and (string-prefix-p (car to-nodes) (cdr a->b))
					 (not (gethash a->b added-edges)))
			(insert (format "\"%s\" -> \"%s\"\n"
							(nth 0 (split-string (car a->b) "@"))
							(nth 0 (split-string (cdr a->b) "@"))))
			(puthash a->b t added-edges)
			(setq to-nodes (append to-nodes (cons (nth 0 (split-string (car a->b))) nil)))))
		(setq to-nodes (cdr to-nodes))))))


(defun go-mod--genrate-dot-graph ()
  "Generate dependency graph.

There must be a buffer *dep-graph* containing the output of `go
mod graph'.  This function is depreciated."
  (with-current-buffer "*dep-graph*"
	(goto-char (point-min))
	(insert "digraph {\n")
	(while (looking-at "[a-zA-Z]")
	  ;; modify digraph lines here
	  (insert "\"")	(skip-chars-forward "^[:blank:]") (insert "\"")
	  (forward-char) (insert "-> \"")
	  (goto-char (line-end-position)) (insert "\"")
	  
	  (forward-line))
	(insert "}")
	(call-process-region (point-min) (point-max) "dot" nil t nil "-Tsvg" "-Kdot" "-o" "dag.svg")))

(defun go-mod--prompt-all-modules ()
  "Prompt the user to select from list of all modules."
  (ido-completing-read "Select module: " (go-mod--get-modules)))

(defun go-mod--prompt-hack-replacements ()
  "Prompt the user for list of modules that have been replaced by gohack."
  (let* ((replacements (go-mod--get-hack-replacements))
		 (modules (mapcar 'car replacements)))
	(if (= 0 (length modules))
		(message "There are no local modules")
	  (let* ((mod-name-version (ido-completing-read "Module to undo: " modules))
			 (mod-name (nth 0 (split-string mod-name-version))))
		mod-name))))

(defun go-mod--prompt-non-hack-modules ()
  "Prompt the user for list of modules that have not been replaced by gohack."
  (let* ((replacements (go-mod--get-hack-replacements))
		 (all-replacements (mapconcat 'identity replacements "|"))
		 (current-mod (go-mod--get-curent-module))
		 (modules (go-mod--get-modules)))
	;; Filter the modules based off the replacemens.
	(setq modules (seq-filter (function (lambda (mod)
										  (and (not (string-match-p (regexp-quote (nth 0 mod))
																	all-replacements))
											   (not (equal (nth 0 mod)
														   current-mod)))))
							  modules))
	(setq modules (mapcar 'car modules))
	(ido-completing-read "Module to copy run locally: " modules)))

(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-mode))
(add-to-list 'auto-mode-alist '("go\\.sum\\'" . go-mod-sum-mode))

(provide 'go-mod-mode)

;;; go-mod-mode.el ends here
