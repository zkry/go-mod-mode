;;; Code: -*- lexical-binding: t -*-

;;; Commentary:

;;;###autoload

;; TODO v1.1.5-pre doesn't match.
(makunbound 'go-mod--source-regexp)
(makunbound 'go-mod-mode-hook)
(makunbound 'go-mod-mode-map)
(makunbound 'go-mod-font-lock-keywords-1)
(makunbound 'go-mod-font-lock-keywords)
(makunbound 'go-mod-sum-font-lock-keywords)
(makunbound 'go-mod-sum-font-lock-keywords-1)
(makunbound 'go-mod-mode-syntax-table)
(fmakunbound 'go-mod-mode)


(defconst go-mod--source-regexp
  "\\<[a-z]+\\(?:\\.[a-z]+\\)+/\\(?:[[:alnum:]-_]+/\\)*[[:alnum:]-_]+\\(?:\\.v[0-9][0-9]?\\)?"
  "Regexp for finding source names such as github.com/lib/pq.")

(defconst go-mod--version-regexp
  "\\<v[0-9]\\.[0-9]+\\.[0-9]+\\(?:-0.[0-9a-f]+\\)?\\(?:-[0-9a-f]+\\)*\\(?:+incompatible\\)?\\(?:/go.mod\\)?\\>"
  "Regexp for finding version strings.")

;;; initialize the go mod mode hook.
(defvar go-mod-mode-hook nil)
(defvar go-mod-sum-mode-hook nil)

(defvar go-mod--version-store nil
  "Hashmap to store list of modules with corresponding versions.")
(defvar go-mod--replacement-store nil
  "Alist to store list of modules with corresponding replacement")

;;; go mod keybindings map
(defvar go-mod-mode-map
  (let ((map (make-keymap)))
;;    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for WPDL major mode.")

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
(defun go-mod-mode ()
  "Major mode for editing go mod files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table go-mod-mode-syntax-table)
  (use-local-map go-mod-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(go-mod-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'go-mod-indent-line)
  (setq major-mode 'go-mod-mode)
  (set (make-local-variable 'eldoc-documentation-function)
	   'go-mod-eldoc-function)
  (go-mod--initialize-version-cache)
  (set (make-local-variable 'go-mod--replacement-store) '())
  (run-hooks 'go-mod-mode-hook))

(defun go-mod-sum-mode ()
  "Major mode for viewing go sum files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table (make-syntax-table))
  (use-local-map (make-keymap))
  (set (make-local-variable 'font-lock-defaults) '(go-mod-sum-font-lock-keywords))
  (setq major-mode 'go-mod-sum-mode)
  (run-hooks 'go-mod-sum-mode-hook))

;;; ==================
;;; eldoc
;;; ==================

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
	   (t (let ((cmd-output (split-string
						   (shell-command-to-string (concat "go list -m -u " mod-name))
						   "\n")))
			(message "==cache-miss==")
			;; Remove the garbage lines at the beginning of cmd output.
			(while (and cmd-output (or (string-prefix-p "go:" (nth 0 cmd-output))
									   (string-prefix-p "cmd output: go:" (nth 0 cmd-output))
									   (equal "" (nth 0 cmd-output))))
			  (message (nth 0 cmd-output))
			  (setq cmd-output (cdr cmd-output)))
            
			;; If command remains process it, else return blank.
            (if cmd-output
				(let ((mod-ver-upg (split-string (nth 0 cmd-output) " ")))
				  (go-mod--add-to-version-cache mod-name
												(nth 1 mod-ver-upg)
												(nth 2 mod-ver-upg))
				  (go-mod--eldoc-format-version-list mod-name))
			  "Error in go.mod file")))))))

(defun go-mod--eldoc-format-version-list (mod-name)
  "Return a string for displaying MODULE information."
  ;; TODO To be implemented.
  (let ((versions (gethash mod-name go-mod--version-store))
		(replacement (cdr (assoc mod-name go-mod--replacement-store))))
	(concat
	 (nth 0 versions) ; original version
	 (if (nth 1 versions) (concat " ↑" (nth 1 versions)) "")
	 (if replacement (format " =>\"%s\"" replacement) ""))))

;;; ====================
;;; version-cache
;;; ====================

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

;;; end version-cache code

;;; =====================
;;; data extraction code
;;; =====================

(defun go-mod--get-modules ()
  "Get all current modules in a list."
  (when (not (go-mod--mod-enabled)) (error "Go modules not turned on"))
  (mapcar 'split-string (process-lines "go" "list" "-m" "all")))

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
  (make-process :name "go-mod-upgradable"
				:buffer "*go-mod-upgradable*"
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

;;; end data extraction code

;;; ==========================
;;; general commands
;;; ==========================

(defun go-mod-modules ()
  "List all of the current modules."
  (interactive)
  (when (not (go-mod--mod-enabled)) (error "Go mod must be enabled")))

(defun go-mod--mod-enabled ()
  "Return if go-mod is supported."
  (and (equal "on" (getenv "GO111MODULE"))
	   (not (equal "command-line-arguments\n" (shell-command-to-string "go list -m")))))

(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-mode))
(add-to-list 'auto-mode-alist '("go\\.sum\\'" . go-mod-sum-mode))

(provide 'go-mod-mode)

;;; go-mod-mode.el ends here
