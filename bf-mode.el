(load-file "./bf-repl-mode.el")

(defun bf-mode-expand-buffer ()
  (interactive)
  (goto-char 0)
  (while (re-search-forward "\\([0-9]+\\)\\([-\\|+\\|<\\|>\\|.]\\)" nil t)
    (let* ((n-string (buffer-substring (match-beginning 0) (match-end 0)))
           (n (string-to-number n-string))
           (token-string (buffer-substring (match-end 1) (+ 1 (match-end 1))))
           (token (string-to-char token-string)))
      (replace-match (make-string n token)))))

(defun bf-mode-start-repl ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (get-buffer-create "*Brainfuck REPL*")
  (switch-to-buffer "*Brainfuck REPL*")
  (bf-repl-mode))

(defvar bf-mode-hook nil)

(defvar bf-mode-map nil "Keymap for BF major mode")
(setq bf-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-ce" 'bf-mode-expand-buffer)
    (define-key map "\C-cr" 'bf-mode-start-repl)
    map))

(defvar bf-mode-syntax-table nil "Syntax table for bf-mode")
(setq bf-mode-syntax-table
      (let ((st (make-syntax-table)))
        (modify-syntax-entry ?/ "<" st)
        (modify-syntax-entry ?* ">" st)
        st))

(defvar bf-font-lock-keywords nil
  "Default highlighting expressions for bf-mode")
(setq bf-font-lock-keywords
      (list
       '("\\+" . font-lock-variable-name-face)
       '("\\-" . font-lock-function-name-face)
       '("\\." . font-lock-keyword-face)
       '("<" . font-lock-doc-face)
       '(">" . font-lock-constant-face)))


(defun bf-mode ()
  "Major mode for editing Brainfuck files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bf-mode-syntax-table)
  (use-local-map bf-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(bf-font-lock-keywords))
  (setq major-mode 'bf-mode)
  (setq mode-name "Brainfuck")
  (run-hooks 'bf-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bf\\'" . bf-mode))
