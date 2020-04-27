(load-file "./bf-repl-mode.el")

(defun bf-mode-expand-buffer ()
  "Expands Brainfuck shorthand:
4+5-2> becomes ++++----->>"
  (interactive)
  (goto-char 0)
  (while (re-search-forward "\\([0-9]+\\)\\([-\\|+\\|<\\|>\\|.]\\)" nil t)
    (let* ((n-string (buffer-substring (match-beginning 0) (match-end 0)))
           (n (string-to-number n-string))
           (token-string (buffer-substring (match-end 1) (+ 1 (match-end 1))))
           (token (string-to-char token-string)))
      (replace-match (make-string n token)))))

(defun bf-mode-start-repl ()
  "Opens a window and instantiates a Brainfuck REPL in it."
  (interactive)
  (split-window-right)
  (windmove-right)
  (get-buffer-create "*Brainfuck REPL*")
  (switch-to-buffer "*Brainfuck REPL*")
  (bf-repl-mode))

(defun bf-mode-evaluate-buffer ()
  "Compiles the current buffer and evaluates it.
The result is echoed to the mini-buffer."
  (interactive)
  (let ((orig-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring orig-buf)
      (bf-mode-expand-buffer)
      (let ((buffer (buffer-string))
            (src "")
            (result nil))
        (setq src (replace-regexp-in-string
                   " \\|\n\\|#.*\n"
                   ""
                   buffer))
        (setq result (vm//main (compiler//compile src t)))
        (message "stdout: %s \nstack: %s \nregisters: %s \nheap: %s"
                 (car result)
                 (cadr result)
                 (caddr result)
                 (cadddr result))))))

(defvar bf-mode-hook nil)

(defvar bf-mode-map nil "Keymap for BF major mode")
(setq bf-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-ce" 'bf-mode-expand-buffer)
    (define-key map "\C-cE" 'bf-mode-evaluate-buffer)
    (define-key map "\C-cr" 'bf-mode-start-repl)
    map))

(defvar bf-mode-syntax-table nil "Syntax table for bf-mode")
(setq bf-mode-syntax-table
      (let ((st (make-syntax-table)))
        (modify-syntax-entry ?# "<" st)
        (modify-syntax-entry ?\n ">" st)
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

(defun  bf-mode-eldoc-function ()
  "Prints to documentation for Brainfuck tokens in the
mini-buffer using Eldoc."
  (let ((current-char (char-to-string (char-after)))
        (return-value "")
        (loop-start-doc "Starts a loop. If the heap value pointed too is zero, then end the loop.")
        (loop-end-doc "Return to the beginning of the loop.")
        (increment-heap-doc "Increment the heap value pointed to.")
        (decrement-heap-doc "Decrement the heap value pointed to.")
        (increment-heap-pointer-doc "Increment the heap pointer.")
        (decrement-heap-pointer-doc "Decrement the heap pointer.")
        (append-stdout-doc "Append the heap value pointed to to stdout."))
    (defmacro catch-token (token doc)
      `(if (equal current-char ,token)
          (setq return-value ,doc)))
    (catch-token "[" loop-start-doc)
    (catch-token "]" loop-end-doc)
    (catch-token "+" increment-heap-doc)
    (catch-token "-" decrement-heap-doc)
    (catch-token ">" increment-heap-pointer-doc)
    (catch-token "<" decrement-heap-pointer-doc)
    (catch-token "." append-stdout-doc)
    return-value))

(defun bf-mode ()
  "Major mode for editing Brainfuck files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bf-mode-syntax-table)
  (use-local-map bf-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(bf-font-lock-keywords))
  (setq major-mode 'bf-mode)
  (setq mode-name "Brainfuck")
  (run-hooks 'bf-mode-hook)
  (set (make-local-variable 'eldoc-documentation-function) 'bf-mode-eldoc-function)
  (flycheck-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bf\\'" . bf-mode))

(defun bf-mode-check-matching-brackets ()
  "Count open and close brackets in the buffer
saving the cursor location."
  (let ((open-count 0)
        (close-count 0))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "\\[" nil t)
        (setq open-count (1+ open-count)))
      (goto-char 0)
      (while (re-search-forward "\\]" nil t)
        (setq close-count (1+ close-count))))

    (not (equal open-count close-count))))

(defun bf-mode-lint (checker callback)
  "Check if the counters match or not:
If not: call the Flycheck callback with an error
        at the first opening bracket in the buffer.
Otherwise: call the Flycheck callback with no errors."
  (interactive)
    (if (bf-mode-check-matching-brackets)
        (funcall callback
                 'finished
                 (save-excursion
                   (goto-char 0)
                   (re-search-forward "\\[" nil t)
                   (list (flycheck-error-new-at
                          (line-number-at-pos) (current-column) 'info
                          "Unbalanced loop" :checker checker))))
      (funcall callback 'finished nil)))

(flycheck-define-generic-checker 'brainfuck
  "A Brainfuck syntax checker."
  :start #'bf-mode-lint
  :modes 'bf-mode)

(add-to-list 'flycheck-checkers 'brainfuck)
