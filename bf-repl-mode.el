(load-file "./vm.el")
(load-file "./bf-compiler.el")

(defun bf-repl//eval-line ()
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (src (replace-regexp-in-string "=: " "" line))
         (result (vm//main (compiler//compile src t))))
    (insert (concat "\n" (car result) "\n\n=: "))))

(defun bf-repl//insert-init-text ()
  (interactive)
  (insert "Welcome to the Brainfuck REPL\n\nPress C-c C-c with the cursor on the last line to evaluate it.\n\n\n=: "))

(defvar bf-repl-mode-hook nil)

(defvar bf-repl-mode-map nil "Keymap for BF REPL major mode")
(setq bf-repl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-c" 'bf-repl//eval-line)
    map))

(defun bf-repl-mode ()
  "Major mode for a live Brainfuck REPL"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bf-mode-syntax-table)
  (use-local-map bf-repl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(bf-font-lock-keywords))
  (setq major-mode 'bf-repl-mode)
  (setq mode-name "Brainfuck REPL")
  (run-hooks 'bf-repl-mode-hook)
  (bf-repl//insert-init-text))
