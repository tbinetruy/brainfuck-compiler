(load-file "./vm.el")


(defun lexer//lex (code)
  (split-string code ""))


(defmacro push-instruction (instr)
  `(setq instructions (nconc instructions (list ,instr))))

(defun compiler//init-code (instructions)
  "Initialize the program counter, stored in ecx, to 0."
  (push-instruction '("PUSH" 0))
  (push-instruction '("PUSH" ecx))
  (push-instruction '("STORE"))
  instructions)

(defun compiler//increment-value (instructions val)
  "Increments by val the content of the address pointed at by the pc.

The steps are as follows:
- Get the program counter from ecx and store it to eax.
  This is the address at which we want to increment the value.
- We then read from the ram at the address in eax (the pc)
- The value we read is now in eax, and we want to load it on the stack
- We can now add 1 to the value we pushed on the stack
- and store it back in ebx.
- Finally, we store the pc in eax again
- and call the store to ram syscall."
  (push-instruction '("PUSH" ecx))
  (push-instruction '("LOAD"))
  (push-instruction '("PUSH" eax))
  (push-instruction '("STORE"))

  (push-instruction '("READ_RAM"))

  (push-instruction '("PUSH" eax))
  (push-instruction '("LOAD"))

  (push-instruction `("PUSH" ,val))
  (push-instruction '("ADD"))

  (push-instruction '("PUSH" ebx))
  (push-instruction '("STORE"))

  (push-instruction '("PUSH" ecx))
  (push-instruction '("LOAD"))
  (push-instruction '("PUSH" eax))
  (push-instruction '("STORE"))

  (push-instruction '("WRITE_RAM"))
  instructions)

(defun compiler//compile (code)
  (let ((tokens (lexer//lex code))
        (instructions '()))
    (setq instructions (compiler//init-code instructions))
    (dolist (el tokens)
      (if (equal el "-")
          (setq instructions (compiler//increment-value instructions -1)))
      (if (equal el "+")
          (setq instructions (compiler//increment-value instructions 1))))
    instructions))

(message "%s" (lexer//lex "++>>"))
(message "%s" (compiler//compile "+"))

(message "%s" (compiler//compile "++-"))

(message "%s" (vm//main (compiler//compile "++-") t))

;(message "%s" (vm//main '(("PUSH" 3)
;                          ("PUSH" eax)
;                          ("STORE")
;                          ("PUSH" 20)
;                          ("PUSH" ebx)
;                          ("STORE")
;                          ("WRITE_RAM")
;                          ("PUSH" 3)
;                          ("PUSH" eax)
;                          ("STORE")
;                          ("READ_RAM")
;                          ("PUSH" 1))
;                        t))
