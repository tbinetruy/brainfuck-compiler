(load-file "./vm.el")
(require 'cl-lib)


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

(defun compiler//increment-pc (instructions val)
  "Increments the value of the program counter by val.

It works as follows:
- Push the value of ecx (where the pc is stored) onto the stack
- Push val on the stack
- Call the add instruction
- Store the resulting value back into ecx"
  (push-instruction '("PUSH" ecx))
  (push-instruction '("LOAD"))

  (push-instruction `("PUSH" ,val))

  (push-instruction '("ADD"))

  (push-instruction '("PUSH" ecx))
  (push-instruction '("STORE")))

(defun compiler//loop-start (instructions jump-length)
  "Check that value at pc (stored in ecx) is not 0.
If it is, then jump by jump-length to matching ]
otherwise, jump over the else instruction to the loop body."
  (push-instruction '("PUSH" ecx))
  (push-instruction '("LOAD"))
  (push-instruction '("PUSH" eax))
  (push-instruction '("STORE"))
  (push-instruction '("READ_RAM"))
  (push-instruction '("PUSH" eax))
  (push-instruction '("LOAD"))
  (push-instruction '("IF"))
  (push-instruction '("RJUMP" 1))
  (push-instruction `("RJUMP" ,(+ 1 jump-length)))
  instructions)

(defun compiler//loop-end (instructions jump-length)
  "Loop back to the start of the loop. It uses a relative jump backwards
with length equal to that of the loop body (jump-length) added to then
loop head code (added by compiler//loop-start)"
  (let ((loop-head-length -11))  ; number of instructions in loop head
    (push-instruction `("RJUMP" ,(+ loop-head-length (- jump-length)))))
  instructions)

(defun find-matching-char (tokens open-char close-char current-pos)
  "Finds the position in an array of chars of the matching characer."
  (let ((counter 0)
        (return-value 0))
    (dolist (el (nthcdr current-pos tokens))
      (if (equal 0 return-value)
          (progn (if (equal el open-char)
                     (setq counter (1+ counter)))
                 (if (equal el close-char)
                     (setq counter (1- counter)))
                 (if (equal counter 0)
                     (setq return-value current-pos)
                   (setq current-pos (1+ current-pos))))))
    return-value))


(defun compiler//loop (code tokens instructions current-pos)
  "Idea:
- keep a record of loop beginnig because we'll need it for the jump
- get value in ram at addr stored in pc and check if 0
  + if 0 then  jump to end of loop location
  + if not call compiler//compile at current-pos+1"
  (let ((start-pos current-pos)
        (matching-pos (find-matching-char tokens "[" "]" current-pos))
        (current-token (nth current-pos tokens))
        (middle-instructions '()))
      (setq middle-instructions (nconc middle-instructions
                                       (compiler//compile
                                        (cl-subseq code
                                                   current-pos
                                                   (1- matching-pos))
                                        nil)))
    (setq instructions (compiler//loop-start instructions
                                             (length middle-instructions)))
    (setq instructions (nconc instructions middle-instructions))
    (message "%s" middle-instructions)
    (setq instructions (compiler//loop-end instructions
                                           (length middle-instructions))))
  instructions)

(defun compiler//append-stdout (instructions)
  "Appends the char in pc (stored in ecx) to stdout.

It works as follows:
- store the pc from ecx to eax
- call the READ_RAM instruction which reads ram at the
  address stored in eax and writes the result in eax.
- call the APPEND_STDOUT instruction, which writes to
  stdout the ascii code stored in eax."
  (push-instruction '("PUSH" ecx))
  (push-instruction '("LOAD"))
  (push-instruction '("PUSH" eax))
  (push-instruction '("STORE"))
  (push-instruction '("READ_RAM"))
  (push-instruction '("APPEND_STDOUT"))
  instructions)

(defun compiler//compile (code include-init-code)
  "Compiles a brainfuck code string. If this function is called
recursively, set include-init-code to t in the first call and to
nil and all subsequent calls."
  (let ((tokens (lexer//lex code))
        (instructions '())
        (current-pos 0))
    (if include-init-code
        (setq instructions (compiler//init-code instructions)))
    (while (< current-pos (length tokens))
      (let ((el (nth current-pos tokens)))
        (if (equal el "[")
            (progn
              (setq instructions (compiler//loop code
                                                 tokens
                                                 instructions
                                                 current-pos))
              (setq current-pos (find-matching-char tokens "[" "]" current-pos))))
        (if (equal el ".")
            (setq instructions (compiler//append-stdout instructions)))
        (if (equal el ">")
            (setq instructions (compiler//increment-pc instructions 1)))
        (if (equal el "<")
            (setq instructions (compiler//increment-pc instructions -1)))
        (if (equal el "-")
            (setq instructions (compiler//increment-value instructions -1)))
        (if (equal el "+")
            (setq instructions (compiler//increment-value instructions 1))))
      (setq current-pos (1+ current-pos)))
    instructions))

;; Hello world example. Code taken from:
;; https://en.wikipedia.org/wiki/Brainfuck#Hello_World!
(setq src "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
(message "%s" (vm//main (compiler//compile src t) nil)) ; "prints Hello World!"
