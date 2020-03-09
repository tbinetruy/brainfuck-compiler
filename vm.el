(defun vm//op (stack op)
  (push (funcall op (pop stack) (pop stack)) stack))

(defun vm//pop (stack)
  (cdr stack))

(defun vm//store (stack registers)
  (setcdr (assq (pop stack) registers) (pop stack))
  stack)

(defun vm//load (stack registers)
  (push (cdr (assq (pop stack) registers)) stack))

(defun vm//push (stack val)
  (push val stack))

(defun vm//increment-pc (registers val)
  (setcdr (assq 'pc registers) (+ (alist-get 'pc registers) val)))

(defun vm//set-pc (registers val)
  (setcdr (assq 'pc registers) val))

(defun vm//if (stack registers)
  (if (equal (pop stack) 0)
      (vm//increment-pc registers 1))
  stack)

(defun vm//write-ram (registers ram)
  "Store value at ram addr
eax: ram addr
ebx: value"
  (let ((addr (alist-get 'eax registers))
        (val (alist-get 'ebx registers)))
    (setcar (nthcdr addr ram) val))
  ram)

(defun vm//read-ram (registers ram)
  "Read value from ram. Return value in eax
eax: ram addr
return eax: value at ram addr"
  (let ((addr (alist-get 'eax registers)))
    (setcdr (assq 'eax registers) (car (nthcdr addr ram)))))

(defun vm//append-stdout (stdout registers)
  "Appends the ascii-code char stored in eax to stdout"
  (concat stdout (char-to-string (alist-get 'eax registers))))

(defun vm//main (instructions &optional log)
  (let ((stack '())
        (registers (list
                    (cons 'eax nil)  ; useful for counters
                    (cons 'ebx nil)
                    (cons 'ecx nil)
                    (cons 'pc  0)))  ; program counter
        (ram (make-list 10 0))
        (stdout ""))
    (while (< (alist-get 'pc registers) (length instructions))
      (let* ((elt (nth (alist-get 'pc registers) instructions))
             (key (nth 0 elt))
             (val (nth 1 elt)))
        (if (equal key "APPEND_STDOUT")
            (setq stdout (vm//append-stdout stdout registers)))
        (if (equal key "READ_RAM")
            (vm//read-ram registers ram))
        (if (equal key "WRITE_RAM")
            (setq ram (vm//write-ram registers ram)))
        (if (equal key "LOAD")
            (setq stack (vm//load stack registers)))
        (if (equal key "STORE")
            (setq stack (vm//store stack registers)))
        (if (equal key "AJUMP") ; absolute jump
            (vm//set-pc registers val))
        (if (equal key "RJUMP") ; relative jump
            (vm//increment-pc registers val))
        (if (equal key "IF")
            (setq stack (vm//if stack registers)))
        (if (equal key "POP")
            (setq stack (vm//pop stack)))
        (if (equal key "PUSH")
            (setq stack (vm//push stack val)))
        (if (equal key "DIV")
            (setq stack (vm//op stack '/)))
        (if (equal key "MUL")
            (setq stack (vm//op stack '*)))
        (if (equal key "SUB")
            (setq stack (vm//op stack '-)))
        (if (equal key "ADD")
            (setq stack (vm//op stack '+)))
        (if log
            (message "%s %s %s %s" elt stack registers ram)))
      (vm//increment-pc registers 1))
    `(,stdout ,stack ,registers ,ram)))


;(message "%s" (vm//main '(("PUSH" 20)
;			  ("PUSH" ebx)
;			  ("STORE")
;			  ("PUSH" ebx)
;			  ("LOAD"))))

;(message "%s" (vm//main '(("PUSH" 1)
;			  ("PUSH" 4)
;			  ("ADD")
;			  ("RJUMP" 1)
;			  ("POP")
;			  ("PUSH" 10)
;			  ("ADD"))))

;; read ram
;(message "%s" (vm//main '(("PUSH" 0)
;                          ("PUSH" eax)
;                          ("STORE")
;                          ("PUSH" 10)
;                          ("PUSH" ebx)
;                          ("STORE")
;                          ("WRITE_RAM")
;                          ("PUSH" 0)
;                          ("PUSH" eax)
;                          ("STORE")
;                          ("READ_RAM"))
;                        t))

;; write ram
;(message "%s" (vm//main '(("PUSH" 0)
;                          ("PUSH" eax)
;                          ("STORE")
;                          ("PUSH" 10)
;                          ("PUSH" ebx)
;                          ("STORE")
;                          ("WRITE_RAM"))
;                        t))

; (message "%s" (vm//main '(("PUSH" 5)
;                           ("PUSH" eax)
;                           ("STORE")
;                           ("PUSH" 1)
;                           ("PUSH" eax)
;                           ("LOAD")
;                           ("SUB")
;                           ("PUSH" eax)
;                           ("STORE")
;                           ("PUSH" eax)
;                           ("LOAD")
;                           ("IF")
;                           ("AJUMP" 2) ; if true
;                           ("RJUMP" 1) ; else
;                           ("POP")
;                           ("PUSH" 10)
;                           ("PUSH" eax)
;                           ("LOAD")
;                           ("ADD"))
;                         t))
