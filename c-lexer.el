(defconst SPECIAL-CHARS (split-string " ;/+*-{}()[]'\"|&<>=" ""))
(defconst TOKENS '((word . 1)
                   (specialchar . 2)))

(defun lexer/create-word-token (name)
  `(((name . ,name)
     (type . ,(alist-get 'word TOKENS)))))

(defun lexer/create-special-char-token (name)
  `(((name . ,name)
     (type . ,(alist-get 'specialchar TOKENS)))))

(defun lexer/lex (src)
  "Trasforms c source code into a list of alists describing
the tokens.

Strategy:
- We remove \n from source code
- We iterate through all the characters in src
  + at each iteration, if the characer is not a specialchar,
    we add it to a current-word string
  + if it is a special char, we add a token of type WORD to the
    list of tokens with value current-word. Unless the current-
    character is a space, we add a token of type SPECIAL-CHAR to
    the list of tokens to return."
  (setq src (s-replace "\n" "" src))
  (let ((tokens '())
        (char-list (split-string src ""))
        (current-word "")
        (current-char "")
        (i 0))
    (while (< i (length char-list))
      (progn
        (setq current-char (nth i char-list))
        (if (and (member current-char SPECIAL-CHARS)
                 (not (equal current-char "")))
            (progn
              (if (not (equal 0 (length current-word)))
                  (progn
                    (setq tokens (nconc
                                  tokens
                                  (lexer/create-word-token
                                   current-word)))
                    (setq current-word "")))
              (if (not (equal current-char " "))
                  (setq tokens (nconc
                                tokens
                                (lexer/create-special-char-token
                                 current-char)))))
          (setq current-word (concat current-word current-char)))
        (setq i (1+ i))))
    tokens))

(setq src "int main() {
int a = 1;
a = 1 + 1;
return a;
}")
(setq result (lexer/lex src))
(message "%s" result)
