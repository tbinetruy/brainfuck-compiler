;;; c-lexer.el --- Support for the Foo programming language

;; Copyright (C) 2010-2019 Your Name

;; Author: Your Name <yourname@example.com>
;; Maintainer: Someone Else <someone@example.com>
;; Created: 14 Jul 2010
;; Keywords: languages
;; Homepage: http://example.com/foo
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This file is free software...
;; along with this file.  If not, see <https://www.gnu.org/licenses/>

(defconst SEPARATOR (split-string ";{}()[]'\"" ""))
(defconst OPERATOR (split-string "/ * + ++ - -- | || & && < <= >= > == !=" " "))
(defconst SPECIAL-CHARS (split-string " ;/+*-{}()[]'\"|&<>=" ""))

(defconst KEYWORD (split-string "int float double function return true false"))

(defconst TOKENS '((identifier . 1)
                   (specialchar . 2)
                   (separator . 3)
                   (operator . 4)
                   (keyword . 5)))

(defun lexer/create-keyword-token (name)
  `(((name . ,name)
     (type . ,(alist-get 'keyword KEYWORD)))))

(defun lexer/create-identifier-token (name)
  `(((name . ,name)
     (type . ,(alist-get 'identifier TOKENS)))))

(defun lexer/create-operator-token (name)
  `(((name . ,name)
     (type . ,(alist-get 'operator TOKENS)))))

(defun lexer/create-separator-token (name)
  `(((name . ,name)
     (type . ,(alist-get 'separator TOKENS)))))

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
                                  (lexer/create-identifier-token
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

;;; c-lexer.el ends here
