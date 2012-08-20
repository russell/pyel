;;; -*- lexical-binding: t -*-
;;; pyel.el --- an improved Python mode.

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author:  Russell Sim <russell.sim@gmail.com
;; Version:  See `pyel-version'
;; Keywords:  languages, python

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(defvar *pyel-lex* nil "The current lexer.")

(defmacro awhen (test if-expr)
  "An anaphoric variant of (when ...). The value of the test
expression is locally bound to 'it' during execution of the
consequent clauses. The binding is present in both consequent
branches."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,if-expr)))

(defmacro aif (test if-expr &optional else-expr)
  "An anaphoric variant of (if ...). The value of the test
expression is locally bound to 'it' during execution of the
consequent clauses. The binding is present in both consequent
branches."
  (declare (indent 1))
  `(let ((it ,test))
     (if it ,if-expr ,else-expr)))

(defstruct (pyel-lexer
            (:constructor nil)
            (:constructor make-pyel-lexer
                          (&key string
                                (point 1)
                                (tab-width 4)
                                indent-stack
                                decorators)))
  "The lexer state object."
  string
  point
  tab-width
  indent-stack
  decorators)               ; a lisp list of the child statement nodes

(defun pyel-point ()
  "Return the currnent position of the lexer."
  (pyel-lexer-point *pyel-lex*))

(defun pyel-char-after ()
  "Return a character in current buffer at the lexers current
position."
  (char-after (pyel-lexer-point *pyel-lex*)))

(defun pyel-char-peek ()
  "Return a character in current buffer at the one position past
the lexers current position."
  (char-after (1+ (pyel-lexer-point *pyel-lex*))))

(defun pyel-forward-char (&optional N)
  "Move lexer point N characters forward (backward if N is
negative)."
  (incf (pyel-lexer-point *pyel-lex*) (or N 1)))

(defun pyel-char-uppercase-p (c)
  "Return non-nil if C is an uppercase character.  Handles
Unicode and latin chars properly."
  (/= c (downcase c)))

(defun pyel-char-lowercase-p (c)
  "Return non-nil if C is an uppercase character.  Handles
Unicode and latin chars properly."
  (/= c (upcase c)))

(defun pyel-number-p (c)
  "Return non-nil if C is a numeric character."
  (and (>= c ?0) (<= c ?9)))

(defun pyel-whitespace-p (c)
  "Return non-nil if C is a whitespace character."
  (memq c '(?\s ?\t ?\n ?\r)))

(defun pyel-identifier-part-p (c)
  "Check if the character is a valid part of an identifier.  If
the name is made up of characters or numbers."
  (or
   (memq c '(?_))
   (pyel-char-uppercase-p c)
   (pyel-char-lowercase-p c)
   (pyel-number-p c)))

;;
;; Keyword Handling.
;;

(defconst pyel-keywords
  '(break
    __debug__
    as assert
    break
    def
    class continue
    else elif except
    finally for
    global
    if in import
    lambda
    None
    pass
    raise return
    try
    while with
    yield))

(defconst pyel-keyword-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in pyel-keywords
          do (puthash
              (symbol-name k)           ; "return"
              (intern (upcase (symbol-name k)))                         ; (quote return)
              table))
    table)
  "Python keywords by name, mapped to their symbols.")

;;
;; Reserved words
;;

(defconst pyel-reserved-words
  '(Ellipsis
    False None NotImplemented True
    __debug__ copyright credits
    exit license quit))

(defconst pyel-reserved-word-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in pyel-reserved-words
          do (puthash
              (symbol-name k)
              'RESERVED
              table))
    table)
  "Python reserved words by name, mapped to 'RESERVED.")

(defun pyel-string-to-keyword (s)
  "Return token for S, a string, if S is a keyword or reserved word.
Returns a symbol such as 'pyel-BREAK, or nil if not keyword/reserved."
  (or (gethash s pyel-keyword-names)
      (gethash s pyel-reserved-word-names)))



;; Format of AST nodes,
;;
;; Each node will be a list, it will need to know it's absolute
;; location and length.  Any issues parsing the node will also be
;; stored on it.  The type will store the node type.

;; Generic type:
;;
;; (list :type TYPE
;;       :beg 1
;;       :length 7
;;       :value banana
;;       :error "")

;; Number type:
;;
;; (list :type NUMBER
;;       :beg 1
;;       :length 4
;;       :base 10
;;       :value 1234
;;       :error "")

(defun pyel-lex-name ()
  "Parse a name token from the current point of the lexer."
  (flet ((bsnp (start end)
               (buffer-substring-no-properties start end)))
    (let* ((start-pos (pyel-point))
           (token (progn (while (pyel-identifier-part-p (pyel-char-after))
                           (pyel-forward-char))
                         (list :value (bsnp start-pos (pyel-point))
                               :beg start-pos
                               :len (- (pyel-point) start-pos)))))
      (aif (pyel-string-to-keyword (plist-get token :value))
        (plist-put token :type it)
        (plist-put token :type 'NAME)))))

(defun pyel-convert-name-to-base (c)
  "Convert char to equivilant base.  Signals an 'unsupported-base
if the base isn't recognised."
  (case c
    (?o 8)
    (?O 8)
    (?x 16)
    (?X 16)
    (?b 2)
    (?B 2)
    (t (if (or (pyel-number-p c) (eq c ?.))
           10
         (signal 'scan-error
                 (list (format "Unsupported base: %s." (char-to-string c))))))))

;; TODO the handling of complex numbers needs to be improved.
(defun pyel-lex-number ()
  "Parse a number token from the current point of the lexer.
Numbers start with a digit but may contain a second character
which can be either o (Octal) an x (Hexadecimal) or b (Binary)."
  (flet ((bsnp (start end)
               (buffer-substring-no-properties start end)))
    (let* ((node (list :beg (pyel-point)))
           (base (condition-case err
                     (pyel-convert-name-to-base (pyel-char-peek))
                   (scan-error
                    (plist-put node :error (cadr err))
                    10)))
           (base-char (+ ?0 base)))
      (flet ((pyel-valid-number-p (c)
                                  (and (>= c ?0) (< c base-char))))
        ;; If there is a base char then skip it.
        (when (or (plist-get node :error) (not (eq base 10)))
          (pyel-forward-char 2))
        (let ((value-pos (pyel-point))
              passed-dot-p
              passed-ell-p
              passed-exponent-p
              passed-complex-p)
          ;; Keep going forward until we hit a char that isn't a number.

          ;; TODO This should be changed to stop when it hits a
          ;; delimiter.  Currently there will be problems with really
          ;; badly formed numbers.
          (while  (pyel-number-p (pyel-char-after))
            (pyel-forward-char)
            (let ((current-char (pyel-char-after)))
              (cond
               ;; Handle exponents.
               ((memq current-char '(?e ?E))
                (progn
                  (when passed-exponent-p
                    (plist-put node :error "Invalid syntax, number can't have more than one exponent."))
                  (setq passed-exponent-p t)
                  (pyel-forward-char)     ; skip the exponent.
                  (when (eq (pyel-char-after) ?-)
                    (pyel-forward-char)))) ; skip the - if after an exponent.

               ;; Handle decimal points.
               ((eq current-char ?.)
                (when passed-dot-p
                  (plist-put node :error "Invalid syntax, number can't contain more than one decimal point."))
                (unless (pyel-number-p (pyel-char-peek))
                  (plist-put node :error "Invalid syntax, number with a decimal point."))
                (unless (eq base 10)
                  (plist-put node :error "Invalid syntax, decimal point within non base 10 number."))
                (setq passed-dot-p t)
                (pyel-forward-char))

               ;; Handle Long integer.
               ((memq current-char '(?l ?L))
                (when passed-ell-p
                  (plist-put node :error "Invalid syntax, number can't contain more than one long modifier."))
                (setq passed-ell-p t)
                (pyel-forward-char))

               ;; Handle Long integer.
               ((memq current-char '(?j ?J))
                (when passed-ell-p
                  (plist-put node :error "Invalid syntax, number can't contain more than one complex part."))
                (setq passed-ell-p t)
                (pyel-forward-char)))))
          ;; Don't try and parse the number if there has already been an
          ;; error or if it's a long int.
          (if (or passed-ell-p (plist-get node :error))
              (plist-put node
                         :value (bsnp (plist-get node :beg) (pyel-point)))
            (plist-put node
                       :value (string-to-number (bsnp value-pos (pyel-point)) base)))
          (plist-put node :len (- (pyel-point) (plist-get node :beg)))
          (plist-put node :base base)
          (plist-put node :type 'NUMBER)
          node)))))

(defun pyel-lex-operator ()
  "Parse a operator from the current point of the lexer."
  (flet ((bsnp (start end)
               (buffer-substring-no-properties start end)))
    (let* ((start-pos (pyel-point))
           (token (progn (while (pyel-identifier-part-p (pyel-char-after))
                           (pyel-forward-char))
                         (list :value (bsnp start-pos (pyel-point))
                               :beg start-pos
                               :len (- (pyel-point) start-pos)))))
      (aif (pyel-string-to-keyword (plist-get token :value))
        (plist-put token :type it)
        (plist-put token :type 'NAME)))))
