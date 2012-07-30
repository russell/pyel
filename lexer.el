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

;; Format of AST nodes,
;;
;; Each node will be a list, it will need to know it's absolute
;; location and length.  Any issues parsing the node will also be
;; stored on it.  The type will store the node type.

;; (list :type TYPE
;;       :beg 0
;;       :length 5
;;       :value
;;       :error "")

(defun pyel-lex-name ()
  "Parse a name token from the current point of the lexer."
  (flet ((bsnp (start end)
               (buffer-substring-no-properties start end)))
    (let* ((start-pos (pyel-point))
           (token (progn (while (pyel-identifier-part-p (pyel-char-after))
                           (pyel-forward-char))
                         (list :str (bsnp start-pos (pyel-point))
                               :beg start-pos
                               :end (pyel-point)))))
      ;; XXX should also detect other keywords like def, while
      (aif (pyel-string-to-keyword (plist-get token :str))
        (if (eq it 'RESERVED)
            (plist-put token :type (pyel-token-code it))
          (plist-put token :type 'NAME))
        (plist-put token :type 'NAME)))))


(defun pyel-lex-number ()
  "Parse a number token from the current point of the lexer.
Numbers start with a digit but may contain a second character
which can be either o (Octal) an x (Hexadecimal) or b (Binary)."
  (flet ((bsnp (start end)
               (buffer-substring-no-properties start end)))
    (let* ((start-pos (pyel-point))
           (base (memq (pyel-char-peek) '(?o ?x ?b)))) ;; XXX Can be removed and put into the parser.
      (while (not (pyel-whitespace-p (pyel-char-after)))
        (pyel-forward-char))
      (list :str (bsnp start-pos (pyel-point))
            :beg start-pos
            :end (pyel-point)
            :type 'NUMBER))))
