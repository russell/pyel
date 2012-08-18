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


(defun pyel-plist-equal (p1 p2)
  (catch 'break
    (let ((keys1 (sort (loop for i in p1 by #'cddr collect i) #'string-lessp))
          (keys2 (sort (loop for i in p2 by #'cddr collect i) #'string-lessp)))
      (unless (equal keys1 keys2) (throw 'break nil))
      (loop for k in keys1
            do (unless (equal (plist-get p1 k) (plist-get p2 k)) (throw 'break nil))))
    t))

(defun pyel-should-equal-alist (a b)
  (should (pyel-plist-equal a b)))

(defmacro pyel-simple-test-case (name doc test-data test)
  (declare (indent 2))
  `(ert-deftest ,name ()
     ,doc
     (with-temp-buffer
       (insert ,test-data)
       (goto-char (point-min))
       (let ((*pyel-lex* (make-pyel-lexer)))
         (should ,test)))))

(pyel-simple-test-case pyel-lex-name-test
    "Test that a simple name can be parsed."
  "temp = bananan\n"
  (equal (pyel-lex-name)
         (list :value "temp" :beg 1 :len 4 :type 'NAME)))

(ert-deftest pyel-lex-name-test-separator ()
  "Test all the possible operator separators."
  (dolist (separator
           (list  "(" ")" "[" "]" "{" "}" "@" "," ":" "." "`" "=" ";" "+=" "-=" "*=" "/=" "//=" "%=" "&=" "|=" "^=" ">>=" "<<=" "**=" "'" "\"" "#" "\\" "\$" "\?"))
    (with-temp-buffer
      (insert "test123e")
      (insert separator)
      (goto-char (point-min))
      (let ((*pyel-lex* (make-pyel-lexer)))
        (should
         (equal (pyel-lex-name)
                (list :value "test123e" :beg 1 :len 8 :type 'NAME)))))))


(pyel-simple-test-case pyel-lex-name-test1
    "Check for parsing of special types."
  "def test():\n    pass\n"
  (should (equal (pyel-lex-name)
                 (list :value "def" :beg 1 :len 3 :type 'DEF))))

(pyel-simple-test-case pyel-lex-number-test
    "Check that basic number parsing works."
  "192912\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value 192912 :beg 1 :len 6 :base 10 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-invalid-number-test
    "Check that separator is honoured."
  "192912[\"banana\"]\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value 192912 :beg 1 :len 6 :base 10 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-number-hex
    "Test parsing a hex number."
  "0x1234\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value 4660 :beg 1 :len 6 :base 16 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-number-oct
    "Test parsing a octal number."
  "0o4321\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value 2257 :beg 1 :len 6 :base 8 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-number-bin
    "Test parsing a binary number."
  "0b10101\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value 21 :beg 1 :len 7 :base 2 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-number-exponant
    "Test parsing an exponential number."
  "1.0000050000069649e-05\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value 1.0000050000069649e-05 :beg 1 :len 22 :base 10 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-number-invalid-base
    "Test parsing and invalid based number."
  "1f1000\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value "1f1000" :error "Unsupported base: f." :beg 1 :len 6 :base 10 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-number-long-test
    "Test parsing a long integer."
  "12381L\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value "12381L" :beg 1 :len 6 :base 10 :type 'NUMBER)))

(pyel-simple-test-case pyel-lex-complex-number-test
    "Test parsing a complex number."
  "12381j\n"
  (pyel-should-equal-alist
   (pyel-lex-number)
   (list :value "12381j" :beg 1 :len 6 :base 10 :type 'NUMBER)))
