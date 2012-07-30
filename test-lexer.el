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


(ert-deftest pyel-lex-name-test ()
  (with-temp-buffer
    (insert "temp = bananan\n")
    (goto-char (point-min))
    (let ((*pyel-lex* (make-pyel-lexer)))
      (should (equal (pyel-lex-name)
                     (list :str "temp" :beg 1 :end 5 :type 'NAME))))))

(ert-deftest pyel-lex-name-test1 ()
  (with-temp-buffer
    (insert "def test():\n    pass\n")
    (goto-char (point-min))
    (let ((*pyel-lex* (make-pyel-lexer)))
      (should (equal (pyel-lex-name)
                     (list :str "def" :beg 1 :end 4 :type 'NAME))))))

(ert-deftest pyel-lex-number-test ()
  (with-temp-buffer
    (insert "192912\n")
    (goto-char (point-min))
    (let ((*pyel-lex* (make-pyel-lexer)))
      (should (equal (pyel-lex-number)
                     (list :str "192912" :beg 1 :end 7 :type 'NUMBER))))))

(ert-deftest pyel-lex-number-hex ()
  (with-temp-buffer
    (insert "0x1234\n")
    (goto-char (point-min))
    (let ((*pyel-lex* (make-pyel-lexer)))
      (should (equal (pyel-lex-number)
                     (list :str "0x1234" :beg 1 :end 7 :type 'NUMBER))))))

(ert-deftest pyel-lex-number-oct ()
  (with-temp-buffer
    (insert "0o4321\n")
    (goto-char (point-min))
    (let ((*pyel-lex* (make-pyel-lexer)))
      (should (equal (pyel-lex-number)
                     (list :str "0o4321" :beg 1 :end 7 :type 'NUMBER))))))

(ert-deftest pyel-lex-number-bin ()
  (with-temp-buffer
    (insert "0b10101\n")
    (goto-char (point-min))
    (let ((*pyel-lex* (make-pyel-lexer)))
      (should (equal (pyel-lex-number)
                     (list :str "0b10101" :beg 1 :end 8 :type 'NUMBER))))))
