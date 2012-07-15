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

;; The majority of this code is shamelessly stolen from Steve Yegge's
;; js2-mode.

(eval-when-compile
  (require 'cl))

(defvar pyel-version 20120715
  "Release number for `pyel'.")

(defvar pyel-dev-mode-p t
  "Non-nil if running in development mode.  Normally nil.")

(defcustom pyel-language-version 27
  "Configures what Python language version to recognize.
Currently versions."
  :type 'integer
  :group 'pyel-mode)

(defvar pyel-verbose-parse-p pyel-dev-mode-p
  "Non-nil to emit status messages during parsing.")

(defvar pyel-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.
This will mostly be useful for interpreters.")

(defvar pyel-statements-per-pause 50
  "Pause after this many statements to check for user input.
If user input is pending, stop the parse and discard the tree.
This makes for a smoother user experience for large files.
You may have to wait a second or two before the highlighting
and error-reporting appear, but you can always type ahead if
you wish.  This appears to be more or less how Eclipse, IntelliJ
and other editors work.")

(defmacro pyel-time (form)
  "Evaluate FORM, discard result, and return elapsed time in sec"
  (declare (debug t))
  (let ((beg (make-symbol "--pyel-time-beg--"))
        (delta (make-symbol "--pyel-time-end--")))
    `(let ((,beg (current-time))
           ,delta)
       ,form
       (/ (truncate (* (- (float-time (current-time))
                          (float-time ,beg)))
                    10000)
          10000.0))))

(defmacro aif (test if-expr &optional else-expr)
  "An anaphoric variant of (if ...). The value of the test
expression is locally bound to 'it' during execution of the
consequent clauses. The binding is present in both consequent
branches."
  (declare (indent 1))
  `(let ((it ,test))
     (if it ,if-expr ,else-expr)))

(defmacro awhen (test if-expr)
  "An anaphoric variant of (when ...). The value of the test
expression is locally bound to 'it' during execution of the
consequent clauses. The binding is present in both consequent
branches."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@if-expr)))

(defmacro awhile (test &rest body)
  "An anaphoric varient of (while ...). The value of the test
expression is locally bound to 'it' during execution of the body
of the loop."
  (declare (indent 1))
  (let ((escape (gensym "awhile-escape-")))
    `(catch ',escape
       (while t
         (let ((it ,test))
           (if it
               (progn ,@body)
             (throw ',escape ())))))))

(defsubst pyel-reset-timer ()
  "Cancel any existing parse timer and schedule a new one."
  (if pyel-parse-timer
      (cancel-timer pyel-parse-timer))
  (setq pyel-parsing nil)
  (setq pyel-parse-timer
        (run-with-idle-timer pyel-idle-timer-delay nil #'pyel-reparse)))

;; Stolen shamelessly from James Clark's nxml-mode.
(defmacro pyel-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without any text property changes modifying the buffer.
Any text properties changes happen as usual but the changes are not treated as
modifications to the buffer."
  (declare (indent 0) (debug t))
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
	   (inhibit-read-only t)
	   (inhibit-modification-hooks t)
	   (buffer-undo-list t)
	   (deactivate-mark nil)
	   ;; Apparently these avoid file locking problems.
	   (buffer-file-name nil)
	   (buffer-file-truename nil))
       (unwind-protect
	   (progn ,@body)
	 (unless ,modified
	   (restore-buffer-modified-p nil))))))

(defmacro pyel-deflocal (name value &optional comment)
  "Define a buffer-local variable NAME with VALUE and COMMENT."
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

;; We record the start and end position of each token.
(pyel-deflocal pyel-token-beg 1)
(pyel-deflocal pyel-token-end -1)

;; `pyel-EOF_CHAR' ; Represents end of stream.  Distinct from pyel-EOF
;; token type.

;; I am not going follow Steve's strategy of using int's, since
;; symbols should be fine.

;; `pyel-ERROR'          ; on error
;; `pyel-EOF'            ; end of file
;; `pyel-EOL'            ; end of line
;; `pyel-COMMENT'        ; comment
;; `pyel-OR'             ; or
;; `pyel-AND'            ; and
;; `pyel-NOT'            ; not
;; `pyel-IS'             ; is
;; `pyel-ISNOT'          ; is not
;; `pyel-EQ'             ; equal
;; `pyel-NEQ'            ; not equal
;; `pyel-LT'             ; less than
;; `pyel-LE'             ; less than or equal to
;; `pyel-GT'             ; greater than
;; `pyel-GE'             ; greater than or equal to
;; `pyel-TRUE'           ; true
;; `pyel-FALSE'          ; false
;; `pyel-NONE'           ; none
;; `pyel-NOTIMPLEMENTED  ; not implemented
;; `pyel-ELLIPSIS'       ; ellipsis
;; `pyel-RETURN'         ; return keyword
;; `pyel-BLOCK'          ; statement block
;; `pyel-FUNCTION'       ; function keyword
;; `pyel-DEBUG'          ; the debug const, assignment is an error


(defconst pyel-tokens
  '(or == and not != = None return def class))

(defconst pyel-token-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in pyel-keywords
          do (puthash
              (symbol-name k)                            ; instanceof
              (intern (concat "pyel-"
                              (upcase (symbol-name k)))) ; pyel-INSTANCEOF
              table))
    table)
  "Python keywords by name, mapped to their symbols.")

(defconst pyel-assignments
  '(= += -= *= /= **= %=))

(defconst pyel-assignment-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in pyel-assignments
          do (puthash
              (symbol-name k)                            ; instanceof
              (intern (concat "pyel-"
                              (upcase (symbol-name k)))) ; pyel-INSTANCEOF
              table))
    table)
  "Python assignment operators by name, mapped to their symbols.")

(defsubst pyel-token-code (sym)
  "Return code for token symbol SYM, e.g. 86 for 'pyel-LP."
  (or (gethash sym pyel-token-names)
      (error "Invalid token symbol: %s " sym)))  ; signal code bug

(pyel-deflocal pyel-ast nil "Private variable.")
(pyel-deflocal pyel-parse-timer nil "Private variable.")
(pyel-deflocal pyel-buffer-dirty-p nil "Private variable.")
(pyel-deflocal pyel-parsing nil "Private variable.")
(pyel-deflocal pyel-parse-stmt-count 0)

(pyel-deflocal pyel-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(pyel-deflocal pyel-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

(pyel-deflocal pyel-ts-hit-eof nil
  "Token stream buffer-local variable.")

(pyel-deflocal pyel-ts-line-start 0
  "Token stream buffer-local variable.")

(pyel-deflocal pyel-ts-line-end-char -1
  "Token stream buffer-local variable.")

;;; Parser instance variables (buffer-local vars for pyel-parse)

(defconst pyel-clear-ti-mask #xFFFF
  "Mask to clear token information bits.")

(defconst pyel-ti-after-eol (lsh 1 16)
  "Flag:  first token of the source line.")

(defconst pyel-ti-check-label (lsh 1 17)
  "Flag:  indicates to check for label.")


(pyel-deflocal pyel-current-flagged-token 'pyel-EOF)
(pyel-deflocal pyel-current-token 'pyel-EOF)

(pyel-deflocal pyel-nesting-of-function 0)

(pyel-deflocal pyel-recorded-assignments nil
  "Tracks assignments found during parsing.")

(pyel-deflocal pyel-current-script-or-fn nil)
(pyel-deflocal pyel-current-scope nil)
(pyel-deflocal pyel-nesting-of-with 0)
(pyel-deflocal pyel-label-set nil
  "An alist mapping label names to nodes.")

(defvar *pyel-lex* nil "The current lexer.")

(defstruct (pyel-lexer
            (:constructor nil)
            (:constructor make-pyel-lexer (&key string
                                                tab-width
                                                (curr-line 1)
                                                indent-stack
                                                decorators)))
  "The lexer state object."
  string
  tab-width
  curr-line
  indent-stack
  decorators)                     ; a lisp list of the child statement nodes


(defstruct (pyel-node
            (:constructor nil))  ; abstract
  "Base AST node type."
  (type -1)  ; token type
  (pos -1)   ; start position of this AST node in parsed input
  (len 1)    ; num characters spanned by the node
  props      ; optional node property list (an alist)
  parent)    ; link to parent node; null for root

(defsubst pyel-node-add-children (parent &rest nodes)
  "Set parent node of NODES to PARENT, and return PARENT.
Does nothing if we're not recording parent links.
If any given node in NODES is nil, doesn't record that link."
  (pyel-fixup-starts parent nodes)
  (dolist (node nodes)
    (and node
         (setf (pyel-node-parent node) parent))))



;; It's important to make sure block nodes have a lisp list for the
;; child nodes, to limit printing recursion depth in an AST that
;; otherwise consists of defstruct vectors.  Emacs will crash printing
;; a sufficiently large vector tree.

(defstruct (pyel-block-node
            (:include pyel-node)
            (:constructor nil)
            (:constructor make-pyel-block-node (&key (type 'pyel-BLOCK)
                                                     (pos pyel-token-beg)
                                                     len
                                                     props
                                                     kids)))
  "A block of statements."
  kids)                     ; a lisp list of the child statement nodes


(defstruct (pyel-scope
            (:include pyel-block-node)
            (:constructor nil)
            (:constructor make-pyel-scope (&key (type 'pyel-BLOCK)
                                                (pos pyel-token-beg)
                                                len
                                                kids)))
  ;; The symbol-table is a LinkedHashMap<String,Symbol> in Rhino.
  ;; I don't have one of those handy, so I'll use an alist for now.
  ;; It's as fast as an emacs hashtable for up to about 50 elements,
  ;; and is much lighter-weight to construct (both CPU and mem).
  ;; The keys are interned strings (symbols) for faster lookup.
  ;; Should switch to hybrid alist/hashtable eventually.
  symbol-table              ; an alist of (symbol . pyel-symbol)
  parent-scope              ; a `pyel-scope'
  top)                      ; top-level `pyel-scope' (script/function)


(defstruct (pyel-symbol
            (:constructor nil)
            (:constructor make-pyel-symbol (decl-type name &optional ast-node)))
  "A symbol table entry."
  ;; One of pyel-FUNCTION, pyel-LP (for parameters), pyel-VAR,
  ;; pyel-LET, or pyel-CONST
  decl-type
  name                                  ; string
  ast-node)                             ; a `pyel-node'

(defstruct (pyel-error-node
            (:include pyel-node)
            (:constructor nil)         ; silence emacs21 byte-compiler
            (:constructor make-pyel-error-node (&key (type 'pyel-ERROR)
                                                     (pos pyel-token-beg)
                                                     len)))
  "AST node representing a parse error.")


(defstruct (pyel-script-node
            (:include pyel-scope)
            (:constructor nil)
            (:constructor make-pyel-script-node (&key (type 'pyel-SCRIPT)
                                                      (pos pyel-token-beg)
                                                      len
                                                      var-decls
                                                      fun-decls)))
  functions                   ; lisp list of nested functions
  regexps                     ; lisp list of (string . flags)
  symbols                     ; alist (every symbol gets unique index)
  (param-count 0)
  var-names                           ; vector of string names
  consts                              ; bool-vector matching var-decls
  (temp-number 0))                    ; for generating temp variables

(defstruct (pyel-ast-root
            (:include pyel-script-node)
            (:constructor nil)
            (:constructor make-pyel-ast-root (&key (type 'pyel-SCRIPT)
                                                   (pos pyel-token-beg)
                                                   len
                                                   buffer)))
  "The root node of a pyel AST."
  buffer          ; the source buffer from which the code was parsed
  comments        ; a lisp list of comments, ordered by start position
  errors          ; a lisp list of errors found during parsing
  warnings        ; a lisp list of warnings found during parsing
  node-count)     ; number of nodes in the tree, including the root

(defstruct (pyel-var-decl-node
            (:include pyel-node)
            (:constructor nil)
            (:constructor make-pyel-var-decl-node (&key (type pyel-VAR)
                                                       (pos pyel-token-beg)
                                                       len
                                                       kids
                                                       decl-type)))
  "AST node for a variable declaration list (VAR, CONST or LET).
The node bounds differ depending on the declaration type.  For VAR or
CONST declarations, the bounds include the var/const keyword.  For LET
declarations, the node begins at the position of the first child."
  kids        ; a lisp list of `pyel-var-init-node' structs.
  decl-type)  ; pyel-VAR, pyel-CONST or pyel-LET

(defstruct (pyel-infix-node
            (:include pyel-node)
            (:constructor nil)
            (:constructor make-pyel-infix-node (&key type
                                                    (pos pyel-ts-cursor)
                                                    len
                                                    op-pos
                                                    left
                                                    right)))
  "Represents infix expressions.
Includes assignment ops like `|=', and the comma operator.
The type field inherited from `pyel-node' holds the operator."
  op-pos    ; buffer position where operator begins
  left      ; any `pyel-node'
  right)    ; any `pyel-node'

(defstruct (pyel-function-node
            (:include pyel-script-node)
            (:constructor nil)
            (:constructor make-pyel-function-node (&key (type pyel-FUNCTION)
                                                       (pos pyel-ts-cursor)
                                                       len
                                                       (ftype 'FUNCTION)
                                                       (form 'FUNCTION_STATEMENT)
                                                       (name "")
                                                       params
                                                       body
                                                       lp
                                                       rp)))
  "AST node for a function declaration.
The `params' field is a lisp list of nodes.  Each node is either a simple
`pyel-name-node', or if it's a destructuring-assignment parameter, a
`pyel-array-node' or `pyel-object-node'."
  ftype            ; FUNCTION, GETTER or SETTER
  form             ; FUNCTION_{STATEMENT|EXPRESSION|EXPRESSION_STATEMENT}
  name             ; function name (a `pyel-name-node', or nil if anonymous)
  params           ; a lisp list of destructuring forms or simple name nodes
  body             ; a `pyel-block-node' or expression node (1.8 only)
  lp               ; position of arg-list open-paren, or nil if omitted
  rp               ; position of arg-list close-paren, or nil if omitted
  ignore-dynamic   ; ignore value of the dynamic-scope flag (interpreter only)
  needs-activation ; t if we need an activation object for this frame
  is-generator     ; t if this function contains a yield
  member-expr)     ; nonstandard Ecma extension from Rhino


(defsubst pyel-node-set-child-list (node kids)
  "Set the child list for NODE to KIDS."
   (cond
    ((pyel-function-node-p node)
     (setf (pyel-function-node-params node) kids))
    ((pyel-block-node-p node)
     (setf (pyel-block-node-kids node) kids))
    ((pyel-var-decl-node-p node)
     (setf (pyel-var-decl-node-kids node) kids))
    (t
     (error "Unsupported node type: %s" (pyel-node-short-name node))))
   kids)


(defsubst pyel-get-string-from-buffer ()
  "Reverse the char accumulator and return it as a string."
  (setq pyel-token-end pyel-ts-cursor)
  (if pyel-ts-string-buffer
      (apply #'string (nreverse pyel-ts-string-buffer))
    ""))

(defsubst pyel-add-to-string (c)
  (push c pyel-ts-string-buffer))

;; Note that when we "read" the end-of-file, we advance pyel-ts-cursor
;; to (1+ (point-max)), which lets the scanner treat end-of-file like
;; any other character:  when it's not part of the current token, we
;; unget it, allowing it to be read again by the following call.
(defsubst pyel-unget-char ()
  (decf pyel-ts-cursor))

(defsubst pyel-get-char ()
  "Read and return the next character from the input buffer.
Increments `pyel-ts-lineno' if the return value is a newline char.
Updates `pyel-ts-cursor' to the point after the returned char.
Returns `pyel-EOF_CHAR' if we hit the end of the buffer.
Also updates `pyel-ts-hit-eof' and `pyel-ts-line-start' as needed."
  (if (>= pyel-ts-cursor (point-max))
      ;; if we are at the end of the file then return
      (progn
        (setq pyel-ts-hit-eof t
              pyel-ts-cursor (1+ pyel-ts-cursor))
        'pyel-EOF_CHAR)
    ;; read a character and update the state if we changed to a new
    ;; line.
    (let ((c (char-before (incf pyel-ts-cursor))))
      (if (= c ?\n)
          (setq pyel-ts-line-start pyel-ts-cursor
                pyel-ts-lineno (1+ pyel-ts-lineno))))
    c))

(defsubst pyel-peek-char ()
  (prog1
      (pyel-get-char)
    (pyel-unget-char)))

(defsubst pyel-char-uppercase-p (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (downcase c)))

(defsubst pyel-char-lowercase-p (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (upcase c)))

(defsubst pyel-identifier-part-p (c)
  "Check if the character is a valid part of an identifier."
  ;; TODO:  make me Unicode-friendly.  See comments above.
  (or
   (eq c '(?@ ?_))
   (pyel-char-uppercase-p c)
   (pyel-char-lowercase-p c)
   (and (>= c ?0) (<= c ?9))))


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
              (symbol-name k)                            ; instanceof
              (intern (concat "pyel-"
                              (upcase (symbol-name k)))) ; pyel-INSTANCEOF
              table))
    table)
  "Python keywords by name, mapped to their symbols.")

(defconst pyel-reserved-words
  '(Ellipsis
    False None NotImplemented True
    __debug__ copyright credits
    exit license quit))

(defconst pyel-reserved-word-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in pyel-reserved-words
          do
          (puthash (symbol-name k) 'pyel-RESERVED table))
    table)
  "JavaScript reserved words by name, mapped to 'pyel-RESERVED.")

(defun pyel-string-to-keyword (s)
  "Return token for S, a string, if S is a keyword or reserved word.
Returns a symbol such as 'pyel-BREAK, or nil if not keyword/reserved."
  (or (gethash s pyel-keyword-names)
      (gethash s pyel-reserved-word-names)))
(defsubst pyel-block-node-push (n kid)
  "Push pyel-node KID onto the end of pyel-block-node N's child list.
KID is always added to the -end- of the kids list.
Function also calls `pyel-node-add-children' to add the parent link."
  (let ((kids (pyel-node-child-list n)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (pyel-node-set-child-list n (list kid)))
    (pyel-node-add-children n kid)))

(defun pyel-get-token ()
  "Return next Python token, an int such as pyel-RETURN.
Also return the start and end position of the token."
  (let ((c (char-after (point))))
    (catch 'return
      (cond
       ((eq c ?#)
        ;; it's a comment
        )
       (t
        (let ((token (catch 'break
                       (let ((start-pos (point)))
                         (while t
                           (if (pyel-identifier-part-p (char-after (point)))
                               (forward-char)
                             (throw 'break (list :str (buffer-substring start-pos (point))
                                                 :beg start-pos
                                                 :end (point)))))))))
          (awhen (pyel-string-to-keyword (plist-get token :str))
            (when (eq it 'pyel-RESERVED)
              (throw 'return (plist-put token :token (pyel-token-code it)))))
          (throw 'return (plist-put token :token 'pyel-NAME))))))))

(defun pyel-parse-expr ()
  (let* ((pn (pyel-parse-assign-expr))
         (pos (pyel-node-pos pn))
         left
         right
         op-pos)
    (while (pyel-match-token 'pyel-COMMA)
      (setq op-pos (- pyel-token-beg pos)) ; relative
      (setq right (pyel-parse-assign-expr)
            left pn
            pn (make-pyel-infix-node :type 'pyel-COMMA
                                     :pos pos
                                     :len (- pyel-ts-cursor pos)
                                     :op-pos op-pos
                                     :left left
                                     :right right))
      (pyel-node-add-children pn left right))
    pn))

(defun pyel-parse-assign-expr ()
  (let* ((token (pyel-peek-token))
        (tt (plist-get token :token))
        (pos pyel-token-beg)
        pn
        left
        right
        op-pos)
    (if (eq tt 'pyel-YIELD)
        (pyel-parse-return-or-yield tt t)
      ;; not yield - parse assignment expression
      (setq pn (pyel-parse-cond-expr)
            tt (pyel-peek-token))
      (when (gethash tt pyel-assignment)
        (pyel-consume-token)
        (setq op-pos (- pyel-token-beg pos)  ; relative
              left pn
              right (pyel-parse-assign-expr)
              pn (make-pyel-assign-node :type tt
                                       :pos pos
                                       :len (- (pyel-node-end right) pos)
                                       :op-pos op-pos
                                       :left left
                                       :right right))
        (when pyel-parse-ide-mode
          (pyel-highlight-assign-targets pn left right)
          (if (or (pyel-function-node-p right)
                  (pyel-object-node-p right))
              (pyel-record-imenu-functions right left)))
        ;; do this last so ide checks above can use absolute positions
        (pyel-node-add-children pn left right))
      pn)))

(defsubst pyel-wrap-with-expr-stmt (pos expr &optional add-child)
  (let ((pn (make-pyel-expr-stmt-node :pos pos
                                     :len (pyel-node-len expr)
                                     :type 'pyel-EXPR_VOID
                                     :expr expr)))
    (if add-child
        (pyel-node-add-children pn expr))
    pn))


(defun pyel-parse-name ()
  "Parser for identifier or label.  Last token matched must be pyel-NAME.
Called when we found a name in a statement context.  If it's a label, we gather
up any following labels and the next non-label statement into a
`pyel-labeled-stmt-node' bundle and return that.  Otherwise we parse an
expression and return it wrapped in a `pyel-expr-stmt-node'."
  (let ((pos pyel-token-beg)
        (end pyel-token-end)
        expr
        stmt
        pn
        bundle
        (continue t))
    ;; set check for label and call down to `pyel-parse-primary-expr'
    (setq expr (pyel-parse-expr))
    (setq pn (pyel-wrap-with-expr-stmt pos expr t))))

(defconst pyel-parsers
  (let ((parsers (make-hash-table))
        (tokens
         (list
          (cons 'pyel-EXPR #'pyel-parse-expr-stmt)
          (cons 'pyel-NAME #'pyel-parse-name)
          )))
    (loop for (k . v) in tokens do
          (puthash k v parsers))
    parsers)
  "A vector mapping token types to parser functions.")


(defun pyel-statement-helper (first-token)
  (let* ((tt (plist-get first-token :token))
         (first-tt first-token)
         (beg pyel-token-beg)
         (parser (if (eq tt 'pyel-ERROR)
                     #'pyel-parse-semi
                   (gethash tt pyel-parsers)))
         pn
         tt-flagged)
    ;; If the statement is set, then it's been told its label by now.
    (and pyel-labeled-stmt
         (pyel-labeled-stmt-node-stmt pyel-labeled-stmt)
         (setq pyel-labeled-stmt nil))
    (setq pn (funcall parser))
    ;; Don't do auto semi insertion for certain statement types.
    (unless (or (memq first-tt pyel-no-semi-insertion)
                (pyel-labeled-stmt-node-p pn))
      (pyel-auto-insert-semicolon pn))
    pn))


(defun pyel-parse-statement (first-token)
  (let (pn beg end)
    ;; coarse-grained user-interrupt check - needs work
    (and pyel-parse-interruptable-p
         (zerop (% (incf pyel-parse-stmt-count)
                   pyel-statements-per-pause))
         (input-pending-p)
         (throw 'interrupted t))
    (setq pn (pyel-statement-helper first-token))
    ;; no-side-effects warning check
    (unless (pyel-node-has-side-effects pn)
      (setq end (pyel-node-end pn))
      (save-excursion
        (goto-char end)
        (setq beg (max (pyel-node-pos pn) (point-at-bol)))))
    pn))


(defun pyel-parse (&optional buf cb)
  (let (ast
        (pyel-lex (make-pyel-lexer)))
    (or buf (setq buf (current-buffer)))
    (message nil)  ; clear any error message from previous parse
    (save-excursion
      (set-buffer buf)
      (setq ast (pyel-with-unmodifying-text-property-changes
                  (pyel-do-parse))))))


(defsubst pyel-peek-token (&optional ignore-eol)
  "Returns the next token without consuming it.
If previous token was consumed, calls scanner to get new token.
If previous token was -not- consumed, returns it (idempotent).

This function will not return a newline (pyel-EOL) - instead, it
gobbles newlines until it finds a non-newline token, and flags
that token as appearing just after a newline.

This function will also not return a pyel-COMMENT.  Instead, it
records comments found in `pyel-scanned-comments'.  If the token
returned by this function immediately follows a jsdoc comment,
the token is flagged as such.

Note that this function always returned the un-flagged token!
The flags, if any, are saved in `pyel-current-flagged-token'."
  (if (not (eq pyel-current-flagged-token 'pyel-EOF)) ; last token not consumed
      pyel-current-token ; most common case - return already-peeked token
    (let* ((token (pyel-get-token))     ; call scanner
           (tt (plist-get token :token)))
      ;; eat comments
      (while (eq tt 'pyel-COMMENT)
        (setq token (pyel-get-token))
        (setq tt (plist-get token :token)))
      token)))

(defun pyel-do-parse ()
  "Parse current buffer starting from current point.
Scanner should be initialized."
  (let ((pos pyel-ts-cursor)
        (end pyel-ts-cursor)  ; in case file is empty
        root n tt)
    (setf root (make-pyel-ast-root :buffer (buffer-name) :pos pos)
          pyel-current-script-or-fn root
          pyel-current-scope root
          pyel-current-flagged-token 'pyel-EOF
          pyel-nesting-of-function 0
          pyel-labeled-stmt nil
          pyel-recorded-assignments nil)
    (while (progn
             (setq tt (pyel-peek-token))
             (not (eq (plist-get tt :token) 'pyel-EOF)))
      (if (eq (plist-get tt :token) 'pyel-FUNCTION)
          (progn
            (pyel-consume-token)
            (setq n (pyel-parse-function 'FUNCTION_STATEMENT)))
        ;; not a function - parse a statement
        (setq n (pyel-parse-statement tt)))
      (pyel-block-node-push root n))))

(defun pyel-reparse (&optional force)
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it.  If FORCE is nil, then the
buffer will only rebuild its `pyel-ast' if the buffer is dirty."
  (let (time
        interrupted-p
        (pyel-compiler-strict-mode pyel-mode-show-strict-warnings))
    (unless pyel-parsing
      (setq pyel-parsing t)
      (unwind-protect
          (when (or pyel-buffer-dirty-p force)
            (pyel-remove-overlays)
            (pyel-with-unmodifying-text-property-changes
              (remove-text-properties (point-min) (point-max) '(syntax-table))
              (setq pyel-buffer-dirty-p nil)
              (if pyel-verbose-parse-p
                  (message "parsing..."))
              (setq time
                    (pyel-time
                     (setq interrupted-p
                           (catch 'interrupted
                             (setq pyel-mode-ast (pyel-parse))))))
              (if interrupted-p
                  (progn
                    ;; unfinished parse => try again
                    (setq pyel-buffer-dirty-p t)
                    (pyel-mode-reset-timer))
                (if pyel-mode-verbose-parse-p
                    (message "Parse time: %s" time)))))
        (setq pyel-parsing nil)
        (unless interrupted-p
          (setq pyel-mode-parse-timer nil))))))

(defun pyel-remove-overlays ()
  "Remove overlays from buffer that have a `pyel-error' property."
  (let ((beg (point-min))
        (end (point-max)))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (overlay-get o 'pyel-error)
          (delete-overlay o))))))

(provide 'pyel)
