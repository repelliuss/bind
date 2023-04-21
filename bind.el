;;; bind.el --- Bind commands to keys -*- lexical-binding: t; -*-

;; Copyright (C) 2023 repelliuss

;; Author: repelliuss <https://github.com/repelliuss>
;; Maintainer: repelliuss <repelliuss@gmail.com>
;; Created: March 26, 2023
;; Modified: March 26, 2023
;; Version: 0.9.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience
;; Homepage: https://github.com/repelliuss/bind

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `bind' many commands to keys in many keymaps, multiple times and
;; support prefix, autoload and repeat-mode.  `bind-undo' can be used
;; to undo key bindings.

;;; Code:

(defgroup bind nil
  "Bind many keys to many keymaps."
  :group 'emacs
  :prefix "bind-"
  :package-version '(Bind . "0.9.0"))

(defcustom bind--metadata nil
  "A plist that carries the info available to upper bind functions to lowers'.
This is so that binding processing functions don't make user
type the same information again.  For example `bind-autoload' can
guess the file function to be autoloaded if not explicitly
given.

This variable will usually be populated lexically, though one can
provide and make use of persistant data."
  :type '(plist))

(defcustom bind--definer #'define-key
  "A function that decides what to do with keymap, key and def.
See `define-key' for what keymap, key and def is.

This is the function called after all of the things are
resolved.  For example it can define the key or unbind it such as
`bind--definer-unbind'.  See `bind--mappings-in-keymap' for where
this is called."
  :type 'function)

(defun bind--definer-unbind (keymap key def)
  "Unbind KEY from KEYMAP and DEF from KEYMAP if DEF is actually a key.
This is to be be used with `bind-undo'."
  (define-key keymap key nil)
  (if (bind-keyp def)
      (define-key keymap def nil)))

(defun bind--mappings-in-keymap (keymap bindings)
  "Actualize defining each key def mapping in BINDINGS to KEYMAP."
  (bind-foreach-key-def bindings
    (lambda (key def)
      (funcall bind--definer
	       keymap
	       (if (stringp key) (kbd key) key)
	       def))))

(defun bind--mappings-foreach-keymap (keymap-s bindings)
  "Define each key def mappings in one or more KEYMAP-S.
This function will be called after each binding processor calls
so BINDINGS need to be flattened."
  (setq bindings (bind-flatten1-key-of-bindings bindings))
  (if (keymapp keymap-s)
      (bind--mappings-in-keymap keymap-s bindings)
    (dolist (keymap keymap-s)
      (bind--mappings-in-keymap keymap bindings))))

(defmacro bind--singular (form)
  "Process a single bind FORM and bind many keys to many keymaps.
FORM's first element can be a keymap, list of keymaps, a function
returning keymap (`setq') or keymaps (a user function).  It is
quoted, if it is a keymap or a list of keymaps.

FORM's rest elements must be bindings.  A binding is in the form
of 'KEY DEF' where KEY and DEF has the same specs as in
`define-key', in the case of `bind'.  It is up to `bind--definer'
what to do with KEY and DEF.

About global and local bindings,

Instead of using different functions for different cases, `bind'
chooses to be verbose about them.  In every case, at the end, each
binding is put in a keymap.  Use `bind-global-map' and
`bind-local-map' functions to get the keymap for the case you
want.  While `bind-global-map' simply
returns `current-global-map', `bind-local-map' implements the
behavior in `local-set-key'.

About processing functions,

Most of the arguments in FORM is evaluated such that `bind'
behaves like a function so they are not quoted and will be
evaluated, unless said so.  That way, bindings can be processed.
For example, there are `bind-prefix', `bind-autoload' and
`bind-repeat' processing functions.  They take bindings as input
and return bindings and possibly transforming bindings but not
required to.  They can be nested and used many times as one
wants.  User can easily define its processing function.  User is
encouraged to make use of `bind-keyp', `bind-foreach-key-def',
`bind-flatten1-key-of-bindings' and `bind-with-metadata' utility
functions for their custom behavior.  See default processing
functions' definitions for examples.

See commentary or homepage for examples."
  `(bind-with-metadata (:main-keymap (bind--main-keymap ',(car form)))
     (bind--mappings-foreach-keymap ,(car form) (list ,@(cdr form)))))

(defmacro bind--multiple (form-prefix forms)
  "Bind multiple `bind' FORMS.
FORM-PREFIX is what each `bind' form is prefixed with.  For
example, its value is `(bind--singular)' when called by `bind'."
  (let (singular-binds)
    (dolist (form forms)
      (setq singular-binds (nconc singular-binds `((,@form-prefix ,form)))))
    (macroexp-progn singular-binds)))

(defmacro bind--main-keymap (bind-first)
  "Extract main keymap from BIND-FIRST argument of `bind' form.
Main keymap is the keymap given to bind form or first of the given keymaps.
Main keymap can be used by binding processor calls.  For example, `bind-repeat'
uses it as a place for putting definitions 'repeat-map prop.

BIND-FIRST is the first element of bind form.  See `bind--singular' for
what a form is."
  `(cond
    ((or (symbolp ,bind-first) (fboundp (car ,bind-first))) ,bind-first)
    (t (car ,bind-first))))		; list of keymaps

(defun bind--singularp (form)
  "T if `bind' FORM doesn't contain multiple `bind' forms."
  (let ((second (cadr form)))
    (or (bind-keyp second)
	(and (symbolp (car second)) (fboundp (car second))))))

(defun bind-keyp (exp)
  "T if EXP is a valid key for `define-key'."
  (or (stringp exp) (vectorp exp)))

(defun bind-foreach-key-def (bindings function)
  "Call FUNCTION for each key def mappings in BINDINGS.
FUNCTION is a function that takes key and def as arguments."
  (declare (indent 1))
  (while bindings
    (funcall function (car bindings) (cadr bindings))
    (setq bindings (cddr bindings))))

(defun bind-flatten1-key-of-bindings (bindings)
  "Flatten each first level key definition in BINDINGS.
A binding processor function will return list of new
bindings.  A function that works on BINDINGS (such as another
processor function) and one that probably uses
`bind-foreach-key-def' expects bindings to be in the form of (KEY
DEF...).  This function can be used to merge list of new bindings
and return the expected form."
  (let (new-bindings)
    (bind-foreach-key-def bindings
      (lambda (key def)
	(if (not (consp key))
	    (setq new-bindings (nconc new-bindings (list key def)))
	  (setq new-bindings (nconc new-bindings key))
	  (if (consp def)
	      (setq new-bindings (nconc new-bindings def))))))
    new-bindings))

(defmacro bind-with-metadata (plist &rest body)
  "Evaluate BODY with PLIST merged with `bind--metadata'."
  (declare (indent 1))
  `(let* ((bind--metadata (append (list ,@plist) bind--metadata)))
     ,@body))

(defun bind-global-map ()
  "Return `current-global-map'."
  (current-global-map))

(defun bind-local-map ()
  "Return local map while replicating the behavior of `local-set-key'."
  (or (current-local-map)
      (let ((local-map (make-sparse-keymap)))
	(use-local-map local-map)
	local-map)))

(defmacro bind (&rest form-or-forms)
  "Bind many keys to many keymaps, multiple times.
Syntax is `(bind FORM)' or `(bind (FORM)...)' so (FORM) is
repeatable.  See `bind--singular' for what a FORM is.
FORM-OR-FORMS can be a single FORM or list of FORMs."
  (if (bind--singularp form-or-forms)
      `(bind--singular ,form-or-forms)
    `(bind--multiple (bind--singular) ,form-or-forms)))

(defmacro bind-undo (&rest form)
  "Undo (or unbind) `bind' FORM keys."
  `(let ((bind--definer #'bind--definer-unbind))
     (bind ,@form)))

(defun bind-prefix (prefix &rest bindings)
  "Prefix each KEY in BINDINGS with PREFIX of KEY is a string.
PREFIX can also be ending with a modifier, such as C-, S- C-S-
etc."
  (declare (indent 1))
  (setq bindings (bind-flatten1-key-of-bindings bindings))
  (let (new-bindings
	(prefix (concat prefix (if (string-match "\\([[:space:]]\\|^\\)\\(.-\\)+$"
						 (car (last (split-string prefix))))
				   ""
				 " "))))
    (bind-foreach-key-def bindings
      (lambda (key def)
	(push def new-bindings)
	(push (if (stringp key)
		  (concat prefix key)
		key)
	      new-bindings)))
    new-bindings))

(defun bind-autoload (&optional file-as-symbol-or-key &rest bindings)
  "If FILE-AS-SYMBOL-OR-KEY if symbol autoload DEF in BINDINGS or use metadata.
Note that `bind' doesn't provide :main-file prop so user must
provide it.  For example, one can utilize its package
configurator."
  (declare (indent 1))
  (let (file)
    (if (symbolp file-as-symbol-or-key)
	(setq file (symbol-name file-as-symbol-or-key))
      (setq file (plist-get bind--metadata :main-file)
	    bindings `(,file-as-symbol-or-key ,@bindings)))
    (if (not file) (error "Bad FILE-AS-SYMBOL-OR-KEY argument to BIND-AUTOLOAD"))
    (setq bindings (bind-flatten1-key-of-bindings bindings))
    (bind-foreach-key-def bindings
      (lambda (_key def)
	(autoload def file nil t))))
  bindings)

(defun bind-repeat (&rest bindings)
  "Add repeating functionality to each DEF in BINDINGS for :main-keymap metadata.
This requires `repeat-mode' to be active to take effect."
  (declare (indent 0))
  (setq bindings (bind-flatten1-key-of-bindings bindings))
  (let ((main-keymap (plist-get bind--metadata :main-keymap)))
    (if (keymapp (symbol-value main-keymap))
	(bind-foreach-key-def bindings
	  (lambda (_key def)
	    (put def 'repeat-map main-keymap)))
      (display-warning 'bind-repeat
		       (format "Couldn't repeat bindings: %s. No main keymap given." bindings))))
  bindings)

(provide 'bind)

;;; bind.el ends here
