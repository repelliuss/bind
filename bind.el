;;; bind.el --- Bind commands to keys -*- lexical-binding: t; -*-

;; Copyright (C) 2023 repelliuss

;; Author: repelliuss <https://github.com/repelliuss>
;; Maintainer: repelliuss <repelliuss@gmail.com>
;; Created: March 26, 2023
;; Modified: March 26, 2023
;; Version: 0.9.1
;; Package-Requires: ((emacs "25.1"))

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
  :package-version '(Bind . "0.9.1"))

(defcustom bind-finalizers '(bind-wk-finalizer)
  "List of finalizer functions to call before binding.
Each function is called with the bundle as argument."
  :type '(repeat function)
  :group 'bind)

(defvar bind--metadata nil
  "A plist that carries the info available to upper bind functions to lowers'.
This is so that binding processing functions don\\='t make user
type the same information again.  For example `bind-autoload' can
guess the file function to be autoloaded if not explicitly
given.

This variable will usually be populated lexically, though one can
provide and make use of persistant data.")

(defvar bind--definer #'define-key
  "A function that decides what to do with keymap, key and def.
See `define-key' for what keymap, key and def is.

This is the function called after all of the things are resolved.
For example it can define the key or unbind it such as
`bind--definer-unbind'.  See `bind--mappings-in-keymap' for where
this is called.")

(defvar bind--savings nil
  "A lexical variable used by `bind-save' which stores a valid `bind' FORM.")

(defun bind--definer-unbind (keymap key def)
  "Unbind KEY from KEYMAP and DEF from KEYMAP if DEF is actually a key.
This is to be be used with `bind-undo'."
  (define-key keymap key nil)
  (if (bind-keyp def)
      (define-key keymap def nil)))

(defun bind--definer-save (keymap key _def)
  "Store existing definition of KEY in KEYMAP.
Value is stored in `bind--savings' such a way that `bind''s form is kept
as much as possible."
  (when-let ((keymap-name (bind--find-keymap-name keymap)))
    (let ((keymap-savings (alist-get keymap-name bind--savings))
	      (cur-def (lookup-key keymap key)))
      (unless keymap-savings
	    (push (list keymap-name) bind--savings))
      (setf (cdr (assoc keymap-name bind--savings))
	        (nconc keymap-savings (list key
					                    (cond
					                     ((keymapp cur-def) (bind--find-keymap-name cur-def))
					                     (t `(quote ,cur-def)))))))))

(defun bind--find-keymap-name (keymap)
  "Find the name of the variable with value KEYMAP.
Return nil if KEYMAP is not a valid keymap, or if there is no
variable with value KEYMAP.

This function is a copy of `help-fns-find-keymap-name'."
  (when (keymapp keymap)
    (let ((name (catch 'found-keymap
                  (mapatoms (lambda (symb)
                              (and (boundp symb)
                                   (eq (symbol-value symb) keymap)
                                   (not (eq symb 'keymap))
                                   (throw 'found-keymap symb))))
                  nil)))
      ;; Follow aliasing.
      (or (ignore-errors (indirect-variable name)) name))))

(defun bind--mappings-in-keymap (keymap bundle)
  "Actualize defining each key def mapping in BUNDLE to KEYMAP.
BUNDLE must be a bind bundle: (PRIVATE . ((PRIV . (KEY . DEF)) ...))."
  (bind-foreach bundle
    (lambda (key def _private)
      (funcall bind--definer
	           keymap
	           (if (stringp key) (kbd key) key)
	           def))))

(defun bind--mappings-foreach-keymap (keymap-s bundle)
  "Define each key def mappings in one or more KEYMAP-S.
BUNDLE is a bind bundle.  Calls finalizers before binding."
  ;; Call finalizers first
  (dolist (finalizer bind-finalizers)
    (funcall finalizer bundle))
  ;; Then do the actual binding
  (if (keymapp keymap-s)
      (bind--mappings-in-keymap keymap-s bundle)
    (dolist (keymap keymap-s)
      (bind--mappings-in-keymap keymap bundle))))

(defun bind--synonymp (keyword)
  "Check if KEYWORD is a symbol when bind package prefix is prepended."
  (when (keywordp keyword)
    (or (intern-soft (concat "bind--" (substring (symbol-name keyword) 1)))
	    (intern-soft (concat "bind-" (substring (symbol-name keyword) 1))))))

(defun bind--expand-synonyms (sexp)
  "Expand keywords at head of lists in SEXP that ends with bind function names.
This function lets you call bind functions without bind- package
prefix but as keywords such that a keyword, `:example', will be
replaced by `bind-example' if bind-example is `fboundp' and if
the keyword is the first element of a list in `bind'
FORM.  Basically, places where you would write your function
names."
  (let ((expansion sexp))
    (while sexp
      (when (consp (car sexp))
	    (if-let ((sym (bind--synonymp (caar sexp))))
	        (setcar (car sexp) sym))
	    (setcar sexp (bind--expand-synonyms (car sexp))))
      (setq sexp (cdr sexp)))
    expansion))

(defmacro bind--singular (form)
  "Process a single bind FORM and bind many keys to many keymaps.
FORM\\='s first element can be a keymap, list of keymaps, a function
returning keymap (`setq') or keymaps (a user function).  It is
quoted, if it is a keymap or a list of keymaps.

FORM\\='s rest elements must be bindings.  A binding is in the form
of \\='KEY DEF\\=' where KEY and DEF has the same specs as in
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
and return a bundle.  They can be nested and used many times as
one wants.  User can easily define its processing function.
User is encouraged to make use of `bind-keyp', `bind-foreach',
`bind-make-bundle', `bind-make-binding' and `bind-with-metadata'
utility functions for their custom behavior.  See default
processing functions\\=' definitions for examples.

About synonyms,

Instead of typing out fully typing omit function names, you can
remove the bind- or bind-- prefix and type remaining part as
keyword.  For example, you can write :prefix, :autoload, :repeat,
:global-map, :local-map, :metadata instead of `bind-prefix',
`bind-autoload', `bind-repeat', `bind-global-map', `bind-local-map',
`bind--metadata', respectively.  You can use this for user defined
functions too if they are also prefixed with bind- or bind--
package prefix.  If indentation is not good for your the keyword,
you can `(put :KEYWORD \\='lisp-indent-function INDENT)' where
`KEYWORD' is the string after bind- prefix and INDENT is the same
as in `(declare (indent INDENT))'.

See commentary or homepage for examples."
  (let ((first (car form)))
    `(bind-with-metadata (:keymaps (bind-ensure-list ,first))
       (bind--mappings-foreach-keymap (plist-get bind--metadata :keymaps)
				                      (bind--bundle-from-args ,@(cdr form))))))

(defmacro bind--multiple (form-prefix forms)
  "Bind multiple `bind' FORMS.
FORM-PREFIX is what each `bind' form is prefixed with.  For
example, its value is `(bind--singular)' when called by `bind'."
  (let (singular-binds)
    (dolist (form forms)
      (setq singular-binds (nconc singular-binds `((,@form-prefix ,form)))))
    (macroexp-progn singular-binds)))

(defun bind--singularp (form)
  "T if `bind' FORM doesn\\='t contain multiple `bind' forms."
  (let ((second (cadr form)))
    (or (bind-keyp second)
	    (and (symbolp (car second)) (fboundp (or (bind--synonymp (car second)) (car second)))))))

(defun bind--map-insertable-formp (form)
  "Return action for singular BIND FORM if a map can be insertable.
This function is only used by extensions for package configurator
support.

# in code comments show where the map will be inserted."
  (cond
   ((bind-keyp (car form)) 'yes)	; (bind # ...)
   ((symbolp (car form)) 'no)		; (bind map ...)
   ((memq (caar form) '(:global-map bind-global-map :local-map bind-local-map)) 'no)
   ((or (eq 'quote (caar form))
	    (not (symbolp (caar form)))) (error (concat "Bad FORM given to USE-PACKAGE :BIND. If (car FORM) "
						                            "neither key or symbol, then (caar FORM) must "
						                            "be equivalent to (SYMBOL ...).")))
   ((or (bind--synonymp (caar form)) (string-prefix-p "bind-" (symbol-name (caar form)))) 'yes) ; (bind # (bind-* ...) ...)
   ((not (fboundp (caar form))) 'yes-merge) ; (bind (# map) ...)
   (t 'no)))				; (bind (function ...) ...)

(defun bind-keyp (exp)
  "T if EXP is a valid key for `define-key'."
  (or (stringp exp) (vectorp exp)))

(defun bind-ensure-list (keymap-or-list)
  "Ensure KEYMAP-OR-LIST is a list of keymaps.
Unlike `ensure-list', this properly handles keymaps which are themselves lists."
  (if (keymapp keymap-or-list)
      (list keymap-or-list)
    (ensure-list keymap-or-list)))

;;; Bind bundle structure accessors/constructors
;; Bundle: (PRIVATE-DATA . BINDINGS-LIST)
;; Each binding: (BINDING-PRIVATE . (KEY . DEF))

(defun bind-make-bundle (bindings-list &optional private)
  "Create bind bundle with PRIVATE data and BINDINGS-LIST."
  (cons private bindings-list))

(defun bind-bundle-private (bundle)
  "Get private data from bind BUNDLE."
  (car bundle))

(defun bind-bundle-bindings (bundle)
  "Get bindings list from bind BUNDLE."
  (cdr bundle))

(defun bind-make-binding (key def &optional private)
  "Create a binding with KEY, DEF, and optional PRIVATE data."
  (cons private (cons key def)))

(defun bind-binding-private (binding)
  "Get private data from BINDING."
  (car binding))

(defun bind-binding-key (binding)
  "Get key from BINDING."
  (cadr binding))

(defun bind-binding-def (binding)
  "Get def from BINDING."
  (cddr binding))

(defun bind--bundle-from-args (&rest args)
  "Convert ARGS in form (KEY DEF KEY DEF ... or BUNDLE ...) to a bind bundle.
When a bundle appears in key position, its bindings and private data are merged."
  (let (bindings
        private
        (next-keyp t)
        cur-key)
    (while args
      (let ((cur (car args)))
        (if next-keyp
            (if (and (consp cur) (listp (cdr cur)))
                ;; Bundle in key position - merge its bindings and private data
                (progn
                  (dolist (b (bind-bundle-bindings cur))
                    (push b bindings))
                  (setq private (append (bind-bundle-private cur) private)))
              (setq cur-key cur
                    next-keyp nil))
          (push (bind-make-binding cur-key cur) bindings)
          (setq next-keyp t)))
      (setq args (cdr args)))
    (bind-make-bundle (nreverse bindings) private)))

(defun bind-foreach (bundle function)
  "Call FUNCTION for each binding in BUNDLE.
FUNCTION is a function that takes key, def, and private as arguments.
BUNDLE must be a bind bundle: (PRIVATE . ((PRIV . (KEY . DEF)) ...))."
  (declare (indent 1))
  (dolist (binding (bind-bundle-bindings bundle))
    (funcall function
             (bind-binding-key binding)
             (bind-binding-def binding)
             (bind-binding-private binding))))

(defmacro bind-with-metadata (plist &rest body)
  "Evaluate BODY with PLIST merged with `bind--metadata'."
  (declare (indent 1))
  `(let* ((bind--metadata (append (list ,@plist) bind--metadata)))
     ,@body))

;;;###autoload
(defun bind-global-map ()
  "Return `current-global-map'."
  (current-global-map))

;;;###autoload
(defun bind-local-map ()
  "Return local map while replicating the behavior of `local-set-key'."
  (or (current-local-map)
      (let ((local-map (make-sparse-keymap)))
	    (use-local-map local-map)
	    local-map)))

;;;###autoload
(defmacro bind (&rest form-or-forms)
  "Bind many keys to many keymaps, multiple times.
Syntax is `(bind FORM)' or `(bind (FORM)...)' so (FORM) is
repeatable.  See `bind--singular' for what a FORM is.
FORM-OR-FORMS can be a single FORM or list of FORMs."
  (let ((expansion (bind--expand-synonyms form-or-forms)))
    (if (bind--singularp expansion)
        `(bind--singular ,expansion)
      `(bind--multiple (bind--singular) ,expansion))))

;;;###autoload
(defmacro bind-undo (&rest form)
  "Undo (or unbind) `bind' FORM keys."
  `(let ((bind--definer #'bind--definer-unbind))
     (bind-with-metadata (:engine 'bind-undo)
       (bind ,@form))))

;;;###autoload
(defmacro bind-save (&rest form)
  "Return a save of current definitions of key sequences for debugging.
This function doesn't bind anything but return current
definitions so that returned save can be restored with
`bind-restore' after FORM is executed with `bind' in case the
result is unwanted.

This function still evaluates functions inside FORM like
`bind-repeat', so it is not side effect free."
  `(let (bind--savings
	     (bind--definer #'bind--definer-save))
     (bind-with-metadata (:engine 'bind-save)
       (bind ,@form))
     bind--savings))

;;;###autoload
(defmacro bind-restore (save)
  "Restore definitions in SAVE from `bind-save'."
  `(bind-with-metadata (:engine 'bind-restore)
     (bind ,@(eval save))))

;;;###autoload
(defun bind-prefix (prefix &rest args)
  "Prefix each KEY in ARGS with PREFIX if KEY is a string.
PREFIX can also be ending with a modifier, such as C-, S- C-S-
etc.  Returns a bundle."
  (declare (indent 1))
  (let ((bundle (apply #'bind--bundle-from-args args)))
    (if (plist-get bind--metadata :engine)
        bundle
      (let (new-bindings
	        (prefix (concat prefix (if (string-match "\\([[:space:]]\\|^\\)\\(.-\\)+$"
						                             (car (last (split-string prefix))))
				                       ""
				                     " "))))
        (bind-foreach bundle
	      (lambda (key def private)
	        (push (bind-make-binding
                   (if (stringp key)
		               (concat prefix key)
		             key)
                   def
                   private)
                  new-bindings)))
        (bind-make-bundle (nreverse new-bindings))))))
(put :prefix 'lisp-indent-function 1)

;;;###autoload
(defun bind-autoload (&optional file-or-key &rest args)
  "Autoload DEFs in ARGS from FILE-OR-KEY or :main-file metadata.
If FILE-OR-KEY is a symbol, it's used as the file name.
Otherwise it's treated as a key and :main-file metadata is used.
Returns a bundle."
  (declare (indent 1))
  (let (file)
    (if (symbolp file-or-key)
        (setq file (symbol-name file-or-key))
      (setq file (plist-get bind--metadata :main-file)
            args `(,file-or-key ,@args)))
    (let ((bundle (apply #'bind--bundle-from-args args)))
      (when (not (plist-get bind--metadata :engine))
        (if (not file)
            (error "No file given to BIND-AUTOLOAD and no :main-file in metadata"))
        (bind-foreach bundle
	      (lambda (_key def _private)
	        (autoload def file nil t))))
      bundle)))
(put :autoload 'lisp-indent-function 1)

;;;###autoload
(defun bind-repeat (&optional target-map-s &rest args)
  "Add repeat prop to each DEF in ARGS for TARGET-MAP-S or :keymaps metadata.
This requires `repeat-mode' to be active to take effect.
Note that definitions should also appear in TARGET-MAP-S.
Returns a bundle."
  (declare (indent 0))
  (unless (or (keymapp target-map-s) (consp target-map-s))
    (setq args `(,target-map-s ,@args)
          target-map-s nil))
  (let ((bundle (apply #'bind--bundle-from-args args)))
    (if-let ((keymaps (ensure-list (or target-map-s
                                       (plist-get bind--metadata :keymaps)))))
        (bind-foreach bundle
          (if (plist-get bind--metadata :engine)
              (lambda (_key def _private)
                (put def 'repeat-map nil))
            (lambda (_key def _private)
              (dolist (keymap keymaps)
                (put def 'repeat-map keymap)))))
      (display-warning 'bind-repeat
                       (format "Couldn't repeat bindings: %s. No repeat map given."
                               bundle)))
    bundle))
(put :repeat 'lisp-indent-function 0)

;;;###autoload
(defun bind-wk (doc &rest args)
  "Add which-key DOC to bindings in ARGS.
For single binding, DOC describes that binding.
For multiple bindings (prefix case), DOC describes the prefix.
Returns a bundle with :which-key data in binding private."
  (declare (indent 1))
  (let* ((bundle (apply #'bind--bundle-from-args args))
         (bindings (bind-bundle-bindings bundle))
         (first-binding (car bindings))
         ;; prefix case = multiple bindings, trim 1; single = trim 0
         (trim-count (if (> (length bindings) 1) 1 0))
         (private (or (bind-binding-private first-binding) nil))
         (wk-list (plist-get private :which-key))
         (new-private (plist-put (copy-sequence private) :which-key
                                 (cons (cons doc trim-count) wk-list)))
         (new-first (bind-make-binding (bind-binding-key first-binding)
                                       (bind-binding-def first-binding)
                                       new-private)))
    (bind-make-bundle (cons new-first (cdr bindings)))))
(put :wk 'lisp-indent-function 1)

(defun bind-wk-finalizer (bundle)
  "Process which-key docs in BUNDLE and register with which-key per keymap."
  (let ((keymaps (plist-get bind--metadata :keymaps)))
    (bind-foreach bundle
      (lambda (key _def private)
        (when-let ((wk-list (plist-get private :which-key)))
          (dolist (entry wk-list)
            (let* ((doc (car entry))
                   (trim-count (cdr entry))
                   (key-parts (split-string key))
                   (target-key (if (zerop trim-count)
                                   key
                                 (string-join (butlast key-parts trim-count) " "))))
              (with-eval-after-load 'which-key
                (dolist (keymap keymaps)
                  (which-key-add-keymap-based-replacements keymap target-key doc))))))))))

(provide 'bind)

;;; bind.el ends here

