;;; bind.el -*- lexical-binding: t; -*-

;; TODO: add header info
;; TODO: check linter
;; TODO: add docstrings
;; TODO: add examples

(defcustom bind-repeat-generate-name #'bind--repeat-prepend-repeat
  "Default.")

(defvar bind--definer #'define-key)

(defvar bind--metadata nil)

(defun bind--undefine-key (keymap key def)
  (define-key keymap key nil)
  (if (or (stringp def)
	  (vectorp def))
      (define-key keymap def nil)))

(defun bind--repeat-prepend-repeat (main-map-name)
  (concat "repeat-" main-map-name))

(defmacro bind--normalize-bindings (arg)
  `(if (consp (car ,arg))
       (setq ,arg (nconc (car ,arg) (cdr ,arg)))))

(defun bind--bind (keymap bindings)
  (while bindings
    (let ((key (car bindings))
	  (def (cadr bindings)))
      (funcall bind--definer
	       keymap (if (stringp key)
			  (kbd key)
			key) def))
    (setq bindings (cddr bindings))))

(defun bind--done (keymap-s bindings)
  (declare (indent 0))
  (bind--normalize-bindings bindings)
  (if (keymapp keymap-s)
      (bind--bind keymap-s bindings)
    (dolist (keymap keymap-s)
      (bind--bind keymap bindings))))

(defmacro bind--many (&rest rest)
  (let (unlisted)
    (dolist (elt rest)
      (setq unlisted
	    (nconc unlisted
		   `((cons (bind--normalize-first ,(car elt))
			   (list ,@(cdr elt)))))))
    `(dolist (elt (list ,@unlisted))
       (bind--done (car elt) (cdr elt)))))

(defmacro bind--normalize-first (map-s-or-fn)
  (if (or (fboundp (car-safe map-s-or-fn))
	  (symbolp map-s-or-fn))
      map-s-or-fn
    `(list ,@map-s-or-fn)))

(defmacro bind--main-map (bind-first)
  `(cond
    ((symbolp ,bind-first) ,bind-first)
    ((fboundp (car ,bind-first)) (cadr ,bind-first))
    (t (caar ,bind-first))))

(defmacro bind--with-metadata (bind-first &rest rest)
  `(let ((bind--metadata '(:main-map ,(bind--main-map first))))
     ,@rest))

(defmacro bind (&rest rest)
  (let ((first (car rest))
	(second (cadr rest)))
    (if (or (stringp second)
	    (vectorp second)
	    (fboundp (car second)))
	`(bind--with-metadata
	  (bind--done (bind--normalize-first ,first)
		      (list ,@(cdr rest))))
      `(bind--with-metadata
	(bind--many ,@rest)))))

(defmacro unbind (&rest rest)
  `(let ((bind--definer #'bind--undefine-key))
     (bind ,@rest)))

(defun bind-prefix (prefix &rest bindings)
  (declare (indent 1))
  (bind--normalize-bindings bindings)
  (let (new-bindings
	(prefix (concat prefix " ")))
    (while bindings
      (let ((key (car bindings))
	    (def (cadr bindings)))
	(push def new-bindings)
	(push (concat prefix key) new-bindings))
      (setq bindings (cddr bindings)))
    new-bindings))

(defun bind-autoload (file &rest bindings)
  (declare (indent 1))
  (bind--normalize-bindings bindings)
  (let ((it-bindings bindings))
    (while it-bindings
      (let ((def (cadr it-bindings)))
	(autoload def file nil t))
      (setq it-bindings (cddr it-bindings))))
  bindings)

(defun bind-repeat (&rest bindings)
  (declare (indent 1))
  (bind--normalize-bindings bindings)
  (let* ((main-map (plist-get bind--metadata :main-map))
	 (repeat-map
	  (intern (funcall bind-repeat-generate-name
			   (symbol-name main-map)))))
    (if (not (boundp repeat-map))
	(set repeat-map (make-sparse-keymap)))
    (set-keymap-parent (symbol-value repeat-map) (symbol-value main-map))
    (let ((it-bindings bindings))
      (while it-bindings
	(let ((def (cadr it-bindings)))
	  (put def 'repeat-map repeat-map))
	(setq it-bindings (cddr it-bindings)))))
  bindings)
