;;; bind-use-package.el --- Bind support for use-package -*- lexical-binding: t; -*-

;; Copyright (C) 2023 repelliuss

;; Author: repelliuss <https://github.com/repelliuss>
;; Maintainer: repelliuss <repelliuss@gmail.com>
;; Created: March 26, 2023
;; Modified: March 26, 2023
;; Version: 0.9.0
;; Package-Requires: ((bind "0.9") (use-package "2.4") (emacs "25.1"))

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
;;

;; Adds support for bind.el in use-package
;; `:main-file' metadata inserted so `bind-autoload' doesn't need explicit file name.
;; Active map of current use-package context is inserted to *first FORM of `bind' FORM if it
;; is insertable.  For example, if no map is given or list of maps is given explicitly.
;; For example,
;; (bind c-mode-map "c" #'foo ...), the map won't be inserted.
;; (bind "c" #'foo), the map will be inserted.
;; (bind (c-mode-map) "c" #'foo), the map will be inserted.
;; (bind (my-function-returning-maps) "c" #'foo), the map won't be inserted.
;; Note that if there are multiple `bind' FORMS, the first one will be selected.

;;; Code:

(require 'use-package-core)
(require 'bind)

;;;###autoload
(defmacro bind-use-package-integrate (keyword &optional anchor after test)
  "Integrate `bind' for `use-package' with KEYWORD.
See `use-package-list-insert' for ANCHOR, AFTER and TEST."
  `(progn
     (use-package-list-insert ,keyword use-package-keywords ,anchor ,after ,test)
     
     (defun ,(make-symbol (concat "use-package-normalize/" (symbol-name keyword))) (name-symbol keyword args)
       (use-package-only-one (symbol-name keyword) args
	 (lambda (label form)
	   (let ((map (make-symbol (concat (symbol-name name-symbol) "-mode-map"))))
	     (if (bind--singularp form)
		 (pcase (bind--map-insertable-formp form)
		   ('no form)
		   ('yes `(,map ,@form))
		   ('yes-merge `((,map ,@(car form))
				 ,@(cdr form))))
	       (pcase (bind--map-insertable-formp (car form))
		 ('no form)
		 ('yes `((,map ,@(car form))
			 ,@(cdr form)))
		 ('yes-merge `(((,map ,@(caar form))
				,@(cdar form))
			       ,@(cdr form)))))))))
     
     (defun ,(make-symbol (concat "use-package-handler/" (symbol-name keyword))) (name-symbol keyword form rest state)
       (let ((body (use-package-process-keywords name-symbol rest state)))
	 (use-package-concat
	  body
	  `((bind-with-metadata (:main-file ,(symbol-name name-symbol))
	      (bind ,@form))))))))

(provide 'bind-use-package)

;;; bind-use-package.el ends here
