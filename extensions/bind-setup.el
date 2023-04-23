;;; bind-setup.el --- Bind support for setup.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 repelliuss

;; Author: repelliuss <https://github.com/repelliuss>
;; Maintainer: repelliuss <repelliuss@gmail.com>
;; Created: March 26, 2023
;; Modified: March 26, 2023
;; Version: 0.9.0
;; Package-Requires: ((bind "0.9") (setup "1.3") (emacs "25.1"))

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

;; Adds support for bind.el in setup.el.
;; `:main-file' metadata inserted so `bind-autoload' doesn't need explicit file name.
;; Active map of current setup context is inserted to *first FORM of `bind' FORM if it
;; is insertable. It is insertable if no map is given or a list of maps are given explicitly.
;; For example,
;; (bind c-mode-map "c" #'foo ...), the map won't be inserted.
;; (bind "c" #'foo), the map will be inserted and act like (bind pkg-mode-map "c" #'foo)
;; (bind (c-mode-map) "c" #'foo), the map will be inserted and act like (bind (pkg-mode-map c-mode-map) "c" #'foo).
;; (bind (my-function-returning-maps) "c" #'foo), the map won't be inserted.
;; Note that if there are multiple `bind' FORMS, the first one will be selected.

;;; Code:

(require 'setup)
(require 'bind)

;;;###autoload
(defmacro bind-setup-integrate (keyword)
  "Integrate `bind' for `use-package' with KEYWORD."
  `(setup-define ,keyword
     (lambda (&rest form)
       `(bind-with-metadata (:main-file ,(symbol-name (setup-get 'feature)))
	  ,(let ((map (setup-get 'map)))
	     (if (bind--singularp form)
		 (pcase (bind--map-insertable-formp form)
		   ('no `(bind ,@form))
		   ('yes `(bind ,map ,@form))
		   ('yes-merge `(bind (,map ,@(car form))
				      ,@(cdr form))))
	       (pcase (bind--map-insertable-formp (car form))
		 ('no `(bind ,@form))
		 ('yes `(bind (,map ,@(car form))
			      ,@(cdr form)))
		 ('yes-merge `(bind ((,map ,@(caar form))
				     ,@(cdar form))
				    ,@(cdr form))))))))
     :documentation "Bind BINDINGS in current map if FORM lets and intents to inserting current map."
     :debug '(form sexp)))

(provide 'bind-setup)

;;; bind-setup.el ends here
