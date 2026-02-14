;;; bind-which-key.el --- Which-key integration for bind -*- lexical-binding: t; -*-

;; Copyright (C) 2023 repelliuss

;; Author: repelliuss <https://github.com/repelliuss>
;; Maintainer: repelliuss <repelliuss@gmail.com>
;; Version: 0.9.1
;; Package-Requires: ((bind "0.9") (emacs "25.1"))

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

;; Provides which-key integration for bind.el.
;; Use `bind-wk' to add which-key descriptions to bindings.
;; Requiring this file automatically adds `bind-wk-finalizer' to `bind-finalizers'.

;;; Code:

(require 'bind)

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

;;;###autoload
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

(add-to-list 'bind-finalizers #'bind-wk-finalizer)

(provide 'bind-which-key)

;;; bind-which-key.el ends here
