;;; inherit-local.el --- Inherited buffer-local variables

;; Copyright (C) 2016 Shea Levy

;; Author: Shea Levy
;; URL: https://github.com/shlevy/inherit-local/tree-master/
;; Version : 1.0

;;; Commentary:

;; This package provides the infrastructure for "inherited"
;; buffer-local variables: Those whose values are not reflected
;; globally but are initially shared by child buffers.

;; Because there is no clear-cut definition of "child" (if I start erc
;; from my notmuch buffer, is there a real relationship there?), this
;; package doesn't decide what you mean by child.  Instead, it
;; provides hooks for determining when inheritance might be relevant
;; and a function for performing the inheritance on a given buffer.

;;; Code:

(defvar-local inherit-local--variables (make-hash-table))

(put 'inherit-local--variables 'permanent-local t)

;;;###autoload
(defun inherit-local (variable)
  "Make VARIABLE (a symbol) inherited."
  (puthash variable nil inherit-local--variables))

(defun inherit-local-uninherit (variable)
  "Uninherit VARIABLE (a symbol)."
  (remhash variable inherit-local--variables))

(defun inherit-local-inherit-child (buffer)
  "Inherit inherited variables in BUFFER.
Returns 't' if there were any variables to inherit, 'nil' otherwise."
  (maphash
   (lambda (key ignored)
     (when (boundp key)
       (let ((val (symbol-value key)))
	 (with-current-buffer buffer
	   (set key val)))))
   inherit-local--variables)
  (not (eq (hash-table-count inherit-local--variables) 0)))

;;;###autoload
(defmacro inherit-local-permanent (var value)
  "Set VAR locally to VALUE, declare VAR permanent, inherit VAR."
  `(progn
     (setq-local ,var ,value)
     (put (quote ,var) 'permanent-local t)
     (inherit-local (quote ,var))))

(provide 'inherit-local)

;;; inherit-local.el ends here
