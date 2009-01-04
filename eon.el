;;; eon.el --- micro minimal object system for GNU Emacs Lisp

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME

;;; Code:

(require 'cl)

;;; Making proper symbols

(defun make-keyword (name)
  (let ((string (etypecase name
		  (symbol (symbol-name name))
		  (string name))))
    (if (string-match "^:" string)
	(intern string)
	(intern (concat ":" string)))))

(defun delimited-symbol (name delimiter)
  (let ((symname (if (stringp name) 
		     name 
		     (symbol-name name))))
    ;; already delimited?
    (if (string-match (regexp-quote delimiter) symname)
	(intern symname)
	;; no, delimit it
	(intern (concat delimiter symname delimiter)))))

(defvar class-symbol-delimiter "+")

(defun class-symbol (class-name)
  (delimited-symbol class-name class-symbol-delimiter))

(defvar constructor-symbol-delimiter "@")

(defun constructor-symbol (class-name)
  (delimited-symbol class-name constructor-symbol-delimiter))

;;; Defining objects, getting and setting slot values

(defstruct object class-name slots)

(defvar slot-lookup-failure (gensym))

(defun slot-value (object slot)
  (let ((result (getf (object-slots object) slot slot-lookup-failure)))
    (if (eq result slot-lookup-failure)
	(error "Cannot find slot %S" slot)
	result)))

(defun set-slot-value (object slot value)
  (setf (getf (object-slots object) slot)
	value))

(defalias '@ 'slot-value)

(defsetf slot-value set-slot-value)
     
;; (defun class-of (object)
;;   (symbol-value (object-class-name object)))

;;; Defining classes

(defstruct class 
  name constructor parent options slot-specs methods)

(defun class-exists (class-name)
  (let ((symbol (class-symbol class-name)))
    (and (boundp symbol)
	 (class-p (symbol-value symbol)))))

(defun class-definition (class-name)
  (let ((symbol (class-symbol class-name)))
    (if (class-exists class-name)
	(symbol-value symbol)
	(error "No such class %s." class-name))))

(defun set-class-definition (class-name class-definition)
  (assert (class-p class-definition))
  (let ((symbol (class-symbol class-name)))
    (when (class-exists class-name)
      (message "Warning; redefinition of class %S" class-name))
    (setf (symbol-value symbol)
	  class-definition)))

(defsetf class-definition set-class-definition)

(defun slot-spec (class-name slot-name)
  (assoc (make-keyword slot-name) 
	 (class-slot-specs (class-definition class-name))))

(defun normalize-slot-spec (spec)
  (etypecase spec
    (symbol (list (make-keyword spec) :initform nil))
    (list (destructuring-bind (name &key initform) spec
	    (list (make-keyword name)
		  :initform initform)))))

(defun* make-slot-initializer (slot-spec)
  (destructuring-bind (slot-name &key initform) slot-spec
    `(setf (slot-value self ,slot-name)
	   ,initform)))

(defmacro define-class (name options &rest slot-specs)
  (let* ((class-name (class-symbol name))
	 (constructor-name (constructor-symbol name))
	 (slots (mapcar #'normalize-slot-spec slot-specs))
	 (newob (gensym)))
    (destructuring-bind (&key parent &allow-other-keys) options
      `(progn
	 (setf (class-definition ',class-name)
	       (make-class :name ',class-name
			   :parent ',parent
			   :constructor #',constructor-name
			   :options ',options
			   :slot-specs ',slots))
	 (defun ,constructor-name ()
	   (let ((,newob (make-object :class-name ',name)))
	     (prog1 ,newob
	       (eon-prog ,newob
		 ,@(mapcar #'make-slot-initializer slots)))))))))

;;; Supporting method syntax

;; We need a general tree transformer function to process the method
;; body definitions.
    
(defun transform-tree (tester transformer tree)
  (cond ((consp tree)
	 ;; it's a cons. process the two subtrees.
	 (destructuring-bind (left . right) tree
	   (cons
	    ;; process left subtree.
	    (if (funcall tester left)
		(funcall transformer left)
		;; nothing to transform here. move on down the left side.
		(if (consp left)
		    (transform-tree tester transformer left)
		    left))
	    ;; process right subtree.
	    (transform-tree tester transformer right))))
	;; it's not a cons. test it.
	((funcall tester tree)
	 (funcall transformer tree))
	;; it failed the test. leave it alone.
	(t tree)))

;; Now we turn to the syntax itself and the required tree
;; transformations.

(defun block-transform-message-sends (body)
  "Process the code in BODY to transform message send syntax.
Vectors of the form [foo: ...] are transformed into Eon method
invocations: (>> :foo ...)"
  (labels ((tester (tree)
	     (and (vectorp tree)
		  (let ((s (aref tree 0)))
		    (and (symbolp s)
			 ;; message name must end in a colon
			 (string-match ":$" (symbol-name s))))))
	   (transformer (vec)
	     (let* ((sym (aref vec 0))
		    (sym-name (symbol-name sym)))
	       (concatenate
		'list
		(if (string-match (rx (and (group (one-or-more (not (any space ?:))))
					   ?:
					   (group (one-or-more (not (any space ?:))))
					   ?:))
				  sym-name)
		    ;; transform messages with an explicit prototype selector
		    (list 'select>>
			  (make-selector (match-string 1 sym-name))
			  (make-selector (match-string 2 sym-name)))
		    ;; transform ordinary message sends (i.e. method from self)
		    (list 'invoke-method
			  (make-keyword (substring sym-name 0 -1))))
		;; paste in the rest of the message args, the same in either case
		(subseq vec 1)))))
    (transform-tree #'tester #'transformer body)))

(defvar block-slot-reference-regexp
  (rx (sequence line-start ?< (group (one-or-more (not (any space)))) ?> line-end)))

(defun block-transform-slot-references (body)
  "Process the code in BODY to transform slot reference syntax.
Symbols of the form `<foo>' will become Eon slot references:
 (slot-value object :foo)"
  (lexical-let (string slot-name)
      (labels ((tester (tree)
		 (and (symbolp tree)
		      (progn
			(setf string (symbol-name tree))
			(setf slot-name
			      (when (string-match block-slot-reference-regexp string)
				(match-string 1 string))))))
	       (transformer (tree) (list 'slot-value 'self (make-keyword slot-name))))
	(transform-tree #'tester #'transformer body))))

(defmacro eon-block (arglist &rest body)
  (declare (indent 1))
  (let ((max-lisp-eval-depth 4096))
    (let* ((body1 (block-transform-message-sends body))
	   (body2 (block-transform-slot-references body1)))
      `(function* (lambda (self ,@arglist) ,@body2)))))

(defmacro eon-prog (object &rest body)
  "Run the BODY forms with the object as self.
The BODY forms are wrapped in an `eon-block+', so full Eon syntax is
available."
  (declare (indent 1))
  `(apply (eon-block () ,@body) ,object nil))

;;; Defining methods

(defun class-method (class method)
  (getf (class-methods class) (make-keyword method)))

(defun set-class-method (class method func)
  (setf (getf (class-methods class) (make-keyword method))
	func))

(defsetf class-method set-class-method)

(defvar method-symbol-delimiter ">>")

(defun method-symbol (class-name method)
  (intern (concat (symbol-name class-name)
		  method-symbol-delimiter
		  (symbol-name (make-keyword method)))))

(defmacro define-method (method class-name arglist &rest body)
  (declare (indent 3)
	   (debug (&define name sexp lambda-list
			   [&optional stringp] def-body)))
  (let ((class (class-definition class-name))
	(method-symbol (method-symbol class-name method)))
    `(progn 
       (setf (class-method (class-definition ',class-name) ',method)
	     ',method-symbol)
       (defalias ',method-symbol (eon-block ,arglist 
				   ,@(remove-if #'stringp body))))))

(defun invoke-method (method object &rest args)
  (let ((handler (class-method (class-definition 
				(object-class-name object))
			       method)))
    (apply handler object args)))

(defalias '>> 'invoke-method)

(defun create-object (class-name &rest ignore)
  (funcall (symbol-function (class-constructor 
			     (class-definition class-name)))))

(defmacro new (class-name &rest ignore)
  `(create-object ',class-name ,@ignore))
  

(provide 'eon)
;;; eon.el ends here
