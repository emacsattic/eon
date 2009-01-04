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

;; TODO define methods
;; TODO single inheritance
;; TODO initforms
;; TODO bring slots into scope <foo>
;; TODO method invocation with [verb object args... ] 
;; TODO font-locking support
;; TODO bracket parenthesis support

;;; Code:

(require 'cl)

;;; Dealing with symbols

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

(defvar struct-symbol-delimiter "++")

(defun struct-symbol (class-name)
  (delimited-symbol class-name struct-symbol-delimiter))
  
(defvar class-symbol-delimiter "+")

(defun class-symbol (class-name)
  (delimited-symbol class-name class-symbol-delimiter))

(defvar constructor-symbol-delimiter "@")

(defun constructor-symbol (class-name)
  (delimited-symbol class-name constructor-symbol-delimiter))

;;; Defining classes

(defstruct class name constructor struct-name options slots methods)

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

(defmacro define-class (name options &rest slots)
  (let* ((class-name (class-symbol name))
	 (struct-name (struct-symbol name))
	 (constructor-name (constructor-symbol name)))
    `(progn
       (defstruct (,struct-name (:constructor ,constructor-name))
	 ,@slots)
       (setf (class-definition ',class-name)
	     (make-class :name ',class-name
			 :struct-name ',struct-name
			 :constructor #',constructor-name
			 :options ',options
			 :slots ',slots)))))

;;; Defining objects, getting and setting slot values

(defstruct object class-name slots)

(defvar slot-lookup-failure (gensym))

(defun slot-value (object slot)
  (let ((result (getf (object-slots object) slot)))
    (if (eq result slot-lookup-failure)
	(error "Cannot find slot %S" slot)
	result)))

(defun set-slot-value (object slot value)
  (setf (getf (object-slots object) slo)
	value))

(defsetf slot-value set-slot-value)
     

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
Vectors of the form [foo: ...] are transformed into Eon message
sends: (>> :foo ...)"
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
		    (list '>>
			  (make-keyword (substring sym-name 0 -1))))
		;; paste in the rest of the message args, the same in either case
		(subseq vec 1)))))
    (transform-tree #'tester #'transformer body)))

(defvar block-slot-reference-regexp
  (rx (sequence line-start ?< (group (one-or-more (not (any space)))) ?> line-end)))

(defun block-transform-slot-references (body)
  "Process the code in BODY to transform slot reference syntax.
Symbols of the form `<foo>' will become Eon slot references:
 (slot-value :foo object)"
  (lexical-let (string slot-name)
      (labels ((tester (tree)
		 (and (symbolp tree)
		      (progn
			(setf string (symbol-name tree))
			(setf slot-name
			      (when (string-match block-slot-reference-regexp string)
				(match-string 1 string))))))
	       (transformer (tree) (list 'slot-value (make-keyword slot-name) 'self)))
	(transform-tree #'tester #'transformer body))))

(defmacro block+ (arglist &rest body)
  (declare (indent 1))
  (let ((max-lisp-eval-depth 4096))
    (let* ((body1 (block-transform-message-sends body))
	   (body2 (block-transform-slot-references body1)))
      `(function* (lambda (self ,@arglist) ,@body2)))))

(defmacro prog+ (object &rest body)
  "Run the BODY forms with the object as self.
The BODY forms are wrapped in a `block+', so full Eon syntax is
available."
  (declare (indent 1))
  `(apply (block+ () ,@body) ,object nil))

(provide 'eon)
;;; eon.el ends here
