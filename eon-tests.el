
(define-class foo () 
  (a :initform 1)
  (b :initform 2)
  c
  (d :initform 4))

(class-definition 'foo)

(define-method format foo ()
  (format "%S" (list <a> <b> <c> <d>)))

(define-method print foo ()
  (message [format: self]))

(define-method header-print foo (header-string)
  (message (format "%s: %S" header-string [format: self])))

(defvar bar (new foo))

(setf bar (new foo))

(@ bar :a)
(@ bar :b) 
(setf (@ bar :b) 44)
(@ bar :b) 

;; error, no such field: 
(@ bar :dd)


(>> :print bar)
