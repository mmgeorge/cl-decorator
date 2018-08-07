(defpackage :decorator/dtype
  (:use :cl)
  (:import-from :decorator/define)
  (:export #:dtype))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun values-p (type-spec)
    (and (listp type-spec)
         (symbolp (car type-spec))
         (string-equal "values" (symbol-name (car type-spec))))))


(defmacro dtype ((fbody declarations) f-arg-types f-ret-type)
  "A macro for defining function type signatures"
  (let* ((f-name (cadr fbody))
         (f-ret-values (if (values-p f-ret-type) f-ret-type
                           `(values ,f-ret-type &optional))))
    (list fbody
          (cons `(declaim (ftype (function ,f-arg-types ,f-ret-values) ,f-name)) declarations))))
