(defun mapcar-safe (fn maybe-list)
  "mapcar enable to iterate maybe-list (include dot-list)"
  (let ((r (list)) (xs maybe-list))
    (condition-case e
        (progn
          (while (not (null xs))
            (push (funcall fn (car xs)) r)
            (setq xs (cdr xs)))
          (nreverse r))
      (error (rlet1 r* (nreverse r)
               (setcdr (last r*) (funcall fn xs)))))))

(defun tree-map (fn tree)
  (with-lexical-bindings (fn)
    (labels ((rec (tree)
                  (mapcar-safe #'(lambda (x) (if (listp x) (rec x) (funcall fn x)))
                               tree)))
      (rec tree))))

(defun cl-prettyprint-to-string (form)
  (with-temp-buffer (cl-prettyprint form)
                    (buffer-string)))

(defun* symbol->definition (name prefix &optional (src util-macro-alist))
  (lexical-let ((src (cons `(,name . ,name) src)))
    (let ((print-level nil)
          (print-length nil)
          (replace-body (lambda (body)
                          (tree-map (lambda (x)
                                      (aif (rassq x src)
                                          (intern (concat prefix (symbol-name (cdr it))))
                                        x))
                                    body)))
          (make-definition (lambda (def args body)
                             (cl-prettyprint-to-string
                              `(,def ,(intern (concat prefix (symbol-name name))) ,args
                                     ,@(funcall replace-body body)))))
          (expr (symbol-function name)))
      (cond ((eq 'macro (car expr))
             (destructuring-bind (_ _ args . body) expr
               (funcall make-definition 'defmacro args body)))
            ((eq 'lambda (car expr))
             (destructuring-bind (_ args . body) expr
               (funcall make-definition 'defun args body)))
            ((eq 'function (car expr))
             (destructuring-bind (_ (_ args . body)) expr
               (funcall make-definition 'defun args body)))
            (t (error "symbol->definition: invalid type -- %s"  (car expr)))))))

(defun util-macro-install (prefix)
  (let1 fmt "(put '%s 'lisp-indent-function %d)"
    (dolist (x (mapcar 'cdr util-macro-alist))
      (insert (symbol->definition x prefix))
      (and-let* ((n (get x 'lisp-indent-function)))
        (insert (format fmt  x (get x 'lisp-indent-function)))))))

(provide 'util-macro-install)