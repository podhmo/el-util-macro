(eval-when-compile (require 'cl))

;;; util for util
(defvar util-macro-alist nil)
(defvar util-macro-call-from-end-of-file-hook nil)
(add-hook 'util-macro-call-from-end-of-file-hook
          (lambda ()
            (let* ((names (mapcar 'car util-macro-alist))
                   (rx (mapconcat 'identity  names "\\|")))
              (font-lock-add-keywords 
               'emacs-lisp-mode
               `((,(format "(\\(%s\\)\\>" rx) 1 font-lock-keyword-face append))))))

(defmacro define-utilmacro (name args &rest body)
  `(progn 
     (defmacro ,name ,args
       ,@body)
     (add-to-list 'util-macro-alist
                  (cons (replace-regexp-in-string "\\*" "\\\\*"
                                                  (symbol-name ',name))
                        ',name))
     ',name))

;; (font-lock-add-keywords ;;buggy
;;  'emacs-lisp-mode
;;  '(("(\\(define-utilmacro\\) +\\([^ ]+?\\)\\>"
;;     (1 font-lock-keyword-face nil t) 
;;     (2 font-lock-function-name-face nil t) append)))

;; anaphoric macro
(define-utilmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(define-utilmacro aand (&rest args)
  "Anaphoric and. anaphorar is `it'"
  (declare (debug (&rest form)))
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif
                ,(car args)
                (aand ,@(cdr args))))))

(define-utilmacro alambda (args &rest body)
  "Anaphoric lambda. enable to self recursion using `self' anaphorar"
  (declare (indent 1)
           (debug lambda))
  `(labels ((self ,args ,@body))
     #'self))

;;; imported from scheme 
(define-utilmacro and-let* (bindings &rest body)
  "imported from srfi-2"
  (declare (indent 1))
  (reduce #'(lambda (binding r)
              (let ((head (car binding)))
                (cond ((and (atom head) (symbolp head))
                       `(let (,binding)
                          (when ,head ,r)))
                      ((listp head)
                       `(when ,head ,r))
                      (t 
                       (error "and-let*: invalid head %s" head)))))
          bindings :from-end t :initial-value `(progn ,@body)))


(define-utilmacro let1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let ((,var ,val))
     ,@body))

(define-utilmacro rlet1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let1 ,var ,val
     ,@body
     ,var))

(define-utilmacro cut (&rest args)
  "impoterd from srfi-28"
  (let* ((partial-args (list))
         (dotted-p nil)
         (args* (mapcar (lambda (x) (cond ((eq '<> x) (rlet1 x* (gensym)
                                                        (push x* partial-args)))
                                          ((eq '<...> x) (rlet1 x* (gensym)
                                                           (setq dotted-p t)
                                                           (push '&rest partial-args)
                                                           (push x* partial-args)))
                                          (t x))) args)))
    (setq partial-args (nreverse partial-args))
    (cond (dotted-p `(lambda ,partial-args (apply ,@args*)))
          (t `(lambda ,partial-args (funcall ,@args*))))))

(define-utilmacro cute (&rest args)
  "impoterd from srfi-28"
  (let* ((partial-args (list))
         (dotted-p nil)
         (pre-eval-sexps (list))
         (args* (mapcar (lambda (x) (cond ((and (listp x) (not (eq 'quote (car x))))
                                           (rlet1 g (gensym)
                                             (push `(,g ,x) pre-eval-sexps)))
                                          ((eq '<> x) (rlet1 x* (gensym)
                                                        (push x* partial-args)))
                                          ((eq '<...> x) (rlet1 x* (gensym)
                                                           (setq dotted-p t)
                                                           (push '&rest partial-args)
                                                           (push x* partial-args)))
                                          (t x)))
                        args)))
    (setq partial-args (nreverse partial-args))
    (cond (dotted-p `(lexical-let* (,@(nreverse pre-eval-sexps))
                       (lambda ,partial-args (apply ,@args*))))
          (t `(lexical-let* (,@(nreverse pre-eval-sexps))
                (lambda ,partial-args (funcall ,@args*)))))))


(define-utilmacro with-gensyms (syms &rest body)
  (declare (indent 1)
           (debug ((&rest symbolp)
                   body)))
  (let ((bindings (mapcar (lambda (x) `(,x ',(gensym))) syms)))
    `(let ( ,@bindings )
       ,@body)))

(define-utilmacro with-lexical-bindings (syms &rest body)
  (declare (indent 1)
           (debug ((&rest symbolp)
                   body)))
  (let ((clauses (loop for sym in syms
                       collect `( ,sym  ,sym ))))
    `(lexical-let ( ,@clauses )
       ,@body)))

(run-hook-with-args 'util-macro-call-from-end-of-file-hook)

(autoload 'symbol->definition "util-macro-install"
  "(_ 'foo \"bar\") -> `bar-foo' definition"
  nil)
(autoload 'util-macro-install "util-macro-install"
  "if you wana using utilmacro in your library, use this. (_ <prefix>)"
  t)
(provide 'util-macro)