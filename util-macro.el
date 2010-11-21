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

;;;
;;;; Tests
;; need el-expectations.el(written by rubikitch) to run.
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "aif")
      (expect 'foo (aif (or nil 'foo) it))
      (expect 'foo (aif (or nil 'foo) it 'bar))
      (desc "aand")
      (expect 123 (aand (+ 1 99) (+ it 20) (+ it 3)))
      (desc "alambda")
      (expect 3628800
        (funcall (alambda (x) (if (= x 0) 1 (* x (self (- x 1)))))
                 10))
      (desc "and-let*")
      (expect 100 (and-let* ((x 10)
                            ((> x 0))
                            (x* (* x x)))
                   x*))
      (expect nil (and-let* ((x 10)
                            ((< x 0))
                            (x* (* x x)))
                   x*))
      (expect 'done (let1 foo nil
                    (and-let* ((x 10)
                               ((> x 0))
                               (x* (* x x))
                               ((progn (setq foo 'done) t)))
                      foo)))
      (expect nil (let1 foo nil
                    (and-let* ((x 10)
                               ((< x 0))
                               (x* (* x x))
                               ((progn (setq foo 'done) t)))
                      foo)))
      (desc "let1")
      (expect 10 (let1 x 5 (+ x x)))
      (desc "rlet1")
      (expect '(3 2 1) (rlet1 xs (list 1 2 3)
                         (setq xs (nreverse xs))))
      (desc "cut")
      (expect '(10 20 30)
        (mapcar (cut '* 10 <>) '(1 2 3)))
      (expect '(11 12 13)
        (mapcar* (cut '+ <...>) '(1 2 3) (make-list 3 10)))
      (desc "cute")
      (expect '(121 122 123)
        (mapcar (cute '+ (* 10 10) (* 10 2) <>) '(1 2 3)))
      (expect '(131 142 153)
        (mapcar* (cute '+ (* 10 10) (* 10 2) <...>) '(1 2 3) '(10 20 30)))
      (expect '(10 20 30)
        (let1 x 9
          (mapcar (cute '* (incf x) <>) '(1 2 3))))
      (desc "with-gensyms")
      (expect t
        (not (memq 'y  (with-gensyms (x y z) (list x y z)))))
      (desc "with-lexical-bindings")
      (expect '(1 2 3)
        (progn
          (defun %counter (n)
            (with-lexical-bindings (n)
              #'(lambda () (incf n))))
          (let1 c (%counter 0)
            (list (funcall c)
                  (funcall c)
                  (funcall c)))))
      )))
;;(expectations-execute)

(provide 'util-macro)