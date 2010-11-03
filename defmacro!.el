(defun mkstr (&rest args)
  (with-output-to-string 
    (dolist (a args) (princ a))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))


(defun group (source n)
  (if (not (listp source)) (error "group: not list"))
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(defun g!-symbol-p (s)
  (if (symbolp s)
      (let ((str (symbol-name s)))
        (string= (substring-no-properties str 0 2) "G!"))))

(defun o!-symbol-p (s)
  (if (symbolp s)
      (let ((str (symbol-name s)))
        (string= (substring-no-properties str 0 2) "O!"))))


(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro/g! (name args &body body)
  (let ((symbs (remove-duplicates
                (remove-if-not #'g!-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              symbs)
         ,@body))))

(defmacro defmacro! (name args &body body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "mkstr")
      (expect "abc" (mkstr 'a 'b 'c))
      (desc "symb")
      (expect '(abc) (symb 'a 'b 'c))
      (desc "flatten")
      (expect '(1 2 3 4 5 6) (flatten '((1 (2 (3 (4 . 5)))) . 6)))
      (desc "group")
      (expect '((a b a a) (c)) (group '(a b a a c) 4))
      (expect '((a b a) (a c)) (group '(a b a a c) 3))
      (expect '((a b) (a a) (c)) (group '(a b a a c) 2))
      (desc "g!-symbol-p")
      (expect t (g!-symbol-p 'G!foo))
      (expect nil (g!-symbol-p 'g!foo))
      (desc "o!-symbol-p")
      (expect t (o!-symbol-p 'O!foo))
      (expect nil (o!-symbol-p 'o!foo))
      (desc "o!-symbol-to-g!-symbol")
      (desc "defmacro/g!")
      (desc "defmacro!"))))
