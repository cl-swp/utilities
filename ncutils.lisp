;;;-*- Mode: common-lisp; syntax: common-lisp; package: nc; base: 10 -*-
;;;
;;;; Utilities for SWCLOS from AIMA and others
;;;
;;; Utilities in this file are taken from AIMA and redefined in package nc.
;;;
;; History
;; -------
;; 
;;; ==================================================================================

(cl:provide :ncutils)

(cl:defpackage :nc
  (:use :common-lisp :utils)
  (:export #:squash #:split-seq-on #:splice-seq-on #:null-string-p 
           #:set-equalp #:set-equal #:set-eq #:set-hash
           #:match+ #:match #:starts-with-char #:ends-with-char #:starts-with-str #:ends-with-str
           #:ensure-external #:external-symbol-from))

(in-package :nc)

(defun ensure-external (symbol &optional (package *package*))
  "ensures <symbol> to be external. If it is a string and such a 
   symbol does not exist, it is made."
  (when (not (null symbol))
    (multiple-value-bind (sym status) (intern (string symbol) package)
      (ecase status
        (:external (values sym status))
        (:internal (export sym package)
                   (values sym :external))
        (:inherited (error "Not Yet!"))
        ((nil)
         (export sym package)
         (values sym :external))))))
;;.............................................................................................
;;
;; Some Utilities
;;

(declaim (inline set-equal set-eq))

;;; the following codes are effective evevthough sets include duplicates.

(defun set-equalp (x y)
  "returns true if <x> and <y> is equal as set, the test function is equalp."
  (and (subsetp x y :test #'equalp)
       (subsetp y x :test #'equalp)))

(defun set-equal (x y)
  "returns true if <x> and <y> is equal as set, the test function is equa."
  (and (subsetp x y :test #'equal)
       (subsetp y x :test #'equal)))

(defun set-eq (x y)
  "returns true if <x> and <y> is equal as set, the test function is eq."
  (and (subsetp x y :test #'eq)
       (subsetp y x :test #'eq)))

(defun set-hash (set)
  "returns hash code for a set <set>, e.g., (= (set-hash '(A B)) (set-hash '(B A))) returns true.
   Note. this function is applicable for <make-hash-table> in 64bit memory machine and string of UNICODE up to 4bytes or the symbol."
  (flet ((make-code (str) (logand 4294967295 ; 32bits
                                  (let ((total 0))
                                    (loop for code in (map 'list #'char-code str)
                                        do (setq total (+ (ash total 16) code)))
                                    total))))
    (loop for e in (remove-duplicates (utils:mklist set))
        sum (typecase e
              (string (make-code e))
              (symbol (make-code (string e)))))))
;;.............................................................................................

;;;; Queues:

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
    (last (setf (rest (car q)) list))))

#|
(setq q (make-queue))
(queue-nconc q (map 'list #'identity "This is a queue"))
(front q)
(front (dequeue q))
|#

;;;
;;;; Delay Evaluation from OnLisp
;;;
;;; Delay mechanism is copied from ``On Lisp'' by Paul Graham.

;;;(defconstant +unforced+ (gensym))
;;;
;;;(defstruct delay forced closure)
;;;
;;;(defmacro delay (expr)
;;;  (declare (inline))
;;;  (let ((self (gensym)))
;;;    `(let ((,self (make-delay :forced +unforced+)))
;;;       (setf (delay-closure ,self)
;;;         #'(lambda ()
;;;             (setf (delay-forced ,self) ,expr)))
;;;       ,self)))
;;;
;;;(defun force (x)
;;;  (if (delay-p x)
;;;      (if (eq (delay-forced x) +unforced+)
;;;          (funcall (delay-closure x))
;;;        (delay-forced x))
;;;    x))

;; for SWCLOS connection
(defun delay-role-p (role)
  (declare (inline))
  (get role 'delay))

(defun set-delay-role (role)
  (setf (get role 'delay) t))

(defun split-seq-on (str &optional (ch #\Space))
  "returns a list of strings formed by breaking <str> at every occurence
of <ch> (which is not included).  Works for any sequence, not just strings,
but optimized for vectors."
  (when str
    (do* ((prev-pos 0 (1+ next-pos))
          (next-pos (position ch str)
                    (position ch str :start prev-pos))
          (stuff (list (subseq str 0 next-pos))
                 (cons (subseq str prev-pos next-pos)
                       stuff)))
         ((null next-pos) (nreverse stuff)))))

(defun external-symbol-from (str)
  (when str
    (let ((pos (position #\: str)))
      (cond (pos (ensure-external (subseq str (1+ pos)) (find-package (subseq str 0 pos))))
            (t (error "not yet"))))))

(defun splice-seq-on (lst &optional (ch #\Space))
  "returns a string formed by splicing every elements in <lst> with inserting <ch>."
  (when (null lst) (return-from splice-seq-on nil))
  (if (length=1 lst) (car lst)
    (labels ((seq-loop (lst)
                       (if (length=2 lst)
                           (list (first lst)
                                 (make-sequence 'cl:string 1 :initial-element ch)
                                 (second lst))
                         (cons (first lst)
                               (cons (make-sequence 'cl:string 1 :initial-element ch)
                                     (seq-loop (cdr lst)))))))
      (apply #'concatenate 'cl:string (seq-loop lst)))))

#|
(defun sequence-size (x)
  "SEQUENCE-SIZE <sequence>
   returns the size of the sequence."
  (and (sequencep x) (length (mop-slots x))))

(defun sequence->list (sequence)
  "SEQUENCE->LIST <sequence>
   returns a list of the members of the sequence, or the filler of first
   role, of second role, and so on."
  (and sequence
       (progn (assert (sequencep sequence) () "SEQUENCE->LIST: illegal MOP.") t)
       (loop for index from 1 to (sequence-size sequence) with filler
             when (setq filler (role-filler sequence index))
             collect filler)))

(defun sequence-member (mop sequence)
  "SEQUENCE-MEMBER <mop> <sequence>
   returns true if <mop> is a member of <sequence>."
  (and (sequencep sequence)
       (loop for slot in (mop-slots sequence)
            thereis (eql (slot-filler slot) mop))))

(defun sequence-splice (new old sequence)
  "SEQUENCE-SPLICE <mop-list> <mop> <sequence>
   returns a new sequence mop with all the elements of <sequence>, except 
   that <mop> is replaced with the elements of <mop-list>.  Note that a NIL 
   <mop-list> returns a sequence with <mop> removed."
  (list->sequence
   (loop for mop in (sequence->list sequence)
        append (cond ((eql mop old) new)
                      (t (list mop))))))

(defun sequence-insert (mop sequence)
  "SEQUENCE-INSERT <mop> <sequence>
   returns a new sequence mop with all the elements of <sequence> plus <mop>,
   added at the end."
  (cond ((null mop) sequence)
        ((sequence-member mop sequence) sequence)
        (t (list->sequence (append (sequence->list sequence) (list mop))))))

(defun sequence-add (time data sequence)
  (push `(,time ,data) (mop-slots sequence)))
|#
;;;; And Others, from Winston's Lisp.

(defun squash (x)
  "flattens a nested list <x> and returns a list that includes only atoms."
  (cond ((consp x) (utils:mappend #'squash x))
        (t (list x))))

;;;
;;; String Utils
;;;

(defun last-char (str)
  (char str (1- (length str))))

;;;
;;;; String Pattern
;;;

#|
(match+ "ñÿÇÃâ∫Ç≈" "ëÂÇ´Ç»åIÇÃñÿÇÃâ∫Ç≈")
(match+ "ñÿÇÃâ∫Ç≈" "ëÂÇ´Ç»åIÇÃñÿÇÃÇ≈")
|#

(defun match+ (source target &optional (start 0))
  (let ((pos (position (char source 0) target :start start :test #'char=)))
    (cond ((and (not (null pos)) (match source target pos))
           (values pos (+ pos (length source))))
          ((and (not (null pos)) (< pos (length source)))
           (match+ source target (1+ pos)))
          (t nil))))

(defun match (source target &optional (start 0))
  "compares <source> string to <target> string starting at <start> in <target>.
   and all characters in <source> are matched to <target> in order, returns true."
  (let ((result (mismatch source target :start2 start :test #'char=)))
    (or (null result)                ; just same string
        (= (length source) result))  ; source is included target and matched
    ))

(defun match1 (source target &optional (start 0))
  (let ((result (mismatch source target :start2 start :test #'char-equal)))
    (or (null result)                ; same string with case insensitive
        (= (length source) result))  ; source is included target and matched
    ))

(defun substitute-pattern (new old sequence &key (start 0))
  (let ((pos nil))
    (cond ((setq pos (position (char old 0) sequence :start start :test #'char=))
           (let ((result (mismatch old sequence :start2 pos :test #'char=)))
             (cond ((null result)                ; just same string
                    (concatenate 'string 
                      (subseq sequence 0 pos)
                      new))
                   ((= (length old) result)      ; old is included in sequence and matched
                    (concatenate 'string
                      (subseq sequence 0 pos)
                      new
                      (subseq sequence (+ pos result))))
                   (t (substitute-pattern new old sequence :start (+ pos result))))))
          (t sequence))))

(defun duplicate-p (list &key (test #'eql) key)
  (cond ((null list) nil)
        (key (cond ((member (funcall key (car list)) (cdr list) :test test :key key) t)
                   (t (duplicate-p (cdr list) :test test :key key))))
        (t (cond ((member (car list) (cdr list) :test test) t)
                 (t (duplicate-p (cdr list) :test test))))))

(declaim (inline null-string-p))
(defun null-string-p (str)
  (or (null str) (string= str "")))

(defun starts-with-char (char string &optional (start 0))
  "Is this string that starts with the given <char>?"
  (char= char (char string start)))

(defun starts-with-str (substr string &optional (start 0))
  "Is this string that starts with the given <substr>?"
  (string= substr (subseq string start (+ start (length substr)))))

(defun ends-with-char (char string &optional (end (length string)))
  "Is this string that ends with the given <char>?"
  (char= char (char string (1- end))))

(defun ends-with-str (substr string &optional (end (length string)))
  "Is this string that ends with the given <substr>?"
  (and (<= (length substr) end) (string= substr (subseq string (- end (length substr)) end))))

;;;
;;;; Negation Normal Form (NNF)
;;;
;;; NNF is a logical form in which negation is applied to only logical atom.
;;; In the following routine, non-NNF should be a prefix form in S-expression.
;;; ----------------------------------------------------------------------------------
;;;  <form> ::= <atom> | (not <form>) | (and <form>*) | (or <form>*) | 
;;;             (forall <var> <form>*) | (exists <var> <form>*) | (fills <var> <form>)
;;;  <NNF>  ::= <atom> | (not <atom>) | (and <NNF>*)  | (or <NNF>*)
;;;             (forall <var> <NNF>) | (exists <var> <NNF>) | (fills <var> <NNF>)
;;; Ex.
;;; (->nnf '(not (and (not (or (not A) (and C (not D)))))))
;;;  -> (or (not A) (and C (not D)))
;;; ----------------------------------------------------------------------------------

(defun ->nnf (P)
  "transforms non-NNF S-expression <P> to NNF and returns it."
  (move-not-inwards (move-not-inwards P)))

;;;
;;; This program is borrowed from AIMA
;;;
;;; Note that <move-not-inwards> returns ~P for P.

(defun move-not-inwards (P)
  "Given P, return ~P, but with the negation moved as far in as possible."
  (case (op P)
    ((t) 'nil)             ; seiji
    ((nil) 't)             ; seiji
    (not (arg1 P))
    (and (disjunction (mapcar #'move-not-inwards (args P))))
    (or  (conjunction (mapcar #'move-not-inwards (args P))))
    (forall (make-exp 'exists (arg1 P) (move-not-inwards (arg2 P))))
    (exists (make-exp 'forall (arg1 P) (move-not-inwards (arg2 P))))
    (fills  (error "Not Yet!") (make-exp 'fills  (arg1 P) (move-not-inwards (arg2 P))))
    (t (make-exp 'not P))))

(defun conjunction (args)
  "Form a conjunction with these args."
  (case (length args)
    (0 't)                   ; seiji
    (1 (first args))
    (t (cons 'and args))))

(defun disjunction (args)
  "Form a disjunction with these args."
  (case (length args)
    (0 'nil)              ; seiji
    (1 (first args))
    (t (cons 'or args))))

;;;
;;;
;;;

(defun print-hex (char)
  (format nil "\\u~A" (string-upcase (format nil "~4,'0X" (char-code char)))))

(defun read-hex (hex-list)
  (cond ((char= #\u (second hex-list))
         (values (code-char (parse-integer (coerce (subseq hex-list 2 6) 'string)
                                           :radix 16))
                 6))
        ((char= #\U (second hex-list))
         (values (code-char (parse-integer (coerce (subseq hex-list 2 10) 'string)
                                           :radix 16))
                 10))
        ((error "Missing '\' for read-hex."))))

#|
(print-hex #\çë)                    => "\\u56FD"
(read-hex (coerce "\\u56FD" 'list)) => #\çë
|#

(defun native2ascii (native-str)
  (labels ((2ascii (native-list)
                   (cond ((endp native-list) nil)
                         ((alphanumericp (car native-list))
                          (reuse-cons (car native-list)
                                      (2ascii (cdr native-list))
                                      native-list))
                         (t (append (coerce (print-hex (car native-list)) 'list)
                                    (2ascii (cdr native-list)))))))
    (coerce (2ascii (coerce native-str 'list))
            'string)))

(defun ascii2native (ascii-str)
  (labels ((2native (ascii-list)
                    (cond ((endp ascii-list) nil)
                          ((and (char= #\\ (first ascii-list))
                                (char-equal #\u (second ascii-list)))
                           (multiple-value-bind (char pos) (read-hex ascii-list)
                             (cons char (2native (subseq ascii-list pos)))))
                          (t (reuse-cons (car ascii-list)
                                         (2native (cdr ascii-list))
                                         ascii-list)))))
    (coerce (2native (coerce ascii-str 'list))
            'string)))

;; End of module
;; --------------------------------------------------------------------
