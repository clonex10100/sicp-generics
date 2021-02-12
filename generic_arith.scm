;;
;; Utils
;;
(define table (make-equal-hash-table))

(define (put op types proc)
  (hash-table-set! table (cons op types) proc))

(define (get op types)
  (hash-table-ref/default table (cons op types) #f))

(define coercion-table (make-equal-hash-table))

(define (put-coercion op types proc)
  (hash-table-set! coercion-table (cons op types) proc))

(define (get-coercion op types)
  (hash-table-ref/default coercion-table (cons op types) #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((number? datum)
         (if (integer? datum)
             'scheme-int
             'scheme-real))
        ((pair? datum)
         (car datum))
        (else
         (error "Bad tagged datum:
                   TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum)
         (cdr datum))
        (else 
         (error "Bad tagged datum:
                   CONTENTS" datum))))
;;
;; Packages
;;
(define (install-int-package)
  (define (tag x)
    (attach-tag 'scheme-int x))
  (put 'add '(scheme-int scheme-int)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-int scheme-int)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-int scheme-int)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-int scheme-int)
       (lambda (x y) (tag (floor (/ x y)))))
  (put 'equ? '(scheme-int scheme-int)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-int)
       (lambda (x) (= 0 x)))
  (put 'sqre '(scheme-int)
       (lambda (x)
         (tag (square x))))
  (put 'sqroot '(scheme-int)
       (lambda (x)
         (sqroot (make-real x))))
  (put 'cose '(scheme-int)
       (lambda (x)
         (cose (make-real x))))
  (put 'sine '(scheme-int)
       (lambda (x)
         (sine (make-real x))))
  (put 'artan '(scheme-int scheme-int)
       (lambda (x y)
         (artan (make-real x)
                (make-real y))))
  (put 'negate '(scheme-int)
       (lambda (x)
         (tag (- x))))
  (put 'raise 'scheme-int
       (lambda (x)
         (make-rational x 1)))
  (put 'pretty-print '(scheme-int)
       display)
  (put 'make 'scheme-int
       (lambda (x) (tag x)))
  'done)
(install-int-package)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (rat->real x)
    (make-real (/ (numer x) (denom x))))
  (define (=zero?-rat x)
    (= (numer x) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  (put 'sqre '(rational)
       (lambda (x)
         (tag (make-rat (square (numer x))
                        (square (denom x))))))
  (put 'sqroot '(rational)
       (lambda (x)
         (sqroot (rat->real x))))
  (put 'cose '(rational)
       (lambda (x)
         (cose (rat->real x))))
  (put 'sine '(rational)
       (lambda (x)
         (sine (rat->real x))))
  (put 'negate '(rational)
       (lambda (x)
         (tag (make-rat (- (numer x))
                        (denom x)))))
  (put 'artan '(rational rational)
       (lambda (x y)
         (artan (rat->real x)
                (rat->real y))))
  (put 'raise 'rational
       (lambda (x)
         (rat->real x)))
  (put 'project 'rational
       (lambda (x)
         (round (/ (numer x)
                   (denom x)))))
  (put 'pretty-print '(rational)
       (lambda (x)
         (display (numer x))
         (display "/")
         (display (denom x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(install-rational-package)

(define (install-real-package)
  (define (tag x)
    (attach-tag 'scheme-real x))
  (put 'add '(scheme-real scheme-real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-real scheme-real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-real scheme-real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-real scheme-real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-real scheme-real)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-real)
       (lambda (x) (= 0 x)))
  
  (put 'sqre '(scheme-real)
       (lambda (x)
         (tag (square x))))
  (put 'sqroot '(scheme-real)
       (lambda (x)
         (tag (sqrt x))))
  (put 'cose '(scheme-real)
       (lambda (x)
         (tag (cos x))))
  (put 'sine '(scheme-real)
       (lambda (x)
         (tag (sin x))))
  (put 'artan '(scheme-real scheme-real)
       (lambda (x y)
         (tag (atan x y))))
  (put 'negate '(scheme-real)
       (lambda (x)
         (tag (- x))))
  (put 'raise 'scheme-real
       (lambda (x)
         (let ((x (contents x)))
           (make-complex-from-real-imag
            x
            0))))
  (put 'project 'scheme-real
       (lambda (x)
         (round x)))
  (put 'pretty-print '(scheme-real)
       display)
  (put 'make 'scheme-real
       (lambda (x) (tag x)))
  'done)
(install-real-package)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqroot (add (sqre (real-part z))
             (sqre (imag-part z)))))
  (define (angle z)
    (artan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cose a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cose (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqroott (add (sqre x) (sqre y)))
          (artan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)

;; Internal complex representation selectors
(define (real-part x)
  (apply-generic 'real-part x))
(define (imag-part x)
  (apply-generic 'imag-part x))
(define (magnitude x)
  (apply-generic 'magnitude x))
(define (angle x)
  (apply-generic 'angle x))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put '=zero? '(complex)
       (lambda (x) (equ? 0 (real-part x) (imag-part x))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (equ? (real-part x) (real-part y))
                          (equ? (imag-part x) (imag-part y)))))
  (put 'negate '(complex)
       (lambda (x)
         (tag (make-from-real-imag
               (negate (real-part x))
               (imag-part x)))))
  (put 'project 'complex
       (lambda (x)
         (real-part x)))
  (put 'pretty-print '(complex)
       (lambda (x)
         (display (real-part x))
         (display " i")
         (display (imag-part x))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  'done)
(install-complex-package)

;; Helpers
(define (findr func lst)
  "Like find but returns the result of the function instead of the
list item."
  (if (null? lst) #f
      (let ((res (func (car lst))))
        (if res res
            (findr func (cdr lst))))))

(define (uniq lst)
  (define (iter lst nlst)
    (if (null? lst) nlst
        (let ((lst (cdr lst))
              (x (car lst)))
          (iter lst
                (if (memq x nlst) nlst
                    (cons x nlst))))))
  (reverse (iter lst '())))

(define (index-of lst x)
  (define (iter lst i)
    (if (null? lst) -1
        (if (eq? (car lst) x) i
            (iter (cdr lst) (+ i 1)))))
  (iter lst 0))

;; Type tower
(define number-tower '(scheme-int rational scheme-real complex))

(define (level x)
  (index-of number-tower x))

(define (drop x)
  (if (not (pair? x)) x
      (let ((project (get 'project (type-tag x))))
        (if (not project) x
            (let ((lx (project (contents x))))
              (if (equ? (raise lx) x)
                  (drop lx)
                  x))))))

(define (apply-generic op . args)
  (drop
   (let ((type-tags (map type-tag args)))
     (define (coerce-args-to target-type)
       (map (lambda (arg tag)
              (let ((coe (get-coercion tag target-type)))
                (if coe (coe arg) arg)))
            args
            type-tags))
     (define (raisen arg n)
       (if (eq? n 0)
           arg
           (raisen (raise arg) (- n 1))))
     (let ((proc (get op type-tags)))
       (if proc
           (apply proc (map contents args))
           (let* ((number-tower (map level type-tags))
                  (target-level (apply max number-tower))
                  (coercions (map
                              (lambda (x)
                                (- target-level x))
                              number-tower))
                  (args (map raisen args coercions)))
             (let ((proc (get op (map type-tag args))))
               (if proc
                   (apply proc (map contents args))
                   (error
                    "No method for these types"
                    (list op type-tags))))))))))

;;
;; User facing 
;;

;; Procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (negate x) (apply-generic 'negate x))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (sqre x) (apply-generic 'sqre x))
(define (sqroot x) (apply-generic 'sqroot x))
(define (cose x) (apply-generic 'cose x))
(define (sine x) (apply-generic 'sine x))
(define (artan x y) (apply-generic 'artan x y))

(define (project x) ((get 'project (type-tag x))
                   (contents x)))
(define (raise x) ((get 'raise (type-tag x))
                   (contents x)))

(define (pretty-print x) (apply-generic 'pretty-print x))

;; Constructors
(define make-int (get 'make 'scheme-number))
(define make-rational (get 'make 'rational))
(define make-real (get 'make 'scheme-real))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-polynomial var term-list)
  ((get 'make 'polynomial) var term-list))
