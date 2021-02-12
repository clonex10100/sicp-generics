;; Term lists
(define (make-term order coeff)
  (cons order coeff))
(define (order term)
  (car term))
(define (coeff term)
  (cdr term))

(define (install-dense-term-list-package)
  (define (tag x)
    (attach-tag 'dense-term-list x))
  (define (the-empty-term-list)
    '())
  (define (empty-term-list? term-list)
    (null? term-list))
  (define (cons0 n l)
    (if (= n 0) l
        (cons 0 (cons0 (- n 1) l))))
  (define (adjoin-term term term-list)
    (let ((order (order term))
          (l (length term-list)))
      (if (< order l) (error "Already have the order")
          (cons (coeff term)
                (cons0 (- order l) term-list)))))
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list)
    (define (iter term-list)
      (if (or (null? term-list)
              (not (=zero? (car term-list))))
          term-list
          (iter (cdr term-list))))
    (iter (cdr term-list)))
  ;; Interface to rest of the system
  (put 'the-empty-term-list 'dense-term-list
       (lambda ()
         (tag (the-empty-term-list))))
  (put 'empty-term-list? '(dense-term-list)
       empty-term-list?)
  (put 'adjoin-term 'dense-term-list
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'first-term '(dense-term-list)
       (lambda (term-list)
         (first-term term-list)))
  (put 'rest-terms '(dense-term-list)
       (lambda (term-list)
         (tag (rest-terms term-list))))
  'done)
(install-dense-term-list-package)

(define (install-sparse-term-list-package)
  (define (tag x)
    (attach-tag 'sparse-term-list x))
  (define (the-empty-term-list)
    '())
  (define (empty-term-list? term-list)
    (null? term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  ;; Interface to rest of the system
  (put 'the-empty-term-list 'sparse-term-list
       (lambda ()
         (tag (the-empty-term-list))))
  (put 'empty-term-list? '(sparse-term-list)
       empty-term-list?)
  (put 'adjoin-term 'sparse-term-list
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'first-term '(sparse-term-list)
       (lambda (term-list)
         (first-term term-list)))
  (put 'rest-terms '(sparse-term-list)
       (lambda (term-list)
         (tag (rest-terms term-list))))
  'done)
(install-sparse-term-list-package)

(define (adjoin-term term term-list)
  ((get 'adjoin-term (type-tag term-list))
   term
   (contents term-list)))
(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
(define (empty-term-list? term-list)
  (apply-generic 'empty-term-list? term-list))
(define (the-empty-sparse-term-list)
  ((get 'the-empty-term-list 'sparse-term-list)))
(define (the-empty-dense-term-list)
  ((get 'the-empty-term-list 'dense-term-list)))
(define (the-empty-term-list)
  (the-empty-sparse-term-list))

(define (install-polynomial-package)
  ;; internal procedures
  (define (tag x)
    (attach-tag 'polynomial x))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y)
         (eq? x y)))

  ;; Poly Representation
  (define (make-poly var term-list)
    (cons var term-list))
  (define (variable poly)
    (car poly))
  (define (term-list poly)
    (cdr poly))

  ;; Add
  (define (add-terms t1 t2)
    (cond ((empty-term-list? t1) t2)
          ((empty-term-list? t2) t1)
          (else
           (let ((x (first-term t1))
                 (y (first-term t2)))
             (cond ((> (order x) (order y))
                    (adjoin-term
                     x
                     (add-terms (rest-terms t1) t2)))
                   ((> (order y) (order x))
                    (adjoin-term
                     y
                     (add-terms t1 (rest-terms t2))))
                   (else (adjoin-term
                          (make-term (order x)
                                     (add (coeff x) (coeff y)))
                          (add-terms (rest-terms t1)
                                     (rest-terms t2)))))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
                   ADD-POLY"
               (list p1 p2))))

  ;; Negate
  (define (negate-terms tl)
    (if (empty-term-list? tl) tl
        (let ((t (first-term tl)))
          (adjoin-term 
           (make-term (order t)
                      (negate (coeff t)))
           (negate-terms (rest-terms tl))))))
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))
  ;; Subtract
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (negate-terms (term-list p2))))
        (error "Polys not in same var:
                   ADD-POLY"
               (list p1 p2))))
  ;; Multiply
  (define (mul-term x y)
    (make-term (+ (order x)
                  (order y))
               (mul (coeff x)
                    (coeff y))))
  (define (mul-terms t1 t2)
    (if (empty-term-list? t1) t1
        (let ((term (first-term t1)))
          (add-terms (mul-term-by-all-terms term t2)
                     (mul-terms (rest-terms t1) t2)))))
  (define (mul-term-by-all-terms t tl)
    (if (empty-term-list? tl) tl
        (adjoin-term
         (mul-term t (first-term tl))
         (mul-term-by-all-terms t
                                (rest-terms tl)))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
                   MUL-POLY"
               (list p1 p2))))
  ;; Divide
  (define (div-terms L1 L2)
    (if (empty-term-list? L1)
        (list (the-empty-term-list)
              (the-empty-term-list))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-term-list) L1)
              (let ((new-c (div (coeff t1)
                                (coeff t2)))
                    (new-o (- (order t1)
                              (order t2))))
                (let ((new-term (make-term new-o new-c)))
                  (let ((rest-of-result
                         (div-terms
                          (add-terms L1
                                     (negate-terms
                                      (mul-terms
                                       (adjoin-term new-term
                                                    (the-empty-term-list))
                                       L2)))
                          L2)))
                    (list
                     (adjoin-term new-term
                                  (car rest-of-result))
                     (cadr rest-of-result)))))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (if (not (=zero-poly? p2))
            (make-poly
             (variable p1)
             (div-terms (term-list p1)
                        (term-list p2)))
            (error "Division by zero:
               DIV-POLY"
                   (list p1 p2)))
        (error "Polys not in same var:
                   DIV-POLY"
               (list p1 p2))))
  ;; Zero?
  (define (=zero-poly? p)
    (empty-term-list? (term-list p)))
  ;; Canonicalize
  (define (var-level var)
    (cond ((eq? var 'x) 3)
          ((eq? var 'y) 2)
          ((eq? var 'z) 1)))

  (define (highest-var terms)
    (define (iter var terms)
      (if (empty-term-list? terms) var
          (let ((term (coeff (first-term terms))))
            (iter (if (and (eq? (type-tag term) 'polynomial)
                           (> (var-level (variable (contents term)))
                              (var-level var)))
                      (variable (contents term))
                      var)
                  (rest-terms terms)))))
    (iter 'z terms))

  (define (invert-poly var-a term)
    "term is term of a poly with var-a that has a poly coeff.
Return a poly of term's coeff's var."
    (let ((torder (order term))
          (tcoeff (contents (coeff term))))
      (let ((var-b (variable tcoeff))
            (terms (term-list tcoeff)))
        (define (switch-terms terms)
          (if (empty-term-list? terms) terms
              (let ((term (first-term terms)))
                (adjoin-term
                 ;; (3x^2)y^3 -> (3y^3)x^2
                 (make-term (order term)
                            (make-polynomial var-a
                                             (adjoin-term
                                              (make-term torder
                                                         (coeff term))
                                              (the-empty-term-list))))
                 (switch-terms (rest-terms terms))))))
        (make-polynomial var-b
                         (switch-terms terms)))))

  (define (switch-term var vart term)
    "Switch TERM of poly with VAR to term of poly with VART"
    (if (and (eq? (type-tag (coeff term)) 'polynomial)
             (eq? (variable (contents (coeff term))) vart))
        (invert-poly var term)
        (tag
         (make-zero-poly vart
                         (make-polynomial var
                                          (adjoin-term
                                           term
                                           (the-empty-term-list)))))))

  (define (canonicalize poly)
    ;; (display "Poly: ") (display poly) (newline)
    ;; (display "High: ") (display (highest-var (term-list poly))) (newline)
    (define (canonicalize-terms terms)
      (if (empty-term-list? terms) terms
          (let ((term (first-term terms)))
            ;; (display "Canonicalizing term:")(display term)(newline)
            (adjoin-term
             (let ((tcoeff (coeff term)))
               ;; (display  "Coe: ")(display tcoeff)(newline)
               (if (eq? (type-tag tcoeff) 'polynomial)
                   (make-term (order term)
                              (canonicalize (contents tcoeff)))
                   term))
             (canonicalize-terms (rest-terms terms))))))
    (let ((var (variable poly))
          ;; Recursively cananocalize all terms in this poly
          (terms (canonicalize-terms (term-list poly))))
      ;; (display "Canon terms: ") (display terms) (newline)
      ;; (display "Var: ") (display var) (newline)
      ;; Determine the highest priority variable of subterms
      (let ((tvar (highest-var terms)))
        ;; (display "Tvar: ") (display tvar) (newline)
        (define (coerce-terms terms)
          (if (empty-term-list? terms)
              (make-polynomial tvar (the-empty-term-list))
              (add (switch-term var tvar (first-term terms))
                   (coerce-terms (rest-terms terms)))))
        ;; If the highest priority variable of subterms is higher
        ;; priority than the variable of the polynomial, switch all the terms,
        ;; else remake the poly
        (if (> (var-level tvar) (var-level var))
            (coerce-terms terms)
            (make-polynomial var
                             terms)))))

  ;; End Canonacalize
  (define (poly-pretty-print poly)
    (let ((var (variable poly)))
      (define (iter terms)
        (if (empty-term-list? terms) #t
            (let ((term (first-term terms)))
              (display "(")
              (pretty-print (coeff term))
              (display ")")
              (display var)
              (display "^")
              (display (order term))
              (when (not (empty-term-list? (rest-terms terms)))
                (display " + ")
                (iter (rest-terms terms))))))
      (iter (term-list poly))))

  ;; interface to rest of the system
  

  ;; Allow arithmetic on other types
  (define (make-zero-poly var val)
    (make-poly var
               (adjoin-term
                (make-term 0 val)
                (the-empty-term-list))))
  (for-each
   (lambda (tag)
     (for-each
      (lambda (op)
        (let ((f (lambda (p v)
                   (let ((p1 p)
                         (p2 (make-zero-poly (variable p)
                                             (attach-tag tag v))))
                     ((get op '(polynomial polynomial))
                      p1 p2)))))
          (put op `(polynomial ,tag)
               (lambda (p v) (f p v)))
          (put op `(,tag polynomial)
               (lambda (v p) (f p v)))))
      '(add sub mul div)))
   number-tower)

  (define (coerce-polys p1 p2)
    (let ((p1 (contents (canonicalize p1)))
          (p2 (contents (canonicalize p2))))
      (let ((v1 (variable p1))
            (v2 (variable p2)))
        (cond ((eq? v1 v2) (list p1 p2))
              ((> (var-level v1) (var-level v2))
               (list p1 (make-zero-poly v1 (tag p2))))
              (else
               (list (make-zero-poly v2 (tag p1)) p2))))))

  (put 'add '(polynomial polynomial)
       (lambda (x y)
         (tag (apply add-poly (coerce-polys x y)))))
  (put 'sub '(polynomial polynomial)
       (lambda (x y)
         (tag (apply sub-poly (coerce-polys x y)))))
  (put 'mul '(polynomial polynomial)
       (lambda (x y)
         (tag (apply mul-poly (coerce-polys x y)))))
  (put 'div '(polynomial polynomial)
       (lambda (x y)
         (tag (apply div-poly (coerce-polys x y)))))
  (put 'negate '(polynomial)
       (lambda (x)
         (tag (negate-poly x))))
  (put 'pretty-print '(polynomial)
       poly-pretty-print)
  (put '=zero? '(polynomial)
       (lambda (x)
         (=zero-poly? x)))
  (put 'canonicalize '(polynomial)
       (lambda (x)
         (tag (canonicalize x))))
  (put 'make 'polynomial
       (lambda (var term-list)
         (tag (make-poly var term-list))))
  'done)
(install-polynomial-package)


;; Testing
(define (canon x)
  (apply-generic 'canonicalize x))

(define t1 (adjoin-term
            (make-term 7 9.0)
            (adjoin-term
             (make-term 3 (make-rational 5 7))
             (adjoin-term
              (make-term 1 8)
              (the-empty-sparse-term-list)))))
(define p1 (make-polynomial 'x t1))

(define t2 (adjoin-term
            (make-term 7 1.5)
            (adjoin-term
             (make-term 2 9)
             (adjoin-term
              (make-term 1 -3)
              (the-empty-sparse-term-list)))))

(define p2 (make-polynomial 'y t2))

(define t3 (adjoin-term
            (make-term 3 p1)
            (adjoin-term
             (make-term 2 (make-rational 69 420))
             (adjoin-term
              (make-term 1 p2)
              (the-empty-sparse-term-list)))))

(define p3 (make-polynomial 'y t3))
