(load "~/.schemerc.scm")
(load-module "sicp.table")

(define (make-vector coords)
  (list 'vector coords (make-table)))

(define (vector? vec)
  (eq? (car vec) 'vector))

(define (vector-cache vec)
  (assert (vector? vec) "not a vector")
  (caddr vec))

(define (vector-coords vec)
  (assert (vector? vec) "not a vector")
  (cadr vec))

(define (vector-coord n vec)
  (nth n (vector-coords vec)))

(define (vector-dimension vec)
  (table-getset! (vector-cache vec)
                  'dimension
                  (lambda () (length (vector-coords vec)))))

(define (vector-add v1 v2)
  (assert (= (vector-dimension v1) (vector-dimension v2))
          "cannot add vectors of different dimensions")
  (define (coord-add c1 c2)
    (map (lambda (t) (+ (car t) (cadr t))) (zip c1 c2)))
  (make-vector (coord-add (vector-coords v1)
                          (vector-coords v2))))

(define (vector-sub v1 v2)
  (assert (= (vector-dimension v1) (vector-dimension v2))
          "cannot sub vectors of different dimensions")
  (define (coord-sub c1 c2)
    (map (lambda (t) (- (car t) (cadr t))) (zip c1 c2)))
  (make-vector (coord-sub (vector-coords v1)
                          (vector-coords v2))))

(define (vector-scale vec c)
  (make-vector (map (lambda (x) (* x c)) (vector-coords vec))))

(define (vector-dot v1 v2)
  (assert (= (vector-dimension v1) (vector-dimension v2))
          "cannot dot vectors of different dimensions")
  (let ((c1 (vector-coords v1))
        (c2 (vector-coords v2)))
    (fold + 0 (map (lambda (t) (* (car t) (cadr t))) (zip c1 c2)))))

(define (vector-magnitude vec)
  (table-getset!
    (vector-cache vec)
    'magnitude
    (lambda () (sqrt (fold + 0 (map (lambda (x) (* x x)) (vector-coords vec)))))))

(define (vector-normalize vec)
  (table-getset!
    (vector-cache vec)
    'normalized
    (lambda () (vector-scale vec (/ 1.0 (vector-magnitude vec))))))

(define tolerance 1e-10)

(define (vector-zero? vec)
  (< (vector-magnitude vec) tolerance))

(define (vector-orthogonal? v1 v2)
  (< (abs (vector-dot v1 v2)) tolerance))

(define (vector-parallel? v1 v2)
  (or (vector-zero? v1)
      (vector-zero? v2)
      (vector-zero? (vector-sub (vector-normalize v1) (vector-normalize v2)))
      (vector-zero? (vector-add (vector-normalize v1) (vector-normalize v2)))))
