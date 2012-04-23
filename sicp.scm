(define (identity x) x)

(define (abs x)
  	(if (< x 0) (- x) x))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (double x) (+ x x))

(define (half x) (/ x 2))

(define (avg x y)
  	(/ (+ x y) 2))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (and (not (= n 1)) (= n (smallest-divisor n))))



(define (goodenough? guess x)
  	(< (abs(- guess x)) 0.000001))
(define (cube_rt target) 
	(define (cube_rt_itr approx)
  		(if (goodenough? (cube approx) target)
		  	approx
			(cube_rt_itr (/
				       (+
					 (/ target (square approx))
					 (* 2 approx))
				       3))))
	(cube_rt_itr 1.0)
	)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;Exercise 1.11
(define (f n)
    (if (< n 3)
        n
        (+ 
	  (f (- n 1)) 
	  (* 2 (f (- n 2))) 
	  (* 3 (f (- n 3))))))

(define (f2 n)
    (f2_iter 2 1 0 3 n))

(define (f2_iter a b c count n)
    (if (> count n) 
        a
	(f2_iter  (+ a 
		     (* 2 b) 
		     (* 3 c)) 
		  a b (+ 1 count) n)))

;Exercise 1.12
(define (pasc y x)
    (cond ((or (< y 0) (< x 0) (> x y)) 0)
	  ((= x y) 1)
	  (else (+ (pasc (- y 1) x) 
	    	   (pasc (- y 1) (- x 1))))))

;Exercise 1.16
(define (expnt b n) (expnt_iter b n 1))
(define (expnt_iter b n a)
    (if (= n 0) a
        (if (even? n)
             (expnt_iter  (square b) (/ n 2)  a)
	     (expnt_iter  b (- n 1) (* a b)))))


;Exercise 1.17
(define (mult_r a b)
    (cond ((= b 0) 0)
	  ((= b 1) a)
	  ((even? b) (mult (double a) (half b)))
	  (else (+ a (mult a (- b 1))))))

;Exercise 1.18
(define (mult a b) (mult_iter a b 0))
(define (mult_iter a b c)
    (cond ((= b 0) 0)
	  ((= b 1) (+ a c))
          ((even? b) (mult_iter (double a) (half b) c))
	  (else (mult_iter a (- b 1) (+ a c)))))
	      
;Exercise 1.19
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib2 n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute q'
                   (+ (* 2 p q) (* q q))      ; compute p'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;Exercise 1.22
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes num limit)
    (if (even? num) 
      (search-for-primes (+ 1 num) limit)
      (if (or (< num limit) (= num limit))
        (and (timed-prime-test num)
             (search-for-primes (+ 2 num) limit)))))

;Exercise 1.28
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((= (remainder (square base) m) 1) 0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 3)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-test n) (fast-prime? n (- times 1)))
        (else false)))

;Exercise 1.29
(define (inc x) (+ x 1))

(define (sum-old term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-old term (next a) next b))))

(define (cube-sum a b)
  (sum cube a inc b))

(define (simpson-approx func a b n)
  (define (h) (/ (- b a) n))
  (define (simpson-param k)
    (+ a (* k (h))))
  (define (simpson-term k)
    (cond ((or (= k 0) (= k n)) (func (simpson-param k)))
          ((even? k) (* 4 (func (simpson-param k)))) 
	  (else (* 2 (func (simpson-param k))))))
  (* (/ (h) 3) (sum simpson-term 0 inc n))) 

(define (integral func a b)
  (simpson-approx func a b 1000))

;Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
	(iter (next a) (+ (term a) result))))
    (iter a 0))

;Exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
	(iter (next a) (* (term a) result))))
    (iter a 1))

(define (approx-pie-slow n)
  (define (pie-term x)
    (if (even? x)
          (/ (+ x 2) (+ x 3))
	  (/ (+ x 3) (+ x 2))))
  (* 4.0 (product pie-term 0 inc n)))

;Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-r combiner null-value term (next a) next b))))

(define (new-sum term a next b)
  (accumulate + 0 term a next b))
(define (new-product term a next b)
  (accumulate * 1 term a next b))

(define (new-sum-r term a next b)
  (accumulate-r + 0 term a next b))
(define (new-product-r term a next b)
  (accumulate-r * 1 term a next b))

;Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
	(if (filter a) (iter (next a) (combiner (term a) result))
	               (iter (next a) result))))
  (iter a null-value))

(define (sum-of-evens a b)
  (filtered-accumulate + 0 identity a inc b even?))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-of-relative-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime?))

;Exercise 1.35
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-approx)
  (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1))

;Exercise 1.36
(define (cont-frac nFunc dFunc k)
  (define (cont-frac nFunc dFunc k i)
    (if (> i k) 0
        (/ (nFunc i) 
	   (+ (dFunc i)
	      (cont-frac nFunc dFunc k (+ i 1))))))
  (cont-frac nFunc dFunc k 1))

(define (cont-frac-iter nFunc dFunc k)
  (define (iter nFunc dFunc k total)
    (if (< k 1) total 
        (iter nFunc dFunc (- k 1) (/ (nFunc k) (+ (dFunc k) total)))))
  (iter nFunc dFunc k 0))

;Exercise 1.38
(define (e-approx k-terms)
  (+ 2 
    (cont-frac-iter
      (lambda (k) 1.0)
      (lambda (k) (if (= (remainder k 3) 2)
    		    (* 2 (/ ( + k 1) 3))
		    1))
      k-terms)))

;Exercise 1.39
(define (tan-cf x k)
  (* 1.0
     (cont-frac-iter
       (lambda (k) (if (= k 1) (* x) (* x x -1)))
       (lambda (k) (if (= k 1) 1 (- (* k 2) 1)))
       k)))

;Exercise 1.46
(define (iterative-improve good-enough-func improve-func)
  (define (iter guess)
    (if (good-enough-func guess)
      guess
      (iter (improve-func guess))))
  (lambda (guess)
    (iter guess)))

(define (sqrt-imp-iter x)
  (define sqrt-iter 
    (iterative-improve 
      (lambda (guess) (< (abs (- (square guess) x)) 0.001))
      (lambda (guess) (avg guess (/ x guess)))))
  (sqrt-iter 1.0))

(define (fixed-point-imp-iter func)
  (define iter
    (iterative-improve
      (lambda (guess) (< (abs (- guess (func guess))) 0.00001))
      func))
  (iter 1.0))

(define (e-approx-imp-iter)
  (define (term guess) (expt (+ 1.0 (/ 1.0 guess)) guess))
  (define iter
    (iterative-improve
      (lambda (guess) (< (abs (- (term guess) (term (+ 1 guess)))) 0.000001))
      (lambda (guess) (+ 1 guess))))
  (term (iter 1)))

;Exercise 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (make-point (avg (x-point (start-segment s))
		   (x-point (end-segment s)))
	      (avg (y-point (start-segment s))
		   (y-point (end-segment s)))))

;Exercise 2.3
(define (make-rect p1 p2) (cons p1 p2))
(define (upperleft-rect r) (car r))
(define (lowerright-rect r) (cdr r))
(define (upperright-rect r)
  (make-point (x-point (lowerright-rect r))
	      (y-point (upperleft-rect r))))
(define (lowerleft-rect t) 
  (make-point (x-point (upperleft-rect r)) 
	      (y-point (lowerright-rect r))))
(define (height-rect r)
  (abs (- (y-point (upperleft-rect r))
	  (y-point (lowerleft-rect r)))))
(define (length-rect r)
  (abs (- (x-point (upperleft-rect r))
	  (x-point (upperright-rect r)))))
(define (area-rect r)
  (* (height-rect r) (length-rect r)))
(define (perimeter-rect r)
  (+ (* 2 (height-rect r))
     (* 2 (length-rect r))))

;Exercise 2.5
(define (int-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (int-car c) (get-fact-num c 2))
(define (int-cdr c) (get-fact-num c 3))
(define (get-fact-num product factor)
  (define (iter p count)
    (if (> (remainder p factor) 0)
        count
	(iter (/ p factor) (+ 1 count))))
  (iter product 0))

;Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f(f x)))))
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define (mult a b)
  (lambda (f) (lambda (x) ((a(b f)) x))))

;Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))
(define (negative i) 
  (make-interval (* -1.0 (lower-bound i))
		 (* -1.0 (upper-bound i))))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;Exercise 2.8
(define (sub-interval x y)
  (add-interval x (negative y)))

;Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100.0)))
		 (+ c (* c (/ p 100.0)))))
(define (percent i)
  (* 100.0 (/ (width i) (center i))))

;Exercise 2.17
(define (lastpair x)
  (if (null? (cdr x))
      (car x)
      (lastpair (cdr x))))

;Exercise 2.18
(define (my-reverse x)
  (define (my-reverse-r x rev-x)
    (if (null? x)
      rev-x
      (my-reverse-r (cdr x) 
		    (cons (car x) rev-x))))
  (my-reverse-r x ()))

;Exercise 2.19
(define (cc-new amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc-new amount
                     (except-first-denomination coin-values))
                 (cc-new (- amount
                        (first-denomination coin-values))
                     coin-values)))))
(define (no-more? coins) (null? coins))
(define (except-first-denomination coins) (cdr coins))
(define (first-denomination coins) (car coins))

;Exercise 2.20
(define (matching-parity? x y)
  (if (even? x) (even? y) (odd? y)))

(define (same-parity lead-val . other-vals)
  (define (parity-r lead-val val-list)
    (if (null? val-list) ()
      (if (matching-parity? lead-val (car val-list)) 
        (cons (car val-list) 
	      (parity-r lead-val (cdr val-list))) 
        (parity-r lead-val (cdr val-list)))))
  (parity-r lead-val (cons lead-val other-vals)))

;Exercise 2.21
(define (my-map func l)
  (if (null? l)
    ()
    (cons (func (car l))
	  (my-map func (cdr l)))))

(define (square-list-a l)
  (if (null? l) ()
    (cons (square (car l))
	  (square-list-a (cdr l)))))

(define (square-list-b l)
  (my-map square l))

;Exercise 2.22
(define (my-for-each func l)
  (if (null? l) ()
    (and (func (car l))
         (my-for-each func (cdr l)))))

;Exercise 2.27
(define (deep-reverse x)
  (define (deep-reverse-r x rev-x)
    (if (null? x)
      rev-x
      (deep-reverse-r
	(cdr x)
	(cons (if (pair? (car x))
		(deep-reverse (car x))
		(car x))
	      rev-x))))
  (deep-reverse-r x ()))

;Exercise 2.28
(define (fringe x)
  (if (null? x)
    ()
    (if (pair? (car x))
      (append (fringe (car x)) (fringe (cdr x)))
      (cons (car x) (fringe (cdr x))))))

;Exercise 2.29
(define (make-mobile left right) (list left right))
(define (mobile-left-branch mob) (car mob))
(define (mobile-right-branch mob) (car (cdr mob)))
(define (make-branch len struct) (list len struct))
(define (branch-length b) (car b))
(define (branch-struct b) (car (cdr b)))
(define (is-mobile? x) (pair? (car x)))
(define (total-weight mob-or-bran)
  (if (null? mob-or-bran)
    0
    (if (pair? mob-or-bran)
      (if (is-mobile? mob-or-bran)
        (+ (total-weight (mobile-left-branch mob-or-bran))
	   (total-weight (mobile-right-branch mob-or-bran)))
	(total-weight (branch-struct mob-or-bran)))
      mob-or-bran)))
