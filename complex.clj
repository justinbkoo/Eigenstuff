;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      
;                    Complex Numbers for Clojure                       
;                            Version 1.0                               
;                                                                      
;                            Justin Koo                                
;                         25 November 2016                             
;                                                                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      
;    View the readme for more information and upcoming additions.      
;                                                                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;   ERROR: throws an exception with message m.

(def error
 (fn [m]
  (Exception. m)))

;   COMPLEX-NUMBER: returns a complex number a + bi in the form ['complex a b].

(def complex-number
 (fn
  ([real] (complex-number real 0))
  ([real complex]
   (vector 'complex (double real) (double complex)))))

;   COMPLEX?: tests if a form is a complex number.
   
(def complex?
 (fn [complex]
  (and
   (vector? complex)
   (= (count complex) 3)
   (= (get complex 0) 'complex)
   (number? (get complex 1))
   (number? (get complex 2)))))

;   GET-REAL: returns the real part of a complex number.
   
(def get-real
 (fn [complex]
  (if (complex? complex)
  (get complex 1)
  (error "Invalid complex number"))))
  
;   GET-IMAGINARY: returns the imaginary part of a complex number.  
  
(def get-imaginary
 (fn [complex]
  (if (complex? complex)
  (get complex 2)
  (error "Invalid complex number"))))
  
;   COMPLEX-IS-ZERO: tests if a complex number is zero

(def complex-is-zero?
 (fn [complex]
  (if (complex? complex)
   (and (zero? (get-real complex)) (zero? (get-imaginary complex)))
   (error "Invalid complex number"))))

;   C-ADD: returns the sum of two complex numbers.
  
(def c-add
 (fn
  ([c1] c1) 
  ([c1 c2]
   (if
    (and (complex? c1) (complex? c2))
    (complex-number
     (+ (get-real c1) (get-real c2))
     (+ (get-imaginary c1) (get-imaginary c2)))
    (error "Invalid complex number(s)")))))

;   C-SUBTRACT: returns the difference of two complex numbers or the negation of one complex number
   
(def c-subtract
 (fn
  ([c1] (c-subtract (complex-number 0 0) c1)) 
  ([c1 c2]
   (if
    (and (complex? c1) (complex? c2))
    (complex-number
     (- (get-real c1) (get-real c2))
     (- (get-imaginary c1) (get-imaginary c2)))
    (error "Invalid complex number(s)")))))

;   C-MULTIPLY: returns the product of two complex numbers.
   
(def c-multiply
 (fn [c1 c2]
  (if
   (and (complex? c1) (complex? c2))
   (complex-number
    (+ 
     (* (get-real c1) (get-real c2))
     (- (* (get-imaginary c1) (get-imaginary c2))))
    (+
     (* (get-real c1) (get-imaginary c2))
     (* (get-real c2) (get-imaginary c1))))
   (error "Invalid complex number(s)"))))

;   C-DIVIDE: returns the quotient of two complex numbers.

(def c-divide
 (fn [c1 c2]
  (if
   (and (complex? c1) (complex? c2))
   (let
    [a (get-real c1)
     b (get-imaginary c1)
     c (get-real c2)
     d (get-imaginary c2)]
    (complex-number
     (/ (+ (* a c) (* b d)) (+ (* c c) (* d d)))
     (/ (- (* c b) (* a d)) (+ (* c c) (* d d)))))
   (error "Invalid complex number(s)"))))

;   C-POWER: returns the result of a complex number raised to some power.

(def c-power
 (fn [complex power]
  (if
   (complex? complex)
   (if
    (< power 0)
    (c-divide (complex-number 1 0) (c-power complex (- power)))
    (if
     (and (= power 0) (complex-is-zero? complex))
     (error "Operation results in an indeterminant form")
     (if
      (= power 0)
      1
      (if
       (= power 1)
       complex
       (c-multiply complex (c-power complex (- power 1)))))))
   (error "Invalid complex number"))))
  

