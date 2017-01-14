;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                  Eigenstuff calculator for Clojure
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

;   macro QRATS: expands into code that computes the roots of a quadratic equation.

(defmacro qrats [s a b c]
 `(let [s# ~s a# ~a b# ~b c# ~c]
   (/ (s# (- b#) (Math/sqrt (- (* b# b#) (* 4 (* a# c#))))) (* 2 a#))))

;   macro QUADRATIC-COEFFICIENTS: expands into code that returns a vector of coefficients in the quadratic equation formed by the determinant of a matrix.
        
(defmacro quadratic-coefficients [matrix]
 `(let [matrix# ~matrix]
   (vector
    1
    (- 
     (- (get (get matrix# 0) 0))
     (get (get matrix# 1) 1))
    (- 
     (* (get (get matrix# 0) 0) (get (get matrix# 1) 1))
     (* (get (get matrix# 1) 0) (get (get matrix# 0) 1))))))

;   ZERO-VECTOR?: tests if a vector is a zero-vector.

(def zero-vector? 
 (fn [vect]
  (loop
   [index 0
    finish (count vect)]
   (if
    (= index finish)
    true
    (if
     (zero? (get vect index))
     (recur (+ index 1) finish)
     false)))))

;   ERROR: throws an exception with message m.
    
(def error
 (fn [m]
  (throw (Exception. m))))

;   NUMBER-VECTOR?: tests if a vector is a vector of numbers.

(def number-vector?
 (fn [vect]
  (loop
   [index 0
    length (count vect)]
   (if
    (= index length)
    true
    (if
     (number? (get vect index))
     (recur (+ index 1) length)
     false)))))

;   N-BY-N?: tests if a matrix is a square matrix.

(def n-by-n?
 (fn [matrix]
  (if
   (and (vector? matrix))
   (loop
    [row# 0
     row (first matrix)
     rows (count matrix)]
    (if
     (= row# rows)
     true
     (if
      (and (vector? row) (= (count row) rows) (number-vector? row))
      (recur (+ row# 1) (first (rest matrix)) rows)
      false)))
    false)))
    
;   EIGEN-MAP: returns a hash-map with the eigenvalues of a matrix as the keys and the eigenvectors as values.

(def eigen-map
 (fn [matrix]
  (if
   (not (n-by-n? matrix))
   (error "Not a valid square matrix")
   (letfn 
    [
     (eigenvalues [matrix]
      (list
       (qrats
        +
        (get (quadratic-coefficients matrix) 0) 
        (get (quadratic-coefficients matrix) 1) 
        (get (quadratic-coefficients matrix) 2))
       (qrats
        - 
        (get (quadratic-coefficients matrix) 0)
        (get (quadratic-coefficients matrix) 1)
        (get (quadratic-coefficients matrix) 2))))
     (eigenvectors [matrix]
      (let
       [vec1 (vector 
              (- (get (get matrix 0) 0) (first (eigenvalues matrix)))
              (get (get matrix 0) 1))
        vec2 (vector (- (get (get matrix 0) 0) (second (eigenvalues matrix)))
              (get (get matrix 0) 1))]
      (list
       (if
        (not (zero-vector? vec1))
        (vector
         [(get (get matrix 0) 1)]
         [(- (- (get (get matrix 0) 0) (first (eigenvalues matrix))))])
        (vector
         [(- (get (get matrix 1) 1) (first (eigenvalues matrix)))]
         [(- (get (get matrix 1) 0))]))
       (if
        (not (zero-vector? vec2))
        (vector
         [(get (get matrix 0) 1)]
         [(- (- (get (get matrix 0) 0) (second (eigenvalues matrix))))])
        (vector
         [(- (get (get matrix 1) 1) (second (eigenvalues matrix)))]
         [(- (get (get matrix 1) 0))])))))]
    (assoc
     (hash-map)
     (first (eigenvalues matrix))
     (first (eigenvectors matrix))
     (second (eigenvalues matrix))
     (second (eigenvectors matrix)))))))
     
;   DIAGONIZABLE?: tests if a matrix is diagonizable.
     
(def diagonizable?
 (fn [matrix]
  (let
   [eigenvalues (keys (eigen-map matrix))]
   (if
    (= (count eigenvalues) 2)
    true
    false))))

;   DIAGONALIZE: prints matrices A, P and D such that AP = PD.

(def diagonalize
 (fn [matrix]
  (if
   (not (diagonizable? matrix))
   (error "Matrix cannot be diagonalized")
   (letfn
    [
     (make-P [matrix]
      (let
       [mapping (eigen-map matrix)
        eigenvalues (keys mapping)
        first-vector (get mapping (first eigenvalues))
        second-vector (get mapping (second eigenvalues))
        a (get (get first-vector 0) 0)
        b (get (get second-vector 0) 0)
        c (get (get first-vector 1) 0)
        d (get (get second-vector 1) 0)]
      (vector [a b] [c d])))
     (make-D [matrix]
      (let
       [mapping (eigen-map matrix)
        eigenvalues (keys mapping)]
      (vector [(first eigenvalues) 0] [0 (second eigenvalues)])))]
     (let
      [A matrix
       P (make-P matrix)
       D (make-D matrix)]
      (do
       (println)
       (println "A =" A)
       (println "P =" P)
       (println "D =" D)
       (println)))))))
  
