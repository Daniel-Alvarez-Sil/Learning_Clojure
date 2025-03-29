;----------------------------------------------------------
; Problem Set #1: Introductory Exercises
; Date: February 21, 2025.
; Authors:
;          A01800182 Daniel Alvarez Sil
;          A01800766 Jan Francisco Cerón García
;----------------------------------------------------------

(ns introductory
  (:require [clojure.test :refer [is run-tests deftest]])
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

; Problem 1
(defn gibibytes->bytes
  "Receives a number of gibibytes and returns the conversion to bytes"
  [x]
  (* x 1024 1024 1024))

; Problem 2
(defn fahrenheit->celsius
  "Receives a temperature in degrees Fahrenheit and returns the conversion to degrees Celsius"
  [f]
  (/ (* 5.0 (- f 32.0))9.0))

; Problem 3
(defn sign
  "Receives an integer returns the following:
    if 0    ->  0
    if > 0  ->  1
    if < 0  -> -1"
  [n]
   (if (< n 0)
     -1
     (if (> n 0)
       1
       0)))

; Problem 4
(defn roots
  "Receives three coefficients and places them in the quadratic equation.
   Returns the roots of said equation. "
  [a b c]
  (let [d (- b)
        e (sqrt (- (* b b)
                   (* 4 a c)))
        f (* 2 a)
        x1 (/ (+ d e)
              f)
        x2 (/ (- d e)
              f)]
    [x1 x2]))

; Problem 5
(defn bmi
  "Receives weight and height as parameters.
   Returns the body mass index based on those two parameters"
  [weight height]
  (let  [value (/ weight
                  (* height height))]
    (cond
      (< value 20) 'underweight
      (< value 25) 'normal
      (< value 30) 'obese1
      (< value 40) 'obese2
      :else 'obese3)))

; Problem 6
(defn type-of-triangle
  "Receives the lengths of the three sides of a triangles.
   Returns the type of triangle: equilateral, isosceles, or scalene."
  [a b c]
  (cond
    (= a b c) 'equilateral
    (or  (= a b) (= b c) (= a c)) 'isosceles
    :else 'scalene)
  )

(deftest test-gibibytes->bytes
  (is (= 0 (gibibytes->bytes 0)))
  (is (= 1073741824 (gibibytes->bytes 1)))
  (is (= 5368709120 (gibibytes->bytes 5)))
  (is (= 26415122612224 (gibibytes->bytes 24601))))

(deftest test-fahrenheit->celsius
  (is (= 100.0 (fahrenheit->celsius 212.0)))
  (is (= 0.0 (fahrenheit->celsius 32.0)))
  (is (= -40.0 (fahrenheit->celsius -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))

(deftest test-type-of-triangle
  (is (= 'equilateral (type-of-triangle 3 3 3)))
  (is (= 'equilateral (type-of-triangle 4.2 4.2 4.2)))
  (is (= 'isosceles (type-of-triangle 4 4 3)))
  (is (= 'isosceles (type-of-triangle 4 3 4)))
  (is (= 'isosceles (type-of-triangle 3 4 4)))
  (is (= 'scalene (type-of-triangle 1 2 3)))
  (is (= 'scalene (type-of-triangle 7.1 6.4 9.2))))

(run-tests)