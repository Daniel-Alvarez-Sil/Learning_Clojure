;----------------------------------------------------------
; Problem Set #3: More Repetitions
; Date: March 14, 2025.
; Authors:
;          A01800182 Daniel Alvarez Sil
;          A01800766 Jan Francisco Cerón García
;----------------------------------------------------------

(ns more-repetitions
  (:require [clojure.test :refer [deftest is run-tests]]))

; Problem 1
(defn expand
  "Returns a list where the 1st element of s appears one time, the second appears twice, and so on. "
  [s]
  (mapcat repeat
          (range 1 (inc (count s)))
          s))

; Problem 2
(defn insert
  "Returns the list in ascending order, s, but with n inserted in its corresponding place."
  [n s]
  (let [[a b] (split-with #(< % n) s)]
    (concat a [n] b)))

; Problem  3
(defn insertion-sort
  "Returns the unordered list s but in ascending order, using the insertionsort algorithm."
  [s]
  (reduce #(insert %2 %1) () s))

; Problem 4
(defn rotate-left
  "Returns the sequence s after performing n rotations.
    If n is positive, rotates to the left
    Else, rotate right"
  [n s]
  (let [l (count s)]
    (if (= l 0)
      ()
      (take l (drop (+ l (mod n l)) (cycle s)))
      )
    )
  )

; Problem 5
(defn binary
  "Returns the binary representation (digits in a list) of the number n."
  [n]
  (loop [new-n   n
         result  ()]
    (if (zero? new-n)
      result
      (recur (quot new-n 2)
             (cons (rem new-n 2) result)))))

; Problem 6
(defn prime-factors
  "Returns a list, in ascending order, of all the prime factors of n."
  [n]
  (loop [new-n   n
         r       []
         divisor 2]
    (cond
      (= new-n 1)
        (concat r ())
      (zero? (rem new-n divisor))
        (recur  (quot new-n divisor)
                (conj r divisor)
                divisor)
      :else
        (recur  new-n
                r
                (inc divisor)))))

; Problem 7
(defn gcd
  "Returns the greatest common divisor of x and y."
  [x y]
  (loop [a x
         b y]
    (cond
      (= a 0)
        b
      :else
        (recur (mod b a) a))))

; Problem 8
(defn insert-everywhere
  "Returns a list of lists with all the possible insertions of x within the list s."
  [x s]
  (loop [b []
         a s
         res []]
      (if (= a [])
        (conj res (conj b x))
        (recur (conj b (first a)) (rest a) (conj res (concat b (cons x a))))
      )
    )
  )

; Problem 9
(defn contains-all-digits?
  "Returns true if n contains all the ten decimal digits (0-9), false otherwise."
  [n]
  (loop
    [bools [0 0 0 0 0 0 0 0 0 0]
     new-n n]
    (if (= new-n 0)
      (if (= (reduce + bools) 10)
        true
        false)
      (recur (assoc bools (mod new-n 10) 1) (quot new-n 10)))
  )
)

; Problem 10
(defn pack
  "Returns a list with sublists that group consecutive repeated elements in s."
  [s]
  (partition-by identity s))

; Problem 11
(defn compress
  "Returns a list with the same elements of s but only once are consecutive repeated elements showed."
  [s]
  (map first (partition-by  identity s)))

; Problem 12
(defn encode
  "Returns a list in which consecutive repeated elements are grouped and represented by the following format:
    [number of repetitions, element]"
  [s]
  (map #(vector (count %) (first %)) (partition-by identity s)))

; Problem 13
(defn encode-modified
  "Returns a list in which:
    If an element appears without consecutive repetitions, represent it as itself.
    Else, the same logic applies as in the previous function."
  [s]
  (map #(if (= 1 (count %)) (first %)
                            (vector (count %) (first %)))
       (partition-by identity s))) ;;

; Problem 14
(defn decode
  "Given an encoded sequence following the format of the previous function, returns the decoded sequence."
  [s]
  (mapcat #(if (coll? %)
             (repeat (first %) (second %))
             (list %))
          s))


(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-insertion-sort
  (is (= () (insertion-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9)
         (insertion-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (insertion-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (insertion-sort '(5 5 5 1 5 5 5)))))

(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))


(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-contains-all-digits?
  (is (contains-all-digits? 1023456789))
  (is (contains-all-digits? 5897230146))
  (is (contains-all-digits? 10123485679))
  (is (contains-all-digits?
        1223334444555566666677777778888888889999999990))
  (is (not (contains-all-digits? 1236)))
  (is (not (contains-all-digits? 1112223334455)))
  (is (not (contains-all-digits? -587230462413578)))
  (is (not (contains-all-digits?
             -122333444455556666667777777888888888999999999))))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)