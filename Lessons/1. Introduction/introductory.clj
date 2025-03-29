(ns introductory
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

(defn gibibytes->bytes
  [x]
  (* x
     1024
     1024
     1024))

(deftest test-gibibytes->bytes
  (is (= 0 (gibibytes->bytes 0))))

(gibibytes->bytes 5)