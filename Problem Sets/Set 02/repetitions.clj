;----------------------------------------------------------
; Problem Set #2: Repetitions
; Date: March 3, 2025.
; Authors:
;          A01800182 Daniel Alvarez Sil
;          A01800766 Jan Francisco Cerón García
;----------------------------------------------------------

(ns repetitions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]])
  (:require [clojure.algo.generic.math-functions
             :refer [sqr approx=]]))

; Problem 1
(defn enlist
  [s]
  (map list s))
"Map nos ayuda a aplicar la funcion  dada a acada elemento  de s para despues aplicar list y enlistar todo el conjunto de elementos en uno solo"
; Problem 2
(defn positives
  [s]
  (filter pos? s))
"Se usa filter para verificar que elementos de la secuencia son positivos y nos devuelve una secuencia de dichos numeros."

; Problem 3
(defn add-squares
  [s]
  (reduce + (map sqr s)))
"Se usa reduce para devolver una operacion de suma una vez aplicamos map para obtener la raiz cuadrada de cada elemento del conjunto dandonos una suma de cuadrados"
; Problem 4
(defn duplicate
  [s]
  (interleave s s))
"Interleave devuelve un conjunto de valores intercalados que al ser la misma secuencia seria como si fueran duplicados"
; Problem 5
(defn aux
  [[a b]]
  [b (+' a b)])
"En la función auxiliar se define el funcionameinto de la sucesion de fobonacci "
(defn fib
  [n]
  (first (first (drop n (iterate aux [0 1])))))
"En la función principal se itera hasta la posicion dada (la cual es n) la sucesion de fibonacci declarado en la  funcion auxiliar"
; Problem 6
(defn aux2
  [[a b]]
  [(*' a b) b])
(defn pow
  [a b]
  (if (= b 0)
    1
    (first (first (drop (- b 1) (iterate aux2 [a a]))))))
"Se tiene una funcion auxiliar para generar los valores de la potencia y despues en la funcion principal se itera deolviendo un nuevo par en donde a se multiplica por b para al final clacular a elevado a la b"
; Problem 7
(defn only-symbols?
  [s]
  (every? symbol? s))
"symbol? nos indica si algun objeto es un simbolo y lo que every? hace es verificar cada elemento de la secuencia dando com resultado una liena que se lee si todos los elementos de s son
 un symbolo devuelve true"

; Problem 8
(defn aux3
  [s]
  [(second s) (first s)])
"Esta función auxiliar nos cambia el orden de la seucencia primero el segundo elemento y vice versa"
(defn invert-pairs
  [s]
  (map aux3 s))
"se usa map para verificar todos los elementos de la secuenca y se aplica la funcion aux3 declarada y explicada anteriormente"
; Problem 9
(defn replic
  [n s]
  (if (= s [])
    ()
    (concat (repeat n (first s)) (replic n (rest s)))))
"Se usa una función recursiva ya que primero se obtine el primer elemento de la secuencia y lo repite n veces para despues seguirse con el sigueinte elemento recursivamente para al final usar concat para obtener la secuencia completa"
; Problem 10
(defn dot-product
  [a b]
  (reduce + (map * a b)))
"Reduce nos devuelve una operacion que en este caso es la suma de aplicar map a la multiplicacion de a por b dandonos el producto punto de dicha secuencia"
; Problem 11
(defn average
  [s]
  (if (= s [])
    nil
    (/ (reduce + s) (count s))))

"En este codigo se implementa el calculo promedio de una secuencia de valores, primero tomando en cuenta si s esta vacio si no es el caso, se hace la division entre el numero de elementos de s (count s ) y el valor de la suma de todos los elementos con reduce +"
; Problem 12
(defn standard-deviation
  [s]
  (if (= s [])
    nil
    (let [xbar (average s)
          n (count s)
          valor (reduce + (map #(* (- % xbar) (- % xbar)) s))]
      (sqrt (/ valor n)))))
"Aqui se considera primero si s tiene algun valor, si ese es el caso con xbar averge s se calcula el promedio de s, despues se cuenta el numero de elementos de s, para
que en la penultima linea de codigo sumar todos los valores de la media de las diferencias de cada numero en s dandonos la varianza y a ese valor sacandole
raiz cuadrada para obtener la desviacion estandar "



(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '((1) (2) (3) (4)) (enlist [1 2 3 4])))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

(deftest test-positives
  (is (= () (positives ())))
  (is (= () (positives [-4 -1 -10 -13 -5])))
  (is (= [3 6] (positives [-4 3 -1 -10 -13 6 -5])))
  (is (= [4 3 1 10 13 6 5] (positives [4 3 1 10 13 6 5]))))

(deftest test-add-squares
  (is (= 0 (add-squares [])))
  (is (= 25 (add-squares [5])))
  (is (= 30 (add-squares [2 4 1 3])))
  (is (= 385 (add-squares [1 2 3 4 5 6 7 8 9 10]))))

(deftest test-duplicate
  (is (= [1 1 2 2 3 3 4 4 5 5]
         (duplicate [1 2 3 4 5])))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))

(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
          987 1597 2584 4181 6765]
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22)))
  (is (= 1267650600228229401496703205376N (pow 2 100))))

(deftest test-only-symbols?
  (is (= true (only-symbols? [])))
  (is (= true (only-symbols? '(a))))
  (is (= true (only-symbols? '(a b c d e))))
  (is (= false (only-symbols? '(a b c d 42 e))))
  (is (= false (only-symbols? '(42 a b c))))
  (is (= false (only-symbols? [4 8 15 16 23 42]))))
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([y x]) (invert-pairs '([x y]))))
  (is (= '([1 a][2 a][1 b][2 b])
         (invert-pairs '([a 1][a 2][b 1][b 2]))))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

(deftest test-dot-product
  (is (= 0 (dot-product [] [])))
  (is (= 42 (dot-product [6] [7])))
  (is (= 32 (dot-product [1 2 3] [4 5 6])))
  (is (= 21.45 (dot-product [1.3 3.4 5.7 9.5 10.4]
                            [-4.5 3.0 1.5 0.9 0.0]))))

(deftest test-average
  (is (nil? (average [])))
  (is (= 4
         (average [4])))
  (is (= 3
         (average [5 6 1 6 0 1 2])))
  (is (= 2.5
         (average [1.7 4.5 0.0 2.0 3.4 5.0 2.5 2.2 1.2]))))

(deftest test-standard-deviation
  (is (nil? (standard-deviation [])))
  (is (approx= 1.87
               (standard-deviation [6 2 3 1])
               0.01))
  (is (approx= 12.3153
               (standard-deviation [4 8 15 16 23 42])
               0.0001))
  (is (approx= 7.07106
               (standard-deviation [110 105 90 100 95])
               0.00001))
  (is (approx= 2.983
               (standard-deviation [9 2 5 4 12 7 8 11
                                    9 3 7 4 12 5 4 10
                                    9 6 9 4])
               0.001)))
(deftest test-replic
  (is (= () (replic 7 [])))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4]
         (replic 4 [1 2 3 4]))))
(run-tests)