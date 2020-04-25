(ns tetris-clj.shapes)

(def i [[1 1 1 1]])
(def o [[1 1] [1 1]])
(def l [[0 0 0 1] [1 1 1 1]])
(def j [[1 1 1 1] [0 0 0 1]])
(def z [[1 1 0] [0 1 1]])
(def s [[0 1 1] [1 1 0]])
(def t [[0 1 0] [1 1 1]])


(defn height [rows]
  (count rows))

(defn width [rows]
  (count (first rows)))

(defn transpose [m]
  (apply mapv vector m))

(defn rotate-90-cc [piece]
  (reverse (transpose piece)))