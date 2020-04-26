(ns tetris-clj.shapes)

(def i [[1 1 1 1]])
(def o [[1 1] [1 1]])
(def l [[0 0 0 1] [1 1 1 1]])
(def j [[1 1 1 1] [0 0 0 1]])
(def z [[1 1 0] [0 1 1]])
(def s [[0 1 1] [1 1 0]])
(def t [[0 1 0] [1 1 1]])

(def pieces [i o l j z s t])

(defn get-random-piece []
  (let [total (count pieces)]
    (nth pieces (rand-int total) )))

(defn height [rows]
  (count rows))

(defn width [rows]
  (count (first rows)))

(defn transpose [m]
  (apply mapv vector m))

(defn rotate-90-cc [piece]
  (reverse (transpose piece)))

(defn xyth
  [matrix x y]
  (nth (nth matrix y) x))

(defn zero-mat [w h]
  (->> (repeat w 0) vec (repeat h) vec))

(defn grow-mat
  [piece x y bw bh]
  (let [w (width piece)
        h (height piece)]
    (concat
      (zero-mat bw y)
      (map
        (fn [row]
          (concat
            (vec (repeat x 0))
            row
            (vec (repeat (- bw (+ x w)) 0)))) piece)
      (zero-mat bw (- bh (+ y h))))))

(defn add-mat [m1 m2]
  (mapv #(mapv + %1 %2) m1 m2))



