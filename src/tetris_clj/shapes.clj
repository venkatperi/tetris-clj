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
  "gets a piece, in random"
  (->> pieces count rand-int (nth pieces)))

(defn height [mat]
  "returns the height of a 2d matrix"
  (count mat))

(defn width [mat]
  "returns the width of a 2d matrix"
  (-> mat first count))

(defn transpose [m]
  "transpose a 2d matrix"
  (apply mapv vector m))

(defn rotate-90-cc [piece]
  "Rotate piece 90 degrees counter clockwise"
  (-> piece transpose reverse))

(defn xyth
  "returns the element at (x,y)"
  [matrix x y]
  (-> matrix (nth y) (nth x)))

(defn zero-mat
  "returns a zero row(w) or matrix(w,h)"
  ([w]
   (->> 0 (repeat w) vec))
  ([w h]
   (->> (zero-mat w) (repeat h) vec)))

(defn grow-mat
  "'grow' matrix `piece` to size bw x bh placing it at (x,y) and zeroing out rest"
  [piece x y bw bh]
  (let [w (width piece)
        h (height piece)]
    (concat
      (zero-mat bw y)
      (map
        (fn [row]
          (concat
            (zero-mat x)
            row
            (zero-mat (- bw (+ x w))))) piece)
      (zero-mat bw (- bh (+ y h))))))

(defn add-mat [m1 m2]
  "adds two 2d matrices"
  (mapv #(mapv + %1 %2) m1 m2))


(defn sub-mat
  "return a sub-matrix of size (w,h) from location (x,y) "
  [matrix w h x y]
  (map #(->> % (drop x) (take w)) (->> matrix (drop y) (take h)))
  )

