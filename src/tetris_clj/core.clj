(ns tetris-clj.core)
(require '[tetris-clj.shapes :as s])
(require '[tetris-clj.term :as t])

(defn init-board
  [width height val]
  (vec (repeat height (vec (repeat width val)))))

(def board-width 40)
(def board-height 20)

(defn xyth
  [matrix x y]
  (nth (nth matrix y) x))

(defn interferes
  "returns true if the piece at (x,y) interferes with the board"
  [board piece x y]
  (let [bw (s/width board)
        bh (s/height board)
        w (s/width piece)
        h (s/height piece)]
    (cond
      (< x 0) true
      (< y 0) true
      (>= (+ x w) bw) true
      (>= (+ y h) bh) true
      (every?
        empty?
        (keep-indexed
          (fn
            [dy row]
            (keep-indexed
              (fn
                [dx pixel]
                (if (> (+ pixel (xyth board (+ x dx) (+ y dy))) 1) pixel))
              row))
          piece)) false
      :else true)))

(defn take-between [n m coll]
  (cond
    (< m n) []
    :else (->> coll (take n) (take (- m n)))))

(defn zero-mat [w h]
  (vec (repeat h (vec (repeat w 0)))))

(defn grow-mat
  [piece x y bw bh]
  (let [w (s/width piece)
        h (s/height piece)]
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

(defn write-piece-to-board [board piece x y]
  (let [bw (s/width board)
        bh (s/height board)
        grown-piece (grow-mat piece x y bw bh)]
    (add-mat board grown-piece)))

(defn start-game
  []
  (t/start)
  (with-local-vars
    [getch nil                                              ; input from user
     frame 0                                                ; frame counter
     board (init-board board-width board-height 0)          ; the board
     current-piece s/j                                      ; the current piece
     current-x 10                                           ; piece x position
     current-y 1]                                           ; piece y position
    (while (not (= @getch \q))
      (t/cls)
      (t/render-piece @board 0 0)
      (t/render-piece @current-piece @current-x @current-y)
      (Thread/sleep 100)
      (var-set frame (inc @frame))
      (if (= 0 (mod @frame 10))
        (if (interferes @board @current-piece @current-x (inc @current-y))
          (do
            (var-set board (write-piece-to-board @board @current-piece @current-x @current-y))
            (var-set current-y 1))
          (var-set current-y (inc @current-y))))
      (var-set getch (t/get-key))
      (cond
        (= @getch :right) (var-set current-x (if (interferes @board @current-piece (inc @current-x) @current-y) @current-x (inc @current-x)))
        (= @getch :left) (var-set current-x (if (interferes @board @current-piece (dec @current-x) @current-y) @current-x (dec @current-x)))
        (= @getch :up) (var-set current-piece (s/rotate-90-cc @current-piece))
        ))
    (t/stop)))

