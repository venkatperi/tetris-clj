(ns tetris-clj.core)
(require '[tetris-clj.shapes :as s])
(require '[tetris-clj.term :as t])

(defn init-board
  [width height val]
  (vec (repeat height (vec (repeat width val)))))

(def board-width 40)
(def board-height 20)
(def board (init-board board-width board-height 0))

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



(defn start-game
  []
  (t/start)
  (with-local-vars
    [getch nil                                              ; input from user
     frame 0                                                ; frame counter
     current-piece s/j                                      ; the current piece
     current-x 10                                           ; piece x position
     current-y 1]                                           ; piece y position
    (while (not (= @getch \q))
      (t/cls)
      (t/render-piece @current-piece @current-x @current-y)
      (Thread/sleep 100)
      (var-set frame (inc @frame))
      (if (= 0 (mod @frame 10)) (var-set current-y (inc @current-y)))
      (var-set getch (t/get-key))
      (cond
        (= @getch :right) (var-set current-x (if (interferes board @current-piece (inc @current-x) @current-y) @current-x (inc @current-x)))
        (= @getch :left) (var-set current-x (if (interferes board @current-piece (dec @current-x) @current-y) @current-x (dec @current-x)))
        (= @getch :up) (var-set current-piece (s/rotate-90-cc @current-piece))
        ))
    (t/stop)))
