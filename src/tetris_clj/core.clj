(ns tetris-clj.core)
(require '[tetris-clj.shapes :as s])
(require '[tetris-clj.term :as t])

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
      (> (+ x w) bw) true
      (> (+ y h) bh) true
      (every?
        empty?
        (keep-indexed
          (fn
            [dy row]
            (keep-indexed
              (fn
                [dx pixel]
                (if (> (+ pixel (s/xyth board (+ x dx) (+ y dy))) 1) pixel))
              row))
          piece)) false
      :else true)))

(defn write-piece-to-board [board piece x y]
  (let [bw (s/width board)
        bh (s/height board)
        grown-piece (s/grow-mat piece x y bw bh)]
    (s/add-mat board grown-piece)))

(defn drop-piece
  [board piece x y]
  (let [bh (s/height board)
        h (s/height piece)]
    (first
      (drop-while
        #(interferes board piece x %)
        (map (partial - bh) (range h bh))))))


(defn full-row?
  [row]
  (every? #(> % 0) row))

(defn clear-lines
  [board]
  (let [bw (s/width board)
        bh (s/height board)
        cleared (filter #(-> % full-row? not) board)
        h (s/height cleared)]
    (concat (s/zero-mat bw (- bh h)) cleared)))

(def board-width 10)
(def board-height 20)

(defn start-game
  []
  (t/start)
  (with-local-vars
    [board (s/zero-mat board-width board-height)            ; the board
     current-piece (s/get-random-piece)                     ; the current piece
     current-x (/ (- board-width 2) 2)                      ; piece x position
     current-y 0]                                           ; piece y position
    (loop [frame 1]
      (t/cls)
      (t/render-piece @board 0 0)
      (t/render-piece @current-piece @current-x @current-y)
      (Thread/sleep 100)

      (if (= 0 (mod frame 10))
        (if (interferes @board @current-piece @current-x (inc @current-y))
          (do
            (var-set board (-> @board (write-piece-to-board @current-piece @current-x @current-y) clear-lines))
            (var-set current-y 0)
            (var-set current-x (/ (- board-width 2) 2))
            (var-set current-piece (s/get-random-piece)))
          (var-set current-y (inc @current-y))))

      (let [getch (t/get-key)]
        (cond
          (= :right getch) (var-set current-x (if (interferes @board @current-piece (inc @current-x) @current-y) @current-x (inc @current-x)))
          (= :left getch) (var-set current-x (if (interferes @board @current-piece (dec @current-x) @current-y) @current-x (dec @current-x)))
          (= \  getch) (var-set current-y (drop-piece @board @current-piece @current-x @current-y))
          (= :up getch) (var-set current-piece (s/rotate-90-cc @current-piece)))
        (if (not (= \q getch))
          (recur (inc frame))))))

  (t/stop))

