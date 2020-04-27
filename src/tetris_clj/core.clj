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
      (let [sub (s/submatrix board w h x y)
            sum (s/add-mat sub piece)]
        (s/every2d? #(<= % 1) sum)) false
      :else true)))

(defn write-piece-to-board [board piece x y]
  "writes the given piece to the board at location (x,y) and returns the new board"
  (let [bw (s/width board)
        bh (s/height board)
        grown-piece (s/grow-mat piece x y bw bh)]
    (s/add-mat board grown-piece)))

(defn drop-piece
  "determines the highest 'y' to which the given piece can drop without
  interfering with the board"
  [board piece x y]
  (let [bh (s/height board)
        h (s/height piece)]
    (first
      (drop-while
        #(interferes board piece x %)
        (map (partial - bh) (range h bh))))))


(defn full-row?
  "true if all squares on the given row are set (> 0)"
  [row]
  (every? #(> % 0) row))

(defn clear-lines
  "clears full rows from the board and top-pads zero rows to preserve dimensions"
  [board]
  (let [bw (s/width board)
        bh (s/height board)
        cleared (filter #(-> % full-row? not) board)
        h (s/height cleared)]
    (concat (s/zero-mat bw (- bh h)) cleared)))

(def board-width 10)
(def board-height 20)

(defn start-game
  "start the game"
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
          (= :up getch) (let [rotated (s/rotate-90-cc @current-piece)]
                          (if (not (interferes @board rotated @current-x @current-y))
                            (var-set current-piece rotated))))
        (if (not (or (= \q getch) (interferes @board @current-piece @current-x @current-y)))
          (recur (inc frame))))))

  (t/stop))

