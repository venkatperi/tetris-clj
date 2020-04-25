(ns tetris-clj.core)
(require '[lanterna.terminal :as t])
(require '[tetris-clj.shapes :as s])

(def term (t/get-terminal :swing))

(defn cls []
  (t/clear term))

(defn set-pixel [ch x y]
  (cond
    (= ch 1) (t/put-string term "#" x y)))

(defn render-piece [piece x y]
  (println
    (map-indexed
      (fn [dy row]
        (map-indexed
          (fn [dx pixel]
            (set-pixel pixel (+ dx x) (+ dy y))) row)) piece)))

(defn draw-piece [piece x y]
  (println piece x y))

(def board-width 40)
(def board-height 20)

(defn start-game []
  (t/start term)
  (with-local-vars
    [getch nil
     frame 0
     current-piece s/j
     current-x 10
     current-y 1]
    (while (not (= @getch \q))
      (cls)
      (render-piece @current-piece @current-x @current-y)
      (Thread/sleep 100)
      (var-set frame (inc @frame))
      (if (= 0 (mod @frame 10)) (var-set current-y (inc @current-y)))
      (var-set getch (t/get-key term))
      (cond
        (= @getch :right) (var-set current-x (inc @current-x))
        (= @getch :left) (var-set current-x (dec @current-x))
        (= @getch :up) (var-set current-piece (s/rotate-90-cc @current-piece))
        ))
    (t/stop term)))
