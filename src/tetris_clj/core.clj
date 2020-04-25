(ns tetris-clj.core)
(require '[tetris-clj.shapes :as s])
(require '[tetris-clj.term :as t])

(def board-width 40)
(def board-height 20)

(defn start-game []
  (t/start)
  (with-local-vars
    [getch nil
     frame 0
     current-piece s/j
     current-x 10
     current-y 1]
    (while (not (= @getch \q))
      (t/cls)
      (t/render-piece @current-piece @current-x @current-y)
      (Thread/sleep 100)
      (var-set frame (inc @frame))
      (if (= 0 (mod @frame 10)) (var-set current-y (inc @current-y)))
      (var-set getch (t/get-key))
      (cond
        (= @getch :right) (var-set current-x (inc @current-x))
        (= @getch :left) (var-set current-x (dec @current-x))
        (= @getch :up) (var-set current-piece (s/rotate-90-cc @current-piece))
        ))
    (t/stop)))
