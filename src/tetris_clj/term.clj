(ns tetris-clj.term
  (:require [lanterna.terminal :as t]))

(def term (t/get-terminal :swing))

(defn cls []
  (t/clear term))

(defn set-pixel [ch x y]
  (cond
    (= ch 1) (t/put-string term "#" x y)))

(defn render-piece [piece x y]
  (doall
    (map-indexed
      (fn [dy row]
        (doall
          (map-indexed
            (fn [dx pixel]
              (set-pixel pixel (+ dx x) (+ dy y))) row))) piece))
  (t/move-cursor term 1 1))

(defn start []
  (t/start term))

(defn stop []
  (t/stop term))

(defn get-key []
  (t/get-key term))
