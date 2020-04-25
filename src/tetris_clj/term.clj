(ns tetris-clj.term
  (:require [lanterna.terminal :as t]))

(def term (t/get-terminal :swing))

(defn cls []
  (t/clear term))

(defn set-pixel [ch x y]
  (cond
    (= ch 1) (t/put-string term "#" x y)))

(defn render-piece [piece x y]
  ; force evaluation of map
  (println
    (map-indexed
      (fn [dy row]
        (map-indexed
          (fn [dx pixel]
            (set-pixel pixel (+ dx x) (+ dy y))) row)) piece)))

(defn start []
  (t/start term))

(defn stop []
  (t/stop term))

(defn get-key []
  (t/get-key term))
