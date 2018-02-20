(ns elevator.color
  "Implements utilities for working with colors in the RGBA colorspace."
  (:require [clojure.string :as string]))

(defn shade
  "Adds x to every component of the given RGB color."
  [[r g b a] x]
  [(+ r x) (+ g x) (+ b x) a])

(defn color-swatch
  "Given the base color, returns a map containing that color as well
  as it's lighter and darker shades."
  [color]
  (zipmap [:lightest :lighter :regular :darker :darkest]
          (map (partial shade color) [40 20 0 -20 -40])))

(def re-rgba #"rgba\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*\d+(\.\d+)?\s*\)")
(def re-rgb  #"rgb\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)")

(defn parse-computed
  [computed-color]
  (println computed-color)
  (let [[_ & matches] (or (re-find re-rgba computed-color)
                          (re-find re-rgb  computed-color))
        matches (vec
                  (map js/parseInt matches))]
    (condp = (count matches)
      4 (conj (vec (map js/parseInt (subvec matches 0 3)))
              (js/parseFloat (pop matches)))
      3 (conj matches 1.0))))

(defn css-color
  [color]
  (str "rgba(" (string/join "," color) ")"))
