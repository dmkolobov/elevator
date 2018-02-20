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
          (map (partial shade color) [-40 -20 0 20 40])))

(def re-rgba #"rgba\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*\d+(\.\d+)?\s*\)")
(def re-rgb  #"rgb\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)")

(defn parse-computed
  [computed-color]
  (println computed-color)
  (let [match (or (re-find re-rgba computed-color)
                  (re-find re-rgb  computed-color))]
    (condp = (count match)
      5 (subvec match 1)
      4 (conj (subvec match 1) 1.0))))

(defn css-color
  [color]
  (str "rgba(" (string/join "," color) ")"))
