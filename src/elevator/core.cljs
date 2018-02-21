(ns elevator.core
    (:require [elevator.color :refer [color-swatch css-color parse-computed]]

              [clojure.string :as string]
              [cljs.pprint :refer [pprint]]

              [reagent.core :as reagent]
              [re-frame.core :refer [reg-event-db
                                     reg-event-fx
                                     reg-sub
                                     subscribe
                                     dispatch]]))

(enable-console-print!)

(defn map-html
  [f html-coll]
  (doall (for [i (range (.-length html-coll))] (f (.item html-coll i)))))

;; -------------------------------------------------------------------
;; ---- data  --------------------------------------------------------
;; -------------------------------------------------------------------

(defrecord Floor [html width height depth base-color])
(defrecord Building [width height layout])

(defn center-floors
  [{:keys [width layout] :as building}]
  (assoc building
    :layout (reduce conj
                    {}
                    (map (fn [[floor [_ y]]]
                      [floor [(/ (- width (:width floor)) 2) y]])
                    layout))))

(defn ->building
  [[pent & floors] & {:keys [gap]}]
  (center-floors
    (reduce (fn [building {:keys [width height] :as floor}]
              (let [y (:height building)]
                (-> building
                    (update :width max width)
                    (assoc :height (+ y gap height))
                    (update :layout assoc floor [0 y]))))
            (Building. (:width pent) (+ gap (:height pent)) {pent [0 0]})
            floors)))

;; -------------------------------------------------------------------
;; ---- events -------------------------------------------------------
;; -------------------------------------------------------------------

(reg-event-db
  ::init
  (fn [db [_ floors window-height]]
    (assoc db ::building      (->building floors :gap 100)
              ::window-height window-height
              ::focus         (first floors))))

(reg-event-db
  ::focus-floor
  (fn [db [_ floor]]
    (assoc db ::focus floor)))

(reg-event-db
  ::record-window-height
  (fn [db [_ height]]
    (assoc db ::window-height height)))

(reg-sub
  ::focus-y
  (fn [db]
    (let [{:keys [height] :as foc} (::focus db)
          {:keys [layout]}         (::building db)
          wh                       (::window-height db)
          [_ y]                    (get layout foc)]
      (+ (- 0 y (/ height 2)) (/ wh 2)))))

;; -------------------------------------------------------------------
;; ---- rendering ----------------------------------------------------
;; -------------------------------------------------------------------

(defn translate [x y] (str "translate("x"px,"y"px)"))
(defn translate-z [z] (str "translateZ("z"px)"))
(defn t-origin [x y]  (str x"px "y"px"))

(defn translate-3d [x y z] (str "translate3d("x"px,"y"px,"z"px)"))

(defn render-floor
  [{:keys [html width height depth base-color] :as floor} [x y] foc-y]
  (let [{:keys [lightest lighter darker darkest]} (color-swatch base-color)
        y (+ y foc-y)]
    [:div {:style {:transform-style "preserve-3d"
                   :backface-visibility "hidden"}}
     ; content
     [:div {:style {:background-color (css-color base-color)
                    :transform        (translate-3d x y 0)
                    :transition       "transform 1s cubic-bezier(0.165, 0.840, 0.440, 1.000)"
                    :position         "absolute"
                    :width            width
                    :height           height}
            :on-click (fn [] (dispatch [::focus-floor floor]))
            :dangerouslySetInnerHTML {:__html html}}]
     ; right
     [:div {:style {:width            depth
                    :height           height
                    :transform-origin (t-origin 0 0)
                    :transform        (str (translate-3d (+ x width) y 0) "rotateY(-90deg)")
                    :transition       "transform 1s cubic-bezier(0.165, 0.840, 0.440, 1.000)"

                    :position         "absolute"
                    :background-color (css-color darker)}}]
     ; left
     [:div {:style {:width            depth
                    :height           height
                    :transform-origin (t-origin 0 0)
                    :transform        (str (translate-3d x y depth) "rotateY(90deg)")
                    :transition       "transform 1s cubic-bezier(0.165, 0.840, 0.440, 1.000)"

                    :position         "absolute"
                    :background-color (css-color lighter)}}
      ]
       ; top
      [:div.top {:style {:width            width
                         :height           depth
                         :transform-origin (t-origin 0 0)
                         :transform        (str (translate-3d x y depth) "rotateX(-90deg)")
                         :transition       "transform 1s cubic-bezier(0.165, 0.840, 0.440, 1.000)"

                         :position         "absolute"
                         :background-color (css-color darkest)}}
       ]
      ; bottom
      [:div {:style {:width            width
                     :height           depth
                     :transform-origin (t-origin 0 0)
                     :transform        (str (translate-3d x (+ y height) 0) "rotateX(90deg)")
                     :transition       "transform 1s cubic-bezier(0.165, 0.840, 0.440, 1.000)"

                     :position         "absolute"
                     :background-color (css-color lightest)}}]
     ]))

;; -------------------------------------------------------------------
;; ---- initialization -----------------------------------------------
;; -------------------------------------------------------------------

(defn node->floor
  [floor-node]
  (let [rect (.getBoundingClientRect floor-node)
        w    (.-width rect)
        h    (.-height rect)
        d    (.getAttribute floor-node "data-depth")
        c    (.getPropertyValue (.getComputedStyle js/window floor-node)
                                "background-color")]
    (Floor. (str (.-outerHTML floor-node))
            w
            h
            300
            (parse-computed c))))

(defn bootstrap
  [elevator-node]
  (let [floors        (map-html node->floor (.-children elevator-node))
        window-height (.-innerHeight js/window)]
    (dispatch [::init floors window-height])))

;; -------------------------------------------------------------------
;; ---- page load ----------------------------------------------------
;; -------------------------------------------------------------------

(def example-floor
  {:html   "<div>foobar</div>"
   :width  500
   :height 300
   :depth  300
   :color  [101 155 155 1.0]})

(reg-sub
  ::building
  #(get % ::building))

(defn hello-world
  []
  (let [building (subscribe [::building])
        foc-y    (subscribe [::focus-y])]
    (fn []
      [:div {:style {:perspective 2000
                     :backface-visibility "hidden"
                     :transform-style "preserve-3d"
                     :display     "block"
                     :margin      "0 auto"
                     :width       (:width @building)
                     :height      "101%"
                     :position    "relative"}}
       (doall
        (map-indexed (fn [i [floor position]]
                       ^{:key i}
                       [render-floor floor position @foc-y])
                     (:layout @building)))])))

(bootstrap (. js/document (getElementById "my-elevator")))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "my-elevator")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
