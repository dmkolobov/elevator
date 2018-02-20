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
  (for [i (range (.-length html-coll))] (f (.item html-coll i))))

;; -------------------------------------------------------------------
;; ---- data  --------------------------------------------------------
;; -------------------------------------------------------------------

(defrecord Floor [html width height depth base-color])
(defrecord Building [width height layout])

(defn center-floors
  [{:keys [width layout] :as building}]
  (assoc building
    :layout (map (fn [[floor [_ y]]]
                   [floor [(/ (- width (:width floor)) 2) y]])
                 layout)))

(defn ->building
  [[pent & floors] & {:keys [gap]}]
  (center-floors
    (reduce (fn [building {:keys [width height] :as floor}]
              (-> building
                  (update :width max width)
                  (update :height + height gap)
                  (update :layout assoc floor [0 (+ (:height building) gap)])))
            (Building. (:width pent) (:height pent) {pent [0 0]})
            floors)))

;; -------------------------------------------------------------------
;; ---- events -------------------------------------------------------
;; -------------------------------------------------------------------

(reg-event-db
  ::make-elevator
  (fn [db [_ floors]]
    (println floors)
    (assoc db ::building (->building floors :gap 20))))

;; -------------------------------------------------------------------
;; ---- rendering ----------------------------------------------------
;; -------------------------------------------------------------------

(defn translate [x y] (str "translate("x"px,"y"px)"))
(defn translate-z [z] (str "translateZ("z"px)"))
(defn t-origin [x y]  (str x"px "y"px"))

(defn render-floor
  [{:keys [html width height depth base-color]} [x y]]
  (let [{:keys [lightest lighter darker darkest]} (color-swatch base-color)]
    [:div {:style {:transform (translate x y)
                   :transform-style "preserve-3d"
                   :position  "absolute"
                   }}
     ;; top
     [:div {:style {:width            width
                    :height           depth
                    :transform-origin (t-origin 0 0)
                    :transform        (str (translate-z depth) "rotateX(-90deg)")
                    :position         "absolute"
                    :background-color (css-color darkest)}}
      ]
     ;; right
     [:div {:style {:width            depth
                    :height           height
                    :transform-origin (t-origin width 0)
                    :transform        (str (translate-z width) "rotateY(-90deg)")
                    :position         "absolute"
                    :background-color (css-color darker)}}
      ]
     ;; bottom
     [:div {:style {:width            width
                    :height           depth
                    :transform-origin (t-origin 0 height)
                    :transform        (str (translate-z height) "rotateX(90deg)")
                    :position         "absolute"
                    :background-color (css-color lightest)}}
      ]
     ;; left
     [:div {:style {:width            depth
                    :height           height
                    :transform-origin (t-origin 0 0)
                    :transform        (str (translate-z depth) "rotateY(90deg)")
                    :position         "absolute"
                    :background-color (css-color lighter)}}
      ]
     [:div {:style {:background-color (css-color base-color)
                    :transform        (str "translateZ(0px)")
                    :width            width
                    :height           height}
            :dangerouslySetInnerHTML {:__html html}}]]))

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
  (let [floors (map-html node->floor (.-children elevator-node))]
    (dispatch [::make-elevator floors])))

;; -------------------------------------------------------------------
;; ---- page load ----------------------------------------------------
;; -------------------------------------------------------------------

(def example-floor
  {:html   "<div>foobar</div>"
   :width  500
   :height 300
   :depth  200
   :color  [101 155 155 1.0]})

(reg-sub
  ::building
  #(get % ::building))

(defn hello-world
  []
  (let [building (subscribe [::building])]
    (fn []
      (println @building)
      [:div {:style {:perspective "1000px"
                     :height      "1000px"
                     :position    "relative"}}
       [render-floor example-floor [0 0]]
       (map (fn [[floor position]]
              ^{:key position}
              [render-floor floor position])
            (:layout @building))])))

(bootstrap (. js/document (getElementById "my-elevator")))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
