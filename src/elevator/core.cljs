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
                   [floor [(- width (:width floor)) y]])
                 layout)))

(defn ->building
  [floors & {:keys [gap]}]
  (reduce (fn [building {:keys [width height] :as floor}]
            (-> building
                (update :width max width)
                (update :height + height gap)
                (update :layout assoc floor [0 (+ (:height building) gap)])))
          (Building. 0 (:height (first floors)) {(first floors) [0 0]})
          (rest floors)))

;; -------------------------------------------------------------------
;; ---- events -------------------------------------------------------
;; -------------------------------------------------------------------

(reg-event-db
  ::make-elevator
  (fn [db [_ floors]]
    (assoc db ::floors (-> floors
                           (->building :gap 20)
                           (center-floors)))))

;; -------------------------------------------------------------------
;; ---- rendering ----------------------------------------------------
;; -------------------------------------------------------------------

(defn translate [x y] (str "translate("x"px,"y"px)"))
(defn translate-z [z] (str "translateZ("z"px)"))
(defn t-origin [x y]  (str x"px "y"px"))

(defn render-floor
  [{:keys [html width height depth color]} [x y]]
  (let [{:keys [lightest lighter darker darkest]} (color-swatch color)]
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
     [:div {:style {:background-color (css-color color)
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
    (Floor. (.-outerHTML floor-node)
            w
            h
            500
            (parse-computed c))))

(defn bootstrap
  [elevator-node]
  (let [floors (map-html node->floor (.-children elevator-node))]
    (pprint (->building floors :gap 20))))

;; -------------------------------------------------------------------
;; ---- page load ----------------------------------------------------
;; -------------------------------------------------------------------

(def example-floor
  {:html   "<div>foobar</div>"
   :width  500
   :height 300
   :depth  200
   :color  [101 155 155 1.0]})

(defn hello-world []
  [:div {:style {:perspective "1000px"
                 :height      "1000px"
                 :position    "relative"}}
   [render-floor example-floor [200 300]]])

(bootstrap (. js/document (getElementById "my-elevator")))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "my-elevator")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
