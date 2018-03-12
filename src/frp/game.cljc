(ns frp.game
  (:require [ubik.core :as l]
            [ubik.geometry :as geo]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.math :as math]))

(defn text [t & [c]]
  (cond-> (l/scale (assoc l/text :text t) 2)
    c (l/translate c)))

(def point
  (assoc l/circle :radius 5 :style {:fill :#fdd017
                                    :opacity 0.8
                                    :stroke :none}))

(def button-bg
  (-> l/rectangle
      (l/style {:fill :none :stroke :black})
       (l/scale 100)))

(def circle-button
  (l/tag [button-bg
          (assoc l/annulus :style {:stroke :none :fill :black}
                 :outer-radius 35
                 :inner-radius 33
                 :centre [50 50])]
         ::circle-button))

(def rule-button
  [button-bg
   (-> l/rectangle
       (assoc :width (* 80 (math/sqrt 2)) :height 2)
       (l/style {:fill :black :stroke :none})
       (l/translate [10 9])
       (l/rotate [10 10] 45)
       (l/tag ::rule-button))
   (assoc point :centre [10 10])
   (assoc point :centre [90 90])])

(def selected
  (assoc l/rectangle :width 100 :height 100
         :style {:fill :green :opacity 0.3}))

(def control-panel
  (spray/sub-form <<
   [circle-button
    (l/translate rule-button [0 100])
    (condp = (<< :game-draw)
      :circle selected
      :line (assoc selected :corner [0 100])
      [])]))

(def draw-points
  (spray/sub-form <<
   (map #(assoc point :centre %) (<< :points))))

(def user-drawing
  (spray/sub-form <<
    (into [] (<< :drawings))))

(def problem-1
  [(text
    "Draw an equilateral triangle with the given line segment as its base."
    [0 300])

   (l/with-style {:stroke :magenta}
     (assoc l/line :from [150 0] :to [400 0]))])

(def initial-points
  [[250 300] [500 300]])

;;;;; FIXME: Duplicates

(defn clicked-on?
  "Returns true if the shape with tag contains location."
  [tag location]
  (when-let [shape (spray/find-by-tag tag)]
    (geo/contains? shape location)))

(defn valid-click?
  "Returns true if the given down and up event are sufficiently close in space
  and time."
  [{{t1 :time [x1 y1] :location} :down
    {t2 :time [x2 y2] :location} :up}]
  (and (< (- t2 t1) 2000)
       (< (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))) 100)))

(defn selection-at [click]
  (let [loc (:location click)]
    (cond
      (clicked-on? ::circle-button loc) :circle
      (clicked-on? ::rule-button loc)   :line
      :else                             nil)))

(defn drag-tx [xf]
  (let [start (volatile! nil)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (:down? n)
         (do
           (vreset! start n)
           acc)
         (let [s @start]
           (vreset! start nil)
           (if (valid-click? {:down s :up n})
             acc
             (xf acc {:start (:location s)
                      :end (:location n)
                      :time (:time s)
                      :duration [(:time s) (:time n)]}))))))))

(defn drawings-tx [xf]
  (let [mode (volatile! nil)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (:mode n)
         (do
           (vreset! mode (:mode n))
           acc)
         (if-let [m @mode]
           acc
           acc))))))

(spray/defsubs subscriptions <<
  {:game-draw     (selection-at (<< :last-click))

   :draw-modes    (map (fn [x] {:mode (selection-at x) :time (:time x)})
                       (<< :clicks))

   :drawings      (eduction drawings-tx (sort-by :time (concat (<< :draw-modes)
                                                               (<< :drags))))

   :points        (:points (<< :db))

   :drags           (eduction drag-tx (reverse (<< :mouse-event)))})

#_(defmethod view ::geo-game []

  [(l/translate control-panel [30 900])
   (l/translate problem-1 [100 300])
   user-drawing
   draw-points])
