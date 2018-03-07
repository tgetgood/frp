(ns frp.core
  #?@(:clj
       [(:require
         [ubik.core :as l]
         [ubik.interactive.core :as spray]
         [ubik.math :as math])]
       :cljs
       [(:require
         [ubik.core :as l]
         [ubik.interactive.core :as spray :include-macros true]
         [ubik.math :as math])]))

#?(:cljs (enable-console-print!))

;; Two text boxes
;; Two colour buttons

(l/deftemplate block
  {:colour :black
   :size 1}
  (assoc l/rectangle :width size :height size
         :style {:fill colour :stroke :none}))

(defn text [t]
  (l/scale (assoc l/text :text t) 2))

(def status-legend
  (spray/sub-form <<
    (map-indexed
     (fn [i s]
       (l/translate s [0 (* -1 50 i)]))
     [(text (str "Clicks: " (<< :click-count)))
      (text (str "Red Clicks: " (<< :red-clicks)))
      (text (str "Blue Clicks: " (<< :blue-clicks)))
      (text (str "Mouse Down? " (<< :pressed?)))
      (text (str "Last click at: " (<< :point)))])))

(def widgets
  [(-> block
       (assoc :colour :red :size 40)
       (l/translate [0 0])
       (l/tag :red-block))

   (-> block
       (assoc :colour :blue :size 40)
       (l/translate [200 0])
       (l/tag :blue-block))

   (-> l/rectangle
       (assoc :width 200 :height 30)
       (l/translate [400 0]))

   (-> l/rectangle
       (assoc :width 200 :height 30)
       (l/translate [400 -100]))])

(def render
  [(l/translate widgets [200 500])
   (l/translate status-legend [100 900])])


(defn valid-click?
  "Returns true if the given down and up event are sufficiently close in space
  and time."
  [{{t1 :time [x1 y1] :location} :down
    {t2 :time [x2 y2] :location} :up}]
  (and (< (- t2 t1) 200)
       (< (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))) 100)))

(defn click-tx [xf]
  (let [state (atom nil)
        down (atom nil)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (:down? n)
         (do
           (reset! state :down)
           (reset! down n)
           acc)
         (let [start @down]
           (reset! down nil)
           (if (compare-and-set! state :down :up)
             (xf acc {:down start :up n})
             acc)))))))

(spray/defsubs subscriptions <<
  {:click-state (:click-state (<< :db))
   :clicks (eduction (comp click-tx (filter valid-click?)) (<< :click-state))
   :click-count (count (<< :clicks))
   :point (str (:location (last (<< :click-state))))
   :pressed? (or (:down? (last (:click-state (<< :db)))) false)})


(def event-map
  {:left-mouse-down (fn [{:keys [time location]}]
                 {:swap! (fn [db]
                           (update db :click-state conj
                                   {:time time
                                    :location location
                                    :down? true}))})

   :left-mouse-up (fn [{:keys [time location]}]
               {:swap! (fn [db]
                         (update db :click-state conj
                                 {:time time
                                  :location location
                                  :down? false}))})
   })

(defn on-reload []
  (spray/initialise!
   {:subscriptions subscriptions
    :event-handlers event-map
    :shape render}))

(defn ^:export init []
  (swap! ubik.interactive.db/app-db assoc :text "HiHiHI" :counter 4
         :click-state [])
  (on-reload))

(def db ubik.interactive.db/app-db)
