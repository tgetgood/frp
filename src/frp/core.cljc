(ns frp.core
  (:require [ubik.core :as l]
            [ubik.interactive.core :as spray #?@(:cljs [:include-macros true])]))

#?(:cljs (enable-console-print!))

(def render
  (spray/subscription [:point :locations]
    (fn [text locations]
      (->
       [(-> l/text
            (assoc :text text)
            (l/scale 4)
            (l/translate [250 550]))
        (-> [(-> l/polyline
                 (assoc :points (take 200 locations)))
             (-> l/polyline
                 (assoc :points (take 200 (drop 200 locations)))
                 (l/style {:stroke :red}))]

            (l/scale 0.2))]
       (l/translate [500 500]))
      )))

(spray/defsubs subscriptions <<
  {:c (:c (<< :db))
   :text (:text (<< :db))
   :counter (:counter (<< :db))
   :locations (:locations (<< :db))
   :point (str (first (<< :locations)))
   :ex (:examples (<< :db))
   :best (nth (<< :ex) (<< :c))})

(def event-map
  {:mouse-move (fn [{:keys [location]}]
                 {:swap! (fn [db]
                           (update db :locations
                                   (fn [locs]
                                     (conj (take 999 locs) location))))})})

(defn ^:export init []
  (swap! ubik.interactive.db/app-db assoc :text "HiHiHI" :counter 4 )

  (spray/initialise!
   {:subscriptions subscriptions
    :event-handlers event-map
    :shape render}))

(defn on-reload []
  (init))
