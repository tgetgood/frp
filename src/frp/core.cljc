(ns frp.core
  (:require [ubik.core :as l]
            [ubik.interactive.core :as spray #?@(:cljs [:include-macros true])]
            [ubik.interactive.system :as sys]))

#?(:cljs (enable-console-print!))

(def render
  (spray/subscription [:text :counter]
    (fn [text counter]
      [(-> l/text
           (assoc :text text)
           (l/scale 4)
           (l/translate [250 550]))
       (map (fn [i] (l/translate
                     (assoc l/circle :radius 100)
                     [(* (inc i) 200) 400]))
            (range counter))])))

(def r2
  (spray/sub++
   [(-> l/text
        (assoc :text (sub :text))
        (l/scale 4)
        (l/translate [250 550]))
    (map (fn [i] (l/translate
                  (assoc l/circle :radius 100)
                  [(* (inc i) 200) 400]))
         (range (sub :counter)))]))

(spray/defsubs subscriptions <<
  {:c (:c (<< :db))
   :text (:text (<< :db))
   :counter (:counter (<< :db))
   :ex (:examples (<< :db))
   :best (nth (<< :ex) (<< :c))})

(defn ^:export init []
  (sys/initialise!
   {:subscriptions subscriptions
    :shape r2}))

(defn on-reload []
  (init))
