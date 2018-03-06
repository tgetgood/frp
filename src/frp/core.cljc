(ns frp.core
  (:require [ubik.core :as l]
            [ubik.interactive.core :as spray #?@(:cljs [:include-macros true])]))

#?(:cljs (enable-console-print!))

(def colour-button
  (assoc l/rectangle :width 10 :height 10 :style {:stroke :none}))

(defn cb [c o]
  (-> colour-button (l/style {:fill c}) (l/translate [o 0])))

(def draw-ui
  (map cb [:red :green :blue] [0 10 20]))

(def axis
  [(assoc l/line :to [1000 0])
   (assoc l/line :to [0 1000])
   (l/with-style {:stroke :grey :opacity 0.5}
     [(map (fn [x] (assoc l/line :from [x 0] :to [x 1000])) (range 0 1000 100))
      (map (fn [y] (assoc l/line :from [0 y] :to [1000 y])) (range 0 1000 100))])])

(defn graph [points]
  [axis
   (assoc l/polyline :points points)])

(defn text [t]
  (assoc l/text :text t))

(defn with-subs [keys f]
  (spray/subscription keys f))

(def render
  (spray/subscription [:point :pressed?]
    (fn [point pressed?]
      [
       (->
        [(text "Mouse Button Down?")
         (l/translate (text pressed?) [120 0])
         (-> [(text "Cursor Position:")
              (l/translate (text point) [120 0])]
             (l/translate [0 -30]))]
        (l/scale 3)
        (l/translate [100 900]))
       (->
        [(l/translate (l/scale (text "X coord over time:") 10) [10 1050])
         (spray/subscription [:x-coords] graph)]
        (l/scale 0.2)
        (l/translate [800 700]))
       (->
        [(-> (text "Y coord over time:") (l/scale 10) (l/translate [10 1050]))
         (l/with-style {:stroke :blue}
           (spray/subscription [:y-coords] graph))]
        (l/scale 0.2)
        (l/translate [1100 700]))
       #_(-> [(-> l/polyline
                  (assoc :points (take 200 locations)))
              (-> l/polyline
                  (assoc :points (take 200 (drop 200 locations)))
                  (l/style {:stroke :red}))])])))

(defn last-30-seconds [locs]
  (let [now     #?(:cljs (js/Date.now) :clj 0)
        in-span (take-while (fn [x] (< (- now (:time x)) 30000))
                            locs)]
    (loop [locs in-span
           out []
           time (- now 30000)]
      (if (empty? locs)
        out
        (if (< time (:time (first locs)))
          (recur locs (conj out (first locs)) (+ time 300))
          (recur (rest locs) out time))))))

(spray/defsubs subscriptions <<
  {:locations (map :location (:locations (<< :db)))
   :timeline (last-30-seconds (:locations (<< :db)))
   :point (str (first (<< :locations)))
   :pressed? (:down? (first (:click-state (<< :db))))
   :x-coords (map-indexed (fn [i [x _]] [i (quot x 2)]) (take 1000 (<< :locations)))
   :y-coords (map-indexed (fn [i [_ y]] [i y]) (take 1000 (<< :locations)))})


(def event-map
  {:mouse-move (fn [{:keys [location time]}]
                 {:swap! (fn [db]
                           (update db :locations conj
                                   {:time     time
                                    :location location}))})
   :left-mouse-down (fn [{:keys [time]}]
                 {:swap! (fn [db]
                           (update db :click-state conj
                                   {:time time :down? true}))})

   :left-mouse-up (fn [{:keys [time]}]
               {:swap! (fn [db]
                         (update db :click-state conj
                                 {:time time :down? false}))})
   })

(defn on-reload []
  (spray/initialise!
   {:subscriptions subscriptions
    :event-handlers event-map
    :shape render}))

(defn ^:export init []
  (swap! ubik.interactive.db/app-db assoc :text "HiHiHI" :counter 4 )
  (on-reload))

(def db ubik.interactive.db/app-db)
