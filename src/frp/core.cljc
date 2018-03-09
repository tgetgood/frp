(ns frp.core
  (:require [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint]]
            [clojure.string :as string]
            [ubik.core :as l]
            [ubik.geometry :as geo]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.math :as math]))

#?(:cljs (enable-console-print!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Geometry Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Re-frame click-me code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reg-event-db
  {:style/indent 1}
  [k f])

(defn reg-sub
  {:style/indent 1}
  [k f])

(defn dispatch! [& args])

(defn subscribe [arg]
  (atom nil))

(def re-click-code
  {:init-db-simple
   "(reg-event-db :init-db
  (fn [_ _]
    {:counter 0}))"

   :init-db-duplex
   "(reg-event-db :init-db
  (fn [_ _]
    {:counter 0
     :last-mouse-down nil}))"

   :init-db-stream
   "(reg-event-db :init-db
  (fn [_ _]
    {:mouse-events []}))"

   :click-event
   "(reg-event-db :click
  (fn [db _]
    (update db :counter inc))) "

   :mouse-down
   "(reg-event-db :mouse-down
  (fn [db [_ e]]
    (assoc db :last-mouse-down e)))"

   :mouse-up
   "(reg-event-db :mouse-up
  (fn [db [_ up]]
    (let [down (:last-mouse-down db)]
      (cond-> (assoc db :last-mouse-down nil)
        (valid-click? down up) (update db :counter inc)))))"

   :click-sub
   "(reg-sub :clicks
  (fn [db]
    (:counter db))) "

   :view
   "(defn re-view []
  [:div
   [:button {:on-click #(dispatch! :click)} \"Click Me!\"]
   [:div {} (str \"Clicked \" @(subscribe :clicks) \" times.\")]])"

   :mouse-event-down
   "(reg-event-db :mouse-down
  (fn [db [_ e]]
    (update db :mouse-events conj e)))"

   :mouse-event-up
   "(reg-event-db :mouse-up
  (fn [db [_ e]]
    (update db :mouse-events conj e)))"

   :click-stream-sub
   "(reg-sub :clicks
     (fn [db]
       (count (valid-clicks (:mouse-events db)))))"

   :view-dup
   "(defn view []
  [:div
   [:button {:on-mouse-down #(dispatch! :mouse-down %)
             :on-mouse-up #(dispatch! :mouse-up %)}
    \"Click Me!\" ]
   [:div (str \"Clicked \" @(subscribe :clicks) \" times.\")]])"
   })


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def code-background
  (assoc l/rectangle :style {:fill "#F1F1F1"
                             :stroke :none}))

(defn set-code [code & [background]]
  (let [h 200
        lines       (take (quot h 16) (string/split-lines code))
        line-height 16
        box-height  (* (inc (count lines)) line-height)
        line-width (* 9 (apply max (map count lines)))]
    [(l/scale (or background code-background) [line-width box-height])
     (l/with-style {:font "14px monospace"}
       (map-indexed (fn [i line]
                      (let [h (- box-height (* line-height (inc i)))]
                        [(assoc l/text :text line :corner [5 h])]))
                    lines))]))

(defn text [t & [c]]
  (cond-> (l/scale (assoc l/text :text t) 2)
    c (l/translate c)))

(l/deftemplate arrow
  {:from [0 0]
   :to [100 0]}
  (let [diff (mapv - to from)
        [mx my] (mapv / diff (repeat (math/norm diff)))
        p (mapv - to (map (partial * 10) [mx my]))
        o (mapv (partial * 10) [(- my) mx])
        t1 (mapv + p o)
        t2 (mapv - p o)]
    [(assoc l/line :from from :to to)
     (assoc l/polyline
            :style {:fill :black :stroke :none}
            :points [to t1 t2 to])]))

(defmulti view identity)

(defmethod view :default [_] [])

(defmethod view ::click-me [_]
  (spray/sub-form <<
    [(l/translate
      [(l/tag (assoc l/rectangle :style {:fill :lightblue :stroke :none}
                     :width 150 :height 50)
              :click-me)
       (l/translate (text "Click Me!") [30 20])
       (l/translate (text (str "Clicked: " (<< :clickme-count) " times."))
                    [-10 -50])]
      [800 300])]))

(defmethod view ::clickme-simple-code []
  (spray/sub-form <<
   [(l/translate
     [(l/with-style {:fill (if (<< :clickme?) :blue :black)}
        (text ":click" [0 0]))
      (assoc arrow :from [70 6] :to [150 6])
      (l/translate (set-code (:click-event re-click-code)) [160 -25])
      (assoc arrow :from [450 6] :to [500 6])
      (l/translate
       [(text "App DB" [20 50])
        (set-code (str "{:counter " (<< :clickme-count) "}"))]
       [520 -20])
      (assoc arrow :from [660 6] :to [760 6])
      (l/translate (set-code (:click-sub re-click-code))
                   [780 -25])
      (assoc arrow :from [990 6] :to [1090 6])
      (text (str (<< :clickme-count)) [1110 0])
      (l/translate (set-code (:view re-click-code))
                   [1270 -30])
      (assoc arrow :from [1150 6] :to [1250 6])
      (assoc arrow :from [1250 -6] :to [920 -270])
      (let [c (if (<< :clickme?) :blue :black)]
        (l/with-style {:stroke c :fill c}
          (assoc arrow :from [740 -280] :to [50 -12])))
      (l/translate
       (set-code (:init-db-simple re-click-code))
       [400 200])
      (assoc arrow :from [500 190] :to [560 60])]
     [50 600])
    (view ::click-me)]))

(defn arrows [v]
  (map (fn [[from to]] (assoc arrow :from from :to to)) v))

(defmethod view ::clickme-duplex-code []
  (spray/sub-form <<
   [(l/translate
     [(text ":mouse-down" [0 200])
      (text ":mouse-up")

      (l/translate (set-code (:mouse-down re-click-code)) [240 180])
      (l/translate (set-code (:mouse-up re-click-code)) [200 -50])

      (l/translate
       [(text "App DB" [20 70])
        (set-code (with-out-str
                    (pprint
                     {:counter (<< :clickme-count)
                      :last-mouse-down (<< :last-mouse-down)}))
                  [])]
       [700 100])

      (l/translate (set-code (:click-sub re-click-code)) [1350 100])

      (text (str (<< :clickme-count)) [1400 -50])

      (l/translate (set-code (:view-dup re-click-code)) [1200 -300])

      (l/translate (set-code (:init-db-duplex re-click-code)) [800 300])

      (arrows
       [[[130 0] [180 0]]
        [[150 210] [220 210]]
        [[400 170] [680 120]]
        [[400 50] [680 100]]
        [[850 170] [1330 140]]
        [[1410 90] [1410 -20]]
        [[1410 -70] [1410 -170]]
        [[1180 -250] [920 -280]]
        [[720 -280] [50 -30]]
        [[900 290] [810 200]]])]
     [50 600])
    (view ::click-me)]))

(defmethod view ::clickme-stream-code []
  (spray/sub-form <<
   [(l/translate
     [(text ":mouse-down" [0 200])
      (text ":mouse-up")

      (l/translate (set-code (:mouse-event-down re-click-code)) [200 170])
      (l/translate (set-code (:mouse-event-up re-click-code)) [200 -20])

      (l/translate
       [(set-code (with-out-str
                    (pprint
                     {:mouse-events (<< :m-stream-disp)}))
                  [])
        (text "App DB" [150 210])]
       [700 50])

      (l/translate (set-code (:click-stream-sub re-click-code)) [1350 100])

      (text (str (<< :clickme-count)) [1400 -50])

      (l/translate (set-code (:view-dup re-click-code)) [1200 -300])

      (l/translate (set-code (:init-db-stream re-click-code)) [500 350])

      (arrows
       [[[130 0] [180 0]]
        [[150 210] [180 210]]
        [[540 210] [680 200]]
        [[530 20] [680 120]]
        [[1250 170] [1330 140]]
        [[1410 90] [1410 -20]]
        [[1410 -70] [1410 -170]]
        [[1180 -250] [920 -280]]
        [[720 -280] [50 -30]]
        [[650 340] [820 270]]])]
     [50 600])
    (view ::click-me)]))

(def root
  (spray/subscription [:mode] view))

(def state-flow
  [::click-me
   ::clickme-simple-code
   ::clickme-duplex-code
   ::clickme-stream-code])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Subscriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-click?
  "Returns true if the given down and up event are sufficiently close in space
  and time."
  [{{t1 :time [x1 y1] :location} :down
    {t2 :time [x2 y2] :location} :up}]
  (and (< (- t2 t1) 2000)
       (< (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))) 100)))

(defn unify-click
  "Returns a click event corresponding to a pair of (mouse-down, mouse-up)
  events. Click time is mouse-up time and location is the midpoint."
  [{{t1 :time [x1 y1] :location} :down
                    {t2 :time [x2 y2] :location} :up}]
  {:time t2 :location [(quot (+ x1 x2) 2) (quot (+ y1 y2) 2)]})

(defn click-tx
  "Stateful transducer which takes a sequence of mouse events and emits a
  sequence of (mouse-down, mouse-up) pairs that could be clicks."
  [xf]
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

(defn clicked-on?
  "Returns true if the shape with tag contains location."
  [tag location]
  (when-let [shape (spray/find-by-tag tag)]
    (geo/contains? shape location)))

(defn keybr-tx [xf]
  (let [state (volatile! nil)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (= (:type n) :down)
         (do
           (vreset! state n)
           acc)
         (let [prev @state]
           (vreset! state nil)
           (if (= (:key n) (:key prev))
             (xf acc (dissoc n :type))
             acc)))))))

(defn keys-pressed [xf]
  (let [pressed (volatile! #{})]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (= :down (:type n))
         (vswap! pressed conj (:key n))
         (if (contains? #{"GroupNext" "GroupPrevious"} (:key n))
           (vswap! pressed disj "Shift")
           (vswap! pressed disj (:key n))))
       (xf acc @pressed)))))

(defn last-rx
  ([] nil)
  ([a] a)
  ([_ x] x))

(defn arrow-counter [xf]
  (let [c (volatile! 0)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (let [next-c (cond
                      (= "ArrowLeft" (:key n)) (dec @c)
                      (= "ArrowRight" (:key n)) (inc @c)
                      :else @c)]
         (vreset! c (max 0 (min (dec (count state-flow)) next-c)))
         (xf acc @c))))))

(defn select-mode [key-stream]
  (nth state-flow (or (transduce (comp keybr-tx arrow-counter)
                                 last-rx key-stream)
                      0)))

(spray/defsubs subscriptions <<
  {:mouse-events  (:mouse-events (<< :db))

   :key-events    (:key-events (<< :db))

   :clicks        (eduction (comp click-tx
                                  (filter valid-click?)
                                  (map unify-click))
                            (<< :mouse-events))

   ;;; Simple view

   :clickme-count (count (filter (partial clicked-on? :click-me)
                                 (map :location (<< :clicks))))

   :last-mouse    (first (<< :mouse-events))

   :clickme?      (and (:down? (<< :last-mouse))
                       (clicked-on? :click-me (:location (<< :last-mouse))))

   :m-stream-disp (take 10 (<< :mouse-events))

   :last-mouse-down
   (if (:down? (<< :last-mouse))
     (dissoc (<< :last-mouse) :down?))

    ;;; Internal subscriptions

   :mode          (select-mode (<< :key-events))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Event Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def event-map
  {:left-mouse-down (fn [{:keys [time location]}]
                      {:swap! (fn [db]
                                (update db :mouse-events conj
                                        {:time     time
                                         :location location
                                         :down?    true}))})

   :left-mouse-up   (fn [{:keys [time location]}]
                      {:swap! (fn [db]
                                (update db :mouse-events conj
                                        {:time     time
                                         :location location
                                         :down?    false}))})

   :key-down        (fn [{:keys [time key]}]
                      {:swap! (fn [db]
                                (update db :key-events conj
                                        {:time time
                                         :key  key
                                         :type :down}))})

   :key-up          (fn [{:keys [time key]}]
                      {:swap! (fn [db]
                                (update db :key-events conj
                                        {:time time
                                         :key  key
                                         :type :up}))})})

(defn on-reload []
  (spray/initialise!
   {:subscriptions subscriptions
    :event-handlers event-map
    :shape root}))

(defn db-init! []
  (reset! ubik.interactive.db/app-db {:mouse-events '() :key-events []}))

(defn ^:export init []
  (db-init!)
  (on-reload))
