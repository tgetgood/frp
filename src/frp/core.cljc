(ns frp.core
  (:require
      [ubik.core :as l]
      [ubik.geometry :as geo]
      [ubik.interactive.core :as spray :include-macros true]
      [ubik.math :as math]))

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

(def legend-data
  (spray/sub-form <<
    (array-map
     "Mouse Down? "    (<< :pressed?)
     "Last Click at: " (<< :point)
     "Total Clicks: "  (<< :click-count)
     "Red Clicks: "    (<< :red-clicks)
     "Blue Clicks: "   (<< :blue-clicks)
     "Key Pressed: "   (<< :key-pressed))))

(def legend
  (spray/sub-form <<
    [(map-indexed
      (fn [i s]
        (l/translate (text s) [0 (* -1 50 i)]))
      (map key (<< :legend-data)))
     (map-indexed
      (fn [i s]
        (l/translate (text (str s)) [160 (* -1 50 i)]))
      (map val (<< :legend-data)))]))

(defn text-box [tag]
  (spray/subscription [:selected]
    (fn [selected]
      (-> l/rectangle
          (assoc :width 350 :height 30)
          (l/tag tag)
          (l/style {:stroke (if (= selected tag) :green :black)})))))

(def box-1-loc [0 -100])
(def box-2-loc [0 -200])

(def text-out
  (spray/sub-form <<
    [(-> (text (<< :box-1-text))
         (l/translate [5 5])
         (l/translate box-1-loc))

     (-> (text (<< :box-2-text))
         (l/translate [5 5])
         (l/translate box-2-loc))]))

(def inputs
  [(-> block
       (assoc :colour :red :size 40)
       (l/translate [0 0])
       (l/tag :red-block))


   (-> block
       (assoc :colour :blue :size 40)
       (l/translate [0 100])
       (l/tag :blue-block))

   (l/translate
    (text-box :box-1)
    box-1-loc)

   (l/translate
    (text-box :box-2)
    box-2-loc)])

(def io-tags
  {:pressed?    [1250 700]
   :point       [1250 650]
   :click-count [1250 600]
   :red-clicks  [1250 550]
   :blue-clicks [1250 500]
   :key-pressed [1250 450]

   :box-1-text  [420 460]
   :box-2-text  [420 360]

   :mouse-up    [50 1000]
   :mouse-down  [50 900]
   :key-down    [50 200]
   :key-up      [50 100]})

(def internal-flow-tags
  {:mouse-events [400 950]
   :clicks       [700 800]
   :selections   [800 600]
   :text-events  [700 400]
   :key-events   [400 150]})

(defn sub-tag-render [tags colour]
  (map (fn [[k v]] (l/with-style {:fill colour}
                     (l/translate (text k) v)))
       tags))

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

(def all-tags (merge io-tags internal-flow-tags))

(defn tag-arrow [[from to of ot]]
  (let [[x1 y1] (get all-tags from)
        [x2 y2] (get all-tags to)
        [x1o y1o] (or of [0 0])
        [x2o y2o] (or ot [0 0])
        p [(+ x1o x1 (* 12 (count (name from)))) (- (+ y1 y1o 6) y1o)]
        q [(+ x2o (- x2 20)) (+ y2o y2 6)]]
    (assoc arrow :from p :to q )))

(def flow-chart
  (map tag-arrow
       [[:mouse-up :mouse-events]
        [:mouse-down :mouse-events]
        [:key-down :key-events]
        [:key-up :key-events]
        [:mouse-events :clicks]
        [:mouse-events :pressed?]
        [:clicks :selections nil [70 12]]
        [:clicks :click-count]
        [:clicks :point]
        [:selections :red-clicks]
        [:selections :blue-clicks]
        [:selections :text-events nil [70 12]]
        [:key-events :key-pressed]
        [:key-events :text-events nil [70 -12]]
        [:text-events :box-1-text [-150 0] [140 0]]
        [:text-events :box-2-text [-150 0] [140 0]]]))

(def render
  (spray/subscription [:mode]
    (fn [mode]
      [(l/translate [inputs text-out] [50 550])
       (when (contains? #{:flow :io :all-tags} mode)
         (sub-tag-render io-tags :teal))
       (when (contains? #{:flow :all-tags} mode)
         (sub-tag-render internal-flow-tags :rebeccapurple))
       (when (contains? #{:flow} mode)
         flow-chart)
       (l/translate legend [1450 700])])))

(def click-me
  (spray/sub-form <<
    [(l/translate
      [(l/tag (assoc l/rectangle :style {:fill :lightblue :stroke :none}
                     :width 150 :height 50)
              :click-me)
       (l/translate (text "Click Me!") [30 20])
       (l/translate (text (str "Clicked: " (<< :clickme-count) " times."))
                    [0 -50])]
      [200 300])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Subscriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-click?
  "Returns true if the given down and up event are sufficiently close in space
  and time."
  [{{t1 :time [x1 y1] :location} :down
    {t2 :time [x2 y2] :location} :up}]
  (and (< (- t2 t1) 200)
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

(defn control-char? [c]
  (contains? #{"Enter" "Shift" "OS" "Alt" "Control" "Backspace" "Tab"
               "ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown"}
             c))

(defn which-click [{loc :location :as point}]
  (cond
    (clicked-on? :box-1 loc) (assoc point :selection :box-1)
    (clicked-on? :box-2 loc) (assoc point :selection :box-2)
    :else                    (assoc point :selection :none)))

(defn keys-in [k]
  (fn [xf]
    (let [in-focus? (volatile! false)]
      (fn
        ([] (xf))
        ([acc] (xf acc))
        ([acc n]
         (if-let [sel (:selection n)]
           (do
             (vreset! in-focus? (= sel k))
             acc)
           (if @in-focus?
             (xf acc n)
             acc)))))))

(defn get-keystrokes-in [tag]
  (comp keybr-tx
        (keys-in tag)
        (map :key)
        (remove control-char?)
        (take 30)))

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

(def state-flow
  [:base
   :io
   :all-tags
   :flow])

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
  {:mouse-events   (:mouse-events (<< :db))

   :mouse-down     (filter :down? (<< :mouse-events))

   :mouse-up       (remove :down? (<< :mouse-events))

   :key-events     (:key-events (<< :db))

   :key-down       (filter #(= :down (:type %)) (<< :key-events))

   :key-up         (filter #(= :up (:type %)) (<< :key-events))

   :clicks         (eduction (comp click-tx
                                   (filter valid-click?)
                                   (map unify-click))
                             (<< :mouse-events))

   :click-count    (count (<< :clicks))

   :red-clicks     (->> (<< :clicks)
                        (map :location)
                        (filter (partial clicked-on? :red-block))
                        count)

   :blue-clicks    (->> (<< :clicks)
                        (map :location)
                        (filter (partial clicked-on? :blue-block))
                        count)

   :selections     (map which-click (<< :clicks))

   :selected       (:selection (last (<< :selections)))

   :point          (:location (last (<< :clicks)))

   :key-pressed    (->> (<< :key-events)
                        (eduction keys-pressed)
                        last
                        (interpose "-")
                        (apply str))

   :text-events    (sort-by :time (concat (<< :key-events)
                                          (<< :selections)))

   :box-1-text     (transduce (get-keystrokes-in :box-1) str (<< :text-events))
   :box-2-text     (transduce (get-keystrokes-in :box-2) str (<< :text-events))

   :pressed?       (or (:down? (last (:mouse-events (<< :db)))) false)

   ;;; Simple view

   :clickme-count (count (filter (partial clicked-on? :click-me)
                                 (map :location (<< :clicks))))


    ;;; Internal subscriptions

   :legend-data    legend-data

   :mode           (select-mode (<< :key-events))}
  )


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
    :shape click-me}))

(defn db-init! []
  (reset! ubik.interactive.db/app-db {:mouse-events [] :key-events []}))

(defn ^:export init []
  (db-init!)
  (on-reload))
