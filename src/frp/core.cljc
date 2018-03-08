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

(def status-legend
  (spray/sub-form <<
    (map-indexed
     (fn [i s]
       (l/translate s [0 (* -1 50 i)]))
     [(text (str "Total Clicks: " (<< :click-count)))
      (text (str "Red Clicks: " (<< :red-clicks)))
      (text (str "Blue Clicks: " (<< :blue-clicks)))
      (text (str "Mouse Down? " (<< :pressed?)))
      (text (str "Last Click at: " (<< :point)))
      (text (str "Key Pressed: " (<< :key-pressed)))])))

(defn text-box [tag selected content]
  [(-> l/rectangle
          (assoc :width 350 :height 30)
          (l/tag tag)
          (l/style {:stroke (if (= selected tag) :green :black)}))
   (-> (text content)
       (l/translate [5 5]))])

(def widgets
  (spray/sub-form <sub
    [(-> block
         (assoc :colour :red :size 40)
         (l/translate [0 0])
         (l/tag :red-block))

     (-> block
         (assoc :colour :blue :size 40)
         (l/translate [100 0])
         (l/tag :blue-block))

     (l/translate
      (text-box :box-1 (<sub :selected) (<sub :box-1-text))
      [300 0])

     (l/translate
      (text-box :box-2 (<sub :selected) (<sub :box-2-text))
      [300 -100])]))

(def render
  [(l/translate widgets [300 500])
   (l/translate status-legend [100 900])])

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
  (contains? #{"Enter" "Shift" "OS" "Alt" "Control" "Backspace" "Tab"} c))

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

(spray/defsubs subscriptions <<
  {:mouse-events (:mouse-events (<< :db))

   :key-events   (:key-events (<< :db))

   :clicks       (eduction (comp click-tx
                                 (filter valid-click?)
                                 (map unify-click))
                           (<< :mouse-events))

   :click-count  (count (<< :clicks))

   :red-clicks   (->> (<< :clicks)
                      (map :location)
                      (filter (partial clicked-on? :red-block))
                      count)

   :blue-clicks  (->> (<< :clicks)
                      (map :location)
                      (filter (partial clicked-on? :blue-block))
                      count)

   :selections   (map which-click (<< :clicks))

   :selected     (:selection (last (<< :selections)))

   :point        (:location (last (<< :clicks)))

   :key-pressed (->> (<< :key-events)
                     (eduction keys-pressed)
                     last
                     (interpose "-")
                     (apply str))

   :chars        (eduction (comp keybr-tx)
                           (<< :key-events))

   :text-events  (sort-by :time (concat (<< :key-events)
                                        (<< :selections)))

   :box-1-text   (transduce (get-keystrokes-in :box-1) str (<< :text-events))
   :box-2-text   (transduce (get-keystrokes-in :box-2) str (<< :text-events))

   :pressed?     (or (:down? (last (:mouse-events (<< :db)))) false)})


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
                      (println key)
                      {:swap! (fn [db]
                                (update db :key-events conj
                                        {:time time
                                         :key  key
                                         :type :up}))})})

(defn on-reload []
  (spray/initialise!
   {:subscriptions subscriptions
    :event-handlers event-map
    :shape render}))

(defn db-init! []
  (reset! ubik.interactive.db/app-db {:mouse-events [] :key-events []}))

(defn ^:export init []
  (db-init!)
  (on-reload))
