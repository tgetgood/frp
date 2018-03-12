(ns frp.core
  (:require [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint]]
            [clojure.string :as string]
            [ubik.core :as l]
            [ubik.geometry :as geo]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.math :as math]))

#?(:cljs (enable-console-print!))

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

(def elm-code
  {:init
   "main =
  beginnerProgram { model = 0, view = view, update = update }"

   :view
   "view model =
  div []
    [ button [ onClick Increment ] [ text \"Click Me!\"]
    , div [] [ text (\"Clicked: \" ++ (toString model) ++ \" times.\")]
    ]"

   :msg
   "type Msg = Increment"

   :main
   "main =
  beginnerProgram { model = 0, view = view, update = update }"

   :update
   "update msg model =
  case msg of
    Increment ->
      model + 1"})

(def redux-code
  {:view "<div>
  <p>
    <button id='increment'>Click Me!</button>
    Clicked: <span id='value'>0</span> times.
  </p>
</div>"

   :action
   "store.dispatch({ type: 'INCREMENT' });"

   :reducer
   "function counter(state, action) {
  if (typeof state === 'undefined') {
    return 0
  }
  switch (action.type) {
    case 'INCREMENT':
      return state + 1
    default:
      return state
  }
}"

   :sub
   "function render () {
  document.getElementById('value').innerHTML =
    store.getState().toString();
};"})

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
   [:div {} (str \"Clicked: \" @(subscribe :clicks) \" times.\")]])"

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
   [:div (str \"Clicked \" @(subscribe :clicks) \" times.\")]])"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn text [t & [c]]
  (cond-> (l/scale (assoc l/text :text t) 2)
    c (l/translate c)))

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

(defn centre [{:keys [xmin xmax ymin ymax]}]
  [(/ (- xmax xmin) 2) (/ (- ymax ymin) 2)])

(defn slope-angle [[x y]]
  (if (zero? x)
    (if (< 0 y)
      (/ math/pi 2)
      (* 3 (/ math/pi 2)))
    (math/atan (/ y x))))

(defn angles [{:keys [xmin xmax ymin ymax]} [x y]]
  (let [possibles (map slope-angle
                       (map (fn [[a b]] [(- a x) (- b y)])
                            [[xmin ymin] [xmin ymax]
                             [xmax ymin] [xmax ymax]]))]
    {:pre  (apply min possibles)
     :post (apply max possibles)}))

(defn circle-layout
  [s]
  (let [base-angle (/ (* 2 math/pi) (count s))
        centres    (map (fn [a] [(math/cos (* a base-angle))
                                 (math/sin (* a base-angle))])
                        (range (count s)))]
    (map (fn [s [x y]]
           (let [box (geo/bound s)
                 [dx dy] (centre box)
                 centre [(- x dx) (- y dy)]]
             (assoc (angles box centre) :centre centre)))
         s centres)))

(defmulti view identity)

(defmethod view :default [_] [])

(defmethod view ::click-me [_]
  (spray/sub-form <<
    [(l/translate
      [(l/tag (assoc l/rectangle :style {:fill :#3bb9ff :stroke :none}
                     :width 150 :height 50)
              :click-me)
       (l/translate (text "Click Me!") [30 20])
       (l/translate (text (str "Clicked: " (<< :clickme-count) " times."))
                    [-10 -50])]
      [800 300])]))

(defn arrows [v]
  (map (fn [[from to]] (assoc arrow :from from :to to)) v))

(defmethod view ::clickme-simple-code []
  (spray/sub-form <<
   [(l/translate
     [(l/translate (set-code "(dispatch! :click)") [0 -150])
      (l/translate (set-code (:click-event re-click-code)) [160 -25])
      (l/translate
       [(text "App DB" [20 50])
        (set-code (str "{:counter " (<< :clickme-count) "}") [])]
       [520 -20])
      (l/translate (set-code (:click-sub re-click-code))
                   [780 -25])
      (text (str (<< :clickme-count)) [1110 0])
      (l/translate (set-code (:view re-click-code))
                   [1270 -30])
      (let [c (if (<< :clickme?) :blue :black)]
        (l/with-style {:stroke c :fill c}
          (assoc arrow :from [740 -280] :to [180 -145])))
      (l/translate
       (set-code (:init-db-simple re-click-code))
       [400 200])

      (arrows
       [[[70 -100] [150 -12]]
        [[450 6] [500 6]]
        [[660 6] [760 6]]
        [[990 6] [1090 6]]
        [[1150 6] [1250 6]]
        [[1250 -6] [920 -270]]
        [[500 190] [560 60]]])]
     [50 600])
    (view ::click-me)]))

(defmethod view ::elm-code [_]
  (spray/sub-form <<
   [(l/translate
     [(set-code (:msg elm-code))
      (l/translate (set-code (:update elm-code)) [460 -25])
      (l/translate
       [(text "Model" [-20 50])
        (set-code (<< :clickme-count) [])]
       [850 -20])
      (l/translate (set-code (:view elm-code))
                   [1120 -30])
      (let [c (if (<< :clickme?) :blue :black)]
        (l/with-style {:stroke c :fill c}
          (assoc arrow :from [740 -280] :to [50 -12])))

      (l/translate (set-code (:main elm-code)) [500 200])
      (arrows
       [[[200 6] [430 6]]
        [[660 6] [760 6]]
        [[990 6] [1090 6]]
        [[1150 -50] [920 -270]]])]
     [50 600])
    (view ::click-me)]))

(defmethod view ::redux-code [_]
  (spray/sub-form <<
   [(l/translate
     [(set-code (:action redux-code))
      (l/translate (set-code (:reducer redux-code)) [550 -75])
      (l/translate
       (l/scale
        (set-code (<< :clickme-count) [])
        1.2)
       [1100 -220])
      (l/translate (set-code (:sub redux-code))
                   [1280 -25])
      (let [c (if (<< :clickme?) :blue :black)]
        (l/with-style {:stroke c :fill c}
          (assoc arrow :from [740 -280] :to [50 -12])))

      (l/translate (set-code (:view redux-code)) [1200 -400])

      (arrows
       [[[370 6] [530 6]]
        [[900 6] [1250 6]]
        [[1350 -50] [920 -270]]])]
     [50 600])
    (view ::click-me)]))


(defmethod view ::cycles [_]
  [(l/translate
    [(l/scale (text "Re-frame" [30 30]) 2)
     (map-indexed (fn [i x]
                    (text x [(* (inc i) 300) 0]))
                     ["event" "event-handler" "effect-handler"])
     (text "subscription" [1300 0])
     (text "view" [1600 0])]
    [0 800])
   (l/translate
    [(l/scale (text "Elm" [30 30]) 2)
     (text "model" [1150 0])
     (text "update" [750 0])
     (text "message" [300 0])
     (text "view" [1450 0])]
    [0 500])
   (l/translate
    [(l/scale (text "Redux" [30 30]) 2)
     (map-indexed (fn [i x]
                    (text x [(* (inc i) 450) 0]))
                  ["" "" "render"])
     (text "reducer" [750 0])
     (text "action" [300 0])]
    [0 200])
   (arrows
    [[[170 650] [1600 650]]
     [[170 350] [1600 350]]])])

(defmethod view ::split-cycles [_]
  [(view ::cycles)
   (-> l/circle
       (l/style {:stroke :red})
       (l/scale [120 450])
       (l/translate [330 500]))
   (-> l/circle
       (l/style {:stroke :red})
       (l/scale [180 450])
       (l/rotate 10)
       (l/translate [720 500]))
   (-> l/circle
       (l/style {:stroke :red})
       (l/scale [180 450])
       (l/rotate -15)
       (l/translate [900 500]))
   (-> l/circle
       (l/style {:stroke :red})
       (l/scale [150 450])
       (l/translate [1380 500]))

   (-> l/circle
       (l/style {:stroke :red})
       (l/scale [200 450])
       (l/translate [1630 500]))

   (text "1" [320 970])
   (text "2" [660 970])
   (text "3" [980 970])
   (text "4" [1370 970])
   (text "5" [1640 970])])

(defmethod view ::general-pattern [_]
  [(text "View" [600 700])
   (text "Event" [1000 700])
   (text "Dataflow" [800 400])
   (-> l/circle
       (l/scale [300 100])
       (l/translate [850 400]))
   (arrows
    [[[690 706] [950 706]]
     [[1020 675] [930 525]]
     [[700 515] [640 675]]])])

(defmethod view ::frp [_]
  [(text "Inputs" [300 600])
   (text "Dataflow" [800 600])
   (-> l/circle
       (l/scale [300 100])
       (l/translate [850 600]))
   (text "View" [1400 600])

   (text "Event" [300 400])
   (text "Model" [500 400])
   (text "Model" [700 400])

   (text "Model" [1200 400])
   (text "View" [1400 400])

   (arrows
    [[[400 606] [500 606]]
     [[1200 606] [1350 606]]

     [[380 406] [470 406]]
     [[580 406] [670 406]]

     [[1280 406] [1380 406]]])])

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
   ::elm-code
   ::redux-code
   ::cycles
   ::split-cycles
   ::general-pattern
   ::frp
   ::clickme-duplex-code
   ::clickme-stream-code
   ::stream-issues])

(def game
  [::geo-game])

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

(defn count-rx
  ([] 0)
  ([a] a)
  ([a _] (inc a)))

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
(defn selection-at [click]
  (let [loc (:location click)]
    (cond
      (clicked-on? ::circle-button loc) :circle
      (clicked-on? ::rule-button loc)   :line
      :else                             nil)))

(spray/defsubs subscriptions <<
  {:mouse-events  (:mouse-events (<< :db))

   :key-events    (:key-events (<< :db))

   :clicks        (eduction (comp click-tx
                                  (filter valid-click?)
                                  (map unify-click))
                            (reverse (<< :mouse-events)))

   :last-click    (reduce last-rx (<< :clicks))

   :drags         (eduction drag-tx (reverse (<< :mouse-event)))

   :clickme-count (count (filter (partial clicked-on? :click-me)
                                 (map :location (<< :clicks))))

   :last-mouse    (first (<< :mouse-events))

   :clickme?      (and (:down? (<< :last-mouse))
                       (clicked-on? :click-me (:location (<< :last-mouse))))

   :m-stream-disp (take 10 (<< :mouse-events))

   :last-mouse-down
   (if (:down? (<< :last-mouse))
     (dissoc (<< :last-mouse) :down?))

   :game-draw     (selection-at (<< :last-click))

   :draw-modes    (map (fn [x] {:mode (selection-at x) :time (:time x)})
                       (<< :clicks))

   :drawings      (eduction drawings-tx (sort-by :time (concat (<< :draw-modes)
                                                               (<< :drags))))

   :points        (:points (<< :db))

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
  (reset! ubik.interactive.db/app-db {:mouse-events '() :key-events []
                                      :points []}))

(defn ^:export init []
  (db-init!)
  (on-reload))
