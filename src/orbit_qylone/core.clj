(ns orbit-qylone.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def init-state {:planets []
                 :agents []
                 :millis 0
                 :diff 0
                 :next-planet-time 0
                 :spc-down true})

(defn player-controller [state a-state]
  (:spc-down state))

(defn agent-collided? [{:keys [planets]} {:keys [angle radius]}]
  (or (< radius 0.1)
      (let [x (* radius (Math/cos angle))
            y (* radius (Math/sin angle))]
        (some (fn [{p-angle :angle p-radius :radius p-size :size}]
                (< (+ (Math/pow (- x (* p-radius (Math/cos p-angle))) 2)
                      (Math/pow (- y (* p-radius (Math/sin p-angle))) 2))
                   (Math/pow (+ (* 0.02 p-size) 0.02) 2)))
              planets))))

(defn make-agent [controller]
  {:updater
   (fn [{dt :diff :as state} {:keys [live angle radius score] :as a-state}]
     (if live
       (if (agent-collided? state a-state)
         (assoc a-state :live false)
         (assoc a-state
                :angle (mod (+ (/ dt 1000 radius) angle) (* 2 Math/PI))
                :radius (if (controller state a-state)
                          (min 0.9 (+ radius (* (/ dt 1000) 0.25)))
                          (- radius (* (/ dt 1000) 0.25)))
                :score (cond
                         (<= 0.8 radius) score
                         (<= 0.6 radius) (+ score (/ dt 1000.0))
                         (<= 0.3 radius) (+ score (/ dt 500.0))
                         :otherwise (+ score (/ dt 250.0)))))
       a-state))
   :angle 0
   :radius 0.9
   :live true
   :score 0.0})

(defn setup []
  init-state)

(defn update-planet [{dt :diff} {:keys [radius] :as x}]
  (when (< 0 radius)
    (-> x
        (update :radius - (/ dt 1000)))))

(defn update-fn [{:keys [planets millis next-planet-time] :as state}]
  (let [time (q/millis)
        diff (- time millis)]
    (cond-> state
      (< next-planet-time time)
      (-> (assoc :next-planet-time (+ time (rand-int 500)))
          (update :planets conj {:radius 2
                                 :angle (rand (* 2 Math/PI))
                                 :size (rand-int 4)}))
      true
      (as-> it (merge init-state it {:millis time :diff diff})
        (update it :planets (partial into
                                     []
                                     (comp (map (partial update-planet it))
                                           (filter identity))))
        (update it :agents (partial into [] (map #((:updater %) it %))))))))

(defn draw-fn [{:keys [spc-down planets millis diff agents]}]
  (let [width (q/width)
        height (q/height)]
    (q/background 240)
    (q/scale 1 1)
    (q/stroke 1)
    (q/fill 1)
    (q/text (str diff) 20 20)
    (when (first agents)
      (q/text (str (Math/round (:score (first agents)))) (* 0.5 width) 20))
    (q/with-fill [255 0 0]
      (q/ellipse (* 0.5 width) (* 0.5 height)
                 (* 0.1 width) (* 0.1 height))
      (doseq [{:keys [radius angle size]} planets]
        (q/ellipse (+ (* 0.5 width) (* 0.5 radius (Math/sin angle) width))
                   (+ (* 0.5 height) (* 0.5 radius (Math/cos angle) height))
                   (* 0.02 width size)
                   (* 0.02 height size))))
    (q/with-fill [0 255 0]
      (doseq [{:keys [radius angle]} agents]
        (q/ellipse (+ (* 0.5 width) (* 0.5 radius (Math/sin angle) width))
                   (+ (* 0.5 height) (* 0.5 radius (Math/cos angle) height))
                   (* 0.02 width)
                   (* 0.02 height))))))

(defn keypress-handler [state event]
  (case (:key event)
    :q (q/exit)
    :s (assoc state
              :agents [(make-agent player-controller)]
              :planets [])
    :space (assoc state :spc-down true)
    state))

(defn keyrelease-handler [state event]
  (case (:key event)
    :space (assoc state :spc-down false)
    state))

(q/defsketch qylone
  :title "Orbit Qylone"
  :size [500 500]
  :setup setup
  :update update-fn
  :draw draw-fn
  :key-pressed keypress-handler
  :key-released keyrelease-handler
  :middleware [m/fun-mode]
  :features [:resizable]
  :renderer :java2d)
