(ns types.core)

(defn variable [n] {:type :var, :value n})

(defn map-term
  "Map a term"
  [t f c]
  (let [walk (fn [t c g]
               (case (:type t)
                 :var (f (:value t))
                 :abs (update t :value g (inc c) g)
                 :app (-> t (update :fn g c g) (update :arg g c g))))]
    (walk t c walk)))

(defn shift-above
  "Shift above indices of variables protecting one under d"
  [t c d]
  (let [f #(if (< % c) (variable %) (variable (+ % d)))] (map-term t f c)))

(defn shift
  "Shift above indices of variables"
  [t d]
  (shift-above t 0 d))
