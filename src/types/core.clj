(ns types.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn map-term
  "Map a term"
  [t f c]
  (let [walk (fn [t c g]
               (case (:type t)
                 :var (f (:value t))
                 :abs (update t :value g (+ c 1) g)
                 :app (-> t (update :fn g c g) (update :arg g c g))))]
    (walk t c walk)))

(defn shift-above
  "Shift above indices of variables protecting one under d"
  [t c d]
  (case (:type t)
    :var (let [n (:value t)] (if (< n c) t (assoc t :value (+ n d))))
    :abs (shift-above (:value t) (+ c 1) d)
    :app (-> t (update :fn shift-above c d) (update :arg shift-above c d))))

(defn shift
  "Shift above indices of variables"
  [t d]
  (shift-above t 0 d))
