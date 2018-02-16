(ns types.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn shift-above
  "Shift above indices of variables"
  [t c d]
  (case (:type t)
    :var (let [n (:value t)] (if (< n c) t (assoc t :value (+ n d))))
    :abs (shift-above (:value t) (+ c 1) d)
    :app (-> t (update :fn shift-above c d) (update :arg shift-above c d)))
  )
