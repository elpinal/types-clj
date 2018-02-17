(ns types.core)

(defn variable [n] {:var n})
(defn abs [t] {:abs t})
(defn app [f x] {:app {:fn f, :arg x}})

(defn map-term
  "Map a term"
  [t f c]
  (let [app' #((partial array-map :app) %)
        walk (fn [t c g]
               (condp #(%2 %1) t
                     :var :>> #(f c %)
                     :abs (update t :abs g (inc c) g)
                     :app :>> #(-> %
                                   (update :fn g c g)
                                   (update :arg g c g)
                                   app')))]
    (walk t c walk)))

(defn shift-above
  "Shift above indices of variables protecting one under d"
  [t c d]
  (let [f #(if (< %2 %1) (variable %2) (variable (+ %2 d)))] (map-term t f c)))

(defn shift
  "Shift above indices of variables"
  [t d]
  (shift-above t 0 d))
