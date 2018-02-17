(ns types.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as t]))

(defn variable [n] {::var n})
(defn abs [t] {::abs t})
(defn app [f x] {::app {::fn f, ::arg x}})

(defn map-term
  "Maps a term.
  `f` is applied to a protection boundary and a variable.
  The protection boundary starts from `c`, increasing each time going through abstractions."
  [t f c]
  (let [app' #((partial array-map ::app) %)
        walk (fn [t c g]
               (condp #(%2 %1) t
                 ::var :>> #(f c %)
                 ::abs (update t ::abs g (inc c) g)
                 ::app :>> #(-> %
                                (update ::fn g c g)
                                (update ::arg g c g)
                                app')))]
    (walk t c walk)))

(defn shift-above
  "Shifts above indices of variables by `d` protecting ones under `c`."
  [t c d]
  (let [f #(if (< %2 %1) (variable %2) (variable (+ %2 d)))] (map-term t f c)))

(defn shift
  "Shifts above indices of variables by `d`."
  [t d]
  (shift-above t 0 d))

(defn subst
  "Substitutes a term.
  Replace `c` in `t` with `t'`.
  `c` is an index of the variable in the context of `t'`."
  [t c t']
  (let [f #(if (= (+ %1 c) %2) (shift t' %1) (variable %2))] (map-term t f c)))

(defn subst-top
  "Substitutes a term.
  This function acts as beta-reduction of `(\\x.t') t`."
  [t t']
  (shift (subst t 0 (shift t' 1)) -1))

(defn eval-app-n
  [f x]
  (condp #(%2 %1) f
    ::var (-> x
              eval-n
              (app f))
    ::abs :>> #(subst-top % x)
    ::app :>> #(eval-app-n (:fn %) (:arg %))))

(defn eval-n
  "Evaluates a term in the normal order strategy.
  The returned term is in the normal form if it has."
  [t]
  (condp #(%2 %1) t
    ::var t
    ::abs (update t ::abs eval-n)
    ::app :>> #(eval-app-n (:fn %) (:arg %))))

(s/fdef variable
        :args (s/cat :index integer?)
        :ret ::term)

(s/fdef abs
        :args (s/cat :term ::term)
        :ret ::term)

(s/fdef app
        :args (s/cat
               :fn ::term
               :arg ::term)
        :ret ::term)

(s/def ::app
  (s/keys :req [::fn ::arg]))

(s/def ::term
  (s/or
   :var (s/keys :req [::var])
   :abs (s/keys :req [::abs])
   :app (s/keys :req [::app])))

(s/def ::boundary integer?)

(s/def ::onvar
  (s/fspec :args (s/cat
                  :boundary ::boundary
                  :index integer?)
           :ret ::term))

(s/fdef map-term
        :args (s/cat
               :term ::term
               :onvar ::onvar
               :boundary ::boundary)
        :ret ::term)

(s/fdef shift-above
        :args (s/cat :term ::term
                     :boundary ::boundary
                     :delta integer?))
