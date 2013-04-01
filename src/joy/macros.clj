(ns joy.macros
  (:import [java.io BufferedReader InputStreamReader])
  (:import [java.net URL]))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defmacro domain [name & body]
  `{:tag :domain
    :attrs {:name (str '~name)}
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping
    :attrs {:name (str '~name)}
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
           (list? a) [:isa (str (second a))]
           (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties
     :attrs nil
     :content (apply vector (for [p props]
                              {:tag :property
                               :attrs {:name (str (first p))}
                               :content nil}))}))

(def d
  (domain man-vs-monster
          (grouping people
                 (Human "A stock human")
                 (Man (isa Human)
                      "A man, baby"
                      [name]
                      [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce, yet elusive, creature."
                     [eats-goats?]))))

(defmacro resolution [] `x)

(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (when ~'it
       (do ~@body))))

(defn joc-www []
  (-> "http://joyofclojure.com"
      URL.
      .openStream
      InputStreamReader.
      BufferedReader.))

(defmacro with-resource [binding close-fn & body]
  `(let ~binding
     (try
       (do ~@body)
       (finally
         (~close-fn ~(binding 0))))))

(declare collect-bodies)
(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))

(declare build-contract)
(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) :require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) :ensure)
                    (assoc {} :post (vec (rest con)))
                    :else (throw (Exception. (str "Unknown tag " (first con)))))))
     (list* 'f args))))

(def doubler-contract
  (contract doubler
            [x]
            (:require (pos? x))
            (:ensure (= (* 2 x) %))))

(def times2 (partial doubler-contract #(* 2 %)))
(def times3 (partial doubler-contract #(* 3 %)))

(def doubler-contract*
  (contract doubler
            [x]
            (:require (pos? x))
            (:ensure (= (* 2 x) %))
            [x y]
            (:require (pos? x) (pos? y))
            (:ensure (= (* 2 (+ x y)) %))))
