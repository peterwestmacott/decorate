(ns decorate.core
  (:require
    [clojure.reflect :as reflect]
    [clojure.string :as string])
  (:import
    (clojure.reflect Method)))


(defn- reflect-on-interface [interface-symbol]
  (->> interface-symbol
       resolve
       reflect/reflect
       :members
       (filter #(instance? Method %))
       (filter (comp :public :flags))
       (remove (comp :final :flags))
       (sort-by :name)))

(defn- handle-arrays [type-hint]
  (or ({"boolean<>" "booleans"
        "byte<>" "bytes"
        "char<>" "chars"
        "double<>" "doubles"
        "float<>" "floats"
        "int<>" "ints"
        "long<>" "longs"
        "short<>" "shorts"} type-hint)
      (when (.endsWith (str type-hint) "<>") (string/replace (str "[L" type-hint) "<>" ";"))
      type-hint))

(defn- type-hint [symbol hint-type]
  (vary-meta symbol assoc :tag hint-type))

(def unsupported-type-hints
  (->> ["boolean" "booleans"
        "byte" "bytes"
        "char" "chars"
        "float" "floats"
        "int" "ints"
        "short" "shorts"]
       (map symbol)
       set))

(defn- no-primitive-hint [symbol hint-type]
  (if (unsupported-type-hints hint-type)
    symbol
    (vary-meta symbol assoc :tag hint-type)))

(defn- build-fn-spec [instance-symbol
                      modifier-symbol
                      {:keys [return-type
                              name
                              declaring-class
                              parameter-types]}]
  (let [type-hints (map handle-arrays parameter-types)
        fn-parameters (map-indexed (fn [idx pt] (no-primitive-hint (symbol (str "var" idx)) pt)) type-hints)
        spec-parameters (map-indexed (fn [idx pt] (type-hint (symbol (str "var" idx)) pt)) type-hints)
        fn-symbol (gensym name)]
    {:signature (concat [return-type name] parameter-types)
     :fn-sym  fn-symbol
     :fn-mod  `(~modifier-symbol
                 ~(str name)
                 (fn [~(no-primitive-hint 'this declaring-class) ~@fn-parameters]
                   (~(symbol (str "." name)) ~(no-primitive-hint 'this declaring-class) ~@fn-parameters)))
     :fn-spec `(~(type-hint name (handle-arrays return-type))
                 [~(type-hint 'this declaring-class) ~@spec-parameters]
                 (~fn-symbol ~instance-symbol ~@fn-parameters))}))

#_(defn debug-walk
  ([x] (println (debug-walk 0 x)))
  ([indent x]
   (let [i (string/join (repeat indent " "))
         recur-fn (partial debug-walk (+ indent 2))]
     (cond
       (vector? x) (str "[" (string/join (str "\n" i) (mapv recur-fn x)) "]")
       (seq? x) (str "(" (string/join (str "\n" i) (mapv recur-fn x)) ")")
       (symbol? x) (if (:tag (meta x))
                     (str "^" (:tag (meta x)) " " x)
                     (str x))
       (string? x) (str \" x \")
       :else (str x)))))

(defn first-by [f coll]
  (reduce (fn [acc item]
            (update acc (f item) #(or % %2) item))
          {}
          coll))

(defn unique-signatures [defs]
  (let [uniques (->> (first-by :signature (mapcat :spec defs)) vals (map :fn-sym) set)]
    (fn [{:keys [fn-sym]}]
      (uniques fn-sym))))

(defmacro decorator [& forwardings]
  (let [defs (for [[interface instance modifier] (partition 3 forwardings)]
               (let [reflections (reflect-on-interface interface)
                     instance-sym (gensym "instance")
                     modifier-sym (gensym "modifier")]
                 {:interface    interface
                  :modifier-sym modifier-sym
                  :modifier     modifier
                  :instance-sym instance-sym
                  :instance     instance
                  :spec         (map (partial build-fn-spec instance-sym modifier-sym) reflections)}))
        allowed? (unique-signatures defs)
        let-bindings (concat (interleave (map :modifier-sym defs)
                                         (map :modifier defs)
                                         (map :instance-sym defs)
                                         (map :instance defs))
                             (->> (mapcat :spec defs)
                                  (filter allowed?)
                                  (mapcat (juxt :fn-sym :fn-mod))))
        specs (mapcat (fn [x]
                        (cons (:interface x)
                              (->> (:spec x)
                                   (filter allowed?)
                                   (map :fn-spec))))
                      defs)]
    `(let [~@let-bindings]
       (reify ~@specs))))

(defn unmodified [_fname f] f)
