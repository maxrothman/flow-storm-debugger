(ns flow-storm.runtime.values
  (:require [clojure.pprint :as pp]
            [clojure.datafy :as datafy]
            [flow-storm.utils :as utils]
            [flow-storm.types :as types]
            [clojure.core.protocols :refer [nav]]))

(defprotocol PWrapped
  (unwrap [_]))

(deftype HashableObjWrap [obj]
  #?@(:clj
      [clojure.lang.IHashEq
       (hasheq [_] (utils/object-id obj))
       (hashCode [_] (utils/object-id obj))
       (equals [_ that]
               (if (instance? HashableObjWrap that)
                 (identical? obj (unwrap that))
                 false))]

      :cljs
      [IEquiv
       (-equiv [_ that]
               (if (instance? HashableObjWrap that)
                 (identical? obj (unwrap that))
                 false))

       IHash
       (-hash [_]
              (utils/object-id obj))])
  
  PWrapped
  (unwrap [_]
    obj))

(defn hashable-obj-wrap [o]
  (->HashableObjWrap o))

(defn hashable-obj-wrapped? [o]
  (instance? HashableObjWrap o))

(defprotocol PValueRefRegistry
  (add-val-ref [_ v])
  (get-value [_ vref])
  (get-value-ref [_ v]))

;; Fast way of going from
;;    value-ref -> value 
;;    value     -> value-ref
;;
;; every object gets wrapped into a HashableObjWrap that will have
;; hashCode based on unique object-id, calculated by `utils/object-id`
;; This is so ^:a [] and ^:b [] get a different value-ref, which won't get unless
;; we wrap them, since both will have the same hashCode because meta isn't used for hashCode calculation
;; Wrapping is also useful for infinite sequences, since you can't put a val as a key of a hash-map if it
;; is a infinite seq
(defrecord ValueRefRegistry [vref->wv wv->vref max-vid]
  PValueRefRegistry

  (add-val-ref [this v]
    (let [wv (hashable-obj-wrap v)]
      (if (contains? wv->vref wv)
        this
        (let [next-vid (inc max-vid)
              vref (types/make-value-ref next-vid)]
          (-> this
              (assoc :max-vid next-vid)
              (update :vref->wv assoc vref wv)
              (update :wv->vref assoc wv vref))))))
  
  (get-value [_ vref]
    (unwrap (get vref->wv vref)))

  (get-value-ref [_ v]
    (let [wv (hashable-obj-wrap v)]
      (get wv->vref wv))))

(def init-ref-registry (map->ValueRefRegistry {:vref->wv {} :wv->vref {} :max-vid 0}))

(def values-ref-registry (atom init-ref-registry))

(defn deref-value [vref]
  (if (types/value-ref? vref)
    (get-value @values-ref-registry vref)

    ;; if vref is not a ref, assume it is a value and just return it
    vref))

(defn deref-val-id [vid]
  (deref-value (types/make-value-ref vid)))

(defn reference-value! [v]
  (try
    
    (swap! values-ref-registry add-val-ref v)
    (-> (get-value-ref @values-ref-registry v)
        (types/add-val-preview v))
    
    ;; if for whatever reason we can't reference the value
    ;; let's be explicit so at least the user knows that 
    ;; something went wrong and the value can't be trusted.
    ;; I have seen a issues of hashing failing for a lazy sequence
    #?(:clj (catch Exception e
              (utils/log-error "Error referencing value" e)    
              (reference-value! :flow-storm/error-referencing-value))
       :cljs (catch js/Error e
               (utils/log-error "Error referencing value" e)    
               (reference-value! :flow-storm/error-referencing-value)))))

(defn clear-vals-ref-registry []
  (reset! values-ref-registry init-ref-registry))

(defprotocol SnapshotP
  (snapshot-value [_]))

(extend-protocol SnapshotP
  #?(:clj Object :cljs default)
  (snapshot-value [v] v))

(extend-protocol SnapshotP
  nil
  (snapshot-value [_] nil))

(defn snapshot-reference [x]
  (cond

    (and (utils/blocking-derefable? x)
         (utils/pending? x))
    (merge
     {:ref/type (type x)}
     (if (realized? x)
       {:ref/snapshot (deref x)}
       {:ref/timeout x}))

    (utils/derefable? x)
    {:ref/snapshot (deref x)
     :ref/type (type x)}

    :else (snapshot-value x)))

(defn value-type [v]
  (if (and (map? v)
           (try ;; the try/catch is for things like sorted-map of symbol keys
             (contains? v :ref/type)
             (catch  #?(:clj Exception :cljs js/Error) _e nil)))
    (pr-str (:ref/type v))
    (pr-str (type v))))

(defn val-pprint [vref {:keys [print-length print-level print-meta? pprint? nth-elems]}]  
  (let [val (deref-value vref)
        val-type (value-type val)
        print-fn #?(:clj (if pprint? pp/pprint print) 
                    :cljs (if (and pprint? (not print-meta?)) pp/pprint print)) ;; ClojureScript pprint doesn't support *print-meta*
        val-str (try
                  
                  (if (and (utils/blocking-derefable? val)
                           (utils/pending? val))

                    "FlowStorm : Unrealized value"

                    (with-out-str
                      (binding [*print-level* print-level
                                *print-length* print-length
                                *print-meta* print-meta?]

                        (if nth-elems

                          (let [max-idx (dec (count val))
                                nth-valid-elems (filter #(<= % max-idx) nth-elems)]
                            (doseq [n nth-valid-elems]
                              (print-fn (nth val n))
                              (print " ")))

                          (print-fn val)))))

                  ;; return somthing so the user knows the value can't be trusted
                  #?(:clj (catch Exception e
                            (utils/log-error "Error pprinting value" e)
                            "Flow-storm error, value couldn't be pprinted")
                     :cljs (catch js/Error e
                             (utils/log-error "Error pprinting value" e)
                             "Flow-storm error, value couldn't be pprinted")))]
    {:val-str val-str
     :val-type val-type}))

(defn- maybe-ref! [x]
  (if (or (boolean? x)
          (nil? x))
    
    x

    (reference-value! x)))

(defn navigate [datafied-coll k v]
  (try
    (nav datafied-coll k v)
    #?(:clj (catch Throwable _ v)
       :cljs (catch js/Error _ v))))

(defn- build-shallow-map [datafied-map]
  (let [entries (->> datafied-map
                     (mapv (fn [[k v]]
                             (let [nv (navigate datafied-map k v)]                               
                               (cond-> {:key-ref (maybe-ref! k)
                                        :val-ref (maybe-ref! v)}
                                 
                                 (not (identical? nv v)) (assoc :nav-ref (maybe-ref! nv)))))))]
    {:val/kind :map
     :val/map-entries entries}))

(defn- build-shallow-seq [datafied-seq]  
  (let [{:keys [page/offset] :or {offset 0}} (meta datafied-seq)
        page-size 50
        cnt (when (counted? datafied-seq) (count datafied-seq))
        shallow-page (->> datafied-seq
                          (take page-size)
                          (map-indexed (fn [i v]
                                         (let [nv (navigate datafied-seq i v)]                               
                                           (cond-> {:val-ref (maybe-ref! v)}                                      
                                             (not (identical? nv v)) (assoc :nav-ref (maybe-ref! nv))))))
                          doall)
        shallow-page-cnt (count shallow-page)
        more-elems (drop shallow-page-cnt datafied-seq)]
    (cond-> {:val/kind :seq
             :val/page shallow-page
             :page/offset offset
             :total-count cnt}
      (seq more-elems) (assoc :val/more (reference-value! (with-meta more-elems {:page/offset (+ offset shallow-page-cnt)}))))))

(defn shallow-val
  
  [vref]  
  (let [v (deref-value vref)
        v-meta (meta v)
        data (cond-> (datafy/datafy v) ;; forward meta since we are storing meta like :page/offset in references
               v-meta (utils/merge-meta v-meta))
        type-name (value-type v)
        shallow-data (cond
                       (utils/map-like? data)
                       (build-shallow-map data)

                       (or (coll? data) (utils/seq-like? data))
                       (build-shallow-seq data)

                       :else {:val/kind :object
                              :val/str (pr-str v)})
        shallow-data (assoc shallow-data
                            :val/type type-name                            
                            :val/shallow-meta (when-let [m (meta v)]
                                                (shallow-val m)))]    
    shallow-data))

(defn tap-value [vref]
  (let [v (deref-value vref)]
    (tap> v)))

#?(:clj
   (defn def-value [var-ns var-name x]     
     (intern (symbol var-ns)
             (symbol var-name)
             (deref-value x))))
