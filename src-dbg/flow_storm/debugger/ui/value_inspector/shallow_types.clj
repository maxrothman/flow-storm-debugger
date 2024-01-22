(ns flow-storm.debugger.ui.value-inspector.shallow-types
  (:import [clojure.lang IDeref Seqable Associative ILookup Sequential Indexed Counted IFn
            IMeta IPersistentCollection IPersistentStack IPersistentMap IPersistentSet
            IPersistentVector ArityException MapEquivalence IHashEq IObj]))

(deftype RemoteMap [entries map-meta]

  Associative
  (containsKey [this k] (boolean true)) ;; TODO
  (entryAt [this k] ) ;; TODO

  IPersistentCollection
  (count [this] ) ;; TODO

  ILookup
  (valAt [this k])  ;; TODO

  Counted

  IFn
  (invoke [this k] )  ;; TODO
  (invoke [this k not-found]) ;; TODO

  IMeta
  (meta [this] ) ;; TODO

  )

#_(deftype RemoteVec [page offset total-count seq-meta]

  Associative
  (containsKey [this k] (boolean true)) ;; TODO
  (entryAt [this k] ) ;; TODO

  Seqable
  (seq [this] ) ;; TODO

  IPersistentCollection
  (count [this] ) ;; TODO

  ILookup
  (valAt [this k] ) ;; TODO

  IPersistentStack
  (peek [this] ;; TODO
    )
  (pop [this] ;; TODO
    #_(cond (zero? count) (throw (IllegalStateException. "Can't pop empty vector"))
          (== count 1)  []
          (pos? count)  (vec (butlast this))))

  Counted

  IMeta
  (meta [this] ) ;; TODO

  IFn
  (invoke [this k] ;; TODO
    )
  (invoke [this k not-found] ;; TODO
    )
  )

#_(deftype RemoteObj [obj-type obj-str]

  ShallowVal
  (shallow-val-type [sv] obj-type))

(defn make-remote-val [{:keys [type kind val] :as shallow-data}]
  (case kind
    :map     (RemoteMap. (:map/entries shallow-data)
                         (make-remote-val (:val/shallow-meta shallow-data)))
    ;; :seq     (ShallowPagedSeq. (:seq/page shallow-data)
    ;;                            (:seq/offset shallow-data)
    ;;                            (:seq/total-count shallow-data)
    ;;                            (:val/shallow-meta shallow-data))
    :number  val
    :string  val
    :boolean val
    ;; :object  (ShallowObj. type (:val/str shallow-data))
    ))
