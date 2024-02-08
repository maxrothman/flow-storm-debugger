(ns flow-storm.runtime.types.expr-trace
  (:require [flow-storm.runtime.indexes.protocols :as index-protos]
            [flow-storm.utils :as utils])
  #?(:clj (:import [flow_storm.runtime.types SoftExprTrace])))

(def nil-idx -1)

(defprotocol ExprTraceP  
  (set-idx [_ idx])
  (set-fn-call-idx [_ idx]))

(deftype ExprTrace
    [coord
     exprVal
     ^:unsynchronized-mutable ^int fnCallIdx
     ^:unsynchronized-mutable ^int thisIdx]

  index-protos/ExpressionTimelineEntryP
  (get-expr-val [_] exprVal)
  
  ExprTraceP
    
  (set-idx [_ idx]
    (set! thisIdx (int idx)))
  (set-fn-call-idx [_ idx]
    (set! fnCallIdx (int idx)))

  index-protos/CoordableTimelineEntryP
  (get-coord-vec [_] (utils/str-coord->vec coord))
  (get-coord-raw [_] coord)
  
  index-protos/TimelineEntryP

  (entry-type [_] :expr)
  (fn-call-idx [_]
    (when (not= fnCallIdx nil-idx)
      fnCallIdx))
  (entry-idx [_]
    (when (not= thisIdx nil-idx)
      thisIdx))
  
  index-protos/ImmutableP
  
  (as-immutable [this]
    {:type :expr
     :coord (index-protos/get-coord-vec this)
     :result (index-protos/get-expr-val this)
     :fn-call-idx (index-protos/fn-call-idx this)
     :idx (index-protos/entry-idx this)})
  
  #?@(:clj
      [Object
       (toString
        [_]
        (utils/format "[%d ExprTrace] coord: %s, valType: %s" thisIdx coord (type exprVal)))]))

(defn make-expr-trace [coord expr-val fn-call-idx this-idx]
  (->ExprTrace coord expr-val fn-call-idx this-idx))


(def hole-expr-trace (make-expr-trace nil ::hole -1 -1))

(defn hole-expr-trace? [x]
  (identical? x hole-expr-trace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Soft expression trace ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
   (extend-type SoftExprTrace
     index-protos/ExpressionTimelineEntryP
     (get-expr-val [this] (.get this))
     
     ExprTraceP
     
     (set-idx [this idx]
       (set! (.-thisIdx this) (int idx)))
     (set-fn-call-idx [this idx]
       (set! (.-fnCallIdx this) (int idx)))

     index-protos/CoordableTimelineEntryP
     (get-coord-vec [this] (utils/str-coord->vec (.-coord this)))
     (get-coord-raw [this] (.-coord this))
     
     index-protos/TimelineEntryP

     (entry-type [_] :expr)
     (fn-call-idx [this]
       (when (not= (.-fnCallIdx this) nil-idx)
         (.-fnCallIdx this)))
     (entry-idx [this]
       (when (not= (.-thisIdx this) nil-idx)
         (.-thisIdx this)))
     
     index-protos/ImmutableP
     
     (as-immutable [this]
       {:type :expr
        :coord (index-protos/get-coord-vec this)
        :result (index-protos/get-expr-val this)
        :fn-call-idx (index-protos/fn-call-idx this)
        :idx (index-protos/entry-idx this)})))


#?(:clj
   (defn make-soft-expr-trace [coord expr-val fn-call-idx this-idx queue]
     (SoftExprTrace. coord expr-val fn-call-idx this-idx queue)))

#?(:clj (defn expr-trace? [x]
          (and x (or (instance? ExprTrace x)
                     (instance? SoftExprTrace x))))
   :cljs (defn expr-trace? [x]
           (and x (instance? ExprTrace x))))
