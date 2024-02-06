(ns flow-storm.runtime.debuggers-api
  (:require [flow-storm.runtime.indexes.api :as index-api]
            [flow-storm.runtime.indexes.protocols :as index-protos]
            [flow-storm.json-serializer :as serializer]
            #?@(:clj  [[flow-storm.utils :as utils :refer [log]]]
                :cljs [[flow-storm.utils :as utils :refer [log] :refer-macros [env-prop]]])
            [flow-storm.runtime.events :as rt-events]            
            [flow-storm.runtime.values :as rt-values :refer [reference-value! deref-value]]
            [flow-storm.runtime.taps :as rt-taps]
            [flow-storm.remote-websocket-client :as remote-websocket-client]
            #?@(:clj [[flow-storm.mem-reporter :as mem-reporter]])
            [flow-storm.tracer :as tracer]
            [clojure.string :as str]
            #?@(:clj [[hansel.api :as hansel]                      
                      [hansel.instrument.utils :as hansel-inst-utils]])
            [flow-storm.runtime.types.fn-return-trace :as fn-return-trace]
            [flow-storm.runtime.types.expr-trace :as expr-trace]))

;; TODO: build script
;; Maybe we can figure out this ns names by scanning (all-ns) so
;; we don't need to list them here
;; Also maybe just finding main is enough, we can add to it a fn
;; that returns the rest of the functions we need
(def debugger-main-ns 'flow-storm.debugger.main)

;; Utilities for long interruptible tasks

(def interruptible-tasks
  "A map from task-id -> interrupt-ch"
  (atom {}))

(defn- submit-interruptible-task! [f args]
  (let [task-id (str (utils/rnd-uuid))        
        on-progress (fn [progress] (rt-events/publish-event! (rt-events/make-task-progress-event task-id progress)))
        on-result (fn [res]
                    ;; TODO: there is a race condition here, if the result of the task
                    ;; finishes faster than we when able to return the task id and
                    ;; the remote connection add a listener to it
                    (log (utils/format "Task %s finished " task-id))
                    (rt-events/publish-event! (rt-events/make-task-result-event task-id res)))
        interrupt-fn (apply f (-> [] (into args) (into [on-progress on-result])))]
    (swap! interruptible-tasks assoc task-id interrupt-fn)    
    (log (utils/format "Submited interruptible task with task-id: %s" task-id))
    (rt-events/publish-event! (rt-events/make-task-submitted-event task-id))
    task-id))

(defn interrupt-task [task-id]  
  (let [int-fn (get @interruptible-tasks task-id)]
    (int-fn)))

(defn interrupt-all-tasks []
  (doseq [int-fn (vals @interruptible-tasks)]
    (int-fn)))

(defn runtime-config []
  {:env-kind #?(:clj :clj :cljs :cljs)
   :storm? (utils/storm-env?)
   :recording? (tracer/recording?)
   :total-order-recording? (tracer/total-order-recording?)
   :breakpoints (tracer/all-breakpoints)})

(defn val-pprint [vref opts]
  (rt-values/val-pprint vref opts))

(defn shallow-datafy [vref]
  (rt-values/shallow-datafy vref))

#?(:clj (def def-value rt-values/def-value))

(def tap-value rt-values/tap-value)

(defn get-form [form-id]
  (let [form (index-api/get-form form-id)]
    (if (:multimethod/dispatch-val form)
      (update form :multimethod/dispatch-val reference-value!)
      form)))

(def timeline-count index-api/timeline-count)

(defn- reference-frame-data! [{:keys [dispatch-val return/kind] :as frame-data}]
  (cond-> frame-data
    true             (dissoc :frame)
    dispatch-val     (update :dispatch-val reference-value!)
    true             (update :args-vec reference-value!)
    (= kind :return) (update :ret reference-value!)
    (= kind :unwind) (update :throwable reference-value!)
    true             (update :bindings (fn [bs] (mapv #(update % :value reference-value!) bs)))
    true             (update :expr-executions (fn [ee] (mapv #(update % :result reference-value!) ee)))))

(defn reference-timeline-entry! [entry]
  (case (:type entry)
    :fn-call   (update entry :fn-args reference-value!)
    :fn-return (update entry :result reference-value!)
    :fn-unwind (update entry :throwable reference-value!)
    :bind      (update entry :value reference-value!)
    :expr      (update entry :result reference-value!)))

(defn timeline-entry [flow-id thread-id idx drift]
  (some-> (index-api/timeline-entry flow-id thread-id idx drift)
          reference-timeline-entry!)) 

(defn frame-data [flow-id thread-id idx opts]
  (let [frame-data (index-api/frame-data flow-id thread-id idx opts)]
    (reference-frame-data! frame-data)))

(defn bindings [flow-id thread-id idx opts]
  (let [bs-map (index-api/bindings flow-id thread-id idx opts)]
    (reduce-kv (fn [bs s v]
                 (assoc bs s (reference-value! v)))
               {}
               bs-map)))

(defn callstack-tree-root-node [flow-id thread-id]
  (index-api/callstack-root-node flow-id thread-id))

(defn callstack-node-childs [node]
  (index-api/callstack-node-childs node))

(defn callstack-node-frame [node]
  (let [frame-data (index-api/callstack-node-frame-data node)]
    (reference-frame-data! frame-data)))

(defn fn-call-stats [flow-id thread-id]
  (let [stats (index-api/fn-call-stats flow-id thread-id)]
    (->> stats
         (mapv (fn [fstats]
                 (update fstats :dispatch-val reference-value!))))))

(defn all-fn-call-stats []
  (reduce (fn [r [flow-id thread-id]]
            (let [thread-stats (index-api/fn-call-stats flow-id thread-id)]
              (reduce (fn [rr {:keys [fn-ns fn-name cnt]}]
                        (update rr (str (symbol fn-ns fn-name)) #(+ (or % 0) cnt)))
                      r
                      thread-stats)))
          {}
          (index-api/all-threads)))

(defn find-fn-frames [flow-id thread-id fn-ns fn-name form-id]
  (let [fn-frames (index-api/find-fn-frames flow-id thread-id fn-ns fn-name form-id)
        frames (into [] (map reference-frame-data!) fn-frames)]
    frames))

(defn find-fn-call [fq-fn-call-symb from-idx opts]
  (some-> (index-api/find-fn-call fq-fn-call-symb from-idx opts)
          reference-timeline-entry!))

(defn find-flow-fn-call [flow-id]
  (some-> (index-api/find-flow-fn-call flow-id)
          reference-timeline-entry!))

(defn find-timeline-entry [{:keys [eq-val-ref] :as criteria}]
  (let [criteria (cond-> criteria
                   eq-val-ref (assoc :eq-val (deref-value eq-val-ref)))]
    (some-> (index-api/find-timeline-entry criteria)
            reference-timeline-entry!)))

(defn total-order-timeline []
  (index-api/total-order-timeline))

(defn thread-prints [print-cfg]
  (index-api/thread-prints print-cfg))

;; NOTE: this is duplicated for Clojure and ClojureScript so I could get rid of core.async in the runtime part
;;       so it can be AOT compiled without too many issues
#?(:clj
   (defn async-search-next-timeline-entry* [flow-id thread-id query-str from-idx {:keys [print-length print-level] :or {print-level 2 print-length 10}} on-progress on-result]     
     (let [thread (Thread.
                   (fn []
                     (try
                       (let [{:keys [timeline-index]} (index-api/get-thread-indexes flow-id thread-id)                             
                             found-entry (locking timeline-index
                                           (let [tl-entries (index-protos/timeline-raw-entries timeline-index from-idx nil)
                                                 total-entries (count tl-entries)]
                                             (loop [i 0
                                                    [tl-entry & tl-rest] tl-entries]
                                               (when (and tl-entry
                                                          (not (.isInterrupted (Thread/currentThread))))                        

                                                 (when (and on-progress (zero? (mod i 10000)))
                                                   (on-progress (* 100 (/ i total-entries))))

                                                 (if (or (expr-trace/expr-trace? tl-entry)
                                                         (fn-return-trace/fn-return-trace? tl-entry))

                                                   (let [result (index-protos/get-expr-val tl-entry)]
                                                     (if (str/includes? (:val-str (rt-values/val-pprint result {:print-length print-length
                                                                                                                :print-level print-level
                                                                                                                :pprint? false}))
                                                                        query-str)

                                                       ;; if matches, finish the loop the found idx
                                                       tl-entry

                                                       ;; else, keep looping
                                                       (recur (inc i) tl-rest)))

                                                   ;; else
                                                   (recur (inc i) tl-rest))))))
                             result (some-> found-entry
                                            index-protos/as-immutable
                                            reference-timeline-entry!)]                         
                         (on-result result))
                       (catch java.lang.InterruptedException _
                         (utils/log "FlowStorm search thread interrupted")))))
           interrupt-fn (fn [] (.interrupt thread))]
       (.setName thread "FlowStorm Search")
       (.start thread)
       interrupt-fn)))

#?(:cljs   
   (defn async-search-next-timeline-entry* [flow-id thread-id query-str from-idx {:keys [print-length print-level] :or {print-level 2 print-length 10}} on-progress on-result]     
     (let [interrupted (atom false)
           interrupt-fn (fn [] (reset! interrupted true))
           {:keys [timeline-index]} (index-api/get-thread-indexes flow-id thread-id)           
           tl-entries (index-protos/timeline-raw-entries timeline-index from-idx nil)
           total-entries (count tl-entries)
           search-next (fn search-next [i [tl-entry & tl-rest]]
                         (if (and tl-entry
                                  (not @interrupted))                        

                           (do
                             (when (and on-progress (zero? (mod i 10000)))
                               (on-progress (* 100 (/ i total-entries))))

                             (if (or (expr-trace/expr-trace? tl-entry)
                                     (fn-return-trace/fn-return-trace? tl-entry))
                               
                               (let [result (index-protos/get-expr-val tl-entry)]
                                 
                                 (if (str/includes? (:val-str (rt-values/val-pprint result {:print-length print-length
                                                                                                 :print-level print-level
                                                                                                 :pprint? false}))
                                                    query-str)

                                   ;; if matches, finish the loop with the found entry
                                   (on-result (some-> tl-entry
                                                      index-protos/as-immutable
                                                      reference-timeline-entry!))

                                   ;; else, keep looping
                                   (js/setTimeout search-next 0 (inc i) tl-rest)))

                               ;; else
                               (js/setTimeout search-next 0 (inc i) tl-rest)))

                           ;; else didn't found it or was interrupted, in any case call on-result with nil
                           (on-result nil)
                           ))]
       (search-next 0 tl-entries)
       interrupt-fn)))

(defn async-search-next-timeline-entry [& args]
  (submit-interruptible-task! async-search-next-timeline-entry* args))

(def discard-flow index-api/discard-flow)

(def all-flows-threads index-api/all-threads)

(defn clear-recordings []
  (let [flows-ids (->> (all-flows-threads)
                       (map first)
                       (into #{}))]
    (doseq [fid flows-ids]
      (discard-flow fid))
    
    (rt-values/clear-vals-ref-registry)))

(def flow-threads-info index-api/flow-threads-info)

(defn goto-location [flow-id {:keys [thread/id thread/idx]}]
  (rt-events/publish-event! (rt-events/make-goto-location-event flow-id id idx)))

(defn stack-for-frame [flow-id thread-id fn-call-idx]
  (index-api/stack-for-frame flow-id thread-id fn-call-idx))

(defn set-recording [enable?]
  (tracer/set-recording enable?)
  (rt-events/publish-event! (rt-events/make-recording-updated-event enable?)))

(defn toggle-recording []
  (if (tracer/recording?)
    (set-recording false)
    (set-recording true)))

(defn set-total-order-recording [x]
  (tracer/set-total-order-recording x))

(defn jump-to-last-expression-in-this-thread
  ([] (jump-to-last-expression-in-this-thread nil))
  ([flow-id]
   (let [thread-id (utils/get-current-thread-id)
         last-ex-loc (when-let [cnt (index-api/timeline-count flow-id thread-id)]
                       {:thread/id thread-id
                        :thread/name (utils/get-current-thread-name)
                        :thread/idx (dec cnt)})]
     (if last-ex-loc
       (goto-location flow-id last-ex-loc)
       (log "No recordings for this thread yet")))))

#?(:clj
   (defn unblock-thread [thread-id]     
     (tracer/unblock-thread thread-id)))

#?(:clj
   (defn unblock-all-threads []
     (tracer/unblock-all-threads)))

#?(:clj
   (defn add-breakpoint!
     ([fq-fn-symb opts] (add-breakpoint! fq-fn-symb opts (constantly true)))
     ([fq-fn-symb {:keys [disable-events?]} args-pred]
      (tracer/add-breakpoint! (namespace fq-fn-symb) (name fq-fn-symb) args-pred)
      (when-not disable-events?
        (rt-events/publish-event! (rt-events/make-break-installed-event fq-fn-symb))))))

#?(:clj
   (defn remove-breakpoint! [fq-fn-symb {:keys [disable-events?]}]
     (tracer/remove-breakpoint! (namespace fq-fn-symb) (name fq-fn-symb))

     (when-not disable-events?
       (rt-events/publish-event! (rt-events/make-break-removed-event fq-fn-symb)))))

#?(:clj
   (defn clear-breakpoints! []
     (let [brks (tracer/all-breakpoints)]
       (tracer/clear-breakpoints!)
       (doseq [[fn-ns fn-name] brks]
         (rt-events/publish-event! (rt-events/make-break-removed-event (symbol fn-ns fn-name)))))))

(defn ping [] :pong)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To be used form the repl connections ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
   (defn publish-event! [kind ev config]
     ;; do it on a different thread since we
     ;; don't want to block for event publication
     (future
       (case kind
         :clj (rt-events/publish-event! ev)

         ;; sending a event for ClojureScript from the Clojure (shadow) side is kind of
         ;; hacky. We evaluate the publish-event! expresion in the cljs runtime.
         :cljs (hansel-inst-utils/eval-in-ns-fn-cljs 'cljs-user
                                                     `(flow-storm.runtime.events/publish-event! ~ev)
                                                     config)))
     nil))

#?(:clj
   (defn instrument-var [kind var-symb {:keys [disable-events?] :as config}]
     (let [inst-fn (case kind
                     :clj  hansel/instrument-var-clj
                     :cljs hansel/instrument-var-shadow-cljs)]
       
       (log (format "Instrumenting var %s %s" var-symb config))

       (inst-fn var-symb (merge (tracer/hansel-config config)
                                config))
       
       (when-not disable-events?
         (publish-event! kind (rt-events/make-var-instrumented-event (name var-symb) (namespace var-symb)) config)))))

#?(:clj
   (defn uninstrument-var [kind var-symb {:keys [disable-events?] :as config}]
     (let [inst-fn (case kind
                     :clj  hansel/uninstrument-var-clj
                     :cljs hansel/uninstrument-var-shadow-cljs)]

       (log (format "Uninstrumenting var %s %s" var-symb config))
       (inst-fn var-symb (merge (tracer/hansel-config config)
                                config))
       (when-not disable-events?
         (publish-event! kind (rt-events/make-var-uninstrumented-event (name var-symb) (namespace var-symb)) config)))))

#?(:clj
   (defn instrument-namespaces [kind ns-prefixes {:keys [disable-events?] :as config}]
     (log (format "Instrumenting namespaces %s" (pr-str ns-prefixes)))
     (let [inst-fn (case kind
                     :clj  hansel/instrument-namespaces-clj
                     :cljs hansel/instrument-namespaces-shadow-cljs)
           {:keys [affected-namespaces]} (inst-fn ns-prefixes (merge (tracer/hansel-config config)
                                                                     config))]
       
       (when-not disable-events?
         (doseq [ns-symb affected-namespaces]
           (publish-event! kind (rt-events/make-ns-instrumented-event (name ns-symb)) config))))))

#?(:clj
   (defn uninstrument-namespaces [kind ns-prefixes {:keys [disable-events?] :as config}]
     (log (format "Uninstrumenting namespaces %s" (pr-str ns-prefixes)))
     (let [inst-fn (case kind
                     :clj  hansel/uninstrument-namespaces-clj
                     :cljs hansel/uninstrument-namespaces-shadow-cljs)
           {:keys [affected-namespaces]} (inst-fn ns-prefixes (merge (tracer/hansel-config config)
                                                                     config))]
       
       (when-not disable-events?
         (doseq [ns-symb affected-namespaces]
           (publish-event! kind (rt-events/make-ns-uninstrumented-event (name ns-symb)) config))))))

#?(:clj
   (defn all-namespaces
     ([kind] (all-namespaces kind nil))
     ([kind build-id]
      (case kind
        :clj (mapv (comp str ns-name) (all-ns))
        :cljs (hansel-inst-utils/cljs-get-all-ns build-id)))))

#?(:clj
   (defn all-vars-for-namespace
     ([kind ns-name] (all-vars-for-namespace kind ns-name nil))
     ([kind ns-name build-id]
      (case kind
        :clj (->> (ns-interns (symbol ns-name)) keys (mapv str))
        :cljs (hansel-inst-utils/cljs-get-ns-interns (symbol ns-name) build-id)))))

#?(:clj
   (defn get-var-meta
     ([kind var-symb] (get-var-meta kind var-symb {}))
     ([kind var-symb {:keys [build-id]}]
      (case kind
        :clj (-> (meta (find-var var-symb)) (update :ns (comp str ns-name)))
        :cljs (hansel-inst-utils/eval-in-ns-fn-cljs 'cljs.user `(meta (var ~var-symb)) {:build-id build-id})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils for calling by name, used by the websocket api calls ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def api-fn {:runtime-config runtime-config
             :val-pprint val-pprint
             :shallow-datafy shallow-datafy
             :get-form get-form
             :timeline-count timeline-count
             :timeline-entry timeline-entry
             :frame-data frame-data
             :bindings bindings
             :callstack-tree-root-node callstack-tree-root-node
             :callstack-node-childs callstack-node-childs
             :callstack-node-frame callstack-node-frame
             :fn-call-stats fn-call-stats
             :find-fn-frames find-fn-frames
             :find-timeline-entry find-timeline-entry
             :total-order-timeline total-order-timeline
             :thread-prints thread-prints
             :async-search-next-timeline-entry async-search-next-timeline-entry
             :discard-flow discard-flow             
             :tap-value tap-value
             :interrupt-task interrupt-task
             :interrupt-all-tasks interrupt-all-tasks
             :clear-recordings clear-recordings
             :flow-threads-info flow-threads-info
             :all-flows-threads all-flows-threads
             :stack-for-frame stack-for-frame
             :toggle-recording toggle-recording
             :set-total-order-recording set-total-order-recording
             :all-fn-call-stats all-fn-call-stats
             :find-fn-call find-fn-call
             :ping ping
             #?@(:clj
                 [:def-value def-value
                  :all-namespaces all-namespaces
                  :all-vars-for-namespace all-vars-for-namespace
                  :get-var-meta get-var-meta
                  :instrument-var instrument-var
                  :uninstrument-var uninstrument-var
                  :instrument-namespaces instrument-namespaces
                  :uninstrument-namespaces uninstrument-namespaces
                  :unblock-thread unblock-thread
                  :unblock-all-threads unblock-all-threads
                  :add-breakpoint! add-breakpoint!
                  :remove-breakpoint! remove-breakpoint!])})

(defn call-by-name [fun-key args]  
  (let [f (get api-fn fun-key)]
    (apply f args)))

#?(:clj
   (defn setup-runtime

     "Setup runtime based on jvm properties. Returns a config map."

     []
     (let [theme-prop (System/getProperty "flowstorm.theme")
           title-prop (System/getProperty "flowstorm.title")
           styles-prop (System/getProperty "flowstorm.styles")
           fn-call-limits (utils/parse-thread-fn-call-limits (System/getProperty "flowstorm.threadFnCallLimits"))
           config (cond-> {}
                    theme-prop            (assoc :theme (keyword theme-prop))
                    styles-prop           (assoc :styles styles-prop)
                    title-prop            (assoc :title  title-prop))]

       (tracer/set-recording (if (= (System/getProperty "flowstorm.startRecording") "false") false true))

       (doseq [[fn-ns fn-name l] fn-call-limits]
         (index-api/add-fn-call-limit fn-ns fn-name l))

       config))

   :cljs ;; ------------------------------------------------------------------------------------------------------------
   (defn setup-runtime

     "This is meant to be called by preloads to initialize the runtime side of things"

     []
     (println "Setting up runtime")

     (index-api/start)

     (println "Index started")

     (let [recording? (if (= (env-prop "flowstorm.startRecording") "false") false true)]
       (tracer/set-recording recording?)
       (println "Recording set to " recording?))

     (let [fn-call-limits (utils/parse-thread-fn-call-limits (env-prop "flowstorm.threadFnCallLimits"))]
       (doseq [[fn-ns fn-name l] fn-call-limits]
         (index-api/add-fn-call-limit fn-ns fn-name l)
         (println "Added function limit " fn-ns fn-name l)))

     (rt-values/clear-vals-ref-registry)
     (println "Value references cleared")

     (rt-taps/setup-tap!)
     (println "Runtime setup ready")))

#?(:clj
   (defn start-runtime [events-dispatch-fn skip-debugger-start? {:keys [skip-index-start?] :as config}]
     (let [config (merge config (setup-runtime))]

       ;; NOTE: The order here is important until we replace this code with
       ;; better component state management

       (when-not skip-index-start?
         (index-api/start))

       ;; start the debugger UI
       (when-not skip-debugger-start?
         (let [start-debugger (requiring-resolve (symbol (name debugger-main-ns) "start-debugger"))]
           (start-debugger config)))

       (rt-events/set-dispatch-fn events-dispatch-fn)

       (rt-values/clear-vals-ref-registry)

       (rt-taps/setup-tap!)

       (mem-reporter/run-mem-reporter))))

#?(:clj
   (defn remote-connect [config]

     ;; connect to the remote websocket
     (remote-websocket-client/start-remote-websocket-client
      (assoc config
             :api-call-fn call-by-name
             :on-connected (fn []
                             (let [enqueue-event! (fn [ev]
                                                    (-> [:event ev]
                                                        serializer/serialize
                                                        remote-websocket-client/send))]
                               (log "Connected to remote websocket")

                               (start-runtime enqueue-event! true config)

                               (log "Remote Clojure runtime initialized"))))))

   :cljs ;;----------------------------------------------------------------------------------------------
   (defn remote-connect [config]
     ;; connect to the remote websocket
     
     (try
       (remote-websocket-client/start-remote-websocket-client
        (assoc config               
               :api-call-fn call-by-name
               :on-connected (fn []
                               ;; subscribe and automatically push all events thru the websocket
                               ;; if there were any events waiting to be dispatched
                               (rt-events/set-dispatch-fn
                                (fn [ev]
                                  (-> [:event ev]
                                      serializer/serialize
                                      remote-websocket-client/send)))

                               (println "Debugger connection ready. Events dispatch function set and pending events pushed."))))
       (catch :default e (utils/log-error "Couldn't connect to the debugger" e))))
   )
