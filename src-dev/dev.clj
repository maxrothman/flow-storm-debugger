(ns dev

  "A bunch of utilities to help with development.

  After loading this ns you can :

  - `start-local` to start the UI and runtime
  - `start-remote` to run only the UI and connect it to a remote process. Looks at the body for config.
  - `stop` for gracefully stopping the system
  - `refresh` to make tools.namespace unmap and reload all the modified files"

  (:require [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.main :as main]
            [flow-storm.debugger.state :as dbg-state]
            [hansel.api :as hansel]
            [flow-storm.api :as fs-api]
            [flow-storm.runtime.indexes.api :as index-api]
            [flow-storm.runtime.indexes.timeline-index :as timeline-index]
            [flow-storm.tracer :as tracer]
            [flow-storm.utils :refer [log-error log]]
            [clojure.tools.namespace.repl :as tools-namespace-repl :refer [set-refresh-dirs disable-unload! disable-reload!]]
            [flow-storm.form-pprinter :as form-pprinter]
            [dev-tester]
            [flow-storm.fn-sampler.core :as sampler]
            [flow-storm.utils :as utils]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [flow-storm.runtime.indexes.protocols :as index-protos]))

(javafx.embed.swing.JFXPanel.)

(comment
  (add-tap (bound-fn* println))
  )

#_(Thread/setDefaultUncaughtExceptionHandler
   (reify
     Thread$UncaughtExceptionHandler
     (uncaughtException [_ _ throwable]
       (tap> throwable)
       (log-error "Unhandled exception" throwable))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for reloading everything ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn spec-instrument-state []
  (add-watch
   dbg-state/state
   :spec-validator
   (fn [_ _ _ s]
     (when-not (s/valid? ::dbg-state/state s)
       (s/explain ::dbg-state/state s))))
  nil)

(comment
  (remove-watch state :spec-validator)
  )

(defn start-local []
  (fs-api/local-connect {:theme :ligth})
  (spec-instrument-state))


(defn start-remote []

  (main/start-debugger {:port 9000
                        :repl-type :shadow
                        :build-id :browser-repl})
  (spec-instrument-state))

(defn stop []
  (fs-api/stop))

(defn after-refresh []
  (alter-var-root #'utils/out-print-writer (constantly *out*))
  (log "Refresh done"))

(defn refresh []
  (let [running? dbg-state/state]
    (when running?
      (log "System is running, stopping first ...")
      (stop))
    (tools-namespace-repl/refresh :after 'dev/after-refresh )))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Playing at the repl ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(comment

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Vanilla FlowStorm ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  (fs-api/instrument-namespaces-clj
   #{"dev-tester"}
   {:disable #{} #_#{:expr-exec :anonymous-fn :bind}})

  (fs-api/uninstrument-namespaces-clj #{"dev-tester"})

  #rtrace (dev-tester/boo [2 "hello" 6])

  ;;;;;;;;;;
  ;; Docs ;;
  ;;;;;;;;;;

  (def r (sampler/sample
             {:result-name "dev-tester-flow-docs-0.1.0"
              :inst-ns-prefixes #{"dev-tester"}
              :verbose? true
              :print-unsampled? true}
           (dev-tester/boo [1 "hello" 6])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Example expressions to generate trace data ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defn bar [a b] (+ a b))
  (defn foo [a b] (let [c (+ a b)] (bar c c)))

  (doall (pmap (fn [i] (foo i (inc i))) (range 4)))

  (dev-tester/boo [1 "hello" 4])

  (flow-storm.api/continue)

  (defn my-sum [a b] (+ a b))

  (doall (pmap (fn my-sum [i] (+ i i)) (range 4)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Querying indexes programatically ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (index-api/print-threads)
  (index-api/select-thread nil 18)
  (index-api/print-forms)


  ;; Synthesizing all the spec information for parameters that flow into a function
  (defn fn-signatures [fn-ns fn-name]
    (let [[flow-id thread-id] @index-api/selected-thread
          frames (index-api/all-frames flow-id thread-id (fn [fns fname _ _]
                                                           (and (= fn-name fname)
                                                                (= fn-ns fns))))
          signature-types (->> frames
                               (reduce (fn [coll-samples frame]
                                         (conj coll-samples (mapv type (:args-vec frame))))
                                       #{}))]
      signature-types))

  (fn-signatures "dev-tester" "factorial")
  (fn-signatures "dev-tester" "other-function")

  ;; Visualization lenses over traces: say I have a loop-recur process in which I am computing
  ;; new versions of an accumulated data structure, but I want to see only some derived data
  ;; instead of the entire data-structure (like, a visualization based on every frame of the loop).
  (defn frame-similar-values [idx]
    (let [[flow-id thread-id] @index-api/selected-thread
          {:keys [fn-call-idx coord]} (index-api/timeline-entry flow-id thread-id idx :at)
          {:keys [expr-executions]} (index-api/frame-data flow-id thread-id fn-call-idx {:include-exprs? true})]

      (->> expr-executions
           (reduce (fn [coll-vals expr-exec]
                     (if (= coord (:coord expr-exec))
                       (conj coll-vals (:result expr-exec))
                       coll-vals))
                   []))))

  (frame-similar-values (dec 161)) ;; sum

  ;; Create a small debugger for the repl
  ;; -------------------------------------------------------------------------------------------

  (require '[flow-storm.form-pprinter :as form-pprinter])
  (def idx (atom 0))

  (defn show-current []
    (let [[flow-id thread-id] @index-api/selected-thread
          {:keys [type fn-ns fn-name coord fn-call-idx result] :as idx-entry} (index-api/timeline-entry flow-id thread-id @idx :at)
          {:keys [form-id]} (index-api/frame-data flow-id thread-id fn-call-idx {})
          {:keys [form/form]} (index-api/get-form form-id)]
      (case type
        :fn-call (let [{:keys [fn-name fn-ns]} idx-entry]
                   (println "Called" fn-ns fn-name))
        (:expr :fn-return) (let [{:keys [coord result]} idx-entry]
                             (form-pprinter/pprint-form-hl-coord form coord)
                             (println "\n")
                             (println "==[VAL]==>" (utils/colored-string result :yellow))))))

  (defn step-next []
    (swap! idx inc)
    (show-current))

  (defn step-prev []
    (swap! idx dec)
    (show-current))

  ;; use the debugger
  (index-api/print-threads)
  (index-api/select-thread nil 18)

  (step-next)
  (step-prev)

  (defn foo []
  (let [a (+ 1 2)]
    (throw (Exception. "damn"))
    (+ a 3)))

(defn bar []
  (try
    (foo)
    (catch Exception e 45)))

(defn blabla []
  (+ 3 4 (* 3 4)))

(defn baz []
  (+ (bar) (blabla) 4))

(println (str (:timeline-index (index-api/get-thread-indexes nil 18))))
(require '[flow-storm.runtime.indexes.protocols :as index-protos])
(index-protos/tree-childs-indexes (:timeline-index (index-api/get-thread-indexes nil 18)) 1)

(type (:fn-args (index-protos/timeline-entry  (:timeline-index (index-api/get-thread-indexes nil 18)) 0 :at)))
  )

(require '[clojure.datafy :refer [datafy nav]])

(defn generate-db
  "Generate a random database of users and departments.
   It is ENTIRELY possible for this database to contain
   circular links, but that is not a problem with the lazy
   nature of datafy/nav."
  []
  (let [user-ids (take 5 (shuffle (range 100)))
        department-ids (take 3 (shuffle (range 100)))
        new-user (fn [id]
                   (let [department-id (rand-nth department-ids)
                         manager-id (rand-nth user-ids)]
                     {:id id
                      :name (str "User " id)
                      :department-id department-id
                      :manager-id manager-id}))
        new-department (fn [id]
                         (let [deptartment-head-id (rand-nth user-ids)]
                           {:id id
                            :name (str "Department " id)
                            :department-head-user-id deptartment-head-id}))]
    {:users (zipmap user-ids (map new-user user-ids))
     :departments (zipmap department-ids (map new-department department-ids))}))

(comment
  ;sample generated db
  {:users
   {65 {:id 65, :name "User 65", :department-id 96, :manager-id 58},
    58 {:id 58, :name "User 58", :department-id 85, :manager-id 58}},
   :departments
   {96 {:id 96, :name "Department 96", :department-head-user-id 65},
    85 {:id 85, :name "Department 85", :department-head-user-id 65}}})


(defn lookup-user
  "Convenience function to get user by id"
  [db id]
  (get-in db [:users id]))

(defn lookup-department
  "Convenience function to get department by id"
  [db id]
  (get-in db [:departments id]))

(declare datafy-ready-user)
(declare datafy-ready-department)

(defn navize-user
  [db user]
  (when user
    (with-meta
     user
     {'clojure.core.protocols/nav (fn [coll k v]
                                    (case k
                                      ;; by returning a datafy-ed object,
                                      ;; the db is propagated to further datafy/nav calls
                                      :manager-id (datafy-ready-user db (lookup-user db v))
                                      :department-id (datafy-ready-department db (lookup-department db v))
                                      v))})))

(defn navize-department
  [db department]
  (when department
    (with-meta
     department
     {'clojure.core.protocols/nav (fn [coll k v]
                                    (case k
                                      :department-head-user-id (datafy-ready-user db (lookup-user db v))
                                      v))})))

(defn datafy-ready-user
  [db user]
  (when user
    (with-meta
     user
     {'clojure.core.protocols/datafy (fn [x] (navize-user db x))})))

(defn datafy-ready-department
  [db department]
  (when department
    (with-meta
     department
     {'clojure.core.protocols/datafy (fn [x] (navize-department db x))})))

;--------------------------------------------------------------
; The rest of the code creates the db and navs around the graph
;--------------------------------------------------------------
(comment
  (def db (generate-db))
  (def user1 (lookup-user db (-> db :users keys first)))
  (tap> (datafy (datafy-ready-user db user1)))

  )
