(ns flow-storm.debugger.ui.value-inspector
  (:require [flow-storm.debugger.ui.utils
             :as ui-utils
             :refer [event-handler button label h-box v-box border-pane]]
            [clojure.string :as str]
            [flow-storm.utils :as utils :refer [log-error]]
            [flow-storm.runtime.values :as rt-values] ;; THIS IS JUST FOR TESTING, CAN'T BE HERE
            [flow-storm.debugger.runtime-api :as runtime-api :refer [rt-api]]
            [flow-storm.types :as types]
            [flow-storm.debugger.state :as dbg-state]
            [flow-storm.debugger.ui.value-renderers :as renderers])
  (:import [javafx.scene Scene]
           [javafx.stage Stage]
           [javafx.scene.layout VBox HBox Priority]
           [javafx.geometry Orientation]
           [javafx.scene.control TextInputDialog SplitPane]
           [javafx.scene.web WebView]

           [javafx.beans.value ChangeListener]
           [javafx.concurrent Worker$State]))


(declare create-value-pane)

(defn def-val [val]
  (let [val-name (ui-utils/ask-text-dialog
                  {:header "Def var with name. You can use / to provide a namespace, otherwise will be defined under [cljs.]user "
                   :body "Var name :"})]
    (when-not (str/blank? val-name)
      (runtime-api/def-value rt-api (symbol val-name) val))))

(defn- update-vals-panes [{:keys [main-split vals-stack]}]
  (let [[head prev & _] @vals-stack
        value-full-pane (let [def-btn (button :label "def"
                                              :on-click (:def-fn head))
                              tap-btn (button :label "tap"
                                              :on-click (:tap-fn head))
                              val-prev-btn (when-let [fvf (:find-val-fn head)]
                                             (ui-utils/icon-button :icon-name "mdi-ray-end-arrow"
                                                                   :on-click (fn [] (fvf true))
                                                                   :tooltip "Find the prev expression that contains this value"))
                              val-next-btn (when-let [fvf (:find-val-fn head)]
                                             (ui-utils/icon-button :icon-name "mdi-ray-start-arrow"
                                                                   :on-click (fn [] (fvf false))
                                                                   :tooltip "Find the next expression that contains this value"))
                              buttons-box (doto (h-box (cond-> [def-btn tap-btn]
                                                         val-prev-btn (conj val-prev-btn)
                                                         val-next-btn (conj val-next-btn)))
                                            (.setSpacing 5))
                              type-lbl (label (format "Type: %s" (-> head :shallow-val :val/type)))
                              val-header-box (border-pane {:left (v-box (if-let [cnt (-> head :shallow-val :total-count)]
                                                                          [type-lbl (label (format "Count: %d" cnt))]
                                                                          [type-lbl]))
                                                           :right buttons-box}
                                                          "value-inspector-header")
                              meta-box (when-let [mp (:meta-pane head)]
                                         (v-box [(label "Meta")
                                                 mp]))
                              val-pane (border-pane {:top val-header-box
                                                     :center (:val-pane head)})
                              val-full-pane (v-box (if meta-box
                                                     [meta-box val-pane]
                                                     [val-pane]))]
                          (VBox/setVgrow val-pane Priority/ALWAYS)
                          val-full-pane)]
    (HBox/setHgrow value-full-pane Priority/ALWAYS)

    (.clear (.getItems main-split))
    (.addAll (.getItems main-split) (if prev
                                    (let [prev-pane (doto (h-box [(:val-pane prev)])
                                                      (.setId "prev-pane"))]
                                      [prev-pane value-full-pane])
                                    [value-full-pane]))))

(defn- drop-stack-to-frame [{:keys [vals-stack]} val-frame]
  (swap! vals-stack
         (fn [stack]
           (loop [[fr :as stack] stack]
             (if (= fr val-frame)
               stack
               (recur (pop stack)))))))

(defn- update-stack-bar-pane [{:keys [stack-bar-pane vals-stack] :as ctx}]
  (.clear (.getChildren stack-bar-pane))
  (.addAll (.getChildren stack-bar-pane)
           (mapv (fn [{:keys [stack-txt] :as val-frame}]
                   (doto (button :label stack-txt :classes ["stack-bar-btn"])
                     (.setOnAction
                      (event-handler
                       [_]
                       (drop-stack-to-frame ctx val-frame)
                       (update-vals-panes ctx)
                       (update-stack-bar-pane ctx)))))
                 (reverse @vals-stack))))

(defn- make-stack-frame [{:keys [find-and-jump-same-val] :as ctx} stack-txt vref]
  (let [{:keys [val/shallow-meta] :as shallow-val} (runtime-api/shallow-val rt-api vref)]
    {:stack-txt stack-txt
     :val-pane (create-value-pane ctx shallow-val)
     :meta-pane (when shallow-meta (create-value-pane ctx shallow-meta))
     :def-fn (fn [] (def-val vref))
     :tap-fn (fn [] (runtime-api/tap-value rt-api vref))
     :find-val-fn (when find-and-jump-same-val
                    (fn [backward?] (find-and-jump-same-val vref {:comp-fn-key :identity} backward?)))
     :shallow-val shallow-val}))

(defn make-item [stack-key v]
  (let [browsable-val? (types/value-ref? v)
        item {:browsable-val? browsable-val?
              :stack-txt (if (types/value-ref? stack-key)
                           (:val-str (runtime-api/val-pprint rt-api stack-key {:print-level 4 :pprint? false :print-length 20}))
                           (pr-str stack-key))}]
    (if :browsable-val?
      (assoc item
             :val-ref v
             :val-txt (:val-str (runtime-api/val-pprint rt-api v {:print-level 4 :pprint? false :print-length 20})))

      (assoc item
             :val-ref v
             :val-txt (pr-str v)))))

(defn- create-value-pane [ctx shallow-val]
  (let [on-selected (fn [{:keys [stack-txt val-ref]} prev-pane?]
                      (let [new-frame (make-stack-frame ctx stack-txt val-ref)]
                        (when prev-pane?
                          (swap! (:vals-stack ctx) pop))

                        (swap! (:vals-stack ctx) conj new-frame)
                        (update-vals-panes ctx)
                        (update-stack-bar-pane ctx)))
        renderer-val (case (:val/kind shallow-val)
                       :object (:val/str shallow-val)
                       :map (->> (:val/map-entries shallow-val)
                                 (map (fn [[k v]]
                                        [(make-item "<key>" k) (make-item k v)])))
                       :seq (map-indexed (fn [i v] (make-item i v)) (:val/page shallow-val)))]

    (case (:val/kind shallow-val)
      :object (h-box [(label (:val/str shallow-val))])
      :map (renderers/create-map-browser-pane renderer-val on-selected)
      :seq (let [load-next-page (when (:val/more shallow-val)
                                  (fn load-next [more-ref]
                                    (let [{:keys [page/offset val/page val/more]} (runtime-api/shallow-val rt-api more-ref)
                                          new-page (map-indexed
                                                    (fn [i v]
                                                      (make-item (+ offset i) v))
                                                    page)]
                                      {:page new-page
                                       :load-next (partial load-next more)})))]
             (renderers/create-seq-browser-pane renderer-val
                                                (when load-next-page (partial load-next-page (:val/more shallow-val)))
                                                on-selected)))))

(defn- create-inspector-pane [vref {:keys [find-and-jump-same-val]}]
  (let [*vals-stack (atom nil)
        stack-bar-pane (doto (h-box [] "value-inspector-stack-pane")
                         (.setSpacing 5))
        main-split (doto (SplitPane.)
                   (.setOrientation (Orientation/HORIZONTAL))
                   (.setDividerPosition 0 0.5))
        ctx {:stack-bar-pane stack-bar-pane
             :main-split main-split
             :vals-stack *vals-stack
             :find-and-jump-same-val find-and-jump-same-val}

        mp (border-pane {:top stack-bar-pane
                         :center main-split}
                        "value-inspector-main-pane")]


    (swap! *vals-stack conj (make-stack-frame ctx "/" vref))
    (update-vals-panes ctx)
    (update-stack-bar-pane ctx)

    mp))

(defn portal-open [{:keys [options] :as args}]
  (let [make-atom (requiring-resolve 'portal.runtime.jvm.client/make-atom)
        rt-sessions @(requiring-resolve 'portal.runtime/sessions)
        portal     (make-atom (random-uuid))
        session-id (:session-id portal)]
    (swap! rt-sessions update-in [session-id :options] merge options)
    portal))

(defonce portal-session nil)

(defn create-portal-pane [val _]
  (let [wv (WebView.)
        _ (println "CREATED WEBVIEW")
        wv-eng (.getEngine wv)
        _ (println "CREATED WEBENGINE")
        p-clear (requiring-resolve 'portal.api/clear)
        p-submit (requiring-resolve 'portal.api/submit)
        session-id (.toString portal-session)]

    #_(-> wv-eng .getLoadWorker .stateProperty
        (.addListener
         (proxy [ChangeListener] []
           (changed [_ prev-val new-val]
             (when (= new-val Worker$State/SUCCEEDED)
               (let [window (.executeScript wv-eng "window")
                     connector (.setMember window
                                           "javaConnector"
                                           (reify ErrP
                                             (handleError [_ msg] (println "@@@ ERR" msg))))]))))))
    #_(println "setup javaConnector ")
    #_(.executeScript wv-eng
                    (str
                     "window.onerror = function(message, source, lineno, colno, error) {"
                     "    javaConnector.handleError(message, source, lineno, colno, error);"
                     "    return true;"
                     "};"))
    #_(println "setup error handler ")

    (.load wv-eng (format "http://localhost:7723?%s" session-id))

    (println "LOADED")
    (p-clear)
    (println "Cleared")
    (p-submit val)
    (println "Submitted")
    wv))
#_((requiring-resolve 'portal.api/clear))
#_((requiring-resolve 'portal.api/submit) {:a 1 :b 10})
#_(#'portal.runtime/var->name #'pr-str)
(defn setup-portal []
  (let [jvm-server-start (requiring-resolve 'portal.runtime.jvm.launcher/start)
        register!        (requiring-resolve 'portal.runtime/register!)
        _ (register! {:name})
        _ (jvm-server-start {:port 7723})
        p-session (portal-open {})]
    (alter-var-root #'portal-session (constantly p-session))))

(var- #'setup-portal)
(defn create-inspector [vref opts]
  (try
    (let [_ (println "@@@@ vref" vref)
          scene (Scene. (create-inspector-pane vref opts) 1000 600)
          stage (doto (Stage.)
                  (.setTitle "FlowStorm value inspector")
                  (.setScene scene))]

      (dbg-state/register-and-init-stage! stage)

      (-> stage .show))

    (catch Exception e
      (log-error "UI Thread exception" e))))

(comment
  (setup-portal)
  (ui-utils/run-later
    (let [vref 15
          val (rt-values/deref-value vref)
          scene (Scene. (create-portal-pane val {}) 1000 600)
          stage (doto (Stage.)
                  (.setTitle "FlowStorm value inspector")
                  (.setScene scene))]

      (dbg-state/register-and-init-stage! stage)

      (-> stage .show)))

  (require '[portal.api :as p])
  (def p (p/open))
  (p/submit {:hello [{:world [1 2 3]}]})
  (p/submit @flow-storm.debugger.state/state)
  (p/submit {:a (range)})
  (p/sessions) ({:session-id #uuid "8ff1776b-1cc9-4d24-bb09-5cb2af01d3d5"})

  (p/clear)
  )
