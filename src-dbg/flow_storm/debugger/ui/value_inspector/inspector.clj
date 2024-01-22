(ns flow-storm.debugger.ui.value-inspector.inspector
  (:require [flow-storm.debugger.ui.utils
             :as ui-utils
             :refer [event-handler button label h-box v-box border-pane]]
            [clojure.string :as str]
            [flow-storm.utils :as utils :refer [log-error]]
            [flow-storm.debugger.runtime-api :as runtime-api :refer [rt-api]]
            [flow-storm.types :as types]
            [flow-storm.debugger.state :as dbg-state]
            [flow-storm.debugger.ui.value-inspector.renderers :as renderers])
  (:import [javafx.scene Scene Node]
           [javafx.stage Stage]
           [javafx.scene.layout VBox HBox Priority]
           [javafx.geometry Orientation]
           [javafx.scene.control SplitPane]))

(declare create-value-pane)

(defn def-val [val]
  (let [val-name (ui-utils/ask-text-dialog
                  {:header "Def var with name. You can use / to provide a namespace, otherwise will be defined under [cljs.]user "
                   :body "Var name :"})]
    (when-not (str/blank? val-name)
      (runtime-api/def-value rt-api (symbol val-name) val))))

(defn- update-value-pane [{:keys [value-pane vals-stack]}]
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
                              type-lbl (label (format "Type: %s" (type (:datafied-val head))))
                              val-header-box (border-pane {} #_{:left (v-box (if-let [cnt (count (:datafied-val head))]
                                                                          [type-lbl (label (format "Count: %d" cnt))]
                                                                          [type-lbl]))
                                                           :right buttons-box}
                                                          "value-inspector-header")
                              meta-box (when-let [mp (:meta-pane head)]
                                         (v-box [(label "Meta")
                                                 mp]))
                              val-pane (border-pane {:top val-header-box
                                                     :center (:value-pane head)})
                              val-full-pane (if meta-box
                                              (let [meta-val-split (doto (SplitPane.)
                                                                     (.setOrientation (Orientation/VERTICAL))
                                                                     (.setDividerPosition 0 0.3))]
                                                (.addAll (.getItems meta-val-split) [meta-box val-pane])
                                                meta-val-split)
                                              val-pane)]
                          (VBox/setVgrow val-pane Priority/ALWAYS)
                          val-full-pane)]
    (HBox/setHgrow value-full-pane Priority/ALWAYS)

    (.clear (.getChildren value-pane))
    (.addAll (.getChildren value-pane) [value-full-pane])))

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
                       (update-value-pane ctx)
                       (update-stack-bar-pane ctx)))))
                 (reverse @vals-stack))))

(defn- make-stack-frame [{:keys [find-and-jump-same-val] :as ctx} stack-txt datafied-val]
  {:stack-txt stack-txt
   :value-pane (create-value-pane ctx datafied-val)
   :meta-pane (when-let [val-meta (meta datafied-val)]
                (create-value-pane ctx val-meta))
   ;; :def-fn (fn [] (def-val vref))
   ;; :tap-fn (fn [] (runtime-api/tap-value rt-api vref))
   ;; :find-val-fn (when find-and-jump-same-val
   ;;                (fn [backward?] (find-and-jump-same-val vref {:comp-fn-key :identity} backward?)))
   :datafied-val datafied-val})

#_(defn make-item [stack-key v]
  (let [browsable-val? (types/value-ref? v)
        item {:browsable-val? browsable-val?
              :stack-txt (if (types/value-ref? stack-key)
                           (-> stack-key meta :val-preview)
                           (pr-str stack-key))}]
    (if browsable-val?
      (assoc item
             :val-ref v
             :val-txt (-> v meta :val-preview))

      (assoc item
             :val-ref v
             :val-txt (pr-str v)))))

(defn- create-value-pane [ctx datafied-val]
  (let [on-selected (fn [val]
                      (println "@@@ selected " val))
        renderers (renderers/renderers-for-val datafied-val)
        val-pane (h-box [])
        rend-combo (ui-utils/combo-box {:items (keys renderers)
                                        :cell-factory-fn (fn [cb rend-key]
                                                           (label (str rend-key)))
                                        :on-showing-fn (fn [cb] (println "@@@ showing" cb))
                                        :on-change-fn  (fn [_ rend-key]
                                                         (println "@@@@ changing " rend-key)
                                                         (let [{:keys[renderer-fn]} (get renderers rend-key)
                                                               rend-pane (renderer-fn datafied-val on-selected)]
                                                           (-> val-pane .getChildren .clear)
                                                           (-> val-pane
                                                               .getChildren
                                                               (.addAll (into-array Node [rend-pane])))))})]
    val-pane))

(defn- create-inspector-pane [datafied-val {:keys [find-and-jump-same-val]}]
  (let [*vals-stack (atom nil)
        stack-bar-pane (doto (h-box [] "value-inspector-stack-pane")
                         (.setSpacing 5))
        main-split (doto (SplitPane.)
                   (.setOrientation (Orientation/HORIZONTAL))
                   (.setDividerPosition 0 0.5))
        ctx {:stack-bar-pane stack-bar-pane
             :value-pane value-pane
             :vals-stack *vals-stack
             :find-and-jump-same-val find-and-jump-same-val}

        mp (border-pane {:top stack-bar-pane
                         :center main-split}
                        "value-inspector-main-pane")]


    (swap! *vals-stack conj (make-stack-frame ctx "/" datafied-val))
    (update-value-pane ctx)
    (update-stack-bar-pane ctx)

    mp))

(defn create-inspector [datafied-val opts]
  (try
    (let [scene (Scene. (create-inspector-pane datafied-val opts) 1000 600)
          stage (doto (Stage.)
                  (.setTitle "FlowStorm value inspector")
                  (.setScene scene))]

      (dbg-state/register-and-init-stage! stage)

      (-> stage .show))

    (catch Exception e
      (log-error "UI Thread exception" e))))

(comment
  (javafx.embed.swing.JFXPanel.)
  (create-inspector :hello {})
  )
