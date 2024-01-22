(ns flow-storm.debugger.ui.value-inspector.renderers
  (:require [clojure.string :as str]
            [flow-storm.debugger.ui.utils
             :as ui-utils
             :refer [event-handler button label v-box h-box table-view icon-button]])
  (:import [javafx.scene.layout HBox VBox Priority]))

(def renderers-registry (atom {}))

(defprotocol RendererVal
  (preview [v]))

(extend-protocol RendererVal
  Object
  (preview [v] (binding [*print-level* 3
                         *print-length* 3]
                 (pr-str v))))

(defn register-renderer! [id-key pred render-fn]
  (swap! renderers-registry assoc id-key {:pred pred :render-fn render-fn}))

(defn renderers-for-val [v]
  (reduce-kv (fn [rr idk {:keys [pred] :as r}]
               (if (pred v)
                 (assoc rr idk r)
                 rr))
             {}
             @renderers-registry))

(defn renderer [id-key]
  (get @renderers-registry id-key))

#_(defn create-browsable-node [{:keys [val-txt] :as item} on-selected]
  (let [click-handler (event-handler
                       [mev]
                       (on-selected item (node-in-prev-pane? (.getSource mev))))
        lbl (doto (label val-txt "link-lbl")
              (.setOnMouseClicked click-handler))]
    {:node-obj lbl
     :click-handler click-handler}))

#_(defn make-node [{:keys [browsable-val? val-txt nav-ref nav-key stack-txt] :as item} on-selected]
  (let [{:keys [node-obj click-handler]} (if browsable-val?
                                           (create-browsable-node item on-selected)
                                           {:node-obj (label val-txt)})]
    {:node-obj (if nav-ref
                 (let [nav-btn (icon-button :icon-name "mdi-arrow-right"
                                            :tooltip "Navigate using datafy/nav")]
                   (.setOnAction nav-btn
                                 (event-handler [mev]
                                   (on-selected {:stack-txt (format "-{%s %s}->" nav-key stack-txt)
                                                 :val-ref nav-ref}
                                                (node-in-prev-pane? (.getSource mev)))))
                   (doto (h-box [node-obj nav-btn])
                     (.setSpacing 5)))
                 node-obj)
     :click-handler click-handler}))

(defn print-val [v _]
  (label (pr-str v)))

(defn map-browser [map-val on-selected]
  (let [{:keys [table-view-pane table-view]}
        (table-view {:columns ["Key" "Value"]
                     :cell-factory-fn (fn [_ elem]
                                        (label (preview elem))
                                        #_(:node-obj (make-node elem on-selected)))
                     #_:search-predicate #_(fn [[k-item v-item] search-str]
                                         (boolean
                                          (or (str/includes? (:val-txt k-item) search-str)
                                              (str/includes? (:val-txt v-item) search-str))))
                     :items (seq map-val)})]
    (VBox/setVgrow table-view Priority/ALWAYS)
    (VBox/setVgrow table-view-pane Priority/ALWAYS)
    (HBox/setHgrow table-view-pane Priority/ALWAYS)
    table-view-pane))

#_(defn create-seq-browser-pane [seq-page load-next-page on-selected]
  (let [{:keys [list-view-pane add-all]} (ui-utils/list-view
                                          {:editable? false
                                           :cell-factory-fn (fn [list-cell item]
                                                              (let [{:keys [node-obj click-handler]} (make-node item on-selected)]
                                                                (.setText list-cell nil)
                                                                (.setGraphic list-cell node-obj)
                                                                ;; the node-obj will already handle the click
                                                                ;; but we also add the handler to the list-cell
                                                                ;; so one single click executes the action instead of
                                                                ;; selecting the cell
                                                                (.setOnMouseClicked list-cell click-handler)))
                                           :search-predicate (fn [item search-str]
                                                               (str/includes? (:val-txt item) search-str))})
        more-button (when load-next-page (button :label "More.."))
        arm-more-button (fn arm-more-button [load-next]
                          (doto more-button
                            (.setOnAction (event-handler
                                           [_]
                                           (let [next-page (load-next)]
                                             (add-all (:page next-page))
                                             (if-let [loadn (:load-next next-page)]
                                               (arm-more-button loadn)
                                               (doto more-button
                                                 (.setDisable true)
                                                 (.setOnAction (event-handler [_])))))))))

        container (v-box (cond-> [list-view-pane]
                           more-button (conj more-button)))]
    (VBox/setVgrow list-view-pane Priority/ALWAYS)
    (HBox/setHgrow container Priority/ALWAYS)
    (add-all seq-page)
    (when load-next-page (arm-more-button load-next-page))
    container))

(register-renderer! :map-browser map? map-browser)
(register-renderer! :print-val   any? print-val)
