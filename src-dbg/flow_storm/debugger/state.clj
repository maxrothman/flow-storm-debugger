(ns flow-storm.debugger.state
  (:require [flow-storm.state-management :refer [defstate]]
            [flow-storm.debugger.ui.utils :as ui-utils]
            [clojure.java.io :as io]))

(def font-size-styles ["font-size-sm.css"
                       "font-size-md.css"
                       "font-size-lg.css"
                       "font-size-xl.css"])

(defn initial-state [{:keys [theme styles]}]
  {:flows {}
   :selected-flow-id nil
   :printers {}
   :selected-font-size-style-idx 0
   :selected-theme (case theme
                     :light :light
                     :dark  :dark
                     :auto  (ui-utils/get-current-os-theme)
                     :light)
   :extra-styles styles})

;; so linter doesn't complain
(declare state)
(declare fn-call-stats-map)
(declare flow-thread-indexers)

(defstate state
  :start (fn [config] (atom (initial-state config)))
  :stop (fn [] nil))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

(defn create-flow [flow-id form-ns form timestamp]
  ;; if a flow for `flow-id` already exist we discard it and
  ;; will be GCed

  (swap! state assoc-in [:flows flow-id] {:flow/id flow-id
                                          :flow/threads {}
                                          ;; the form that started the flow
                                          :flow/execution-expr {:ns form-ns
                                                                :form form}
                                          :timestamp timestamp}))

(defn remove-flow [flow-id]
  (swap! state update :flows dissoc flow-id))

(defn all-flows-ids []
  (keys (get @state :flows)))

(defn update-thread-info [thread-id info]
  (swap! state assoc-in [:threads-info thread-id] info))

(defn get-thread-info [thread-id]
  (get-in @state [:threads-info thread-id]))

(defn get-flow [flow-id]
  (get-in @state [:flows flow-id]))

(defn create-thread [flow-id thread-id]
  (swap! state assoc-in [:flows flow-id :flow/threads thread-id]
         {:thread/id thread-id
          :thread/curr-timeline-entry nil
          :thread/callstack-tree-hidden-fns #{}}))

(defn get-thread [flow-id thread-id]
  (get-in @state [:flows flow-id :flow/threads thread-id]))

(defn current-timeline-entry [flow-id thread-id]
  (:thread/curr-timeline-entry (get-thread flow-id thread-id)))

(defn current-idx [flow-id thread-id]
  (:idx (current-timeline-entry flow-id thread-id)))

(defn set-current-timeline-entry [flow-id thread-id entry]
  (swap! state assoc-in [:flows flow-id :flow/threads thread-id :thread/curr-timeline-entry] entry))

(defn set-current-frame [flow-id thread-id frame-data]
  (swap! state assoc-in [:flows flow-id :flow/threads thread-id :thread/curr-frame] frame-data))

(defn current-frame [flow-id thread-id]
  (get-in @state [:flows flow-id :flow/threads thread-id :thread/curr-frame]))

(defn callstack-tree-hide-fn [flow-id thread-id fn-name fn-ns]
  (swap! state update-in [:flows flow-id :flow/threads thread-id :thread/callstack-tree-hidden-fns] conj {:name fn-name :ns fn-ns}))

(defn callstack-tree-hidden? [flow-id thread-id fn-name fn-ns]
  (let [hidden-set (get-in @state [:flows flow-id :flow/threads thread-id :thread/callstack-tree-hidden-fns])]
    (contains? hidden-set {:name fn-name :ns fn-ns})))

(defn set-sytem-fully-started []
  (swap! state assoc :system-fully-started? true))

(defn system-fully-started? []
  (get @state :system-fully-started?))

(defn add-printer [form-id coord printer-data]
  (swap! state assoc-in [:printers form-id coord] printer-data))

(defn printers []
  (get @state :printers))

(defn remove-printer [form-id coord]
  (swap! state update-in [:printers form-id] dissoc coord))

(defn update-printer [form-id coord k new-val]
  (swap! state assoc-in [:printers form-id coord k] new-val))

(defn inc-font-size []
  (-> (swap! state update :selected-font-size-style-idx
             (fn [idx] (min (dec (count font-size-styles))
                            (inc idx))))
      :selected-font-size-style-idx))

(defn dec-font-size []
  (-> (swap! state update :selected-font-size-style-idx
             (fn [idx] (max 0 (dec idx))))
      :selected-font-size-style-idx))

(defn set-theme [theme]
  (swap! state assoc :selected-theme theme))

(defn rotate-theme []
  (swap! state update :selected-theme {:light :dark
                                       :dark :light}))


(defn current-stylesheets []
  (let [{:keys [selected-theme extra-styles selected-font-size-style-idx]} @state
        default-styles (str (io/resource "styles.css"))
        theme-base-styles (str (io/resource (case selected-theme
                                              :dark  "theme_dark.css"
                                              :light "theme_light.css")))
        font-size-style (-> (get font-size-styles selected-font-size-style-idx)
                            io/resource
                            str)
        extra-styles (when extra-styles
                       (str (io/as-url (io/file extra-styles))))]
    (cond-> [theme-base-styles
             default-styles
             font-size-style]
      extra-styles (conj extra-styles))))
