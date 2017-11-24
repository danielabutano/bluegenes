(ns bluegenes.events.templates
  (:require [re-frame.core :refer [reg-event-db reg-event-fx]]))

(reg-event-db ::select-template
              (fn [db [_ [template-kw details]]]
                (-> db
                    (assoc-in [:templates :selected-template] template-kw)
                    (cond->
                      (empty? (not-empty (get-in db [:templates :edits template-kw])))
                      (assoc-in [:templates :edits template-kw] details)))))

(reg-event-db ::update-constraint
              (fn [db [_ template-kw idx con]]
                (assoc-in db [:templates :edits template-kw :where idx] con)))

(reg-event-db ::reset-template
              (fn [db [_ template-kw idx con]]
                (update-in db [:templates :edits] dissoc template-kw)))

(reg-event-db ::set-filter-text
              (fn [db [_ text]]
                (assoc-in db [:templates :filters :text] text)))

(reg-event-db ::set-filter-tag
              (fn [db [_ tag]]
                (let [existing (get-in db [:templates :filters :tags] #{})]
                  (if (contains? existing tag)
                    ; Remove the tag from the filter set
                    (update-in db [:templates :filters :tags] (comp set (partial remove #{tag})))
                    ; Add the tag to the filter set
                    (update-in db [:templates :filters :tags] (comp set conj) tag)))))