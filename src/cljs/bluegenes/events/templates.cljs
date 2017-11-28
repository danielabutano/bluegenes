(ns bluegenes.events.templates
  (:require [re-frame.core :refer [reg-event-db reg-event-fx]]
            [bluegenes.effects :as fx]
            [imcljs.fetch :as fetch]))


; The following is the general data structure of how template events are handled in app-db
{:templates {
             ; This is a "cursor" to whatever template is in view
             :selected-template :MyFavouriteTemplate
             ;
             ; This is a map of any edits made to the default values of a template
             :edits {:SomeTemplateFromIntermine {:query "..."
                                                 :preview "..."
                                                 :count "..."
                                                 :fetching? false}
                     :MyFavouriteTemplate {
                                           ; The (potentially edited) query
                                           :query "..."
                                           ; The first five results from the query
                                           :preview "..."
                                           ; The total row count of the query
                                           :count "..."
                                           ; Is the preview currently being fetched?
                                           :fetching? false}}
             :filters {
                       ; A set of tags used for filtering the list of templates
                       :tags #{"im:aspect:Function" "im:aspect:Homology"}
                       ; A string used for filtering the list of templates
                       :text "melanogaster"
                       }}}

; A user has selected a template. Templates are editable, so
; we copy its query to a new place in app-db where we can store the changes
(reg-event-fx ::select-template
              (fn [{db :db} [_ [template-kw details]]]
                {:db (-> db
                         ; Store the "cursor" as seen in the example above
                         (assoc-in [:templates :selected-template] template-kw)
                         (cond->
                           ; If there's no query in the 'editable' map of templates
                           ; (meaning a user hasn't yet viewed the template)...
                           (empty? (not-empty (get-in db [:templates :edits template-kw :query])))
                           ; Then copy over the query
                           (assoc-in [:templates :edits template-kw :query] details)))
                 ; Fetch the preview for the template
                 :dispatch [::fetch-preview template-kw details]}))

(reg-event-fx ::fetch-preview
              (fn [{db :db} [_ template-kw details]]
                ; TODO: This is too stateful. 'service' should be passed in, not fetched from db.
                (let [service (get-in db [:mines (:current-mine db) :service])]
                  {:db (update-in db [:templates :edits template-kw]
                                  ; Clear any data from the previous query results
                                  assoc :preview nil :fetching? true)
                   ; Fetch a preview for the query
                   :im-chan {:on-success [::store-preview template-kw]
                             :chan (fetch/table-rows
                                     service
                                     details
                                     {:size 5})}})))

; Store the results of the preview
(reg-event-db ::store-preview
              (fn [db [_ template-kw response]]
                (update-in db [:templates :edits template-kw] assoc
                           :preview response
                           :fetching? false)))

(reg-event-db ::update-constraint
              (fn [db [_ template-kw idx con]]
                (assoc-in db [:templates :edits template-kw :query :where idx] con)))

; Replace the query in the templates edit tree with the original query from the template
(reg-event-db ::reset-template
              (fn [db [_ current-mine template-kw]]
                (let [original-query (get-in db [:assets :templates current-mine template-kw])]
                  (assoc-in db [:templates :edits template-kw :query] original-query))))

; Set the string of the text filter
(reg-event-db ::set-filter-text
              (fn [db [_ text]]
                (assoc-in db [:templates :filters :text] text)))

; Add or remove tags from the tag filter
(reg-event-db ::set-filter-tag
              (fn [db [_ tag]]
                (let [existing (get-in db [:templates :filters :tags] #{})]
                  (if (contains? existing tag)
                    ; Remove the tag from the filter set
                    (update-in db [:templates :filters :tags] (comp set (partial remove #{tag})))
                    ; Add the tag to the filter set
                    (update-in db [:templates :filters :tags] (comp set conj) tag)))))

; Store the query and hand it off to the results page
(reg-event-fx ::run
              (fn [{db :db} [_ query]]
                {:dispatch-n [[:bluegenes.events/store-query query]
                              [:results/set-query
                               ; TODO: This is too stateful. 'mine-name' should be passed in, not fetched from db.
                               {:source (:current-mine db)
                                :type :query
                                :value query}]]
                 :navigate (str "#/results")}))

