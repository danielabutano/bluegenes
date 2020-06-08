(ns bluegenes.components.search.typeahead
  (:require [reagent.core :as reagent]
            [reagent.dom :as dom]
            [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [dommy.core :as dommy :refer-macros [sel1]]
            [bluegenes.route :as route]
            [oops.core :refer [oget]]))

(defn navigate-to-report
  "Navigate to the report page for the given item and reset the UI"
  [{:keys [type id] :as _item}]
  (dispatch [:search/reset-selection])
  (dispatch [::route/navigate ::route/report {:type type, :id id}]))

(defn navigate-to-full-results
  "Navigate to the full results page. duh."
  []
  (let [term @(subscribe [:search-term])
        evt [::route/navigate ::route/search]]
    (dispatch (cond-> evt (some? term) (into [nil {:keyword term}])))
    (when (some? term)
      (dispatch [:search/full-search term]))))

(defn suggestion
  "The UI element and behaviour for a single suggestion in the dropdown"
  []
  (let [search-term (subscribe [:search-term])]
    (fn [item is-active?]
      (let [info   (clojure.string/join " " (interpose ", " (vals (:fields item))))
            parsed (clojure.string/split info (re-pattern (str "(?i)" @search-term)))]
        [:div.list-group-item.quicksearch-result
         {:on-mouse-down
          (fn [e]
            (let [clicked-button (.-button e)]
              (cond (= clicked-button 0) ;;left click only pls.
                    (navigate-to-report item))))
          :class (cond is-active? "active ")}
         [:div.row-content {:class (str "type type-" (:type item))}
          [:h4.list-group-item-heading (:type item)]
          (into
           [:div.list-group-item-text]
           (interpose [:span.highlight @search-term] (map (fn [part] [:span part]) parsed)))]]))))

(defn monitor-enter-key [e]
  (let [keycode          (.-charCode e)
        active-selection (subscribe [:quicksearch-selected-index])
        results          (subscribe [:suggestion-results])
        selected-result  (nth @results @active-selection nil)]
    (cond
      (= keycode 13) ;;enter key is 13
      (do
        (if selected-result
          ;; go to the result direct if they're navigating with keyboard
          ;; and they just pressed enter
          (navigate-to-report selected-result)
          ;; go to the results page if they just type and press enter without
          ;; selecting a typeahead result
          (navigate-to-full-results))
        ;; no matter what the result, stop showing the quicksearch when
        ;; we press enter, kthx.
        (.blur (. e -target))))))

(defn monitor-arrow-keys
  "Navigate the dropdown suggestions if the user presses up or down"
  [e]
  (let [keycode (.-key e)]
    (cond
      (= keycode "ArrowUp")
      (dispatch [:search/move-selection :prev])
      (= keycode "ArrowDown")
      (dispatch [:search/move-selection :next]))))

(defn show-all-results
  "UI element within the dropdown to show all results."
  []
  (let [active-selection (subscribe [:quicksearch-selected-index])
        is-active?       (= -1 @active-selection)]
    [:div.show-all.list-group
     {:on-mouse-down
      (fn [e] (let [clicked-button (.-button e)]
                (cond (= clicked-button 0) ;;left click only pls.
                      (navigate-to-full-results))))}
     [:div.list-group-item {:class (cond is-active? "active")}
      [:h4 "Show all results"]]]))

(defn main []
  (reagent/create-class
   (let [results     (subscribe [:suggestion-results])
         search-term (subscribe [:search-term])]
     {:component-did-mount
      (fn [e]
        (let [node (dom/dom-node e)]
          (-> node
              (sel1 :input)
              (dommy/listen! :focus (fn [] (dommy/add-class! node :open)))
              (dommy/listen! :blur (fn [] (dommy/remove-class! node :open))))))
      :reagent-render
      (fn []
        [:div.dropdown
         [:input.typeahead-search
          {:type         "text"
           :value        @search-term
           :placeholder  "Search"
           :on-change    (fn [e] (dispatch [:bounce-search (oget e :target :value)]))
           ;; Navigate to the main search results page if the user presses enter.
           :on-key-press (fn [e] (monitor-enter-key e))
           ;; Why is this separate from on-key-press, you ask?
           ;; Arrow keys don't trigger keypress events apparently. What meanies.
           :on-key-up    (fn [e] (monitor-arrow-keys e))
           :on-focus     (fn [e]
                           ;; Results is nil when it has been cleared after
                           ;; switching mines. (If there really are no
                           ;; suggestions, it would be an empty vector.)
                           (when (nil? @results)
                             (dispatch [:bounce-search (oget e :target :value)])))}]
         (when (> (count @results) 0)
           [:div.dropdown-menu.quicksearch
            [show-all-results]
            (into [:div.list-group]
                  (map-indexed
                   (fn [index result]
                     (let [active-selection (subscribe [:quicksearch-selected-index])
                           is-active?       (= index @active-selection)]
                       [suggestion result is-active?]))
                   @results))])])})))
