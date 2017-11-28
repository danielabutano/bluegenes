(ns bluegenes.components.templates.views
  (:require [reagent.core :as reagent]
            [re-frame.core :refer [subscribe dispatch]]
            [accountant.core :refer [navigate!]]
            [clojure.string :refer [split join blank?]]
            [json-html.core :as json-html]
            [bluegenes.components.imcontrols.views :refer [op-dropdown list-dropdown]]
            [bluegenes.components.inputgroup :as input]
            [bluegenes.components.lighttable :as lighttable]
            [imcljs.path :as im-path]
            [bluegenes.components.ui.constraint :refer [constraint]]
            [bluegenes.components.ui.results_preview :refer [preview-table mini-results-table]]
            [oops.core :refer [oget]]
            [clojure.string :as s]
            [bluegenes.subs.templates :as subs]
            [bluegenes.events.templates :as evts]))


(defn categories []
  (let [categories        (subscribe [:template-chooser-categories])
        selected-category (subscribe [:selected-template-category])]
    (fn []
      (into [:ul.nav.nav-pills.template-categories
             [:li {:on-click #(dispatch [:template-chooser/set-category-filter nil])
                   :class (if (nil? @selected-category) "active")}
              [:a.type-all "All"]]]
            (map (fn [category]
                   [:li {:on-click #(dispatch [:template-chooser/set-category-filter category])
                         :class
                         (if (= category @selected-category) " active")}
                    [:a {:class (str
                                  "type-" category)} category]])
                 @categories)))))

(def css-transition-group
  (reagent/adapt-react-class js/React.addons.CSSTransitionGroup))

(defn results-count-text [results-preview]
  (if (< (:iTotalRecords @results-preview) 1)
    "No Results"
    (str "View "
         (js/parseInt (:iTotalRecords @results-preview))
         (if (> (:iTotalRecords @results-preview) 1) " rows" " row"))))

(defn preview-results
  "Preview results of template as configured by the user or default config"
  [results-preview fetching-preview]
  (let [fetching-preview? (subscribe [:template-chooser/fetching-preview?])
        results-preview   (subscribe [:template-chooser/results-preview])]
    [:div.col-xs-8.preview
     [:h4 "Results Preview"]
     [preview-table
      :loading? @fetching-preview?
      :query-results @results-preview]
     [:button.btn.btn-primary.btn-raised.view-results
      {:type "button"
       :disabled (< (:iTotalRecords @results-preview) 1)
       :on-click (fn [] (dispatch [:templates/send-off-query]))}
      (if @fetching-preview?
        "Loading"
        (results-count-text results-preview))]]))

(defn select-template-settings
  "UI component to allow users to select template details, e.g. select a list to be in, lookup value grater than, less than, etc."
  [selected-template]
  (let [service   (subscribe [:selected-template-service])
        row-count (subscribe [:template-chooser/count])
        lists     (subscribe [:lists])]
    [:div.col-xs-4.border-right
     (into [:form.form]
           ; Only show editable constraints, but don't filter because we want the index!
           (->> (keep-indexed (fn [idx con] (if (:editable con) [idx con])) (:where @selected-template))
                (map (fn [[idx con]]
                       [:div
                        [:label {:style {:color "black"}} (s/join " > " (take-last 2 (s/split (im-path/friendly (:model @service) (:path con)) " > ")))]
                        [constraint
                         :model (:model @service)
                         :typeahead? false
                         :path (:path con)
                         :value (:value con)
                         :op (:op con)
                         :code (:code con)
                         :hide-code? true
                         :label? true
                         :lists (second (first @lists))
                         :on-change (fn [new-constraint]
                                      (dispatch [:template-chooser/replace-constraint
                                                 idx (merge con new-constraint)]))]]))))]))

(defn tags
  "UI element to visually output all aspect tags into each template card for easy scanning / identification of tags.
  ** Expects: format im:aspect:thetag.
  ** Will output 'thetag'.
  ** Won't output any other tag types or formats"
  [tagvec]
  (into
    [:div.template-tags "Template categories: "]
    (if (> (count tagvec) 0)
      (map (fn [tag]
             (let [tag-parts (clojure.string/split tag #":")
                   tag-name  (peek tag-parts)
                   is-aspect (and (= 3 (count tag-parts)) (= "aspect" (nth tag-parts 1)))]
               (if is-aspect
                 [:span.tag-type {:class (str "type-" tag-name)} tag-name]
                 nil)
               )) tagvec)
      " none"
      )
    ))

(defn template
  "UI element for a single template." []
  (let [selected-template (subscribe [:selected-template])]
    (fn [[id query]]
      [:div.grid-1
       [:div.col.ani.template
        {:on-click (fn []
                     (if (not= (name id) (:name @selected-template))
                       (dispatch [:template-chooser/choose-template id])))
         :class (if (= (name id) (:name @selected-template)) "selected")}
        (into [:h4] (->> (s/split (:title query) #"-{1,}>")
                         (interpose [:svg.icon.icon-arrow-right [:use {:xlinkHref "#icon-arrow-right"}]])
                         (map (fn [part]
                                (if (string? part)
                                  (interpose [:svg.icon.icon-arrow-left [:use {:xlinkHref "#icon-arrow-left"}]] (s/split part #"<-{1,}"))
                                  part)))))
        [:div.description
         {:dangerouslySetInnerHTML {:__html (:description query)}}]
        (if (= (name id) (:name @selected-template))
          [:div.body
           [select-template-settings selected-template]
           [preview-results]])
        [tags (:tags query)]
        ]])))

(defn templates
  "Outputs all the templates that match the user's chosen filters."
  []
  (fn [templates]
    (if (seq templates)
      ;;return the list of templates if there are some
      (into [:div.template-list] (map (fn [t] [template t]) templates))
      ;;if there are no templates, perhaps because of filters or perhaps not...
      [:div.no-results
       [:svg.icon.icon-wondering [:use {:xlinkHref "#icon-wondering"}]]
       " No templates available"
       (let [category-filter (subscribe [:selected-template-category])
             text-filter     (subscribe [:template-chooser/text-filter])
             filters-active? (or (some? @category-filter) (not (blank? @text-filter)))]
         (cond filters-active?

               [:span
                [:span
                 (cond @category-filter
                       (str " in the '" @category-filter "' category"))
                 (cond @text-filter
                       (str " containing the text '" @text-filter "'"))]
                [:span ". Try "
                 [:a {:on-click
                      (fn []
                        (dispatch [:template-chooser/set-text-filter ""])
                        (dispatch [:template-chooser/set-category-filter nil]))
                      } "removing the filters"]
                 " to view more results. "]])
         )
       ])))

(defn template-filter []
  (let [text-filter (subscribe [:template-chooser/text-filter])]
    (fn []
      [:input.form-control.input-lg
       {:type "text"
        :value @text-filter
        :placeholder "Filter text..."
        :on-change (fn [e]
                     (dispatch [:template-chooser/set-text-filter (.. e -target -value)]))}])))


(defn filters [categories template-filter filter-state]
  [:div.template-filters.container-fluid
   [:div.template-filter
    [:label.control-label "Filter by category"]
    [categories]]
   [:div.template-filter
    [:label.control-label "Filter by description"]
    [template-filter filter-state]]])


(defn replace-arrows [s]
  (->> (s/split s #"-{1,}>")
       (interpose [:svg.icon.icon-arrow-right [:use {:xlinkHref "#icon-arrow-right"}]])
       (map (fn [part]
              (if (string? part)
                (interpose [:svg.icon.icon-arrow-left [:use {:xlinkHref "#icon-arrow-left"}]] (s/split part #"<-{1,}"))
                part)))
       (reduce (fn [total next]
                 (if (seq? next)
                   (conj total (first next))
                   (conj total next))) [])))

(defn parse-aspect-tags
  "Take a collection of tags [im:aspect:Function im:public] and return categories"
  [coll]
  (->> coll
       (keep (fn [s] (when (s/starts-with? s "im:aspect") s)))
       (map (comp last #(s/split % ":")))
       (sort <)))

(defn template-item []
  (fn [[k {:keys [title description rank tags]} :as v] selected-template-kw]
    (let [active (= k selected-template-kw)]
      [:li.list-group-item
       {:class (when active "active")
        :on-click (fn [] (dispatch [:bluegenes.events.templates/select-template v]))}
       [:div
        (into [:span.pull-right] (->> tags
                                      parse-aspect-tags
                                      (map (fn [t]
                                             [:span.label.label-info
                                              {:style {:margin-right "5px"}
                                               :class (when active "label-inverted")}
                                              t]))))
        (into [:h4] (replace-arrows title))
        [:p description]]])))

(defn template-list []
  (let [current-templates    (subscribe [::subs/current-templates])
        selected-template-kw (subscribe [::subs/selected-template-kw])]
    (fn []
      (into [:ul.list-group] (map (fn [[k details :as t]] ^{:key (str k)} [template-item t @selected-template-kw]) @current-templates)))))

(defn template-details
  "UI component to allow users to select template details, e.g. select a list to be in, lookup value grater than, less than, etc."
  []
  (let [selected-template    (subscribe [::subs/selected-template-details])
        selected-template-kw (subscribe [::subs/selected-template-kw])
        model                (subscribe [:current-model])
        row-count            (subscribe [:template-chooser/count])
        lists                (subscribe [:lists])
        current-mine-name    (subscribe [:current-mine-name])]
    (fn []
      (let [query (:query @selected-template)]
        [:div.panel.tpl-arrow-box-left
         [:div.panel-body
          (into [:h2] (replace-arrows (:title query)))
          [:p (:description query)]
          (into [:form.form]
                ; Only show editable constraints, but don't filter because we want the index!
                (->> (keep-indexed (fn [idx con] (if (:editable con) [idx con])) (:where query))
                     (map (fn [[idx con]]
                            [:div.form-group
                             [:label {:style {:color "black"}} (s/join " > " (take-last 2 (s/split (im-path/friendly @model (:path con)) " > ")))]
                             [constraint
                              :model @model
                              :typeahead? false
                              :path (:path con)
                              :value (:value con)
                              :op (:op con)
                              :code (:code con)
                              :hide-code? true
                              :label? true
                              :lists (second (first @lists))
                              :on-change (fn [new-constraint]
                                           (dispatch [::evts/update-constraint
                                                      (keyword (:name query))
                                                      idx (merge con new-constraint)]))]]))))
          [:div.btn-group.pull-right
           [:button.btn.btn-default
            {:on-click (fn [] (dispatch [::evts/reset-template @current-mine-name @selected-template-kw]))}
            "Reset"]
           [:button.btn.btn-primary "Run"]]
          ]]))))


(defn filter-dashboard []
  (let [filter-text   (subscribe [::subs/filter-text])
        template-tags (subscribe [::subs/template-tags])
        filter-tags   (subscribe [::subs/filter-tags])]
    (fn []
      [:div
       [:label "Filter by text"]
       [:div.input-group.input-group
        [:input.form-control
         {:placeholder "Templates containing text..."
          :value @filter-text
          :on-change (fn [e] (dispatch [::evts/set-filter-text (oget e :target :value)]))
          :type "text"}]
        [:span.input-group-btn
         [:button.btn.btn-default
          {:on-click (fn [] (dispatch [::evts/set-filter-text nil]))}
          "Clear"]]]
       [:label "Filter by category"]
       [:div.input-group
        (into [:div.tpl-categories]
              (map (fn [t]
                     (let [active? (or (empty? @filter-tags) (contains? @filter-tags t))]
                       [:div.tpl-category
                        {:class (when active? "active")
                         :on-click (fn [] (dispatch [::evts/set-filter-tag t]))}
                        (if active?
                          [:i.fa.fa-eye.fa-fw]
                          [:i.fa.fa-eye-slash.fa-fw]) (last (s/split t ":"))])) @template-tags))]])))

(defn preview []
  (let [selected-template (subscribe [::subs/selected-template-details])]
    (fn []
      [:div.panel.tpl-arrow-box-top
       [:div.panel-body
        [:h2 "Query Results Preview"]
        [mini-results-table (:preview @selected-template)]]])))

(defn main []
  (let [im-templates (subscribe [:templates-by-category])
        filter-state (reagent/atom nil)]
    (fn []
      [:div.container-fluid
       ;(json-html/edn->hiccup @selected-template)
       [:div.tpl-layout
        [:div.column-templates
         [:div.filter-container [filter-dashboard]]
         [:div.template-list-container [template-list]]]
        [:div.column-parameters [template-details] [preview]]
        #_[:div.column-results]
        ]
       #_[:div.row
          [:div.col-xs-12.templates
           [filters categories template-filter filter-state]
           [:div.template-list
            ;;the bad placeholder exists to displace content, but is invisible. It's a duplicate of the filters header
            [:div.bad-placeholder [filters categories template-filter filter-state]]
            [templates @im-templates]]]
          ]])))
