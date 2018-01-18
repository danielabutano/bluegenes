(ns bluegenes.sections.reportpage.views
  (:require [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [bluegenes.sections.reportpage.components.summary :as summary]
            [bluegenes.components.table :as table]
            [bluegenes.components.lighttable :as lighttable]
            [bluegenes.components.loader :refer [loader]]
            [bluegenes.sections.reportpage.components.minelinks :as minelinks]
            [accountant.core :refer [navigate!]]
            [im-tables.views.core :as tables]))

(defn main []
  (let [params           (subscribe [:panel-params])
        report           (subscribe [:report])
        categories       (subscribe [:template-chooser-categories])
        templates        (subscribe [:runnable-templates])
        collections      (subscribe [:collections])
        fetching-report? (subscribe [:fetching-report?])
        s (subscribe [:current-mine])]
    (fn []
      [:div.container-fluid.report
       [:h1 "Table"]
       (js/console.log "S" @s)

       (if @fetching-report?
         [loader (str (:type @params) " Report")]
         [:div
          [:ol.breadcrumb
           [:li [:a {:href "#/" :on-click #(navigate! "/")} "Home"]]
           [:li [:a {:href "#/search" :on-click #(navigate! "/search")} "Search Results"]]
           [:li.active [:a "Report"]]]
          [summary/main (:summary @report)]
           (cond (= "Gene" (:type @params))
           [minelinks/main (:id @params)])

          (into [:div.collections]
                (map (fn [query]
                       #_[lighttable/main query {:title true}]
                       [:div
                        [:h4 (:class query)]
                        [tables/main {:location [:test (:class query)]
                                      :service (assoc (:service @s) :summary-fields {})
                                      ;:query {:from "Gene"
                                      ;        :select ["Gene.symbol"]
                                      ;        :where [{:path "Gene.symbol"
                                      ;                 :op "LIKE"
                                      ;                 :value "M*"}]}
                                      :query (assoc (:query query) :size 5)
                                      }]]
                       ;(js/console.log "BUILDING" [:test] query)

                       ) @collections))
          (into [:div.templates] (map (fn [[id details]] [table/main details]) @templates))])])))
