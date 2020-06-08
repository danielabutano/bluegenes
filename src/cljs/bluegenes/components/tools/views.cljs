(ns bluegenes.components.tools.views
  (:require [re-frame.core :refer [subscribe dispatch]]
            [oops.core :refer [ocall+ oapply oget oget+ oset!]]
            [bluegenes.components.tools.events :as events]
            [bluegenes.components.tools.subs :as subs]
            [bluegenes.route :as route]))

;;fixes the inability to iterate over html vector-like things
(extend-type js/HTMLCollection
  ISeqable
  (-seq [array] (array-seq array 0)))

(defmulti navigate
  "Can be used from JS like this:
      navigate('report', {type: 'Gene', id: 1018204}, 'humanmine');
      navigate('query', myQueryObj, 'flymine');
  Note that the third argument specifying the mine namespace is optional."
  (fn [target data mine] (keyword target)))

(defmethod navigate :report [_ report-data mine]
  (let [{:keys [type id]} (js->clj report-data :keywordize-keys true)
        source (keyword mine)]
    (dispatch [::route/navigate ::route/report {:type type, :id id, :mine source}])))

(defmethod navigate :query [_ query-data mine]
  (let [query (js->clj query-data :keywordize-keys true)
        source (or (keyword mine)
                   @(subscribe [:current-mine-name]))]
    (dispatch [::events/navigate-query query source])))

(defn navigate_
  "JS can't call `navigate` if we pass it the multimethod directly, so wrap it!"
  [target data mine]
  (navigate target data mine))

(defn run-script
  "Executes a tool-api compliant main method to initialise a tool"
  [tool tool-id]
  ;;the default method signature is
  ;;package-name(el, service, package, state, config)
  (let [el (.getElementById js/document tool-id)
        service (clj->js (:service @(subscribe [:current-mine])))
        package (clj->js @(subscribe [::subs/entity]))
        config (clj->js (:config tool))
        main (str (get-in tool [:names :cljs]) ".main")]
    (ocall+ js/window main el service package nil config navigate_)))

(defn fetch-script
  ;; inspired by https://stackoverflow.com/a/31374433/1542891
  ;; I don't much like fetching the script in the view, but given
  ;; that this is heavy dom manipulation it seems necessary.
  ;; TODO: could active scripts in appdb be dereferenced and added to
  ;; the head automatically? That might be better if possible.
  "Dynamically inserts the tool api script into the head of the document"
  [tool tool-id]
  (let [script-tag (.createElement js/document "script")
        head (first (.getElementsByTagName js/document "head"))
        tool-path (get-in tool [:config :files :js])]
    (if tool-path
      (do
        ;;fetch script from npm's node_modules directory
        (oset! script-tag "src" (str "/tools/" (get-in tool [:names :npm]) "/" tool-path))
        ;;run-script will automatically be triggered when the script loads
        (oset! script-tag "onload" #(run-script tool tool-id))
        ;;append script to dom
        (.appendChild head script-tag))
      ;; there must be a script tag. If there isn't, console error.
      (.error js/console "%cNo script path provided for %s" "background:#ccc;border-bottom:solid 3px indianred; border-radius:2px;" (get-in tool [:names :human])))))

(defn fetch-styles
  "If the tool api script has a stylesheet as well, load it and insert into the doc"
  [tool]
  (let [style-tag (.createElement js/document "link")
        head (first (.getElementsByTagName js/document "head"))
        style-path (get-in tool [:config :files :css])]
    (cond style-path
    ;;fetch stylesheet and set some properties
          (do (oset! style-tag "href" (str "/tools/" (get-in tool [:names :npm]) "/" style-path))
              (oset! style-tag "type" "text/css")
              (oset! style-tag "rel" "stylesheet")
    ;;append to dom
              (.appendChild head style-tag)))))

(defn main
  "Initialise all the tools on the page"
  []
  (let [toolses           (subscribe [::subs/suitable-tools])]
    (.log js/console "%c@toolses" "color:mediumorchid;font-weight:bold;" (clj->js @toolses))
    (into [:div.tools]
          (map
           (fn [tool]
             (let [tool-id (gensym (get-in tool [:config :toolName :cljs]))]
               (fetch-script tool tool-id)
               (fetch-styles tool)
               [:div.tool {:class (get-in tool [:names :cljs])}
                [:h3 (get-in tool [:names :human])]
                [:div {:id tool-id}]]))
           @toolses))))
