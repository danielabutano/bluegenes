(ns bluegenes.subs.templates
  (:require [re-frame.core :refer [reg-sub]]
            [clojure.string :as s]))

; Thanks to https://gist.github.com/danielpcox/c70a8aa2c36766200a95
; Unlike merge, any nil parameter will nullify the merge and return nil
(defn deep-merge
  "Deeply merges maps so that nested maps are combined rather than replaced.
  For example:
  (deep-merge {:foo {:bar :baz}} {:foo {:fuzz :buzz}})
  ;;=> {:foo {:bar :baz, :fuzz :buzz}}
  ;; contrast with clojure.core/merge
  (merge {:foo {:bar :baz}} {:foo {:fuzz :buzz}})
  ;;=> {:foo {:fuzz :quzz}} ; note how last value for :foo wins"
  [& vs]
  (if (every? map? vs)
    (apply merge-with deep-merge vs)
    (last vs)))

(defn deep-merge-with
  "Deeply merges like `deep-merge`, but uses `f` to produce a value from the
  conflicting values for a key in multiple maps."
  [f & vs]
  (if (every? map? vs)
    (apply merge-with (partial deep-merge-with f) vs)
    (apply f vs)))

(defn template-contains-string?
  "Return true if a template's description contains a string"
  [string [_ details]]
  (if string
    (if-let [description (:description details)]
      (re-find (re-pattern (str "(?i)" string)) description)
      false)
    true))

(defn has-tag? [filter-tags [k {tags :tags}]]
  (if (not-empty filter-tags)
    (some? (not-empty (filter true? (map (partial contains? filter-tags) (set tags)))))
    true))

(reg-sub ::all-templates (fn [db] (get-in db [:assets :templates])))

(reg-sub ::current-mine-name (fn [db] (:current-mine db)))

(reg-sub ::filter-text (fn [db] (get-in db [:templates :filters :text])))

(reg-sub ::filter-tags (fn [db] (get-in db [:templates :filters :tags])))


(reg-sub ::current-templates
         :<- [::all-templates]
         :<- [::current-mine-name]
         :<- [::filter-text]
         :<- [::filter-tags]
         (fn [[templates current-mine-name filter-text filter-tags]]
           (->> current-mine-name
                (get templates)
                (map (fn [[k details]]
                       (if (= (:rank details) "unranked")
                         [k (assoc details :rank 999)]
                         [k (update details :rank js/parseInt)])))
                (filter (partial template-contains-string? filter-text))
                (filter (partial has-tag? filter-tags))
                (sort-by (comp js/parseInt :rank second) <))))

(reg-sub ::template-tags
         :<- [::all-templates]
         :<- [::current-mine-name]
         (fn [[templates current-mine-name]]
           (->> current-mine-name
                (get templates)
                (mapcat (comp :tags second))
                (filter #(s/starts-with? % "im:aspect"))
                distinct
                (sort <))))

(reg-sub ::selected-template-kw
         (fn [db]
           (get-in db [:templates :selected-template])))

(reg-sub ::template-edits
         (fn [db]
           (get-in db [:templates :edits])))

(reg-sub ::selected-template-edit
         :<- [::selected-template-kw]
         :<- [::template-edits]
         (fn [[selected-template-kw template-edits]]
           (get template-edits selected-template-kw)))

(reg-sub ::selected-template-details
         :<- [::selected-template-kw]
         :<- [::current-mine-name]
         :<- [::all-templates]
         :<- [::template-edits]
         (fn [[selected-template-kw mine-name all-templates template-edits]]
           (get-in template-edits [selected-template-kw])))
