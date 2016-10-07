(ns redgenes.components.querybuilder.subs
  (:require
    [redgenes.utils :refer [reg-all-subs!]]))

(reg-all-subs!
  [
   [:autoupdate?]
   [:constraint :query-builder/current-constraint]
   [:counting?]
   [:count]
   [:queried?]
   [:used-codes]
   [:io-query]
   [:query]
   ])