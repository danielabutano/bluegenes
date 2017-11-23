(ns bluegenes.components.toast
  (:require [reagent.core :as r]
            [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [oops.core :refer [oget ocall]]))

(def css-transition-group (r/adapt-react-class js/React.addons.CSSTransitionGroup))

(def width-height (juxt #(ocall % :outerWidth) #(ocall % :outerHeight)))

(defn toast-item []
  (fn [[id {:keys [entry size] :as toast}] offset]
    [:div.toast-item
     {:ref (fn [n] (when n (dispatch [:report-size id (-> n js/$ width-height)])))
      :style {:bottom (str offset "px")}}
     [:div.toast-item-toolbar.pull-right [:i.fa.fa-times.fa-2x {:on-click (fn [] (dispatch [:remove-toast id]))}]]
     entry]))

(defn main []
  (let [toasts (subscribe [:toasts])]
    (fn []
      [css-transition-group
       {:transition-name "customer"
        :component "div"
        :class "toast-container"
        :style {:display "flex"}
        :transition-enter-timeout 1000
        :transition-leave-timeout 700}
       (map-indexed (fn [idx [id :as toast]]
                      (let [offset (+ (apply + (map (comp second :size second) (take idx @toasts))) (* idx 15))]
                        ^{:key id} [toast-item toast offset]
                        )) @toasts)])))

