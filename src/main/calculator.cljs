
(ns main.calculator
  (:require [reagent.core :as r :refer [adapt-react-class]]
            ["@material-ui/core/Button" :default MTButton]
            ["@material-ui/core/Input" :default MTInput]
            [main.utils :as utils :refer [infix-to-prefix prepare-expression]]))



(def calc-state (r/atom {:infix-expression ()
                         :display ()}))

(def operators ["AC" "/" "+" "*" "-"])

(defn is-an-operators [value] (some #(= value %1) operators))

(defn last-is-operator? [collection]
  (some? (is-an-operators (first collection))))

(defn update-display [value]
  (swap! calc-state update :infix-expression (partial cons value))
  (swap! calc-state assoc :display (:infix-expression @calc-state)))

(defn update-without-operator [value]
  (if (and (is-an-operators value) (last-is-operator? (:infix-expression @calc-state)))
    (identity value)
    (update-display value)))

(defn evaluate [expression] (->> expression prepare-expression infix-to-prefix))

(def button-styles {:height "4rem"
                    :flex 1
                    :border "2px solid blkc"
                    :border-radius "7px"
                    :background-color "olivedrab"
                    :color "white"
                    :justify-content "center"
                    :font-size "1rem"})

(defn num-button [value button-css]
  ^{:key value}
  [:> MTButton
   {:color "primary"
    :on-click #(update-without-operator value)
    :style button-css} value])

(def num-0-9 (map #(num-button (inc %1) button-styles) (take 9 (range))))

(defn prepare-display [expression]
  (reduce (fn [result character] (conj result character " ")) () expression))

(defn bottom-row []
  (let [bottom-row-styles {:display "flex"
                          :justify-content "center"
                          :flex-direction "row"
                          :width "100%"}]
  [:div {:style bottom-row-styles}
   (num-button " " button-styles)
   (num-button "0" button-styles)
   [:> MTButton {:color "primary"
                 :style button-styles
                 :on-click #(swap! calc-state update :display (comp prepare-display evaluate))} "="]]))

(def operator-display-component
  (let [operator-buttons-styles {:display "flex"
                                 :flex 1
                                 :height "4rem"
                                 :align-items "flex-end"
                                 :border "0px solid black"
                                 :width "4rem"
                                 :background-color "olivedrab"
                                 :color "white"
                                 :font-size "1.5rem"}
        operator-container-styles {:height "100%"
                                   :display "flex"
                                   :flex-direction "column"}]
    [:div {:style operator-container-styles}
     (map-indexed (fn [idx itm] ^{:key idx}
                    [:> MTButton {:style operator-buttons-styles
                                  :on-click #(update-without-operator itm)} itm]) operators)]))


(defn display-component []
  (let [display-styles
        {:width "100%"
         :display "flex"
         :height "1rem"
         :justify-content "center"
         :border-radius "7px"
         :padding "2rem 0"
         :background-color "white"
         :font-size "1.3rem"
         :border "1px solid black"}]
    [:div {:style display-styles} (reverse (:display @calc-state))]))

(defn create-row [idx row-cells]
  ^{:key (inc idx)}
  [:div
   {:style {:display "flex"
            :flex-direction "row"
            :width "100%"}}
   row-cells])

(defn construct-calculator-rows [cells]
  (let [partitions (partition 3 cells)]
    (map-indexed (fn [idx itms] (create-row idx itms)) partitions)))

(defn calculator-component []
  (let [numbers-styles {:display "flex"
                        :align-items "center"
                        :flex-direction "column"
                        :width "20.33%"}
        calculator-container-styles {:height "100vh"
                                     :display "flex"
                                     :justify-content "center"
                                     :align-items "center"
                                     :background-color "black"}
        calculator-styles {:width "100%"
                           :display "flex"
                           :flex-direction "row"
                           :justify-content "center"}]
    [:div {:style calculator-container-styles}
     [:div
      {:style calculator-styles}
      [:div {:style numbers-styles}
       (display-component)
       (construct-calculator-rows num-0-9)
       (bottom-row)]
      [:div
       {:style
        {:width "6.33%"}} operator-display-component]]]))