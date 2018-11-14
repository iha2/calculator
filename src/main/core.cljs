; NOTES
(ns main.core
  (:require [reagent.core :as r :refer [adapt-react-class]]
            ["@material-ui/core/Input" :default MTInput]
            [main.calculator :as calc :refer [calculator-component]])
  (:require-macros [cljs.core.async :refer [go]]))

(defn operator-precedence [operator]
  (cond (or (= operator "/") (= operator "*")) 2
        (or (= operator "-") (= operator "+")) 1
        :else 0))

(defn is-number? [x] (-> x js/isNaN not))

(def operator-symbols ["AC" "/" "+" "*" "-" "(" ")"])

(defn operator-symbol? [value] (some? (some #(= value %1) operator-symbols)))

;; TODO - impliment the ability to handle brackets
(defn infix-to-postfix [expression-list]
  (loop [expression expression-list
         operator-stack ()
         postfix-stack ()]
    (let [character (first expression)
          remaining-expression (rest expression)
          higher-precedence-operator? #(>= (operator-precedence %1) (operator-precedence character))]
      (cond
        (empty? expression) (concat (reverse operator-stack) postfix-stack)
        (is-number? (js/parseInt character)) (recur (rest expression) operator-stack (conj postfix-stack character))
        (operator-symbol? character)
        (let [higher-precedence-operators (filter higher-precedence-operator? operator-stack)]
          (recur
           remaining-expression
           (conj (filter (comp not higher-precedence-operator?) operator-stack) character)
           (concat (reverse higher-precedence-operators) postfix-stack)))
        :else (concat remaining-expression  postfix-stack)))))

  ; (println (infix-to-postfix '("3" "+" "5" "*" "6" "+" "12")))

(defn app []
  (calculator-component))

(defn ^:dev/after-load start []
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))
