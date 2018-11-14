(ns main.utils)

(defn operator-precedence [operator]
  (cond (or (= operator "/") (= operator "*")) 2
        (or (= operator "-") (= operator "+")) 1
        :else 0))

(defn is-number? [x] (-> x js/isNaN not))

(defn is-string-number? [x] (is-number? (js/parseInt x)))

(def operator-symbols ["AC" "/" "+" "*" "-" "(" ")"])

(defn operator-symbol? [value] (some? (some #(= value %1) operator-symbols)))

;; TODO - impliment the ability to handle brackets
(defn infix-to-prefix [expression-list]
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

(defn prepare-expression [expression]
  (loop [exp expression
         result ()]
    (cond
      (empty? exp) result
      (and (is-string-number? (first exp)) (is-string-number? (first result))) (recur (rest exp) (conj (rest result) (str (first result) (first exp))))
      :else (recur (rest exp) (conj result (first exp))))))

; (println (prepare-expression '("3" "+" "5" "*" "6" "+" "1" "3")))
; (println (infix-to-prefix '("3" "+" "5" "*" "6" "+" "1" "3")))