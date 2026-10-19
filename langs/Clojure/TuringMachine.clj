(require '[clojure.string :as str])

;; The canonical machine: a binary incrementer.  Reading the tape from the
;; right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
;; stops; if the carry runs off the tape a new leading 1 is added.
(defn increment [bits]
  (let [reversed (reverse bits)]
    (reverse
     (loop [remaining reversed
            result []]
       (if (empty? remaining)
         (conj result \1)
         (if (= (first remaining) \1)
           (recur (rest remaining) (conj result \0))
           (into (conj result \1) (rest remaining))))))))

(defn -main [& _]
  (let [line (or (read-line) "")
        line (str/replace line #"[\r\n]+$" "")]
    (if (some #(and (not= % \0) (not= % \1)) line)
      (println "Invalid input: expected a binary string.")
      (do
        (println (str "Initial tape: " line))
        (println (str "Final tape: " (apply str (increment line))))))))

(-main)
