(defn turing-machine [input]
  (loop [tape (vec (str "  " input "  "))
         head 2
         state :q0]
    (let [current-symbol (tape head)]
      (cond
        (= state :accept) (println "Input is accepted by the Turing Machine.")
        (= state :reject) (println "Input is rejected by the Turing Machine.")
        :else
        (recur
          (case [state current-symbol]
            [:q0 \a] (assoc tape head \X)
            [:q1 \b] (assoc tape head \Y)
            tape)
          (case [state current-symbol]
            [:q0 \a] (inc head)
            [:q1 \a] (inc head)
            [:q1 \X] (inc head)
            [:q2 \b] (inc head)
            [:q2 \Y] (inc head)
            head)
          (case [state current-symbol]
            [:q0 \a] :q1
            [:q0 \space] :accept
            [:q1 \b] :q2
            [:q2 \space] :accept
            state))))))

(turing-machine "aabb")
