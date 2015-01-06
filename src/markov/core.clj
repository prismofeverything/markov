(ns markov.core)

(defrecord MarkovWheel [slices total])
(defrecord MarkovSlice [token weight data])
(defrecord MarkovNode [token incoming outgoing beginning ending])
(defrecord MarkovChain [nodes beginning ending])

(defn empty-wheel
  []
  (MarkovWheel. {} 0))

(defn empty-node
  [token]
  (MarkovNode. token (empty-wheel) (empty-wheel) (empty-wheel) (empty-wheel)))

(defn empty-chain
  []
  (MarkovChain. {} (empty-wheel) (empty-wheel)))

(defn wheel-slice 
  [wheel token]
  (or
   (-> wheel :slices (get token))
   (MarkovSlice. token 0 nil)))

(defn inc-slice 
  [slice]
  (update-in slice [:weight] inc))

(defn wheel-empty?
  [wheel]
  (if wheel
    (empty? (:slices wheel))
    true))

(defn wheel-weight 
  [wheel token]
  (-> wheel :slices (get token) :weight))

(defn add-slice
  [wheel slice]
  (-> wheel
      (assoc-in [:slices (:token slice)] slice)
      (update-in [:total] (partial + (:weight slice)))))

(defn observe-token
  [wheel token]
  (let [slice (inc-slice (wheel-slice wheel token))]
    (-> wheel
        (update-in [:slices token] (constantly slice))
        (update-in [:total] inc))))

(defn filter-slices
  [wheel p]
  (let [[slices weight] 
        (reduce
         (fn [[slices weight] slice]
           (if (p (:token slice))
             [(assoc slices (:token slice) slice) (+ weight (:weight slice))]
             [slices weight]))
         [{} 0]
         (vals (:slices wheel)))]
    (MarkovWheel. slices weight)))

(defn spin-slice
  [wheel]
  (if (empty? (:slices wheel))
    nil
    (let [fate (* (rand) (:total wheel))]
      (loop [slices (-> wheel :slices vals seq)
             step 0]
        (let [slice (first slices)
              next-step (+ step (:weight slice))]
          (if (> fate next-step)
            (recur (next slices) next-step)
            slice))))))

(defn spin 
  [wheel]
  (let [slice (spin-slice wheel)]
    (:token slice)))

(defn remove-slice
  [wheel slice]
  (-> wheel
      (update-in [:slices] #(dissoc % (:token slice)))
      (update-in [:total] #(- % (:weight slice)))))

(defn pull
  [wheel]
  (let [slice (spin-slice wheel)
        wheel (remove-slice wheel slice)]
    [(:token slice) wheel]))

(defn node-for
  [chain token]
  (or
   (-> chain :nodes (get token))
   (empty-node token)))

(defn continuing-node
  [chain token terminal value]
  (let [node (node-for chain token)]
    (update-in node [terminal] #(observe-token % value))))

(defn add-orientation
  [node orientation token]
  (update-in node [orientation] #(observe-token % token)))

(def direction-map {:outgoing :ending :incoming :beginning})

(defn single-link
  [chain from-token to-token direction]
  (let [continuing (continuing-node chain from-token (direction-map direction) :false)
        oriented (add-orientation continuing direction to-token)]
    (assoc-in chain [:nodes from-token] oriented)))

(defn add-link
  [chain from-token to-token]
  (-> chain
      (single-link from-token to-token :outgoing)
      (single-link to-token from-token :incoming)))

(defn add-terminal
  [chain terminal token]
  (let [observed (observe-token (get chain terminal) token)
        terminated (assoc chain terminal observed)
        continuing (continuing-node terminated token terminal :true)]
    (assoc-in terminated [:nodes token] continuing)))

(defn add-token-stream
  [chain tokens]
  (let [head (first tokens)]
    (if head
      (loop [chain (add-terminal chain :beginning head)
             tokens (rest tokens)
             previous head]
        (if (empty? tokens)
          (add-terminal chain :ending previous)
          (recur (add-link chain previous (first tokens)) (rest tokens) (first tokens)))))))

(defn from-focus
  [chain token direction]
  (if-let [node ((get chain :nodes) token)]
    (loop [token token
           path '()
           node node]
      (if (= (spin (get node (direction-map direction))) :true)
        path
        (let [follow (spin (get node direction))]
          (recur follow (cons follow path) (-> chain :nodes (get follow))))))))

(defn follow-strand
  [chain]
  (let [beginning (-> chain :beginning spin)]
    (cons beginning (reverse (from-focus chain beginning :outgoing)))))

(defn issue-strand
  [chain token]
  (concat
   (from-focus chain token :incoming)
   [token]
   (reverse (from-focus chain token :outgoing))))
