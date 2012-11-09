(ns markov.core)

(defrecord MarkovWheel [slices total])
(defrecord MarkovSlice [token weight])
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

(defn inc-slice 
  [slice]
  (update-in slice [:weight] inc))

(defn wheel-slice 
  [wheel token]
  (or
   (-> wheel :slices (get token))
   (MarkovSlice. token 0)))

(defn wheel-weight 
  [wheel token]
  (-> wheel :slices (get token) :weight))

(defn observe-token 
  [wheel token]
  (let [slice (inc-slice (wheel-slice wheel token))]
    (MarkovWheel. (assoc (:slices wheel) token slice) (+ (:total wheel) 1))))

(defn spin 
  [wheel]
  (if (empty? (:slices wheel))
    nil
    (let [fate (* (rand) (:total wheel))]
      (loop [tokens (seq (keys (:slices wheel)))
             step 0]
        (let [next-step (+ step (wheel-weight wheel (first tokens)))]
          (if (> fate next-step)
            (recur (next tokens) next-step)
            (first tokens)))))))

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
  (assoc-in
   chain
   [:nodes from-token]
   (add-orientation
    (continuing-node chain from-token (direction-map direction) :false)
    direction to-token)))

(defn add-link
  [chain from-token to-token]
  (let [outgoing (single-link chain from-token to-token :outgoing)]
    (single-link outgoing to-token from-token :incoming)))

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
  (let [beginning (spin (get chain :beginning))]
    (cons beginning (reverse (from-focus chain beginning :outgoing)))))

(defn issue-strand
  [chain token]
  (concat
   (from-focus chain token :incoming)
   [token]
   (reverse (from-focus chain token :outgoing))))