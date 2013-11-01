(ns markov.nlp
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [opennlp.nlp :as nlp]
            [markov.core :as markov]))

(import markov.core.MarkovSlice)

(def get-sentences (nlp/make-sentence-detector (io/resource "models/en-sent.bin")))
(def tokenize (nlp/make-tokenizer (io/resource "models/en-token.bin")))
(def detokenize (nlp/make-detokenizer (io/resource "models/english-detokenizer.xml")))
(def pos-tag (nlp/make-pos-tagger (io/resource "models/en-pos-maxent.bin")))
(def name-find (nlp/make-name-finder (io/resource "models/namefind/en-ner-person.bin")))

(defrecord NaturalNode [token incoming outgoing])
(defrecord NaturalChain [templates nodes pos])

(def template-limit 24)

(defn empty-node
  [token]
  (NaturalNode. token {} {}))

(defn empty-chain
  []
  (NaturalChain. (markov/empty-wheel) {} {}))

(defn parse-sentence
  [sentence]
  (pos-tag (tokenize sentence)))

(defn continue-node
  [node direction [to-token to-pos]]
  (update-in 
   node 
   [direction to-pos]
   #(markov/observe-token (or % (markov/empty-wheel)) to-token)))

(defn single-link
  [chain from to direction]
  (update-in 
   chain
   [:nodes from]
   (fn [node]
     (let [node (or node (empty-node (first from)))]
       (continue-node node direction to)))))

(defn add-link
  [chain from to]
  (-> chain
      (single-link from to :outgoing)
      (single-link to from :incoming)))

(defn distribute-sentence
  [chain raw]
  (let [sentence (parse-sentence raw)]
    (if-let [[token pos] (first sentence)]
      (let [template (map last sentence)]
        (loop [chain (update-in chain [:templates] #(markov/observe-token % template))
               [previous-token previous-pos] [token pos]
               sentence (rest sentence)]
          (let [chain (update-in
                       chain
                       [:pos previous-pos]
                       #(markov/observe-token (or % (markov/empty-wheel)) previous-token))]
            (if (empty? sentence)
              chain
              (let [[token pos] (first sentence)]
                (recur 
                 (add-link chain [previous-token previous-pos] [token pos])
                 [token pos]
                 (rest sentence))))))))))

(defn distribute-speech
  [chain speech]
  (reduce distribute-sentence chain speech))

(defn parse-speech
  [chain speech]
  (distribute-speech chain (string/split speech #"\n")))

(defn spin-pos
  [chain from direction pos]
  (let [node (get-in chain [:nodes from])]
    (if node
      (if-let [token (markov/spin (get-in node [direction pos]))]
        token
        (markov/spin (get-in chain [:pos pos])))
      (markov/spin (get-in chain [:pos pos])))))

(defn refine-topiary
  [branches]
  (let [wheel (reduce
               (fn [wheel [subtoken branch weight]]
                 (if subtoken
                   (markov/add-slice wheel (markov/MarkovSlice. subtoken weight branch))
                   wheel))
               (markov/empty-wheel) branches)]
    (and (not (markov/wheel-empty? wheel)) wheel)))

(defn build-template-tree
  [chain template pos [token slice]]
  (if-let [node (get-in chain [:nodes [token pos]])]
    (if-let [head (first template)]
      (let [outgoing (get-in node [:outgoing head])
            branches (map (partial build-template-tree chain (rest template) head) (seq (:slices outgoing)))
            topiary (refine-topiary branches)]
        (if topiary
          [token topiary (:total topiary)]))
      [token nil (:weight slice)])))

(defn build-template-chain
  [chain template]
  (let [head (first template)
        start (get-in chain [:pos head])
        branches (map (partial build-template-tree chain (rest template) head) (seq (:slices start)))]
    (refine-topiary branches)))

(defn pluck-tree
  [tree]
  (loop [tones []
         tree tree]
    (if tree
      (let [slice (markov/spin-slice tree)]
        (recur (conj tones (:token slice)) (:data slice)))
      tones)))

(defn pluck-template-tree
  [chain template]
  (let [tree (build-template-chain chain template)]
    (pluck-tree tree)))

(defn pluck-chain
  ([chain] (pluck-chain chain template-limit))
  ([chain limit] 
     (let [slim (markov/filter-slices (:templates chain) #(> limit (count %)))
           template (markov/spin slim)]
       (println "template length" (count template) template)
       (pluck-template-tree chain template))))

(defn generate-parts
  [chain]
  (let [template (markov/spin (:templates chain))
        pos (first template)
        token (markov/spin (-> chain :pos (get pos)))]
    (loop [template (rest template)
           sentence [token]
           from [token pos]]
      (if (empty? template)
        sentence
        (let [pos (first template)
              token (spin-pos chain from :outgoing pos)]
          (recur (rest template) (conj sentence token) [token pos]))))))

(defn unparse
  [parts]
  (reduce
   (fn [sentence token]
     (if (or 
          (= (re-find #"[^a-zA-Z0-9\($]+" token) token)
          (re-find #"^://" token)
          (#{\( \{ \[ \" \$} (last sentence))
          (#{\' \.} (first token))
          (= token "n't"))
       (str sentence token)
       (str sentence " " token)))
   (first parts) (rest parts)))

(defn generate-sentence
  [chain]
  (unparse (generate-parts chain)))

(defn generate-coherent-sentence
  [chain limit]
  (unparse (pluck-chain chain limit)))
