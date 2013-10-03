(ns markov.nlp
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [opennlp.nlp :as nlp]
            [markov.core :as markov]))

(def get-sentences (nlp/make-sentence-detector (io/resource "models/en-sent.bin")))
(def tokenize (nlp/make-tokenizer (io/resource "models/en-token.bin")))
(def detokenize (nlp/make-detokenizer (io/resource "models/english-detokenizer.xml")))
(def pos-tag (nlp/make-pos-tagger (io/resource "models/en-pos-maxent.bin")))
(def name-find (nlp/make-name-finder (io/resource "models/namefind/en-ner-person.bin")))

(defrecord NaturalNode [token incoming outgoing])
(defrecord NaturalChain [templates nodes pos])

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
          (#{\' \.} (first token)))
       (str sentence token)
       (str sentence " " token)))
   (first parts) (rest parts)))

(defn generate-sentence
  [chain]
  (unparse (generate-parts chain)))
