# markov

Markov generates streams of symbols based on streams of symbols you feed into it.  It will adopt whatever immediate statistical properties are present in the original stream.  

## Usage

Add the dependency in your project.clj:

```clj
[markov "0.0.1"]
```

Then build a chain:

```clj
(require '[markov.core :as markov])
(def chain
  (let [chain (markov/empty-chain)]
    (markov/add-token-stream chain [:what :is :up :is :what :with :up :with :this?])))

(println (markov/follow-strand chain))
--> (:what :with :up :is :what :with :up :with :this?)
```


## License

Copyright © 2012 Ryan Spangler

Distributed under the Eclipse Public License, the same as Clojure.
