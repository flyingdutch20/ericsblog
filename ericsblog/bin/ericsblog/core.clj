(ns ericsblog.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(def token-regex #"\w+")
(def stop-words #{"a" "in" "that" "for" "was" "is"
                  "it" "the" "of" "and" "to" "he"})

(defn to-lower-case [token-string]
  (.toLowerCase token-string))
(defn tokenise-str 
  ([input-string]
  (map to-lower-case
       (re-seq token-regex input-string)))
  ([input-string stop-word?]
    (filter (complement stop-word?)
            (tokenise-str input-string))))
(defn tokenise
  ([filename]
    (tokenise-str (slurp filename)))
  ([filename stop-word?]
    (tokenise-str (slurp filename) stop-word?)))

(defstruct stemmer :word :index)
(defn make-stemmer
  "This returns a stemmer structure for the given word"
  [word]
  (struct stemmer (vec word) (dec (count word))))
(defn reset-index
  [word-vec]
  (struct stemmer word-vec (dec (count word-vec))))
(defn get-index
  [stemmer]
  (if-let j (:index stemmer)
    (min j (dec (count (:word stemmer))))
    (dec (count (:word stemmer)))))
(defn subword
  [stemmer]
  (let [b (:word stemmer), j (inc (get-index stemmer))]
    (if (< j (count b))
      (subvec b 0 j)
      b)))
(defn index-char
  [stemmer]
  (nth (:word stemmer) (get-index stemmer)))
(defn pop-word
  [stemmer]
  (assoc stemmer :word (pop (:word stemmer))))
