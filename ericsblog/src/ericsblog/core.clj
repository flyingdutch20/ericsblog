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
  (if-let [j (:index stemmer)]
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
(defn pop-stemmer-on
  "This is an amalgam of a number of different functions:
   pop (it walks through the :word sequence using pop);
   drop-while (it drops items off while testing the sequence against drop-while);
   and maplist from Common Lisp (the prediacte is tested against the entire current stemmer,
   not just the first element)"
  [predicate stemmer]
  (if (and (seq (:word stemmer)) (predicate stemmer))
    (recur predicate (pop-word stemmer))
    stemmer))
(def vowel-letter? #{\a \e \i \o \u})
(defn consonant?
  "Returns true if the ith character in a stemmer is a consonant. i defaults to :index."
  ([stemmer]
    (consonant? stemmer (get-index stemmer)))
  ([stemmer i]
    (let [c (nth (:word stemmer) i)]
      (cond (vowel-letter? c) false
            (= c \y) (if (zero? i)
                       true
                       (not (consonant? stemmer (dec i))))
            :else true))))
(def vowel? (complement consonant?))
(defn vowel-in-stem?
  "true iff 0 ... j contains a vowel"
  [stemmer]
  (let [j (get-index stemmer)]
    (loop [i 0]
      (cond (> i j) false
            (consonant? stemmer i) (recur (inc i))
            :else true))))
(defn double-c?
  "returns true if this is a double consonant."
  ([stemmer]
    (double-c? stemmer (get-index stemmer)))
  ([stemmer j]
    (and (>= j 1)
         (= (nth (:word stemmer) j)
            (nth (:word stemmer) (dec j)))
         (consonant? stemmer j))))
(defn cvc?
  "true if (i-2 i-1 i) has the form of CVC and also if the second C is not w, x or y.
   This is used when trying to restore an *e* at the end of a short word.
   e.g. cav(e), lov(e), hop(e), crim(e) but snow, box, tray"
  ([stemmer]
    (cvc? stemmer (get-index stemmer)))
  ([stemmer i]
    (and (>= i 2)
         (consonant? stemmer (- i 2))
         (vowel? stemmer(dec i))
         (consonant? stemmer i)
         (not (#{\w \x \y} (nth (:word stemmer) i))))))
(defn m
  "Measures the number of consonant sequences between
  the start of word and position j. If c is a consonant
  sequence and v a vowel sequence, and <...> indicates
  arbitrary presence,
    <c><v>       -> 0
    <c>vc<v>     -> 1
    <c>vcvc<v>   -> 2
    <c>vcvcvc<v> -> 3
    ...
  "
  [stemmer]
  (let [
        j (get-index stemmer)
        count-v (fn [n i]
                  (cond (> i j) [:return n i]
                        (vowel? stemmer i) [:break n i]
                        :else (recur n (inc i))))
        count-c (fn [n i]
                  (cond (> i j) [:return n i]
                        (consonant? stemmer i) [:break n i]
                        :else (recur n (inc i))))
        count-cluster (fn [n i]
                        (let [[stage1 n1 i1] (count-c n i)]
                          (if (= stage1 :return)
                            n1
                            (let [[stage2 n2 i2] (count-v (inc n1) (inc i1))]
                              (if (= stage2 :return)
                                n2(recur n2 (inc i2)))))))
        [stage n i] (count-v 0 0)
        ]
    (if (= stage :return)
      n(count-cluster n (inc i)))))

(defn ends?
  "true if the word ends with s."
  [stemmer s]
  (let [word (subword stemmer), sv (vec s), j (- (count word) (count sv))]
    (if (and (pos? j) (= (subvec word j) sv))
      [(assoc stemmer :index (dec j)) true]
      [stemmer false])))

(defn set-to 
  "this sets the last j+1 characters to x and readjusts the length of b."
  [stemmer new-end]
  (reset-index (into (subword stemmer) new-end)))

(defn r 
  "This is used further down."
  [stemmer orig-stemmer s]
  (if (pos? (m stemmer))
    (set-to stemmer s)
    orig-stemmer))

(defmacro if-ends?
  "Instead of the function ends?, I'm using this:
  (if-ends? x (make-stemmer \"names\") [\\s]
            (println x \"no longer has a plural suffix\")
            (println x \"never had a plural suffix\"))
  "
  ([var stemmer end true-expr]
    (let [vend (vec end)]
    `(let [stemmer# ~stemmer, 
           end# ~vend,
           word# (subword stemmer#),
           j# (- (count word#) (count end#))]
       (if (and (pos? j#) (= (subvec word# j#) end#))
         (let [~var (assoc stemmer# :index (dec j#))]
           ~true-expr)
         stemmer#))))
  ;; ... the full version of if-ends? goes here
  ([var stemmer end true-expr false-expr]
    (let [vend (vec end)]
    `(let [stemmer# ~stemmer, 
           end# ~vend,
           word# (subword stemmer#),
           j# (- (count word#) (count end#))]
       (if (and (pos? j#) (= (subvec word# j#) end#))
         (let [~var (assoc stemmer# :index (dec j#))]
           ~true-expr)
         (let [~var stemmer#]
           ~false-expr))))))

(defn make-cond-ends-test
  [var stemmer word end true-expr false-expr]
  (let [vend (vec end)]
    `(let [j# (- (count ~word) ~(count vend))]
       (if (and (pos? j#) (= (subvec ~word j#) ~vend))
         (let [~var (assoc ~stemmer :index (dec j#))]
           ~true-expr)
         ~false-expr))))

(defmacro cond-ends-helper
  "this helps cond-ends? by processing the test-exprs pairs in cond-ends?'s environment."
  ([var stemmer word end true-expr]
    (if (= end :else)
      `(let [~var ~stemmer]
         ~true-expr)
      (make-cond-ends-test var stemmer word end true-expr stemmer)))
  ([var stemmer word end true-expr & more]
    (make-cond-ends-test
      var stemmer word end true-expr
      `(cond-ends-helper ~var ~stemmer ~word ~@more))))

(defmacro cond-ends?
  "This is the same as a stacked series of if-ends?.
   This just sets up the environment for cond-ends-helper."
  [var stemmer & test-exprs]
  `(let [stemmer# ~stemmer, word# (subword stemmer#)]
     (cond-ends-helper ~var stemmer# word# ~@test-exprs)))

(defn stem-plural
  "This is part of step 1ab. It removes plurals (-s) from a stem."
  [stemmer]
  (if (= (last (:word stemmer)) \s)
    (cond-ends? st stemmer
                "sses" (reset-index (pop (pop (:word st))))
                "ies" (set-to st "i")
                :else (if (and (>= (count (:word st)) 2)
                               (not= (nth (:word st) (- (count (:word st)) 2)) \s))
                        (assoc st :word (pop (:word st)))
                        st))
    stemmer))

(defn stem-expand-suffix
  "This is part of Step 1ab. It expands -at, -bl and -iz by adding an -e in certain circumstances."
  [stemmer]
  (cond-ends? st stemmer 
              "at" (set-to st "ate")
              "bl" (set-to st "ble")
              "iz" (set-to st "ize")
              :else (cond 
                      (double-c? st (dec (count (:word st))))
                      (if (#{\l \s \z} (last (:word st)))
                        st
                        (assoc st :word (pop (:word st))))
                      (and (= (m st) 1) (cvc? st (dec (count (:word st)))))
                      (set-to st "e")
                      :else st)))

(defn stem-verb-ending
  "This is part of step 1ab. It removes verb endings -ed and -ing."
  [stemmer]
  (cond-ends? st stemmer
              "eed" (if (pos? (m st))
                      (assoc st :word (pop (:word st)))
                      stemmer)
              "ed" (if (vowel-in-stem? st)
                     (stem-expand-suffix (assoc st :word (subword st)))
                     stemmer)
              "ing" (if (vowel-in-stem? st)
                      (stem-expand-suffix (assoc st :word (subword st)))
                      stemmer)))

(defn step-1ab
  "step-1ab gets rid of plurals and -ed or -ing. E.g.,
    caresses -> caress
    ponies -> poni
    ties -> ti
    caress -> caress
    cats -> cat
    feed -> feed
    agreed -> agree
    disabled -> disable
    matting -> mat
    mating -> mate
    meeting -> meet
    milling -> mill
    messing -> mess
    meetings -> meet
  "
  [stemmer]
  (-> stemmer stem-plural stem-verb-ending))

(defn step-1c
  "Turns terminal y to i when there is another vowel in the stem."
  [stemmer]
  (if-ends? st stemmer "y"
            (if (vowel-in-stem? st)
              (reset-index (conj (pop (:word st)) \i))
              stemmer)))

(defn step-2
  [stemmer]
  (cond-ends? st stemmer
              "ational" (r st stemmer "ate")
              "tional" (r st stemmer "tion")
              "enci" (r st stemmer "ence")
              "anci" (r st stemmer "ance")
              "izer" (r st stemmer "ize")
              "bli" (r st stemmer "ble")
              "alli" (r st stemmer "al")
              "entli" (r st stemmer "ent")
              "eli" (r st stemmer "e")
              "ousli" (r st stemmer "ous")
              "ization" (r st stemmer "ize")
              "ation" (r st stemmer "ate")
              "ator" (r st stemmer "ate")
              "alism" (r st stemmer "al")
              "iveness" (r st stemmer "ive")
              "fulness" (r st stemmer "ful")
              "ousness" (r st stemmer "ous")
              "fulness" (r st stemmer "ful")
              "ousness" (r st stemmer "ous")
              "aliti" (r st stemmer "al")
              "iviti" (r st stemmer "ive")
              "biliti" (r st stemmer "ble")
              "logi" (r st stemmer "log")))

(defn step-3
  "deals with -ic-, -full, -ness, etc., using
  a similar strategy to step-2."
  [stemmer]
  (cond-ends? st stemmer
              "icate" (r st stemmer "ic")
              "ative" (r st stemmer "")
              "alize" (r st stemmer "al")
              "iciti" (r st stemmer "ic")
              "ical" (r st stemmer "ic")
              "ful" (r st stemmer "")
              "ness" (r st stemmer "")))

(defn chop 
  "If there is more than one internal consonant cluster in the stem,
   this chops the ending (as identified by the index)."
  [stemmer]
  (if (> (m stemmer) 1)
    (assoc stemmer :word (subword stemmer))
    stemmer))

(defn step-4
  "takes off -ant, -ence, etc., in context <c>vcvc<v>."
  [stemmer]
  (cond-ends? st stemmer
              "al" (chop st)
              "ance" (chop st)
              "ence" (chop st)
              "er" (chop st)
              "ic" (chop st)
              "able" (chop st)
              "ible" (chop st)
              "ant" (chop st)
              "ement" (chop st)
              "ment" (chop st)
              "ent" (chop st)
              "ion" (if (#{\s \t} (index-char st))
                      (chop st)
                      stemmer)
              "ou" (chop st)
              "ism" (chop st)
              "ate" (chop st)
              "iti" (chop st)
              "ous" (chop st)
              "ive" (chop st)
              "ize" (chop st)))

(defn rule-e 
  "This removes the final -e from a word if:
   - there is more than one internal consonant cluster or 
   - there is exactly one final consonant cluster and 
   it is not preceded by a CVC sequence."
  [stemmer]
  (if (= (last (:word stemmer)) \e)
    (let [a (m stemmer)]
      (if (or (> a 1)
              (and (= a 1)
                   (not (cvc? stemmer (dec (:index stemmer))))))
        (pop-word stemmer)
        stemmer))
    stemmer))

(defn rule-l 
  "This changes -ll to -l if (> (m) 1)."
  [stemmer]
  (if (and (= (last (:word stemmer)) \l)
           (double-c? stemmer (dec (count (:word stemmer))))
           (> (m stemmer) 1))
    (pop-word stemmer)
    stemmer))

(defn step-5 
  "removes a final -e and changes -ll to -l if (> (m) 1)."
  [stemmer]
  (-> stemmer :word reset-index rule-e rule-l))

