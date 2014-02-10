(ns dt
 (:import [java.net URLEncoder])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io  :as io])
  (:require [clojure.data.json :as json])
  (:use clojure.set)
  )

(declare read-csv-input)

(defn read-csv-file
  "Reads CSV-data from file

   Valid options are
     :encoding  string name of encoding to use (default UTF-8)
     + the options of (read-csv-input)
  "
  [file-name & options]
    (let [{:keys [encoding] :or [encoding "UTF-8"]}  options]

    (with-open [in-file (io/reader file-name :encoding encoding)]
      (apply read-csv-input in-file options))))

(defn read-csv-input
  "Reads CSV-data from input (String or java.io.Reader) input (String or java.io.Reader)
   Valid options are
     :header-rows (default 1, 0 for no header, if > 1 tuples of values of all corresponding
                   header rows will be used to identify fields)
     :separator (default \\,)
     :quote (default \\\")
     :use-keywords (default true) Use keywords as the field titles
    "
  [input
   & {:keys [separator quote header-rows use-keywords]
     :or {separator \,
          quote \"
          header-rows 1
          use-keywords true
      }}
   ]
    (let [keywordize #(map keyword %)
          rows (doall
                 (csv/read-csv input
                   :separator separator
                   :quote quote))]
    (if (> header-rows 0)
      (let [cols (if (> header-rows 1)
                     ; create tuples of values of all header rows
                     ; to identify the fields
                     (apply map vector
                       (let [header-rows (take header-rows rows)]
                         (if use-keywords
                           (map #(keywordize %) header-rows)
                           header-rows
                         )))

                   (if use-keywords
                     (keywordize (first rows))
                     (first rows)))

            rows (drop header-rows rows)]


          (map #(zipmap cols %) rows))
      rows)))


(defn save-to-json [data output-file]
  (with-open [wrtr (io/writer output-file)]
      (json/write data wrtr)))



(defn save-to-csv
  "The optional :columns param specifies the ordered list of columns in the output
   (use the names or keywords before renaming).
   :rename-columns is a map of renamings applied to the columns (str or keyword) -> str."
  [data output-file & { columns :columns  rename-columns :rename-columns }]

  (let [cols             (or columns (sort (apply union (map #(-> % keys set) data))))
                             ; if no columns are specified use a union of all of the keys
                             ; used by the records
        stringify        #(if (keyword? %) (name %) (str %))
        rename           #(if rename-columns (or (get rename-columns %) %) %)
        column-names     (map  #(-> % rename stringify) cols)]
  (with-open [wrtr (io/writer output-file)]
      (csv/write-csv wrtr
        (concat
          [column-names]
          (for [row data]  (map #(get row %) cols)))))))


(defn fetch-url [address]
  "
    This will work on text files but corrupt binary files because BufferedReader
    assumes it is dealing with textual data.
   "
  (with-open [stream (.openStream (java.net.URL. address))]
    (let  [buf (java.io.BufferedReader.
                (java.io.InputStreamReader. stream))]
      (apply str (line-seq buf)))))


(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))



(defn words [text]
  (re-seq #"\p{Alpha}+" text))


; https://github.com/technomancy/swank-clojure/blob/master/src/swank/util/string.clj
(defn largest-common-prefix
  "Returns the largest common prefix of two strings."
  ([#^String a, #^String b]
     (apply str (take-while (comp not nil?) (map #(when (= %1 %2) %1) a b))))
  {:tag String})



(defn common-prefixes
  "Finds words in two given seqs which have common prefixes of at least min-length"
  [words1 words2 min-length]
  (filter #(>= (count %) min-length)
        (for [w1 words1  w2 words2]
          (largest-common-prefix w1 w2))))

(defn common-word-prefixes [text1 text2 min-length]
  (let [words1  (words text1)
        words2  (words text2)]
    (common-prefixes words1 words2 min-length)))


(let [name-similarity
      (fn [name1 name2]
        (let [words1            (words (.toLowerCase name1))
              words2            (words (.toLowerCase name2))
              common            (common-prefixes words1 words2 2)
              common-lengths    (map count common)
              common-length     (reduce + common-lengths)
              total-length      (fn [words] (reduce + (map count words)))
              uncommon-length   (+ (total-length words1) (total-length words2) (- (* 2 common-length)))
              score             (+ (reduce + (map #(Math/pow 10 %) common-lengths)) (- uncommon-length))]
          score
          ))]

(defn closest-name-code-finder
  "Returns a function which finds the closest entry of the original entries by name and returns its code.
   Expects a seq of entries: { :name ... :code ... }.
   The function can be used to find country codes by names which are not quite canonical."
  [entries]
    (fn [name]
      (let [get-score   (fn [country] {:country country :score (name-similarity name (:name country))})
            scores      (map get-score entries)]
        (->> scores
              (sort-by :score)
              (filter #(> (:score %) 0))
              reverse)))))

      ;(apply max-key #(name-similarity name (:name %)) countries))))

(defn encode-url-params [request-params]
  (let [encode #(URLEncoder/encode (str %) "UTF-8")
        coded (for [[n v] request-params] (str (encode n) "=" (encode v)))]
    (apply str (interpose "&" coded))))


(defn round
   [x & {p :precision}]
   (if (number? x)
     (if (integer? x)
       x
       (if p
         (let [scale (Math/pow 10 p)]
           (-> x (* scale) Math/round (/ scale)))
         (Math/round x)))))



; see https://github.com/clojure/algo.generic/blob/master/src/main/clojure/clojure/algo/generic/functor.clj
; and http://stackoverflow.com/questions/1676891/mapping-a-function-on-the-values-of-a-map-in-clojure
(defn fmap
  "Applies function f to each value of the map m."
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))


(defn parse-number [str]
  (if-not (empty? str) (let [n (read-string str)] (if (number? n) n))))
