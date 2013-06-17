(ns data-tackle
 (:import [java.net URLEncoder])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io  :as io])
  (:require [clojure.data.json :as json])
  (:use clojure.set)
  )

(declare read-csv-input)

(defn read-csv-file
  [file-name
   & opts]

  "Reads CSV-data from file

   Valid options are
     :encoding  string name of encoding to use (default UTF-8)
     + the options of (read-csv-input)
  "
    (with-open [in-file (io/reader file-name :encoding (get opts :encoding))]
      (apply read-csv-input in-file opts)))


(defn read-csv-input
  [input
   & {:keys [separator quote header-rows use-keywords]
     :or {separator \,
          quote \"
          header-rows 1
          use-keywords true
      }}
   ]
  "Reads CSV-data from input (String or java.io.Reader) input (String or java.io.Reader)
   Valid options are
     :header-rows (default 1, 0 for no header, if > 1 tuples of values of all corresponding
                   header rows will be used to identify fields)
     :separator (default \\,)
     :quote (default \\\")
     :use-keywords (default true) Use keywords as the field titles
    "
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


(defn save-to-csv [data output-file & {renamings :rename-columns}]
  "rename-columns is a map of renamings str->str"
  (let [columns       (sort (apply union (map #(-> % keys set) data)))
        column-names  (map #(if (keyword? %) (name %) (str %)) columns)
        column-names  (if renamings (replace renamings column-names) column-names)
        ]
  (with-open [wrtr (io/writer output-file)]
      (csv/write-csv wrtr
        (concat
          [column-names]
          (for [row data]  (map #(get row %) columns)))))))


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



(defn common-word-prefixes [text1 text2 min-length]
  (let [
     words1  (words text1)
     words2  (words text2)]

    (filter #(>= (count %) min-length)
      (for [w1 words1  w2 words2]
        (largest-common-prefix w1 w2)
      )))
  )


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
(defn fmap [f m]
  "Applies function f to each value of the map m."
  (into (empty m) (for [[k v] m] [k (f v)])))


(defn parse-number [str]
  (if-not (empty? str) (let [n (read-string str)] (if (number? n) n))))
