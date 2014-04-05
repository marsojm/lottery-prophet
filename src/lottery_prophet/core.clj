(ns lottery-prophet.core
  (:require [clojure.java.io :as io]
   :require [clojure.string :as s]))

(defn read-resource
  [path]
  (slurp  path))

(defn find-lottery-lines 
  [source]
  (re-seq #": <strong>([0-9 ,|\+]*)</strong><br />" source))

(defn clean-data
  [data]
  (map second data))

(defn write-to-file [filename a-seq]
  (with-open [wrt (io/writer filename)]
    (doseq [line a-seq]
      (.write wrt (str line "\n")))))

(defn remove-from-plus
  [data]
  (s/trim (s/replace data #"\+.*" "")))

(defn str->int
  [x]
  (Integer/parseInt (s/trim x)))  
  
(defn str->list
  [data]
  (s/split data #", "))
  
(defn str->int-seq
  [data]
  (vec (map str->int (str->list data))))
  
(defn normalize 
  [data]
  (let [a-sum (reduce + 0 (map second data))
        add-normalized-value (fn [[n freq]]
                              (vector n (with-precision 10 (/ freq a-sum))))]
    (map add-normalized-value (vec data)))) 
    
(defn closest-by-acc
  [nums to-find]
  (let [ res (first (drop-while #(< (second %) to-find) nums))]
    (if (nil? res) (last nums) res)))      
      
(defn add-acc
  [numbers]
  (loop [[lot-num freq] (first numbers)
          xs (rest numbers)
          acc 0
          acc-coll '()]
    (if (nil? lot-num)
      (sort acc-coll)
      (let [new-acc (+ acc freq)]
        (recur (first xs) (rest xs) new-acc (conj acc-coll (vector lot-num new-acc)))))))

(defn freqs
  "return a map of frequencies: {item freq}"
  [data]
  (frequencies (apply concat data)))
  
(defn remove-numbers
  [nums nums-to-remove]
  (let [pred (fn [[num freq]]
               (not (some #(= num %) nums-to-remove)))]
    (filter pred nums)))

(defn filter-rows-containing-num
  [nums n]
  (filter #(some #{n} %) nums))

(defn find-num
  [nums n]
  (first (drop-while #(not (== n (first %))) nums)))  
  
(defn inc-frequencies
  [numbers inc-numbers]
  (let [increase-if-matches (fn [[num freq] i-nums]
                              (let [[i-num i-freq] (find-num i-nums num)]
                              (if (nil? i-num)
                                (vector num freq)
                                (vector num (+ freq i-freq)))))]
    (map #(increase-if-matches % inc-numbers ) numbers )))  
;(def original-data (read-resource "data/original.htm"))
;(def preprocessed-data (find-lottery-lines original-data))
;(def cleaned-data (clean-data preprocessed-data))
;(write-to-file "data/lottery.dat" cleaned-data)
(def lottery-data ( s/split-lines (read-resource "data/lottery.dat")))

(def lottery-numbers (map remove-from-plus lottery-data))
(def final-data (map str->int-seq lottery-numbers))

(def number-frequencies (sort (freqs final-data)))
(def freq-count (reduce + 0 (map second number-frequences)))  

(def accumulated-number-frequencies (add-acc number-frequencies))


(defn find-winning-numbers!
  [numbers]
  (loop [nums numbers
         acc-nums (add-acc numbers)
         upper-limit (second (last acc-nums))
         winning-row '()
         win-num (first (closest-by-acc acc-nums (rand-int (inc upper-limit))))
         rounds 7]
    (if (== rounds 0)
      (sort winning-row)
      (let [new-nums (remove-numbers nums [win-num])
            new-acc (add-acc new-nums)
            new-upper-limit (second (last new-acc))
            new-winning-row (conj winning-row win-num)
            new-win-num (first (closest-by-acc new-acc (rand-int (inc new-upper-limit))))]
        (recur new-nums new-acc new-upper-limit new-winning-row new-win-num (dec rounds))))))

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  