(ns test.csv-test
  (:gen-class)
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]))

;; this was found at https://stackoverflow.com/questions/13641618/read-csv-into-a-list-in-clojure
(defn take-csv
  [fname]
  (with-open [file (io/reader fname)]
    (-> file
        (slurp)
        (csv/parse-csv))))

(defn parseHistoryFiles [fileNames]
  (seq (.list (clojure.java.io/file fileNames))))

(defn boxMullard [randNum1, randNum2]
  (* (Math/sqrt (* -2 (Math/log randNum1))) (Math/cos (* 2 (* Math/PI randNum2)))))

(defn squaredDiff [real projected]
  (let [r (if (= "" real) 0 (Double/parseDouble real))
        proj (if (= "" projected) 0 (Double/parseDouble projected))]
          (Math/pow (- r proj) 2)
        )
)


(defn getProjected [map]
  (get map 25))

(defn getActual [map]
  (get map 34))

(defn getStd [map]
  (if (= map nil)
    0
    (Math/sqrt (/ (get map :squaredDiffs) (get map :n)))
    )
  )

(defn totalDiffs [map new]
  (+ (get map :squaredDiffs) new))

(defn incrementN [map]
  (+ (get map :n) 1))

;; newSlate = [{name: "Lebron James", projectedScore: 53, actualScore: 62}]
;; varianceMap = {"Lebron James": {squaredDiffs: 124, n: 10}
;;                "Kevin Durant": {squaredDiffs: 150, n: 12}}

(defn addSlate [varianceMap newSlate]
    (let [playerMap
          (if (get varianceMap (get newSlate 0))
            (get varianceMap (get newSlate 0))
            {:squaredDiffs 0 :n 0})
          diffChange
          (assoc playerMap :squaredDiffs (totalDiffs playerMap (squaredDiff (getActual newSlate) (getProjected newSlate))))
          nChange
          (assoc diffChange :n (incrementN diffChange))
          finalMap 
          (assoc varianceMap (get newSlate 0) nChange)]
      finalMap
))

(defn applyVariance [playerVariance playerScore]
  ;; create random normal distribution with box-mullard
  (let [std (boxMullard (rand 1) (rand 1))
        score (if (= "" playerScore) 0 (Double/parseDouble playerScore))]
    [(+ score (* playerVariance std)) std]))

;; scoreMap = [{name: "Lebron James", projectedScore: 53, price: 7300}]
(defn findBestTeam [scoreMap]
  (let [allTeams (for [i (range (count scoreMap))
                       j (range (count scoreMap))
                       k (range (count scoreMap))
                       :let [currentPrice (+ 
                                           (get (nth scoreMap i) :price) 
                                           (get (nth scoreMap j) :price) 
                                           (get (nth scoreMap k) :price))
                             currentScore (+ 
                                           (get-in (nth scoreMap i) [:score 0]) 
                                           (get-in (nth scoreMap j) [:score 0]) 
                                           (get-in (nth scoreMap k) [:score 0]))
                             currentStd (+
                                         (get-in (nth scoreMap i) [:score 1])
                                         (get-in (nth scoreMap j) [:score 1])
                                         (get-in (nth scoreMap k) [:score 1]))]
                       :when (and (< i j k)
                                  (> currentStd 3)
                                  (<= currentPrice 15000)
                                  (> currentScore 120))]
                   [[(get (nth scoreMap i) :name) 
                     (get (nth scoreMap j) :name) 
                     (get (nth scoreMap k) :name)] 
                    currentScore 
                    currentPrice 
                    currentStd])
       ]
    allTeams)
  )
  
(defn -main
  []
  (let [allFiles (parseHistoryFiles "C:/Users/Zach/dfs-optimizer/prevData")
        varMap
        (loop [i 0
               varianceArr {}]
          (let [allPlayers (take-csv (str "C:/Users/Zach/dfs-optimizer/prevData/" (nth allFiles i)))
                currVar
                (loop [j 1
                       cVar varianceArr]
                  (let [fSlate (addSlate cVar (nth allPlayers j))]

                    (if (< (+ 1 j) (count allPlayers))
                      (recur (+ j 1) fSlate)
                      fSlate)))]

            (if (< (+ 1 i) (count allFiles))
              (recur (+ i 1), currVar)
              currVar)))

        todayScores
        (take-csv (str "C:/Users/Zach/dfs-optimizer/projections/todayProjections.csv"))
        allScores
           (loop [i 1
                  playerList []]
             (let [currScoreMap (cons {:name (get (nth todayScores i) 0) 
                                       :score (applyVariance (getStd (get varMap (get (nth todayScores i) 0))) (get (nth todayScores i) 1)) 
                                       :price (Integer/parseInt (.replaceAll (get (nth todayScores i) 6) "[^0-9]" ""))} playerList)]
               (if (< (+ 1 i) (count todayScores))
                 (recur (+ i 1), currScoreMap)
                 currScoreMap)))]
    (findBestTeam allScores)

    )
  )
