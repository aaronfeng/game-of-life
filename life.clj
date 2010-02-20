;;; The Game of Life, also known simply as Life, is a cellular automaton.
(ns com.leftrightfold.life)

(defn show-env [board]
  (doseq [rows board] 
    (doseq [item rows] (print (format "%-8s" (name item)))) 
    (println))
  (println))

(defn kill-cell [board {:keys [row column]}]
  (assoc board row (assoc (board row) column :dead)))
  
(defn resurrect-cell [board {:keys [row column]}]
  (assoc board row (assoc (board row) column :alive)))

(defn dead-rule [neighbors] 
  ({3 resurrect-cell} neighbors kill-cell))

(defn alive-rule [neighbors]
  ({2 resurrect-cell 3 resurrect-cell} neighbors kill-cell))

(def *rules* {:alive alive-rule :dead  dead-rule})

(defn valid [board {:keys [row column status]}]
  (when-not (or (< row 0) 
		(>= row (count board)) 		
		(< column 0)
		(>= column (count (first board))))
    {:row row :column column :status status}))

(defn nnth [array-2d row column]
  (-> array-2d (nth row) (nth column)))

(defn all-neighbors [board {:keys [row column]}]
  (let [neighbors {:up         (valid board {:row (dec row) :column column       :status (fn [] (nnth board (dec row) column))       })
		   :down       (valid board {:row (inc row) :column column       :status (fn [] (nnth board (inc row) column))       })
		   :left       (valid board {:row row       :column (dec column) :status (fn [] (nnth board row (dec column)))       })
		   :right      (valid board {:row row       :column (inc column) :status (fn [] (nnth board row (inc column)))       })
		   :up-left    (valid board {:row (dec row) :column (dec column) :status (fn [] (nnth board (dec row) (dec column))) })
		   :up-right   (valid board {:row (dec row) :column (inc column) :status (fn [] (nnth board (dec row) (inc column))) })
		   :down-left  (valid board {:row (inc row) :column (dec column) :status (fn [] (nnth board (inc row) (dec column))) })
		   :down-right (valid board {:row (inc row) :column (inc column) :status (fn [] (nnth board (inc row) (inc column))) })}]
    neighbors))

(defn alive-neighbors [neighbors]
  (count (remove (fn [[k v]] 
		   (or (nil? v) 
		       (= ((:status v)) :dead))) 
		 neighbors)))

(defn check-neighbors [board {:keys [row column status] :as cell}]
  (let [neighbors       (all-neighbors board cell)
	alive-neighbors (alive-neighbors neighbors)]
    (((*rules* status) alive-neighbors) board cell)))

(defn run [board]
  (loop [b board row 0]
    (if (< row (count b))
      (recur 
       (loop [bb b column 0]
	 (if (< column (count (first bb)))
	   (recur (check-neighbors bb {:row row :column column :status (nnth bb row column)})
		  (inc column))
	   bb))
       (inc  row))
      b)))

(defn seed [board]
  (loop [b board [[row column] :as c] [[0 3] [1 3] [0 4] [1 4] [1 2] [1 3] [1 4] [1 5] [1 6]
				       [2 1] [2 2] [2 3] [3 4] [3 3]]]
    (if (nil? row)
      b
      (recur (assoc b row (assoc (b row) column :alive)) (rest c)))))

(comment
  (def board (seed (vec (repeat 20 (vec (repeat 20 :dead))))))
  ;; run 100 iterations
  (show-env (nth (iterate run board) 101))
)
