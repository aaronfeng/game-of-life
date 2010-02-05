;;; The Game of Life, also known simply as Life, is a cellular automaton.
(ns com.leftrightfold.life)

(def *dimension* {:rows 20 :columns 20})
(def *env* (to-array-2d (repeat (:rows *dimension*) 
				(repeat (:columns *dimension*) :dead))))

(defn kill-cell [cell] 
  (aset *env* (:row cell) (:column cell) :dead))

(defn resurrect-cell [cell]
  (aset *env* (:row cell) (:column cell) :alive))

(def *alive-cell-rules* {0 kill-cell
			 1 kill-cell
			 2 resurrect-cell
                         3 resurrect-cell
			 4 kill-cell
			 5 kill-cell
			 6 kill-cell
			 7 kill-cell
			 8 kill-cell})

(def *dead-cell-rules* {0 kill-cell
			1 kill-cell
			2 kill-cell
			3 resurrect-cell
			4 kill-cell
			5 kill-cell
			6 kill-cell
			7 kill-cell
			8 kill-cell})

(def *rules* {:alive *alive-cell-rules*
	      :dead  *dead-cell-rules*})

(defn valid [{:keys [row column status]}]
  (when-not (or (< row 0) 
		(>= row (:rows *dimension*)) 		
		(< column 0)
		(>= column (:columns *dimension*)))
    {:row row :column column :status status}))


(defn all-neighbors [{:keys [row column]}]
  (let [neighbors {:up         (valid {:row (dec row) :column column       :status (fn [] (aget *env* (dec row) column))       })
		   :down       (valid {:row (inc row) :column column       :status (fn [] (aget *env* (inc row) column))       })
		   :left       (valid {:row row       :column (dec column) :status (fn [] (aget *env* row (dec column)))       })
		   :right      (valid {:row row       :column (inc column) :status (fn [] (aget *env* row (inc column)))       })
		   :up-left    (valid {:row (dec row) :column (dec column) :status (fn [] (aget *env* (dec row) (dec column))) })
		   :up-right   (valid {:row (dec row) :column (inc column) :status (fn [] (aget *env* (dec row) (inc column))) })
		   :down-left  (valid {:row (inc row) :column (dec column) :status (fn [] (aget *env* (inc row) (dec column))) })
		   :down-right (valid {:row (inc row) :column (inc column) :status (fn [] (aget *env* (inc row) (inc column))) })}]
    neighbors))

(defn alive-neighbors [neighbors]
  (count (remove (fn [[k v]] 
		   (or (nil? v) 
		       (= ((:status v)) :dead))) 
		 neighbors)))

(defn check-neighbors [cell]
  (let [neighbors       (all-neighbors cell)
	cell-status     (aget *env* (:row cell) (:column cell))
	alive-neighbors (alive-neighbors neighbors)]
    ;(prn (format "cell-status=%s alive-neighbors=%s" cell-status alive-neighbors))
    (((*rules* cell-status) alive-neighbors) cell)))

(defn show-env []
  (doseq [rows *env*] 
    (doseq [item rows] (print (format "%-8s" (name item)))) 
    (println))
  (println))

(defn seed []
  (doseq [[row column] [[0 3] [1 3] [0 4] [1 4] [1 2] [1 3] [1 4] [1 5] [1 6]
			[2 1] [2 2] [2 3] [3 4] [3 3]]]
    (aset *env* row column :alive)))

(defn run []
  (loop [row 0]
    (when (< row (:rows *dimension*))
      (loop [column 0]
	(when (< column (:columns *dimension*))
	  (check-neighbors {:row row :column column :status (aget *env* row column)})
	  (recur (inc column))))
      (recur (inc  row))))
  (show-env))

      
(seed)
(dotimes [x 100] (run))