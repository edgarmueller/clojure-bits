(ns cma
  (:refer-clojure :exclude [pop peek load and or new]))
		  
;;; instr macro
;;; handle optional parameters
;;; sp/cp not really needed (make them intrinsic)
;;; remove make-* functions
;;; is it possible to extract update-vm?
;;; split into 2 modules

;;; what's missing
;;; malloc (p. 42)
;;; loadr (p. 52)
;;; mark (p. 55)
;;; call
;;; enter
;;; alloc
;;; slide
;;; return
;;; see page 58 for complete reference

;;; defprotocol VM
(defprotocol CMa
  "Protocol for the C-Machine"
  (add [this])
  (and [this])
  
  (or [this])
  (neg [this])
  (neq [this])
  (leq [this])
  (le [this])
  (gr [this])
  (geq [this])
  (eq [this])
  
  (load [this] [this args])
  (loada [this q])
  (loadc [this val])
  (store [this] [this args])
  (storea [this q])

  (loadrc [this j])
  (loadr [this j m])
  (storer [this j m])
  (mark [this])
  (call [this])
  (enter [this m])
  (alloc [this m])
  (slide [this q m])
  (return [this q])

  (sub [this])
  (mul [this])
  (pop [this])
  (peek [this])
  (frame [this from to])
  (new_hp [this])
  (apply-bin-op-instr [this op])
  (push [this arg])) ;; make private(?)

;;; todo: error checking, index range
(defn replace-nth [coll index el]
  (concat
   (take index coll)
   [el]
   (drop (inc index) coll)))

;; hide acc
(defn peek-n [mach i acc]
  (if (= i 0)
    acc
    (recur (pop mach) (dec i) (cons (peek mach) acc))))

(defn pop-n [mach n]
  (if (= n 0)
    mach
    (recur (pop mach) (dec n))))

(defn c-nil? [v]
  (if (not= v false) 1 0))

;  (peek [this])
;  (sp [this])
;  (hp [this])
;  (new-hp [this n]))

(defrecord Instr [name params])

;; handle error conditions, e.g.
;; even number of args, unknown keywords
(defmacro update-vm [vm & kvs]
  `(assoc ~vm ~@kvs))

(defrecord Mach [pc sp ep c heap hp]
  CMa
  (push [this v]
	(let [h (:heap this)
	      sp (:sp this)]
	  (update-vm this :heap (concat (take (inc sp) h) [v] (drop (+ 2 sp) h))
		     :sp (inc sp))))
  ;;; nth < hp && nth < sp (include as precondition)
  (load [this]
	(load this 1))
  
  (load [this arg]
;	(if (empty? args)
;	  (let [a (peek this)
;		n (nth (:heap this) a)]
;	    (push (pop this) n))
	  (let [m arg
		addr (peek this)
		f (frame this addr m)
		new-mach (pop this)]
	    (reduce #(push %1 %2) new-mach f)))

  (loadc [this val]
	 (push this val))

  (loada [this q]
	 (load (loadc this q)))

  (store [this]
	 (let [a (peek this)
	       new-mach (pop this)]
	   (update-vm this :sp (dec (:sp this)) :heap 
		      (replace-nth (:heap new-mach) a (peek new-mach)))))

  (storea [this q]
	  (store (loadc this q)))
  
  (pop [this]
       	(let [h (:heap this)
	      sp (:sp this)]
	  (update-vm this :heap (concat (take sp h) [nil] (drop (inc sp) h))
		     :sp (dec sp))))

  (peek [this]
	(nth (:heap this) (:sp this)))

  ;;; add sanity checks: to <= sp, from >= 0 (use pre)
  (frame [this from to]
	 (let [h (:heap this)]
	   (drop from (take to h))))

  (apply-bin-op-instr [this op]
		      (let [a (peek this)
			    b (peek (pop this))
			    res (apply op [a b])]
			(push (pop (pop this)) res)))

  (add [this] ;;; handle errorcases
       (apply-bin-op-instr this +))

  (and [this]
       (let [a (peek this)
	     b (peek (pop this))]
	 (push (pop (pop this))
	       (c-nil?
		(clojure.core/and (not= a 0) (not= b 0))))))

  (or [this]
      (let [a (peek this)
	    b (peek (pop this))]
	 (push (pop (pop this))
	       (c-nil?
		(clojure.core/or (not= a 0) (not= b 0))))))
      
;  (neg [this]
;       (push (pop this) (peek this)))
  
  (eq [this]
      (apply-bin-op-instr this #(c-nil? (= %1 %2))))

  (leq [this]
       (apply-bin-op-instr this #(c-nil? (<= %2 %1))))
         
  (le [this]
      (apply-bin-op-instr this #(c-nil? (< %2 %1))))
  
  (gr [this]
      (apply-bin-op-instr this #(c-nil? (> %2 %1))))
  
  (geq [this]
       (apply-bin-op-instr this #(c-nil? (>= %2 %1))))
  
  (neq [this]
       (apply-bin-op-instr this #(c-nil? (not= %1 %2))))
  
  (sub [this]
       (apply-bin-op-instr this -))

  (mul [this]
       (apply-bin-op-instr this *))

  (loadrc [this j]
	  (push this (+ (:fp this) j)))

  (loadr [this j m]
	 (loadrc this j)
	 (load this m))

  (storer [this j m]
	  (loadrc this j)
	  (store m))

  (mark [this]
	(push (push this (:ep this)) (:fp this)))

  (call [this]
	(let [q (peek this)
	      p (:pc this)]
	  (update-vm this :heap (push p (pop this))
		     :fp p :pc q)))

  (enter [this m]
	 (update-vm this :ep (+ (:ep this) m)))

  (alloc [this m]
	 (update-vm this :sp (+ (:sp this) m)))

  ;; pre: q > 0
  (slide [this q m]
	 (let [t (- (:sp this) q m)
	       p (peek-n this m [])]
	   (reduce #(push %1 %2) (pop-n this (+ q m)) p)))
; factor out
;		 	    (reduce #(push %1 %2) new-mach f)))

  (return [this q]
	  (let [fp (dec (:fp this))]
	    (update-vm :fp fp
		       :pc (:fp this)
		       :ep (peek (pop (pop this)))
		       :heap (pop-n this (+ 3 q)))))
  
  (new_hp [this]
       (let [h (:heap this)
	     n (peek this)
	     sp (:sp this)]
	 (update-vm this :heap (concat (take sp h)
				       [(- (:hp this) n)]
				       (drop (inc sp) h))
		    :hp (- (:hp this) n)))))

(defn make-instr [name & params]
  (Instr. name params))

(defn make-mach [pc sp ep c heap heapsize]
  (Mach. pc sp ep c (replicate heapsize nil) heapsize))

(defn std-vm []
  (make-mach 0 -1 0 [] [] 10))

(defmacro instr [name & params]
  `(make-instr (keyword '~name) ~@params))

(defn apply-bin-op-instr [op mach]
  (let [s (:heap mach)]
    (update-vm mach :sp (dec (:sp mach))
	       :s (cons (apply op (take 2 s)) (drop 2 s)))))

(defn instr-dispatch [i m]
  [(:name i) :any])

(defmulti exec-instr instr-dispatch)

(defmethod exec-instr [:jump :any] [i mach]
	   (update-vm mach :pc (first (:params i))))

(defmethod exec-instr [:jumpz :any] [i mach]
	   (let [s (:s mach)]
	     (update-vm mach :s (rest s) :sp (dec (:sp mach))
			:pc (if (= (first s) 0)
			      (first (:params i))
			      (:pc mach)))))

(defmethod exec-instr [:jumpi :any] [i mach]
	   (let [s (:s mach)
		 q (first s)]
	     (update-vm mach :s (rest s) :sp (dec (:sp mach))
			:pc (+ (first (:params i)) q))))

(defmethod exec-instr [:gt :any] [i mach]
	   (apply-bin-op-instr #(if (> %2 %1) 1 0) mach))

(defmethod exec-instr [:leq :any] [i mach]
	   (apply-bin-op-instr #(if (<= %2 %1) 1 0) mach))

(defmethod exec-instr [:geq :any] [i mach]
	   (apply-bin-op-instr #(if (>= %2 %1) 1 0) mach))

(defmethod exec-instr [:lt :any] [i mach]
	   (apply-bin-op-instr #(if (< %2 %1) 1 0) mach))

(defmethod exec-instr [:loadc :any] [i mach]
	   (loadc mach (first (:params i))))

(defmethod exec-instr [:add :any] [i mach]
	   (add mach))

;(defmethod exec-instr [:new :any] [i mach]
	   
;;; exec-instr sollte pc inkrementieren
(defn exec [instr mach]
  (let [i (first instr)
	r (rest instr)]
    (if (empty? instr)
      mach
      (exec r (exec-instr i mach)))))
  
; example
(def instrs [(instr loadc 5)
	     (instr loadc 1)
	     (instr loadc 20)
	     (instr add)])
;	     (instr load)
;	     (instr loadc 1)
;
;	     (instr loadc 0)
;	     (instr store)
;	     (instr loadc 0)
;	     (instr gt)
;	     (instr jumpi 40)])

(def instrs2 [
	      (instr loadc 1)
	      (instr loadc 2)
	      (instr loadc 3)
	      (instr loadc 0)
	      (instr loadc 13)
	      (instr loadc 14)
	      (instr storea 15 3)])
;    	     (instr loadc 15)
;	     (instr store 3)])
     
;;; TODO: 


		    
  