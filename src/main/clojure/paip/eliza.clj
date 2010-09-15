;; Eliza, adapted from PAIP chapter 5
(ns #^{:doc "Eliza in Clojure"}
  paip.eliza
  (:require [clojure.contrib.seq-utils :as seq-utils])
  (:use [clojure.test]))

(def ^{:private true}
     *eliza-rules*
     '(((?*x hello ?*y)
	(How do you do. Please state your problem.))
       ((?*x I want ?*y)
	(What would it mean if you got ?y ?)
	(Why do you want ?y ?)
	(Suppose you got ?y soon))
       ((?*x if ?*y)
	(Do you really think it is likely that ?y ?)
	(Do you wish that ?y ?)
	(What do you think about ?y ?)
	(Really? If ?y ?))
       ((?*x no ?*y)
	(Why not?) (You are being rather negative.)
	(Are you saying no just to be negative?))
       ((?*x I was ?*y)
	(Were you really?) (Perhaps I already knew you were $y .)
	(Why do you tell me you were ?y now?))
       ((?*x I feel ?*y)
	(Do you often feel ?y ?))
       ((?*x I felt ?*y)
	(What other feelings do you have?))
       ((?*x)
	(I am not sure I understand.)
	(Could you put that another way?)
	(Go on.))))


(defn- pattern-variable?
  "Is the argument a symbol that looks like a pattern variable?"
  [x]
  (and (symbol? x)
       (.startsWith (name x) "?")
       (> (.length (name x)) 1)))

(deftest test-pattern-variable?
  (is (pattern-variable? '?x))
  (is (pattern-variable? '?1))
  (is (pattern-variable? '?xy))
  (is (pattern-variable? '?1-2))
  (is (not (pattern-variable? 'a)))
  (is (not (pattern-variable? 'x?)))
  (is (not (pattern-variable? '(?x))))
  (is (not (pattern-variable? :?x)))
  (is (not (pattern-variable? "?x")))
  (is (not (pattern-variable? ['?x])))
  (is (not (pattern-variable? '?))))

(defn- segment-pattern?
  "Is x a pattern variable representing a segment match?"
  [x]
  (and (symbol? x)
       (.startsWith (name x) "?*")
       (> (.length (name x)) 2)))

(deftest test-segment-pattern?
  (is (not (segment-pattern? '?x)))
  (is (not (segment-pattern? '?1)))
  (is (not (segment-pattern? '?xy)))
  (is (not (segment-pattern? '?1-2)))
  (is (segment-pattern? '?*x))
  (is (segment-pattern? '?*1))
  (is (segment-pattern? '?*xy))
  (is (segment-pattern? '?*1-2))
  (is (not (segment-pattern? '*)))
  (is (not (segment-pattern? 'x?*)))
  (is (not (segment-pattern? 'x*?)))
  (is (not (segment-pattern? '(?*x))))
  (is (not (segment-pattern? :?*x)))
  (is (not (segment-pattern? "?*x")))
  (is (not (segment-pattern? ['?*x])))
  (is (not (segment-pattern? '?*)))
  (is (not (segment-pattern? '?))))

(defn- segment-pattern-variable
  "Retrieves the variable corresponding to a segment pattern."
  [p]
  {:pre [(.startsWith (name p) "?*")]}
  (symbol (str "?" (subs (name p) 2))))

(deftest test-segment-pattern-variable
  (is (= '?x (segment-pattern-variable '?*x)))
  (is (= '?foo (segment-pattern-variable '?*foo)))
  (is (= '?+= (segment-pattern-variable '?*+=))))

(defn- match-pattern-variable
  "Does var match input? Uses or updates bindings, then returns them."
  [var input bindings]
  (let [binding (bindings var)]
    (cond (nil? binding) (assoc bindings var input)
	  (= input binding) bindings
	  :else nil)))

(defn- match-pattern)

(defn- match-pattern-segment
  "Match the segment pattern (?*var pat) against input."
  [pattern input bindings]
  {:pre [(segment-pattern? (first pattern))]}
  (let [var (segment-pattern-variable (first pattern))
	pat (rest pattern)]
    (if (empty? pat)
      (match-pattern-variable var input bindings)
      (loop [ps (seq-utils/positions #(= % (first pat)) input)]
	(if (empty? ps)
	  nil
	  (let [p (first ps)
		b2 (match-pattern pat (nthnext input p)
				  (match-pattern-variable
				   var (take p input) bindings))]
	    (if (nil? b2) (recur (rest ps)) b2)))))))


(defn- match-pattern
  "Match pattern against input in the context of the bindings"
  ([pattern input bindings]
     (cond (empty? pattern) (if (empty? input) bindings nil)

	   (= (first pattern) (first input))
	   (match-pattern (rest pattern) (rest input) bindings)

	   (segment-pattern? (first pattern))
	   (match-pattern-segment pattern input bindings)

	   (pattern-variable? (first pattern))
	   (match-pattern (rest pattern) (rest input)
			  (match-pattern-variable (first pattern)
						  (first input)
						  bindings))

	   :else nil))
  ([pattern input]
     (match-pattern pattern input {})))

(defn- rule-pattern [rule] (first rule))

(defn- rule-responses [rule] (rest rule))

(def ^{:private true}
     *viewpoint-map* {'I 'you 'you 'I 'me 'you 'am 'are})

(defn- switch-viewpoint
  "Change I to you and vice versa, and so on."
  [bindings]
  (reduce (fn [m pair]
	    (assoc m (first pair)
		   (map #(*viewpoint-map* % %) (second pair))))
	  {} bindings))

(defn use-eliza-rules
  "Find some rule with which to transform the input."
  [input]
  (some #(let [result (match-pattern (rule-pattern %) input)]
	   (when result
	     (replace (switch-viewpoint result)
		      (rand-nth (rule-responses %)))))
	*eliza-rules*))

(defn eliza
  "Respond to user input using pattern matching rules."
  []
  (print "eliza> ")
  (flush)
  (println (flatten (use-eliza-rules (read))))
  (recur))