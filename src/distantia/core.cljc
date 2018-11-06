;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;;   which can be found in the LICENSE file at the root of this
;;   distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns distantia.core)

(declare map-diff map-patch vec-diff vec-patch)

(defn diff
  [a b]
  {:pre [(or (and (map? a) (map? b))
             (and (vector? a) (vector? b)))]}
  (try
    (cond
      (and (map? a) (map? b)) (map-diff a b)
      (and (vector? a) (vector? b)) (vec-diff a b))
    (catch #?(:clj Exception :cljs js/Error) e
      (#?(:clj println :cljs js/console.error) "diff:" (pr-str a) (pr-str b))
      (throw e))))

(defn patch
  [a p]
  (try
    (cond
      (map? a) (map-patch a p)
      (vector? a) (vec-patch a p))
    (catch #?(:clj Exception :cljs js/Error) e
      (#?(:clj println :cljs js/console.error) "patch:" (pr-str a) (pr-str p))
      (throw e))))

(declare key-paths)

(defn map-diff
  [x y]
  (let [prefix? (fn [s pre] (= (take (count pre) s) pre))
        key-paths (distinct (concat (key-paths x) (key-paths y)))]
    [:m (reduce (fn [{:keys [a r c] :as patch} kp]
                  (let [xv (get-in x kp ::not-found)
                        yv (get-in y kp ::not-found)
                        xv? (not= ::not-found xv)
                        yv? (not= ::not-found yv)]
                    (cond
                      (and xv? yv? (= xv yv)) patch
                      (and xv? yv? (vector? xv) (vector? yv)) (cond-> patch
                                                                (->> c (map first) (not-any? (partial prefix? kp)))
                                                                (update :c conj [kp (vec-diff xv yv) :s]))
                      (and xv? yv?) (cond-> patch
                                      (->> c (map first) (not-any? (partial prefix? kp)))
                                      (update :c conj [kp yv]))
                      xv? (cond-> patch
                            (not-any? (partial prefix? kp) r)
                            (update :r conj kp))
                      yv? (cond-> patch
                            (->> a (map first) (not-any? (partial prefix? kp)))
                            (update :a conj [kp yv])))))
                {:a [] :r [] :c []} key-paths)]))

(defn map-patch
  [m [patch-type patch]]
  {:pre [(= :m patch-type)]}
  (as-> m $
    (reduce (fn [m c]
              (if (= 2 (count c))
                (apply assoc-in m c)
                (let [[key-path patch _] c]
                  (update-in m key-path vec-patch patch)))) $ (:c patch))
    (reduce (fn [m a] (apply assoc-in m a)) $ (:a patch))
    (reduce (fn [m r]
              (if (= 1 (count r))
                (dissoc m (first r))
                (cond-> m
                  (not= ::not-found (get-in m (butlast r) ::not-found))
                  (update-in (butlast r) dissoc (last r))))) $ (:r patch))))

(declare lcs-matrix lcs-diff)

(defn vec-diff
  [v w]
  [:v (->> (lcs-diff (lcs-matrix v w) v w)
           (partition-by first)
           (map (fn [changes]
                  (case (ffirst changes)
                    :k [:k (count changes)]
                    :r [:r (count changes)]
                    :a [:a (count changes) (map second changes)]))))])

(defn vec-patch
  [v [patch-type patch]]
  {:pre [(= :v patch-type)]}
  (vec
   (:output
    (reduce (fn [{:keys [input output] :as v} [op count values]]
              (case op
                :k (-> v (update :output concat (take count input))
                       (update :input (partial drop count)))
                :r (-> v (update :input (partial drop count)))
                :a (-> v (update :output concat values))))
            {:input v :output []} patch))))

;;; Private

(defn- key-paths
  ([m] (key-paths m []))
  ([m prefix]
   (mapcat (fn [[k v]]
             (let [kp (concat prefix [k])]
               (if (and (map? v) (not-empty v))
                 (cons kp (key-paths v kp))
                 [kp]))) m)))

(defn lcs-matrix
  [s t & {:keys [compare] :or {compare =}}]
  (let [rows (inc (count s))
        cols (inc (count t))
        a (make-array Long rows cols)]
    (doseq [r (range rows)] (aset a r 0 0))
    (doseq [c (range cols)] (aset a 0 c 0))
    (doseq [i (range 1 rows)
            j (range 1 cols)]
      (if (compare (nth s (dec i)) (nth t (dec j)))
        (aset a i j (inc (aget a (dec i) (dec j))))
        (aset a i j (max (aget a i (dec j))
                         (aget a (dec i) j)))))
    a))

(defn lcs-diff
  [lcs-matrix s t & {:keys [compare] :or {compare =}}]
  (let [m lcs-matrix
        f (fn lcs-diff-rec [i j]
            (cond (and (< 0 i) (< 0 j) (compare (nth s (dec i)) (nth t (dec j))))
                  (conj (lcs-diff-rec (dec i) (dec j)) [:k (nth s (dec i))])
                  (and (< 0 j) (or (= i 0) (<= (aget m (dec i) j) (aget m i (dec j)))))
                  (conj (lcs-diff-rec i (dec j)) [:a (nth t (dec j))])
                  (and (< 0 i) (or (= j 0) (>  (aget m (dec i) j) (aget m i (dec j)))))
                  (conj (lcs-diff-rec (dec i) j) [:r (nth s (dec i))])
                  :else []))]
    (f (count s) (count t))))
