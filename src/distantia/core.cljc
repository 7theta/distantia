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
  (try
    (cond
      (and (map? a) (map? b)) (map-diff a b)
      (and (vector? a) (vector? b)) (vec-diff a b)
      :else [:s b])
    (catch #?(:clj Exception :cljs js/Error) e
      (throw e))))

(defn patch
  [a [type p :as patch]]
  (try
    (cond
      (and (map? a) (= :m type)) (map-patch a patch)
      (and (vector? a) (= :v type)) (vec-patch a patch)
      (= :s type) p)
    (catch #?(:clj Exception :cljs js/Error) e
      (throw e))))


;;; Private

(declare key-paths)

(defn- map-diff
  [x y]
  (let [prefix? (fn [pre s] (= (take (count pre) s) pre))]
    [:m (loop [a (transient []) r (transient []) c (transient [])
               paths (distinct (concat (key-paths x) (key-paths y)))]
          (if-let [kp (first paths)]
            (let [xv (get-in x kp ::not-found)
                  yv (get-in y kp ::not-found)
                  xv? (not= ::not-found xv)
                  yv? (not= ::not-found yv)]
              (cond
                (and xv? yv? (= xv yv))
                (recur a r c (next paths))

                (and xv? yv? (vector? xv) (vector? yv))
                (recur a r (conj! c [kp (vec-diff xv yv) :s])
                       (remove (partial prefix? kp) (next paths)))

                (and xv? yv?)
                (recur a r (conj! c [kp yv])
                       (remove (partial prefix? kp) (next paths)))

                xv?
                (recur a (conj! r kp) c
                       (remove (partial prefix? kp) (next paths)))

                yv? (recur (conj! a [kp yv]) r c
                           (remove (partial prefix? kp) (next paths)))))
            (cond-> {}
              (pos? (count a)) (assoc :a (persistent! a))
              (pos? (count r)) (assoc :r (persistent! r))
              (pos? (count c)) (assoc :c (persistent! c)))))]))

(defn- map-patch
  [m [patch-type patch]]
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

(defn- vec-diff
  [v w]
  [:v (->> (lcs-diff (lcs-matrix v w) v w)
           (partition-by first)
           (map (fn [changes]
                  (case (ffirst changes)
                    :k [:k (count changes)]
                    :r [:r (count changes)]
                    :a [:a (count changes) (map second changes)]))))])

(defn- vec-patch
  [v [patch-type patch]]
  (vec
   (:output
    (reduce (fn [{:keys [input output] :as v} [op count values]]
              (case op
                :k (-> v (update :output concat (take count input))
                       (update :input (partial drop count)))
                :r (-> v (update :input (partial drop count)))
                :a (-> v (update :output concat values))))
            {:input v :output []} patch))))

(defn- key-paths
  ([m] (key-paths m []))
  ([m prefix]
   (mapcat (fn [[k v]]
             (let [kp (concat prefix [k])]
               (if (and (map? v) (not-empty v))
                 (cons kp (key-paths v kp))
                 [kp]))) m)))

(defn- lcs-matrix
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

(defn- lcs-diff
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
