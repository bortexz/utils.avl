(ns bortexz.utils.avl
  "Utilities to work with data.avl sorted collections"
  (:refer-clojure :exclude [first last nth])
  (:require [clojure.data.avl :as avl]
            [better-cond.core :as bc]
            [clojure.core.protocols :as p]))

(defn- maybe-f
  [f]
  (fn [x] (some-> x f)))

(def ^:private some-key (maybe-f key))
(def ^:private some-val (maybe-f val))

(defprotocol -EntryKey
  "Protocol to make some fns work both on sorted maps and sorted sets.
   For sorted maps, will return the key of the entry.
   For sorted sets, the entry is the key."
  (-entry-key [this entry]))

(extend-protocol -EntryKey
  clojure.data.avl.AVLMap
  (-entry-key [_ entry] (when entry (key entry)))
  clojure.data.avl.AVLSet
  (-entry-key [_ entry] entry))

(defn index-by
  "Given an (optional) comparator `cmp`, function `f` and `coll`, creates a new sorted-map whose keys
   are obtained by calling `f` on each `coll` element and vals are the element untouched."
  ([f coll] (into (avl/sorted-map) (map (juxt f identity)) coll))
  ([cmp f coll] (into (avl/sorted-map-by cmp) (map (juxt f identity)) coll)))

(defn nth
  "Like clojure.core/nth, except it always returns nil if index ouf of bounds."
  [sc idx]
  (clojure.core/nth sc idx nil))

(defn nth-key
  "Returns key at `idx` for sorted map `sm` if it exists, otherwise nil.
   Same as (some-> (nth sc idx nil) key)"
  [sm idx]
  (some-key (nth sm idx)))

(defn nth-val
  "Returns val at `idx` for sorted map `sm` if it exists, otherwise nil.
   Same as (some-> (nth sc idx nil) val)"
  [sc idx]
  (some-val (nth sc idx)))

(defn first
  "Returns the first element of sorted collection `sc` if not empty, else nil."
  [sc]
  (nth sc 0))

(defn first-key
  "Returns the first key of sorted map `sm`, or nil if empty.
   Same as (some-> (first sm) key)"
  [sm]
  (some-key (first sm)))

(defn first-val
  "Returns the first val of sorted map `sm`, or nil if empty.
   Same as (some-> (first sm) val)"
  [sm]
  (some-val (first sm)))

(defn last
  "Returns the last element of sorted collection `sc`, or nil if sc is empty."
  [sc]
  (when (not (zero? (count sc)))
    (nth sc (dec (count sc)))))

(defn last-key
  "Returns last key of sorted map `sm`, or nil if `sm` is empty.
   Same as (some-> (last sm) key)"
  [sc]
  (some-key (last sc)))

(defn last-val
  "Returns last key of sorted map `sm`, or nil if `sm` is empty.
   Same as (some-> (last sm) val)"
  [sc]
  (some-val (last sc)))

(defn rank-of
  "like avl/rank-of, but returns nil instead of -1 when item not found"
  [sc k]
  (bc/when-let [r (avl/rank-of sc k)
                _exists? (>= r 0)]
    r))

(defn nearest-key
  "Same as (some-> (avl/nearest sm test k) key)."
  [sm test k]
  (some-key (avl/nearest sm test k)))

(defn nearest-val
  "Same as (some-> (avl/nearest sm test k) val)."
  [sm test k]
  (some-val (avl/nearest sm test k)))

(defn nearest-rank
  "Returns the rank of the nearest key.
   Same as (rank-of sc (nearest-key sc test k))"
  [sc test k]
  (bc/when-let [near-key (-entry-key sc (avl/nearest sc test k))]
    (avl/rank-of sc near-key)))

(defn offset
  "Takes the item that is `n` items away from `k`. If `k` or the resulting item do not exist, returns nil.
   e.g. (offset sc k -1) takes the previous item to key `k`
   e.g. (offset sc k 1) takes the next item to key `k`."
  [sc k n]
  (bc/when-let [k-rank (rank-of sc k)]
    (nth sc (+ k-rank n))))

(defn offset-key
  "Same as (some-> (offset sc k n) key)"
  [sm k n]
  (some-key (offset sm k n)))

(defn offset-val
  "Same as (some-> (offset sc k n) val)"
  [sm k n]
  (some-val (offset sm k n)))

(defn -tail-vec-fn
  "Returns a [[tail]] function where items are mapped using `f` when building the resulting vector."
  [f]
  (fn tail-vec
    ([sc n]
     (bc/when-let [lk (-entry-key sc (last sc))]
       (tail-vec sc n lk)))
    ([sc n end-k]
     (bc/when-let [end (rank-of sc end-k)
                   start (max 0 (- end (dec n)))]
       (persistent!
        (reduce (fn [v k] (conj! v (f (nth sc k))))
                (transient [])
                (range start (inc end))))))))

(def tail
  ^{:doc
    "Returns a vector of the last up to `n` items of `sc` until `ending-k`.
     If `ending-k` is not provided, [[last-key]] will be used.
     `ending-k` must exist if it's specified, otherwise returns nil."
    :arglists '([sc n] [sc n end-k])}
  (-tail-vec-fn identity))

(def tail-keys
  ^{:doc "faster (mapv key (tail ...))"
    :arglists '([sc n] [sc n end-k])}
  (-tail-vec-fn key))

(def tail-vals
  ^{:doc "faster (mapv val (tail ...))"
    :arglists '([sc n] [sc n end-k])}
  (-tail-vec-fn val))

(defn tail-sorted
  "Like [[tail]] but returns an avl sorted collection.
   Usually faster than (into (sorted-map) (tail sc n)),
   except when the resulting collection is very small ~(< 10 items)"
  ([sc n]
   (when (not (zero? (count sc)))
     (tail-sorted sc n (last-key sc))))
  ([sc n end-k]
   (bc/when-let [ending-rank (rank-of sc end-k)
                 starting-rank (max (- ending-rank (dec n)) 0)
                 starting-k (-entry-key sc (nth sc starting-rank))]
     (avl/subrange sc >= starting-k <= end-k))))

(defn -full-tail-vec-fn
  "Returns a [[full-tail]] function where items are mapped using `f` when building the resulting vector."
  [f]
  (fn full-tail-vec
    ([sc n]
     (bc/when-let [lk (last-key sc)]
       (full-tail-vec sc n lk)))
    ([sc n end-k]
     (bc/when-let [end   (avl/rank-of sc end-k)
                   start (- end (dec n))
                   _     (nat-int? start)]
       (some-> (reduce (fn [xs k]
                         (bc/if-let [item (nth sc k)
                                     _?   (some? (val item))]
                           (conj! xs (f item))
                           (reduced nil)))
                       (transient [])
                       (range start (inc end)))
               (persistent!))))))

(def full-tail
  ^{:doc
    "Like [[tail]] but only returns the tail vector when it has `n` non-nil value items, nil otherwise.
     Only works for sorted-maps (it checks existance of values).
     Sometimes when you're building incremental series you might not want to do anything until you have
     a certain amount of non-nil values of your source. By calling full-tail, you avoid having to check 
     `(every? some? tail)`, and can do something like this `(when-let [tail (full-tail sm n)] ... )` and
     be sure you have n non-nil values on the tail."
    :arglists '([sm n] [sm n end-k])}
  (-full-tail-vec-fn identity))

(def full-tail-keys
  ^{:doc "Faster (some->> (full-tail sc n) (mapv key)). See [[full-tail]]"
    :arglists '([sm n] [sm n end-k])}
  (-full-tail-vec-fn key))

(def full-tail-vals
  ^{:doc "Faster (some->> (full-tail sc n) (mapv val)). See [[full-tail]]"
    :arglists '([sm n] [sm n end-k])}
  (-full-tail-vec-fn val))

(defn full-tail-sorted
  "Faster (some->> (full-tail sc n) (into (avl/sorted-map))). See [[full-tail]]"
  ([sm n]
   (bc/when-let [lk (last-key sm)]
     (full-tail-sorted sm n lk)))
  ([sm n end-k]
   (bc/when-let [end   (avl/rank-of sm end-k)
                 start (- end (dec n))
                 _     (nat-int? start)]
     (some-> (reduce (fn [xs k]
                       (bc/if-let [item (nth sm k)
                                   _?   (some? (val item))]
                         (conj! xs item)
                         (reduced nil)))
                     (transient (avl/sorted-map-by (.comparator sm)))
                     (range start (inc end)))
             (persistent!)))))

(defn datafy-avl-maps!
  "Makes clojure.data.avl.AVLMap implement p/Datafiable. datafy on an AVLMap will return a clojure.core
   sorted-map."
  []
  (extend-type clojure.data.avl.AVLMap
    p/Datafiable
    (datafy [this] (into (clojure.core/sorted-map-by (.comparator this)) this))))

(defn datafy-avl-sets!
  "Makes clojure.data.avl.AVLSet implement p/Datafiable. datafy on an AVLMap will return a clojure.core
   sorted-set"
  []
  (extend-type clojure.data.avl.AVLSet
    p/Datafiable
    (datafy [this] (into (clojure.core/sorted-map-by (.comparator this)) this))))

(defn datafy-avl!
  "Calls both [[datafy-avl-maps!]] and [[datafy-avl-sets!]]"
  []
  (datafy-avl-maps!)
  (datafy-avl-sets!))

(defn find-val
  "Iterates over sorted-map `sm` testing vals against previous found val using 2-arity `testf`, fn accepting new
   val and previously found val. if `testf` returns logical true, then the new tested val will be the found val, used to
   test against following vals. Iteration starts on the second val, and the first val is used as the starting found val.
   
   Can optionally specify a map of options including:
  - `start-rank` start rank of the iteration (included). Defaults to 0.
  - `end-rank` last rank of the iteration (included). Defaults to latest-rank `(dec (count sm))`
   
   Returns a tuple [found-rank found-val].
   
   E.g. find the maximum value and its rank of a sorted-map bounded by 2 ranks, returning first value found for the max
   value:
   `(test-vals sm > {:start-rank 1 :end-rank 10})` (if >= then latest max-value is returned)
   "
  ([sm testf] (find-val sm testf {}))
  ([sm testf {:keys [start-rank end-rank] :or {start-rank 0 end-rank (dec (count sm))}}]
   (bc/when-let [init-val  (nth-val sm start-rank)]
     (let [curr-v (volatile! init-val)
           curr-r (volatile! start-rank)]
       (run! (fn [r]
               (let [v (nth-val sm r)]
                 (when (testf v @curr-v)
                   (vreset! curr-v v)
                   (vreset! curr-r r))))
             (range (inc start-rank) (inc end-rank)))
       [@curr-r @curr-v]))))
