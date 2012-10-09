;;;; Copyright (c) Rich Hickey and Paul Stadig. All rights reserved.
;;;;
;;;; The use and distribution terms for this software are covered by the Eclipse
;;;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;;;; can be found in the file epl-v10.html at the root of this distribution. By
;;;; using this software in any fashion, you are agreeing to be bound by the
;;;; terms of this license.  You must not remove this notice, or any other, from
;;;; this software.
(ns name.stadig.polyfn
  (:import (clojure.lang AFunction MethodImplCache)))

(defn- shift-mask [shift mask x]
  (-> x (bit-shift-right shift) (bit-and mask)))

(def ^:private max-mask-bits 13)
(def ^:private max-switch-table-size (bit-shift-left 1 max-mask-bits))

(defn- maybe-min-hash
  "takes a collection of hashes and returns [shift mask] or nil if none found"
  [hashes]
  (first
   (filter (fn [[s m]]
             (apply distinct? (map #(shift-mask s m %) hashes)))
           (for [mask (map #(dec (bit-shift-left 1 %))
                           (range 1 (inc max-mask-bits)))
                 shift (range 0 31)]
             [shift mask]))))

(defn- expand-method-impl-cache [^clojure.lang.MethodImplCache cache c f]
  (if (.map cache)
    (let [cs (assoc (.map cache) c (clojure.lang.MethodImplCache$Entry. c f))]
      (MethodImplCache. (.protocol cache) (.methodk cache) cs))
    (let [cs (into {} (remove (fn [[c e]] (nil? e))
                              (map vec (partition 2 (.table cache)))))
          cs (assoc cs c (clojure.lang.MethodImplCache$Entry. c f))]
      (if-let [[shift mask] (maybe-min-hash (map hash (keys cs)))]
        (let [table (make-array Object (* 2 (inc mask)))
              table (reduce (fn [^objects t [c e]]
                              (let [i (* 2 (int (shift-mask shift mask
                                                            (hash c))))]
                                (aset t i c)
                                (aset t (inc i) e)
                                t))
                            table cs)]
          (MethodImplCache. (.protocol cache) (.methodk cache)
                            shift mask table))
        (MethodImplCache. (.protocol cache) (.methodk cache)
                          cs)))))

(defn- super-chain [^Class c]
  (when c
    (cons c (super-chain (.getSuperclass c)))))

(defn- pref
  ([] nil)
  ([a] a)
  ([^Class a ^Class b]
     (if (.isAssignableFrom a b) b a)))

(defn find-polyfn-impl [dispatch c]
  (let [impl #(get dispatch %)]
    (or (impl c)
        (and c
             (or (first (remove nil? (map impl (butlast (super-chain c)))))
                 (when-let [t (reduce pref
                                      (filter impl
                                              (disj (supers c) Object)))]
                   (impl t))
                 (impl Object))))))

(defn -cache-polyfn-fn [^AFunction polyfn x]
  (let [cache (.__methodImplCache polyfn)
        c (class x)
        f (find-polyfn-impl @(::dispatch (meta polyfn)) c)]
    (when-not f
      (throw (IllegalArgumentException.
              (str "No implementation of polyfn: " (::var (meta polyfn))
                   " found for class: " (if (nil? x) "nil" (.getName c))))))
    (set! (.__methodImplCache polyfn)
          (expand-method-impl-cache cache c f))
    f))

(defn reset-cache [^AFunction polyfn]
  (set! (.__methodImplCache polyfn)
        (loop [cache (MethodImplCache. nil nil)
               [[c f] & dispatch] (seq @(::dispatch (meta polyfn)))]
          (if c
            (recur (expand-method-impl-cache cache c f) dispatch)
            cache))))

(defmacro defpolyfn
  ([name params]
     (let [[target & args] params]
       `(defonce ~name
          ^{::dispatch (atom {})
            ::var (var ~name)}
          (fn polyfn# ~params
            (let [cache# (.__methodImplCache ^AFunction polyfn#)
                  f# (.fnFor cache# (clojure.lang.Util/classOf ~target))]
              (if f#
                (f# ~@params)
                ((-cache-polyfn-fn ~name ~target) ~@params))))))))

(defn add-impl [polyfn type f]
  (swap! (::dispatch (meta polyfn)) assoc type f)
  (reset-cache polyfn)
  polyfn)

(defn remove-impl [polyfn type]
  (swap! (::dispatch (meta polyfn)) dissoc type)
  (reset-cache polyfn)
  polyfn)

(defn reset-polyfn [polyfn]
  (reset! (::dispatch (meta polyfn)) {})
  (reset-cache polyfn)
  polyfn)

(defmacro extend-polyfn [name type params & body]
  (let [[target & args] params]
    `(do (add-impl ~name ~type (fn [~(with-meta target {:tag type})
                                    ~@args]
                                 ~@body))
         (reset-cache ~name)
         (var ~name))))
