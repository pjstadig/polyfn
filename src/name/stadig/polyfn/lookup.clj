;;;; Copyright (c) Rich Hickey and Paul Stadig. All rights reserved.
;;;;
;;;; The use and distribution terms for this software are covered by the Eclipse
;;;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;;;; can be found in the file epl-v10.html at the root of this distribution. By
;;;; using this software in any fashion, you are agreeing to be bound by the
;;;; terms of this license.  You must not remove this notice, or any other, from
;;;; this software.
(ns name.stadig.polyfn.lookup)

(defn- super-chain [^Class c]
  (when c
    (cons c (super-chain (.getSuperclass c)))))

(defn find-polyfn-impl [dispatch c]
  (let [impl #(get dispatch %)]
    (or (impl c)
        (and c
             (or (first (remove nil? (map impl (butlast (super-chain c)))))
                 (when-let [t (reduce @#'clojure.core/pref
                                      (filter impl
                                              (disj (supers c) Object)))]
                   (impl t))
                 (impl Object))))))
