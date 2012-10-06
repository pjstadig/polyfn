;;;; Copyright (c) Paul Stadig. All rights reserved.
;;;;
;;;; The use and distribution terms for this software are covered by the Eclipse
;;;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;;;; can be found in the file epl-v10.html at the root of this distribution. By
;;;; using this software in any fashion, you are agreeing to be bound by the
;;;; terms of this license.  You must not remove this notice, or any other, from
;;;; this software.
(ns name.stadig.polyfn
  (:require [name.stadig.polyfn.lookup :refer [find-polyfn-impl]])
  (:import (clojure.lang IPersistentCollection)))

(defmacro defpolyfn
  ([name]
     `(do (defonce ~name
            (with-meta
              (fn [this# & args#]
                (let [cache# (::cache (meta ~name))
                      dispatch# @(::dispatch (meta ~name))
                      c# (class this#)]
                  (if-let [f# (get @cache# c#)]
                    (apply f# this# args#)
                    (if-let [f# (find-polyfn-impl dispatch# c#)]
                      (do (swap! cache# assoc c# f#)
                          (apply f# this# args#))
                      (throw (Exception. (str '~name " is not defined for "
                                              (pr-str c#))))))))
              {::cache (atom {})
               ::dispatch (atom {})}))
          (var ~name)))
  ([name type params & body]
     `(do (defpolyfn ~name)
          (reset! (::cache (meta ~name)) {})
          (swap! (::dispatch (meta ~name))
                 assoc ~type (fn ~(vec params) ~@body))
          (var ~name))))
