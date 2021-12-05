(ns build
  (:require [clojure.tools.build.api :as b]
             [org.corfield.build :as bb]))

(defn tests
  [opts]
  (bb/run-tests opts))
