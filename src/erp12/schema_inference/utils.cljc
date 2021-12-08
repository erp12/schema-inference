(ns erp12.schema-inference.utils)

(defn first-non-nil
  "Returns the first non-nil values from the collection, or returns `nil` if
  the collection is empty or only contains `nil`."
  [coll]
  (first (filter some? coll)))
