(ns erp12.schema-inference.impl.ground)

(def canonical-ground
  ;; @todo Expand this map to include more schemas.
  {:boolean 'boolean?
   :int     'int?
   :float   'float?
   :double  'double?
   :string  'string?
   :char    'char?
   :keyword 'keyword?
   :symbol  'symbol?})

(defn cls->schema
  [cls]
  {:type (case (.getName cls)
           "boolean" 'boolean?
           "byte" 'int?
           "short" 'int?
           "int" 'int?
           "long" 'int?
           "double" 'double?
           "float" 'float?
           "char" 'char?
           "[B" 'bytes?
           "java.lang.String" 'string?
           "clojure.lang.Keyword" 'keyword?
           "clojure.lang.Symbol" 'symbol?
           "java.util.UUID" 'uuid?
           ;; @todo 'inst?
           cls)})
