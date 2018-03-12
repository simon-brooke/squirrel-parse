(ns ^{:doc "A parser for SQL: generate HUGSQL queries file."
      :author "Simon Brooke"}
  squirrel-parse.to-hugsql-queries
  (:require [clojure.java.io :refer [file]]
            [clojure.string :as s]
            [squirrel-parse.to-adl :refer [migrations-to-xml]]))


(defn key-names [entity-map]
  (remove
    nil?
    (map
      #(:name (:attrs %))
      (vals (:content (:key (:content entity-map)))))))


(defn has-primary-key? [entity-map]
  (> (count (key-names entity-map)) 0))


(defn has-non-key-properties? [entity-map]
  (>
    (count (vals (:properties (:content entity-map))))
    (count (key-names entity-map))))


(defn where-clause [entity-map]
  (str
    "WHERE "
    (s/join " AND\n\t" (map #(str % " = " (keyword %)) (key-names entity-map)))))


(defn insert-query [entity-map]
  (let [entity-name (:name (:attrs entity-map))
        pretty-name (s/replace (s/replace entity-name #"_" "-") #"s$" "")
        all-property-names (map #(:name (:attrs %)) (vals (:properties (:content entity-map))))
        ]
    (str "-- :name create-" pretty-name "! :! :n\n"
         "-- :doc creates a new " pretty-name " record\n"
         "INSERT INTO " entity-name "\n("
         (s/join ", " all-property-names)
         ")\nVALUES ("
         (s/join ", " (map keyword all-property-names))
         ")\n\n")))


(defn update-query [entity-map]
  (if
    (and
      (has-primary-key? entity-map)
      (has-non-key-properties? entity-map))
    (let [entity-name (:name (:attrs entity-map))
          pretty-name (s/replace (s/replace entity-name #"_" "-") #"s$" "")
          property-names (remove
                           nil?
                           (map
                             #(if (= (:tag %) :property) (:name (:attrs %)))
                             (vals (:properties (:content entity-map)))))]
      (str "-- :name update-" pretty-name "! :! :n\n"
           "-- :doc updates an existing " pretty-name " record\n"
           "UPDATE " entity-name "\n"
           "SET "
           (s/join ",\n\t" (map #(str % " = " (keyword %)) property-names))
           "\n"
           (where-clause entity-map)
           "\n\n"))))


(defn select-query [entity-map]
  (if
    (has-primary-key? entity-map)
    (let [entity-name (:name (:attrs entity-map))
          pretty-name (s/replace (s/replace entity-name #"_" "-") #"s$" "")]
      (str "-- :name get-" pretty-name "! :! :n\n"
           "-- :doc updates an existing " pretty-name " record\n"
           "SELECT * FROM " entity-name "\n"
           (where-clause entity-map)
           "\n\n"))))


(defn delete-query [entity-map]
  (if
    (has-primary-key? entity-map)
    (let [entity-name (:name (:attrs entity-map))
          pretty-name (s/replace (s/replace entity-name #"_" "-") #"s$" "")]
      (str "-- :name delete-" pretty-name "! :! :n\n"
           "-- :doc updates an existing " pretty-name " record\n"
           "DELETE FROM " entity-name "\n"
           (where-clause entity-map)
           "\n\n"))))


(defn queries
  [entity-map]
  (str
    (insert-query entity-map)
    (update-query entity-map)
    (select-query entity-map)
    (delete-query entity-map)))


(defn migrations-to-queries-sql
  ([migrations-path]
   (migrations-to-queries-sql migrations-path "queries.sql"))
  ([migrations-path output]
   (let
     [adl-struct (migrations-to-xml migrations-path "Ignored")
      file-content (apply str (map queries (vals adl-struct)))]
     (spit output file-content)
     file-content)))
