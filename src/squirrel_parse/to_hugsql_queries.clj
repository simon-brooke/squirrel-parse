(ns ^{:doc "A parser for SQL: generate HUGSQL queries file."
      :author "Simon Brooke"}
  squirrel-parse.to-hugsql-queries
  (:require [clojure.java.io :refer [file]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as s]
            [squirrel-parse.to-adl :refer [migrations-to-xml]]
            [squirrel-parse.utils :refer [is-link-table? singularise]]))


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
  (let
    [entity-name (:name (:attrs entity-map))]
    (str
      "WHERE " entity-name "."
      (s/join
        (str " AND\n\t" entity-name ".")
        (map #(str % " = " (keyword %)) (key-names entity-map))))))


(defn order-by-clause [entity-map]
  (let
    [entity-name (:name (:attrs entity-map))
        preferred (map
                  #(:name (:attrs %))
                  (filter #(= (-> % :attrs :distinct) "user")
                          (-> entity-map :content :properties vals)))]
    (str
      "ORDER BY " entity-name "."
      (s/join
        (str ",\n\t" entity-name ".")
        (doall (flatten (cons preferred (key-names entity-map))))))))


(defn insert-query [entity-map]
  (let [entity-name (:name (:attrs entity-map))
        pretty-name (s/replace (s/replace entity-name #"_" "-") #"s$" "")
        all-property-names (map #(:name (:attrs %)) (vals (:properties (:content entity-map))))
        ]
    (str "-- :name create-" pretty-name "! :<!\n"
         "-- :doc creates a new " pretty-name " record\n"
         "INSERT INTO " entity-name " ("
         (s/join ",\n\t" all-property-names)
         ")\nVALUES ("
         (s/join ",\n\t" (map keyword all-property-names))
         ")"
         (if
           (has-primary-key? entity-map)
           (str "\nreturning " (s/join ",\n\t" (key-names entity-map))))
         "\n\n")))


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
      (str "-- :name get-" pretty-name " :? :1\n"
           "-- :doc selects an existing " pretty-name " record\n"
           "SELECT * FROM " entity-name "\n"
           (where-clause entity-map)
           "\n"
           (order-by-clause entity-map)
           "\n\n"))))


(defn list-query [entity-map]
  (let [entity-name (:name (:attrs entity-map))
        pretty-name (s/replace (s/replace entity-name #"_" "-") #"s$" "")]
    (str "-- :name list-" pretty-name " :? :*\n"
         "-- :doc lists all existing " pretty-name " records\n"
         "SELECT * FROM " entity-name "\n"
         (order-by-clause entity-map)
         "\n\n")))


(defn foreign-queries [entity-map entities-map]
  (let [entity-name (:name (:attrs entity-map))
        pretty-name (s/replace (s/replace entity-name #"_" "-") #"s$" "")
        links (filter #(-> % :attrs :entity) (-> entity-map :content :properties vals))]
    (apply
      str
      (map
        #(let [far-name (-> % :attrs :entity)
               far-entity ((keyword far-name) entities-map)
               pretty-far (s/replace (s/replace far-name #"_" "-") #"s$" "")
               farkey (-> % :attrs :farkey)
               link-field (-> % :attrs :name)]
           (str "-- :name list-" entity-name "-by-" pretty-far " :? :*\n"
                "-- :doc lists all existing " pretty-name " records related to a given " pretty-far "\n"
                "SELECT * \nFROM " entity-name "\n"
                "WHERE " entity-name "." link-field " = :id\n"
                (order-by-clause entity-map)
                "\n\n"))
        links))))


(defn link-table-query [near link far]
  (let [properties (-> link :content :properties vals)
        links (apply
                merge
                (map
                  #(hash-map (keyword (-> % :attrs :entity)) %)
                  (filter #(-> % :attrs :entity) properties)))
        near-name (-> near :attrs :name)
        link-name (-> link :attrs :name)
        far-name (-> far :attrs :name)
        pretty-far (singularise far-name)]
    (println links)
    (str "-- :name list-" link-name "-" near-name "-by-" pretty-far " :? :*\n"
         "-- :doc lists all existing " near-name " records related through " link-name " to a given " pretty-far "\n"
         "SELECT "near-name ".*\n"
         "FROM " near-name ", " link-name "\n"
         "WHERE " near-name "." (first (key-names near)) " = " link-name "." (-> (links (keyword near-name)) :attrs :name) "\n\t"
         "AND " link-name "." (-> (links (keyword far-name)) :attrs :name) " = :id\n"
         (order-by-clause near)
         "\n\n")))


(defn link-table-queries [entity-map entities-map]
  (let
    [entities (map
                #((keyword %) entities-map)
                (remove nil? (map #(-> % :attrs :entity) (-> entity-map :content :properties vals))))
     pairs (combinations entities 2)]
    (apply
      str
      (map
        #(str
           (link-table-query (nth % 0) entity-map (nth % 1))
           (link-table-query (nth % 1) entity-map (nth % 0)))
        pairs))))



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
  [entity-map entities-map]
  (str
    (insert-query entity-map)
    (update-query entity-map)
    (delete-query entity-map)
    (if
      (is-link-table? entity-map)
      (link-table-queries entity-map entities-map)
      (str
        (select-query entity-map)
        (list-query entity-map)
        (foreign-queries entity-map entities-map)))))


(defn migrations-to-queries-sql
  ([migrations-path]
   (migrations-to-queries-sql migrations-path "queries.auto.sql"))
  ([migrations-path output]
   (let
     [adl-struct (migrations-to-xml migrations-path "Ignored")
      file-content (apply str (map #(queries % adl-struct) (vals adl-struct)))]
     (spit output file-content)
     file-content)))
