(ns ^{:doc "A parser for SQL: generate Application Description Language."
      :author "Simon Brooke"}
  squirrel-parse.to-adl
  (:require [clojure.xml :refer [emit-element]]
            [clj-time.core :refer [now]]
            [clj-time.format :refer [formatters unparse]]
            [squirrel-parse.parser :refer [parse]]
            [squirrel-parse.simplify :refer [simplify]]
            [squirrel-parse.utils :refer [deep-merge]]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; squirrel-parse.to-adl: generate Application Description Language.
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
;;;; USA.
;;;;
;;;; Copyright (C) 2018 Simon Brooke
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def xml-header
  "XML header for ADL files."
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <!DOCTYPE application PUBLIC \"-//JOURNEYMAN//DTD ADL 1.4//EN\"
    \"http://www.journeyman.cc/adl/stable/adl/schemas/adl-1.4.dtd\">")

(def sql-datatype-to-adl-datatype
  "Map to convert SQL datatypes to the nearest ADL equivalent."
  {:DT-BIGINT            :integer
   :DT-BIGSERIAL         :integer
   :DT-BIT               :integer
   :DT-BOOLEAN           :boolean
   :DT-BYTEA             :unsupported
   :DT-DATE              :date
   :DT-DOUBLE-PRECISION  :real
   :DT-FLOAT             :real
   :DT-INTEGER           :integer
   :DT-MONEY             :money
   :DT-NUMERIC           :real
   :DT-REAL              :real
   :DT-SERIAL            :integer
   :DT-TEXT              :text
   :DT-CHAR              :string
   :DT-CHARACTER         :string
   :DT-CHARACTER-VARYING :string
   :DT-VARCHAR           :string
   :DT-TIME              :string
   :DT-TIMESTAMP         :timestamp
   :DT-INTERVAL          :unsupported
   })

(defn is-subtree-of-type?
  "Is this `subtree` a parser subtree of the specified `type`, expected to be a keyword?"
  [subtree type]
  (and (coll? subtree) (= (first subtree) type)))


(defn subtree?
  "Does this `subtree` appear to be a subtree of a parse tree?"
  [subtree]
  (and (seq? subtree) (keyword? (first subtree))))


(defn subtree-to-map
  "Converts `subtree` to a map. **Note** that this will return unexpected
  results if the subtree contains repeating entries of the same type
  (i.e. having the same initial keyword), as only the last of such
  a sequence will be retained. Use with care."
  [subtree]
  (if
    (subtree? subtree)
    (if
      (and
        (> (count subtree) 1)
        (reduce #(and %1 %2) (map seq? (rest subtree))))
      {(first subtree) (reduce merge {} (map subtree-to-map (rest subtree)))}
      {(first subtree) (first (rest subtree))})
    subtree))


(defn is-create-table-statement?
  "Is this statement a create table statement?"
  [statement]
  (is-subtree-of-type? statement :CREATE-TABLE-STMT))

(defn get-children-of-type [subtree type]
  (if
    (coll? subtree)
      (remove
        nil?
        (map
          #(if
             (and (coll? %) (= (first %) type))
             %)
          subtree))))


(defn get-first-child-of-type [subtree type]
  (first (get-children-of-type subtree type)))


(defn get-name
  "Return the value the first top-level :NAME element of this `subtree`."
  [subtree]
  (let [name-elt (get-first-child-of-type subtree :NAME)]
    (if name-elt (second name-elt))))


(defn get-column-datatype
  "Get the datatype of this column specification."
  [column-spec]
  (let [datatype-spec (get-first-child-of-type column-spec :DATATYPE)
        sql-datatype (first (second datatype-spec))]
    (sql-datatype-to-adl-datatype sql-datatype)))


(defn make-property
  "Make an ADL property representing this column specification."
  [column-spec]
  (if
    (is-subtree-of-type? (second column-spec) :COLUMN-SPEC)
    (make-property (second column-spec))
    (let [name (get-name column-spec)]
      {(keyword name)
       {:tag :property
        :attrs
        {:name (get-name column-spec)
         :type (get-column-datatype column-spec)}}})))


(defn make-entity-map [table-decl]
  "Make an ADL entity representing this table declaration"
  {:tag :entity
   :attrs
   {:name (get-name table-decl)}
   :content
   {:properties
     (apply
       merge
       (map
         make-property
         (remove
           nil?
           (map
             #(if
                (is-subtree-of-type? % :COLUMN-SPEC)
                %)
             (get-first-child-of-type table-decl
                                      :TABLE-SPEC-ELEMENTS)))))}})


(defn table-definition-to-map
  "Return a map like this `map` with, if this `statement` is a table declaration,
  a map reprentation of an ADL entity representing that table added to it."
  [entities-map statement]
  (if
    (is-create-table-statement? statement)
    (let [table-name (get-name statement)]
      (merge entities-map {table-name (make-entity-map statement)}))
    entities-map))


(defn is-column-constraint-statement-of-type?
  [statement key]
  (and
    (is-subtree-of-type? statement :ALTER-TABLE)
    (let [sm (subtree-to-map statement)]
      (or
        (key
          (:COLUMN-CONSTRAINT
            (:ADD-CONSTRAINT
              (:ALTER-TABLE-ELEMENTS
                (:ALTER-TABLE sm)))))
        (key
          (:COLUMN-CONSTRAINT
            (:COLUMN-CONSTRAINT
              (:ADD-CONSTRAINT
                (:ALTER-TABLE-ELEMENTS
                  (:ALTER-TABLE sm))))))
        ))))


(defn is-foreign-key-statement?
  "Returns non-nil (actually the relevant fragment) if `statement` is an
  'alter table... add foreign key' statement"
  [statement]
  (is-column-constraint-statement-of-type? statement :REFERENCES-CC))


(defn is-primary-key-statement?
  "Returns non-nil (actually the relevant fragment) if `statement` is an
  'alter table... add primary key' statement"
  [statement]
  (is-column-constraint-statement-of-type? statement :PRIMARY-CC))


(defn decorate-with-relationship
  "If this `statement` is a foreign key statement, return an entity-map like this `entity-map`
  but with the relevant property decorated with the appropriate foreign key details"
  [entity-map statement]
  (if
    (is-foreign-key-statement? statement)
    (let [sm (subtree-to-map (is-foreign-key-statement? statement))
          table (:name (:attrs entity-map))
          ns-table (:NAME (:QUAL-NAME (:ALTER-TABLE (subtree-to-map statement))))
          ns-cols (:NAME (:NAMES sm))
          fs-table (:NAME (:REFERENCES-CC sm))
          fs-cols (:NAME (:NAMES (:REFERENCES-CC sm)))]
      (if
        (= table ns-table)
        (do
          (println "...Firing!")
        (deep-merge
          entity-map
          {:content
           {:properties
             {(keyword ns-cols)
              {:attrs
               {:type "entity" :entity fs-table :farkey fs-cols}}}}}))
        ;; else this statement doesn't refer to us...
        ))))


(defn decorate-with-primary-key
  "If this `statement` is a primary key statement, return an entity-map like this `entity-map`
  but with the relevant property removed from the 'content' sub-element and added to the 'key'
  sub-element."
  [entity-map statement]
  (if
    (is-primary-key-statement? statement)
    (let [sm (subtree-to-map (is-primary-key-statement? statement))
          table (:name (:attrs entity-map))
          st-table (:NAME (:QUAL-NAME (:ALTER-TABLE (subtree-to-map statement))))
          col (keyword (:NAME (:NAMES (:INDEX-PARAMS sm))))
          properties (:properties (:content entity-map))]
      (if
        (= table st-table)
        (deep-merge
          entity-map
          {:content
           {:properties
            {col {:tag :key}}}})))))


(defn decorate-with-all
  [entity-map statements function]
  (reduce
    deep-merge
    entity-map
    (remove
      nil?
      (map #(apply function (list entity-map %)) statements))))


(defn table-definitions-to-entities
  "Extract table definitions from these `statements` as a map of ADL
  entities indexed by name."
  [statements]
  (let
    [base-map (reduce table-definition-to-map {} statements)]
    (apply
      merge
      (map
        (fn [x]
          (let [entity-map (base-map x)]
          {x
           (decorate-with-all
             (decorate-with-all entity-map statements #(decorate-with-relationship %1 %2))
             statements
             #(decorate-with-primary-key %1 %2))}))
        (keys base-map)))))


(defn to-adl-xml
  "Turn `object`, a fragment of the mappy sort of structure created
  by `table-definitions-to-entities`, into serialisable XML"
  [object]
  (cond
    (keyword? object)
    object
    (seq? object)
    (vec (map to-adl-xml object))
    (map? object)
    (case
      (:tag object)
      :entity
      (merge
        object
        {:content
         (map
           to-adl-xml
           (vals (:properties (:content object))))})
      (apply assoc (cons {} (interleave (keys object) (map to-adl-xml (vals object))))))
    true
    object))


(defn to-adl
  "Take this `input` (filename, url, whatever) assumed to contain a stream of SQL
  statements; convert them to ADL with this `application-name`; if `version` is
  provided, tag it with that version number else tag it with a version number drawn
  from the current date; if `output` is provided write it as XML to that output."
  ([input application-name]
   (to-adl input application-name (unparse (formatters :basic-date) (now))))
  ([input application-name version]
   (let [entities (table-definitions-to-entities (simplify (parse (slurp input))))]
     {:tag :application
      :attrs {:name application-name
              :version version }
      :content (to-adl-xml (vals entities))}))
  ([input application-name version output]
   (let [adl (to-adl input application-name version)]
     (spit output (str xml-header "\n" (with-out-str (emit-element adl))))
     adl)))
