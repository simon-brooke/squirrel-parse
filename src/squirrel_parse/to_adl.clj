(ns ^{:doc "A parser for SQL: generate Application Description Language."
      :author "Simon Brooke"}
  squirrel-parse.to-adl
  (:require [clojure.java.io :refer [file]]
            [clojure.string :as s]
            [clojure.xml :refer [emit-element]]
            [clj-time.core :refer [now]]
            [clj-time.format :refer [formatters unparse]]
            [squirrel-parse.parser :refer [parse]]
            [squirrel-parse.simplify :refer [simplify]]
            [squirrel-parse.utils :refer :all]
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
  {:DT-BIGINT            "integer"
   :DT-BIGSERIAL         "integer"
   :DT-BIT               "integer"
   :DT-BOOLEAN           "boolean"
   :DT-BYTEA             "unsupported"
   :DT-DATE              "date"
   :DT-DOUBLE-PRECISION  "real"
   :DT-FLOAT             "real"
   :DT-INT               "integer"
   :DT-INTEGER           "integer"
   :DT-MONEY             "money"
   :DT-NUMERIC           "real"
   :DT-REAL              "real"
   :DT-SERIAL            "integer"
   :DT-TEXT              "text"
   :DT-CHAR              "string"
   :DT-CHARACTER         "string"
   :DT-CHARACTER-VARYING "string"
   :DT-VARCHAR           "string"
   :DT-TIME              "string"
   :DT-TIMESTAMP         "timestamp"
   :DT-INTERVAL          "unsupported"
   })


(defn get-column-datatype
  "Get the datatype of this column specification."
  [column-spec]
  (let [datatype-spec (get-first-child-of-type column-spec :DATATYPE)
        sql-datatype (first (second datatype-spec))]
    (sql-datatype-to-adl-datatype sql-datatype)))


(defn make-property
  "Make an ADL property representing this column specification.
  TODO: many things, but does not cope with multi-column foreign keys.
  TODO: default value is not extracted correctly."
  [column-spec]
  (if
    (is-subtree-of-type? (second column-spec) :COLUMN-SPEC)
    (make-property (second column-spec))
    (let [name (get-name column-spec)
          size-spec (get-first-child-of-type column-spec :INT-VAL)
          size (if size-spec (nth size-spec 1))
          constraints (rest (get-first-child-of-type column-spec :COLUMN-CONSTRAINTS))
          required? (first (filter #(get-first-child-of-type % :NOT-NULL-CC) constraints))
          default? (first (filter #(get-first-child-of-type % :DEFAULT-CC) constraints))
          foreign? (first (filter #(get-first-child-of-type % :REFERENCES-CC) constraints))
          dflt-val (if (and default? (> (count default?) 2)) (nth (nth default? 1) 2))]
      {(keyword name)
       {:tag :property
        :attrs
        (merge
          (if size {:size size} {})
          (if required? {:required "true"} {})
          (if default? {:default dflt-val})
          (if
            foreign?
            (let [subtree-map (subtree-to-map foreign?)]
              {:type "entity"
               :entity (-> subtree-map :COLUMN-CONSTRAINT :REFERENCES-CC :NAME)
               :farkey (-> subtree-map :COLUMN-CONSTRAINT :REFERENCES-CC :NAMES :NAME)})
            {:type (get-column-datatype column-spec)})
          {:name name
           :column name})
        :content
        {:prompts
         {:en-GB
          {:tag :prompt
           :attrs
           {:prompt name
            :local "en-GB"}}}}}})))


(defn make-entity-map [table-decl]
  "Make an ADL entity representing this table declaration"
  {:tag :entity
   :attrs
   {:name (get-name table-decl)}
   :content
   {:key {:content {}}
    :properties
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
        (deep-merge
          entity-map
          {:content
           {:properties
             {(keyword ns-cols)
              {:attrs
               {:type "entity" :entity fs-table :farkey fs-cols}}}}})
        ;; else this statement doesn't refer to us...
        ))))


(defn decorate-with-primary-key
  "If this `statement` is a primary key statement, return an entity-map like this `entity-map`
  but with the relevant property moved into 'keys'."
  [entity-map statement]
  (if
    (is-primary-key-statement? statement)
    (let [sm (subtree-to-map (is-primary-key-statement? statement))
          em-table (:name (:attrs entity-map))
          st-table (:NAME (:QUAL-NAME (:ALTER-TABLE (subtree-to-map statement))))
          col (keyword (:NAME (:NAMES (:INDEX-PARAMS sm))))
          properties (:properties (:content entity-map))
          property (col properties)
          remaining-properties (dissoc properties col)
          pk (merge
               (:content (:key (:content entity-map)))
               {:content
                {col
                 (merge
                   property
                   {:attrs
                    (merge (:attrs property)
                           {:distinct "system" :immutable "true" :required "true"})})}})]
      (if
        (= em-table st-table)
        (merge
          entity-map
          {:content
           {:key pk
            :properties remaining-properties }})))))


;; (merge electors {:content (merge (:content electors) {:properties (dissoc (:properties (:content electors)) :id :email)})})

(defn decorate-with-all
  "Apply this `function` to this `entity-map` and each of these statements
  in sequence, and return a merge of the map with each of the statements
  which actually returned a value."
  [entity-map statements function]
  (reduce
    deep-merge
    entity-map
    (remove
      nil?
      (map #(apply function (list entity-map %)) statements))))


;; TODO: link tables are not entities, and should be removed from the entities map.
;; (defn fixup-many-to-many

;;   [entities-map]
;;   (let [entities (filter #(not (is-link-table? %)) (vals entities-map))
;;         link-tables (filter is-link-table? (vals entities-map))]
;;     (reduce #() entities-map link-tables)))


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
          {(keyword x)
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
      (:property :key)
      (merge
        object
        {:content
          (map
            to-adl-xml
            (apply
              concat
              (map
                #(vals (% (:content object)))
                '(:permissions :options :prompts :helps :ifmissings))))})
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

(defn migrations-to-xml
  "As above, but for all 'up' migrations in the migrations directory specified by
  `migrations-path`. Writes XML to `output` (if specified), but returns, instead
  of the serialisable XML structure, the intermediate mappy structure, because
  that is more tractable in Clojure."
  ([migrations-path application-name]
   (migrations-to-xml migrations-path application-name (unparse (formatters :basic-date) (now))))
  ([migrations-path application-name version]
   (migrations-to-xml migrations-path application-name version nil))
  ([migrations-path application-name version output]
   (let
     [filenames
      (filter
        #(re-matches #".*\.up\.sql" %)
        (map
          #(.getAbsolutePath %)
          (filter
            #(.isFile %)
            (file-seq (file migrations-path)))))
      statements (simplify
                   (apply concat (map #(parse (slurp %)) filenames)))
      entities
      (table-definitions-to-entities
        statements)
      adl {:tag :application
           :attrs {:name application-name
                   :version version }
           :content (to-adl-xml (vals entities))}]
     (if
       output
       (spit output (str xml-header "\n" (with-out-str (emit-element adl)))))
     entities)))

