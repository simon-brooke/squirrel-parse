(ns ^{:doc "A parser for SQL: generate Application Description Language."
      :author "Simon Brooke"}
  squirrel-parse.to-adl
  (:require [clojure.xml :refer [emit-element]]
            [squirrel-parse.parser :refer [parse]]
            [squirrel-parse.simplify :refer [simplify]]))


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
   :DT-TEXT              :string
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

(defn is-create-table-statement?
  "Is this statement a create table statement?"
  [statement]
  (and
    (is-subtree-of-type? statement :STATEMENT)
    (is-subtree-of-type? (second statement) :TABLE-DECL)))

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
  (let [name-elt (make-property subtree :NAME)]
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
    {:tag :property
     :attrs
     {
       :name (get-name column-spec)
       :type (get-column-datatype column-spec)
       }}
    ))

(defn make-entity [table-decl]
  "Make an ADL entity representing this table declaration"
  {:tag :entity
    :name (get-name table-decl)
    :content
    (apply
      vector
      (map
        make-property
        (remove
          nil?
          (map
            #(if
               (and
                 (is-subtree-of-type? % :TABLE-SPEC-ELEMENT)
                 (is-subtree-of-type? (second %) :COLUMN-SPEC))
               (second %))
            (get-first-child-of-type table-decl :TABLE-SPEC-ELEMENTS)))))})


(defn table-definition-to-entity
  "Return a map like this `map` with, if this `statement` is a table declaration,
  an ADL entity representing that table added to it."
  [entity-map statement]
  (if
    (is-create-table-statement? statement)
    (let [table-decl (second statement)
          table-name (get-name table-decl)]
      (merge entity-map {table-name (make-entity table-decl)}))
    entity-map))

(defn table-definitions-to-entities
  "Extract table definitions from these `statements` as a map of ADL
   entities indexed by name."
  ([statements]
   (reduce table-definition-to-entity {} statements)))


(defn to-adl [filename name]
  (let [entities (table-definitions-to-entities (simplify (parse (slurp filename))))]
    [{:tag :application
      :name name
      :content (vals entities)}
     nil]))

