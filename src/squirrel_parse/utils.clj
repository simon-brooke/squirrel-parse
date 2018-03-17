(ns ^{:doc "A parser for SQL: utility functions."
      :author "Simon Brooke"}
  squirrel-parse.utils
  (:require [clojure.string :as s]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; squirrel-parse.utils: utility functions supporting the parser.
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


(defn deep-merge [a b]
  "Merge maps `a` and `b`, recursively. If you can't find an online
  implementation which actually works, do it yourself."
  (cond
    (= a b)
    a
    (and (map? a) (map? b))
    (merge-with deep-merge a b)
    true
    b))


(defn- make-unterminated-case-insensitive-match-rule
  "Make a grammar rule which matches this `token` case-insensitively,
  without the terminal semi-colon. Keywords may always optionally be preceded
  by whitespace and are usually succeeded by whitespace."
  [token]
  (let [subtokens (s/split token #"\s+")
        name (s/join "-" subtokens)]
    (apply str
           (flatten
             (list
               (s/upper-case name)
               " := OPT-SPACE "
               (s/join " SPACE " (map #(str "#'(?i)" % "'") subtokens))
               " OPT-SPACE ")))))


(defn make-case-insensitive-match-rule
  "Make a grammar rule which matches this `token` case-insensitively, optionally prefixing the
  name of the rule with this `prefix`"
  ([token prefix]
   (str prefix "-" (make-case-insensitive-match-rule token)))
  ([token]
     (str (make-unterminated-case-insensitive-match-rule token) ";")))


(defn make-simple-datatype-rule
  "Make a rule which matches this `datatype`, for datatypes which take no parameters or special grammar."
  [token]
  (make-case-insensitive-match-rule token "DT"))


(defn make-with-integer-parameter-datatype-rule
  "Make a rule which matches this `datatype`, for datatypes which take a single integer parameter."
  [token]
  (str "DT-" (make-unterminated-case-insensitive-match-rule token) " LPAR INT-VAL RPAR ;"))


(defn- make-timezone-clause
  [match with-tz? with-precision?]
  (s/join
    " "
    (list
      (if with-precision? (str match " LPAR INT-VAL RPAR") match)
      (if with-tz? "KW-WITH" "KW-WITHOUT")
      "KW-TIME"
      "KW-ZONE")))


(defn make-with-or-without-timezone-datatype-rule
  "Make a rule which matches this `datatype`, for datatypes which may optionally take
  'with (or without) time zone'."
  [token]
  (let [subtokens (s/split token #"\s+")
        name (s/join "-" subtokens)
        match (s/join " SPACE " (map #(str "#'(?i)" % "'") subtokens))]
    (apply str
           (flatten
             (list
               "DT-"
               (s/upper-case name)
               " := "
               (s/join
                 " | "
                 (list
                   match
                   (str match " LPAR INT-VAL RPAR")
                   (make-timezone-clause match true false)
                   (make-timezone-clause match true true)
                   (make-timezone-clause match false false)
                   (make-timezone-clause match false true))))))))


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


(defn is-column-constraint-statement-of-type?
  "Returns non-nil (actually the relevant fragment) if `statement` is an
  'alter table... add column constraint' statement with the specified `key`"
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
                  (:ALTER-TABLE sm))))))))))


(defn is-create-table-statement?
  "Is this statement a create table statement?"
  [statement]
  (is-subtree-of-type? statement :CREATE-TABLE-STMT))


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


(defn is-link-table?
  [entity-map]
  (let [properties (-> entity-map :content :properties vals)
        links (filter #(-> % :attrs :entity) properties)]
    (= (count properties) (count links))))


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


(defn singularise [string]
  (s/replace (s/replace (s/replace string #"_" "-") #"s$" "") #"ie$" "y"))

