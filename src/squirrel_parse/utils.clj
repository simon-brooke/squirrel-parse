(ns ^{:doc "A parser for SQL: utility functions."
      :author "Simon Brooke"}
  squirrel-parse.utils
  (:require [clojure.string :refer [join split trim triml upper-case]]))


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


(defn- make-unterminated-case-insensitive-match-rule
  "Make a grammar rule which matches this `token` case-insensitively,
  without the terminal semi-colon"
  [token]
  (let [subtokens (split token #"\s+")
        name (join "-" subtokens)]
    (apply str
           (flatten
             (list
               (upper-case name)
               " := "
               (join " SPACE " (map #(str "#'(?i)" % "'") subtokens)))))))


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
  (join " SPACE "
        (list (if with-precision? (str match " LPAR INT-VAL RPAR") match) (if with-tz? "KW-WITH" "KW-WITHOUT") "KW-TIME" "KW-ZONE")))


(defn make-with-or-without-timezone-datatype-rule
  "Make a rule which matches this `datatype`, for datatypes which may optionally take
  'with (or without) time zone'."
  [token]
  (let [subtokens (split token #"\s+")
        name (join "-" subtokens)
        match (join " SPACE " (map #(str "#'(?i)" % "'") subtokens))]
    (apply str
           (flatten
             (list
               "DT-"
               (upper-case name)
               " := "
               (join
                 " | "
                 (list
                   match
                   (str match " LPAR INT-VAL RPAR")
                   (make-timezone-clause match true false)
                   (make-timezone-clause match true true)
                   (make-timezone-clause match false false)
                   (make-timezone-clause match false true))))))))


