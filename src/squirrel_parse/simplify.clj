(ns ^{:doc "A parser for SQL: simplify/normalise a parse tree."
      :author "Simon Brooke"}
  squirrel-parse.simplify
  (:require [clojure.string :refer [join split trim triml upper-case]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; squirrel-parse.simplify: simplify/normalise a parse tree.
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


(declare in-simplify)


(defn ignorable?
  "True if `x` is something which just clutters up the parse tree."
  [x]
  (and
    (coll? x)(contains? #{:COMMENT
                          :LPAR
                          :OPT-KW-DATA
                          :OPT-SPACE
                          :QUOTE-MK
                          :RPAR
                          :SPACE
                          :TERMINATOR} (first x))))


(defn simplify-second-of-two
  "There are a number of possible simplifications such that if the `tree` has
  only two elements, the second is semantically sufficient."
  [tree]
  (if
    (and (= (count tree) 2) (coll? (nth tree 1)))
    (in-simplify (nth tree 1))
    tree))

(defn simplify-second-if-not-empty
  "Like `simplify-second-of-two`, but returns nil if there is no second element
  of `tree`."
  [tree]
  (if
    (= (count tree) 1)
    nil
    (simplify-second-of-two tree)))

(defn remove-recursive
  "Return a collection like this `collection` from which items which are matched
  by this `predicate` have been removed at all levels."
  [predicate collection]
  (map
    #(if
       (coll? %)
       (remove-recursive predicate %)
       %)
    (remove predicate collection)))


(defn- in-simplify
  "Simplify/canonicalise this `tree`, presumed already to have had ignorables
  removed. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (if
    (coll? tree)
    (case (first tree)
      (:STATEMENTS) (remove nil? (map in-simplify (rest tree)))
      (:EXISTENCE
        :PERMANENCE) (simplify-second-if-not-empty tree)
      (:ALTER-COL-SPEC
        :ALTER-SEQ-ELEMENT
        :ALTER-STMT
        :ALTER-TABLE-ELEMENT
        :CREATE-STMT
        :EXPRESSION
        :MATCH-TYPE
        :ONLY
        :OPT-KW-SCHEMA
        :PERMISSION
        :PERMISSIONS-STMT
        :REF-DIRECTIVE
        :RO-BYPASSRLS
        :RO-CREATEDB
        :RO-CREATEROLE
        :RO-INHERIT
        :RO-LOGIN
        :RO-REPLIC
        :RO-SUPERUSER
        :ROLE-OPTION
        :SEQ-SPEC-ELEMENT
        :STATEMENT
        :TABLE-SPEC-ELEMENT
        :TC-ELEMENT
        :VALUE) (simplify-second-of-two tree)
      (:PERMISSION-COMMA
        :TABLE-SPEC-ELT-COMMA) (in-simplify (nth tree 1))
      (:ROLE) (first tree)
      (remove nil? (map in-simplify tree)))
    tree))


(defn simplify
  "Simplify/canonicalise this `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [parse-tree]
  (in-simplify (remove-recursive ignorable? parse-tree)))


