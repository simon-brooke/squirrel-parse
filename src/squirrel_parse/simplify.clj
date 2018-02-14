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


(defn ignorable?
  "True if `x` is something which just clutters up the parse tree."
  [x]
  (and
    (coll? x)(contains? #{:SPACE :OPT-SPACE :COMMENT :OPT-KW-DATA} (first x))))


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

(defn flatten-statements
  [parse-tree]
  (if
    (and (coll? parse-tree) (not (empty? parse-tree)))
    (if
      (= (first parse-tree) :STATEMENTS)
      (cond
        (>= (count parse-tree) 3)
        (cons (nth parse-tree 1) (flatten-statements (nth parse-tree 2)))
        (>= (count parse-tree) 2)
        (list (nth parse-tree 1))
        true
        ())
      parse-tree)
    parse-tree))


(defn simplify [parse-tree]
  (flatten-statements
    (remove-recursive ignorable? parse-tree)))


