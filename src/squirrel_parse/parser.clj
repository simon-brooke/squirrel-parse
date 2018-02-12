(ns ^{:doc "A very simple parser which parses production rules."
      :author "Simon Brooke"}
  squirrel-parse.parser
  (:require [instaparse.core :as insta]
            [clojure.string :refer [split trim triml]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; squirrel-parse.parser: parse SQL into Clojure structures.
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

(def grammar
  (str
  "TABLE-SPEC := 'create' SPACE 'table' EXISTENCE SPACE NAME OPT-SPACE LPAR TABLE-SPEC-ELEMENTS RPAR ;
   TABLE-SPEC-ELEMENTS := TABLE-SPEC-ELEMENT | TABLE-SPEC-ELEMENT COMMA TABLE-SPEC-ELEMENTS ;
   TABLE-SPEC-ELEMENT := COLUMN-SPEC | TABLE-CONSTRAINT ;
   COLUMN-SPEC := NAME SPACE DATATYPE | COLUMN-SPEC SPACE COLUMN-CONSTRAINTS ;
   TABLE-CONSTRAINT := 'foo' ;
   COLUMN-CONSTRAINTS := COLUMN-CONSTRAINT | COLUMN-CONSTRAINT SPACE COLUMN-CONSTRAINT ;
   COLUMN-CONSTRAINT := 'constraint' SPACE NAME COLUMN-CONSTRAINT | NOT-NULL-CC | NULL-CC | DEFAULT-CC | UNIQUE-CC | PRIMARY-CC | REFERENCES-CC ;
   NOT-NULL-CC := 'not' SPACE 'null' ;
   NULL-CC := 'null' ;
   DEFAULT-CC := 'default' SPACE EXPR ;
   UNIQUE-CC := 'unique' SPACE INDEX-PARAMS ;
   PRIMARY-CC := 'primary' SPACE 'key' INDEX-PARAMS ;
   REFERENCES-CC := 'references' SPACE NAME SPACE LPAR NAMES RPAR REF-DIRECTIVES ;
   REF-DIRECTIVES := '' | REF-DIRECTIVE SPACE REF-DIRECTIVES ;
   REF-DIRECTIVE := REF-MATCH | REF-ON-UPDATE | REF-ON-DELETE ;
   REF-MATCH := 'match' SPACE MATCH-TYPE ;
   REF-ON-DELETE := 'on' SPACE 'delete' SPACE REF-ACTION ;
   REF-ON-UPDATE := 'on' SPACE 'update' SPACE REF-ACTION ;
   MATCH-TYPE := 'full' | 'partial' | 'simple' ;
   INDEX-PARAMS := '' ;
   REF-ACTION := 'no' SPACE 'action' | 'restrict' | 'cascade' | 'set' SPACE 'null' | 'set' SPACE 'default';
   EXPR := 'foo' ; "
  ;; there are some things we ignore
  "EXISTENCE := SPACE 'if' SPACE 'not' SPACE 'exists' | '' ;
   COMMENT := #'--[~\\n\\r]*' ;"
  "DATATYPE := 'integer' | 'float' | 'char' LPAR INT-VAL RPAR | 'varchar' LPAR INT-VAL RPAR | 'text' ;
   INT-VAL := #'[0-9]+' ;"
  ;; there don't seem to be any valid cases where a comma may not be either preceded or succeeded by white-space
  "COMMA := OPT-SPACE ',' OPT-SPACE ;
   LPAR := OPT-SPACE '(' OPT-SPACE ;
   RPAR := OPT-SPACE ')' OPT-SPACE ; "
  ;; OPT-SPACE is optional space - it's acceptable as OPT-SPACE if there are no whitespace characters. Comments are
  ;; also acceptable wherever OPT-SPACE is acceptable
  "OPT-SPACE := #'\\s*' | OPT-SPACE COMMENT OPT-SPACE;"
  ;; SPACE is mandatory white-space; comments are acceptable here too but there must be a real space character
  "SPACE := #'\\s+' | OPT-SPACE SPACE | SPACE OPT-SPACE ;
   NAMES := NAME | NAME COMMA NAMES ;
   NAME := #'[a-zA-Z][a-zA-Z0-9_]*'"))


(defn ignorable?
  "True if `x` is something which just clutters up the parse tree."
  [x]
  (and
    (coll? x)(contains? #{:SPACE :OPT-SPACE :COMMENT} (first x))))


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


(def parse
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (insta/parser grammar))

