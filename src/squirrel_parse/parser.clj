(ns ^{:doc "A parser for SQL."
      :author "Simon Brooke"}
  squirrel-parse.parser
  (:require [instaparse.core :as insta]
            [clojure.string :refer [join split trim triml upper-case]]
            [squirrel-parse.utils :refer :all]))

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

(def keywords ["action" "alter" "as" "by" "cache" "cascade" "comment" "constraint" "create" "cycle" "day" "default" "delete" "exists"
               "extension" "false" "from" "full" "go" "hour" "if" "increment" "key" "match" "maxvalue" "minute" "minvalue"
               "month" "no" "none" "not" "null" "off" "on" "owned" "partial" "primary" "references" "restrict" "schema" "second"
               "select" "sequence" "set" "simple" "start" "table" "temp" "temporary" "time" "to" "true" "unique" "update" "where" "with" "without"
               "year" "zone"])


(def keyword-rules
  (flatten
    (list
      (apply str
             (flatten
               (list
                 "KEYWORD :="
                 (join " | "
                       (map #(str "KW-" (upper-case (join "-" (split % #"\s+"))))
                            keywords)))))
      (map #(make-case-insensitive-match-rule % "KW") keywords))))


(def simple-datatypes
  "datatypes which take no arguments or special syntax"
  ["bigserial" "bit" "boolean" "bytea"  "date" "double precision" "float" "integer" "money" "numeric" "real" "serial" "text" ])

(def with-integer-parameter-datatypes
  "datatypes which take a single integer parameter, typically the storage size"
  ["char" "character" "character varying" "varchar"])

(def with-or-without-timezone-datatypes
  "datatypes which may optionally take 'with (or without) time zone"
  ["time" "timestamp"])

(def special-datatypes
  "Datatypes with complex special grammar"
  ["interval"])

(def interval-datatype-rules
  "interval has special complex grammar. TODO: this is not entirely correct; some
  combinations of 'x to y' are not allowed."
  (list "DT-INTERVAL := #'(?i)interval' | #'(?i)interval' SPACE INTERVAL-FIELDS;"
  "INTERVAL-FIELDS := INTERVAL-FIELD | INTERVAL-FIELD SPACE KW-TO SPACE INTERVAL-FIELD;"
  "INTERVAL-FIELD := KW-YEAR | KW-MONTH | KW-DAY | KW-HOUR | KW-MINUTE | KW-SECOND;"))

(def datatype-rules
  "All rules for all datatypes"
  (list
    (apply str
           (flatten
             (list "DATATYPE := "
                   (join " | "
                         (map #(str "DT-" (upper-case (join "-" (split % #"\s+"))))
                              (flatten
                                (list simple-datatypes
                                      with-integer-parameter-datatypes
                                      with-or-without-timezone-datatypes
                                      special-datatypes)))) " ;\n")))
    (map make-simple-datatype-rule simple-datatypes)
    (map make-with-integer-parameter-datatype-rule with-integer-parameter-datatypes)
    (map make-with-or-without-timezone-datatype-rule with-or-without-timezone-datatypes)
    interval-datatype-rules))

(def basic-rules
  (list
    "STATEMENTS := STATEMENT TERMINATOR | STATEMENT TERMINATOR STATEMENTS ;"
    "STATEMENT := TABLE-DECL | ALTER-STMT | SET-STMT | COMMENT-STMT | EXTENSION-DECL | SEQUENCE-DECL;"
    "ALTER-STMT := ALTER-TABLE ;"
    "ALTER-TABLE := KW-ALTER SPACE KW-TABLE SPACE QUAL-NAME SPACE NAME SPACE KW-TO SPACE NAME ;"
    "SET-STMT := KW-SET SPACE NAME OPT-SPACE '=' OPT-SPACE EXPRESSION ;"
    "COMMENT-STMT := KW-COMMENT #'[~;]*' ;"
    "EXTENSION-DECL := KW-CREATE SPACE KW-EXTENSION EXISTENCE SPACE NAME SPACE KW-WITH SPACE KW-SCHEMA SPACE NAME ;"

    ;; taken from https://www.postgresql.org/docs/10/static/sql-createsequence.html
    "SEQUENCE-DECL := KW-CREATE SPACE PERMANENCE KW-SEQUENCE EXISTENCE SPACE NAME SPACE SEQ-SPEC-ELEMENTS ;"
    "SEQ-SPEC-ELEMENTS := SEQ-SPEC-ELEMENT | SEQ-SPEC-ELEMENT SPACE SEQ-SPEC-ELEMENTS ;"
    "SEQ-SPEC-ELEMENT := SEQ-SPEC-AS | SEQ-SPEC-INCREMENT | SEQ-SPEC-MIN | SEQ-SPEC-MAX | SEQ-SPEC-START | SEQ-SPEC-CACHE | SEQ-SPEC-CYCLE | SEQ-SPEC-OWNER ;"
    "SEQ-SPEC-AS := KW-AS DATATYPE ;"
    "SEQ-SPEC-INCREMENT := INT-VAL | KW-INCREMENT SPACE INT-VAL | KW-INCREMENT SPACE KW-BY SPACE INT-VAL ;"
    ;; I can't tell from the spec whether expressions are allowed as the value of minvalue or maxvalue.
    "SEQ-SPEC-MIN := KW-NO SPACE KW-MINVALUE | KW-MINVALUE SPACE INT-VAL ;"
    "SEQ-SPEC-MAX := KW-NO SPACE KW-MAXVALUE | KW-MAXVALUE SPACE INT-VAL ;"
    "SEQ-SPEC-START := KW-START SPACE KW-WITH SPACE INT-VAL | KW-START SPACE INT-VAL ;"
    "SEQ-SPEC-CACHE := KW-CACHE SPACE INT-VAL ;"
    "SEQ-SPEC-CYCLE := KW-CYCLE | KW-NO SPACE KW-CYCLE ;"
    "SEQ-SPEC-OWNER := KW-OWNED SPACE KW-BY SPACE QUAL-NAME | KW-OWNED SPACE KW-BY SPACE KW-NONE ;"

    ;; taken from https://www.postgresql.org/docs/10/static/sql-createtable.html
    ;; but by no means all of that is implemented.
    "TABLE-DECL := KW-CREATE SPACE PERMANENCE KW-TABLE EXISTENCE SPACE NAME OPT-SPACE LPAR TABLE-SPEC-ELEMENTS RPAR ;"
    "TABLE-SPEC-ELEMENTS := TABLE-SPEC-ELEMENT | TABLE-SPEC-ELEMENT COMMA TABLE-SPEC-ELEMENTS ; "
    "TABLE-SPEC-ELEMENT := COLUMN-SPEC | TABLE-CONSTRAINT ;"
    "COLUMN-SPEC := NAME SPACE DATATYPE | COLUMN-SPEC OPT-SPACE COLUMN-CONSTRAINTS ;"
    "TABLE-CONSTRAINT := 'TODO' ;"
    "COLUMN-CONSTRAINTS := COLUMN-CONSTRAINT | COLUMN-CONSTRAINT SPACE COLUMN-CONSTRAINT ;"
    "COLUMN-CONSTRAINT := KW-CONSTRAINT SPACE NAME COLUMN-CONSTRAINT | NOT-NULL-CC | NULL-CC | DEFAULT-CC | UNIQUE-CC | PRIMARY-CC | REFERENCES-CC ;"
    "NOT-NULL-CC := KW-NOT SPACE KW-NULL ;"
    "NULL-CC := KW-NULL ;"
    "DEFAULT-CC := KW-DEFAULT SPACE EXPR ;"
    "UNIQUE-CC := KW-UNIQUE SPACE INDEX-PARAMS ;"
    "PRIMARY-CC := KW-PRIMARY SPACE KW-KEY INDEX-PARAMS ;"
    "REFERENCES-CC := KW-REFERENCES SPACE NAME SPACE LPAR NAMES RPAR REF-DIRECTIVES ;"
    "REF-DIRECTIVES := '' | REF-DIRECTIVE SPACE REF-DIRECTIVES ;"
    "REF-DIRECTIVE := REF-MATCH | REF-ON-UPDATE | REF-ON-DELETE ;"
    "REF-MATCH := KW-MATCH SPACE MATCH-TYPE ;"
    "REF-ON-DELETE := KW-ON SPACE KW-DELETE SPACE REF-ACTION ;"
    "REF-ON-UPDATE := KW-ON SPACE KW-UPDATE SPACE REF-ACTION ;"
    "MATCH-TYPE := KW-FULL | KW-PARTIAL | KW-SIMPLE ;"
    "INDEX-PARAMS := 'foo' ;"
    "REF-ACTION := KW-NO SPACE KW-ACTION | KW-RESTRICT | KW-CASCADE | KW-SET SPACE VALUE;"
    "EXPR := 'TODO' ; "
    "EXISTENCE := '' | SPACE KW-IF SPACE KW-NOT SPACE KW-EXISTS ;"
    "PERMANENCE := '' | KW-TEMP SPACE | KW-TEMPORARY SPACE ;"
    "COMMENT := #'--[~\\n\\r]*' ;"
    ;; TODO: much more to do here!
    "EXPRESSION := VALUE | KEYWORD;"
    ;; TODO: same for values.
    "VALUE := INT-VAL | CHAR-VAL | TRUTH-VAL | KW-NULL | NAMES;"
    "INT-VAL := #'[0-9]+' ;"
    "CHAR-VAL := #'\\047.*\\047' ;"
    "TRUTH-VAL := KW-TRUE | KW-FALSE ;"
    "DOT := '.' ;"
    ;; there don't seem to be any valid cases where a comma may not be either preceded or succeeded by white-space
    "COMMA := OPT-SPACE ',' OPT-SPACE ;"
    "LPAR := OPT-SPACE '(' OPT-SPACE ;"
    "RPAR := OPT-SPACE ')' OPT-SPACE ; "
    ;; OPT-SPACE is optional space - it's acceptable as OPT-SPACE if there are no whitespace characters. Comments are
    ;; also acceptable wherever OPT-SPACE is acceptable
    "OPT-SPACE := #'\\s*' | OPT-SPACE COMMENT OPT-SPACE;"
    ;; SPACE is mandatory white-space; comments are acceptable here too but there must be a real space character
    "SPACE := #'\\s+' | OPT-SPACE SPACE | SPACE OPT-SPACE ;"
    "QUAL-NAME := NAME | NAME DOT NAME ;"
    "NAMES := NAME | NAME COMMA NAMES ;"
    "NAME := #'[a-zA-Z][a-zA-Z0-9_]*'"
    "TERMINATOR := OPT-SPACE SEMI-COLON OPT-SPACE | OPT-SPACE KW-GO OPT-SPACE;"
    "SEMI-COLON := ';';"))


(def grammar (join "\n" (flatten (list basic-rules datatype-rules keyword-rules))))


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

