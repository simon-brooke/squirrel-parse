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


(def keywords
  "Keywords from the subset of Postgresql I have so far needed to support. Feel free to either
  1. Add further Postrgresql keywords, or
  2. Start a new def for non-postgresql keywords."
  ["action" "add" "all" "alter" "as" "by" "cache" "cascade" "collate" "column" "comment" "constraint" "create"
   "cycle" "data" "day" "default" "delete" "exists" "extension" "false" "foreign" "from" "full"
   "go" "grant" "hour" "if" "increment" "insert" "into" "key" "match" "maxvalue" "minute" "minvalue"
   "month" "no" "none" "not" "null" "off" "on" "only" "owned" "owner" "partial" "primary"
   "references" "rename" "restrict" "revoke" "schema" "second" "select" "sequence" "set"
   "simple" "start" "table" "temp" "temporary" "time" "to" "true" "type" "unique"
   "update" "using" "values" "where" "with" "without" "year" "zone"])


(def keyword-rules
  "Rules to match keywords case-insensitively. It is my practice to write SQL in
  lower case, but I know many prefer upper case."
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
  "(Postgresql) datatypes which take no arguments or special syntax"
  ["bigint" "bigserial" "bit" "boolean" "bytea"  "date" "double precision" "float" "integer" "money" "numeric" "real" "serial" "text" ])

(def with-integer-parameter-datatypes
  "(Postgresql) datatypes which take a single integer parameter, typically the storage size"
  ["char" "character" "character varying" "varchar"])

(def with-or-without-timezone-datatypes
  "(Postgresql) datatypes which may optionally take 'with (or without) time zone"
  ["time" "timestamp"])

(def special-datatypes
  "(Postgresql) Datatypes with complex special grammar"
  ["interval"])

(def interval-datatype-rules
  "`interval` has special complex grammar. TODO: this is not entirely correct; some
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
  "Rough general syntax of the subset of Postgresql I have so far needed to support.
  There will be some differences from ANSI 92. Rules are generally expressed as single
  strings. Order is not very significant, but instaparse treats the first rule as special."
  (list
    "STATEMENTS := OPT-SPACE STATEMENT | OPT-SPACE STATEMENT OPT-SPACE STATEMENTS ;"
    "STATEMENT := TABLE-DECL | ALTER-STMT | SET-STMT | COMMENT | EXTENSION-DECL | SEQUENCE-DECL | INSERT-STMT | PERMISSIONS-STMT;"
    "ALTER-STMT := ALTER-TABLE | ALTER-SEQUENCE ;"

    "SET-STMT := KW-SET NAME EQUALS EXPRESSION TERMINATOR ;"
    "EXTENSION-DECL := KW-CREATE KW-EXTENSION EXISTENCE NAME KW-WITH KW-SCHEMA NAME TERMINATOR ;"

    ;; taken from https://www.postgresql.org/docs/10/static/sql-createsequence.html
    "SEQUENCE-DECL := KW-CREATE PERMANENCE KW-SEQUENCE EXISTENCE NAME SEQ-SPEC-ELEMENTS TERMINATOR ;"
    "SEQ-SPEC-ELEMENTS := SEQ-SPEC-ELEMENT | SEQ-SPEC-ELEMENT SEQ-SPEC-ELEMENTS ;"
    "SEQ-SPEC-ELEMENT := SEQ-SPEC-AS | SEQ-SPEC-INCREMENT | SEQ-SPEC-MIN | SEQ-SPEC-MAX | SEQ-SPEC-START | SEQ-SPEC-CACHE | SEQ-SPEC-CYCLE | SEQ-SPEC-OWNER ;"
    "SEQ-SPEC-AS := KW-AS DATATYPE ;"
    "SEQ-SPEC-INCREMENT := INT-VAL | KW-INCREMENT INT-VAL | KW-INCREMENT KW-BY INT-VAL ;"
    ;; I can't tell from the spec whether expressions are allowed as the value of minvalue or maxvalue.
    "SEQ-SPEC-MIN := KW-NO KW-MINVALUE | KW-MINVALUE INT-VAL ;"
    "SEQ-SPEC-MAX := KW-NO KW-MAXVALUE | KW-MAXVALUE INT-VAL ;"
    "SEQ-SPEC-START := KW-START KW-WITH INT-VAL | KW-START INT-VAL ;"
    "SEQ-SPEC-CACHE := KW-CACHE INT-VAL ;"
    "SEQ-SPEC-CYCLE := KW-CYCLE | KW-NO KW-CYCLE ;"
    "SEQ-SPEC-OWNER := KW-OWNED KW-BY QUAL-NAME | KW-OWNED KW-BY KW-NONE ;"

    ;; from https://www.postgresql.org/docs/10/static/sql-altersequence.html
    "ALTER-SEQUENCE := KW-ALTER KW-SEQUENCE EXISTENCE NAME ALTER-SEQ-ELEMENTS TERMINATOR ;"
    "ALTER-SEQ-ELEMENTS := ALTER-SEQ-ELEMENT | ALTER-SEQ-ELEMENT ALTER-SEQ-ELEMENTS ;"
    "ALTER-SEQ-ELEMENT := SEQ-SPEC-ELEMENT | OWNER-TO | SEQ-RENAME | SEQ-SET-SCHEMA ;"
    "OWNER-TO := KW-OWNER KW-TO NAME ;"
    "SEQ-RENAME := KW-RENAME KW-TO NAME ;"
    "SEQ-SET-SCHEMA := KW-SET KW-SCHEMA NAME ;"

    ;; TODO: there is a *lot* more gramar to do here, but I don't need it (yet)
    "INSERT-STMT := KW-INSERT KW-INTO QUAL-NAME KW-VALUES LPAR VALUES RPAR TERMINATOR ;"

    ;; taken from https://www.postgresql.org/docs/10/static/sql-createtable.html
    ;; but by no means all of that is implemented.
    "TABLE-DECL := KW-CREATE PERMANENCE KW-TABLE EXISTENCE NAME LPAR TABLE-SPEC-ELEMENTS RPAR TERMINATOR ;"
    "TABLE-SPEC-ELEMENTS := TABLE-SPEC-ELEMENT | TABLE-SPEC-ELEMENT COMMA TABLE-SPEC-ELEMENTS ; "
    "TABLE-SPEC-ELEMENT := COLUMN-SPEC | TABLE-CONSTRAINT ;"
    "COLUMN-SPEC := NAME SPACE DATATYPE | COLUMN-SPEC OPT-SPACE COLUMN-CONSTRAINTS ;"
    "TABLE-CONSTRAINT := 'TODO' ;"
    "COLUMN-CONSTRAINTS := COLUMN-CONSTRAINT | COLUMN-CONSTRAINT SPACE COLUMN-CONSTRAINT ;"
    "COLUMN-CONSTRAINT := KW-CONSTRAINT NAME COLUMN-CONSTRAINT | NOT-NULL-CC | NULL-CC | DEFAULT-CC | UNIQUE-CC | PRIMARY-CC | REFERENCES-CC ;"
    "NOT-NULL-CC := KW-NOT KW-NULL ;"
    "NULL-CC := KW-NULL ;"
    "DEFAULT-CC := KW-DEFAULT EXPRESSION ;"
    "UNIQUE-CC := KW-UNIQUE INDEX-PARAMS ;"
    "PRIMARY-CC := KW-PRIMARY KW-KEY INDEX-PARAMS ;"
    "REFERENCES-CC := KW-REFERENCES NAME LPAR NAMES RPAR REF-DIRECTIVES | KW-FOREIGN KW-KEY LPAR NAMES RPAR REFERENCES-CC;"
    "REF-DIRECTIVES := '' | REF-DIRECTIVE REF-DIRECTIVES ;"
    "REF-DIRECTIVE := REF-MATCH | REF-ON-UPDATE | REF-ON-DELETE ;"
    "REF-MATCH := KW-MATCH MATCH-TYPE ;"
    "REF-ON-DELETE := KW-ON KW-DELETE REF-ACTION ;"
    "REF-ON-UPDATE := KW-ON KW-UPDATE REF-ACTION ;"
    "MATCH-TYPE := KW-FULL | KW-PARTIAL | KW-SIMPLE ;"
    "INDEX-PARAMS := EXPRESSION ;"
    "REF-ACTION := KW-NO KW-ACTION | KW-RESTRICT | KW-CASCADE | KW-SET VALUE;"
    "EXISTENCE := '' |  KW-IF KW-NOT KW-EXISTS |  KW-IF KW-EXISTS ;"
    "PERMANENCE := '' | KW-TEMP | KW-TEMPORARY ;"
    "ONLY := KW-ONLY | '' ;"

    "ALTER-TABLE := KW-ALTER KW-TABLE EXISTENCE ONLY QUAL-NAME OPT-SPACE ALTER-TABLE-ELEMENTS TERMINATOR ;"
    "ALTER-TABLE-ELEMENTS := ALTER-TABLE-ELEMENT | ALTER-TABLE-ELEMENT OPT-SPACE ALTER-TABLE-ELEMENTS ;"
    "ALTER-TABLE-ELEMENT :=  OWNER-TO | ALTER-COLUMN | ADD-CONSTRAINT;"
    "ALTER-COLUMN := KW-ALTER KW-COLUMN NAME ALTER-COL-SPEC ;"
    "ALTER-COL-SPEC := ALTER-COL-TYPE | ALTER-COL-DFLT;"
    "ALTER-COL-TYPE := KW-SET OPT-KW-DATA KW-TYPE DATATYPE | ALTER-COL-TYPE KW-COLLATE NAME | ALTER-COL-TYPE KW-USING EXPRESSION ;"
    "ALTER-COL-DFLT := KW-SET KW-DEFAULT EXPRESSION ;"
    "ADD-CONSTRAINT := KW-ADD COLUMN-CONSTRAINT ;"
    "OPT-KW-DATA := KW-DATA | '' ;"
;;    "TABLE-ADD-COL :=

    "PERMISSIONS-STMT := REVOKE-STMT | GRANT-STMT;"
    "REVOKE-STMT := KW-REVOKE PERMISSIONS KW-ON OPT-KW-SCHEMA QUAL-NAME KW-FROM NAMES TERMINATOR;"
    "GRANT-STMT := KW-GRANT PERMISSIONS KW-ON OPT-KW-SCHEMA QUAL-NAME KW-TO NAMES TERMINATOR;"

    "PERMISSIONS := PERMISSION | PERMISSION COMMA PERMISSIONS ;"
    "PERMISSION := KW-ALL | KW-SELECT | KW-INSERT | KW-UPDATE | KW-DELETE ;"

    "OPT-KW-SCHEMA := KW-SCHEMA | '' ;"

    "COMMENT := KW-COMMENT #'[^;]*' TERMINATOR |#'--[^\\n\\r]*' ;"
    ;; TODO: much more to do here!
    "EXPRESSION := VALUE | KEYWORD | FUNCTION | LPAR EXPRESSION RPAR;"
    "FUNCTION := NAME LPAR VALUES RPAR | NAME LPAR RPAR ;"
    ;; TODO: same for values.
    "VALUES := VALUE | VALUE COMMA VALUES ;"
    "VALUE := INT-VAL | CHAR-VAL | TRUTH-VAL | KW-NULL | NAMES | VALUE '::' NAME;"
    "INT-VAL := #'[0-9]+' ;"
    "CHAR-VAL := #'\\047.*\\047' ;"
    "TRUTH-VAL := KW-TRUE | KW-FALSE ;"
    "DOT := '.' ;"
    ;; there don't seem to be any valid cases where a comma may not be either preceded or succeeded by white-space
    "COMMA := OPT-SPACE ',' OPT-SPACE ;"
    "LPAR := OPT-SPACE '(' OPT-SPACE ;"
    "RPAR := OPT-SPACE ')' OPT-SPACE ; "
    "EQUALS := OPT-SPACE '=' OPT-SPACE ;"
    ;; OPT-SPACE is optional space - it's acceptable as OPT-SPACE if there are no whitespace characters. Comments are
    ;; also acceptable wherever OPT-SPACE is acceptable
    "OPT-SPACE := #'\\s*' | OPT-SPACE COMMENT OPT-SPACE;"
    ;; SPACE is mandatory white-space; comments are acceptable here too but there must be a real space character
    "SPACE := #'\\s+' | OPT-SPACE SPACE | SPACE OPT-SPACE ;"
    "QUAL-NAME := NAME | NAME DOT NAME ;"
    "NAMES := NAME | NAME COMMA NAMES ;"
    "NAME := #'[a-zA-Z][a-zA-Z0-9_]*'"
    "TERMINATOR := SEMI-COLON | KW-GO | OPT-SPACE TERMINATOR OPT-SPACE;"
    "SEMI-COLON := ';';"))


(def grammar (join "\n" (flatten (list basic-rules datatype-rules keyword-rules))))

(def parse
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (insta/parser grammar))

(def parse-comment (insta/parser "COMMENT := #'--[^\\n\\r]*' ;"))
