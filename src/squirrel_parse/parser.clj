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
  ["action" "add" "admin" "all" "alter" "as" "asc"
   "by"
   "cache" "cascade" "collate" "collation" "column" "comment" "concurrently" "connection" "constraint" "create" "cycle"
   "data" "day" "default" "delete" "desc" "drop"
   "encrypted" "exists" "extension"
   "false" "first" "foreign" "from" "full"
   "go" "grant" "group"
   "hour"
   "if" "increment" "index" "insert" "in" "into"
   "key"
   "last" "limit"
   "match" "maxvalue" "method" "minute" "minvalue" "month"
   "no" "none" "not" "null" "nulls"
   "off" "on" "only" "owned" "owner"
   "partial" "password" "primary"
   "references" "rename" "restrict" "revoke" "role"
   "schema" "second" "select" "sequence" "set" "simple" "start" "sysid"
   "table" "temp" "temporary" "time" "to" "true" "type"
   "unique" "until" "update" "user" "using"
   "valid" "values"
   "where" "with" "without"
   "year"
   "zone"

   ;; the next group are all role options. I'm not sure whether or not they
   ;; really count as keywords. I'm also not sure whether 'NO' should be created
   ;; as a special case insensitive prefix, which would save half of these.
   "bypassrls" "createdb" "createrole" "inherit" "login" "nobypassrls" "nocreatedb"
   "nocreaterole" "noinherit" "nologin" "noreplication" "nosuperuser" "replication" "superuser"
   ])


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
  ["bigint" "bigserial" "bit" "boolean" "bytea"  "date" "double precision" "float" "int" "integer" "money" "numeric" "real" "serial" "text" ])

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
    "STATEMENTS := OPT-SPACE STATEMENT + ;"
    "STATEMENT := CREATE-STMT |
                  ALTER-STMT |
                  SET-STMT |
                  COMMENT |
                  DROP-STMT |
                  EXTENSION-DECL |
                  SEQUENCE-DECL |
                  INSERT-STMT |
                  PERMISSIONS-STMT;"
    "ALTER-STMT := ALTER-TABLE | ALTER-SEQUENCE ;"

    "CREATE-STMT := CREATE-TABLE-STMT | CREATE-INDEX-STMT | CREATE-ROLE-STMT ;"

    "DROP-STMT := KW-DROP OBJ-TYPE EXISTENCE QUAL-NAME TERMINATOR ;"
    "OBJ-TYPE := KW-TABLE | KW-INDEX ;"

    "SET-STMT := KW-SET NAME EQUALS EXPRESSION TERMINATOR ;"
    "EXTENSION-DECL := KW-CREATE KW-EXTENSION EXISTENCE NAME KW-WITH KW-SCHEMA NAME TERMINATOR ;"

    ;; taken from https://www.postgresql.org/docs/10/static/sql-createsequence.html
    "SEQUENCE-DECL := KW-CREATE PERMANENCE KW-SEQUENCE EXISTENCE NAME SEQ-SPEC-ELEMENTS TERMINATOR ;"
    "SEQ-SPEC-ELEMENTS := SEQ-SPEC-ELEMENT + ;"
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
    "ALTER-SEQ-ELEMENTS := ALTER-SEQ-ELEMENT + ;"
    "ALTER-SEQ-ELEMENT := SEQ-SPEC-ELEMENT | OWNER-TO | SEQ-RENAME | SEQ-SET-SCHEMA ;"
    "OWNER-TO := KW-OWNER KW-TO NAME ;"
    "SEQ-RENAME := KW-RENAME KW-TO NAME ;"
    "SEQ-SET-SCHEMA := KW-SET KW-SCHEMA NAME ;"

    ;; TODO: there is a *lot* more gramar to do here, but I don't need it (yet)
    "INSERT-STMT := KW-INSERT KW-INTO QUAL-NAME KW-VALUES LPAR VALUES RPAR TERMINATOR ;"

    ;; taken from https://www.postgresql.org/docs/10/static/sql-createtable.html
    ;; but by no means all of that is implemented.
    "CREATE-TABLE-STMT := KW-CREATE PERMANENCE KW-TABLE EXISTENCE NAME LPAR TABLE-SPEC-ELEMENTS RPAR TERMINATOR ;"
    "TABLE-SPEC-ELEMENTS := TABLE-SPEC-ELT-COMMA * TABLE-SPEC-ELEMENT OPT-COMMA; "
    "TABLE-SPEC-ELT-COMMA := TABLE-SPEC-ELEMENT COMMA ;"
    "TABLE-SPEC-ELEMENT := COLUMN-SPEC | TABLE-CONSTRAINT ;"
    "COLUMN-SPEC := NAME SPACE DATATYPE COLUMN-CONSTRAINTS ;"
    "TABLE-CONSTRAINT := KW-CONSTRAINT NAME TC-ELEMENT ;"
    "TC-ELEMENT := TC-ELT-FK ;" ;; OR OTHERS...
    "TC-ELT-FK := REFERENCES-CC ;"
    "COLUMN-CONSTRAINTS := COLUMN-CONSTRAINT * ;"
    "COLUMN-CONSTRAINT := KW-CONSTRAINT NAME COLUMN-CONSTRAINT | NOT-NULL-CC | NULL-CC | DEFAULT-CC | UNIQUE-CC | PRIMARY-CC | REFERENCES-CC ;"
    "NOT-NULL-CC := KW-NOT KW-NULL ;"
    "NULL-CC := KW-NULL ;"
    "DEFAULT-CC := KW-DEFAULT EXPRESSION ;"
    "UNIQUE-CC := KW-UNIQUE INDEX-PARAMS ;"
    "PRIMARY-CC := KW-PRIMARY KW-KEY INDEX-PARAMS ;"
    "REFERENCES-CC := KW-REFERENCES NAME LPAR NAMES RPAR REF-DIRECTIVES | KW-REFERENCES NAME REF-DIRECTIVES | KW-FOREIGN KW-KEY LPAR NAMES RPAR REFERENCES-CC;"
    "REF-DIRECTIVES := REF-DIRECTIVE * ;"
    "REF-DIRECTIVE := REF-MATCH | REF-ON-UPDATE | REF-ON-DELETE ;"
    "REF-MATCH := KW-MATCH MATCH-TYPE ;"
    "REF-ON-DELETE := KW-ON KW-DELETE REF-ACTION ;"
    "REF-ON-UPDATE := KW-ON KW-UPDATE REF-ACTION ;"
    "MATCH-TYPE := KW-FULL | KW-PARTIAL | KW-SIMPLE ;"
    "INDEX-PARAMS := EXPRESSION | '' ;"
    "REF-ACTION := KW-NO KW-ACTION | KW-RESTRICT | KW-CASCADE | KW-SET VALUE;"
    "EXISTENCE := '' |  KW-IF KW-NOT KW-EXISTS | KW-IF KW-EXISTS ;"
    "PERMANENCE := '' | KW-TEMP | KW-TEMPORARY ;"

    "ALTER-TABLE := KW-ALTER KW-TABLE EXISTENCE  KW-ONLY ? QUAL-NAME OPT-SPACE ALTER-TABLE-ELEMENTS TERMINATOR ;"
    "ALTER-TABLE-ELEMENTS := ALTER-TABLE-ELEMENT + ;"
    "ALTER-TABLE-ELEMENT :=  OWNER-TO | ALTER-COLUMN | ADD-CONSTRAINT;"
    "ALTER-COLUMN := KW-ALTER KW-COLUMN NAME ALTER-COL-SPEC ;"
    "ALTER-COL-SPEC := ALTER-COL-TYPE | ALTER-COL-DFLT;"
    "ALTER-COL-TYPE := KW-SET OPT-KW-DATA KW-TYPE DATATYPE | ALTER-COL-TYPE KW-COLLATE NAME | ALTER-COL-TYPE KW-USING EXPRESSION ;"
    "ALTER-COL-DFLT := KW-SET KW-DEFAULT EXPRESSION ;"
    "ADD-CONSTRAINT := KW-ADD COLUMN-CONSTRAINT ;"

    "CREATE-INDEX-STMT := KW-CREATE OPT-KW-UNIQUE KW-INDEX OPT-KW-CONCURRENTLY NAME ID-METHOD ? KW-ON QUAL-NAME LPAR NAMES RPAR INDEX-DIRECTIVES TERMINATOR;"
    "INDEX-DIRECTIVES := INDEX-DIRECTIVE *;"
    "INDEX-DIRECTIVE := ID-COLLATION ;"

    "ID-COLLATION := KW-COLLATION NAME "
    "ID-METHOD := KW-USING KW-METHOD NAME ;"
    "ID-NULLS := KW-NULLS KW-FIRST | KW-NULLS KW-LAST;"
    "ID-SORT-ORDER := KW-ASC | KW-DESC ;"

    ;; from https://www.postgresql.org/docs/current/static/sql-createrole.html
    ;;      https://www.postgresql.org/docs/10/static/sql-createuser.html
    "CREATE-ROLE-STMT := KW-CREATE ROLE NAME ROLE-OPTIONS TERMINATOR;"
    "ROLE := KW-GROUP | KW-ROLE | KW-USER ;"
    "ROLE-OPTIONS := KW-WITH ROLE-OPTIONS | OPT-SPACE ROLE-OPTION *;"
    "ROLE-OPTION := RO-SUPERUSER | RO-CREATEDB | RO-CREATEROLE | RO-INHERIT | RO-REPLIC | RO-BYPASSRLS | RO-LOGIN| RO-CONN-LIMIT | RO-PASSWORD | RO-TIMEOUT | RO-IN-ROLE | RO-ROLE | RO-ADMIN | RO-USER | RO-SYSID;"
    "RO-SUPERUSER := KW-SUPERUSER | KW-NOSUPERUSER ;"
    "RO-CREATEDB := KW-CREATEDB | KW-NOCREATEDB ;"
    "RO-CREATEROLE := KW-CREATEROLE | KW-NOCREATEROLE ;"
    "RO-INHERIT := KW-INHERIT | KW-NOINHERIT ;"
    "RO-REPLIC := KW-REPLICATION | KW-NOREPLICATION ;"
    "RO-BYPASSRLS := KW-BYPASSRLS | KW-NOBYPASSRLS ;"
    "RO-LOGIN := KW-LOGIN | KW-NOLOGIN ;"
    "RO-CONN-LIMIT := KW-CONNECTION KW-LIMIT INT-VAL ;"
    "RO-PASSWORD := KW-PASSWORD CHAR-VAL | KW-ENCRYPTED KW-PASSWORD CHAR-VAL ;"
    ;; The value here is actually a date/time value, but that's a level of detail we don't need.
    "RO-TIMEOUT := KW-VALID KW-UNTIL CHAR-VAL ;"
    "RO-IN-ROLE := KW-IN KW-ROLE NAMES | KW-IN KW-GROUP NAMES ;"
    "RO-ROLE := KW-ROLE NAMES ;"
    "RO-ADMIN := KW-ADMIN NAMES ;"
    "RO-USER := KW-USER NAMES ;"
    "RO-SYSID := KW-SYSID CHAR-VAL ;"


    "PERMISSIONS-STMT := REVOKE-STMT | GRANT-STMT;"
    "REVOKE-STMT := KW-REVOKE PERMISSIONS KW-ON OPT-KW-SCHEMA QUAL-NAME KW-FROM NAMES TERMINATOR;"
    "GRANT-STMT := KW-GRANT PERMISSIONS KW-ON OPT-KW-SCHEMA QUAL-NAME KW-TO OPT-ROLE NAMES TERMINATOR;"
    "OPT-ROLE := ROLE | '';"

    "PERMISSIONS := PERMISSION-COMMA * PERMISSION ;"
    "PERMISSION-COMMA := PERMISSION COMMA ;"
    "PERMISSION := KW-ALL | KW-SELECT | KW-INSERT | KW-UPDATE | KW-DELETE ;"

    "OPT-KW-CONCURRENTLY := KW-CONCURRENTLY | '' ;"
    "OPT-KW-DATA := KW-DATA | '' ;"
    "OPT-KW-SCHEMA := KW-SCHEMA | '' ;"
    "OPT-KW-UNIQUE := KW-UNIQUE | '' ;"

    "COMMENT := KW-COMMENT #'[^;]*' TERMINATOR |#'--[^\\n\\r]*' ;"
    ;; TODO: much more to do here!
    "EXPRESSION := VALUE | KEYWORD | FUNCTION | LPAR EXPRESSION RPAR;"
    "FUNCTION := NAME LPAR VALUES RPAR | NAME LPAR RPAR ;"
    ;; TODO: same for values.
    "VALUES := VALUE COMMA *;"
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
    "OPT-COMMA := COMMA | OPT-SPACE ;"
    ;; OPT-SPACE is optional space - it's acceptable as OPT-SPACE if there are no whitespace characters. Comments are
    ;; also acceptable wherever OPT-SPACE is acceptable
    "OPT-SPACE := #'\\s*' | OPT-SPACE COMMENT OPT-SPACE;"
    ;; SPACE is mandatory white-space; comments are acceptable here too but there must be a real space character
    "SPACE := #'\\s+' | OPT-SPACE SPACE | SPACE OPT-SPACE ;"
    "QUAL-NAME := NAME | NAME DOT NAME ;"
    "NAMES := NAME | NAME COMMA NAMES ;"
    "NAME := #'[a-zA-Z][a-zA-Z0-9_]*' | QUOTE-MK NAME QUOTE-MK ;"
    "QUOTE-MK := '\"' ;"
    "TERMINATOR := SEMI-COLON | KW-GO | OPT-SPACE TERMINATOR OPT-SPACE;"
    "SEMI-COLON := ';';"))


(def grammar (join "\n" (flatten (list basic-rules datatype-rules keyword-rules))))

(def parse
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (insta/parser grammar))


