(ns squirrel-parse.core
  (:require [squirrel-parse.parser :refer [parse]]
            [squirrel-parse.simplify :refer [simplify]]
            [squirrel-parse.to-adl :refer [table-definitions-to-entities]]
            [squirrel-parse.to-hugsql-queries :refer [migrations-to-queries-sql]]))

;;; This is get-you-started code.

(defn parsed-statements-from-file
  "Parses the file of SQL commands indicated by `filename`, and returns a sequence of parsed statements."
  [filename]
  (simplify (parse (slurp filename))))

(defn mappy-structure-from-file
  "Parses the file of SQL commands indicated by `filename`, and returns a more useful map of maps."
  [filename]
  (table-definitions-to-entities (parsed-statements-from-file filename)))


