(ns ^{:doc "A parser for SQL: generate JSON routes."
      :author "Simon Brooke"}
  squirrel-parse.to-json-routes
  (:require [clojure.java.io :refer [file]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.pprint :refer [pprint write]]
            [clojure.string :as s]
            [squirrel-parse.to-adl :refer [migrations-to-xml]]
            [squirrel-parse.to-hugsql-queries :refer [queries]]
            [squirrel-parse.utils :refer [is-link-table? singularise]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; squirrel-parse.to-json-routes: generate JSON routes.
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

;;; The overall structure of this has quite closely to follow the structure of
;;; to-hugsql-queries, because essentially we need one JSON entry point to wrap
;;; each query.

(defn file-header [parent-name this-name]
  (list
    'ns
    ^{:doc "JSON routes auto-generated by squirrel-parse"}
    (symbol (str parent-name "." this-name))
    (list
      'require
      '[noir.response :as nresponse]
      '[noir.util.route :as route]
      '[compojure.core :refer [defroutes GET POST]]
      '[ring.util.http-response :as response]
      '[clojure.java.io :as io]
      '[hugsql.core :as hugsql]
      (vector (symbol (str parent-name ".db.core")) :as 'db))))


(defn make-safe-name [string]
  (s/replace string #"[^a-zA-Z0-9-]" ""))


(defn declarations [handlers-map]
  (cons 'declare (sort (map #(symbol (make-safe-name (name %))) (keys handlers-map)))))


(defn generate-handler-src
  [handler-name query-map method doc]
  (hash-map
    :method method
    :src
    (remove
    nil?
    (list
      'defn
      handler-name
      (str "Auto-generated method to " doc)
      [{:keys ['params]}]
      (list 'do (list (symbol (str "db/" (:name query-map))) 'params))
      (case
        (:type query-map)
        (:delete-1 :update-1)
        '(response/found "/")
        nil)))))


(defn handler
  "Generate declarations for handlers from query with this `query-key` in this `queries-map` taken from within
  this `entities-map`. This method must follow the structure of
  `to-hugsql-queries/queries` quite closely, because we must generate the same names."
  [query-key queries-map entities-map]
  (let [query (query-key queries-map)
        handler-name (symbol (make-safe-name (name query-key)))]
    (hash-map
      (keyword handler-name)
      (merge
        {:name handler-name
         :route (str "/json/" handler-name)}
        (case
          (:type query)
          :delete-1
          (generate-handler-src
            handler-name query :post
            (str "delete one record from the "
                 (-> query :entity :attrs :name)
                 " table. Expects the following key(s) to be present in `params`: "
                 (doall (-> query :entity :content :key :content keys))
                 "."))
          :insert-1
          (generate-handler-src
            handler-name query :post
            (str "insert one record to the "
                 (-> query :entity :attrs :name)
                 " table. Expects the following key(s) to be present in `params`: "
                 (pr-str (-> query :entity :content :properties keys))
                 ". Returns a map containing the keys "
                 (pr-str (-> query :entity :content :key :content keys))
                 " identifying the record created."))
          :update-1
          (generate-handler-src
            handler-name query :post
            (str "update one record in the "
                 (-> query :entity :attrs :name)
                 " table. Expects the following key(s) to be present in `params`: "
                 (pr-str
                   (distinct
                     (sort
                       (flatten
                         (cons
                           (-> query :entity :content :properties keys)
                           (-> query :entity :content :key :content keys))))))
                 "."))
          :select-1
          (generate-handler-src
            handler-name query :post
            (str "select one record from the "
                 (-> query :entity :attrs :name)
                 " table. Expects the following key(s) to be present in `params`: "
                 (pr-str (-> query :entity :content :key :content keys))
                 ". Returns a map containing the following keys: "
                 (pr-str
                   (distinct
                     (sort
                       (flatten
                         (cons
                           (-> query :entity :content :properties keys)
                           (-> query :entity :content :key :content keys))))))
                 "."))
            :select-many
          (generate-handler-src
            handler-name query :get
            (str "select all records from the "
                 (-> query :entity :attrs :name)
                 " table. If the keys (:limit :offset) are present in the request then they will be used to page through the data. Returns a sequence of maps each containing the following keys: "
                 (pr-str
                   (distinct
                     (sort
                       (flatten
                         (cons
                           (-> query :entity :content :properties keys)
                           (-> query :entity :content :key :content keys))))))
                 "."))

          (:select-many-to-many
            :select-one-to-many)
          (hash-map :method :get
                    :src (list 'defn handler-name [{:keys ['params]}]
                               (list 'do (list (symbol (str "db/" (:name query))) 'params))))
          ;; default
          (hash-map
            :src
            (str ";; don't know what to do with query " :key " of type " (:type query))))))))


(defn defroutes [handlers-map]
  (cons
    'defroutes
    (cons
      'auto-rest-routes
      (map
        #(let [handler (handlers-map %)]
           (list
             (symbol (s/upper-case (name (:method handler))))
             (str "/json/auto/" (:name handler))
             'request
              (list
                'route/restricted
               (list (:name handler) 'request))))
        (sort
          (keys handlers-map))))))


(defn migrations-to-json-routes
  ([migrations-path parent-namespace-name]
   (migrations-to-json-routes migrations-path parent-namespace-name "auto-json-routes"))
  ([migrations-path parent-namespace-name namespace-name]
   (let [output (str (s/replace namespace-name #"-" "_") ".clj")
         adl-struct (migrations-to-xml migrations-path "Ignored")
         q (reduce
             merge
             {}
             (map
               #(queries % adl-struct)
               (vals adl-struct)))
         h (reduce
             merge
             {}
             (map
               #(handler % q adl-struct)
               (keys q)))
         f (cons
             (file-header parent-namespace-name namespace-name)
             ;;                          (pre-declare
             (cons
               (declarations h)
               (cons
                 (defroutes h)
                 (map #(:src (h %)) (sort (keys h))))))]
     (spit
       output
       (with-out-str
         (doall
           (for [expr f]
             (do
               (pprint expr)
               (print "\n\n"))))))
     f
     )))
