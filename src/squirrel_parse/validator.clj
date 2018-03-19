(ns ^{:doc "A parser for SQL: validator for ADL structure."
      :author "Simon Brooke"}
  squirrel-parse.validator
  (:require [clojure.set :refer [union]]
            [clojure.string :as s]
            [bouncer.core :as b]
            [bouncer.validators :as v]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; squirrel-parse.to-adl: validate Application Description Language.
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


;;; this is a fairly straight translation of the ADL 1.4 DTD into Clojure

(def permissions
  "permissions a group may have on an entity, list, page, form or field
	permissions are deemed to increase as you go right. A group cannot
	have greater permission on a field than on the form it is in, or
	greater permission on form than the entity it belongs to

	* `none`:			none
	* `read`:			select
	* `insert`:			insert
	* `noedit`:			select, insert
	* `edit`:			select, insert, update
	* `all`:			select, insert, update, delete"
  #{"none", "read", "insert", "noedit", "edit", "all"})

(def cascade-actions
  "actions which should be cascaded to dependent objects. All these values except
  'manual' are taken from Hibernate and should be passed through the adl2hibernate
  mapping transparently. Relevent only for properties with type='entity', type='link'
  and type='list'

  * `all`:       cascade delete, save and update
  * `all-delete-orphan`: see hibernate documentation; relates to transient objects only
  * `delete`:    cascade delete actions, but not save and update
  * `manual`:    cascading will be handled in manually managed code, code to
              handle cascading should not be generated
  * `save-update`: cascade save and update actions, but not delete."
  #{"all", "all-delete-orphan", "delete", "manual", "save-update"})

(def defineable-data-types
	"data types which can be used in a typedef to provide validation -
	e.g. a string can be used with a regexp or a scalar can be used with
	min and max values
	* `string`: 		varchar		java.sql.Types.VARCHAR
	* `integer`:		int			java.sql.Types.INTEGER
	* `real`:			double		java.sql.Types.DOUBLE
	* `money`:			money		java.sql.Types.INTEGER
	* `date`:			date		java.sql.Types.DATE
	* `time`:			time		java.sql.Types.TIME
	* `timestamp`:		timestamp	java.sql.Types.TIMESTAMP
	* `uploadable`:		varchar		java.sql.Types.VARCHAR
	* `image`:			varchar		java.sql.Types.VARCHAR

	uploadable is as string but points to an uploaded file; image is as
	uploadable but points to an uploadable graphical image file."
  #{"string", "integer", "real", "money", "date", "time", "timestamp", "uploadable"})

(def simple-data-types (union
                        defineable-data-types
                        #{"boolean" "text"}))

(def complex-data-types #{"entity", "link", "list", "defined"})

(def special-data-types #{"geopos", "image", "message"})

(def all-data-types (union
                     simple-data-types
                     complex-data-types
                     special-data-types))

(def content #{"head", "top", "foot"})

(def field-stuff #{"field", "fieldgroup", "auxlist", "verb"})

(def page-content (union content field-stuff))

(def page-stuff (union page-content #{"permission", "pragma"}))

(def generator-actions #{"assigned", "guid", "manual", "native"})

(def sequences #{"canonical", "reverse-canonical"})

(def specification-validations
  {:tag [v/required [#(= % :specification)]]})

(def documentation-validations
  "contains documentation on the element which immediately contains it. TODO:
  should HTML markup within a documentation element be allowed? If so, are
  there restrictions?"
  {:tag [v/required [#(= % :documentation)]]})

(def content-validations
  {:tag [v/required [#(= % :content)]]})

(def help-validations
	"helptext about a property of an entity, or a field of a page, form or
	list, or a typedef. Typically there will be only one of these per property
  per locale; if there are more than one all those matching the locale may
  be concatenated, or just one may be used.

	* `locale`:			the locale in which to prefer this prompt"
  {:tag [v/required [#(= % :help)]]
   [:attrs :locale] [v/string v/required [v/matches #"[a-z]{2}-[A-Z]{2}"]]})

(def ifmissing-validations
  "helpful text to be shown if a property value is missing, typically when
  a form is submitted. Typically there will be only one of these per property
  per locale; if there are more than one all those matching the locale may
  be concatenated, or just one may be used. Later there may be more sophisticated
  behaviour here.

	* `locale`:			the locale in which to prefer this prompt"
  {:tag [v/required [#(= % :if-missing)]]
   [:attrs :locale] [v/string v/required [v/matches #"[a-z]{2}-[A-Z]{2}"]]})

(def param-validations
  "A parameter passed to the generator. Again, based on the Hibernate
  implementation.

  * `name`:   the name of this parameter."
  {:tag [v/required [#(= % :param)]]
   [:attrs :name] [v/string v/required]})


(def permission-validations
  "permissions policy on an entity, a page, form, list or field

	* `group`: 			the group to which permission is granted
	* `permission`:		the permission which is granted to that group."
  {:tag [v/required [#(= % :permission)]]
   [:attrs :group] [v/string v/required] ;; TODO: and it must be the name of a group that has already been defined.
   [:attrs :permission] [v/required [v/matches permissions]]})


(def prompt-validations
	"a prompt for a property or field; used as the prompt text for a widget
	which edits it. Typically there will be only one of these per property
  per locale; if there are more than one all those matching the locale may
  be concatenated, or just one may be used.

	* `prompt`:			the prompt to use
	* `locale`:			the locale in which to prefer this prompt."
  {:tag [v/required [#(= % :prompt)]]
   [:attrs :prompt] [v/string v/required]
   [:attrs :locale] [v/string v/required [v/matches #"[a-z]{2}-[A-Z]{2}"]]})

(def ifmissing-validations
	"helpful text to be shown if a property value is missing, typically when
  a form is submitted. Typically there will be only one of these per property
  per locale; if there are more than one all those matching the locale may
  be concatenated, or just one may be used. Later there may be more sophisticated
  behaviour here.

	* `locale`:			the locale in which to prefer this prompt."
  {:tag [v/required [#(= % :ifmissing)]]
   [:attrs :locale] [v/string v/required [v/matches #"[a-z]{2}-[A-Z]{2}"]]})

(def option-validations
  "one of an explicit list of optional values a property may have
  NOTE: whether options get encoded at application layer or at database layer
  is UNDEFINED; either behaviour is correct. If at database layer it's also
  UNDEFINED whether they're encoded as a single reference data table or as
  separate reference data tables for each property.

  * `value`:	the value of this option."
  {:tag [v/required [#(= % :option)]]
   [:attrs :value] [v/required]
   :content [[v/every #(or
                        (b/validate % documentation-validations)
                        (b/validate % prompt-validations))]]})

(def pragma-validations
  "pragmatic advice to generators of lists and forms, in the form of
  name/value pairs which may contain anything. Over time some pragmas
  will become 'well known', but the whole point of having a pragma
  architecture is that it is extensible."
  {:tag [v/required [#(= % :pragma)]]
   [:attrs :name] [v/string v/required]
   [:attrs :value] [v/string v/required]})



(def generator-validations
  "marks a property which is auto-generated by some part of the system.
  This is based on the Hibernate construct, except that the Hibernate
  implementation folds both its internal generators and custom generators
  onto the same attribute. This separates them onto two attributes so we
  can police values for Hibernate's 'builtin' generators.

  * `action`:       one of the supported Hibernate builtin generators, or
                'manual'. 'native' is strongly recommended in most instances
  * `class`:        if action is 'manual', the name of a manually maintained
                class conforming to the Hibernate IdentifierGenerator
                interface, or its equivalent in other languages."
  {:tag [v/required [#(= % :generator)]]
   [:attrs :action] [v/string v/required [v/member generator-actions]]
   [:attrs :class] v/string
   :content [[v/every #(or
                        (b/validate % documentation-validations)
                        (b/validate % param-validations))]]})


(def in-implementation-validations
  "information about how to translate a type into types known to different target
  languages. TODO: Once again I'm not wholly comfortable with the name; I'm not
  really comfortable that this belongs in ADL at all.

  * `target`:     the target language
  * `value`:      the type to use in that target language
  * `kind`:       OK, I confess I don't understand this, but Andrew needs it... "

  {:tag [v/required [#(= % :in-implementation)]]
   [:attrs :target] [v/string v/required]
   [:attrs :value] [v/string v/required]
   [:attrs :kind] v/string
   :content [[v/every documentation-validations]]})

(def typedef-validations
  "the definition of a defined type. At this stage a defined type is either
	* a string		in which case it must have size and pattern, or
	* a scalar		in which case it must have minimum and/or maximum
	pattern must be a regular expression as interpreted by org.apache.regexp.RE
	minimum and maximum must be of appropriate format for the datatype specified.
	Validation may be done client-side and/or server-side at application layer
	and/or server side at database layer.

  * `name`:     the name of this typedef
  * `type`:     the simple type on which this defined type is based; must be
            present unless in-implementation children are supplied
  * `size`:     the data size of this defined type
  * `pattern`:  a regular expression which values for this type must match
  * `minimum`:  the minimum value for this type (if base type is scalar)
  * `maximum`:  the maximum value for this type (if base type is scalar)"
  {:tag [v/required [#(= % :typedef)]]
   [:attrs :name] [v/required v/string]
   [:attrs :type] [[v/member defineable-data-types]]
   [:attrs :size] [[#(or
                      (integer? %)
                      (integer? (read-string %)))]]
   [:attrs :pattern] v/string
   [:attrs :minimum] [[#(or
                      (integer? %)
                      (integer? (read-string %)))]]
   [:attrs :maximum] [[#(or
                      (integer? %)
                      (integer? (read-string %)))]]
   :content [[v/every #(or
                        (b/validate % documentation-validations)
                        (b/validate % in-implementation-validations)
                        (b/validate % help-validations))]]})

(def group-validations
  "a group of people with similar permissions to one another

  * `name`: the name of this group
  * `parent`: the name of a group of which this group is subset"
  {:tag [v/required [#(= % :group)]]
   [:attrs :name] [v/string v/required]
   [:attrs :parent] v/string
   :content [[v/every documentation-validations]]})

(def property-validations
	"a property (field) of an entity (table)

	* `name`:			  the name of this property.
	* `type`:			  the type of this property.
	* `default`:		the default value of this property. There will probably be
					    magic values of this!
	* `typedef`:	  name of the typedef to use, it type = 'defined'.
	* `distinct`:		distinct='system' required that every value in the system
					    will be distinct (i.e. natural primary key);
					    distinct='user' implies that the value may be used by users
					    in distinguishing entities even if values are not formally
					    unique;
					    distinct='all' implies that the values are formally unique
					    /and/ are user friendly (NOTE: not implemented).
	* `entity`:	if type='entity', the name of the entity this property is
					    a foreign key link to.
              if type='list', the name of the entity that has a foreign
              key link to this entity
	* `farkey`:   if type='list', the name of farside key in the listed
              entity; if type='entity' and the farside field to join to
              is not the farside primary key, then the name of that
              farside field
	* `required`:		whether this propery is required (i.e. 'not null').
	* `immutable`:		if true, once a value has been set it cannot be changed.
	* `size`: 			fieldwidth of the property if specified.
	* `concrete`: if set to 'false', this property is not stored in the
              database but must be computed (manually written code must
              be provided to support this)
	* `cascade`:  what action(s) on the parent entity should be cascaded to
              entitie(s) linked on this property. Valid only if type='entity',
              type='link' or type='list'.
	* `column`:   name of the column in a SQL database table in which this property
              is stored. TODO: Think about this.
	* `unsaved-value`:
              of a property whose persistent value is set on first being
              committed to persistent store, the value which it holds before
              it has been committed"
  {:tag [v/required [#(= % :property)]]
  [:attrs :name] [v/required v/string]
   [:attrs :type] [v/required [v/member all-data-types]]
   ;; [:attrs :default] []
   [:attrs :typedef] v/string
   [:attrs :distinct] [v/string [v/member #{"none", "all", "user", "system"}]]
   [:attrs :entity] v/string
   [:attrs :farkey] v/string
   [:attrs :required] v/boolean
   [:attrs :immutable] v/boolean
   [:attrs :size] [[#(or (integer? %)(integer? (read-string %)))]]
   [:attrs :column] v/string
   [:attrs :concrete] v/boolean
   [:attrs :cascade] [[v/member cascade-actions]]
   :content [[v/every #(or
                        (b/validate % documentation-validations)
                        (b/validate % generator-validations)
                        (b/validate % permission-validations)
                        (b/validate % option-validations)
                        (b/validate % prompt-validations)
                        (b/validate % help-validations)
                        (b/validate % ifmissing-validations))]]})


(def permission-validations
  {:tag [v/required [#(= % :permission)]]})

(def form-validations
  {:tag [v/required [#(= % :form)]]})

(def page-validations
  {:tag [v/required [#(= % :page)]]})

(def list-validations
  {:tag [v/required [#(= % :list)]]})

;; (def prompt-validations
;;   {:tag [v/required [#(= % :prompt)]]})

(def key-validations
  {:tag [v/required [#(= % :key)]]
   :content [[v/every property-validations]]})


(def entity-validations
  "an entity which has properties and relationships; maps onto a database
	table or a Java serialisable class - or, of course, various other things

  * `name`:         obviously, the name of this entity
  * `natural-key`:  if present, the name of a property of this entity which forms
                a natural primary key [NOTE: Only partly implemented. NOTE: much of
                the present implementation assumes all primary keys will be
                integers. This needs to be fixed!] DEPRECATED: remove; replace with the
                'key' element, below.
  * `table`:        the name of the table in which this entity is stored. Defaults to same
                as name of entity. Strongly recommend this is not used unless it needs
                to be different from the name of the entity
  * `foreign`:      this entity is part of some other system; no code will be generated
                for it, although code which links to it will be generated"
  {:tag [v/required [#(= % :entity)]]
   [:attrs :name] [v/required v/string]
   [:attrs :natural-key] v/string
   [:attrs :table] v/string
   [:attrs :foreign] v/boolean
   :content [[v/every #(or
                        (b/validate % documentation-validations)
                        (b/validate % prompt-validations)
                        (b/validate % content-validations)
                        (b/validate % key-validations)
                        (b/validate % property-validations)
                        (b/validate % permission-validations)
                        (b/validate % form-validations)
                        (b/validate % page-validations)
                        (b/validate % list-validations)
                        )]]})

(def application-validations
  {:tag [v/required [#(= % :application)]]
   [:attrs :name] [v/required v/string]
   [:attrs :version] v/string
   [:attrs :revision] v/string
   [:attrs :currency] v/string
   :content [[v/every #(or
                        (b/validate % specification-validations)
                        (b/validate % documentation-validations)
                        (b/validate % content-validations)
                        (b/validate % typedef-validations)
                        (b/validate % group-validations)
                        (b/validate % entity-validations))]]})



