(in-package #:coalton-impl/typechecker)

;;;
;;; Parsing type defintions
;;;


(defstruct type-definition
  (name              (required 'name)              :type symbol                 :read-only t)
  (type              (required 'type)              :type ty                     :read-only t)
  (runtime-type      (required 'runtime-type)      :type t                      :read-only t)

  ;; See the fields with the same name on type-entry
  (enum-repr         (required 'enum-repr)         :type boolean                :read-only t)
  (newtype           (required 'newtype)           :type boolean                :read-only t)

  (constructors      (required 'constructors)      :type constructor-entry-list :read-only t)
  (constructor-types (required 'constructor-types) :type scheme-list            :read-only t)

  (docstring         (required 'docstring)         :type (or null string)       :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-definition))

(defstruct (partial-define-type (:constructor partial-define-type))
  (name         (required 'name)         :type symbol           :read-only t)
  (tyvar-names  (required 'tyvar-names)  :type symbol-list      :read-only t)
  (constructors (required 'constructors) :type list             :read-only t)
  (docstring    (required 'docstring)    :type (or null string) :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type partial-define-type))

(defun partial-define-type-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'partial-define-type-p x)))

(deftype partial-define-type-list ()
  '(satisfies partial-define-type-list-p))

(defun type-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'type-definition-p x)))

(deftype type-definition-list ()
  '(satisfies type-definition-list-p))


(defun parse-type-definition (partial-type self-type type-vars ksubs env)
  (declare (type partial-define-type partial-type)
           (type ty self-type)
           (type list type-vars)
           (type ksubstitution-list ksubs)
           (type environment env)
           (values list list ksubstitution-list))

  (let* ((tyvar-names (partial-define-type-tyvar-names partial-type))

         (unparsed-ctors (partial-define-type-constructors partial-type))

         (local-type-vars
           (loop :for tyvar-name :in tyvar-names
                 :collect (list tyvar-name (make-variable (make-kvariable)))))

         (tyvars
           (append
            type-vars
            local-type-vars))

         (constructors
           (loop :for ctor :in unparsed-ctors
                 :for name := (first ctor)
                 :for fields := (rest ctor)
                 :collect (cons
                           name
                           (loop :for field :in fields
                                 :collect (multiple-value-bind (type new-ksubs)
                                              (parse-type-expr env field tyvars ksubs)
                                            (setf ksubs new-ksubs)
                                            (setf ksubs (kunify (kind-of type) kstar ksubs))
                                            type))))))

    ;; Unify the kind of the type with the kind:
    ;; kind(tvar1) -> kind(tvar2) ... -> *
    (setf ksubs (kunify
                 (kind-of self-type)
                 (make-kind-function*
                  (loop :for (name type) :in local-type-vars
                        :collect (kind-of type))
                  kstar)
                 ksubs))

    (values
     constructors
     local-type-vars
     ksubs)))

(defun parse-type-impls (partial-types env)
  "Parse the PARTIAL-TYPES in a single scc"
  (declare (type partial-define-type-list partial-types)
           (type environment env))

  (let* ((type-names (mapcar #'partial-define-type-name partial-types))

         (type-vars
           (loop :for type-name :in type-names
                 :collect (list type-name
                                (%make-tcon
                                 (%make-tycon
                                  :name type-name
                                  :kind (make-kvariable))))))

         (ksubs nil)

         (type-constructors
           (loop :for partial-type :in partial-types
                 :for self-type := (second (find (partial-define-type-name partial-type) type-vars :key #'first))
                 :collect (multiple-value-bind (constructors local-tyvars new-ksubs)
                              (parse-type-definition partial-type self-type type-vars ksubs env)
                            (setf ksubs new-ksubs)
                            (cons constructors local-tyvars)))))

    (values
     (loop :for partial-type :in partial-types
           :for (name type) :in type-vars
           :for (ctors . local-tyvars) :in type-constructors

           :for type_ := (apply-ksubstitution ksubs type)
           :for ksubs_ := (kind-monomorphize-subs (kind-variables type_) ksubs)
           :for type__ := (apply-ksubstitution ksubs_ type)

           :for tyvar-types
             := (loop :for (name tyvar) :in local-tyvars
                      :collect (apply-ksubstitution ksubs_ tyvar))

           :for applied-type
             := (apply-type-argument-list
                 type__
                 tyvar-types)

           ;; Declare the current type in the env. This decleration is incomplete, and is discarded away after parsing
           :do (setf
                env
                (set-type
                 env
                 name 
                 (type-entry
                  :name name
                  :runtime-type name
                  :type type__
                  :enum-repr nil
                  :newtype nil
                  :docstring nil)))

           :collect (list
                     name
                     type__
                     (loop :for (ctor-name . args) :in ctors
                           :for arity := (length args)
                           :for classname := (alexandria:format-symbol
                                              (symbol-package name)
                                              "~A/~A" name ctor-name)
                           :for args_ := (apply-ksubstitution ksubs_ args)
                           :for type := (make-function-type* args_ applied-type)
                           :for scheme := (quantify-using-tvar-order (mapcar #'tvar-tyvar tyvar-types) (qualify nil type))
                           :collect
                           (cons
                            scheme
                            (make-constructor-entry
                             :name ctor-name
                             :arity arity
                             :constructs name
                             :classname classname
                             :compressed-repr nil)))))
     env)))


(defun parse-type-definitions (forms repr-table env)
  "Parse the type defintion FORM in the ENVironment

Returns TYPE-DEFINITIONS"
  (declare (type list forms)
           (type environment env)
           (values type-definition-list))

  ;; Pull out and verify DEFINE-TYPE and type
  (let ((type-definitions nil) ; list (name tvars constructors docstring)
        (type-dependencies)    ; list (name dependencies*)
        )
    (dolist (form forms)
      (assert (and (eql 'coalton:define-type (first form))
                   (<= 2 (length form))
                   (or (listp (second form))
                       (symbolp (second form))))
          () "Malformed DEFINE-TYPE form ~A" form)
      (destructuring-bind (def-type type &rest ctors) form
        (declare (ignore def-type))
        ;; Pull bare symbols into a list for easier parsing
        (setf type (alexandria:ensure-list type))

        ;; Pull out the type name and type variables
        (destructuring-bind (tycon-name &rest tyvar-names) type
          (assert (and (symbolp tycon-name)
                       (every #'symbolp tyvar-names))
              () "Malformed DEFINE-TYPE type ~A" type)

          ;; Push this tycon onto the list
          (let (;; If the first ctor is a string then it is the docstring and we should skip it.
                (constructors
                  (mapcar #'alexandria:ensure-list
                          (if (stringp (car ctors))
                              (cdr ctors)
                              ctors)))

                ;; Pull out the docstring if it exists
                (docstring
                  (when (stringp (car ctors))
                    (car ctors))))

            (with-parsing-context ("definition of type ~A" tycon-name)
              ;; Check for invalid type variables
              (unless (every (lambda (var)
                               (equalp (symbol-package var)
                                       keyword-package))
                             tyvar-names)
                (error-parsing form "type variables must all be in the KEYWORD package."))

              ;; Check for duplicate constructors
              (labels ((check-for-duplicate-constructors (ctors)
                         (if (find (car (car ctors)) (rest ctors) :key #'car :test #'equalp)
                             (error-parsing form "duplicate constructor ~A" (car (car ctors)))
                             (when (rest ctors)
                               (check-for-duplicate-constructors (rest ctors))))))
                (check-for-duplicate-constructors constructors))

              ;; Check for duplicate type variables
              (labels ((check-for-duplicate-type-variables (tyvar-names)
                         (if (find (car tyvar-names) (rest tyvar-names) :test #'equalp)
                             (error-parsing form "duplicate type variable ~S" (car tyvar-names))
                             (when (rest tyvar-names) 
                               (check-for-duplicate-type-variables (rest tyvar-names))))))
                (check-for-duplicate-type-variables tyvar-names))

              ;; Check for type variables appearing in constructors but not in the type
              (loop :for (ctor-name . fields) :in constructors
                    :do (loop :for field :in fields
                              :for field-tyvars := (collect-type-vars field)
                              :do (loop :for field-tyvar :in field-tyvars
                                        :do (unless (find field-tyvar tyvar-names :test #'equalp)
                                              (error-parsing
                                               form
                                               "type variable ~S appears in constructor ~A but not in type"
                                               field-tyvar
                                               ctor-name))))))

            (push
             (partial-define-type
              :name tycon-name
              :tyvar-names tyvar-names
              :constructors constructors
              :docstring docstring)
             type-definitions)

            (push
             (cons
              tycon-name
              (remove-duplicates
               (loop :for (name . args) :in constructors
                     :append (collect-types args))
               :test #'equalp))
             type-dependencies)))))


    (let* ((translation-unit-types (mapcar #'car type-dependencies))

           (type-dependencies
             (loop :for (name . deps) :in type-dependencies
                   :collect (cons name (intersection deps translation-unit-types))))

           (parsed-tcons
             (loop :for scc :in (reverse (tarjan-scc type-dependencies))
                   :for partial-types
                     := (loop :for type-name :in scc
                              :collect (find type-name type-definitions :test #'equalp :key #'partial-define-type-name))
                   :append (multiple-value-bind (data new-env)
                               (parse-type-impls partial-types env)
                             (setf env new-env)
                             data))))

      (loop :for (tycon-name tcon ctor-data docstring) :in parsed-tcons
            :collect
            (with-parsing-context ("definition of type ~A" tycon-name)
              
              ;; Parse out the ctors
              (let* ((parsed-ctors (mapcar #'cdr ctor-data))

                     (ctor-types (mapcar #'car ctor-data))

                     ;; If every constructor entry has an arity of 0
                     ;; then this type can be compiled as an enum
                     (enum-type (every (lambda (ctor)
                                         (= 0 (constructor-entry-arity ctor)))
                                       parsed-ctors))

                     ;; If there is a single constructor with a single
                     ;; field then this type can be compiled as a
                     ;; newtype
                     (newtype (and (= 1 (length parsed-ctors))
                                   (= 1 (constructor-entry-arity (first parsed-ctors)))))

                     (repr (car (gethash tycon-name repr-table)))
                     (repr-arg (cdr (gethash tycon-name repr-table))))
                (cond
                  ;; If the type is repr lisp then do *not* attempt to
                  ;; generate an optimized implementation
                  ((eql repr :lisp)
                   (make-type-definition
                    :name tycon-name
                    :type tcon
                    :runtime-type tycon-name
                    :enum-repr nil
                    :newtype nil
                    :constructors parsed-ctors
                    :constructor-types ctor-types
                    :docstring docstring))

                  ((eql repr :native)
                   (progn
                     (unless repr-arg
                       (error "Type ~A cannot have native repr of NIL" tycon-name)))
                   (make-type-definition
                    :name tycon-name
                    :type tcon
                    :runtime-type repr-arg
                    :enum-repr nil
                    :newtype nil
                    :constructors parsed-ctors
                    :constructor-types ctor-types
                    :docstring docstring))

                  ((or (and newtype (eql repr :transparent))
                       (and newtype (coalton-impl:coalton-release-p)))
                   (let (;; The runtime type of a newtype is the runtime type of it's only constructor's only argument
                         (runtime-type (function-type-from
                                        (qualified-ty-type
                                         (fresh-inst (first ctor-types))))))
                     (make-type-definition
                      :name tycon-name
                      :type tcon
                      :runtime-type runtime-type
                      :enum-repr nil
                      :newtype t
                      :constructors parsed-ctors
                      :constructor-types ctor-types
                      :docstring docstring)))

                  ((and (eql repr :transparent) (not newtype))
                   (error "Type ~A cannot be repr transparent. To be repr transparent a type must have a single constructor with a single field." tycon-name))

                  ((or (and enum-type (eql repr :enum))
                       (and enum-type (coalton-impl:coalton-release-p)))
                   (let ((parsed-ctors (mapcar #'rewrite-ctor parsed-ctors)))
                     (make-type-definition
                      :name tycon-name
                      :type tcon
                      :runtime-type `(member ,@(mapcar #'constructor-entry-compressed-repr parsed-ctors))
                      :enum-repr t
                      :newtype nil
                      :constructors parsed-ctors
                      :constructor-types ctor-types
                      :docstring docstring)))

                  ((and (eql repr :enum) (not enum-type))
                   (error "Type ~A cannot be repr enum. To be repr enum a type must only have constructors without fields." tycon-name))


                  (t
                   (make-type-definition
                    :name tycon-name
                    :type tcon
                    :runtime-type tycon-name
                    :enum-repr nil
                    :newtype nil
                    :constructors parsed-ctors
                    :constructor-types ctor-types
                    :docstring docstring)))))))))

(defun rewrite-ctor (ctor)
  (assert (= 0 (constructor-entry-arity ctor)))
  (make-constructor-entry
   :name (constructor-entry-name ctor)
   :arity (constructor-entry-arity ctor)
   :constructs (constructor-entry-constructs ctor)
   :classname (constructor-entry-classname ctor)
   :compressed-repr (constructor-entry-classname ctor)))

(defun quantify-using-tvar-order (tyvars type)
  (let* ((vars (remove-if
                (lambda (x) (not (find x (type-variables type) :test #'equalp)))
                tyvars))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (%make-substitution var (%make-tgen id)))))
    (%make-ty-scheme kinds (apply-substitution subst type))))

(defun tvar-count-to-kind (tvar-count)
  "Create a KIND from the number of type variables"
  (if (= 0 tvar-count)
      kStar
      (kFun kStar (tvar-count-to-kind (1- tvar-count)))))
