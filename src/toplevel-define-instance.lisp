(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-INSTANCE.

(defun process-toplevel-instance-definitions (definstance-forms package env)
  (declare (values environment instance-definition-list))

  (let ((instances nil))
    (loop :for form :in definstance-forms
          :do (multiple-value-bind (env-new instances-new)
                  (parse-instance-definition form package env)
                (setf env env-new)
                (alexandria:appendf instances instances-new)))
    (values env instances)))

(defun predeclare-toplevel-instance-definitions (definstance-forms package env)
  "Predeclare all instance definitions in the environment so values can be typechecked"
  (declare (type list definstance-forms)
           (type package package)
           (type environment env)
           (values environment))

  (loop :for form :in definstance-forms
        :do (multiple-value-bind (predicate context methods)
                (coalton-impl/typechecker::parse-instance-decleration form env)
              (declare (ignore methods))
              (setf env (add-instance-definition predicate context package env))))
  env)
