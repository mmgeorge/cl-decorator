#-asdf3.3 (error "Building requires asdf 3.3 but ~a is installed" (asdf:asdf-version))

(asdf:defsystem "decorator"
  :class :package-inferred-system
  :pathname "core"
  :depends-on ("decorator/exports"))
