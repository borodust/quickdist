(asdf:defsystem quickdist
  :description "Create a Quicklisp distribution from a directory of local projects."
  :author "Orivej Desh <orivej@gmx.fr>, Pavel Korolev <dev@borodust.org"
  :mailto "dev@borodust.org"
  :licence "Unlicense <http://unlicense.org/UNLICENSE>"
  :depends-on (alexandria cl-fad babel-streams inferior-shell ironclad
               cl-ppcre quicklisp split-sequence)
  :serial t
  :components ((:file "package")
               (:file "quickdist")))
