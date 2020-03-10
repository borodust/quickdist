(asdf:defsystem :quickdist
  :description "Create a Quicklisp distribution from a directory of local projects."
  :author "Orivej Desh <orivej@gmx.fr>, Pavel Korolev <dev@borodust.org"
  :mailto "dev@borodust.org"
  :licence "Unlicense <http://unlicense.org/UNLICENSE>"
  :depends-on (:alexandria :uiop :cl-fad
               :babel-streams :inferior-shell :ironclad :cl-ppcre
               :split-sequence :cl-mustache)

  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "template")
               (:file "dist")))
