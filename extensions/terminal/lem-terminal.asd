;;;; lem-terminal.asd --- System definition of Lem Terminal mode

(defsystem "lem-terminal"
  :depends-on ("lem" "3bst")
  :serial t
  :components ((:file "terminal-mode")))

;;;; lem-terminal.asd ends here