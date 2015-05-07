(in-package :cs400-compiler)

(defmacro expand (code)
  `(let ((*package* (find-package :cs400-compiler)))
     (print (macroexpand ',code))))
