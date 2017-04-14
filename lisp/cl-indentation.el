(setq lisp-indent-function #'common-lisp-indent-function)

(cl-macrolet ((my/indent (&rest indentations)
		`(progn
		   ,@(cl-loop for (sym indent elisp) in indentations
			for type = (if elisp
				       'common-lisp-indent-function-for-elisp
				     'common-lisp-indent-function)
			collect
			  `(put ',sym
				',type
				,(if (symbolp indent)
				     `(get ',indent ',type)
				   indent))))))
  (my/indent
   (iter `(,lisp-body-indent &body))
   (iterate iter)
   (in '(&lambda &body))
   (defcommand '(4 &lambda &lambda &body))
   (defkeys `(,lisp-body-indent &body) t)))
