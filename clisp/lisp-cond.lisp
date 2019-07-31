(defvar *age* 18)

(if (= *age* 18)
    (format t "You can vote~%")
    (format t "You cannot vote~%")
)

(if (and (>= *age* 14) (<= *age* 67))
    (format t "Time for work~%")
    (format t "Work if you want%d")
)

(defun get-school (age)
    (case age
        (5 (print "Kindergarten"))
        (6 (print "First Grade"))
        (otherwise (print "moddle school"))
     )
)

(setq *age* 6)
(get-school *age*)

(setq *age* 18)
(defvar *college-ready* nil)

(cond ( (> *age* 18)
            (setf *colleage-ready* 'yes)
            (format t "Ready for colleage ~%"))
      ( (< *age* 18)
            (setf *colleage-ready* 'no)
            (format t "Not Ready for colleage ~%"))
      (t (format t "Dont Know ~%"))  
)