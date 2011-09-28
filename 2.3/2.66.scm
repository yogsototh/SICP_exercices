(load "displaylib.scm")
(title "Exercise 2.66")
(print "
       Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys. 
       ")

(define (lookup given-key set-of-records)
  (if (null? set-of-records) 
    false
    (let ((root-key ((key root set-of-records)))) 
      (cond ((= given-key root-key) (root set-of-records)) 
            ((< given-key root-key) 
             (lookup given-key (left-tree set-of-records)))
            ((> given-key root-key) 
             (lookup given-key (right-tree set-of-records)))))))

