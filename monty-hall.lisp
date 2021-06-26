(defpackage #:monty-hall
   (:use #:cl #:arrow-macros))

(in-package #:monty-hall)

(defun list-of-integers-p (list)
  "Return t if LIST is non nil and contains only strings."
  (and (consp list)
       (every #'integerp list)))

(deftype list-of-integers ()
  `(satisfies list-of-integers-p))

(deftype list-of ()
  `(satisfies consp))

(declaim (ftype (function (integer list-of-integers) integer) random-with-exclusions))
(defun random-with-exclusions (n exclusions)
  "Returns a random number within the range that isn't in the list"
  (let ((random-number (-> n random (+ 1))))
    (if (member random-number exclusions)
        (random-with-exclusions n exclusions)
        random-number)))

(declaim (ftype (function (integer boolean) t) play))
(defun play (num-doors swap-choice)
  "Play the Monty Hall game"
  (let* (;; 1. Initialize with car randomly placed somewhere
         (car-door (-> num-doors random (+ 1)))
         ;; 2. Guest picks a door (simulate randomly missing the car)
         (guest-door-choice (-> num-doors random (+ 1)))
         ;; 3. Host picks a door (chooses a door with a goat)
         (host-goat-door-choice (random-with-exclusions num-doors `(,car-door ,guest-door-choice)))
         ;; Host points to another door that the guest can swap
         (host-swap-door-choice
           (if (= car-door guest-door-choice)
               ;; If the guest chose the car door, then the host chooses another
               ;; random door ith a goat
               (random-with-exclusions num-doors `(,guest-door-choice ,host-goat-door-choice))
               ;; Otherwise, choose the car door (guest doesn't know)
               car-door)))
    (if swap-choice (= car-door host-swap-door-choice)
        (= car-door guest-door-choice))))

(declaim (ftype (function (hash-table t) hash-table) sort-into-buckets))
(defun sort-into-buckets (acc x)
  "Sorts a list of values into buckets with the tally"
  (let ((count (gethash x acc)))
    (setf (gethash x acc) (+ (or count 0) 1))
    acc))

(declaim (ftype (function (list-of) hash-table) score))
(defun score (list)
  "Get the total scores from a given list of rounds"
  (reduce #'sort-into-buckets list :initial-value (make-hash-table)))

(declaim (ftype (function (function list-of) list-of) sort-alist))
(defun sort-alist (f alist)
  "Sorts an alist by key given a function f"
  (stable-sort alist f :key #'car))

;; Simulate N rounds where guest won't swap vs will swap
(declaim (ftype (function (&key (:num-doors integer) (:switch-choice boolean) (:num-rounds integer)) list-of) test))
(defun test (&key num-doors switch-choice (num-rounds 10000))
  (let ((rounds (make-list num-rounds)))
    (->>
      rounds
      (map 'list #'(lambda (_) (play num-doors switch-choice)))
      score
      alexandria:hash-table-alist
      (sort-alist #'(lambda (a b) (when a 1 0))))))

(declaim (ftype (function (integer &key (:num-rounds integer)) t) run))
(defun run (num-doors &key (num-rounds 10000))
  (print
   `(:keep ,(test :num-doors num-doors :switch-choice nil :num-rounds num-rounds)))
  (print
   `(:switch ,(test :num-doors num-doors :switch-choice t :num-rounds num-rounds)))
  (terpri))

(run 3 :num-rounds 1000000)
;;(:KEEP ((T . 334409) (NIL . 665591)))
;;(:SWITCH ((T . 666442) (NIL . 333558)))

(run 4 :num-rounds 1000000)
;;(:KEEP ((T . 250303) (NIL . 749697)))
;;(:SWITCH ((T . 749986) (NIL . 250014)))

(run 5 :num-rounds 1000000)
;;(:KEEP ((T . 200124) (NIL . 799876)))
;;(:SWITCH ((T . 799602) (NIL . 200398)))

(run 20 :num-rounds 1000000)
;;(:KEEP ((T . 49878) (NIL . 950122)))
;;(:SWITCH ((T . 949733) (NIL . 50267)))

(run 200 :num-rounds 1000000)
;;(:KEEP ((T . 4897) (NIL . 995103)))
;;(:SWITCH ((T . 994996) (NIL . 5004)))
