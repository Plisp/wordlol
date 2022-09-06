(defun read-file-lines (s)
  (loop for line = (read-line s nil)
        while line
        collect (coerce line 'simple-base-string)))

(defparameter words
  (with-open-file (s "answers.txt")
    (coerce (read-file-lines s) 'simple-vector)))

(defparameter guesslist
  (with-open-file (g "guesses.txt")
    (concatenate 'simple-vector words (read-file-lines g))))

(defparameter first-guess (coerce "salet" 'simple-base-string))

(defun colors (word guess result matched)
  (declare (optimize speed)
           (type simple-base-string word guess result)
           (type (simple-array boolean) matched))
  (fill result #\b)
  (loop for i fixnum below 5
        do (when (char= (schar word i)
                        (schar guess i))
             (setf (aref matched i) t
                   (aref result i) #\g)))

  (loop for gi below 5
        for c = (schar guess gi)
        do (when (and (not (aref matched gi))
                      (loop for wi below 5
                            when (and (char= c (schar word wi))
                                      (not (aref matched wi)))
                              do (setf (aref matched wi) t)
                                 (return t)))
             (setf (aref result gi) #\o)))
  (fill matched nil)
  result)

(defun guess (candidates colors matched)
  (declare (optimize speed)
           (type simple-base-string colors)
           (type (simple-array boolean) matched)
           (type simple-vector candidates guesslist))
  (loop initially (when (<= (length candidates) 2)
                    (setf best-guess (aref candidates 0))
                    (loop-finish))
        with best-avg = most-positive-double-float
        with best-guess
        for guess across guesslist ;; too many. Which guesses are bad
        do (let ((hash (make-hash-table :test #'equal)))
             (loop for word across candidates
                   do (colors word guess colors matched)
                      (let ((colors (copy-seq colors)))
                        (if (gethash colors hash)
                            (incf (gethash colors hash))
                            (setf (gethash colors hash) 1))))
             (let ((sum 0))
               (maphash (lambda (k v) (declare (ignore k)) (incf sum v)) hash)
               (let ((avg (/ sum (hash-table-count hash))))
                 (when (< avg best-avg)
                   (setf best-avg avg
                         best-guess guess)))))
        finally (return (values best-guess best-avg))))

(defun play (answer)
  (declare (optimize speed)
           (type simple-base-string answer))
  (loop with guesses = (list)
        with remaining simple-vector = (copy-seq words)
        with answer-colors = (make-array 5 :element-type 'base-char)
        with colors = (make-array 5 :element-type 'base-char)
        with matched = (make-array 5 :element-type 'boolean :initial-element nil)
        for guess = first-guess then (guess remaining colors matched)
        do (push guess guesses)
           (colors answer guess answer-colors matched)
           (when (string= answer-colors "ggggg")
             (return-from play (print (nreverse guesses))))
           (loop with count = 0
                 for index from 0 below (length remaining)
                 do (let ((word (aref remaining index)))
                      (colors word guess colors matched)
                      (when (string= answer-colors colors)
                        (setf (aref remaining count) word)
                        (incf count)))
                 finally (setf remaining (adjust-array remaining count)))))

(defun main ()
  (with-open-file (s "results.txt" :direction :output :if-does-not-exist :create)
    (loop for guesses in (map 'list #'play words)
          do (format s "~{~a~^,~}~%" guesses))))

#|
(defun starting-letter-frequencies ()
  (loop with a = (map 'list #'list "abcdefghijklmnopqrstuvwxyz")
        for word across words
        for assoc = (position (schar word 0) a :key 'car)
        do (setf (nth assoc a) (append (nth assoc a) (list word)))
        finally (return (sort a '< :key (lambda (entry)
                                          (length (cdr entry)))))))

(defun matches (candidate guess result greens)
  (declare (optimize speed)
           (type simple-base-string candidate guess result)
           (type (simple-array boolean) greens))

  (loop for x base-char across result
        for i fixnum from 0
        do (when (char= x #\g)
             (if (char= (schar candidate i)
                        (schar guess i))
                 (setf (aref greens i) t)
                 (return-from matches nil))))

  (loop for x base-char across result
        for gi below 5
        do (case x
             (#\o
              (or (loop for ci below 5
                        thereis (and (/= gi ci)
                                     (char= (schar candidate ci)
                                            (schar guess gi))
                                     (not (aref greens ci))))
                  (return-from matches nil)))
             (#\b
              (or (loop for ci below 5
                        never (and (char= (schar candidate ci)
                                          (schar guess gi))
                                   (not (aref greens ci))))
                  (return-from matches nil)))))
  t)

(defun frequencies ()
  (sort
   (loop for letter across "abcdqefghwijkvlxmnzoprstuy"
         for count = (loop for str across words
                           count (find letter str))
         collect (cons letter count))
   #'< :key #'cdr))

(defun duplicate-characters ()
  (loop for word across words
        for length = (length word)
        for deduplicated-length = (length (remove-duplicates word))
        if (>= (- length deduplicated-length) 3)
          do (format t "over 3 duplicates: ~s~%" word)
        count (/= length deduplicated-length)))

(defun green-info ()
  (loop with result
        for i below 5
        do (loop for c across "abcdqefghwijkvlxmnzoprstuy"
                 for count = (count-if (lambda (w) (eql c (char w i)))
                                           words)
                 do (push (list count c i) result))
        finally (return (sort result '< :key 'car))))


(defun find-most-covering-first-guess ()
  "find the first guess that will provide at least one clue for the most words. Note this is
 not actually the best guess to use, considering possible orthogonal second guesses."
  (loop
    with max-cands = 0
    with best-guess
    for guess across words
    for count = (count-if (lambda (w)
                            (loop for c across w
                                  thereis (find c guess)))
                          words)
    do (when (> count max-cands)
         (setf max-cands count
               best-guess guess)
         (:printv max-cands best-guess))))

(defun find-most-covering-first-two-guesses ()
  (loop
    with result = (coerce "rrrrr" 'simple-base-string)
    with min-cands = (length words)
    with min-guesses
    with greens = (make-array 5 :element-type 'boolean :initial-element nil)
    for start from 0 below (length words)
    for guess1 across words
    do (loop
         for i from (1+ start) below (length words)
         for guess2 = (svref words i)
         do (loop with count = 0
                  for candidate across words
                  do (when (and (matches candidate guess1 result greens)
                                (matches candidate guess2 result greens))
                       (incf count))
                     (when (> count min-cands)
                       (return))
                  finally (setf min-cands count
                                min-guesses (cons guess1 guess2))
                          (:printv min-cands min-guesses)
                  ))))

(defparameter *smallest* (cons (length words) nil)) ; car can be CASed

(defun precompute-guess (guesslist wordlist)
  (declare (optimize speed))
  (loop with worst-case-left fixnum = (length (the vector wordlist))
        with best-guess = ""
        with greens = (make-array 5 :element-type 'boolean :initial-element nil)
        with result = (make-array 5 :element-type 'base-char :initial-element #\b)
        for guess across (the simple-vector guesslist)
        do (:printv guess) (force-output)
           (loop :named check-guesses
                 with max-cands-left fixnum = 0
                 with matching fixnum
                 for actual simple-base-string across (the simple-vector wordlist)
                 do (colors actual guess result greens) ; evaluation stored in result
                    (setf matching (loop with count fixnum = 0
                                         for candidate across (the simple-vector wordlist)
                                         do (fill greens nil)
                                            (when (matches candidate guess result greens)
                                              (incf count))
                                            (when (= count worst-case-left) ; cant be better
                                              (return-from check-guesses))
                                         finally (return count)))
                    (fill result #\b)
                    (fill greens nil)
                    (when (> matching max-cands-left)
                      (setf max-cands-left matching))
                 finally (:printv max-cands-left)
                         ;;(when (< max-cands-left worst-case-left)) must be
                         (setf worst-case-left max-cands-left
                               best-guess guess))
        finally (return (cons best-guess worst-case-left))))
|#
