(use-modules (ice-9 rdelim))

(define (print-list size)
  (if (zero? size)
      (quote ())
      (cons size (print-list (- size 1)))))

;; debug the function.
(define (insert-test filename)
  (call-with-input-file filename
    (lambda (input-port)
      (let loop ((result '()))
	(let ((line (read input-port)))
	  (if (eof-object? line)
	      result
	      (loop (cons line result))))))))

;; resolve stubs
(define (retrieve-test filename)
  '())

(define (list-dump filename)
  (let ((flist (call-with-input-file filename
		 (lambda (input-port)
		   (let loop ((result '()))
		     (let ((line (read-line input-port)))
		       (if (eof-object? line)
			   result
			   (loop (cons line result)))))))))
    (string-join (reverse flist)
		 "->"
		 'prefix)))

(define (search-test filename)
  '())

(define (sort-test filename)
  '())
