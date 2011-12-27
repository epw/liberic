#lang racket/base

(require racket/async-channel)

(define-syntax-rule (let-concurrent ((id expr) ...) body ...)
  (with-syntax (((channel ...) (generate-temporaries #'((car (list id ...))
		((result ...) (generate-temporaries #'(expr ...))))
    #'(list (list channel ...) (list result ...))))


  (let ((id (let ((channel (make-async-channel 1)))
	      (thread
	       (lambda ()
		 (async-channel-put channel expr))
				   
    body
    ...))
