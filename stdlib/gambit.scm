    
(define printf
  (lambda (format . args)
    (let ((len (string-length format)))
      (let loop ((i 0) (args args))
        (let ((output
                (lambda (fn)
                  (fn (car args))
                  (loop  (+ i 2) (cdr args))))
              (outputc
                (lambda (fn)
                  (fn)
                  (loop (+ i 2) args))))
          (if (>= i len) #t
            (let ((c (string-ref format i)))
              (if (char=? c #\~)
                (case (string-ref format (+ i 1))
                  ((#\s) (output write))
                  ((#\a) (output display))
                  ((#\c) (output write-char))
                  ((#\% #\n) (outputc newline))
                  ((#\~) (outputc (lambda () (write-char #\~))))
                  (else
                    (write
                      "error in eopl:printf: unknown format character ")
                    (write-char  (string-ref format (+ i 1)))
                    (write-char #\newline)
                    (error "WTF!")))
                (begin
                  (display c)
                  (loop (+ i 1) args))))))))))
                  
                  
(define (format . argz)
  (with-output-to-string
    (lambda()
      (apply printf argz))))
      
(define (tcp-listen-px prt)
  (open-tcp-server prt))
  
(define (tcp-accept-px lst)
  (read lst))

(define (tcp-connect-px host port)
  (open-tcp-client (list
                      server-address: host
                      port-number: port)))
  
(define (tcp-close-px lst)
  (close-port lst))
  
(define (make-bytes n)
  (make-u8vector n))
  
(define (read-bytes-avail bts port)
  (read-subu8vector bts 0 (u8vector-length bts) port 1))
  
(define (write-bytes bts port)
  (write-subu8vector bts 0 (u8vector-length bts) port))
  
(define (bytes-length x)
  (u8vector-length x))
  
(define bytes u8vector)
(define bytes? u8vector?)

(define bytes-ref u8vector-ref)
  
(define subbytes subu8vector)

(define bytes-append u8vector-append)

(define (bytes2string bts)
  (with-output-to-string (lambda() (write-bytes bts (current-output-port)))))
