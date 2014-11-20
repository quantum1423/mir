(include "/home/sawatari/Documents/Projects/racket/mirlang/./stdlib/gambit.scm")
(include "/home/sawatari/Documents/Projects/racket/mirlang/./stdlib/main.scm")

;; /home/sawatari/Documents/Projects/racket/mirlang/./stdlib/mir/io.mir
(define io::Printf printf)

(define io::Sprintf format)

(define io::Reader (mir-struct io::Read))

(define io::Writer (mir-struct io::Write))

(define io::ReadWriter (mir-struct io::Read io::Write))

(define io::ReadWriteCloser (mir-struct io::Read io::Write io::Close))

(define io::stdin_prt (call current-input-port))

(define io::stdout_prt (call current-output-port))

(define io::Stdio
  (let ()
    (define io::rbuf (call make-u8vector 4096))
    (define io::read
      (mir-fun
       (list io::n)
       (let ()
         (define io::num
           (call read-subu8vector io::rbuf 0 io::n io::stdin_prt 1))
         (call subu8vector io::rbuf 0 io::num))))
    (define io::write
      (mir-fun
       (list io::towr)
       (let ()
         (define io::throwaway159
           (call
            write-subu8vector
            io::towr
            0
            (call u8vector-length io::towr)
            io::stdout_prt))
         (call force-output io::stdout_prt))))
    (call io::ReadWriter io::read io::write)))

(define io::EOF EOF)

(define io::IsEOF (mir-fun (list io::x) (let () (call eof-object? io::x))))

(define io::CopyIO
  (mir-fun
   (list io::in io::out)
   (let ()
     (mir-loop
      io::loop
      (list)
      (let ()
        (define io::bts (call (call io::in 'Read) 4096))
        (mir-if
         (equal? io::bts io::EOF)
         (let () io::bts)
         else
         (let ()
           (define io::throwaway160 (call (call io::out 'Write) io::bts))
           (call io::loop))))))))

(define io::ReadFull
  (mir-fun
   (list io::rdr io::num)
   (let ()
     (mir-if
      (equal? io::num 0)
      (let () (call make-bytes 0))
      else
      (let ()
        (define io::thing (call (call io::rdr 'Read) io::num))
        (mir-if
         (call io::IsEOF io::thing)
         (let () (call raise "Unexpected EOF!"))
         else
         (mir-if
          (equal? (call bytes-length io::thing) io::num)
          (let () io::thing)
          else
          (let ()
            (call
             bytes-append
             io::thing
             (call
              io::ReadFull
              io::rdr
              (- io::num (call bytes-length io::thing))))))))))))


;; /home/sawatari/Documents/Projects/racket/mirlang/./stdlib/mir/net.mir
(define net::Listener (mir-struct net::Accept net::Close))

(define net::TCPListen
  (mir-fun
   (list net::host)
   (let ()
     (define net::server (call tcp-listen-px net::host))
     (define net::accept
       (mir-fun
        (list)
        (let ()
          (define net::llprt (call tcp-accept-px net::server))
          (define net::read
            (mir-fun
             (list net::n)
             (let ()
               (define net::rbuf (call make-bytes net::n))
               (define net::num (call read-bytes-avail net::rbuf net::llprt))
               (mir-if
                (equal? net::num 0)
                (let () io::EOF)
                else
                (let () (call subbytes net::rbuf 0 net::num))))))
          (define net::write
            (mir-fun
             (list net::towr)
             (let ()
               (define net::throwaway157
                 (call write-bytes net::towr net::llprt))
               (call force-output net::llprt))))
          (define net::close
            (mir-fun (list) (let () (call close-port net::llprt))))
          (call io::ReadWriteCloser net::read net::write net::close))))
     (define net::close
       (mir-fun (list) (let () (call tcp-close-px net::server))))
     (call net::Listener net::accept net::close))))

(define net::TCPConnect
  (mir-fun
   (list net::host net::port)
   (let ()
     (define net::llprt (call tcp-connect-px net::host net::port))
     (define net::read
       (mir-fun
        (list net::n)
        (let ()
          (define net::rbuf (call make-bytes net::n))
          (define net::num (call read-bytes-avail net::rbuf net::llprt))
          (mir-if
           (equal? net::num 0)
           (let () io::EOF)
           else
           (let () (call subbytes net::rbuf 0 net::num))))))
     (define net::write
       (mir-fun
        (list net::towr)
        (let ()
          (define net::throwaway158 (call write-bytes net::towr net::llprt))
          (call force-output net::llprt))))
     (define net::close (mir-fun (list) (let () (call close-port net::llprt))))
     (call io::ReadWriteCloser net::read net::write net::close))))


;; /home/sawatari/Documents/Projects/racket/mirlang/samples/socks5.mir
(define socks5::handle
  (mir-fun
   (list socks5::client)
   (let ()
     (define socks5::throwaway142
       (mir-defer (call (call socks5::client 'Close))))
     (define socks5::greeting (call io::ReadFull socks5::client 2))
     (define socks5::throwaway143
       (mir-when
        (not (equal? (dict-ref socks5::greeting 0) 5))
        (let () (mir-raise "Does not support version other than 5"))))
     (define socks5::throwaway144
       (call io::ReadFull socks5::client (dict-ref socks5::greeting 1)))
     (define socks5::repl (bytes 5 0))
     (define socks5::throwaway145
       (call (call socks5::client 'Write) socks5::repl))
     (define socks5::conreq1 (call io::ReadFull socks5::client 4))
     (define socks5::throwaway146
       (mir-when
        (not (equal? (dict-ref socks5::conreq1 2) 0))
        (let () (mir-raise "Reserved field not zero"))))
     (define socks5::host "")
     (define socks5::port 0)
     (define socks5::throwaway147
       (mir-if
        (equal? (dict-ref socks5::conreq1 3) 1)
        (let ()
          (define socks5::addr (call io::ReadFull socks5::client 4))
          (call
           assign
           socks5::host
           (call
            io::Sprintf
            "~a.~a.~a.~a"
            (dict-ref socks5::addr 0)
            (dict-ref socks5::addr 1)
            (dict-ref socks5::addr 2)
            (dict-ref socks5::addr 3))))
        else
        (mir-if
         (equal? (dict-ref socks5::conreq1 3) 3)
         (let ()
           (define socks5::adlen
             (dict-ref (call io::ReadFull socks5::client 1) 0))
           (define socks5::adbts
             (call io::ReadFull socks5::client socks5::adlen))
           (call assign socks5::host (call bytes2string socks5::adbts)))
         else
         (let () (mir-raise "WTF!")))))
     (define socks5::throwaway148
       (call assign socks5::pb (call io::ReadFull socks5::client 2)))
     (define socks5::throwaway149
       (call
        assign
        socks5::port
        (+ (* (dict-ref socks5::pb 0) 256) (dict-ref socks5::pb 1))))
     (define socks5::throwaway150
       (call io::Printf "Tunneling: ~a:~a\n" socks5::host socks5::port))
     (define socks5::throwaway151
       (call (call socks5::client 'Write) (bytes 5 0 0 1 0 0 0 0 0 0)))
     (define socks5::remconn (call net::TCPConnect socks5::host socks5::port))
     (define socks5::throwaway152
       (mir-yarn
        (let ()
          (define socks5::throwaway153
            (mir-defer (call (call socks5::remconn 'Close))))
          (define socks5::throwaway154
            (mir-recover (list socks5::e) (call void)))
          (call io::CopyIO socks5::client socks5::remconn))))
     (define socks5::throwaway155 (mir-recover (list socks5::e) (call void)))
     (call io::CopyIO socks5::remconn socks5::client))))

(define socks5::listener (call net::TCPListen 1080))

(mir-loop
 socks5::continue
 (list socks5::n 0)
 (let ()
   (define socks5::client (call (call socks5::listener 'Accept)))
   (define socks5::throwaway156
     (mir-yarn (call socks5::handle socks5::client)))
   (call socks5::continue (+ socks5::n 1))))

