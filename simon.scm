(use irc regex)

(define simon-local-host "localhost")
(define simon-nick "simon")

(define simon-running #f)
(define (start-simon host port)
  (if simon-running 
    (print "simon is already running")
    (simon-connect host port)))

(define quit-regex (regexp "simon:? quit" #t #f #t))
(define id-regex (regexp "simon:? id .*" #t #f #t))
(define simon-regex (regexp "simon" #t #f #t))

(define (handle-priv msg)
  (let* ([params (irc:message-parameters msg)]
         [source (if (string-prefix? "#" (car params))
                   (car params)
                   (irc:message-sender msg))]
         [body (cadr params)])
    (printf "handling message ~A~N" body)
    (cond [(string-match quit-regex body) (exit)]
          [(string-match id-regex body) 
           (irc:say 
             simon-conn 
             (string-drop 
               body 
               (if (string= (substring/shared body 5 6) ":")
                 10 9))
             source)]
          [(string-match simon-regex body)
           (irc:say simon-conn "piss off" source)])))

(define simon-conn #f)
(define (simon-connect host port)
  (let ([conn (irc:connection
                server:      host
                port:        port
                log-traffic: (current-output-port)
                user:        (string-append "simon@" simon-local-host)
                nick:        simon-nick)])
    (set! simon-conn conn)
    (irc:connect conn)
    (if (irc:connected? conn)
      (begin
        (handle-exceptions exn
          ; exception handler
          (if (condition? exn)
            (let ([code ((condition-property-accessor 'irc 'code) exn)])
              (if (= code 422)
                (irc:run-message-loop conn debug:  #t pong:   #t)
                (with-output-port (current-error-port)
                                  (print exn))))
            ;(abort exn))))
            (abort exn))
          ; actual code
          (set! simon-running #t)
          (printf "connected to ~A~N" host)
          (irc:add-message-handler! conn handle-priv
                                    tag:     'handle-priv
                                    command: "PRIVMSG")
          (irc:add-message-handler! conn 
                                    (lambda (_) (irc:join conn "#bots"))
                                    tag: 'join-bots
                                    command: "266")
          (irc:run-message-loop 
            conn 
            debug:  #t
            pong:   #t)))
      (printf "could not connect to ~A~N" host))))
