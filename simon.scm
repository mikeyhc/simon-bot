(module simon 
        ( simon:localhost
          simon:nick
          simon:connect)

        (import scheme chicken extras) 
        (require-extension srfi-13 srfi-14)
        (use irc regex ports)

        (define simon:localhost "localhost")
        (define simon:nick "simon")

        (define simon:running #f)
        (define (start-simon host port)
          (if simon:running 
            (print "simon is already running")
            (simon:connect host port)))

        (define quit-regex (regexp "simon:? quit" #t #f #t))
        (define id-regex (regexp "simon:? id .*" #t #f #t))
        (define idto-regex (regexp "simon:? idto .*" #t #f #t))
        (define simon-regex (regexp "simon" #t #f #t))

        (define (id-func body)
          (string-drop body 
                       (if (string= (substring/shared body 5 6) ":") 10 9)))

        (define (idto-func body)
          (let* ([body-tail 
                   (string-drop body 
                                (if (string= (substring/shared body 5 6) ":") 
                                  12 11))]
                 [space-idx (string-index body-tail #\space)])
            (values (string-take body-tail space-idx)
                    (string-trim-both (string-drop body-tail space-idx)
                                      (char-set #\space #\tab)))))
        (define (handle-priv msg)
          (let* ([params (irc:message-parameters msg)]
                 [source (if (string-prefix? "#" (car params))
                           (car params)
                           (irc:message-sender msg))]
                 [body (cadr params)])
            (printf "handling message ~A~N" body)
            (cond [(string-match quit-regex body) (exit)]
                  [(string= (irc:message-sender msg) "plobot")
                   (irc:say simon:conn "plobot: go fuck yourself" source)]
                  [(string-match id-regex body) 
                   (irc:say simon:conn (id-func body) source)]
                  [(string-match idto-regex body)
                   (let-values ([(chan msg) (idto-func body)])
                     (irc:say simon:conn msg chan))]
                  [(string-match simon-regex body)
                   (irc:say simon:conn "piss off" source)])))

        (define simon:conn #f)
        (define (simon:connect host port)
          (let ([conn (irc:connection
                        server:      host
                        port:        port
                        log-traffic: (current-output-port)
                        user:        (string-append "simon@" simon:localhost)
                        nick:        simon:nick)])
            (set! simon:conn conn)
            (irc:connect conn)
            (if (irc:connected? conn)
              (begin
                (handle-exceptions exn
                  ; exception handler
                  (if (condition? exn)
                    (let ([code ((condition-property-accessor 'irc 'code) 
                                 exn)])
                      (if (= code 422)
                        (irc:run-message-loop conn debug:  #t pong:   #t)
                        (with-output-to-port (current-error-port)
                                             (print exn))))
                    (abort exn))
                  ; actual code
                  (set! simon:running #t)
                  (printf "connected to ~A~N" host)
                  (irc:add-message-handler! conn handle-priv
                                            tag:     'handle-priv
                                            command: "PRIVMSG")
                  (irc:add-message-handler! conn 
                                            (lambda (_) 
                                              (irc:join conn "#bots"))
                                            tag: 'join-bots
                                            command: "266")
                  (irc:run-message-loop 
                    conn 
                    debug:  #t
                    pong:   #t)))
              (printf "could not connect to ~A~N" host))))

        )
