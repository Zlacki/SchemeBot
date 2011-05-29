(load-option 'regular-expression)
(load-option 'format)

(define HOSTNAME "irc.rizon.net")
(define PORT 6667)
(define NICKNAME "schemebot")
(define CHAN "#perwl")

(define io
  (open-tcp-stream-socket HOSTNAME PORT))

(define (rwloop)
  (let ((line (read-line io)))
    (if (eof-object? line)
      (disconnect)
      (process line)))
  (rwloop))

(define (disconnect)
  (display "Lost connection.  Attempting to re-establish...");
  (exit)) ; todo: recovery from losing connection

(define (process line)
  (let ((r (re-string-match "^PING \\(.*\\)" line)))
    (and r
      (pong (re-match-extract line r 1))))
  (let ((r (re-string-match "^:\\([^!]+\\)!\\([^ ]+\\) PRIVMSG \\([^ ]+\\) :\\(.+\\)" line)))
    (and r
      (handle-message (re-match-extract line r 1)
        (re-match-extract line r 2)
        (re-match-extract line r 3)
        (re-match-extract line r 4)))))

(define (out string)
;  (display (format #f "OUT: ~A" string))
  (write-string string io)
  (flush-output io))

(define (pong host)
  (out (format #f "PONG ~A~%" host)))

(define (user name mode realname)
  (out (format #f "USER ~A-2 ~A * :~A-2~%" name mode realname)))

(define (nick name)
  (out (format #f "NICK ~A~%" name)))

(define (join channel)
  (out (format #f "JOIN ~A~%" channel)))

(define (privmsg dest text)
  (out (format #f "PRIVMSG ~A :~A~%" dest text)))

(define (handle-message name host channel message)
  (if (char=? (string-ref message 0) #\!) (handle-command name host channel message)))

(define (handle-text name host channel message)
  (display message) (newline)
  (let ((r (re-string-match "v=\\(.*\\)" message)))
    (and r
      (display (re-match-extract message r 1)))))

(define (handle-command name host channel message)
  (define index (string-length message))
  (letrec ((crawler (lambda (i)
      (if (char=? (string-ref message i) #\space)
        (set! index i)
        (if (> (string-length message) (+ i 1)) (crawler (+ i 1)))))))
    (crawler 0))
  (define command (substring message 0 index))
  (display command)
  (define raw (if (< (string-length command) (string-length message)) (substring message (+ 1 (string-length command)) (string-length message)) '()))
;  (if (string=? command "!say") (privmsg channel raw))
;  (if (string=? command "!play") (privmsg channel "PLAYAN"))
  (if (string=? command "!eval") (if (string=? host "~Sl@ck.ware") (if (string=? name "slacky") (eval (read (open-input-string raw)) user-initial-environment))))
)

(user NICKNAME 0 NICKNAME)
(nick NICKNAME)
(join CHAN)

(rwloop)
