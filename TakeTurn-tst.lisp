(in-package "ACL2")
(include-book "utilities")
(include-book "doublecheck" :dir :teachpacks)
(include-book "testing" :dir :teachpacks)
(include-book "TakeTurn")

;TEST #1
;test for standard case of making a bet that does not end the round
(defconst *bet-1-request*
   (make-request	:type "play"
                 	:player "player-1"
                  	:bet 100))

(defconst *player-1*
   (make-player :name "player-1"
                :chips 1000
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *D13* *H2*) :handRank Nil)))

(defconst *post-bet-1-player-1*
   (make-player :name "player-1"
                :chips 900
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *D13* *H2*) :handRank Nil)))

(defconst *player-2*
   (make-player :name "player-2"
                :chips 1000
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *H10* *C4*) :handRank Nil)))

(defconst *post-bet-1-player-2*
   (make-player :name "player-2"
                :chips 1000
                :call-amount 100
                :ready t
                :cards (make-hand :cards (list *H10* *C4*) :handRank Nil)))

(defconst *player-3*
   (make-player :name "player-3"
                :chips 1000
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *S11* *S8*) :handRank Nil)))

(defconst *post-bet-1-player-3*
   (make-player :name "player-3"
                :chips 1000
                :call-amount 100
                :ready t
                :cards (make-hand :cards (list *S11* *S8*) :handRank Nil)))

(defconst *pre-bet-1-gamestate*
   (make-gamestate :players (list *player-1* *player-2* *player-3*)
                   :common (make-hand :cards (list *s2* *s4* *c1*)
                                      :handRank Nil)
                   :last-raise ""
                   :seed 234234
                   :pot 1000
                   :deck (make-deck :cards (list *d1*))
                   :current-player-turn "player-1"
                   :game-status-message ""
                   :is-hand-over Nil
                   :error-message ""))

(defconst *post-bet-1-gamestate*
   (make-gamestate :players (list *post-bet-1-player-1* *post-bet-1-player-2* *post-bet-1-player-3*)
                   :common (make-hand :cards (list *s2* *s4* *c1*)
                                      :handRank Nil)
                   :last-raise "player-1"
                   :seed 234234
                   :pot 1100
                   :deck (make-deck :cards (list *d1*))
                   :current-player-turn "player-2"
                   :game-status-message ""
                   :is-hand-over Nil
                   :error-message ""))

(check-expect (takeTurn *pre-bet-1-gamestate* *bet-1-request*) *post-bet-1-gamestate*)


;TEST #2
;bet that causes the round to end
(defconst *bet-2-request*
   (make-request	:type "play"
                 	:player "player-3"
                  	:bet 100))

(defconst *bet-2-player-1*
   (make-player :name "player-1"
                :chips 900
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *D13* *H2*) :handRank Nil)))

(defconst *bet-2-player-2*
   (make-player :name "player-2"
                :chips 900
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *H10* *C4*) :handRank Nil)))

(defconst *pre-bet-2-player-3*
   (make-player :name "player-3"
                :chips 1000
                :call-amount 100
                :ready t
                :cards (make-hand :cards (list *S11* *S8*) :handRank Nil)))

(defconst *post-bet-2-player-3*
   (make-player :name "player-3"
                :chips 900
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *S11* *S8*) :handRank Nil)))

(defconst *pre-bet-2-gamestate*
   (make-gamestate :players (list *bet-2-player-1* *bet-2-player-2* *pre-bet-2-player-3*)
                   :common (make-hand :cards (list *s2* *s4* *c1*)
                                      :handRank Nil)
                   :last-raise "player-1"
                   :seed 234234
                   :pot 1200
                   :deck (make-deck :cards (list *d1*))
                   :current-player-turn "player-3"
                   :game-status-message ""
                   :is-hand-over Nil
                   :error-message ""))

(defconst *post-bet-2-gamestate*
   (make-gamestate :players (list *bet-2-player-1* *bet-2-player-2* *post-bet-2-player-3*)
                   :common (make-hand :cards (list *d1* *s2* *s4* *c1*)
                                      :handRank Nil)
                   :last-raise ""
                   :seed 234234
                   :pot 1300
                   :deck (make-deck :cards Nil)
                   :current-player-turn "player-1"
                   :game-status-message ""
                   :is-hand-over Nil
                   :error-message ""))

(check-expect (takeTurn *pre-bet-2-gamestate* *bet-2-request*) *post-bet-2-gamestate*)