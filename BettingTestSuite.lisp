(in-package "ACL2")
(include-book "doublecheck" :dir :teachpacks)
(include-book "testing" :dir :teachpacks)
(include-book "MakeBet")
(include-book "BetValidation")


;=============================================================
; isBetLegal - checks boundary cases for bet amount
;=============================================================

(check-expect (isBetLegal 5)   t)
(check-expect (isBetLegal 0)   t)
(check-expect (isBetLegal 456) t)
(check-expect (isBetLegal -1)  nil)
(check-expect (isBetLegal -13) nil)


;(defproperty is-bet-legal :repeat 100
;  (betamount :value (random-rational))
;  (equal (isBetLegal betamount) (>= betamount 0)))


;=============================================================
; isBetsufficient - checks if player has enough chips for bet
;=============================================================

(check-expect (isBetSufficient
               (player "Bob" 100 0 t (hand nil nil)) 10)    t)
(check-expect (isBetSufficient
               (player "Bob" 100 0 t (hand nil nil)) 100)   t)
(check-expect (isBetSufficient
               (player "Bob" 100 100 t (hand nil nil)) 100) t)
(check-expect (isBetSufficient
               (player "Bob" 0 0 t (hand nil nil)) 0)       t)
(check-expect (isBetSufficient
               (player "Bob" 100 0 t (hand nil nil)) -13)   t)
(check-expect (isBetSufficient
               (player "Bob" 100 0 t (hand nil nil)) 500)   nil)
(check-expect (isBetSufficient
               (player "Bob" 100 111 t (hand nil nil)) 101) nil)
(check-expect (isBetSufficient
               (player "Bob" -38 0 t (hand nil nil)) -13)   nil)


;(defproperty is-bet-sufficient :repeat 100
;  (player1   :value (player "Bob" (random-rational) 0 t (hand nil))
;   betamount :value (random-rational))
;  (equal (isBetSufficient player1 betamount) 
;         (>= (player-chips player1) betamount)))


;=============================================================
; isBetValid - main call for legal and sufficient bets
;=============================================================


(check-expect (isBetValid
               (player "Bob" 100 0 t (hand nil nil)) 100)    t)
(check-expect (isBetValid
               (player "Bob" 100 10 t (hand nil nil)) 10)    t)
(check-expect (isBetValid
               (player "Bob" 0 10 t (hand nil nil)) 0)       t)
(check-expect (isBetValid
               (player "Bob" 0 0 t (hand nil nil)) 10)       nil)
(check-expect (isBetValid
               (player "Bob" 0 10 t (hand nil nil)) 10)      nil)
(check-expect (isBetValid
               (player "Bob" 10 0 t (hand nil nil)) 100)     nil)
(check-expect (isBetValid
               (player "Bob" 100 0 t (hand nil nil)) -13)    nil)
(check-expect (isBetValid
               (player "Bob" -13 0 t (hand nil nil)) -7)     nil)


(defproperty is-bet-valid :repeat 100
  (player1   :value (player "Bob" (random-rational) 0 t (hand nil))
   betamount :value (random-rational))
  (equal (isBetValid player1 betamount) 
         (and (>= (player-chips player1) betamount) 
              (>= betamount 0))))

;=============================================================
; addPot - adds a specified number of chips to the pot
;=============================================================


(defconst *player* (player "Bob" 13 0 t (hand nil nil)))
(defconst *world1* (gamestate nil (hand nil nil) "" 0 0 (deck nil) "" "" nil ""))
(defconst *world2* (gamestate nil (hand nil nil) "" 0 10 (deck nil) "" "" nil ""))


(check-expect (gamestate-pot (addPot *world1* 0)) 0)
(check-expect (gamestate-pot (addPot *world1* 10)) 10)
(check-expect (gamestate-pot (addPot *world1* -13)) -13)
(check-expect (gamestate-pot (addPot *world2* 10)) 20)
(check-expect (gamestate-pot (addPot *world2* 0)) 10)
(check-expect (gamestate-pot (addPot *world2* -10)) 0)



(defproperty add-pot :repeat 100
  (game  :value (gamestate nil (hand nil nil) "" 0 (random-rational) (deck nil) "" "" nil ""))
   toadd :value (random-rational))
  (equal (gamestate-pot (addPot game toadd))
         (+ (gamestate-pot game) toadd)))


;=============================================================
; removePlayerChips - removes chips from a particular player
;=============================================================

(check-expect (player-chips (removePlayerChips *player* 13))  0)
(check-expect (player-chips (removePlayerChips *player* 0))   13)
(check-expect (player-chips (removePlayerChips *player* 7))   6)
(check-expect (player-chips (removePlayerChips *player* -13)) 26)
(check-expect (player-chips (removePlayerChips *player* 1))   12)


(defproperty remove-player-chips :repeat 100
  (player1  :value (player "Bob" (random-rational) 0 t (hand nil))
   toremove :value (random-rational))
  (equal (player-chips (removePlayerChips player1 toremove))
         (- (player-chips player1) toremove)))
