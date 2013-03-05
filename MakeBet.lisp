(in-package "ACL2")
(include-book "structs")
(include-book "io-utilities" :dir :teachpacks)


; Adds specified amount of chips to the gamestate pot.
(defun addPot (game amount)
   (gamestate (gamestate-players game) 
              (gamestate-common game) 
              (gamestate-last-raise game) 
              (gamestate-seed game) 
              (+ (gamestate-pot game)amount)))


; Removes the specified amount of chips from the players total.
; Assumes validation has already been made for having enough chips.
(defun removePlayerChips (curPlayer amount)
   (player (player-name curPlayer) 
           (- (player-chips curPlayer) amount) 
           (player-call-amount curPlayer) 
           (player-ready curPlayer) 
           (player-hand curPlayer)))

; Loops through a list of players looking for the specified name.
; Subtracts the amount of chips to make and returns the list of players.
(defun findPlayer (players curPlayer restPlayers amount)
   (if (consp players)
       (if (equal (player-name (car players)) (player-name curPlayer) )
           (findPlayer (cdr players) 
                       curPlayer 
                       (cons restPlayers 
                             (removePlayerChips (car players) amount)) 
                       amount)
           (findPlayer (cdr players) 
                       curPlayer 
                       (cons restPlayers (car players)) 
                       amount))
           restPlayers))

(defun setBetHistory (game amount)
;   (gamestate (gamestate-players game)
;              (gamestate-common game)
;              (+ (gamestate-last-raise game) amount)
;              (gamestate-seed game)
;              (gamestate-pot game)))
   (+ (gamestate-last-raise game) amount))


; Main bet function. 
; Given a player that is betting a certain amount,
; 	it looks through the list of players and adjusts
; 	that players chip amounts while adding it to the pot total.
; Also adds to how much has been bet for future players.
(defun makeBet (game req)
   (let* ((amount (request-bet req)))
   (gamestate (findPlayer (gamestate-players game) 
                          (request-player req) 
                          nil 
                          amount) 
              (gamestate-common game) 
              (setBetHistory game amount) 
              (gamestate-seed game) 
              (addPot game amount))))

