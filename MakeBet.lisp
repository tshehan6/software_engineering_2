(in-package "ACL2")
(include-book "structs")
(include-book "io-utilities" :dir :teachpacks)


; Adds specified amount of chips to the gamestate pot.
(defun addPot (game amount)
   (gamestate (gamestate-players game) 
              (gamestate-common game) 
              (gamestate-last-raise game) 
              (gamestate-seed game) 
              (+ (gamestate-pot game)amount)
              (gamestate-deck game)
              (gamestate-current-player-turn game)
              (gamestate-game-status-message game)
              (gamestate-is-hand-over game)
              (gamestate-error-message game)))


; Removes the specified amount of chips from the players total.
; Assumes validation has already been made for having enough chips.
(defun removePlayerChips (curPlayer amount)
   (player (player-name curPlayer) 
           (if (<= (- (player-chips curPlayer) amount) 0) 
               0
               (- (player-chips curPlayer) amount))
           0 
           (player-ready curPlayer) 
           (player-cards curPlayer)))


; Adds a specified number of chips to how much the player needs to call.
(defun addToCallAmount (curPlayer amount)
   (player (player-name curPlayer)
           (player-chips curPlayer)
           (+ (player-call-amount curPlayer) amount)
           (player-ready curPlayer)
           (player-cards curPlayer)))


; Loops through a list of players looking for the specified name.
; Subtracts the amount of chips to make and returns the list of players.
; For every other player, adds to how much they need to call.
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
                       (if (not restPlayers)
   				  (cons restPlayers 
                           	(addToCallAmount (car players) amount) )
                           (addToCallAmount (car players) amount) )
                       amount))
           restPlayers))


(defun setBetHistory (req)
;   (gamestate (gamestate-players game)
;              (gamestate-common game)
;              (+ (gamestate-last-raise game) amount)
;              (gamestate-seed game)
;              (gamestate-pot game)))
   (request-player req))


; Given a list of players and a string, iterates through the list
; looking for the player whose name matches the given string.
(defun getPlayer (players reqname)
   (if (consp players)
       (if (equal (player-name (car players)) reqname)
           (car players)
           (getPlayer (cdr players) reqname))
       nil))


; Given a list of players and a string, iterates through the list
; looking for the player whose name matches the given string.
; Then select the player after that for whose turn it is next.
(defun getNextPlayer (players firstPlayer reqname)
   (if (consp players)
       (if (equal (player-name (car players)) reqname)
           (if (> (len players) 1)
           	(cadr players)
               firstPlayer)
           (getNextPlayer (cdr players) firstPlayer reqname))
       nil))


; Main bet function. 
; Given a player that is betting a certain amount,
; 	it looks through the list of players and adjusts
; 	that players chip amounts while adding it to the pot total.
; Also adds to how much has been bet for future players.
(defun makeBet (game req)
   (let* ((amount (request-bet req)))
   (gamestate (findPlayer (gamestate-players game) 
                          (getPlayer (gamestate-players game) 
                                     (request-player req))
                          nil 
                          amount) 
              (gamestate-common game) 
              (if (> amount 0)
                  (setBetHistory req) 
                  (gamestate-last-raise game))
              (gamestate-seed game) 
              (+ (gamestate-pot game) amount)
              (gamestate-deck game)
              (getNextPlayer (gamestate-players game)
                             (car (gamestate-players game))
                             (request-player req)) 
              (gamestate-game-status-message game)
              (gamestate-is-hand-over game)
              (gamestate-error-message game))))

