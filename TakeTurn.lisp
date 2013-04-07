(in-package "ACL2")
(include-book "structs")
(include-book "MakeBet")
(include-book "BetValidation")
(include-book "AfterTurn")


; Given a list of players and a string, iterates through the list
; looking for the player whose name matches the given string.
(defun getPlayer (players reqname)
   (if (consp players)
       (if (equal (player-name (car players)) reqname)
           (car players)
           (getPlayer (cdr players) reqname))
       nil))


; Checks the gamestate to see if the current request
; is the player that needs to take a turn.
(defun curPlayerTurn (game req)
   (if (equal
  	(gamestate-current-player-turn game)
          (request-player req))
       t
       nil))


; Modifies the gamestate to set the error response message
; so the player knows if something has gone wrong while making a request.
(defun createError (game message)
   (gamestate (gamestate-players game) 
              (gamestate-common game) 
              (gamestate-last-raise game) 
              (gamestate-seed game) 
              (gamestate-pot game)
              (gamestate-deck game)
              (gamestate-current-player-turn game)
              (gamestate-game-status-message game)
              (gamestate-is-hand-over game)
              message))


; Main function - receives the gamestate and a turn request
; and proceeds to validate the bet, execute the bet,
; and then calculate any after turn procdures.
(defun takeTurn (game req)
	(if (curPlayerTurn game req)
		(if (isBetValid game req)
			(afterTurn (makeBet game req))
			(createError game "Invalid bet"))
   		(createError game "Wrong player's turn")))



