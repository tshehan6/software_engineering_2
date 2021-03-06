(in-package "ACL2")
(include-book "structs")
(include-book "io-utilities" :dir :teachpacks)

; Checks whether all players in the game are ready
(defun checkPlayersReady (players)
   (if (consp players)
       (if (player-ready (car players))
           (checkPlayersReady (cdr players))
           nil)
        t))

; Hands the list of players to the checkPlayersReady iterator function
(defun isGameStarted (game)
   (checkPlayersReady
    (gamestate-players game)))


; Authenticity (for later)
;(generatePlayerKey (gamestate))


; Creates a new player given a particular string value for the name
(defun generatePlayerStruct (name ready)
	(player 
  		name 		; player name
         	1000 		; initial chips
          0 			; call amount (game hasn't started)
          ready 		; is player ready (just created player)
          (hand nil nil)))	; card hand (game hasn't started)


; Adds a player to the gamestate structure
(defun addPlayer (game newPlayer)
   (gamestate 
    		(reverse (cons newPlayer (reverse (gamestate-players game))))
		(gamestate-common game)
          (gamestate-last-raise game)
          (gamestate-seed game)
          (gamestate-pot game)
          (gamestate-deck game)
		(gamestate-current-player-turn game)
		(gamestate-game-status-message game)
		(gamestate-is-hand-over game)
		(gamestate-error-message game)))




; Given a list of players, find the one with a particular name
; and set its ready state to true. Then return the list back.
(defun findPlayerToReady (curPlayers restPlayers playerName)
   (let* ((thisPlayer (car curPlayers)))
   (if (consp curPlayers)
       (if (equal (player-name thisPlayer) playerName)
           (findPlayerToReady (cdr curPlayers)
                              (cons (player
                                   	(player-name thisPlayer)
                                   	(player-chips thisPlayer)
                                   	(player-call-amount thisPlayer)
               					t
               					(player-cards thisPlayer))
                                    restPlayers)
                    		playerName)
           (findPlayertoReady (cdr curPlayers) 
                              (cons thisPlayer restPlayers) 
                              playerName))
       restPlayers)))


; Retrieve the list of players to ready a specific player in the game.
;  Returns the gamestate with the updated player.
(defun readyPlayer (game playerName)
   (gamestate
    		(findPlayerToReady (reverse (gamestate-players game)) nil playerName)
          (gamestate-common game)
          (gamestate-last-raise game)
          (gamestate-seed game)
          (gamestate-pot game)
          (gamestate-deck game)
		(gamestate-current-player-turn game)
		(gamestate-game-status-message game)
		(gamestate-is-hand-over game)
		(gamestate-error-message game)))


(defun playerExists (players playerName)
   (if (consp players)
       (if (equal (player-name (car players)) playerName)
           t
           (playerExists (cdr players) playerName))
       nil))


; Main Join Game function
;  Registers players, assigns chip amounts, and sets the flag 
;  to allow the game to start once all players are ready.
(defun joinGame (game playerName ready)
	(if (playerExists (gamestate-players game) playerName)
		(if ready
			(readyPlayer game playerName)
			game)
		(addPlayer game (generatePlayerStruct playerName ready))))
   
   
