(in-package "ACL2")
(include-book "structs")
(include-book "utilities")

(defconst *test-request*
   (make-request :type "refresh"
   :player "player_1"
   :bet 0))

;recursive helper to build a list of response-other-player structs
;adds an entry for all players except the player corresponding to thisPlayerName
(defun getOtherPlayers (thisPlayerName playersList)
   (if (and (stringp thisPlayerName)
            (listp playersList)
            (player-p (car playersList))
            (not (endp playersList)))
       (if (equal thisPlayerName (player-name (car playersList)))
           (getOtherPlayers thisPlayerName (cdr playersList))
           (cons (make-response-other-player 
                  	:name (player-name (car playersList))
                    :money (player-chips (car playersList))
                    :cards (player-cards (car playersList)))
                 (getOtherPlayers thisPlayerName (cdr playersList))))
       Nil))

;returns a response struct for the player represented by playerName
(defun updateResponseStructForPlayer (playerName gamestate)
   (if (and (stringp playerName)
            (gamestate-p gamestate))
       (let* ((playerStruct (findPlayerInPlayersList playerName gamestate))
              (otherPlayers (getOtherPlayers playerName (gamestate-players gamestate))))
             (make-response :player-cards (player-cards playerStruct)
                            :player-money (player-chips playerStruct)
                            :player-name playerName
                            :other-players otherPlayers
                            :community-cards (gamestate-common gamestate)
                            :pot (gamestate-pot gamestate)
                            :current-player-turn (gamestate-current-player-turn gamestate)
                            :game-status-message (gamestate-game-status-message gamestate)
                            :is-hand-over (gamestate-is-hand-over gamestate)
                            :error-message (gamestate-error-message gamestate)))
        Nil))