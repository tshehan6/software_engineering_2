; includes and setup
(in-package "ACL2")
(include-book "JSONEncode")
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")
(include-book "utilities")
(include-book "refresh")
(include-book "JoinGame")
(include-book "TakeTurn")


;;;;These should be defined elsewhere by somebody else;;;;;;;;;;
;(defun refreshRequest (request)
;   	(let* ((dontUse request))
;		(response->JSON (response (list(card 5 5) (card 4 4)) 10 "tom" (list (response-other-player "test" 123 (list (card 1 2) (card 3 4))) (response-other-player "testing" 6543 (list (card 1 2) (card 3 4)))) (list (card 1 1) (card 2 2)) 40 "" "" nil ""))     
 ;    )
;)
;(defun playRequest (request)
;   	(let* ((dontUse request))
;     "{\"player_cards\" : [{\"card\":\"cards/c1.png\"},{\"card\":\"cards/h5.png\"}], \"player_money\" : \"3864\", \"player_name\" : \"tom\", \"other_players\" :     [{\"name\": \"bob\",\"money\" : \"100\",\"cards\" :[{\"card\":\"cards/s9.png\"},{\"card\":\"cards/s2.png\"}]}], \"community_cards\" : [{\"card\" : \"cards/d12.png\"},{\"card\" : \"cards/h8.png\"},{\"card\" : \"cards/c13.png\"}], \"pot\" : \"1234\"}"                         
;     )
 ;)
;(defun joinRequest (request)
;   	(let* ((dontUse request))
;     "{\"player_cards\" : [{\"card\":\"cards/c1.png\"},{\"card\":\"cards/h5.png\"}], \"player_money\" : \"87934\", \"player_name\" : \"tom\", \"other_players\" :     [{\"name\": \"bob\",\"money\" : \"100\",\"cards\" :[{\"card\":\"cards/s9.png\"},{\"card\":\"cards/s2.png\"}]}], \"community_cards\" : [{\"card\" : \"cards/d12.png\"},{\"card\" : \"cards/h8.png\"},{\"card\" : \"cards/c13.png\"},{\"card\" : \"cards/d4.png\"}], \"pot\" : \"1234\"}"                         
;     )
 ;)
;;;;End of things that someone else needs to do somewhere else;;;;;;

:set-state-ok t
; writes content to file and returns state
(defun toFile (state content file) 
		(string-list->file	
			file 
			(list content)
			state
		)
)


; returns the file as a string
(defun fromFile (state file)
  (file->string file state) 
)

(set-guard-checking :none);
(defun writeResponse (state response)
   (mv-let (error state)
           (toFile state (response->JSON response) "response.json")
      	(declare (ignore error))
      	state))

(defun writeGamestate (state gamestate)
   (toFile state (gamestate->JSON gamestate) "gamestate.txt"))

(defun writeGamestateAndResponse (state gamestate response)
   (let* ((state (writeResponse state response)))
         (writeGamestate state gamestate)))

;creates a response struct for the player who intitated a request
;used to get the response to be written out after a join or takeTurn request
(defun getResponse (request gamestate)
   (if (gamestate-p gamestate)
       (updateResponseStructForPlayer (request-player request) gamestate)
       Nil))

(defun readGamestate (state)
	(mv-let (input-string error state)
		(fromFile state "gamestate.txt")
          (mv (JSON->gamestate input-string) error state)))



      
(defun processRequest (request state game)
	(let* ((req (JSON->request request)) 
        	  (requestType (request-type req)))
		(cond ((string-equal requestType "refresh")
         		(let* ((state (writeResponse state (refreshRequest request))))
       				(mv game req state)))
        		((string-equal requestType "play") 
           		(mv (takeTurn game req) req state))
        		((string-equal requestType "join") 
           		(mv (joinGame game (request-player req) t) req state))
        		(t (mv game req state)))
	)
  )


(defconst *game* (gamestate nil nil nil 12345 0 nil nil nil nil nil))

(defun main (state)
   (mv-let (game state)
	   (mv-let (game error state) 
	           (readGamestate state)
	      (if error
	          (mv *game* state)
	          (mv game state)))
	(mv-let (str1 error state)
		(fromFile state "request.json")
		(if error
    			(toFile state "Request failed" "error.txt")
			(mv-let (game req state)
           		(processRequest str1 state game)
      			(writeGamestateAndResponse state game (getResponse req game))
)))))
   
   
   