; includes and setup
(in-package "ACL2")
(include-book "JSONEncode")
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")
(include-book "utilities")
(include-book "refresh")


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
          (mv input-string error state)))

      
(defun processRequest (state request)
	(let* (;(requestStruct (JSON->request request)) 
        	  (requestType (request-type request)))
		(cond ((string-equal requestType "refresh") 
				(writeResponse state (refreshRequest request)))
        		((string-equal requestType "play") state)
        		((string-equal requestType "join") state)
        		(t state))
	)	  
  )

(defun main (state)
	    (toFile state (processRequest (fromFile state "request.json")) "response.json")

)