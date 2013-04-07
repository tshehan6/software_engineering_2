; includes and setup
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")
(include-book "JSONEncode")
(include-book "utilities")
(include-book "refresh")


;;;;These should be defined elsewhere by somebody else;;;;;;;;;;
;(defun refreshRequest (request)
   	;(let* ((dontUse request))
     ;"{\"player_cards\" : [{\"card\":\"cards/c1.png\"},{\"card\":\"cards/h5.png\"}], \"player_money\" : \"100\", \"player_name\" : \"tom\", \"other_players\" :     [{\"name\": \"bob\",\"money\" : \"100\",\"cards\" :[{\"card\":\"cards/s9.png\"},{\"card\":\"cards/s2.png\"}]}], \"community_cards\" : [{\"card\" : \"cards/d12.png\"},{\"card\" : \"cards/h8.png\"}], \"pot\" : \"1234\"}"                         
     ;)
;)
(defun playRequest (request)
   	(let* ((dontUse request))
     "{\"player_cards\" : [{\"card\":\"cards/c1.png\"},{\"card\":\"cards/h5.png\"}], \"player_money\" : \"3864\", \"player_name\" : \"tom\", \"other_players\" :     [{\"name\": \"bob\",\"money\" : \"100\",\"cards\" :[{\"card\":\"cards/s9.png\"},{\"card\":\"cards/s2.png\"}]}], \"community_cards\" : [{\"card\" : \"cards/d12.png\"},{\"card\" : \"cards/h8.png\"},{\"card\" : \"cards/c13.png\"}], \"pot\" : \"1234\"}"                         
     )
 )
(defun joinRequest (request)
   	(let* ((dontUse request))
     "{\"player_cards\" : [{\"card\":\"cards/c1.png\"},{\"card\":\"cards/h5.png\"}], \"player_money\" : \"87934\", \"player_name\" : \"tom\", \"other_players\" :     [{\"name\": \"bob\",\"money\" : \"100\",\"cards\" :[{\"card\":\"cards/s9.png\"},{\"card\":\"cards/s2.png\"}]}], \"community_cards\" : [{\"card\" : \"cards/d12.png\"},{\"card\" : \"cards/h8.png\"},{\"card\" : \"cards/c13.png\"},{\"card\" : \"cards/d4.png\"}], \"pot\" : \"1234\"}"                         
     )
 )
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

(defun processRequest (request)
	(let* ((requestStruct (JSON->request request)) 
        	  (requestType (request-type requestStruct)))
		(if (string-equal requestType "refresh") 
			(refreshRequest requestStruct)
      		(if (string-equal requestType "play")  
            		(playRequest requestStruct)
              		(if (string-equal requestType "join")  
            			(joinRequest requestStruct)
              			"bad request type"
            		)
            	)
		)	  
	)
	
)

(defun main (state)
	    (toFile state (processRequest (fromFile state "request.json")) "response.json")

)