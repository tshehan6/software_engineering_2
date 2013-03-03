; includes and setup
(include-book "io-utilities" :dir :teachpacks)
(include-book "JSONEncode")
(include-book "structs")
(set-state-ok t)
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


(defun main (state)
	(let*	(
			(json (fromFile state "request.json"))
			(request (JSON->request json))
			(type (request-type request))
		)
		(if (string-equal type "bet")

			(toFile state (concatenate 'string "You bet " (rat->str(request-bet request) 0) " dollars") "output.html")
		
			(if (string-equal type "join")
			
				(toFile state (concatenate 'string "You have joined the game as " (request-player request)) "output.html")
			
				(if (string-equal type "refresh")

					(toFile state (concatenate 'string "As of this refresh your status is " (if (request-ready request) "READY" "NOT READY") ) "output.html")

					(toFile state "Invalid request type" "output.html")

				)

			)

		)
	) 
)