; includes and setup
(include-book "io-utilities" :dir :teachpacks)
(set-state-ok t) 

; writes content to file and returns state
(defun toFile (state content file) 
	(let*((state 
		(string-list->file	
			file 
			(list content)
			state
		) 
	)))
)

; returns the file as a string
(defun fromFile (state file)
  (file->string file state) 
)

; get the request type
; this will be redefined once we have a json parser
(defun requestType ()
  (fromFile state "request.json")   
)

; point of entry
(defun main (state)
	(let* ((rtype (requestType)))
       (if (equal rtype "refresh")
           (toFile state "you made a refresh request" "output.html")
           (if (equal rtype "join")
             (toFile state "you joined a game" "output.html")
             (if (equal rtype "play")
               (toFile state "you took a turn" "output.html")  
               (toFile state "invalid request type" "output.html")  
             )
           )
       )
  	)
)


