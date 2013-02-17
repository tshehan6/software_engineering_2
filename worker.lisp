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

; point of entry
(defun main (state)
	; write the contents of the request file to the output file
 	; this is the first interactive version of the program, however simple
	(toFile state (fromFile state "request.json") "output.html")
)

