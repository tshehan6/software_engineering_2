; Tom Shehan
; Team Stroustrup
; Encode / Decode Module

; includes and setup
(include-book "list-utilities" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")


; get a token from a larger string with a given terminator
(defun getStringToken (charList terminator)
	; go until the end of the string or the terminating character is reached
	(if (or (not (consp charList)) (char-equal terminator (car charList)))

	  	; base case
     		'()

		;recursive case
		(cons (car charList) (getStringToken (cdr charList) terminator))
     	)
)

; convert a JSON string into a list of tokens
(defun tokenize (charList)
	
	; go until there are no more characters in the JSON character list
	(if (not (consp charList))

		;base case
		'()
		
		;recursive case
		(if (char-equal (car charList) #\')
		  	
		  	; string token
			(let* (( token (getStringToken (cdr charList) #\') ))
				(cons token (tokenize (nthcdr (+ 2 (length token)) charList)))
			)

			(if (or (char-equal (car charList) #\{) (char-equal (car charList) #\}) (char-equal (car charList) #\[) (char-equal (car charList) #\]) )

				; control token
				(cons (list (car charList)) (tokenize (cdr charList)))

				; unrecognized , unnecessary, or whitespace token
				(tokenize (cdr charList))
			)
		)
	)
)


; pair elements into tuples to create dictionary elements
(defun makeNameContentPairs (items)
	(if (consp items)
		(cons (list (car items) (cadr items)) (makeNameContentPairs (cddr items)))
		'()
	)
)

; generate a parse tree from the token list
(defun parser (tokens terminator) 

	(if (string-equal (chrs->str(car tokens)) terminator)

		; base case
		(list '() 1)

		; recursive case
		(if (string-equal (chrs->str(car tokens)) "[")

			; new child list
			(let*	(
					(newLevel (parser (cdr tokens) "]" )) 
					(newLevelContent (car newLevel)) 
					(newLevelLength (+ 1 (cadr newLevel)))
				)
				(let*	(
						(theRest (parser (nthcdr newLevelLength tokens) terminator)) 
						(restContent (car theRest)) 
						(restConsumed (cadr theRest))
					)
					(list (cons newLevelContent restContent) (+ restConsumed newLevelLength))
				)
			)
                    
			(if (string-equal (chrs->str(car tokens)) "{")

				; new child dictionary
				(let*	(
						(newLevel (parser (cdr tokens) "}" )) 
						(newLevelContent (car newLevel)) 
						(newLevelLength (+ 1 (cadr newLevel)))
					)
					(let*	(
							(theRest (parser (nthcdr newLevelLength tokens) terminator)) 
							(restContent (car theRest)) 
							(restConsumed (cadr theRest))
						)
						(list (cons (makeNameContentPairs newLevelContent) restContent) (+ restConsumed newLevelLength))
					)
				)
				
				; just another regular string token
				(let*	(
						(theRest (parser (cdr tokens) terminator)) 
						(restContent (car theRest)) 
						(restConsumed (cadr theRest))
					)
					(list (cons (chrs->str(car tokens)) restContent) (+ 1 restConsumed))
				)
			)
		) 
	)
)

; get the JSON in the correct form for tokenizing, tokenize it, parse it, and extract the result from its wrapper
(defun getParseTree (JSON)
	(car(car (parser(tokenize (str->chrs JSON)) "" )))
)

; convert a parse tree to a request structure
; expects a list of the form '(("type" "join|bet|refresh") ("player" "name") ("bet" "amount") ("ready" "T|F"))
(defun JSON->request (JSON)
	(let*	(
		 	(parsedJSON (getParseTree JSON))
			(type (second (first parsedJSON)))
			(player (second (second parsedJSON)))
			(bet (str->rat(second (third parsedJSON))))
			(ready (if(string-equal "yes" (second (fourth parsedJSON))) T nil))
		)
		(request type player bet ready)
	)
)


(JSON->request "{'type':'join','player':'Tom','bet':'12358','ready':'yes'}")
