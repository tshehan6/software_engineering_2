# tom shehan
# team stroustrup

# import bottlepy framework and the subprocess module
import subprocess, os
from bottle import *

# create a JSON file representing the user's request
def write_request_file():

	request_type = request.query.get('type','')
	request_player = request.query.get('player','')
	request_bet = request.query.get('bet','')

	content = """{'type':'"""+str(request_type)+"""', 'player':'"""+str(request_player)+"""', 'bet':'"""+str(request_bet)+"""'}"""

	with open ('request.json', 'w') as f:
		f.write (content)

# remove the request file
def remove_request_file():
	os.remove('request.json')

# route the root url to some statically defined html
@route('/')
def app():
	with open('app.html') as f:
		output = f.read()
	return output

# route the root url to the knockout library
@route('/knockout.js')
def knockout():
	with open('knockout-2.2.1.js') as f:
		output = f.read()
	return output

# route the root url to the jQuery library
@route('/jquery.js')
def knockout():
	with open('jquery-1.9.1.min.js') as f:
		output = f.read()
	return output

# route the interface url the interface function.
# this prepares the input, calls the ACL2 executable, and handles the results
@route('/interface')
def handler():

	write_request_file()
	subprocess.call(['./worker'])

	with open('response.json') as f:
		output = f.read()

	os.remove('response.json')
#	remove_request_file()

	return output

# start the bottlepy server on http://localhost
# by running "python3 interface.py"
run(host='localhost', reloader=True)
