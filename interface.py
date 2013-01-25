# tom shehan
# 1/22/2012
# team stroustrup

# import bottlepy framework and the subprocess module
import subprocess
from bottle import *


# route the root url to our (not so) cleverly named function
@route('/')
def run_executable_then_get_contents_of_a_file_then_delete_that_file():
	subprocess.call(['./worker'])
	with open('output.html') as f:
		output = f.read()
	subprocess.call(['rm','output.html'])
	return output

# start the bottlepy server on http://localhost:12358
# by running "python3 interface.py"
run(host='localhost', port='12358')
