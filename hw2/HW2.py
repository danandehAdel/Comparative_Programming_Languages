#! /usr/bin/env python
import sys

"""
HW2: Question 11 from HW2
Authors: Prateek Chawla and Adel Danandeh

Program Description: 
Spells out the number as string in English.

Instructions on how to run HW2.py: 
This python script can be run in a number of manners (listed below). Tested on Python 2.7.10. 
1. python HW2.py NUMBER  
	python HW2.py 23
	twenty three
2. ./HW2.py NUMBER
	./HW2.py 93218065
	ninety three million two hundred eighteen thousand sixty five
3. Open the python interpreter, import HW2 and execute HW2.sayNum(NUMBER)
	>>> import HW2
	>>> HW2.sayNum(82379)
	'eighty two thousand three hundred seventy nine'

IMPORTANT NOTE: Replace the NUMBER command line argument, above, with an integer number like: 82379. 
"""
#constants 
digits = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"  ]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
twentyPlus = ["","", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
bigIntervals = ["","", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion", "sextillion", "septillion", "octillion", "nonillion", "decillion", "undecillion", "duodecillion", "tredecillion", "quattuordecillion", "quindecillion", "sexdecillion", "septendecillion", "octodecillion", "novemdecillion", "vigintillion"]

def sayNum(x):	
	"""
	Spells out the number as string in English.

	@type x: int
	@param x: the number to be converted
	"""
	x = _inputValidation(x) 
	x = x[::-1]

	inputList = []
	outputStr = ""

	for indx, val in enumerate(str(x)):
		#parse and divide the input by unit
		if (indx) % 3 == 0: 
			inputList.append([val]) 
		else: 
			inputList[len(inputList)-1] += val 

	inputList[::-1] = [val[::-1] for val in inputList] #reverses list

	for indx, val in enumerate(inputList):
		hundreds =  bigIntervals[len(inputList) - indx]
		tripleZero = False 
		temp = ""

		if len(val) > 2:
			if int(val[0]) == 0 and int(val[1]) == 0 and int(val[2]) == 0:
				tripleZero = True

			temp += digits[int(val[0])]
			outputStr += temp 

			if int(val[0]) != 0:
				outputStr += " hundred " 
			del val[0]
		outputStr += _underHundredChecker(val) + " "
		if not tripleZero:
			outputStr += hundreds + " "

	outputStr = " ".join(outputStr.split())	#removes extra white spaces
	return outputStr

def _underHundredChecker(val):
	"""
	For numbers under 100 translate them to their respective 
	individual digits, teens or a number between 20 and 90. 
	"""
	outputStr = ""

	if len(val) == 1:
		outputStr += digits[int(val[0])] + " "
	elif len(val) == 2:
		if int(val[0]) == 1:
			outputStr += teens[int(''.join(val))%10] + " "
		else:
			outputStr += twentyPlus[int(val[0])%10] + " " + digits[int(val[1])] + " "
	return outputStr

def _inputValidation (x):
	"""
	Validates that the input is an integer. 
	"""
	try: 
		int(x)
	except ValueError: 
		print ("Input not valid. Please enter an integer number.")
		exit()

	return str(x)

if __name__ == "__main__":
	try:
		print (sayNum(sys.argv[1]))
	except IndexError:
		print ("Please enter an integer as a command line argument.")