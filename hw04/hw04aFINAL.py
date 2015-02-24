import threading, time, random, os

#Class from downey
class Semaphore(threading._Semaphore):
	wait = threading._Semaphore.acquire
	signal = threading._Semaphore.release

class Thread(threading.Thread):
	def __init__(self, t, *args):
		threading.Thread.__init__(self, target=t, args=args)
		self.start()

#boolean array, n true if philo n is eating
eating = []
#sema array that phlio n waits on for a fork
philos = []
#boolean array, index n is true if philo n is waiting
waiting = []

baton = Semaphore(1)
delay = Semaphore(0)
total = 0

#main function
def main(n):
	global total
	global waiting
	global eating 
	global philos

	total = n
	waiting = [False] * n
	eating = [False] * n
	for i in range(0, n):
		philos.append(Semaphore(0))
	for i in range(0, n):
		Thread(philo, i)
	try:
		while True: time.sleep(0.1)
	except (KeyboardInterrupt, SystemExit):
		os._exit(0)

def philo(n):
	global eating
	#set up left and right sides
	if (n-1 == -1):
		left = len(eating)-1
	else:
		left = n-1
	if (n+1 > len(eating)-1):
		right = 0
	else:
		right = n+1

	while (True):          
		dine(n, left, right)

	os._exit(0)

#function for checking assertion
def check(left, right, n):
	global eating
	if (((eating[left]) or (eating[right])) and (eating[n])):
		print "ERROR"

def dine(n, left, right):
	global eating	
	global baton
	global philos
	global waiting
	global total
	hasBaton = False

	time.sleep(random.random())

	check(left, right, n)
	baton.wait()

	check(left, right, n)

	if ((eating[left]) or (eating[right])):
		waiting[n] = True
		baton.signal()
		while (waiting[n]):
			philos[n].wait()
			if ((not eating[left]) and (not eating[right])):
				waiting[n] = False
				eating[n] = True
				print(str(n) + " is eating")
			baton.signal()
	else:
		eating[n] = True
		print(str(n) + " is eating")
		baton.signal()			

	check(left, right, n)

	time.sleep(random.random())

	baton.wait()
	hasBaton = True
	eating[n] = False
	print(str(n) + " is done eating")	

	check(left, right, n)

	#give baton to lowest index waiting philo
	for i in range (0, total):
		if (waiting[i]):
			philos[i].signal()
			hasBaton = False
			break
	if (hasBaton):
		baton.signal()	
		hasBaton = False	
	
	check(left, right, n)

#start
main(5)
