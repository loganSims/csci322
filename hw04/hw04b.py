#############################################
#
# Logan Sims
# hw04: part b
#
# Using a FIFO queue tries to solve the
# dining philosophers problem with no starvation.
# the first philosopher in the queue is given many
# chances to start eating
#
##############################################

import threading, time, random, os, collections

#Class from downey
class Semaphore(threading._Semaphore):
	wait = threading._Semaphore.acquire
	signal = threading._Semaphore.release

class Thread(threading.Thread):
	def __init__(self, t, *args):
		threading.Thread.__init__(self, target=t, args=args)
		self.start()

#declare global array for threads

#boolean array, n true if philo n is eating
eating = []
#sema array that phlio n waits on for a fork
philos = []
#boolean array, index n is true if philo n is waiting
waiting = []

queue = collections.deque()
baton = Semaphore(1)
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
	i = 0
	while (True):          
		dine(n, left, right)
		i = i + 1
	os._exit(0)

#function for checking assertion
def check(left, right, n, eating):

	if (((eating[left]) or (eating[right])) and (eating[n])):
		print "ERROR"

#called when thread has baton and needs to pass it on
def SIGNAL():
	global queue
	global philos
	global baton

	hasBaton = True

	if (len(queue) > 0):
		philos[queue.popleft()].signal()
		hasBaton = False
	if (hasBaton):
		baton.signal()		

def dine(n, left, right):
	global eating	
	global baton
	global philos
	global waiting
	global total
	global queue

	#Think
	time.sleep(random.random())

	check(left, right, n, eating)

	#enter mutex
	baton.wait()
	check(left, right, n, eating)

	if ((eating[left]) or (eating[right])):
		waiting[n] = True
		#enter FIFO queue
		queue.append(n)
		#print(str(n) + " has entered queue")
		baton.signal()
		while (waiting[n]):
			philos[n].wait()
			#put self back in front queue
			queue.appendleft(n)			
			if ((not eating[left]) and (not eating[right])):
				waiting[n] = False
				eating[n] = True
				#remove self from queue
				queue.popleft()
				#print(str(n) + " has left queue")
				#print(str(n) + " is eating")
			baton.signal()
	else:
		eating[n] = True
		print(str(n) + " is eating")
		SIGNAL() #pass baton

	#exited mutex

	check(left, right, n, eating)

	#eat
	time.sleep(random.random())

	#enter mutex
	baton.wait()
	eating[n] = False
	#print(str(n) + " is done eating")	

	check(left, right, n, eating)
	
	SIGNAL() #pass baton
	
	check(left, right, n, eating)

#Change num philos here
main(5)
