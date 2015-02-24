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
delay = Semaphore(0)
total = 0
justAte = -1

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
	while (i < 100):          
		eat(n, left, right)
		i = i + 1
	os._exit(0)

#function for checking assertion
def check(left, right, n):
	global eating
	if (((eating[left]) or (eating[right])) and (eating[n])):
		print "ERROR"

def eat(n, left, right):
	global eating	
	global baton
	global philos
	global waiting
	global total
	global queue
	global justAte

	hasBaton = False

	#time.sleep(random.random())

	check(left, right, n)

	baton.wait()
	hasBaton = True
	check(left, right, n)

	if ((eating[left]) or (eating[right]) or (justAte == n)):
		waiting[n] = True
		#enter FIFO queue
		queue.append(n)
		print(str(n) + " has entered queue")
		hasBaton = False
		baton.signal()
		while (waiting[n]):
			philos[n].wait()
			#put self back in front queue
			queue.appendleft(n)			
			if ((not eating[left]) and (not eating[right]) and (justAte != n)):
				waiting[n] = False
				eating[n] = True
				#remove self from queue
				queue.popleft()
				print(str(n) + " has left queue")
				#print(str(n) + " is eating")
			baton.signal()
	else:
		eating[n] = True
		#print(str(n) + " is eating")
######SIGNAL#############################
		if (len(queue) > 0):
			philos[queue.popleft()].signal()
			hasBaton = False
		if (hasBaton):
			baton.signal()		
			hasBaton = False		
#########################################

	check(left, right, n)

	#time.sleep(random.random())

	baton.wait()
	justAte = n
	hasBaton = True
	eating[n] = False
	#print(str(n) + " is done eating")	

	check(left, right, n)

	#give baton to first in queue
######SIGNAL#############################
	if (len(queue) > 0):
		philos[queue.popleft()].signal()
		hasBaton = False
	if (hasBaton):
		baton.signal()
		hasBaton = False			
#########################################	
	
	check(left, right, n)

#start
main(5)
