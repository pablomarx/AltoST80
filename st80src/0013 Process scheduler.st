Object subclass: #LinkedList
	instanceVariableNames: 'firstLink lastLink'
	classVariableNames: ''
	category: 'Process scheduler'!
!LinkedList methodsFor: 'Instance Life Cycle' stamp: ''!
init | |
	 firstLink ← nil.
	 lastLink ← nil
! !
!LinkedList methodsFor: 'Link Deletion' stamp: ''!
removeLink: oldLink | aLink |
	 oldLink==firstLink
		ifTrue:
			[firstLink ← oldLink nextLink.
			oldLink==lastLink ifTrue: [lastLink ← nil]]
		ifFalse:
			[aLink ← firstLink.
			[aLink nextLink==oldLink] whileFalse: [aLink ← aLink nextLink].
			aLink nextLink: oldLink nextlink.
			oldLink==lastLink ifTrue: [lastLink ← aLink]].
	 oldLink nextLink: nil
! !
!LinkedList methodsFor: 'Link Deletion' stamp: ''!
removeFirstLink | oldLink |
	 self isEmpty ifTrue: [user notify: 'LinkedList is empty'].
	 oldLink ← firstLink.
	 firstLink==lastLink
		ifTrue: [firstLink ← nil.  lastLink ← nil]
		ifFalse: [firstLink ← oldLink nextLink].
	 oldLink nextLink: nil.
	 ^oldLink
! !
!LinkedList methodsFor: 'Link Deletion' stamp: ''!
removeLastLink | oldLink aLink |
	 self isEmpty ifTrue: [user notify: 'LinkedList is empty'].
	 oldLink ← lastLink.
	 firstLink==lastLink
		ifTrue: [firstLink ← nil.  lastLink ← nil]
		ifFalse:
			[aLink ← firstLink.
			[aLink nextLink==oldLink] whileFalse: [aLink ← aLink nextLink].
			aLink nextLink: nil.
			lastLink ← aLink].
	 oldLink nextLink: nil.
	 ^oldLink
! !
!LinkedList methodsFor: 'Link Insertion' stamp: ''!
addLastLink: aLink | |
	 self isEmpty
		ifTrue: [firstLink ← aLink]
		ifFalse: [lastLink nextLink: aLink].
	 aLink nextLink: nil.
	 lastLink ← aLink
! !
!LinkedList methodsFor: 'Link Insertion' stamp: ''!
addFirstLink: aLink | |
	 self isEmpty ifTrue: [lastLink ← aLink].
	 aLink nextLink: firstLink.
	 firstLink ← aLink
! !
!LinkedList methodsFor: 'Testing' stamp: ''!
isEmpty | | ^firstLink==nil
! !
Link subclass: #Process
	instanceVariableNames: 'suspendedContext priority myList'
	classVariableNames: ''
	category: 'Process scheduler'!
!Process methodsFor: 'Access to Parts' stamp: ''!
suspendedContext: aContext | | suspendedContext ← aContext
! !
!Process methodsFor: 'Access to Parts' stamp: ''!
suspendedContext | | ^suspendedContext
! !
!Process methodsFor: 'Access to Parts' stamp: ''!
priority: anInteger | | priority ← anInteger
! !
!Process methodsFor: 'Access to Parts' stamp: ''!
priority | | ^priority
! !
!Process methodsFor: 'Access to Parts' stamp: ''!
onList: aLinkedList | | myList ← aLinkedList
! !
!Process methodsFor: 'Access to Parts' stamp: ''!
offList | | myList ← nil
! !
!Process methodsFor: 'Initialization' stamp: ''!
forContext: aContext priority: anInteger | |
	 self suspendedContext: aContext.
	 self priority: anInteger
! !
!Process methodsFor: 'Termination' stamp: ''!
terminate | |
	 Processor activeProcess==self
		ifTrue:
			[thisContext sender==nil
				ifFalse: [thisContext sender release].
			Processor suspend]
		ifFalse:
			[self suspendedContext sender==nil
				ifFalse: [self suspendedContext sender release].
			myList removeLink: self]
! !
Object subclass: #ProcessorScheduler
	instanceVariableNames: 'quiescentProcessLists activeProcess'
	classVariableNames: ''
	category: 'Process scheduler'!
!ProcessorScheduler methodsFor: 'Access to Parts' stamp: ''!
activeProcess | | ^activeProcess
! !
!ProcessorScheduler methodsFor: 'Access to Parts' stamp: ''!
activePriority | | ^activeProcess priority
! !
!ProcessorScheduler methodsFor: 'Critical sections' stamp: ''!
critical⦂ aBlock | savedPriority value |
	savedPriority ← activeProcess priority.
	activeProcess priority: highestPriority + 1.  "Can't be pre-empted"
	value ← aBlock value.
	activeProcess priority: savedPriority.
	^value
! !
!ProcessorScheduler methodsFor: 'Initialization' stamp: ''!
systemInit |
 |
	"Really create and install the scheduler, and snapshot the result."
	self init.
	activeProcess suspendedContext: thisContext.
	Processor ← self.
	"Do a user snapshot without setting InitialContext, which no longer exists."
	user releaseExternalViews.
	user snapshotPrimitive.
	"Control returns here in the new system with everything as it should be."
	user mapDisplay.
	user restore
! !
!ProcessorScheduler methodsFor: 'Initialization' stamp: ''!
init | |
	 quiescentProcessLists ← Vector new: highestPriority.
	 (1 to: highestPriority)
		do: [:priority |
			quiescentProcessLists◦priority ← LinkedList init].
	 activeProcess ← Process new priority: 2
! !
!ProcessorScheduler methodsFor: 'Initialization' stamp: ''!
classInit | | highestPriority ← 4
! !
!ProcessorScheduler methodsFor: 'List Maintainance' stamp: ''!
wakeHighestPriority | priority processList process | 
	 priority ← highestPriority.
	 [(quiescentProcessLists at: priority) isEmpty]
				whileTrue: [priority ← priority-1].
	 processList ← quiescentProcessLists at: priority.
	 process ← processList removeFirstLink.
	 process offList.
	 ^process
! !
!ProcessorScheduler methodsFor: 'List Maintainance' stamp: ''!
sleep: aProcess | processList | 
	 processList ← quiescentProcessLists at: aProcess priority.
	 processList addLastLink: aProcess.
	 aProcess onList: processList
! !
!ProcessorScheduler methodsFor: 'Primitive Control Transfer' stamp: ''!
transferTo: newProcess | |
	 activeProcess suspendedContext: thisContext sender.
	 activeProcess ← newProcess.
	 thisContext sender← newProcess suspendedContext
! !
!ProcessorScheduler methodsFor: 'Process Resumption and Suspension' stamp: ''!
simulateResume: newProcess | | 
	 (newProcess priority) > (activeProcess priority)
		ifTrue: [self sleep: activeProcess.
			self transferTo: newProcess]
		ifFalse: [self sleep: newProcess]
! !
!ProcessorScheduler methodsFor: 'Process Resumption and Suspension' stamp: ''!
simulateSuspend | oldProcess | 
	 oldProcess ← activeProcess.
	 self transferTo: (self wakeHighestPriority).
	 ^oldProcess
! !
!ProcessorScheduler methodsFor: 'Process Resumption and Suspension' stamp: ''!
suspend | | <primitive: 71>
	user croak
! !
!ProcessorScheduler methodsFor: 'Process Resumption and Suspension' stamp: ''!
resume: newProcess | | <primitive: 70>
	user croak
! !
