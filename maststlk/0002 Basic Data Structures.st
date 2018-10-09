Object subclass: #Array
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Array methodsFor: 'Comparing' stamp: ''!
hash | |
	 "make sure = arrays hash =ly"
	self length = 0
	  ifTrue: [^17171].
	^(self ◦ 1) hash + (self ◦ self length) hash
! !
!Array methodsFor: 'Compatibility' stamp: ''!
isArray | |
	
! !
!Array methodsFor: 'Compatibility' stamp: ''!
species | |
	^Vector
! !
!Array methodsFor: 'Compatibility' stamp: ''!
isIntervalBy1 | |
	^false
! !
!Array methodsFor: 'Conversion' stamp: ''!
asSet | |
	^Set new of: self to: self length
! !
!Array methodsFor: 'Conversion' stamp: ''!
frequencies | d x |
	 "return a sorted vector ((freq item) (freq item) ...)"
	d ← Dictionary new init: 64.
	self do: [:x | d tally: x].
	^d asInvertedVector sort
! !
!Array methodsFor: 'Conversion' stamp: ''!
transform⦂ each to⦂ expr | s i |
	 "a copy of me with each element transformed"
	s ← (self species new: self length) asStream.
	(1 to: self length) do:
		[:i | each value← self ◦ i.
		s next← expr eval].
	^s asArray
! !
!Array methodsFor: 'Conversion' stamp: ''!
sum: aBlock | i len sum |
	len← self length.  i← sum← 0.
	[(i←i+1) ≤ len] whileTrueDo: [sum← sum + (aBlock value: self◦i)].
	^sum
"
user classNames sum: [:name | (Smalltalk◦name) myinstvars length]
"
! !
!Array methodsFor: 'Conversion' stamp: ''!
viewer | |
	^SetReader new of: self
! !
!Array methodsFor: 'Conversion' stamp: ''!
asStream | |
	^Stream new of: self
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
concat: arg | x s |
	x ← self species new: self length + arg length.
	self copyto: (s ← x asStream).
	arg copyto: s.
	^x
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
copyto: t | |
	^self copy: 1 to: self length to: t
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
insertNonDescending: x | |
	 "self is assumed to be sorted"
	^self insertSorted: x
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
copy: a to: b to: t | i s me |
	s ← t asStream.
	me ← Stream new of: self from: a to: b.
	(a to: b) do: [:i |  "general code wont stop at false"
		s next← me next].
	^t
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
copy: a to: b | |
	^self copy: a to: b to: (self species new: b - a + 1)
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
copy | |
	^self copy: 1 to: self length
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
collect: aBlock | i len result |
	result← self species new: (len← self length).
	i←0.
	[(i←i+1) ≤ len]
		whileTrueDo: [result◦i← aBlock value: self◦i].
	^result
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
delete: obj | s each |
	s ← (self species new: self length) asStream.
	self do:
		[:each | obj = each
		  ifFalse: [s next← each]].
	^s contents
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
grow | |
	^self growto: (4 max: self length * 2)
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
replace: a to: b by: s | x xs |
	x ← self species new: self length + s length - (1 + b - a).
	xs ← x asStream.
	self copy: 1 to: a - 1 to: xs.
	s copy: 1 to: s length to: xs.
	self copy: b + 1 to: self length to: xs.
	^x
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
notNil | t i |
	 "copy self (which contains no falses) removing all nils"
	t ← (self species new: self length - (self count: nil)) asStream.
	self do:
		[:i | i ≡ nil
		  ifFalse: [t next← i]].
	^t asArray
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
without: index | s me i |
	 "if index in range, return self without ◦index"
	(index cansubscript: self)
	  ifTrue:
		[s ← (self species new: self length - 1) asStream.
		me ← self asStream.
		(1 to: self length) do:
			[:i | i = index
			  ifTrue: [me next]
			  ifFalse: [s next← me next]].
		^s asArray]
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
growby: n | |
	^self growto: self length + n
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
growto: n | |
	^self copy: 1 to: self length "copyto:" to: (self species new: n)
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
do: aBlock | i len |
	len← self length.  i←0.
	[(i←i+1) ≤ len] whileTrueDo: [aBlock value: self◦i]
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
insertSorted: x | a c i |
	 "self is assumed to be sorted"
	i ← self findSorted: x.
	c ← (a ← self species new: self length + 1) asStream.
	self ◦ (1 to: i) copyto: c.
	c next← x.
	self ◦ (i + 1 to: self length) copyto: c.
	^a
! !
!Array methodsFor: 'Copying and Altering' stamp: ''!
+ arg | |
	^self concat: arg
! !
!Array methodsFor: 'Mapping' stamp: ''!
subscripts: x ← val | |
	 "subrange replacement"
	self length ≠ val length
	  ifTrue: [user notify: 'lengths not commensurate']
	  ifFalse:
		[val copyto: (Substring new data: x map: self).
		^val]
! !
!Array methodsFor: 'Mapping' stamp: ''!
subscripts: x | |
	 "subarrays"
	^Substring new data: x map: self
! !
!Array methodsFor: 'Mapping' stamp: ''!
cansubscript: a | i |
	self do:
		[:i | (i cansubscript: a)
		  ifFalse: [^false]]
! !
!Array methodsFor: 'Permutation' stamp: ''!
swap: i with: j | t |
	t ← self ◦ i.
	self ◦ i ← self ◦ j.
	self ◦ j ← t
! !
!Array methodsFor: 'Permutation' stamp: ''!
sort: i to: j | di dij dj tt ij k l n |
	(n ← j + 1 "Sort elements i through j of self to be nondescending." "The prefix d means the data at." - i) ≤ 1
	  ifFalse:
		[ "Nothing to sort." "Sort di,dj."
		di ← self ◦ i.
		dj ← self ◦ j.
		di > dj
		  ifTrue:
			[self swap: i with: j.
			tt ← di.
			di ← dj.
			dj ← tt].
		n = 2
		  ifFalse:
			[ "They are the only two elements."
			ij ← i + j lshift: ¬1 "ij is the midpoint of i and j." "Sort di,dij,dj.  Make dij be their median.".
			dij ← self ◦ ij.
			di > dij
			  ifTrue:
				[self swap: i with: ij.
				dij ← di]
			  ifFalse:
				[dj < dij
				  ifTrue:
					[self swap: j with: ij.
					dij ← dj]].
			n = 3
			  ifFalse:
				[ "They are the only three elements." "Find k>i and l<j such that dk,dij,dl are in reverse order.  Swap k and l.  Repeat this procedure until j and k pass each other."
				k ← i.
				l ← j.
				[[self ◦ (l ← l - 1) > dij] whileTrueDo: [].
				[self ◦ (k ← k + 1) < dij] whileTrueDo: [].
				k ≤ l] whileTrueDo: [self swap: k with: l "Now l<k (either 1 or 2 less), and di through dl are all less than dk through dj.  Sort those two segments."].
				self sort: i to: l.
				self sort: k to: j]]]
! !
!Array methodsFor: 'Permutation' stamp: ''!
permutationToSort | |
	^(self ◦ (1 to: self length "Return a Vector, permutation, such that self◦permutation is sorted nondescending.  Do not alter self.") copy sort: 1 to: self length) map
! !
!Array methodsFor: 'Permutation' stamp: ''!
sort | |
	self sort: 1 to: self length "Permute my elements so they are sorted nondescending.  Note: if I am a substring, only my map will be permuted.  In certain situations, this may not be what you expect."
! !
!Array methodsFor: 'Permutation' stamp: ''!
reverse | |
	^Substring new data: self map: (self length to: 1 by: ¬1)
! !
!Array methodsFor: 'Permutation' stamp: ''!
promote: t | n |
	n ← self find: t.
	n = 0
	  ifFalse:
		[self ◦ (n to: 2 by: ¬1) ← self ◦ (n - 1 to: 1 by: ¬1).
		self ◦ 1 ← t]
! !
!Array methodsFor: 'Reading and Writing' stamp: ''!
all← val | i |
	(1 to: self length) do: [:i | self ◦ i ← val]
! !
!Array methodsFor: 'Reading and Writing' stamp: ''!
first | |
	^self ◦ 1
! !
!Array methodsFor: 'Reading and Writing' stamp: ''!
last← val | |
	^self ◦ self length ← val
! !
!Array methodsFor: 'Reading and Writing' stamp: ''!
last | |
	^self ◦ self length
! !
!Array methodsFor: 'Reading and Writing' stamp: ''!
= arg | x |
	arg isArray
	  ifTrue:
		[self length ≠ arg length
		  ifTrue: [^false].
		(1 to: self length) do:
			[:x | self ◦ x = (arg ◦ x)
			  ifFalse: [^false]].
		^true]
	  ifFalse: [^false]
! !
!Array methodsFor: 'Reading and Writing' stamp: ''!
> v | |
	 "for sorting vectors by first element"
	^self ◦ 1 > (v ◦ 1)
! !
!Array methodsFor: 'Reading and Writing' stamp: ''!
< v | |
	 "for sorting vectors by first element"
	^self ◦ 1 < (v ◦ 1)
! !
!Array methodsFor: 'Searching' stamp: ''!
find: x | i |
	(1 to: self length) do:
		[:i | self ◦ i = x
		  ifTrue: [^i]].
	^0
! !
!Array methodsFor: 'Searching' stamp: ''!
count: x | i n |
	n ← 0.
	(1 to: self length) do:
		[:i | x = (self ◦ i)
		  ifTrue: [n ← n + 1]].
	^n
! !
!Array methodsFor: 'Searching' stamp: ''!
findSorted: x | lo mid hi |
	 " returns index of largest element ≤ x "
	hi ← self length + 1.
	lo ← 1.
	[lo < hi] whileTrueDo:
		[ "binary search; self must be sorted"
		self ◦ (mid ← lo + hi / 2) > x
		  ifTrue: [hi ← mid]
		  ifFalse: [lo ← mid + 1]].
	^hi - 1 " 0≤result≤length "
! !
!Array methodsFor: 'Searching' stamp: ''!
allSuchThat: aBlock | s i len | "a copy of some of me"
	s ← (self species new: self length) asStream.
	i←0. len← self length.
	[(i←i+1) ≤ len]
		whileTrueDo: [(aBlock value: self◦i) ifTrue: [s next← self◦i]].
	^s contents
! !
!Array methodsFor: 'Searching' stamp: ''!
first⦂ x suchThat⦂ predicate | i |
	(1 to: self length) do:
		[:i | x value← self ◦ i.
		predicate eval
		  ifTrue: [^self ◦ i]].
	^false
! !
!Array methodsFor: 'Searching' stamp: ''!
all⦂ variable suchThat⦂ expr | s i x |
	 "a copy of some of me"
	s ← (self species new: self length) asStream.
	(1 to: self length) do:
		[:i | x ← self ◦ i.
		variable value← x.
		expr eval
		  ifTrue: [s next← x]].
	^s contents
! !
!Array methodsFor: 'Searching' stamp: ''!
find⦂ x suchThat⦂ predicate | i |
	(1 to: self length) do:
		[:i | x value← self ◦ i.
		predicate eval
		  ifTrue: [^i]].
	^0
! !
!Array methodsFor: 'Searching' stamp: ''!
findnon: x | i |
	(1 to: self length) do:
		[:i | self ◦ i ≠ x
		  ifTrue: [^i]].
	^0
! !
!Array methodsFor: 'Searching' stamp: ''!
has: x | |
	^(self find: x) ≠ 0
! !
Array subclass: #Interval
	instanceVariableNames: 'start stop step length'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Interval methodsFor: 'Compatibility' stamp: ''!
= int | |
	^start = int start and: [(stop = int stop and: [length = int length])]
! !
!Interval methodsFor: 'Compatibility' stamp: ''!
cansubscript: a | |
	^length ≤ 0 or: [((start cansubscript: a) and: [(length - 1 * step + start cansubscript: a)])]
! !
!Interval methodsFor: 'Compatibility' stamp: ''!
hash | |
	^(((start lshift: 2) lxor: stop) lshift: 1) lxor: length
! !
!Interval methodsFor: 'Compatibility' stamp: ''!
isIntervalBy1 | |
	^step = 1
! !
!Interval methodsFor: 'Initialization' stamp: ''!
from: t1 to: t2 by: t3 | |
	start ← t1.
	stop ← t2.
	step ← t3.
	length ← 1 + (stop - start / step).
	step < 0
	  ifTrue:
		[start < stop
		  ifTrue: [length ← 0]]
	  ifFalse:
		[stop < start
		  ifTrue: [length ← 0]]
! !
!Interval methodsFor: 'Random Numbers' stamp: ''!
random | |  "See Lehmers linear congruential method, Knuth Vol. 1:
	modulus m=2^16   **Hey, lets clean this up some day, Huh? (DI)**
	a=27181 odd, and 5 = a mod 8
	c=13849 odd, and c/m around 0.21132"
	step ← (13849 + (27181 * step)) land: 65535.
	^(start + (length asFloat * (32768.0 + step) / 65536.0)) asInteger-(length/2)

" | a | a← 1 to: 16. (1 to: 20) collect: [:i | a random] "
 
! !
!Interval methodsFor: 'Random Numbers' stamp: ''!
randomInit: x | |
	 "Call with const to get repeatable sequence"
	step ← x "step holds the current state".
	(start is: Float)
	  ifTrue: [length ← stop - start] "for Float intervals"
! !
!Interval methodsFor: 'Random Numbers' stamp: ''!
randomInit | |
	self randomInit: (user ticks land: 8r177777)
! !
!Interval methodsFor: 'Reading and Writing' stamp: ''!
start | |
	^start
! !
!Interval methodsFor: 'Reading and Writing' stamp: ''!
stop | |
	^stop
! !
!Interval methodsFor: 'Reading and Writing' stamp: ''!
length | |
	^length
! !
!Interval methodsFor: 'Reading and Writing' stamp: ''!
◦ x ← val | |
	user notify: 'Intervals are not for writing into'
! !
!Interval methodsFor: 'Reading and Writing' stamp: ''!
◦ x | |
	((x isNumber) and: [x≥1 and: [x≤length]])
	  ifTrue: [^start + (step * (x - 1))]
	  ifFalse: [^super ◦ x]
! !
!Interval methodsFor: 'Reading and Writing' stamp: ''!
do: aBlock | var |
	var← start-step.
	step>0
		ifTrue: [[(var← var+step)≤stop] whileTrueDo: [aBlock value: var]]
		ifFalse: [[(var← var+step)≥stop] whileTrueDo: [aBlock value: var]]
! !
Stream subclass: #Queue
	instanceVariableNames: 'readposition'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Queue methodsFor: 'FIFO access' stamp: ''!
peek | |
	readposition ≥ position
	  ifTrue:
		[readposition ← position ← 0.
		^false].
	^array ◦ (readposition + 1)
! !
!Queue methodsFor: 'FIFO access' stamp: ''!
skip: x | |
	readposition ← readposition + x
! !
!Queue methodsFor: 'FIFO access' stamp: ''!
next | |
	readposition ≥ position
	  ifTrue:
		[readposition ← position ← 0.
		^false].
	^array ◦ (readposition ← readposition + 1)
! !
!Queue methodsFor: 'FIFO access' stamp: ''!
length | |
	^position - readposition
! !
!Queue methodsFor: 'FIFO access' stamp: ''!
dequeue | |
	readposition ≥ position
	  ifTrue:
		[readposition ← position ← 0.
		^false].
	^array ◦ (readposition ← readposition + 1)
! !
!Queue methodsFor: 'FIFO access' stamp: ''!
deQ1 | n |
	 "A noninterruptable dequeue"
	Top critical⦂ [(n ← self dequeue)].
	^n
! !
!Queue methodsFor: 'FIFO access' stamp: ''!
dequeue: num | n |
	position - readposition < num
	  ifTrue: [^false].
	n ← (array ◦ (readposition + 1 to: readposition + num)) copy.
	readposition ← readposition + num.
	^n
! !
!Queue methodsFor: 'FIFO access' stamp: ''!
enQ1: n | |
	 "A noninterruptable enqueue"
	Top critical⦂ [(super next← n)].
	^n
! !
!Queue methodsFor: 'LIFO access' stamp: ''!
push: x | |
	 "treat as LIFO queue"
	readposition > 0
	  ifTrue:
		[array ◦ readposition ← x.
		readposition ← readposition - 1]
	  ifFalse: [ "readpositon > 0, just jam it in"
		self insert: x] "otherwise insert on front"
! !
!Queue methodsFor: 'Stream protocol' stamp: ''!
contents | |
	^(array ◦ (readposition + 1 to: position)) copy
! !
!Queue methodsFor: 'Stream protocol' stamp: ''!
empty | |
	^readposition ≥ position
! !
!Queue methodsFor: 'Stream protocol' stamp: ''!
of: t1 | |
	array ← t1.
	position ← 0.
	readposition ← 0.
	limit ← array length
! !
!Queue methodsFor: 'Stream protocol' stamp: ''!
reset | |
	readposition ← position ← 0
! !
!Queue methodsFor: 'Stream protocol' stamp: ''!
of: t1 from: t2 to: t3 | |
	array ← t1.
	position ← t2.
	limit ← t3.
	user notify: 'of:from:to: is not appropriate for Queues'
! !
!Queue methodsFor: 'Stream protocol' stamp: ''!
end | |
	^readposition ≥ position
! !
!Queue methodsFor: 'Stream protocol' stamp: ''!
pastend← x | n |
	readposition = 0
	  ifTrue: [^super pastend← x].
	n ← position - readposition.
	array ◦ (1 to: n) ← array ◦ (readposition + 1 to: position).
	readposition ← 0.
	position ← n.
	^self next← x
! !
Array subclass: #RunVector
	instanceVariableNames: ' min max starts values offset'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!RunVector methodsFor: 'Reading and writing' stamp: ''!
min | |
	^min
! !
!RunVector methodsFor: 'Reading and writing' stamp: ''!
max | |
	^max
! !
!RunVector methodsFor: 'Reading and writing' stamp: ''!
length | |
	max ≡ nil
	  ifTrue: [^0].
	^max - min + 1
! !
!RunVector methodsFor: 'Reading and writing' stamp: ''!
◦ i ← val | |
	offset ← 0.
	min ≡ nil
	  ifTrue:
		[min ← max ← i.
		starts ← i inVector.
		values ← val inVector]
	  ifFalse:
		[i - 1 ≠ max
		  ifTrue:
			[user notify: 'RunVectors must be loaded sequentially'.
			^val].
		max ← i.
		val = values last
		  ifTrue:
			[offset ← i - starts last.
			^val].
		starts ← {starts , i}.
		values ← {values , val}.
		^val]
! !
!RunVector methodsFor: 'Reading and writing' stamp: ''!
◦ i | index |
	index ← starts findSorted: i.
	offset ← i - (starts ◦ index) "distance into run".
	^values ◦ index
! !
Stream subclass: #Set
	instanceVariableNames: 'views'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Set methodsFor: 'Arithmetic operations' stamp: ''!
product: s | product i |
	product ← Set new "product of two sets ... sets must be of equal length" default.
	self length = s length
	  ifTrue:
		[(1 to: position) do: [:i | product add: s ◦ i * (self ◦ i)].
		^product].
	user notify: 'product undefined...sets are not of equal length'
! !
!Set methodsFor: 'Arithmetic operations' stamp: ''!
dotproduct: s | i dotproduct |
	dotproduct ← 0.0 "dot product of two sets ... sets must be of equal length".
	self length = s length
	  ifTrue:
		[(1 to: position) do: [:i | dotproduct ← dotproduct + (s ◦ i * (self ◦ i))].
		^dotproduct].
	user notify: 'dot product undefined...sets are not of equal length'
! !
!Set methodsFor: 'Arithmetic operations' stamp: ''!
summation | i summation |
	summation ← 0.0 "sum of the values in the set".
	(1 to: position) do: [:i | summation ← summation + (self ◦ i)].
	^summation
! !
!Set methodsFor: 'Index operations' stamp: ''!
◦ i ← val | |
	position + 1 = i
	  ifTrue: [self next← val]
	  ifFalse: [^array ◦ i ← val "self checkIndex:"]
! !
!Set methodsFor: 'Index operations' stamp: ''!
◦ i | |
	^array ◦ i "self checkIndex:"
! !
!Set methodsFor: 'Index operations' stamp: ''!
insertI: i value: v | old j |
	i > position
	  ifTrue: [self next← v]
	  ifFalse:
		[old ← array.
		position = limit
		  ifTrue:
			[limit ← limit + (10 max: limit / 4).
			array ← array species new: limit.
			(1 to: i - 1) do: [:j | array ◦ j ← old ◦ j]].
		(position to: i by: ¬1) do: [:j | array ◦ (j + 1) ← old ◦ j].
		array ◦ i ← v.
		position ← position + 1]
! !
!Set methodsFor: 'Index operations' stamp: ''!
deleteI: i to: j | n k |
	n ← j - i + 1.
	(i to: position - n) do: [:k | array ◦ k ← array ◦ (k + n)].
	(position - n + 1 to: position) do: [:k | array ◦ k ← nil].
	position ← position - n
! !
!Set methodsFor: 'Index operations' stamp: ''!
deleteI: i | v j |
	v ← self ◦ i.
	(i to: position - 1) do: [:j | array ◦ j ← array ◦ (j + 1)].
	array ◦ position ← nil.
	position ← position - 1.
	^v
! !
!Set methodsFor: 'Initialization' stamp: ''!
string: t1 | |
	limit ← t1.
	self of: (String new: limit)
! !
!Set methodsFor: 'Initialization' stamp: ''!
default | |
	self vector: 8
! !
!Set methodsFor: 'Initialization' stamp: ''!
vector: t1 | |
	limit ← t1.
	self of: (Vector new: limit)
! !
!Set methodsFor: 'Initialization' stamp: ''!
of: t1 to: t2 | |
	array ← t1.
	position ← t2.
	limit ← array length
! !
!Set methodsFor: 'Private' stamp: ''!
growby: n | old |
	old ← Set new "grow and reset self. return old Set for copying" of: array to: position.
	self of: (array species new: limit + n) to: 0.
	^old
! !
!Set methodsFor: 'Private' stamp: ''!
next | |
	user notify: 'no direct reading of a Set'
! !
!Set methodsFor: 'Private' stamp: ''!
pastend← x | |
	^self append: self grow.
	self next← x
! !
!Set methodsFor: 'Private' stamp: ''!
checkIndex: i | |
	(i ≥ 1 and: [i ≤ position])
	  ifTrue: [^i].
	^user notify: 'illegal index'
! !
!Set methodsFor: 'Private' stamp: ''!
grow | |
	^self growby: (10 max: limit / 4 "self grown and reset. returns another Set with old contents")
! !
!Set methodsFor: 'Value operations' stamp: ''!
append: x | |
	x do: [:x | self next← x]
! !
!Set methodsFor: 'Value operations' stamp: ''!
find: v | i |
	(1 to: position) do:
		[:i | array ◦ i = v
		  ifTrue: [^i]].
	^0
! !
!Set methodsFor: 'Value operations' stamp: ''!
insert: x | |
	(self find: x) = 0
	  ifTrue: [self next← x]
! !
!Set methodsFor: 'Value operations' stamp: ''!
delete: x | i |
	(1 to: position) do:
		[:i | array ◦ i ≡ x
		  ifTrue: [^self deleteI: i]].
	^false
! !
!Set methodsFor: 'Value operations' stamp: ''!
has: x | |
	^(self find: x) > 0
! !
!Set methodsFor: 'Value operations' stamp: ''!
add: x | |
	self next← x
! !
!Set methodsFor: 'Viewing' stamp: ''!
viewer | |
	^SetReader new of: array from: 1 to: position "self viewRange: 1 to: position"
! !
!Set methodsFor: 'Viewing' stamp: ''!
printOn: strm | t |
	strm append: 'a Set: '.
	(array is: String)
	  ifTrue: [strm append: self]
	  ifFalse: [self do:
			[:t | strm space.
			strm print: t]]
! !
!Set methodsFor: 'Viewing' stamp: ''!
length | |
	^position
! !
!Set methodsFor: 'Viewing' stamp: ''!
notViewed: v | |
	views delete: v.
	views empty
	  ifTrue: [views ← nil]
! !
!Set methodsFor: 'Viewing' stamp: ''!
initView: v | |
	^v of: array to: position
! !
!Set methodsFor: 'Viewing' stamp: ''!
viewer: v | |
	views ≡ nil
	  ifTrue: [views ← Set default].
	views next← v
! !
!Set methodsFor: 'Viewing' stamp: ''!
viewRange: i to: j | |
	^SetReader new "self viewer:" of: array from: i to: j "max: 1" "min: position"
! !
!Set methodsFor: 'Viewing' stamp: ''!
species | |
	^array species
! !
!Set methodsFor: 'Viewing' stamp: ''!
asSet | |
	
! !
!Set methodsFor: 'Viewing' stamp: ''!
copy | |
	^self viewer copy
! !
!Set methodsFor: 'Viewing' stamp: ''!
asStream | |
	^self viewer
! !
Stream subclass: #SetReader
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Basic Data Structures'!
!SetReader methodsFor: 'Initialization' stamp: ''!
of: t1 from: t2 for: n | |
	array ← t1.
	position ← t2.
	position ← position - 1.
	limit ← position + n
! !
!SetReader methodsFor: 'Reading' stamp: ''!
copy | t1 |
	 "yield contents all at once as a Set"
	^(t1 ← Set new) of: (array species new: limit - position).
	t1 append: self
! !
!SetReader methodsFor: 'Reading' stamp: ''!
length | |
	^limit - position "how much left"
! !
!SetReader methodsFor: 'Reading' stamp: ''!
asSet | |
	^self copy
! !
Object subclass: #Stream
	instanceVariableNames: 'array position limit'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Stream methodsFor: 'As yet unclassified' stamp: ''!
reverseContents | a s i |
	a ← array species new: position.
	s ← a asStream.
	(position to: 1 by: ¬1) do: [:i | s next← array ◦ i].
	^a
! !
!Stream methodsFor: 'As yet unclassified' stamp: ''!
do: aBlock | |
	[self end] whileFalseDo:  [aBlock value: self next].
	^nil
! !
!Stream methodsFor: 'Character printing' stamp: ''!
print: obj | |
	obj printOn: self
! !
!Stream methodsFor: 'Character printing' stamp: ''!
cr | |
	self next← 13
! !
!Stream methodsFor: 'Character printing' stamp: ''!
space | |
	self next← 32
! !
!Stream methodsFor: 'Character printing' stamp: ''!
semicrtab | |
	self append: ';
	'
! !
!Stream methodsFor: 'Character printing' stamp: ''!
crtab: n | i |
	self next← 13.
	(1 to: n) do: [:i | self next← 9]
! !
!Stream methodsFor: 'Character printing' stamp: ''!
tab | |
	self next← 9
! !
!Stream methodsFor: 'Coercions' stamp: ''!
asArray | |
	^array
! !
!Stream methodsFor: 'Coercions' stamp: ''!
viewer | |
	^SetReader new of: array from: 1 to: position
! !
!Stream methodsFor: 'Coercions' stamp: ''!
asReadStream | |
	^Stream new "an alternative to Set/SetReader.
	create another Stream which reads the contents of this one" of: array from: 1 to: position
! !
!Stream methodsFor: 'Coercions' stamp: ''!
asStream | |
	
! !
!Stream methodsFor: 'Coercions' stamp: ''!
asVector | |
	 "Convert a string to a vector of tokens"
	^(Reader new of: self) read
! !
!Stream methodsFor: 'Compiler object code' stamp: ''!
emitLong: jmpOrBfp by: dist | |
	dist < 0
	  ifTrue: [dist ← dist + 1024]
	  ifFalse:
		[dist > 1023
		  ifTrue: [dist ← ¬1]
		  ifFalse: [jmpOrBfp ← jmpOrBfp + 4]].
	dist < 0
	  ifTrue: [user notify: 'A block compiles more than 1K bytes of code']
	  ifFalse:
		[self next← dist / 256 + jmpOrBfp.
		self next← dist \ 256]
! !
!Stream methodsFor: 'Initialization' stamp: ''!
of: t1 from: pos to: lim | len |
	array ← t1.
	limit ← (lim > (len ← array length)
			  ifTrue: [len]
			  ifFalse: [lim]).
	position ← (pos ≤ 1
			  ifTrue: [0]
			  ifFalse: [pos - 1])
! !
!Stream methodsFor: 'Initialization' stamp: ''!
release | |
	array ← nil
! !
!Stream methodsFor: 'Initialization' stamp: ''!
close | |
	limit ← position.
	position ← 0
! !
!Stream methodsFor: 'Initialization' stamp: ''!
default | |
	self of: (String new: 16)
! !
!Stream methodsFor: 'Initialization' stamp: ''!
of: t1 | |
	array ← t1.
	position ← 0.
	limit ← array length
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
∢ x | y |
	(y ← self next)
	  ifTrue:
		[ "peek for matching element"
		x = y
		  ifTrue: [^y].
		 "gobble it if found"
		position ← position - 1.
		^false]
	  ifFalse: [^false]
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
append: x | i |
	 "Array arg"
	x do: [:i | self next← i].
	^x
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextElement: element | |
	self next← element
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
into: x | |
	^self into: x endError: true "generate an error if the Stream is exhausted before x is filled"
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
dequeue | |
	 "use it as a FIFO"
	^self dequeue: 1
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
dequeue: n | t |
	position < n
	  ifTrue: [^false].
	t ← (array ◦ (1 to: n)) copy.
	array ◦ (1 to: position - n) ← array ◦ (n + 1 to: position).
	position ← position - n.
	^t
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextString | len |
	^self into: (String new: ((len ← self next) < 192
	  ifTrue: [len]
	  ifFalse: [ "up to 191 chars (BCPL compat)"
		len - 192 * 256 + self next])) endError: true "up to 16383 chars"
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextword | hi lo |
	(hi ← self next)
	  ifTrue:
		[(lo ← self next)
		  ifTrue: [^(hi*256) + lo].
		^false]
	  ifFalse: [^false]
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextPoint | x |
	x ← self nextword.
	^Point new x: x y: self nextword
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
peek | x |
	(x ← self next)
	  ifTrue:
		[position ← position - 1.
		^x].
	 "peek at next element"
	^false
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
pop | |
	 "use it as a LIFO"
	position < 1
	  ifTrue: [^false].
	position ← position - 1.
	^array ◦ (position + 1)
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
integerScan | sign base maxdigit c val |
	 "get the next Integer or LargeInteger (Float?) from a Stream.
	copied from String asInteger"
	sign ← (self ∢ 21  "¬"
			  ifTrue: [¬1]
			  ifFalse: [1]).
	base ← (self ∢ 48  "0"
			  ifTrue: [8]
			  ifFalse: [10]).
	maxdigit ← 48 + base.
	val ← 0.
	[(c ← self next) and: [c ≥ 48 and: [c < maxdigit]]] whileTrueDo:
		[val ← val * base + (c - 48)].
	c
	  ifTrue: [self skip: ¬1].
	(base = 8 and: [val > 32767 and: [sign = 1 and: [val < 65536]]])
	  ifTrue: [^val - 65536].  "Special hack for 16-bit octal numbers"
	^(val * sign) asInteger
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next: n ← v | |
	(1 to: n) do: [:n | self next← v]
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextNumber: n | i s |
	"return next n characters as a positive Integer or LargeInteger"
	n ≤ 2 ifTrue:
		[s ← 0.
		(1 to: n) do: [:i| s ← s * 256 + self next].
		^s].
	s ← LargeInteger new: n.
	(1 to: n) do:
		[:i | s ◦ (n + 1 - i) ← self next].  "reverse order of significance"
	^s asInteger
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextPoint← p | |
	self nextword← p x.
	self nextword← p y
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next: n | |
	^self into: (array species new: n) endError: true
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
upto: x | y s |
	s ← (String new: 250) asStream.
	self do:
		[:y | y = x
		  ifTrue: [^s contents].
		s next← y].
	^s contents
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next← x | |<primitive: 21>
	 "simple arg"
	self myend
	  ifTrue: [^self pastend← x].
	^array ◦ (position ← position + 1) ← x
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
pop: n | t |
	position < n
	  ifTrue: [^false].
	t ← self last: n.
	position ← position - n.
	^t
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next | |<primitive: 20>
	 "simple result"
	self myend
	  ifTrue: [^self pastend].
	^array ◦ (position ← position + 1)
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextword← val | |
	self next← val lshift: ¬8.
	self next← val land: 255.
	^val
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
padNext | |
	position even "make position even (on word boundary), returning padding character if any"
	  ifTrue: [^false].
	^self next
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
padNext← c | |
	position even
	  ifTrue: [^false].
	^self next← c
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextNumber: n ← v | vlen i |
	"write a positive SmallInteger or LargeInteger as n characters"
	vlen ← v digitLength.
	n < vlen
	  ifTrue: [user notify: 'number too big']
	  ifFalse:
		[n > vlen
		  ifTrue: [self next: (n - vlen) "pad beginning with 0's" ← 0]].
	vlen = 1
	  ifTrue: [self next← v]
	  ifFalse:
		[(vlen = 2 and: [v is: SmallInteger])
		  ifTrue: [self nextword← v]
		  ifFalse: [ "LargeInteger (assume pos, no negative convention)"
			(1 to: vlen) do: [:i| self next ← v ◦ (vlen + 1 - i)]]]
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextString← s | len |
	(len ← s length) < 192
	  ifTrue: [self next← len]
	  ifFalse:
		[self next← len / 256 + 192.
		self next← len \ 256].
	self append: s.
	^s
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next: n from: strm | |
	(1 to: n) do: [:n | self next← strm next]
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
into: x endError: err | i t len |
	i ← 0 "Array result".
	len ← x length "read until count or stream is exhausted".
	[i < len and: [(t ← self next)]] whileTrueDo: [x ◦ (i ← i + 1) ← t].
	err
	  ifTrue:
		[t
		  ifTrue: [^x].
		user notify: 'only read first ' + i asString]
	  ifFalse: [ "return number that were read"
		^i]
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
rest | |
	^array copy: position + 1 to: limit
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
first | |
	position ≠ 0
	  ifTrue: [^array ◦ 1]
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
◦ x ← val | |
	^array ◦ x ← val
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
◦ x | |
	^array ◦ x
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
last | |
	position ≠ 0
	  ifTrue: [^array ◦ position]
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
contents | |
	^array copy: 1 to: position
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
last: n | |
	^(array ◦ (position - n + 1 to: position)) copy
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
insert: x | i |
	 "treat as LIFO queue, insert in front"
	 "grow array if necessary"
	position = limit
	  ifTrue:
		[array ← array grow.
		limit ← array length].
	(1 to: position) do: [:i | array ◦ (position - i + 2) ← array ◦ (position - i + 1)].
	array ◦ 1 ← x.
	position ← position + 1
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
wordposition | |
	^self position / 2
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
empty | |
	 "for"
	^position = 0
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
reset | |
	position ← 0
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
wordposition← w | |
	self position← w * 2
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
settoend | |
	position ← limit
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
limit | |
	^limit
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
skipTo: x | y |
	self do:
		[:y | y = x
		  ifTrue: [^true]].
	^false
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
skip: x | |
	position ← position + x
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
limit: t1 | |
	limit ← t1
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
skipwords: w | |
	self skip: 2 * w
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
end | |
	^position ≥ limit
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
position← t1 | |
	position ← t1
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
myend | |
	^position ≥ limit
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
pastend← x | |
	array ← array grow.
	limit ← array length.
	^self next← x
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
position | |
	^position
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
loc | |
	 "synonym for compiler"
	^position
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
pastend | |
	^false
! !
Array subclass: #String
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Basic Data Structures'!
!String methodsFor: 'Comparison' stamp: ''!
match: text | star pound pattern scanning p t back |
	star ← 42 "*".
	pound ← 35 "#".
	pattern ← self asStream.
	text ← text asStream.
	scanning ← false.
	[true] whileTrueDo:
		[(p ← pattern next) = star
		  ifTrue:
			[pattern end
			  ifTrue: [^true].
			scanning ← pattern position]
		  ifFalse:
			[(t ← text next) ≡ false
			  ifTrue: [^t ≡ p].
			p ≡ false
			  ifTrue:
				[scanning
				  ifTrue:
					[back ← scanning - pattern position.
					pattern skip: back.
					text skip: back]
				  ifFalse: [^false]]
			  ifFalse:
				[(UpperCase ◦ (t + 1) = (UpperCase ◦ (p + 1)) or: [p = pound])
				  ifFalse:
					[scanning
					  ifTrue:
						[back ← scanning - pattern position.
						pattern skip: back.
						text skip: back + 1]
					  ifFalse: [^false]]]]]
! !
!String methodsFor: 'Comparison' stamp: ''!
hash | l m |
	(l ← m ← self length) ≤ 2
	  ifTrue:
		[l = 2
		  ifTrue: [m ← 3]
		  ifFalse:
			[l = 1
			  ifTrue: [^(self ◦ 1 land: 127) * 106].
			^21845]].
	^self ◦ 1 * 48 + (self ◦ (m - 1) + l)
! !
!String methodsFor: 'Comparison' stamp: ''!
systemRehash | dicts d left loop |
	String understands: 'hash | l m
	[[(l← m← self length)≤2⇒
		[l=2⇒[m←3]; =1⇒[⇑((self◦1) land: 0177)*0152] ⇑052525]].
	 ⇑(self◦1)*060+(self◦(m-1)+l)]' "change the meaning of hash for Strings" "rehash the atom table".
	#a rehash "rehash all dictionaries which have strings in them".
	dicts ← HashSet allInstances + Dictionary allInstances + SymbolTable allInstances.
	dicts do:
		[:d | left ← d objects asStream.
		loop ← left next.
		[loop] whileTrueDo:
			[(loop is: String)
			  ifTrue:
				[d rehash.
				loop ← false]
			  ifFalse: [loop ← left next]]]
! !
!String methodsFor: 'Comparison' stamp: ''!
compare: s | i len lcomp u1 u2 t7 |
	lcomp ← ((t7 ← self length) < (len ← s length)
			  ifTrue:
				[len ← self length.
				1]
			  ifFalse:
				[t7 = len
				  ifTrue: [2]
				  ifFalse: [3]]).
	(1 to: len) do:
		[:i | (u1 ← UpperCase ◦ (self ◦ i + 1)) = (u2 ← UpperCase ◦ (s ◦ i + 1))
		  ifFalse:
			[u1 < u2
			  ifTrue: [^1].
			^3]].
	^lcomp
! !
!String methodsFor: 'Comparison' stamp: ''!
> s | |
	^(self compare: s) "Return true iff I collate after s.  The collation sequence is ascii with case differences ignored." = 3
! !
!String methodsFor: 'Comparison' stamp: ''!
< s | |
	^(self compare: s) "Return true iff I collate before s.  The collation sequence is ascii with case differences ignored." = 1
! !
!String methodsFor: 'Comparison' stamp: ''!
- s | i c ldiff |
	(1 to:  "Return a negative, zero, or positive integer as I compare < = or > s" "The collation sequence is ascii with case differences ignored."
	((ldiff ← self length - s length) < 0
	  ifTrue: [self length]
	  ifFalse: [s length])) do:
		[:i | (c ← UpperCase ◦ (self ◦ i + 1) - (UpperCase ◦ (s ◦ i + 1))) ≠ 0
		  ifTrue: [^c]].
	^ldiff
! !
!String methodsFor: 'Compatibility' stamp: ''!
species | |
	^String
! !
!String methodsFor: 'Compatibility' stamp: ''!
stringhash | |
	^self hash
! !
!String methodsFor: 'Conversion' stamp: ''!
asFloat | strm int frac exp |
	strm ← Stream new of: self.
	int ← strm upto: 46.
	frac ← strm upto: 101.
	exp ← strm rest asInteger - frac length.
	int ← (int concat: frac) asDecimalDigits asFloat.
	exp = 0
	  ifTrue: [^int].
	exp > 0
	  ifTrue: [^int * (10.0 ipow: exp)].
	.
	^int / (10.0 ipow: 0 - exp)
! !
!String methodsFor: 'Conversion' stamp: ''!
asBitmap | t |
	t ← Bitmap new: self length / 2.
	^t fromString: self
! !
!String methodsFor: 'Conversion' stamp: ''!
asBytes | s c |
	s ← Stream default.
	self do:
		[:c | s append: c base8.
		s space].
	^s contents
! !
!String methodsFor: 'Conversion' stamp: ''!
printOn: strm | x |
	 "print inside string quotes"
	strm next← 39.
	self do:
		[:x | strm next← x.
		x = 39
		  ifTrue: [strm next← x]].
	strm next← 39 "imbedded quotes get doubled"
! !
!String methodsFor: 'Conversion' stamp: ''!
asUppercase | s c |
	s ← Stream default.
	self do: [:c | s next← UpperCase ◦ (c + 1)].
	^s contents
! !
!String methodsFor: 'Conversion' stamp: ''!
asInteger | sign base maxdigit c val i i1 i2 |
	self length = 0
	  ifTrue: [^0].
	i1 ← 1.
	i2 ← self length.
	sign ← (self ◦ 1 = 21
			  ifTrue:
				[i1 ← 2.
				¬1]
			  ifFalse: [1]).
	base ← (self ◦ self length = 72
			  ifTrue:
				[i2 ← i2 - 1.
				16]
			  ifFalse:
				[self ◦ i1 = 48
				  ifTrue: [8]
				  ifFalse: [10]]).
	maxdigit ← (base = 16
			  ifTrue: [70]
			  ifFalse: [47 + base]).
	val ← (self length > 4
			  ifTrue: [0.0]
			  ifFalse: [0]).
	(i1 to: i2) "octal and hex packed into 16-bit Integers" do:
		[:i | c ← self ◦ i.
		(c < 48 or: [c > maxdigit])
		  ifTrue: [user notify: self + ' isn''t a valid Integer']
		  ifFalse: [val ← val * base + (c ≤ 57
					  ifTrue: [c - 48]
					  ifFalse: [c - 55])]].
	val > 32767
	  ifTrue:
		[(base = 8 or: [base = 16])
		  ifTrue:
			[sign = 1
			  ifTrue:
				[val < 65536
				  ifTrue: [^(val - 65536) asInteger].
				user notify: 'Warning: Octal or Hex number exceeds 16 bits']]].
	^(val * sign) asInteger
! !
!String methodsFor: 'Conversion' stamp: ''!
asDecimalDigits | strm sign c val |
	 "Not asInteger, because the result may be a Float if it's too big"
	strm ← Stream new of: self.
	sign ← strm ∢ 21.
	val ← (self length > 4
			  ifTrue: [0.0]
			  ifFalse: [0]).
	strm do:
		[:c | (c < 48 or: [c > 57])
		  ifTrue: [user notify: self + ' isn''t a valid integer']
		  ifFalse: [val ← val * 10 + (c - 48)]].
	sign
	  ifTrue: [^val * ¬1].
	^val
! !
!String methodsFor: 'Conversion' stamp: ''!
asParagraph | |
	^Paragraph new text: self alignment: 0
! !
!String methodsFor: 'Conversion' stamp: ''!
asFileName | |
	^dp0 checkName: self fixing: true
! !
!String methodsFor: 'Conversion' stamp: ''!
unique | u |
	 "copy and intern"
	^#a intern: self
! !
!String methodsFor: 'Conversion' stamp: ''!
asVector | |
	^self asStream asVector
! !
!String methodsFor: 'Conversion' stamp: ''!
hasBeenUniqued | |
	^#a hasInterned: self
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
recopy | |
	^self copy
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
concat: s | len |
	((len ← self length) + s length > 20 and: [(s Is: String)])
	  ifTrue: [^self replace: len + 1 "this concatenates more quickly if BitBlt is used" to: len by: s from: 1 to: s length].
	^super concat: s
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
copy: a to: b | |
	^(self species new: 1 + b - a) copy: 1 to: 1 + b - a with: self from: a to: b
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
copy: a to: b with: s from: c to: d | i |
	((b - a "like replace, except in place. self◦(a to: b) ← s◦(c to: d).
	use BitBlt unless size too small, StringBlter≡false, or index/sizes too large" > 12 and: [StringBlter]) and: [(BitBlt new stringCopy: self from: a to: b with: s from: c to: d)])
	  ifFalse:
		[(self ≡ s and: [(c < a and: [d ≥ a])])
		  ifTrue: [(b - a "overlap of second range with below first in same string.
		copy in reverse order: self◦(b to: a by: ¬1) ← self◦(d to: c by: ¬1)" to: 0 by: ¬1) do: [:i | self ◦ (a + i) ← self ◦ (c + i)]]
		  ifFalse: [s copy: c to: d to: (Stream new of: self from: a to: b)]]
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
replace: a to: b by: s | |
	(s Is: String)
	  ifTrue: [^self replace: a to: b by: s from: 1 to: s length].
	^self replace: a to: b by: s asArray from: 1 to: s position
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
replace: a to: b by: r from: c to: d | s t |
	s ← self species new: self length + (d - c) - (b - a) "use BitBlt unless StringBlter≡false or index/sizes too large".
	(StringBlter and: [(BitBlt new stringReplace: s with: self from: a to: b and: [r] from: c to: d)])
	  ifTrue: [^s].
	 "see Array concat:"
	t ← Stream new of: s.
	self copy: 1 to: a - 1 to: t.
	r copy: c to: d to: t.
	self copy: b + 1 to: self length to: t.
	^s
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
findString: str startingAt: start | i t |
	str length = 0
	  ifTrue: [^0].
	t ← str ◦ 1.
	(start to: self length - str length + 1) do:
		[:i | self ◦ i = t
		  ifTrue:
			[self ◦ (i to: i + str length - 1) = str
			  ifTrue: [^i]]].
	^0
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
growto: n | len |
	(len ← self length) ≤ n
	  ifFalse: [len ← n].
	^(self species new: n) copy: 1 to: len with: self from: 1 to: len
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
subst: repl for: key | key1 i nskip result |
	nskip ← 0.
	key1 ← key ◦ 1.
	result ← Stream default.
	(1 to: self length) do:
		[:i |  " the Boyer Slow string replacement "
		nskip > 0
		  ifTrue: [nskip ← nskip - 1]
		  ifFalse:
			[self ◦ i = key1
			  ifTrue:
				[self ◦ (i to: (self length min: i + key length - 1)) = key
				  ifTrue:
					[result append: repl.
					nskip ← key length - 1]
				  ifFalse: [result next← self ◦ i]]
			  ifFalse: [result next← self ◦ i]]].
	^result contents
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
doubleWordAt: i | b0 b1 b2 w |
	"Primarily for reading socket #s in Pup headers"
	b0 ← self◦i.  b1 ← self◦(i+1).  b2 ← self◦(i+2).  w ← self◦(i+3).
	"Following sequence minimizes LargeInteger arithmetic for small results."
	b2=0 ifFalse: [w ← b2*256 + w].
	b1=0 ifFalse: [w ← b1*65536 + w].
	b0=0 ifFalse: [w ← b0*16777216 + w].
	^w
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
doubleWordAt: i ← value | w |
	"Primarily for setting socket #s in Pup headers"
	w ← value asInteger.
	self◦i ← w◦4.
	self◦(i+1) ← w◦3.
	self◦(i+2) ← w◦2.
	self◦(i+3) ← w◦1.
	^value
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
all← val | |
	self fill: 1 to: self length with: val
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
unsignedWord: x | |  "Will replace word: someday"
	^self◦(x+x-1)*256 + (self◦(x+x))
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
word: x ← y | |
	 "write word in String"
	self ◦ (x + x - 1) ← y lshift: ¬8.
	self ◦ (x + x) ← y land: 255.
	^y
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
fill: a to: b with: val | i |
	(a to: b) do: [:i |  "eventually use BitBlt?"
		self ◦ i ← val]
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
word: x | |
	 "read word in String"
	^self ◦ (x + x) + (self ◦ (x + x - 1) lshift: 8)
! !
Array subclass: #Substring
	instanceVariableNames: 'data map'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Substring methodsFor: 'Compatability' stamp: ''!
species | |
	^data species
! !
!Substring methodsFor: 'Conversion' stamp: ''!
asStream | |
	map isIntervalBy1
	  ifTrue: [ "direct stream for simple substrings"
		^Stream new of: data from: map start to: map stop].
	^Stream new of: self from: 1 to: map length
! !
!Substring methodsFor: 'Copying and Altering' stamp: ''!
swap: i with: j | t |
	t ← map ◦ i "By permuting my map (a writable Array), swap elements i and j.".
	map ◦ i ← map ◦ j.
	map ◦ j ← t
! !
!Substring methodsFor: 'Initialization' stamp: ''!
data: t1 map: t2 | |
	data ← t1.
	map ← t2
! !
!Substring methodsFor: 'Reading and Writing' stamp: ''!
length | |
	^map length
! !
!Substring methodsFor: 'Reading and Writing' stamp: ''!
◦ x ← val | |
	^data ◦ (map ◦ x) ← val
! !
!Substring methodsFor: 'Reading and Writing' stamp: ''!
◦ x | |
	^data ◦ (map ◦ x)
! !
!Substring methodsFor: 'Reading and Writing' stamp: ''!
map | |
	^map "Return my map."
! !
String subclass: #UniqueString
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Basic Data Structures'!
!UniqueString methodsFor: 'Comparison' stamp: ''!
= x | |
	^self ≡ x
! !
!UniqueString methodsFor: 'Comparison' stamp: ''!
hash | |<primitive: 39>
	
! !
!UniqueString methodsFor: 'Comparison' stamp: ''!
stringhash | |
	^super hash
! !
!UniqueString methodsFor: 'Compatibility' stamp: ''!
species | |
	^String
! !
!UniqueString methodsFor: 'Compatibility' stamp: ''!
recopy | |
	^self
! !
!UniqueString methodsFor: 'Compatibility' stamp: ''!
copy | |
	^self
! !
!UniqueString methodsFor: 'Conversion' stamp: ''!
printOn: strm | |
	strm append: self
! !
!UniqueString methodsFor: 'Conversion' stamp: ''!
asString | |
	^super copy
! !
!UniqueString methodsFor: 'Initialization' stamp: ''!
hasInterned: s | i v |
	 "⇑false if String s hasnt been interned, else ⇑s unique"
	s length = 1
	  ifTrue:
		[s ◦ 1 < 128
		  ifTrue: [^UST1 ◦ (s ◦ 1 + 1)]].
	v ← USTable ◦ (s stringhash \ USTable length + 1).
	(1 to: v length) do:
		[:i | v ◦ i ≡ nil
		  ifFalse:
			[s length = (v ◦ i) length
			  ifTrue:
				[s = (v ◦ i)
				  ifTrue: [^v ◦ i]]]].
	^false
! !
!UniqueString methodsFor: 'Initialization' stamp: ''!
intern: s | ustr h |
	(ustr ← self hasInterned: s)
	  ifTrue: [^ustr].
	 "check if already exists"
	ustr ← ((s is: UniqueString)
			  ifTrue: [s]
			  ifFalse: [ "create a new one"
				(UniqueString new: s length) str: s]).
	h ← s stringhash \ USTable length + 1.
	USTable ◦ h ← {USTable ◦ h , ustr}.
	^ustr
! !
!UniqueString methodsFor: 'Initialization' stamp: ''!
unique | |
	
! !
!UniqueString methodsFor: 'Initialization' stamp: ''!
classInit | i a v |
	 "make up table of 1-char atoms"
	v ← Vector new: 128.
	a ← String new: 1.
	(1 to: 128) do:
		[:i | a ◦ 1 ← i - 1.
		v ◦ i ← a unique].
	UST1 ← v
! !
!UniqueString methodsFor: 'Initialization' stamp: ''!
rehash | old s i |
	 " ↪a rehash. "
	old ← USTable.
	USTable ← Vector new: old length.
	(1 to: old length) do:
		[:i | USTable ◦ i ← Vector new: 0.
		old ◦ i do:
			[:s | s ≡ nil
			  ifFalse: [self intern: s]].
		old ◦ i ← nil]
! !
!UniqueString methodsFor: 'Initialization' stamp: ''!
str: s | j |
	(1 to: s length) do: [:j | super ◦ j ← s ◦ j].
	^self
! !
!UniqueString methodsFor: 'Reading and Writing' stamp: ''!
◦ x ← val | |
	user notify: 'UniqueStrings are not for writing into'
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
isarrow | |
	 "ends with ←"
	self length ≤ 1
	  ifTrue: [^false].
	^self ◦ self length = 95
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
keywords | result strm i l char colon ocolon |
	 "return a vector of the keywords that compose me"
	'◦←' = self
	  ifTrue: [^#('◦' '←' )].
	result ← (Vector new: 10) asStream.
	strm ← Stream default.
	colon ← ':' ◦ 1.
	ocolon ← '⦂' ◦ 1.
	i ← 1.
	l ← self length.
	[i ≤ l] whileTrueDo:
		[char ← self ◦ i.
		strm append: char.
		((char = colon or: [char = ocolon]) or: [i = l])
		  ifTrue:
			[result next← strm contents.
			strm reset].
		i ← i + 1].
	^result contents
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
numArgs | len n i t4 |
	 "the number of arguments I take when I am a selector"
	len ← self length.
	len = 1
	  ifTrue: [^(self ◦ 1) isletter
		  ifTrue: [0]
		  ifFalse: [1]].
	n ← 0 "count colons, dots, and arrows".
	(1 to: len) do:
		[:i | (t4 ← self ◦ i) = 58
		  ifTrue: [n ← n + 1]
		  ifFalse:
			[t4 = 3
			  ifTrue: [n ← n + 1]
			  ifFalse:
				[t4 = 95
				  ifTrue: [n ← n + 1]
				  ifFalse:
					[t4 = 7
					  ifTrue: [n ← n + 1]]]]].
	^n
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
isuneval | x |
	 "ends with open colon"
	^self ◦ self length = 3
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
iskeyword | x |
	 "ends with colon"
	self length ≤ 1
	  ifTrue: [^false].
	x ← self ◦ self length.
	x = 58
	  ifTrue: [^true].
	^x = 3
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
isinfix | x |
	self length ≠ 1
	  ifTrue: [^false].
	^(self ◦ 1) isletter ≡ false
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
mustTake: nargs | |
	 "fatal error if I am not a selector that takes nargs arguments"
	self numArgs ≠ nargs
	  ifTrue: [user notify: self + ' does not take ' + nargs asString + ' arguments']
! !
Array subclass: #Vector
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Vector methodsFor: 'As yet unclassified' stamp: ''!
base: b | strm i |
	 "convert vector of numbers to a base"
	strm ← Stream default.
	(1 to: self length) do:
		[:i | strm append: (self ◦ i base: b).
		strm space].
	^strm contents
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
remote: generator | x |
	self do: [:x | x remote: generator]
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
sizeForValue | size x |
	size ← 0.
	self do: [:x | size ← size + x sizeForValue].
	^size
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
emitForValue: code on: stack | x |
	self do: [:x | x emitForValue: code on: stack]
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
firstPush | |
	^(self ◦ 1) firstPush
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
argsOff: stack | |
	stack pop: self length
! !
!Vector methodsFor: 'Conversion' stamp: ''!
printOn: strm | i |
	strm append: '('.
	(1 to: self length) do:
		[:i | strm print: self ◦ i.
		strm space].
	strm append: ')'
! !
!Vector methodsFor: 'Conversion' stamp: ''!
asVector | |
	
! !
!Vector methodsFor: 'Copying and Altering' stamp: ''!
, x | v |
	v ← self growby: 1 "use a stream if youre in a hurry".
	v last← x.
	^v
! !
!Vector methodsFor: 'Searching' stamp: ''!
max | biggest i |
	biggest ← self ◦ 1 "return largest value in a vector".
	(1 to: self length) do:
		[:i | self ◦ i > biggest
		  ifTrue: [biggest ← self ◦ i]].
	^biggest
! !
