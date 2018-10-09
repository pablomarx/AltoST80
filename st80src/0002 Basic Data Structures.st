Array subclass: #Interval
	instanceVariableNames: 'start stop step length'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Interval methodsFor: 'Compatibility' stamp: ''!
isIntervalBy1 | |
	^step = 1
! !
!Interval methodsFor: 'Compatibility' stamp: ''!
hash | |
	^(((start lshift: 2) lxor: stop) lshift: 1) lxor: length
! !
!Interval methodsFor: 'Compatibility' stamp: ''!
= int | |
	^start = int start and: [(stop = int stop and: [length = int length])]
! !
!Interval methodsFor: 'Compatibility' stamp: ''!
cansubscript: a | |
	^length ≤ 0 or: [((start cansubscript: a) and: [(length - 1 * step + start cansubscript: a)])]
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
randomInit | |
	self randomInit: user ticks
! !
!Interval methodsFor: 'Random Numbers' stamp: ''!
random | |
	 "See Lehmers linear congruential method, Knuth Vol. 1:
	modulus m=2^16
	a=27181 odd, and 5 = a mod 8
	c=13849 odd, and c/m around 0.21132"
	step ← (13849 + (27181 * step)) asInt16.
	^(start + (length asFloat * (32768.0 + step) / 65536.0)) asSmall
! !
!Interval methodsFor: 'Random Numbers' stamp: ''!
randomInit: x | |
	 "Call with const to get repeatable sequence"
	step ← x "step holds the current state".
	(start is: Float)
	  ifTrue: [length ← stop - start] "for Float intervals"
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
Stream subclass: #Set
	instanceVariableNames: 'views'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Set methodsFor: 'Arithmetic operations' stamp: ''!
summation | i summation |
	summation ← 0.0 "sum of the values in the set".
	(1 to: position) do: [:i | summation ← summation + (self ◦ i)].
	^summation
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
product: s | product i |
	product ← Set new "product of two sets ... sets must be of equal length" default.
	self length = s length
	  ifTrue:
		[(1 to: position) do: [:i | product add: s ◦ i * (self ◦ i)].
		^product].
	user notify: 'product undefined...sets are not of equal length'
! !
!Set methodsFor: 'Index operations' stamp: ''!
deleteI: i to: j | n k |
	n ← j - i + 1.
	(i to: position - n) do: [:k | array ◦ k ← array ◦ (k + n)].
	(position - n + 1 to: position) do: [:k | array ◦ k ← nil].
	position ← position - n
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
deleteI: i | v j |
	v ← self ◦ i.
	(i to: position - 1) do: [:j | array ◦ j ← array ◦ (j + 1)].
	array ◦ position ← nil.
	position ← position - 1.
	^v
! !
!Set methodsFor: 'Index operations' stamp: ''!
◦ i | |
	^array ◦ i "self checkIndex:"
! !
!Set methodsFor: 'Index operations' stamp: ''!
◦ i ← val | |
	position + 1 = i
	  ifTrue: [self next← val]
	  ifFalse: [^array ◦ i ← val "self checkIndex:"]
! !
!Set methodsFor: 'Initialization' stamp: ''!
string: t1 | |
	limit ← t1.
	self of: (String new: limit)
! !
!Set methodsFor: 'Initialization' stamp: ''!
vector: t1 | |
	limit ← t1.
	self of: (Vector new: limit)
! !
!Set methodsFor: 'Initialization' stamp: ''!
default | |
	self vector: 8
! !
!Set methodsFor: 'Initialization' stamp: ''!
of: t1 to: t2 | |
	array ← t1.
	position ← t2.
	limit ← array length
! !
!Set methodsFor: 'Private' stamp: ''!
grow | |
	^self growby: (10 max: limit / 4 "self grown and reset. returns another Set with old contents")
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
next | |
	user notify: 'no direct reading of a Set'
! !
!Set methodsFor: 'Private' stamp: ''!
growby: n | old |
	old ← Set new "grow and reset self. return old Set for copying" of: array to: position.
	self of: (array species new: limit + n) to: 0.
	^old
! !
!Set methodsFor: 'Value operations' stamp: ''!
delete: x | i |
	(1 to: position) do:
		[:i | array ◦ i ≡ x
		  ifTrue: [^self deleteI: i]].
	^false
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
has: x | |
	^(self find: x) > 0
! !
!Set methodsFor: 'Value operations' stamp: ''!
append: x | |
	x do: [:x | self next← x]
! !
!Set methodsFor: 'Value operations' stamp: ''!
add: x | |
	self next← x
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
viewer | |
	^SetReader new of: array from: 1 to: position "self viewRange: 1 to: position"
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
viewRange: i to: j | |
	^SetReader new "self viewer:" of: array from: i to: j "max: 1" "min: position"
! !
!Set methodsFor: 'Viewing' stamp: ''!
asStream | |
	^self viewer
! !
!Set methodsFor: 'Viewing' stamp: ''!
species | |
	^array species
! !
!Set methodsFor: 'Viewing' stamp: ''!
copy | |
	^self viewer copy
! !
!Set methodsFor: 'Viewing' stamp: ''!
length | |
	^position
! !
!Set methodsFor: 'Viewing' stamp: ''!
asSet | |
	
! !
!Set methodsFor: 'Viewing' stamp: ''!
viewer: v | |
	views ≡ nil
	  ifTrue: [views ← Set default].
	views next← v
! !
Object subclass: #Stream
	instanceVariableNames: 'array position limit'
	classVariableNames: ''
	category: 'Basic Data Structures'!
!Stream methodsFor: 'As yet unclassified' stamp: ''!
do: aBlock | |
	[self end] whileFalseDo:  [aBlock value: self next].
	^nil
! !
!Stream methodsFor: 'As yet unclassified' stamp: ''!
reverseContents | a s i |
	a ← array species new: position.
	s ← a asStream.
	(position to: 1 by: ¬1) do: [:i | s next← array ◦ i].
	^a
! !
!Stream methodsFor: 'Character printing' stamp: ''!
print: obj | |
	obj printOn: self
! !
!Stream methodsFor: 'Character printing' stamp: ''!
crtab: n | i |
	self next← 13.
	(1 to: n) do: [:i | self next← 9]
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
tab | |
	self next← 9
! !
!Stream methodsFor: 'Character printing' stamp: ''!
cr | |
	self next← 13
! !
!Stream methodsFor: 'Coercions' stamp: ''!
asReadStream | |
	^Stream new "an alternative to Set/SetReader.
	create another Stream which reads the contents of this one" of: array from: 1 to: position
! !
!Stream methodsFor: 'Coercions' stamp: ''!
viewer | |
	^SetReader new of: array from: 1 to: position
! !
!Stream methodsFor: 'Coercions' stamp: ''!
asArray | |
	^array
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
release | |
	array ← nil
! !
!Stream methodsFor: 'Initialization' stamp: ''!
close | |
	limit ← position.
	position ← 0
! !
!Stream methodsFor: 'Initialization' stamp: ''!
of: t1 | |
	array ← t1.
	position ← 0.
	limit ← array length
! !
!Stream methodsFor: 'Initialization' stamp: ''!
default | |
	self of: (String new: 16)
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
nextPoint← p | |
	self nextword← p x.
	self nextword← p y
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextPoint | x |
	x ← self nextword.
	^Point new x: x y: self nextword
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
padNext | |
	position even "make position even (on word boundary), returning padding character if any"
	  ifTrue: [^false].
	^self next
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextElement: element | |
	self next← element
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
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next: n ← v | |
	(1 to: n) do: [:n | self next← v]
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next: n | |
	^self into: (array species new: n) endError: true
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next← x | |<primitive: 21>
	 "simple arg"
	self myend
	  ifTrue: [^self pastend← x].
	^array ◦ (position ← position + 1) ← x
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
upto: x | y s |
	s ← (String new: 250) asStream.
	self do:
		[:y | y = x
		  ifTrue: [^s contents].
		s next← y].
	^s contents
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
into: x | |
	^self into: x endError: true "generate an error if the Stream is exhausted before x is filled"
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
next | |<primitive: 20>
	 "simple result"
	self myend
	  ifTrue: [^self pastend].
	^array ◦ (position ← position + 1)
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
integerScan | sign base maxdigit c val |
	sign ←  "get the next Integer or LargeInteger (Float?) from a Stream.
	copied from String asInteger"
			(self ∢ 21
			  ifTrue: [¬1]
			  ifFalse: [1]).
	base ← (self ∢ 48
			  ifTrue: [8]
			  ifFalse: [10]).
	maxdigit ← 48 + base.
	val ← 0.
	[(c ← self next) and: [(c ≥ 48 and: [c < maxdigit])]] whileTrueDo: [val ← val * base + (c - 48)].
	c
	  ifTrue: [self skip: ¬1].
	(base = 8 and: [(val > 32767 and: [(sign = 1 and: [val < 65536])])])
	  ifTrue: [^val asSmall].
	^(val * sign) asInteger
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
padNext← c | |
	position even
	  ifTrue: [^false].
	^self next← c
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
next: n from: strm | |
	(1 to: n) do: [:n | self next← strm next]
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextword← val | |
	self next← val lshift: ¬8.
	self next← val land: 255.
	^val
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
append: x | i |
	 "Array arg"
	x do: [:i | self next← i].
	^x
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
pop: n | t |
	position < n
	  ifTrue: [^false].
	t ← self last: n.
	position ← position - n.
	^t
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
nextString | len |
	^self into: (String new: ((len ← self next) < 192
	  ifTrue: [len]
	  ifFalse: [ "up to 191 chars (BCPL compat)"
		len - 192 * 256 + self next])) endError: true "up to 16383 chars"
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextNumber: n | i s t |
	s ← false "return next n characters s as a positive Integer or LargeInteger" "scan for first non-zero byte, then collect rest appropriately".
	(1 to: n) do:
		[:i | t ← self next.
		s
		  ifTrue: [s ◦ (n + 1 "more LargeInteger: reverse order of significance" - i) ← t]
		  ifFalse:
			[i = n
			  ifTrue: [^t].
			t = 0
			  ifFalse:
				[(i ≤ (n - 2) or: [(t land: 128) "i=n-1" ≠ 0])
				  ifTrue:
					[s ← Natural new: n + 1 "LargeInteger of 2 or more bytes" - i.
					s last← t]
				  ifFalse: [ "positive Integer"
					^(t lshift: 8) + self next]]]].
	^LargeInteger new bytes: s neg: false
! !
!Stream methodsFor: 'Sequential reading and writing' stamp: ''!
nextNumber: n ← v | vlen |
	v ← v bytes "write a positive SmallInteger or LargeInteger as n characters".
	vlen ← v length.
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
			self append: v reverse]]
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
first | |
	position ≠ 0
	  ifTrue: [^array ◦ 1]
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
last: n | |
	^(array ◦ (position - n + 1 to: position)) copy
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
contents | |
	^array copy: 1 to: position
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
last | |
	position ≠ 0
	  ifTrue: [^array ◦ position]
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
rest | |
	^array copy: position + 1 to: limit
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
!Stream methodsFor: 'Static reading and writing' stamp: ''!
◦ x | |
	^array ◦ x
! !
!Stream methodsFor: 'Static reading and writing' stamp: ''!
◦ x ← val | |
	^array ◦ x ← val
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
wordposition | |
	^self position / 2
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
skipwords: w | |
	self skip: 2 * w
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
limit | |
	^limit
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
position | |
	^position
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
empty | |
	 "for"
	^position = 0
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
pastend | |
	^false
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
reset | |
	position ← 0
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
pastend← x | |
	array ← array grow.
	limit ← array length.
	^self next← x
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
settoend | |
	position ← limit
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
myend | |
	^position ≥ limit
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
skipTo: x | y |
	self do:
		[:y | y = x
		  ifTrue: [^true]].
	^false
! !
!Stream methodsFor: 'Test and alter position' stamp: ''!
loc | |
	 "synonym for compiler"
	^position
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
wordposition← w | |
	self position← w * 2
! !
Array subclass: #String
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Basic Data Structures'!
!String methodsFor: 'As yet unclassified' stamp: ''!
stringhash | |
	^self hash
! !
!String methodsFor: 'As yet unclassified' stamp: ''!
setSourcePosition: pos inFile: f | loc p1 |
	 "set last three bytes to be pos in file (0-3)"
	f > 3
	  ifTrue: [user notify: 'invalid file number']
	  ifFalse:
		[loc ← self length - 2.
		self ◦ (loc + 2) ← f * 64 + (p1 ← pos / 65536).
		pos ← pos - (p1 * 65536) "into two bytes".
		self ◦ (loc + 1) ← p1 ← pos / 256.
		pos ← pos land: 255 "into one byte".
		self ◦ loc ← pos]
! !
!String methodsFor: 'As yet unclassified' stamp: ''!
alignForDisplay | |<primitive: 84>
	
! !
!String methodsFor: 'As yet unclassified' stamp: ''!
inBase: b | neg i j large large10 |
	 "convert to a LargeInteger"
	self ◦ 1 = 21
	  ifTrue: [neg ← true]
	  ifFalse: [neg ← false].
	large ← 0 asLarge.
	large10 ← b asLarge.
	((neg
	  ifTrue: [2]
	  ifFalse: [1]) to: self length) do: [:i | large ← large * large10 + ((j ← self ◦ i) ≥ 65 "A"
				  ifTrue: [j - 65 + 10]
				  ifFalse: [j - 48 "0"])].
	neg
	  ifTrue: [^large negated].
	^large
! !
!String methodsFor: 'As yet unclassified' stamp: ''!
asCompiledMethod | |
	^self copyto: (CompiledMethod new: self length)
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
- s | i c ldiff |
	(1 to:  "Return a negative, zero, or positive integer as I compare < = or > s" "The collation sequence is ascii with case differences ignored."
	((ldiff ← self length - s length) < 0
	  ifTrue: [self length]
	  ifFalse: [s length])) do:
		[:i | (c ← UpperCase ◦ (self ◦ i + 1) - (UpperCase ◦ (s ◦ i + 1))) ≠ 0
		  ifTrue: [^c]].
	^ldiff
! !
!String methodsFor: 'Comparison' stamp: ''!
< s | |
	^(self compare: s) "Return true iff I collate before s.  The collation sequence is ascii with case differences ignored." = 1
! !
!String methodsFor: 'Comparison' stamp: ''!
> s | |
	^(self compare: s) "Return true iff I collate after s.  The collation sequence is ascii with case differences ignored." = 3
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
!String methodsFor: 'Compatibility' stamp: ''!
species | |
	^String
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
asFileName | |
	^dp0 checkName: self fixing: true
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
asBitmap | t |
	t ← Bitmap new: self length / 2.
	^t fromString: self
! !
!String methodsFor: 'Conversion' stamp: ''!
unique | u |
	 "copy and intern"
	^#a intern: self
! !
!String methodsFor: 'Conversion' stamp: ''!
base8: i | |
	 "word: i  in base 8 as a String"
	^(self word: i) base8
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
asLarge | neg i large large10 |
	 "convert to a LargeInteger"
	self ◦ 1 = 21
	  ifTrue: [neg ← true]
	  ifFalse: [neg ← false].
	large ← 0 asLarge.
	large10 ← 10 asLarge.
	((neg
	  ifTrue: [2]
	  ifFalse: [1]) to: self length) do: [:i | large ← large * large10 + (self ◦ i - 48)].
	neg
	  ifTrue: [^large negated].
	^large
! !
!String methodsFor: 'Conversion' stamp: ''!
asVector | |
	^self asStream asVector
! !
!String methodsFor: 'Conversion' stamp: ''!
hasBeenUniqued | |
	^#a hasInterned: self
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
asUppercase | s c |
	s ← Stream default.
	self do: [:c | s next← UpperCase ◦ (c + 1)].
	^s contents
! !
!String methodsFor: 'Conversion' stamp: ''!
asParagraph | |
	^Paragraph new text: self alignment: 0
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
growto: n | len |
	(len ← self length) ≤ n
	  ifFalse: [len ← n].
	^(self species new: n) copy: 1 to: len with: self from: 1 to: len
! !
!String methodsFor: 'Copying and Altering' stamp: ''!
recopy | |
	^self copy
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
replace: a to: b by: s | |
	(s Is: String)
	  ifTrue: [^self replace: a to: b by: s from: 1 to: s length].
	^self replace: a to: b by: s asArray from: 1 to: s position
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
doubleWordAt: i ← value | w |
	"Primarily for setting socket #s in Pup headers"
	w ← value bytes.
	self◦i ← w◦4.
	self◦(i+1) ← w◦3.
	self◦(i+2) ← w◦2.
	self◦(i+3) ← w◦1.
	^value
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
unsignedWord: x | |
	 "Will replace word: someday"
	^self◦(x+x+1)*256 + (self◦(x+x))
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
all← val | |
	self fill: 1 to: self length with: val
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
fill: a to: b with: val | i |
	(a to: b) do: [:i |  "eventually use BitBlt?"
		self ◦ i ← val]
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
word: x ← y | |
	 "write word in String"
	self ◦ (x + x - 1) ← y lshift: ¬8.
	self ◦ (x + x) ← y land: 255.
	^y
! !
!String methodsFor: 'Reading and Writing' stamp: ''!
word: x | |
	 "read word in String"
	^self ◦ (x + x) + (self ◦ (x + x - 1) lshift: 8)
! !
String subclass: #UniqueString
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Basic Data Structures'!
!UniqueString methodsFor: 'Comparison' stamp: ''!
hash | |<primitive: 39>
	
! !
!UniqueString methodsFor: 'Comparison' stamp: ''!
stringhash | |
	^super hash
! !
!UniqueString methodsFor: 'Comparison' stamp: ''!
= x | |
	^self ≡ x
! !
!UniqueString methodsFor: 'Compatibility' stamp: ''!
species | |
	^String
! !
!UniqueString methodsFor: 'Compatibility' stamp: ''!
copy | |
	^self
! !
!UniqueString methodsFor: 'Compatibility' stamp: ''!
recopy | |
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
str: s | j |
	(1 to: s length) do: [:j | super ◦ j ← s ◦ j].
	^self
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
unique | |
	
! !
!UniqueString methodsFor: 'Reading and Writing' stamp: ''!
◦ x ← val | |
	user notify: 'UniqueStrings are not for writing into'
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
isuneval | x |
	 "ends with open colon"
	^self ◦ self length = 3
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
isarrow | |
	 "ends with ←"
	self length ≤ 1
	  ifTrue: [^false].
	^self ◦ self length = 95
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
mustTake: nargs | |
	 "fatal error if I am not a selector that takes nargs arguments"
	self numArgs ≠ nargs
	  ifTrue: [user notify: self + ' does not take ' + nargs asString + ' arguments']
! !
!UniqueString methodsFor: 'Selectors' stamp: ''!
isinfix | x |
	self length ≠ 1
	  ifTrue: [^false].
	^(self ◦ 1) isletter ≡ false
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
!Vector methodsFor: 'As yet unclassified' stamp: ''!
hex | s y |
	s ← (String new: 4 * self length) asStream.
	self do: [:y | s append: y hex].
	^s contents
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
firstPush | |
	^(self ◦ 1) firstPush
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
emitForValue: code on: stack | x |
	self do: [:x | x emitForValue: code on: stack]
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
sizeForValue | size x |
	size ← 0.
	self do: [:x | size ← size + x sizeForValue].
	^size
! !
!Vector methodsFor: 'Compiler argument list' stamp: ''!
remote: generator | x |
	self do: [:x | x remote: generator]
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
