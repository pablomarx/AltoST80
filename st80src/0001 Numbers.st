Number subclass: #Float
	instanceVariableNames: ''
	classVariableNames: 'radiansPerDegree ln2 halfpi sqrt2 twopi'
	category: 'Numbers'!
!Float methodsFor: 'Arithmetic' stamp: ''!
≠ aNumber | |<primitive: 56>
	aNumber isNumber ≡ false
	  ifTrue: [^true].
	^self retry: #≠ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
= aNumber | |<primitive: 55>
	aNumber isNumber ≡ false
	  ifTrue: [^false].
	^self retry: #= coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
reciprocal | |
	^1.0 / self
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
≤ aNumber | |<primitive: 53>
	^self retry: #≤ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
≥ aNumber | |<primitive: 54>
	^self retry: #≥ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
* aNumber | |<primitive: 57>
	^self retry: #* coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
- aNumber | |<primitive: 50>
	^self retry: #- coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
+ aNumber | |<primitive: 49>
	^self retry: #+ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
< aNumber | |<primitive: 51>
	^self retry: #< coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
hash | |
	^self instfield: 2  "High bits of fraction"
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
| arg | |
	 "By analogy with integers"
	^(self / arg) ipart * arg
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
/ aNumber | |<primitive: 58>
	^self retry: #/ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
> aNumber | |<primitive: 52>
	^self retry: #> coercing: aNumber
! !
!Float methodsFor: 'Conversion' stamp: ''!
truncated | |<primitive: 60>
	 "Return an Integer = self integerPart"
	^(self / 10000.0) truncated * 10000 + (self \ 10000.0) truncated
! !
!Float methodsFor: 'Conversion' stamp: ''!
readFrom: aStream radix: radix | value exp int neg |
	aStream ← aStream asStream.
	radix ← radix asFloat.
	value ← 0.0.
	exp ← 0.
	neg ← (aStream ∢ 45 "-" ifTrue: [true] ifFalse: [aStream ∢ 21 "¬"]).
	[aStream end or: [(aStream peek isDigitRadix: radix) ≡ false]] whileFalseDo:  [value ← value * radix + (aStream next - 48)].
	aStream ∢ 46 "."
	  ifTrue: [[aStream end or: [(aStream peek isDigitRadix: radix) ≡ false]] whileFalseDo: 
			[value ← value * radix + (aStream next - 48).
			exp ← exp - 1]].
	aStream ∢ 101 "e"
	  ifTrue:
		[int ← Integer new readFrom: aStream radix: 10.  "exp is always base 10"
		(int is: Error)
		  ifTrue: [^int].
		exp ← exp + int].
	neg
	  ifTrue: [value ← value negated].
	exp = 0
	  ifTrue: [^value].
	exp > 0
	  ifTrue: [^value * (radix ipow: exp)].
	^value / (radix ipow: exp negated)
! !
!Float methodsFor: 'Conversion' stamp: ''!
minVal | |
	^self maxVal negated
! !
!Float methodsFor: 'Conversion' stamp: ''!
maxMag | |
	self error: 'hash problem'
! !
!Float methodsFor: 'Conversion' stamp: ''!
asFraction | denom |
	 "only use 5 significant figures"
	denom ← (10.0 ipow: 4 - (self abs floorLog: 10.0)) asInteger.
	^(Fraction new numerator: (self * denom) rounded denominator: denom) reduced
! !
!Float methodsFor: 'Conversion' stamp: ''!
asFloat | |
	^self
! !
!Float methodsFor: 'Conversion' stamp: ''!
minMag | |
	self error: 'hash problem'
! !
!Float methodsFor: 'Conversion' stamp: ''!
asInteger | |<primitive: 60>
	 "Return an Integer = self integerPart"
	^(self / 10000.0) asInteger * 10000 + (self \ 10000.0) asInteger
! !
!Float methodsFor: 'Conversion' stamp: ''!
fractionalPart | |<primitive: 59>
	user croak
! !
!Float methodsFor: 'Conversion' stamp: ''!
coerce: aNumber | |
	^aNumber asFloat
! !
!Float methodsFor: 'Conversion' stamp: ''!
generality | |
	^80
! !
!Float methodsFor: 'Conversion' stamp: ''!
maxVal | |
	^self maxMag
! !
!Float methodsFor: 'Conversion' stamp: ''!
readFrom: aStream | value  |
	value ← Number new readFrom: aStream.
	(value is: Error) ifTrue: [^value].
	^value asFloat
! !
!Float methodsFor: 'Conversion' stamp: ''!
ipart | |
	 "Returns a Float with zero fractional part"
	^self - self fpart
! !
!Float methodsFor: 'Conversion' stamp: ''!
recopy | |
	^self
! !
!Float methodsFor: 'Conversion' stamp: ''!
asLarge | me digits nat i |
	 "convert to LargeInteger"
	self < 0
	  ifTrue: [^(0.0 - self) asLarge negated].
	digits ← Stream default.
	self = 0.0
	  ifTrue: [digits next← 0]
	  ifFalse:
		[me ← self ipart.
		[me ≥ 1] whileTrueDo:
			[digits next← (me \ 256.0) asInteger.
			me ← me / 256.0]].
	digits ← digits contents.
	nat ← Natural new: digits length.
	(1 to: digits length) do: [:i | nat ◦ i ← digits ◦ i].
	^LargeInteger new bytes: nat neg: false
! !
!Float methodsFor: 'Conversion' stamp: ''!
copy | |
	^self
! !
!Float methodsFor: 'Conversion' stamp: ''!
fpart | |<primitive: 59>
	user croak
! !
!Float methodsFor: 'Printing' stamp: ''!
printOn: strm digits: digits | |
	 "print me using digits significant figures"
	self > 0.0
	  ifTrue: [self absPrintOn: strm digits: digits]
	  ifFalse:
		[self = 0.0
		  ifTrue: [strm append: '0.0']
		  ifFalse:
			[strm append: '-'.
			self negated absPrintOn: strm digits: digits]]
! !
!Float methodsFor: 'Printing' stamp: ''!
absPrintOn: strm digits: digits | fuzz x exp q i |
	 "print me using digits significant figures"
	"x is myself normalized to [1.0, 10.0), exp is my exponent"
	exp ← (self < 1.0
		  	ifTrue: [(10.0/self floorLog: 10.0) negated]
			ifFalse: [self floorLog: 10.0]).
	x ← self / (10.0 ipow: exp). 
	fuzz ← 10.0 ipow: 1 - digits. 	"round the last digit to be printed"
	x ← 0.5 * fuzz + x.
	x ≥ 10.0					"check if rounding has unnormalized x"
	  ifTrue:
		[x ← x / 10.0.
		exp ← exp + 1].
	(exp < 6 and: [exp > -4])
	  ifTrue:					"decimal notation"
		[q ← 0. 
		exp < 0
		  ifTrue: [strm append: '0.0000' ◦ (1 to: 1 - exp)]]
	  ifFalse:					"scientific notation" 
		[q ← exp.
		exp ← 0].
	[x >= fuzz] whileTrueDo: 		"use fuzz to track significance"
		[i ← x truncated.
		strm next← 48 + i.
		x ← (x - i) * 10.0.
		fuzz ← fuzz * 10.0.
		exp ← exp - 1.
		exp = -1
		  ifTrue: [strm append: '.']].
	[exp ≥ -1] whileTrueDo:
		[strm next← 48.
		exp ← exp - 1.
		exp = -1
		  ifTrue: [strm append: '.']].
	q ≠ 0
	  ifTrue:
		[strm append: 'e'.
		strm print: q]
! !
!Float methodsFor: 'Printing' stamp: ''!
printOn: strm | |
	self printOn: strm digits: 8
! !
Integer subclass: #LargeInteger
	instanceVariableNames: 'bytes	"A Natural number (digits are 0 to 255)"
					 neg	"The sign" '
	classVariableNames: ''
	category: 'Numbers'!
!LargeInteger methodsFor: 'Access' stamp: ''!
hibit | |
	 "Return the index of the high order bit of the binary representation of this number"
	^bytes last hibit + (8 * (bytes length - 1))
! !
!LargeInteger methodsFor: 'Access' stamp: ''!
bit: index | byte |
	 "Return bit number i in the binary representation of this number. Bit number 1 is the low order bit"
	byte ← bytes ◦ (1 + (index - 1 / 8)).
	^(byte lshift: 0 - (index - 1 \ 8)) land: 1
! !
!LargeInteger methodsFor: 'Access' stamp: ''!
bytes: t1 neg: t2 | |
	bytes ← t1.
	neg ← t2.
	[bytes isLarge] whileTrueDo: [bytes ← bytes bytes]
! !
!LargeInteger methodsFor: 'Access' stamp: ''!
neg | |
	^neg
! !
!LargeInteger methodsFor: 'Access' stamp: ''!
neg← t1 | |
	 "Smashes sign - be careful!"
	neg ← t1
! !
!LargeInteger methodsFor: 'Access' stamp: ''!
bytes | |
	^bytes
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
quoRem: aNumber | qr quo rem |
	qr ← bytes natdiv: aNumber bytes.
	quo ← (qr ◦ 1) asInteger.
	rem ← (qr ◦ 2) asInteger.
	qr ◦ 1 ← ((neg ~= aNumber neg)
	  ifTrue: [quo negated]
	  ifFalse: [quo]).
	qr ◦ 2 ← (neg
	  ifTrue: [rem negated]
	  ifFalse: [rem]).
	^qr
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
divMod: aNumber | qr |
	qr ← self quoRem: aNumber.
	(qr ◦ 2 ≠ 0 and: [(neg ~= aNumber neg)])
	  ifTrue:
		[qr ◦ 1 ← qr ◦ 1 - 1.
		qr ◦ 2 ← qr ◦ 2 + aNumber].
	^qr
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
= aNumber | |
	aNumber isNumber ≡ false
	  ifTrue: [^false].
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #= coercing: aNumber].
	^(self compare: aNumber) = 2
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
rem: aNumber | |
	^(self quoRem: aNumber) ◦ 2
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
quo: aNumber | |
	^(self quoRem: aNumber) ◦ 1
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
+ aNumber | as r |
	 "take care of sign. Arithmetic is done in Natural numbers."
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #+ coercing: aNumber].
	as ← aNumber neg.
	neg ≡ as
	  ifTrue:
		[r ← bytes natadd: aNumber bytes.
		^LargeInteger new bytes: r neg: neg].
	r ← bytes natsubtract: aNumber bytes.
	neg
	  ifTrue: [^r negated].
	^r
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
* aNumber | as r |
	 "take care of sign. Arithmetic is done in Natural numbers."
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #* coercing: aNumber].
	as ← aNumber neg.
	r ← bytes nattimes: aNumber bytes.
	^LargeInteger new bytes: r neg: neg ≡ as ≡ false
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
< aNumber | |
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #< coercing: aNumber].
	^(self compare: aNumber) = 1
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
// aNumber | |
	^(self divMod: aNumber) ◦ 1
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
\\ aNumber | |
	^(self divMod: aNumber) ◦ 2
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
coerce: aNumber | |
	^aNumber asLarge
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
generality | |
	^40
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
negated | |
	self = 0
	  ifTrue: [^self].
	^LargeInteger new bytes: bytes neg: neg ≡ false
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
even | |
	^(bytes ◦ 1 land: 1) = 0
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
compare: arg | i a |
	(((bytes length = 1 and: [bytes ◦ 1 = 0]) and: [arg bytes length = 1]) and: [arg bytes ◦ 1 = 0])
	  ifTrue: [^2].
	neg
	  ifTrue:
		[arg neg
		  ifTrue: [^arg bytes natcompare: bytes].
		^1]
	  ifFalse:
		[arg neg
		  ifTrue: [^3].
		^bytes natcompare: arg bytes]
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
intdiv: arg | quo rem ng qr z |
	qr ← bytes natdiv: arg bytes "returns a vector of (quotient, remainder)".
	quo ← qr ◦ 1.
	rem ← (qr ◦ 2) asInteger.
	ng ← neg ≡ arg neg ≡ false.
	quo last = 0
	  ifTrue:
		[quo length < 2
		  ifFalse: [quo ← quo growby: ¬1]].
	qr ◦ 1 ← LargeInteger new bytes: quo neg: ng.
	qr ◦ 2 ← ((ng and: [0 ≠ rem])
	  ifTrue: [arg abs - rem]
	  ifFalse: [rem]).
	^qr
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
\ arg | |
	^(self intdiv: arg) ◦ 2
! !
!LargeInteger methodsFor: 'Arithmetic' stamp: ''!
/ arg | |
	^((self intdiv: arg) ◦ 1) asInteger
! !
!LargeInteger methodsFor: 'As yet unclassified' stamp: ''!
inFourBytes | four i |
	bytes length > 4
	  ifTrue: [user notify: 'wont fit in 4 bytes']
	  ifFalse:
		[four ← String new: 4.
		four all← 0.
		(1 to: bytes length) do: [:i | four ◦ (5 - i) ← bytes ◦ i].
		^four]
! !
!LargeInteger methodsFor: 'As yet unclassified' stamp: ''!
lxor: n | a b c |
	 "simulated 16 bit logic"
	NoteTaker
	  ifTrue:
		[a ← self logicArg bytes.
		b ← n asLarge logicArg bytes.
		c ← Natural new: 2.
		c ◦ 1 ← a ◦ 1 lxor: b ◦ 1.
		c ◦ 2 ← a ◦ 2 lxor: b ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg].
	^self asSmall lxor: n
! !
!LargeInteger methodsFor: 'As yet unclassified' stamp: ''!
logicUnArg | |
	 "convert 2's complement to signed"
	bytes length ≠ 2
	  ifTrue: [user notify: 'not logic arg']
	  ifFalse:
		[bytes ◦ 2 ≥ 128
		  ifTrue: [^(self - 65536) asInteger].
		^self asInteger]
! !
!LargeInteger methodsFor: 'As yet unclassified' stamp: ''!
lor: n | a b c |
	 "simulated 16 bit logic"
	NoteTaker
	  ifTrue:
		[a ← self logicArg bytes.
		b ← n asLarge logicArg bytes.
		c ← Natural new: 2.
		c ◦ 1 ← a ◦ 1 lor: b ◦ 1.
		c ◦ 2 ← a ◦ 2 lor: b ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg].
	^self asSmall lor: n
! !
!LargeInteger methodsFor: 'As yet unclassified' stamp: ''!
bits: left to: right ← val | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width < 15
			  ifTrue: [biton ◦ (width + 1) - 1]
			  ifFalse:
				[width = 15
				  ifTrue: [32767]
				  ifFalse: [¬1]]).
	^(self land: ((mask lshift: 15 - right) lxor: ¬1)) lor: ((val land: mask) lshift: 15 - right)
! !
!LargeInteger methodsFor: 'As yet unclassified' stamp: ''!
logicArg | b a i |
	 "convert to two byte twos complement number"
	neg
	  ifTrue:
		[b ← Natural new: 2.
		b ◦ 1 ← (a ← (bytes ◦ 1 lxor: 255) + 1) land: 255.
		a ← a lshift: ¬8.
		b ◦ 2 ← (bytes ◦ 2 lxor: 255) + a land: 255.
		^LargeInteger new bytes: b neg: false] "if positive, just leave it"
! !
!LargeInteger methodsFor: 'As yet unclassified' stamp: ''!
bits: left to: right | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width < 15
			  ifTrue: [biton ◦ (width + 1) - 1]
			  ifFalse:
				[width = 15
				  ifTrue: [32767]
				  ifFalse: [¬1]]).
	^(self lshift: right - 15) land: mask
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
land: n | a b c |
	 "simulated 16 bit logic"
	NoteTaker
	  ifTrue:
		[a ← self logicArg bytes.
		b ← n asLarge logicArg bytes.
		c ← Natural new: 2.
		c ◦ 1 ← a ◦ 1 land: b ◦ 1.
		c ◦ 2 ← a ◦ 2 land: b ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg].
	^self asSmall land: n
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
anymask: b | |
	^0 ≠ (self land: b)
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
allmask: b | |
	^b = (self land: b)
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
nomask: b | |
	^0 = (self land: b)
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
field: spec ← val | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: (right - (spec / 16) + 1) to: right ← val
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
field: spec | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: right - (spec / 16) + 1 to: right
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
hash | |
	^bytes hash
! !
!LargeInteger methodsFor: 'Bit Manipulation' stamp: ''!
lshift: n | a c s1 s2 h |
	NoteTaker
	  ifTrue:
		[a ← self logicArg.
		c ← Natural new: 2.
		c ◦ 1 ← a bytes ◦ 1.
		c ◦ 2 ← a bytes ◦ 2.
		a bytes: c neg: false.
		(h ← n abs) ≥ 16
		  ifTrue: [^0].
		s1 ← h / 2.
		s2 ← h - s1.
		s1 ← 1 lshift: s1.
		s2 ← 1 lshift: s2.
		h ← s1 asLarge * s2 asLarge.
		a ← (n < 0
				  ifTrue: [a / h]
				  ifFalse: [a * h]).
		c ← Natural new: 2.
		c ◦ 1 ← a bytes ◦ 1.
		c ◦ 2 ← a bytes ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg]
	  ifFalse: [^self asSmall lshift: n]
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
asInteger | |
	self canBeSmall
	  ifTrue: [^self asSmall].
	^self
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
isLarge | |
	
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
asSmall | t u |
	 "Truncate high bits to make small integer."
	t ← bytes ◦ 1.
	bytes length > 1
	  ifTrue:
		[u ← bytes ◦ 2.
		t ← ((NoteTaker
				  ifTrue: [(u land: 63) - (u land: 64)]
				  ifFalse: [u]) lshift: 8) + t].
	neg
	  ifTrue:
		[t = 0 minVal
		  ifTrue: [^t].
		^0 - t]
	  ifFalse: [^t]
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
asObject | |
	NoteTaker
	  ifTrue:
		[self canBeSmall
		  ifTrue: [^self asSmall asObject].
		self < 49152
		  ifTrue:
			[self < 32768
			  ifTrue: [self error: 'invalid oop']
			  ifFalse: [^(self - 32768) asSmall]]
		  ifFalse:
			[self ≥ 65536
			  ifTrue: [self error: 'invalid oop']
			  ifFalse: [^(self - 65536) asSmall]]]
	  ifFalse: [^self asSmall asObject]
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
canBeSmall | i |
	bytes length > 2
	  ifTrue: [(3 to: bytes length) do:
			[:i | bytes ◦ i ≠ 0
			  ifTrue: [^false]]].
	(self ≤ 1 maxVal and: [self ≥ 1 minVal])
	  ifTrue: [^true].
	^false
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
asFloat | |
	 "Built for comfort, not for speed"
	^self asString asFloat
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
asLarge | |
	
! !
!LargeInteger methodsFor: 'Printing' stamp: ''!
printOn: strm base: b | |
	neg
	  ifTrue: [strm append: '-'].
	bytes printOn: strm base: b
! !
!LargeInteger methodsFor: 'Subscripts' stamp: ''!
cansubscript: a | |
	^self ≥ 1 and: [self ≤ a length]
! !
!LargeInteger methodsFor: 'Subscripts' stamp: ''!
subscripts: a ← val | t |
	(self cansubscript: a)
	  ifTrue:
		[((a class isBytes) and: [t ← val asInteger.  (t<0 or: [t>255])])
		  ifTrue: [user notify: 'Improper store into String']
		  ifFalse: [^a ◦ self asInteger ← t]]
	  ifFalse: [user notify: 'Subscript out of bounds: ' + self asString]
! !
!LargeInteger methodsFor: 'Subscripts' stamp: ''!
subscripts: a | |
	(self cansubscript: a)
	  ifTrue: [^a ◦ self asInteger].
	user notify: 'Subscript out of bounds: ' + self asString
! !
String subclass: #Natural
	instanceVariableNames: ''
	classVariableNames: 'Naturalzero'
	category: 'Numbers'!
!Natural methodsFor: 'Arithmetic' stamp: ''!
natdiv: arg | quo rem ql d div dh dnh z z2 dl q i j k l carry digit flag |
	l ← self length "returns a vector of (quotient, remainder)" - arg length + 1.
	l ≤ 0
	  ifTrue: [^{Naturalzero , self}].
	d ← 8 - arg last hibit.
	rem ← self natnormalize: d "makes a copy and shifts".
	div ← arg natnormalize: d "shifts so high order word is >127".
	quo ← Natural new: l.
	dl ← div length - 1.
	ql ← l.
	dh ← div ◦ dl.
	dnh ← (dl = 1
			  ifTrue: [0]
			  ifFalse: [div ◦ (dl - 1)]).
	z ← MachineDouble init.
	z2 ← MachineDouble new.
	(1 to: ql) do:
		[:k |  "maintain quo*arg+rem=self"
		j ← rem length + 1 - k.
		z high← rem ◦ j.
		z high = dh
		  ifTrue: [q ← ¬1]
		  ifFalse:
			[z low← rem ◦ (j - 1).
			q ← z mdiv: dh.
			z low← (j < 3
			  ifTrue: [0]
			  ifFalse: [rem ◦ (j - 2)]).
			z2 gets: q mtimes: dnh.
			flag ← true.
			[z < z2 and: [flag]] whileTrueDo:
				[q ← q unsignedadd: ¬1.
				z2 decreaseby: dnh.
				z2 high < dh
				  ifTrue: [flag ← false]
				  ifFalse: [z2 high← z2 high - dh]]].
		l ← j - dl.
		z2 init.
		carry ← 0.
		(1 to: div length) do:
			[:i | z gets: q mtimes: div ◦ i.
			z2 increaseby: rem ◦ l.
			z2 decreaseby: carry "subtract q * div from rem".
			z2 decreaseby: z low.
			carry ← z high.
			rem ◦ l ← z2 extract.
			l ← l + 1].
		z2 low = 255
		  ifTrue:
			[q ← q unsignedadd: ¬1.
			l ← j - dl.
			z init.
			(1 to: div length) do:
				[:i | z increaseby: rem ◦ l.
				z increaseby: div ◦ i.
				rem ◦ l ← z extract.
				l ← l + 1]].
		quo ◦ (quo length + 1 - k) ← q].
	rem ← rem natunnormalize: d lookfirst: dl.
	quo last = 0
	  ifTrue:
		[ql < 2
		  ifFalse: [quo ← quo growby: ¬1]].
	^{quo , rem}
! !
!Natural methodsFor: 'Arithmetic' stamp: ''!
natadd: arg | shorter longer i z sum |
	z ← MachineDouble init "returns a Natural number".
	self length < arg length
	  ifTrue:
		[longer ← arg.
		shorter ← self]
	  ifFalse:
		[longer ← self.
		shorter ← arg].
	sum ← Natural new: longer length.
	(1 to: longer length) do:
		[:i | z increaseby: longer ◦ i.
		z increaseby: shorter ◦ i.
		sum ◦ i ← z extract].
	z low ≠ 0
	  ifTrue:
		[sum ← sum growby: 1.
		sum last← z low].
	^sum
! !
!Natural methodsFor: 'Arithmetic' stamp: ''!
nattimes: arg | prod z pl carry digit i j k |
	(self length = 1 and: [self ◦ 1 = 0])
	  ifTrue: [^Naturalzero].
	pl ← self length + arg length.
	prod ← Natural new: pl.
	z ← MachineDouble new.
	(1 to: pl) do: [:i | prod ◦ i ← 0].
	(1 to: self length) do:
		[:i | k ← i - 1.
		carry ← 0.
		digit ← self ◦ i.
		digit ≠ 0
		  ifTrue:
			[(1 to: arg length) do:
				[:j | z gets: digit mtimes: arg ◦ j.
				z increaseby: carry.
				k ← k + 1.
				z increaseby: prod ◦ k "k=i+j-1".
				prod ◦ k ← z low.
				carry ← z high].
			prod ◦ (k + 1) ← carry]].
	prod ◦ pl = 0
	  ifTrue: [^prod growby: ¬1].
	^prod
! !
!Natural methodsFor: 'Arithmetic' stamp: ''!
natsubtract: arg | shorter longer i z sum sl al ng lastdigit |
	sl ← self length "returns an Integer that is created by this operation".
	al ← arg length.
	z ← MachineDouble init.
	sl = al
	  ifTrue:
		[i ← sl.
		[self ◦ i = (arg ◦ i) and: [i > 1]] whileTrueDo: [i ← i - 1].
		sl ← i.
		(self ◦ i unsignedlessthan: arg ◦ i)
		  ifTrue:
			[longer ← arg.
			ng ← true.
			shorter ← self]
		  ifFalse:
			[longer ← self.
			shorter ← arg.
			ng ← false]]
	  ifFalse:
		[sl < al
		  ifTrue:
			[longer ← arg.
			shorter ← self.
			ng ← true.
			sl ← al]
		  ifFalse:
			[longer ← self.
			shorter ← arg.
			ng ← false]].
	sum ← Natural new: longer length.
	lastdigit ← 1.
	(1 to: longer length) do:
		[:i | z increaseby: longer ◦ i.
		z decreaseby: shorter ◦ i.
		sum ◦ i ← z extract ≠ 0
		  ifTrue: [lastdigit ← i]].
	lastdigit = longer length
	  ifFalse:
		[z ← Natural new: lastdigit.
		(1 to: lastdigit) do: [:i | z ◦ i ← sum ◦ i].
		sum ← z].
	^LargeInteger new bytes: sum neg: ng
! !
!Natural methodsFor: 'Arithmetic' stamp: ''!
natnormalize: n | x i r f digit |
	r ← Natural new: self length "n is the number of bits to shift by. The Natural number returned will be written over repeatedly, so we must make a new one." + 1.
	x ← 0.
	f ← n - 8.
	(1 to: r length) do:
		[:i | digit ← self ◦ i.
		r ◦ i ← ((digit lshift: n) lor: x) land: 255.
		x ← digit lshift: f].
	^r
! !
!Natural methodsFor: 'Arithmetic' stamp: ''!
natcompare: arg | i len t4 t5 |
	 "speeded up for Integer args, same speed for LargeInteger (Natural) args"
	len ← self length.
	(t4 ← arg length) < len
	  ifTrue: [^3].
	t4 > len
	  ifTrue: [^1].
	(len to: 1 by: ¬1) do:
		[:i | (t5 ← arg ◦ i) < (self ◦ i)
		  ifTrue: [^3].
		t5 > (self ◦ i)
		  ifTrue: [^1]].
	^2
! !
!Natural methodsFor: 'Arithmetic' stamp: ''!
natdivideandCarry: arg extra: pair | i len z |
	z ← pair ◦ 2 "arg is an integer < 256 - returns remainder, smashes self to quotient - pair is a 2-vector of len (index of high order non-zero word in self) and a MachineDouble - be careful!!!".
	z high← 0.
	len ← pair ◦ 1.
	(len to: 1 by: ¬1) do:
		[:i | z low← self ◦ i.
		self ◦ i ← z mdiv: arg].
	self ◦ len = 0
	  ifTrue:
		[len ← len - 1.
		len = 0
		  ifTrue: [len ← 1]].
	pair ◦ 1 ← len.
	^z high
! !
!Natural methodsFor: 'Arithmetic' stamp: ''!
natunnormalize: n lookfirst: a | x i r f digit |
	n ← 0 - n.
	x ← 0.
	f ← n + 8.
	i ← a.
	digit ← self ◦ i.
	[((digit lshift: n) lor: x) = 0 and: [i ≠ 1]] whileTrueDo:
		[x ← digit lshift: f.
		i ← i - 1.
		digit ← self ◦ i].
	r ← Natural new: i.
	a ← i.
	x ← self ◦ 1 lshift: n.
	(1 to: a) do:
		[:i | digit ← self ◦ (i + 1).
		r ◦ i ← ((digit lshift: f) lor: x) land: 255.
		x ← digit lshift: n].
	^r
! !
!Natural methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm | |
	self printOn: strm base: 10
! !
!Natural methodsFor: 'As yet unclassified' stamp: ''!
asInteger | |
	self length = 1
	  ifTrue: [^self ◦ 1].
	^LargeInteger new bytes: self neg: false
! !
!Natural methodsFor: 'As yet unclassified' stamp: ''!
species | |
	^Natural
! !
!Natural methodsFor: 'As yet unclassified' stamp: ''!
isLarge | |
	^false
! !
!Natural methodsFor: 'As yet unclassified' stamp: ''!
classInit | |
	Naturalzero ← Natural new: 1.
	Naturalzero ◦ 1 ← 0.
	^self
! !
!Natural methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm base: b | p z n b2 x |
	 "only works if b≤10"
	p ← Stream default.
	z ← {self length , MachineDouble new}.
	n ← Natural new: self length.
	b2 ← b * b.
	self copyto: n.
	[z ◦ 1 = 1 and: [n ◦ 1 < b2]] whileFalseDo: 
		[x ← n natdivideandCarry: b2 extra: z.
		p next← x \ b + 48.
		p next← x / b + 48].
	n ◦ 1 printOn: strm base: b.
	strm append: p contents reverse
! !
!Natural methodsFor: 'As yet unclassified' stamp: ''!
◦ n | |
	self length < n "changed from super for Glenn"
	  ifTrue: [^0].
	^super ◦ n
! !
