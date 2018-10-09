Magnitude subclass: #Date
	instanceVariableNames: 'day year'
	classVariableNames: ''
	category: 'Numbers-classes'!
!Date methodsFor: 'Arithmetic' stamp: ''!
= date | |
	^day = date day and: [year = date year]
! !
!Date methodsFor: 'Arithmetic' stamp: ''!
< date | |
	year = date year
	  ifTrue: [^day < date day].
	^year < date year
! !
!Date methodsFor: 'Arithmetic' stamp: ''!
- date | |
	(date is: Date)
	  ifTrue:
		[year = date year
		  ifTrue: [^day - date day].
		^year - 1 / 4 - (date year / 4) + day + date daysleft + (year - 1 - date year * 365)]
	  ifFalse: [^self + (0 - date)]
! !
!Date methodsFor: 'Arithmetic' stamp: ''!
+ days | t |
	days ← day + days.
	t ← Date new.
	(days > 0 and: [days < 366])
	  ifTrue:
		[t day← days "same year".
		t year← year.
		^t].
	^t day: days year: year
! !
!Date methodsFor: 'Arithmetic' stamp: ''!
previous: di | |
	^self + (0 - (7 + self weekdayIndex "e.g. previous: 6 (Sunday) returns Date which is previous closest Sunday.
	note: di=self weekdayIndex returns self+0" - di \ 7))
! !
!Date methodsFor: 'Aspects' stamp: ''!
hash | |
	^(year lshift: 3) lxor: day
! !
!Date methodsFor: 'Aspects' stamp: ''!
daysleft | |
	^self daysinyear - day
! !
!Date methodsFor: 'Aspects' stamp: ''!
month | m leap |
	leap ← self leap.
	(12 to: 1 by: ¬1) do:
		[:m | #(0 31 59 90 120 151 181 212 243 273 304 334 ) ◦ m + (m > 2
		  ifTrue: [leap]
		  ifFalse: [0]) "self monthday: m" < day
		  ifTrue: [^m]].
	user notify: 'illegal month'
! !
!Date methodsFor: 'Aspects' stamp: ''!
day← t1 | |
	day ← t1
! !
!Date methodsFor: 'Aspects' stamp: ''!
monthday: m | |
	 "Return first day-in-year of m'th month"
	^#(0 31 59 90 120 151 181 212 243 273 304 334 ) ◦ m + (m > 2
	  ifTrue: [self leap]
	  ifFalse: [0])
! !
!Date methodsFor: 'Aspects' stamp: ''!
asSeconds | |
	 "Seconds since the beginning of time (local time)"
	^secsinday * (self - (Date new day: 1 year: 1901))
! !
!Date methodsFor: 'Aspects' stamp: ''!
daysinyear | |
	^365 + self leap
! !
!Date methodsFor: 'Aspects' stamp: ''!
dayinyear | |
	^day
! !
!Date methodsFor: 'Aspects' stamp: ''!
whichmonth: m | a |
	 "M may be a (partial) month name, or a number.  Return the month number, or false"
	(m Is: String)
	  ifTrue:
		[m ← m + '*'.
		(1 to: 12) do:
			[:a | (m match: monthnames ◦ a "first partial match")
			  ifTrue: [^a]].
		^false].
	^m ≥ 1 and: [m ≤ 12]
! !
!Date methodsFor: 'Aspects' stamp: ''!
daysinmonth: m | |
	^#(31 28 31 30 31 30 31 31 30 31 30 31 ) ◦ m + (m = 2
	  ifTrue: [self leap]
	  ifFalse: [0])
! !
!Date methodsFor: 'Aspects' stamp: ''!
weekday | |
	^#(Tuesday Wednesday Thursday Friday Saturday Sunday Monday ) ◦ self weekdayIndex
! !
!Date methodsFor: 'Aspects' stamp: ''!
monthname | |
	^monthnames ◦ self month
! !
!Date methodsFor: 'Aspects' stamp: ''!
year | |
	^year
! !
!Date methodsFor: 'Aspects' stamp: ''!
year← t1 | |
	year ← t1
! !
!Date methodsFor: 'Aspects' stamp: ''!
weekdayIndex | a d |
	day ≤ (self monthday: 3)
	  ifTrue:
		[a ← year - 1.
		d ← 306]
	  ifFalse:
		[a ← year.
		d ← ¬59 - self leap] "Tuesday=1,..., Monday=7".
	^d + day + a + (a // 4) + (a // 400) - (a // 100) \\ 7 + 1
! !
!Date methodsFor: 'Aspects' stamp: ''!
daysinmonth | |
	^self daysinmonth: self month
! !
!Date methodsFor: 'Aspects' stamp: ''!
day | |
	^day
! !
!Date methodsFor: 'Aspects' stamp: ''!
dayinmonth | |
	^day - (self monthday: self month)
! !
!Date methodsFor: 'Aspects' stamp: ''!
leap | |
	year \ 4 = 0
	  ifTrue:
		[year \ 100 = 0
		  ifTrue:
			[year \ 400 = 0
			  ifTrue: [^1].
			^0]
		  ifFalse: [^1]]
	  ifFalse: [^0]
! !
!Date methodsFor: 'Initialization' stamp: ''!
classInit | |
	monthnames ← #(January February March April May June July August September October November December ).
	secsinday ← 24 * 60 * 60
! !
!Date methodsFor: 'Printing and reading' stamp: ''!
printOn: strm | |
	self printOn: strm format: #(1 2 3 32 3 1 )
! !
!Date methodsFor: 'Printing and reading' stamp: ''!
from: s | |
	self readfrom: s asVector "asSet" viewer format: nil
! !
!Date methodsFor: 'Printing and reading' stamp: ''!
format: f | strm |
	strm ← Stream default.
	self printon: strm format: f.
	^strm contents
! !
!Date methodsFor: 'Printing and reading' stamp: ''!
printOn: strm format: f | i m t5 t6 |
	"f is print format.
	1-3	positions to print day,month,year respectively
	4	character separator
	5	month format (1 month #, 2 first 3 chars, 3 entire name)
	6	year format (1 year #, 2 year # m\\ 100)"
	
	m ← self month.
	 (1 to: 3) do:
		[:i | (t5 ← f ◦ i) = 1
		  ifTrue: [day - (self monthday: m) printOn: strm]
		  ifFalse:
			[t5 = 2
			  ifTrue:
				[(t6 ← f ◦ 5) = 1
				  ifTrue: [m printOn: strm]
				  ifFalse:
					[t6 = 2
					  ifTrue: [strm append: monthnames ◦ m ◦ (1 to: 3)]
					  ifFalse: [strm append: monthnames ◦ m]]]
			  ifFalse:
				[t5 = 3
				  ifTrue: [(f ◦ 6 = 1
					  ifTrue: [year]
					  ifFalse: [year \\ 100]) printOn: strm]]].
		i < 3
		  ifTrue:
			[f ◦ 4 ≠ 0
			  ifTrue: [strm next← f ◦ 4 "separator"]]]
! !
!Date methodsFor: 'Setting state' stamp: ''!
default | |
	^user now "today" ◦ 1
! !
!Date methodsFor: 'Setting state' stamp: ''!
fromDays: d | |
	"d = days since Jan 1 1901. There are 1461 days in a 4-year cycle.
	2000 is a leap year, so no extra correction is necessary.
	day:year: will fix things up"
	d ← d asInteger divMod: 1461.
	self day: 1 + (d ◦ 2) year: 1901 + ((d ◦ 1) * 4)
! !
!Date methodsFor: 'Setting state' stamp: ''!
day: t1 month: month year: t3 | |
	day ← t1.
	year ← t3.
	year < 100
	  ifTrue: [year ← 1900 + year].
	(month ← self whichmonth: month) ≡ false
	  ifTrue: [user notify: 'illegal month']
	  ifFalse:
		[(day < 1 or: [day > (self daysinmonth: month)])
		  ifTrue: [user notify: 'illegal day in month']
		  ifFalse: [day ← day + (self monthday: month)]]
! !
!Date methodsFor: 'Setting state' stamp: ''!
day: t1 year: t2 | d |
	day ← t1.
	year ← t2.
	[day > (d ← self daysinyear)] whileTrueDo:
		[year ← year + 1.
		day ← day - d].
	[day ≤ 0] whileTrueDo:
		[year ← year - 1.
		day ← day + self daysinyear]
! !
Number subclass: #Float
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers-classes'!
!Float methodsFor: 'Arithmetic' stamp: ''!
hash | |
	^self instfield: 2  "High bits of fraction"
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
reciprocal | |
	^1.0 / self
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
/ aNumber | |<primitive: 58>
	^self retry: #/ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
* aNumber | |<primitive: 57>
	^self retry: #* coercing: aNumber
! !
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
≥ aNumber | |<primitive: 54>
	^self retry: #≥ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
≤ aNumber | |<primitive: 53>
	^self retry: #≤ coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
> aNumber | |<primitive: 52>
	^self retry: #> coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
< aNumber | |<primitive: 51>
	^self retry: #< coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
- aNumber | |<primitive: 50>
	^self retry: #- coercing: aNumber
! !
!Float methodsFor: 'Arithmetic' stamp: ''!
+ aNumber | |<primitive: 49>
	^self retry: #+ coercing: aNumber
! !
!Float methodsFor: 'Conversion' stamp: ''!
fpart | |<primitive: 59>
	user croak
! !
!Float methodsFor: 'Conversion' stamp: ''!
asFloat | |
	^self
! !
!Float methodsFor: 'Conversion' stamp: ''!
recopy | |
	^self
! !
!Float methodsFor: 'Conversion' stamp: ''!
fractionalPart | |<primitive: 59>
	user croak
! !
!Float methodsFor: 'Conversion' stamp: ''!
copy | |
	^self
! !
!Float methodsFor: 'Conversion' stamp: ''!
ipart | |
	 "Returns a Float with zero fractional part"
	^self - self fpart
! !
!Float methodsFor: 'Conversion' stamp: ''!
asInteger | |<primitive: 60>
	 "Return an Integer = self integerPart"
	^(self / 16384.0) asInteger * 16384 + (self \ 16384.0) asInteger
! !
!Float methodsFor: 'Conversion' stamp: ''!
minVal | |
	^self maxVal negated
! !
!Float methodsFor: 'Conversion' stamp: ''!
minMag | |
	self error: 'hash problem'
! !
!Float methodsFor: 'Conversion' stamp: ''!
truncated | |<primitive: 60>
	 "Return an Integer = self integerPart"
	^(self / 10000.0) truncated * 10000 + (self \ 10000.0) truncated
! !
!Float methodsFor: 'Conversion' stamp: ''!
maxMag | |
	self error: 'hash problem'
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
readFrom: aStream | value  |
	value ← Number new readFrom: aStream.
	(value is: Error) ifTrue: [^value].
	^value asFloat
! !
!Float methodsFor: 'Conversion' stamp: ''!
maxVal | |
	^self maxMag
! !
!Float methodsFor: 'Conversion' stamp: ''!
coerce: aNumber | |
	^aNumber asFloat
! !
!Float methodsFor: 'Conversion' stamp: ''!
asFraction | denom |
	 "only use 5 significant figures"
	denom ← (10.0 ipow: 4 - (self abs floorLog: 10.0)) asInteger.
	^(Fraction new numerator: (self * denom) rounded denominator: denom) reduced
! !
!Float methodsFor: 'Conversion' stamp: ''!
generality | |
	^80
! !
!Float methodsFor: 'Conversion' stamp: ''!
asLarge | me digits int i |
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
	int ← LargeInteger new: digits length.
	(1 to: digits length) do: [:i | int ◦ i ← digits ◦ i].
	^int
! !
!Float methodsFor: 'Printing' stamp: ''!
printOn: strm | |
	self printOn: strm digits: 8
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
Number subclass: #Integer
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers-classes'!
!Integer methodsFor: 'Arithmetic' stamp: ''!
gcd: n | m d |
	 "Greatest common divisor, R. Silver's algorithm"
	m ← self abs max: n abs.
	n ← self abs min: n abs.
	(m\\n) = 0 ifTrue: [^n].  "easy test, speeds up rest"
	d ← 1.
	[n even and: [m even]] whileTrueDo:
		[d ← d * 2.
		n ← n//2.
		m ← m//2].
	[n even] whileTrueDo: [n ← n // 2].
	[m even] whileTrueDo: [m ← m // 2].
	[m = n] whileFalseDo:
		[m > n ifTrue: [m ← m-n] ifFalse: [m ← n-m].
		[m even] whileTrueDo: [m ← m // 2]].
	^m * d
! !
!Integer methodsFor: 'Arithmetic' stamp: ''!
lcm: n | |
	 "least common multiple"
	^(self // (self gcd: n) * n) asInteger
! !
!Integer methodsFor: 'Arithmetic' stamp: ''!
intdiv: arg | |
	^{(self / arg), (self \ arg)}
! !
!Integer methodsFor: 'Bit manipulation' stamp: ''!
bits: left to: right ← val | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width ≤ 15
			  ifTrue: [bitson ◦ (width + 1)]
			  ifFalse: [¬1]).
	^(self land: ((mask lshift: 15 - right) lxor: ¬1)) lor: ((val land: mask) lshift: 15 - right)
! !
!Integer methodsFor: 'Bit manipulation' stamp: ''!
allmask: b | |
	^b = (self land: b)
! !
!Integer methodsFor: 'Bit manipulation' stamp: ''!
bits: left to: right | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width ≤ 15
			  ifTrue: [bitson ◦ (width + 1)]
			  ifFalse: [¬1]).
	^(self lshift: right - 15) land: mask
! !
!Integer methodsFor: 'Bit manipulation' stamp: ''!
bitFieldAt: field put: val | width right mask |
	"Field is 16 * leftmost bit + width - 1"
	width ← field land: 15.
	right ← 15 - ((field lshift: -4) + width).
	mask ← (width ≤ 15
			  ifTrue: [bitson ◦ (width + 2)]
			  ifFalse: [¬1]).
	^(self land: ((mask lshift: right) lxor: ¬1)) lor: ((val land: mask) lshift: right)
! !
!Integer methodsFor: 'Bit manipulation' stamp: ''!
nomask: b | |
	^0 = (self land: b)
! !
!Integer methodsFor: 'Bit manipulation' stamp: ''!
bitFieldAt: field | width | "<primitive: xxx>"
	"Field is 16 * leftmost bit + width - 1"
	width ← field land: 15.
	^(self lshift: (field lshift: -4) + width - 15) land: bitson ◦ (width + 2)
! !
!Integer methodsFor: 'Bit manipulation' stamp: ''!
anymask: b | |
	^0 ≠ (self land: b)
! !
!Integer methodsFor: 'Conversion' stamp: ''!
asDoubleWord | s |
	(self digitLength > 4 or: [self negative]) ifTrue:
		[user notify: 'Can''t convert to double word'].
	s ← String new: 4.
	s ◦ 1 ← self ◦ 4.
	s ◦ 2 ← self ◦ 3.
	s ◦ 3 ← self ◦ 2.
	s ◦ 4 ← self ◦ 1.
	^s
! !
!Integer methodsFor: 'Conversion' stamp: ''!
base: b | strm |
	strm ← Stream default.
	strm print: b; append: 'r'.
	self printOn: strm base: b.
	^strm contents
! !
!Integer methodsFor: 'Conversion' stamp: ''!
base8 | |
	^self base: 8
! !
!Integer methodsFor: 'Rounding' stamp: ''!
isIntegral | |
	^true
! !
!Integer methodsFor: 'Subscripts' stamp: ''!
subscripts: a ← val | t |
	(self cansubscript: a)
	  ifTrue:
		[((a class isBits) and:
			[t ← val asInteger.
			(t<0 or: [t>(a class isBytes ifTrue: [255] ifFalse: [65535])])])
		  ifTrue: [user notify: 'Improper store into String']
		  ifFalse: [^a ◦ self asInteger ← t]]
	  ifFalse: [user notify: 'Subscript out of bounds: ' + self asString]
! !
!Integer methodsFor: 'Subscripts' stamp: ''!
subscripts: a | |
	(self cansubscript: a)
	  ifTrue: [^a ◦ self asInteger].
	user notify: 'Subscript out of bounds: ' + self asString
! !
!Integer methodsFor: 'Subscripts' stamp: ''!
cansubscript: a | |
	^self ≥ 1 and: [self ≤ a length]
! !
Integer subclass: #LargeInteger
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers-classes'!
!LargeInteger methodsFor: 'Aspects' stamp: ''!
positive | |
	^true
! !
!LargeInteger methodsFor: 'Aspects' stamp: ''!
copyto: x | i |
	(1 to: (self digitLength min: x digitLength)) do:
		[:i| x◦i ← self◦i].
	^x
! !
!LargeInteger methodsFor: 'Aspects' stamp: ''!
growto: n | |
	^self copyto: (self species new: n)
! !
!LargeInteger methodsFor: 'Aspects' stamp: ''!
◦ n | | <primitive: 63>
	self digitLength < n
	  ifTrue: [^0].
	^super ◦ n "user croak"
! !
!LargeInteger methodsFor: 'Aspects' stamp: ''!
digitLength | | <primitive: 65>
	user croak
! !
!LargeInteger methodsFor: 'Aspects' stamp: ''!
asStream | |
	^Stream new of: self
! !
!LargeInteger methodsFor: 'Backward compatibility' stamp: ''!
lshift: n | t b |
	"16-bit shifts for backward compatibility"
	(n ≥ 0 or: [self positive]) ifTrue: [t ← self arithmeticShift: n]
		ifFalse: [t ← (self land: 65535) asLarge arithmeticShift: n].
	b ← t ◦ 2.
	^((b land: 127) - (b land: 128)) * 256 + (t ◦ 1)
! !
!LargeInteger methodsFor: 'Bit manipulation' stamp: ''!
bitAnd: n | |
	n ← n asInteger.
	^self digitLogic: n op: #land: length: (self digitLength max: n digitLength)
! !
!LargeInteger methodsFor: 'Conversion' stamp: ''!
asDoubleWord | s |
	(self digitLength > 4 or: [self negative]) ifTrue:
		[user notify: 'Can''t convert to double word'].
	s ← String new: 4.
	s ◦ 1 ← self ◦ 4.
	s ◦ 2 ← self ◦ 3.
	s ◦ 3 ← self ◦ 2.
	s ◦ 4 ← self ◦ 1.
	^s
! !
!LargeInteger methodsFor: 'Initialization' stamp: ''!
new: length neg: neg | |
	neg
	  ifTrue: [^LargeNegInt new: length].
	^LargeInteger new: length
! !
!LargeInteger methodsFor: 'Private arithmetic' stamp: ''!
digitAdd: arg | len i accum sum |
	accum ← 0.
	len ← self digitLength max: arg digitLength.
	sum ← self class new: len.
	i ← 1.
	[i ≤ len] whileTrueDo:
		[accum ← accum + (self ◦ i) + (arg ◦ i).
		sum ◦ i ← accum land: 255.
		accum ← accum lshift: -8.
		i ← i + 1].
	accum ≠ 0
	  ifTrue:
		[sum ← sum growby: 1.
		sum lastDigit← accum].
	^sum asInteger
! !
LargeInteger subclass: #LargeNegInt
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers-classes'!
!LargeNegInt methodsFor: 'Arithmetic' stamp: ''!
- arg | |
	arg negative
	  ifTrue: [^self digitSubtract: arg].
	^self digitAdd: arg
! !
!LargeNegInt methodsFor: 'Arithmetic' stamp: ''!
+ arg | |
	arg negative
	  ifTrue: [^self digitAdd: arg].
	^self digitSubtract: arg
! !
!LargeNegInt methodsFor: 'Arithmetic' stamp: ''!
abs | |
	^self negated
! !
!LargeNegInt methodsFor: 'Arithmetic' stamp: ''!
negated | |
	^self copyto: (self new: self digitLength neg: false)
! !
!LargeNegInt methodsFor: 'Aspects' stamp: ''!
positive | |
	^false
! !
!LargeNegInt methodsFor: 'Aspects' stamp: ''!
negative | |
	^true
! !
!LargeNegInt methodsFor: 'Aspects' stamp: ''!
byte: i | b j |
	i > self digitLength "Return i'th byte considering 2's complement"
	  ifTrue: [^255].
	b ← 255 - (self ◦ i).
	(1 to: i - 1) "b is to left of lsb" do:
		[:j | self ◦ j ≠ 0
		  ifTrue: [^b]].
	^b + 1 land: 255 "b contains lsb"
! !
!LargeNegInt methodsFor: 'Bit manipulation' stamp: ''!
highBit | |
	user notify: 'highBit not defined for negative numbers'
! !
!LargeNegInt methodsFor: 'Comparison' stamp: ''!
= arg | |
	(arg isNumber and: [arg negative])
	  ifTrue: [^(self digitCompare: arg) = 0].
	^false
! !
!LargeNegInt methodsFor: 'Comparison' stamp: ''!
compare: arg | |
	arg negative
	  ifTrue: [^3 - (self digitCompare: arg)].
	^1
! !
!LargeNegInt methodsFor: 'Comparison' stamp: ''!
< arg | |
	arg negative
	  ifTrue: [^(self digitCompare: arg) > 0].
	^self
! !
!LargeNegInt methodsFor: 'Conversion' stamp: ''!
asInteger | |
	(self digitLength ≤ 2 and: [self ◦ 2 < minHi or: [self ◦ 2 = minHi and: [self ◦ 1 = 0]]])
	  ifTrue: [^(0 - (self ◦ 2) lshift: 8) - (self ◦ 1)].
	^self
! !
!LargeNegInt methodsFor: 'Initialization' stamp: ''!
classInit | |
	minHi ← 0 - (0 minVal / 256)
! !
!LargeNegInt methodsFor: 'Printing' stamp: ''!
printOn: strm base: b | |
	strm append: '-'.
	super printOn: strm base: b
! !
Object subclass: #Magnitude
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers-classes'!
Magnitude subclass: #Number
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers-classes'!
!Number methodsFor: 'Arithmetic' stamp: ''!
squared | |
	^self * self
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
positive | |
	^self >= 0
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
\\ aNumber | | "modulo"
	 "remainder defined in terms of //.  returns a number with the same sign as aNumber.  e.g.  9\\4 = 1,  -9\\4 = 3, 9\\-4 = -1,  0.9\\0.4 = 0.1 "
	^self - ((self//aNumber) * aNumber)
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
abs | |
	 "return the absolute value (positive magnitude) of this number"
	self < 0
	  ifTrue: [^self negated].
	^self
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
strictlyPositive | |
	^self > 0
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
divMod: aNumber | |
	 "returns a vector of (quotient, remainder) defined by division with truncation toward negative infinity."
	^{(self//aNumber) , (self\\aNumber)}
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
sign | |
	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
even | |
	^(self\\2) = 0
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
rem: aNumber | |
	 "remainder defined in terms of quo:.  returns a number with the same sign as self.  e.g. 9 rem: 4 = 1,  -9 rem: 4 = -1.  0.9 rem: 0.4 = 0.1 "
	^self - ((self quo: aNumber) * aNumber)
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
// aNumber | |
	 "integer quotient defined by division with truncation toward negative infinity.  9//4 = 2,  -9//4 = -3.  -0.9//0.4 = -3.  \\ returns the remainder from this division. "
	^(self / aNumber) floor
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
negated | |
	 "circular with -"
	^0 - self
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
quo: aNumber | |
	 "integer quotient defined by divsion with truncation toward zero.  -9 quo: 4 = -2,  -0.9 quo: 0.4 = -2.  rem: returns the remainder from this division. "
	^(self / aNumber) truncated
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
intdiv: aNumber | | "temporary"
	^self divMod: aNumber
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
\ aNumber | |
	 ^self \\ aNumber
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
/ aNumber | |
	 "circular with reciprocal"
	^self * aNumber reciprocal
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
reciprocal | |
	 "circular with /"
	^1 / self
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
- aNumber | |
	 "circular with negated"
	^self + aNumber negated
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
+ aNumber | |
	self subclassResponsibility
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
negative | |
	^self < 0
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
odd || ^self even≡false
! !
!Number methodsFor: 'As yet unclassified' stamp: ''!
bits: interval | |
	^self bits: interval start to: interval stop
! !
!Number methodsFor: 'Backward compatibility' stamp: ''!
ipow: x | | ^self raisedTo: x
! !
!Number methodsFor: 'Coercion mechanism' stamp: ''!
differentGenerality: aNumber | |
	^self generality ≠ aNumber generality
! !
!Number methodsFor: 'Coercion mechanism' stamp: ''!
retry: aUniqueString coercing: aNumber | t3 |
	(t3 ← self generality compare: aNumber generality) = 1
	  ifTrue: [^(aNumber coerce: self) perform: aUniqueString with: aNumber].
	t3 = 3
	  ifTrue: [^self perform: aUniqueString with: (self coerce: aNumber)].
	user notify: 'coercion attempt failed'
! !
!Number methodsFor: 'Coercion mechanism' stamp: ''!
moreGeneralThan: aNumber | |
	^self generality > aNumber generality
! !
!Number methodsFor: 'Comparison' stamp: ''!
near: aNumber within: epsilon | |
	 "test absolute difference"
	^(self - aNumber) abs ≤ epsilon
! !
!Number methodsFor: 'Comparison' stamp: ''!
near: aNumber error: relativeError | |
	 "for testing near equality, e.g. error convergence"
	self = aNumber
	  ifTrue: [^true].
	^self near: aNumber within: (self abs min: aNumber abs) * relativeError
! !
!Number methodsFor: 'Comparison' stamp: ''!
near: aNumber | |
	^self near: aNumber error: 1.0e-8
! !
!Number methodsFor: 'Compatibility' stamp: ''!
isLarge | |
	^false
! !
!Number methodsFor: 'Compatibility' stamp: ''!
isNumber | |
	^true
! !
!Number methodsFor: 'Compatibility' stamp: ''!
sameAs: anObject | | "temporary?"
	^self = anObject
! !
!Number methodsFor: 'Conversion' stamp: ''!
asDirection | |
	^self cos ⌾ self sin
! !
!Number methodsFor: 'Conversion' stamp: ''!
fractionalPart | |
	^self - self truncated
! !
!Number methodsFor: 'Conversion' stamp: ''!
integerPart | |
	^self - self fractionalPart
! !
!Number methodsFor: 'Conversion' stamp: ''!
degreesToRadians | |
	^self * radiansPerDegree
! !
!Number methodsFor: 'Conversion' stamp: ''!
to: stop | |
	 "create an Interval"
	^Interval new from: self to: stop by: 1
! !
!Number methodsFor: 'Conversion' stamp: ''!
to: stop by: step | |
	 "create an Interval"
	^Interval new from: self to: stop by: step
! !
!Number methodsFor: 'Conversion' stamp: ''!
readFrom: aStream | start radix value |
	aStream ← aStream asStream.
	start ← aStream position.
	radix ← Integer new readFrom: aStream radix: 10.
	(radix is: Error) ifTrue: [^radix].
	aStream ∢ 114
	  ifTrue: "<radix>r<number>"
		[radix <= 1 ifTrue: [^Error new message: 'INVALID RADIX'].
		start ← aStream position.
		value ← Integer new readFrom: aStream radix: radix.
		(value is: Error) ifTrue: [^value]]
	  ifFalse:
		[value ← radix.
		radix ← 10].
	aStream ∢ 46
	  ifTrue:
		[(aStream end ≡ false and: [aStream peek isDigitRadix: radix])
	  	   ifTrue: ".<fraction>"
			[aStream position← start.
			^Float new readFrom: aStream radix: radix]
		   ifFalse:
			[aStream skip: -1.  "back up"
			^value]].
	aStream ∢ 101
	  ifTrue: "e<exponent>"
		[aStream position← start.
		^Float new readFrom: aStream radix: radix].
	^value
! !
!Number methodsFor: 'Conversion' stamp: ''!
coerce: aNumber | |
	 "must be defined by all Number classes."
	self subclassResponsibility.
	"e.g. for Integer, 
	^ aNumber asInteger"
! !
!Number methodsFor: 'Conversion' stamp: ''!
for: n | |
	 "create an Interval, starting at me"
	^Interval new from: self to: self + (n - 1) by: 1
! !
!Number methodsFor: 'Conversion' stamp: ''!
radiansToDegrees | |
	^self / radiansPerDegree
! !
!Number methodsFor: 'Conversions' stamp: ''!
asPoint | |
	^self ⌾ self "Return a Point with me as both coordinates."
! !
!Number methodsFor: 'Conversions' stamp: ''!
asRectCorner | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Number methodsFor: 'Conversions' stamp: ''!
asRectOrigin | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Number methodsFor: 'Conversions' stamp: ''!
asPtX | |
	 "pretend to be a Point for Point +-*/"
! !
!Number methodsFor: 'Conversions' stamp: ''!
asRectangle | |
	^self ⌾ self "Return a Rectangle with me as all coordinates." rect: self ⌾ self
! !
!Number methodsFor: 'Conversions' stamp: ''!
asPtY | |
	 "pretend to be a Point for Point +-*/"
! !
!Number methodsFor: 'Initialization' stamp: ''!
classInit | |
 "constants from Computer Approximations, pp. 182-183
	pi = 3.14159265358979323846264338327950288
	pi/2 = 1.57079632679489661923132169163975144
	pi/4 = 0.78539816339744830961566084581987572
	pi*2 = 6.28318530717958647692528676655900576
	pi/180 = 0.01745329251994329576923690768488612
	2.0 ln = 0.69314718055994530941723212145817657
	2.0 sqrt = 1.41421356237309504880168872420969808"

	pi ← 3.1415927.
	halfpi ← pi / 2.0.
	fourthpi ← pi / 4.0.
	twopi ← pi * 2.0.
	radiansPerDegree ← pi / 180.0.
	degreesPerRadian ← 180.0 / pi.
	ln2 ← 0.69314718.
	sqrt2 ← 1.4142136
! !
!Number methodsFor: 'Intervals, Points' stamp: ''!
to: limit do: aBlock | | ^(self to: limit by: 1) do: aBlock
! !
!Number methodsFor: 'Intervals, Points' stamp: ''!
⌾ y | |
	^Point new x: self y: y
! !
!Number methodsFor: 'Math functions' stamp: ''!
ln | a x x2 n P |
	 "see Computer Approximations, pp. 105-111, p. 227 (LOGE 2663)"
	self ≤ 0.0
	  ifTrue: [user notify: 'ln not valid for ' + self asString]
	  ifFalse:
		[x ← (self + 0.0) asFloat "copy into a Float" "exponent".
		n ← ln2 * ((x instfield: 1)//2 - 0.5) "mantissa between 0.5 and 1.0".
		.
		x instfield: 1 ← 0.
		x ← x * sqrt2.
		x ← x - 1.0 / (x + 1.0).
		x2 ← x * x.
		P ← 0.0 "↪(0.2000000000046727e1 0.666666635059382 0.4000059794795
		0.28525381498 0.2376245609) reverse copy".
		#(0.23762456 0.28525381 0.40000598 0.66666664 2.0 ) do: [:a | P ← P * x2 + a].
		^n + (x * P)]
! !
!Number methodsFor: 'Math functions' stamp: ''!
floorLog: base | x |
	 "gives floor log.base self"
	self < base
	  ifTrue: [^0].
	 "self assumed positive"
	self < base squared
	  ifTrue: [^1].
	x ← 2 * (self floorLog: base squared). "binary recursion like ipow"
	^x + (self / (base ipow: x) floorLog: base)
! !
!Number methodsFor: 'Math functions' stamp: ''!
tan | x x2 sum const |
	 "for angles in radians"
	self < 0.0
	  ifTrue: [^self negated tan negated].
	 " normalize to 0≤self≤(pi/4) "
	self > pi
	  ifTrue: [^(self asFloat \\ pi) tan].
	self > halfpi
	  ifTrue: [^(self - halfpi) tan negated].
	self > fourthpi
	  ifTrue: [^1.0 / (halfpi - self) tan].
	sum ← x ← self.
	x2 ← x * x.
	#(0.3333314 0.1333924 0.05337406 0.024565089 0.002900525 0.0095168091 ) do: [:const |  "Now compute the series"
		sum ← const * (x ← x * x2) + sum].
	^sum
! !
!Number methodsFor: 'Math functions' stamp: ''!
sin | x x2 sum const |
	 "for angles in radians"
	self < 0.0
	  ifTrue: [^self negated sin negated].
	 " normalize to 0≤self≤(pi/4) "
	self > twopi
	  ifTrue: [^(self asFloat \\ twopi) sin].
	self > pi
	  ifTrue: [^(self - pi) sin negated].
	self > halfpi
	  ifTrue: [^(pi - self) sin].
	sum ← x ← self.
	x2 ← x * x.
	#(0.16666667 0.0083333315 ¬1.98409e¬4 2.7526e¬6 ¬2.39e¬8 ) do: [:const |  "Now compute the series"
		sum ← const * (x ← x * x2) + sum].
	^sum
! !
!Number methodsFor: 'Math functions' stamp: ''!
cos | |
	 "for angles in radians"
	self < 0.0
	  ifTrue: [^(self + halfpi) sin].
	^(halfpi - self) sin
! !
!Number methodsFor: 'Math functions' stamp: ''!
log: base | |
	^self ln / base ln
! !
!Number methodsFor: 'Math functions' stamp: ''!
epart: aNumber | |
	"Floor of log"
	^(self log: aNumber) floor
! !
!Number methodsFor: 'Math functions' stamp: ''!
raisedTo: x | |
	 "fixed powers in log n steps"
	x = 0
	  ifTrue: [^1.0].
	x = 1
	  ifTrue: [^self].
	x > 1
	  ifTrue: [^(self * self raisedTo: x//2) * (self raisedTo: x\\2)].
	^(self raisedTo: x negated) reciprocal
! !
!Number methodsFor: 'Math functions' stamp: ''!
floorLog2 | i cnt |
	 "floor of log base 2"
	self < 0
	  ifTrue: [^self negated floorLog2].
	self < 1
	  ifTrue: [^self reciprocal floorLog2 negated].
	i ← 1.
	cnt ← 0.
	[self ≥ i] whileTrueDo:
		[i ← i + i.
		cnt ← cnt + 1].
	^cnt - 1
! !
!Number methodsFor: 'Math functions' stamp: ''!
exp | a n1 x x2 P Q |
	 "see Computer Approximations, pp. 96-104, p. 205 (EXPB 1065)"
	self abs > 9212.0 "0.0 maxVal ln"
	  ifTrue: [user notify: 'exp overflow']
	  ifFalse:
		[x ← self asFloat / ln2.
		(n1 ← Float new "2.0 ipow: x truncated") instfield: 1 ← x truncated * 2.
		(x ← x fractionalPart) ≥ 0.5
		  ifTrue:
			[n1 ← n1 * sqrt2.
			x ← x - 0.5].
		x2 ← x * x "compute 2.0 power: x".
		P ← Q ← 0.0 "↪(0.25250428525576241933744e4 0.28875563776168927289e2) reverse copy".
		#(28.875564 2525.0429 ) do: [:a | P ← P * x2 + a "↪(0.72857336028361108885189e4 0.375021654220866600213e3 0.1e1) reverse copy"].
		#(1.0 375.02165 7285.7336 ) do: [:a | Q ← Q * x2 + a].
		^n1 * (Q + (x * P) / (Q - (x * P)))]
! !
!Number methodsFor: 'Math functions' stamp: ''!
log2 | |
	 "for VirtualMemory"
	^self floorLog2
! !
!Number methodsFor: 'Math functions' stamp: ''!
arcTan | theta term y eps i |
	 "return angle in degrees good to .02 degrees."
	self = 1.0
	  ifTrue: [^45.0].
	self = 1.0
	  ifTrue: [^45.0].
	self * self > 1.0
	  ifTrue:
		[theta ← halfpi.
		y ← 1.0 / (self * self).
		term ← 1.0 / self abs]
	  ifFalse:
		[theta ← 0.0.
		y ← 0.0 - (self * self).
		term ← self abs].
	i ← 1.
	eps ← 1.0e¬4.
	[term abs > eps] whileTrueDo:
		[theta ← theta + term.
		term ← term * y * i asFloat / (i + 2) asFloat.
		i ← i + 2].
	theta ← self sign asFloat * theta * 360.0 / twopi.
	^theta
! !
!Number methodsFor: 'Math functions' stamp: ''!
sqrt | guess i |
	self ≤ 0.0
	  ifTrue:
		[self = 0.0
		  ifTrue: [^0.0].
		user notify: 'sqrt invalid for x<0.']
	  ifFalse:
		[guess ← (self + 0.0) asFloat "copy me into a Float".
		guess instfield: 1 ← (guess instfield: 1)//4 * 2 "and halve expt for first guess".
		(1 to: 5) do: [:i | guess ← self - (guess * guess) / (guess * 2.0) + guess].
		^guess]
! !
!Number methodsFor: 'Rounding' stamp: ''!
roundTo: d | |
	^(self / d) rounded * d
! !
!Number methodsFor: 'Rounding' stamp: ''!
truncateTo: aNumber | |
	 "truncates to next multiple of aNumber toward zero"
	^(self//aNumber) * aNumber
! !
!Number methodsFor: 'Rounding' stamp: ''!
isIntegral | |
	^false
! !
!Number methodsFor: 'Rounding' stamp: ''!
ceiling | |
	 "returns the nearest integer toward positive infinity"
	self ≤ 0.0
	  ifTrue: [^self integerPart].
	^self negated floor negated
! !
!Number methodsFor: 'Rounding' stamp: ''!
truncated | |
	 "truncates to the nearest integer toward zero"
	self subclassResponsibility
! !
!Number methodsFor: 'Rounding' stamp: ''!
floor | trunc |
	 "returns the nearest integer toward negative infinity"
	trunc ← self integerPart.
	self ≥ 0
	  ifTrue: [^trunc].
	self = trunc
	  ifTrue: [^trunc].
	^self truncated - 1
! !
!Number methodsFor: 'Rounding' stamp: ''!
rounded | |
	 "rounds to the nearest integer"
	^(self + (self < 0
	  ifTrue: [0.5]
	  ifFalse: [0.5])) integerPart
! !
!Number methodsFor: 'Subscripts' stamp: ''!
subscripts: a ← val | |
	^a ◦ self asInteger ← val
! !
!Number methodsFor: 'Subscripts' stamp: ''!
subscripts: a | |
	^a ◦ self asInteger
! !
!Number methodsFor: 'Subscripts' stamp: ''!
cansubscript: a | |
	^self asInteger cansubscript: a
! !
Integer subclass: #SmallInteger
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers-classes'!
!SmallInteger methodsFor: 'Arithmetic' stamp: ''!
compare: arg | |
	(arg is: SmallInteger)
	  ifTrue:
		[self < arg
		  ifTrue: [^1].
		self = arg
		  ifTrue: [^2].
		^3]
	  ifFalse: [^4 - (arg compare: self)]
! !
!SmallInteger methodsFor: 'Arithmetic' stamp: ''!
intdiv: arg | |
	(arg is: LargeInteger)
	  ifTrue: [^self asLarge intdiv: arg].
	^super intdiv: arg
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
fromString: str radix: radix | maxdigit c val i |
	maxdigit ← radix + (radix > 10
			  ifTrue: [55 - 1]
			  ifFalse: [48 - 1]).
	val ← 0.
	(1 to: str length) do:
		[:i | c ← str ◦ i.
		(c < 48 or: [c > maxdigit])
		  ifTrue: [^false].
		val ← val * radix + (c ≤ 57
				  ifTrue: [c - 48]
				  ifFalse:
					[c < 65
					  ifTrue: [^false].
					c - 55])].
	^val
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asFloat | |<primitive: 62>
	user croak
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
odd | |
	^(self land: 1) = 1
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asObject | |<primitive: 40>
	user notify: 'This object does not exist!'
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
isdigit | |
	self ≥ 48
	  ifTrue: [ " 0 "
		^self ≤ 57].
	 " 9 "
	^false
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
isletter | |
	self ≥ 97
	  ifTrue: [ " a "
		^self ≤ 122].
	 " z "
	self ≥ 65
	  ifTrue: [ " A "
		^self ≤ 90].
	 " Z "
	^false
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
elementStream | |
	^Stream new of: (Vector new: self)
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
tokenish | |
	 "test for token-chars"
	self isletter
	  ifTrue: [^true].
	 "lower-case"
	self isdigit
	  ifTrue: [^true].
	 "digits"
	^'¬.:⦂' has: self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
isalphanumeric | |
	self isletter
	  ifTrue: [^true].
	 "lower-case"
	^self isdigit
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
quo: aNumber | |
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #quo: coercing: aNumber].
	^self / aNumber  "invoke integer divide primitive"
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
last | |
	^self ◦ self length
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asInt16 | |
	
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
generality | | ^ 20
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
coerce: n | | ^n asInteger
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
inUniqueString | |
	self < 128
	  ifTrue: [^UST1 ◦ (self + 1)].
	^self inString unique
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asOop | |
	self ≥ 0 ifTrue: [^32768 + self]. "0...16K-1 --> 32K...48K-1"
	^65536 + self				"-16K...-1 --> 48K...64K-1"
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
instfield: i | |
	 "small integer gives trouble"
	i = 1
	  ifTrue: [^self].
	user notify: 'arg too big'
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
classInit | |
	 "Initialize the digit buffer"
	digitbuffer ← String new: 16
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
printOn: aStream base: b | i j x |
	(x ← self) < 0
	  ifTrue:
		[i ← 1.
		digitbuffer ◦ 1 ← 16384 \ b * 2 + self - ¬32768 \ b "Im trying to print 16-bit nos.".
		x ← 16384 / b * 2 + (self - ¬32768 / b)]
	  ifFalse: [i ← 0].
	[x ≥ b] whileTrueDo:
		[digitbuffer ◦ (i ← i + 1) ← x \ b.
		x ← x / b].
	digitbuffer ◦ (i ← i + 1) ← x.
	[i > 0] whileTrueDo:
		[x ← digitbuffer ◦ i.
		aStream next← (x < 10
		  ifTrue: [48 + x]
		  ifFalse: [55 + x]).
		i ← i - 1]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asLowercase | |
	65 ≤ self
	  ifTrue:
		[self ≤ 90
		  ifTrue: [^self + 32]]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
hash | |
	 "used to find large integers in dictionaries"
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
recopy | |
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
compareChar: c | a |
	a ← self "⇑self asLowercase compare: c asLowercase" "written in-line for speed".
	65 ≤ a
	  ifTrue:
		[a ≤ 90
		  ifTrue: [a ← a + 32]].
	65 ≤ c
	  ifTrue:
		[c ≤ 90
		  ifTrue: [c ← c + 32]].
	a < c
	  ifTrue: [^1].
	a = c
	  ifTrue: [^2].
	^3
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asUppercase | |
	97 ≤ self
	  ifTrue:
		[self ≤ 122
		  ifTrue: [^self - 32]]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
even | |
	^(self land: 1) = 0
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
\\ aNumber | m |
	m ← self\aNumber.  "invoke integer modulo primitive"
	m = 0
	  ifTrue: [^m].
	(self < 0) ~= (aNumber < 0)
	  ifTrue: [^m - aNumber].
	^m
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
isDigitRadix: radix | |
	^(self - (self ≥ 65 ifTrue: [55] ifFalse: [48]) between: 0 and: radix-1)
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asObjectNoFail | |<primitive: 40>
	^#VanishedObject
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
copy | |
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
inString | t |
	t ← String new: 1.
	t ◦ 1 ← self.
	^t
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
// aNumber | d | "div"
	d ← self quo: aNumber.  "temporary crock"
	(self\\aNumber) = 0
	  ifTrue: [^d].
	(self < 0) ~= (aNumber < 0)
	  ifTrue: [^d - 1].
	^d
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asInteger | |
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
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
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
minVal | |
	^¬16384
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
field: spec | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: right - (spec / 16) + 1 to: right
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
field: spec ← val | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: (right - (spec / 16) + 1) to: right ← val
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
hibit | i |
	(1 to: 16) do:
		[:i | (self land: biton ◦ (17 - i)) ≠ 0
		  ifTrue: [^17 - i]].
	^0
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
length | |
	(self ≥ 256 "behave like a Natural" or: [self ≤ ¬256])
	  ifTrue: [^2].
	^1
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
maxVal | |
	^16383
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
◦ n | t |
	n = 1 "behave like a Natural"
	  ifTrue:
		[self < 0
		  ifTrue: [^((self land: 255) lxor: 255) + 1 land: 255].
		^self land: 255]
	  ifFalse:
		[n = 2
		  ifTrue:
			[self < 0
			  ifTrue:
				[t ← (self lshift: ¬8) lxor: 255.
				(self land: 255) = 0
				  ifTrue: [^t + 1 land: 255].
				^t]
			  ifFalse: [^self lshift: ¬8]]
		  ifFalse: [^0]]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
lor: arg | |<primitive: 16>
	^arg lor: self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
land: arg | |<primitive: 15>
	^arg land: self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
lxor: arg | |<primitive: 14>
	^arg lxor: self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
\ arg | | "primitive" <primitive: 11>
	 "mod"
	0 = arg
	  ifTrue: [user notify: 'Attempt to divide by 0']
	  ifFalse:
		[arg isLarge
		  ifTrue: [^self asLarge \ arg].
		^self \ arg asInteger]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
/ arg | | "primitive" <primitive: 10>
	0 = arg
	  ifTrue: [user notify: 'Attempt to divide by 0']
	  ifFalse:
		[arg isLarge
		  ifTrue: [^self asLarge / arg].
		^self / arg asInteger]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
* aNumber | | "primitive" <primitive: 9>
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge * aNumber].  "overflow"
	^self retry: #* coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
≠ aNumber | | "primitive"
	aNumber isNumber ifFalse: [^true].
	^self retry: #≠ coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
= aNumber | | "primitive"
	aNumber isNumber ifFalse: [^false].
	^self retry: #= coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
≥ aNumber | | "primitive"
	^self retry: #≥ coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
≤ aNumber | | "primitive"
	^self retry: #≤ coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
> aNumber | | "primitive"
	^self retry: #> coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
< aNumber | | "primitive"
	^self retry: #< coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
- aNumber | | "primitive"
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge - aNumber].  "overflow"
	^self retry: #- coercing: aNumber 
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
+ aNumber | | "primitive"
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge + aNumber].  "overflow"
	^self retry: #+ coercing: aNumber
! !
!SmallInteger methodsFor: 'Bit Manipulation' stamp: ''!
lshift: arg | |<primitive: 13>
	arg ← arg asInteger.
	arg class ≡ SmallInteger
	  ifTrue: [^self asLarge lshift: arg].
	user croak
! !
!SmallInteger methodsFor: 'Characters' stamp: ''!
asDigit | |
	self < 10 ifTrue: [^self + 48 "0"] ifFalse: [^self + 55 "A -10"]
! !
!SmallInteger methodsFor: 'Conversion' stamp: ''!
asLarge | digits |
	 "convert to LargeInteger"
	digits ← (self < 0 ifTrue: [LargeNegInt] ifFalse: [LargeInteger]) new: self digitLength.
	digits ◦ 1 ← self ◦ 1.
	digits length = 2
	  ifTrue: [digits ◦ 2 ← self ◦ 2].
	^digits
! !
!SmallInteger methodsFor: 'LargeInteger Compatability' stamp: ''!
lastDigit | |
	^self ◦ self digitLength
! !
!SmallInteger methodsFor: 'LargeInteger Compatability' stamp: ''!
digitLength | |
	(self ≥ 256 "behave like a Natural" or: [self ≤ ¬256])
	  ifTrue: [^2].
	^1
! !
!SmallInteger methodsFor: 'Printing' stamp: ''!
printOn: aStream | |
	self < 0
	  ifTrue:
			[aStream append: '-'.
			self negated printOn: aStream base: 10]
	  ifFalse: [self printOn: aStream base: 10]
! !
Magnitude subclass: #Time
	instanceVariableNames: 'h m s'
	classVariableNames: ''
	category: 'Numbers-classes'!
!Time methodsFor: 'Arithmetic' stamp: ''!
= aTime | |
	^h = aTime hours and: [m = aTime minutes and: [s = aTime seconds]]
! !
!Time methodsFor: 'Arithmetic' stamp: ''!
< aTime | |
	h ≠ aTime hours ifTrue: [^h < aTime hours].
	m ≠ aTime minutes ifTrue: [^m < aTime minutes].
	^s < aTime seconds
! !
!Time methodsFor: 'Arithmetic' stamp: ''!
- aTime | |
	^Time new fromSeconds: (self asSeconds - aTime asSeconds)
! !
!Time methodsFor: 'Arithmetic' stamp: ''!
+ aTime | |
	^Time new fromSeconds: (self asSeconds + aTime asSeconds)
! !
!Time methodsFor: 'Aspects' stamp: ''!
asSeconds | |
	^3600 * h + (60 * m + s)
! !
!Time methodsFor: 'Aspects' stamp: ''!
seconds | |
	^s
! !
!Time methodsFor: 'Aspects' stamp: ''!
minutes | |
	^m
! !
!Time methodsFor: 'Aspects' stamp: ''!
hours | |
	^h
! !
!Time methodsFor: 'Printing' stamp: ''!
printOn: strm | |
	 "Format is h:mm:ss am/pm"
	strm print: (h > 12
	  ifTrue: [h - 12]
	  ifFalse:
		[h < 1
		  ifTrue: [12]
		  ifFalse: [h]]).
	strm append: (m < 10
	  ifTrue: [':0']
	  ifFalse: [':']).
	strm print: m.
	strm append: (s < 10
	  ifTrue: [':0']
	  ifFalse: [':']).
	strm print: s.
	strm space append: (h < 12
	  ifTrue: ['am']
	  ifFalse: ['pm'])
! !
!Time methodsFor: 'Setting state' stamp: ''!
minutes: t1 | |
	m ← t1
! !
!Time methodsFor: 'Setting state' stamp: ''!
hours: t1 | |
	h ← t1
! !
!Time methodsFor: 'Setting state' stamp: ''!
seconds: t1 | |
	s ← t1
! !
!Time methodsFor: 'Setting state' stamp: ''!
fromSeconds: sec | |
	"seconds since midnight (adjusted for time zone and DST)" 
	sec ← sec asInteger divMod: 3600.
	h ← sec ◦ 1.
	sec ← sec ◦ 2.
	m ← sec // 60.
	s ← sec \\ 60.
! !
!Time methodsFor: 'Setting state' stamp: ''!
default | |
	^user now "right now" ◦ 2
! !
