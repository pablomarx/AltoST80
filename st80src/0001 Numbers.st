Number subclass: #Date
	instanceVariableNames: 'day year'
	classVariableNames: 'monthnames secsinday '
	category: 'Numbers'!
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
dayinyear | |
	^day
! !
!Date methodsFor: 'Aspects' stamp: ''!
dayinmonth | |
	^day - (self monthday: self month)
! !
!Date methodsFor: 'Aspects' stamp: ''!
day | |
	^day
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
!Date methodsFor: 'Aspects' stamp: ''!
year | |
	^year
! !
!Date methodsFor: 'Aspects' stamp: ''!
weekday | |
	^#(Tuesday Wednesday Thursday Friday Saturday Sunday Monday ) ◦ self weekdayIndex
! !
!Date methodsFor: 'Aspects' stamp: ''!
daysleft | |
	^self daysinyear - day
! !
!Date methodsFor: 'Aspects' stamp: ''!
day← t1 | |
	day ← t1
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
year← t1 | |
	year ← t1
! !
!Date methodsFor: 'Aspects' stamp: ''!
hash | |
	^(year lshift: 3) lxor: day
! !
!Date methodsFor: 'Aspects' stamp: ''!
monthname | |
	^monthnames ◦ self month
! !
!Date methodsFor: 'Aspects' stamp: ''!
asSeconds | |
	 "Seconds since the beginning of time (local time)"
	^secsinday * (self - (Date new day: 1 year: 1901))
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
daysinyear | |
	^365 + self leap
! !
!Date methodsFor: 'Aspects' stamp: ''!
monthday: m | |
	 "Return first day-in-year of m'th month"
	^#(0 31 59 90 120 151 181 212 243 273 304 334 ) ◦ m + (m > 2
	  ifTrue: [self leap]
	  ifFalse: [0])
! !
!Date methodsFor: 'Aspects' stamp: ''!
daysinmonth: m | |
	^#(31 28 31 30 31 30 31 31 30 31 30 31 ) ◦ m + (m = 2
	  ifTrue: [self leap]
	  ifFalse: [0])
! !
!Date methodsFor: 'Initialization' stamp: ''!
classInit | |
	monthnames ← #(January February March April May June July August September October November December ).
	secsinday ← 24 * 60 * 60
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
!Date methodsFor: 'Setting state' stamp: ''!
fromDays: d | |
	"d = days since Jan 1 1901. There are 1461 days in a 4-year cycle.
	2000 is a leap year, so no extra correction is necessary.
	day:year: will fix things up"
	d ← d asInteger divMod: 1461.
	self day: 1 + (d ◦ 2) asSmall year: 1901 + ((d ◦ 1) asSmall * 4)
! !
!Date methodsFor: 'Setting state' stamp: ''!
default | |
	^user now "today" ◦ 1
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
Number subclass: #Fraction
	instanceVariableNames: 'numerator denominator'
	classVariableNames: ''
	category: 'Numbers'!
!Fraction methodsFor: 'Arithmetic' stamp: ''!
= frac | |
	frac isNumber ≡ false
	  ifTrue: [^false].
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #= coercing: frac].
	frac numerator = 0
	  ifTrue: [^numerator = 0].
	^self - frac = 0
! !
!Fraction methodsFor: 'Arithmetic' stamp: ''!
reciprocal | |
	^Fraction new numerator: denominator denominator: numerator
! !
!Fraction methodsFor: 'Arithmetic' stamp: ''!
+ frac | com |
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #+ coercing: frac].
	denominator = frac denominator
	  ifTrue: [^Fraction new numerator: numerator + frac numerator denominator: denominator].
	com ← denominator lcm: frac denominator. "common denominator"
	^((self withDenominator: com) + (frac withDenominator: com)) reduced
! !
!Fraction methodsFor: 'Arithmetic' stamp: ''!
* frac | |
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #* coercing: frac].
	^(Fraction new numerator: numerator * frac numerator denominator: denominator * frac denominator) reduced
! !
!Fraction methodsFor: 'Arithmetic' stamp: ''!
compare: frac | diff |
	diff ← self - frac.
	diff < 0
	  ifTrue: [^1].
	diff = 0
	  ifTrue: [^2].
	^3
! !
!Fraction methodsFor: 'Arithmetic' stamp: ''!
< frac | |
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #< coercing: frac].
	frac numerator = 0
	  ifTrue: [^numerator < 0].
	^self - frac < 0
! !
!Fraction methodsFor: 'Arithmetic' stamp: ''!
negated | |
	^Fraction new numerator: numerator negated denominator: denominator
! !
!Fraction methodsFor: 'Aspects' stamp: ''!
numerator | |
	^numerator
! !
!Fraction methodsFor: 'Aspects' stamp: ''!
denominator | |
	^denominator
! !
!Fraction methodsFor: 'Conversion' stamp: ''!
readFrom: strm | num denom |
	strm ← strm asStream.
	num ← Integer new readFrom: strm radix: 10.
	(num is: Error) ifTrue: [^num].
	denom ← (strm ∢ ('%' ◦ 1)
			  ifTrue: [Integer new nextInteger: strm base: 10]
			  ifFalse: [1]).
	(denom is: Error) ifTrue: [^denom].
	^(Fraction new numerator: num denominator: denom) reduced
! !
!Fraction methodsFor: 'Conversion' stamp: ''!
asFraction | |
	^self
! !
!Fraction methodsFor: 'Conversion' stamp: ''!
asInteger | |
	^self asFloat asInteger
! !
!Fraction methodsFor: 'Conversion' stamp: ''!
generality | |
	^60
! !
!Fraction methodsFor: 'Conversion' stamp: ''!
coerce: aNumber | |
	^aNumber asFraction
! !
!Fraction methodsFor: 'Conversion' stamp: ''!
asFloat | |
	^numerator asFloat / denominator asFloat
! !
!Fraction methodsFor: 'Intialization' stamp: ''!
numerator: n denominator: d | |
	d = 0
	  ifTrue: [self error: 'denominator cannot be zero']
	  ifFalse:
		[numerator ← n asInteger.
		denominator ← d asInteger abs "keep sign in numerator".
		d < 0
		  ifTrue: [numerator ← numerator negated]]
! !
!Fraction methodsFor: 'Normalization' stamp: ''!
withDenominator: denom | f |
	denom ← denom asInteger.
	(denom\\denominator) = 0
	  ifTrue: [f ← self]
	  ifFalse:
		[f ← self reduced.
		(denom\\f denominator) ≠ 0
		  ifTrue: [^self error: 'unable to normalize fraction']].
	^Fraction new numerator: f numerator * (denom//f denominator) denominator: denom
! !
!Fraction methodsFor: 'Normalization' stamp: ''!
reduced | gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd ← numerator gcd: denominator.
	numer ← numerator//gcd.
	denom ← denominator//gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction new numerator: numer denominator: denom
! !
!Fraction methodsFor: 'Normalization' stamp: ''!
normalized: denom | f |
	 "use least multiple of denom for denominator"
	denom ← denom asInteger.
	(denom\\denominator) = 0
	  ifTrue: [f ← self]
	  ifFalse:
		[f ← self reduced.
		denom ← denom lcm: f denominator].
	^Fraction new numerator: f numerator * (denom // f denominator) denominator: denom
! !
!Fraction methodsFor: 'Printing' stamp: ''!
printOn: strm | |
	strm append: '('.
	strm print: numerator.
	strm append: '%'.
	strm print: denominator.
	strm append: ')'
! !
!Fraction methodsFor: 'Rounding' stamp: ''!
truncated | |
	^numerator quo: denominator
! !
Number subclass: #Integer
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers'!
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
| arg | |
	 "truncate"
	^self / arg * arg
! !
!Integer methodsFor: 'Arithmetic' stamp: ''!
lcm: n | |
	 "least common multiple"
	^(self // (self gcd: n) * n) asInteger
! !
!Integer methodsFor: 'Rounding' stamp: ''!
isIntegral | |
	^true
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
Object subclass: #LinearMeasure
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Numbers'!
!LinearMeasure methodsFor: 'Comparison' stamp: ''!
> aMeasure | |
	^aMeasure < self
! !
!LinearMeasure methodsFor: 'Comparison' stamp: ''!
< aMeasure | |
	^self subclassResponsibility
! !
!LinearMeasure methodsFor: 'Comparison' stamp: ''!
≤ aMeasure | |
	^aMeasure < self ≡ false
! !
!LinearMeasure methodsFor: 'Comparison' stamp: ''!
= aMeasure | |
	^self subclassResponsibility
! !
!LinearMeasure methodsFor: 'Comparison' stamp: ''!
≠ aMeasure | |
	^self = aMeasure ≡ false
! !
!LinearMeasure methodsFor: 'Comparison' stamp: ''!
≥ aMeasure | |
	^self < aMeasure ≡ false
! !
Number subclass: #MachineDouble
	instanceVariableNames: 'high low'
	classVariableNames: 'low8'
	category: 'Numbers'!
!MachineDouble methodsFor: 'Access' stamp: ''!
low | |
	^low
! !
!MachineDouble methodsFor: 'Access' stamp: ''!
high | |
	^high
! !
!MachineDouble methodsFor: 'Access' stamp: ''!
low← t1 | |
	low ← t1.
	^self
! !
!MachineDouble methodsFor: 'Access' stamp: ''!
high← t1 | |
	high ← t1.
	^self
! !
!MachineDouble methodsFor: 'Access' stamp: ''!
extract | x |
	x ← low "returns low, moves high down and propagates sign.".
	low ← high.
	high ← ((low land: 128) = 0
			  ifTrue: [0]
			  ifFalse: [255]).
	^x
! !
!MachineDouble methodsFor: 'Arithmetic' stamp: ''!
mdiv: y | x |
	high > y "Ignores y high (assumes it to be zero. Also assumes that y > high.
      This does a single precision unsigned divide into a double precision dividend
      that results in a single precision quotient (returned) and
      a single precision remainder(placed in self high).
		Fixed for NT 15-bit Integers - Dan I."
	  ifTrue: [user notify: 'illegal MachineDouble division']
	  ifFalse:
		[high < 64
		  ifTrue:
			[x ← high * 256 + low.
			high ← x \ y.
			^x / y].
		high < 128
		  ifTrue:
			[x ← (high lshift: 2) + (low lshift: ¬6) - y.
			high ← x lshift: ¬2.
			low ← low field: 38 ← x.
			^(self mdiv: y) + 64].
		x ← (high lshift: 1) + (low lshift: ¬7) - y.
		high ← x lshift: ¬1.
		low ← low field: 23 ← x.
		^(self mdiv: y) + 128]
! !
!MachineDouble methodsFor: 'Arithmetic' stamp: ''!
increaseby: y | x |
	 "y is a positive <256 integer"
	x ← low + y.
	x > 255
	  ifTrue: [high ← high + 1 land: 255].
	low ← x land: 255.
	^self
! !
!MachineDouble methodsFor: 'Arithmetic' stamp: ''!
gets: x mtimes: y | xh xl yh yl p1 p2 |
	xh ← x lshift: ¬4 "x and y are 8-bit positive #'s.
      Does single precision unsigned multiplication
      returning a double precision result.".
	xl ← x land: 15.
	yh ← y lshift: ¬4.
	yl ← y land: 15.
	low ← yl * xl.
	high ← yh * xh.
	p2 ← yh * xl.
	p1 ← p2 + (yl * xh).
	high ← high + (p1 lshift: ¬4).
	low ← ((p1 land: 15) lshift: 4) + low.
	low ≥ 256
	  ifTrue:
		[high ← high + 1.
		low ← low - 256].
	^self
! !
!MachineDouble methodsFor: 'Arithmetic' stamp: ''!
decreaseby: y | x |
	 "y is a positive <256 integer"
	x ← low - y.
	x < 0
	  ifTrue: [high ← high - 1 land: 255].
	low ← x land: 255.
	^self
! !
!MachineDouble methodsFor: 'Arithmetic' stamp: ''!
< arg | |
	high = arg high
	  ifTrue: [^low < arg low].
	^high < arg high
! !
!MachineDouble methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm | |
	strm append: '[MachineDouble 0'.
	high printOn: strm base: 8.
	strm append: ' 0'.
	low printOn: strm base: 8.
	strm append: ']'
! !
!MachineDouble methodsFor: 'As yet unclassified' stamp: ''!
init | |
	low ← 0.
	high ← 0.
	^self
! !
!MachineDouble methodsFor: 'As yet unclassified' stamp: ''!
classInit | |
	low4 ← 64 "low4 is a field description for the low order 4 bits of an Integer
      high4 is a field description for the high order 4 bits of an 8-bit Integer".
	high4 ← 68
! !
!MachineDouble methodsFor: 'As yet unclassified' stamp: ''!
asInt | n i |
	 "may return a negative number"
	^(high lshift: 8) lor: low
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
Object subclass: #Number
	instanceVariableNames: ''
	classVariableNames: 'fourthpi halfpi sqrt2 twopi degreesPerRadian '
	category: 'Numbers'!
!Number methodsFor: 'Arithmetic' stamp: ''!
odd || ^self even≡false
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
abs | |
	 "return the absolute value (positive magnitude) of this number"
	self < 0
	  ifTrue: [^self negated].
	^self
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
divMod: aNumber | |
	 "returns a vector of (quotient, remainder) defined by division with truncation toward negative infinity."
	^{(self//aNumber) , (self\\aNumber)}
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
\\ aNumber | | "modulo"
	 "remainder defined in terms of //.  returns a number with the same sign as aNumber.  e.g.  9\\4 = 1,  -9\\4 = 3, 9\\-4 = -1,  0.9\\0.4 = 0.1 "
	^self - ((self//aNumber) * aNumber)
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
quo: aNumber | |
	 "integer quotient defined by divsion with truncation toward zero.  -9 quo: 4 = -2,  -0.9 quo: 0.4 = -2.  rem: returns the remainder from this division. "
	^(self / aNumber) truncated
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
+ aNumber | |
	self subclassResponsibility
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
intdiv: aNumber | | "temporary"
	^self divMod: aNumber
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
- aNumber | |
	 "circular with negated"
	^self + aNumber negated
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
/ aNumber | |
	 "circular with reciprocal"
	^self * aNumber reciprocal
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
squared | |
	^self * self
! !
!Number methodsFor: 'Arithmetic' stamp: ''!
reciprocal | |
	 "circular with /"
	^1 / self
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
\ aNumber | |
	 ^self \\ aNumber
! !
!Number methodsFor: 'As yet unclassified' stamp: ''!
hex4 | strm i x bytes d t |
	strm ← (String new: 5) asStream.
	strm space.
	x ← self asInteger.
	x isLarge
	  ifTrue:
		[bytes ← x bytes.
		d ← bytes last.
		d > 15
		  ifTrue:
			[t ← d / 16.
			strm next← (t < 10
			  ifTrue: [48 + t]
			  ifFalse: [55 + t])].
		t ← d \ 16.
		strm next← (t < 10
		  ifTrue: [48 + t]
		  ifFalse: [55 + t]).
		(bytes length - 1 to: 1 by: ¬1) do:
			[:i | d ← bytes ◦ i.
			t ← d / 16.
			strm next← (t < 10
			  ifTrue: [48 + t]
			  ifFalse: [55 + t]).
			t ← d \ 16.
			strm next← (t < 10
			  ifTrue: [48 + t]
			  ifFalse: [55 + t])].
		^strm contents].
	bytes ← String new: 4.
	(1 to: 4) do:
		[:i | bytes ◦ i ← x \ 16.
		x ← x lshift: ¬4].
	(4 to: 1 by: ¬1) do:
		[:i | t ← bytes ◦ i.
		strm next← (t < 10
		  ifTrue: [48 + t]
		  ifFalse: [55 + t])].
	^strm contents
! !
!Number methodsFor: 'As yet unclassified' stamp: ''!
bits: interval | |
	^self bits: interval start to: interval stop
! !
!Number methodsFor: 'As yet unclassified' stamp: ''!
hex | |
	^self base: 16
! !
!Number methodsFor: 'Backward compatibility' stamp: ''!
ipow: x | | ^self raisedTo: x
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
differentGenerality: aNumber | |
	^self generality ≠ aNumber generality
! !
!Number methodsFor: 'Coercion mechanism' stamp: ''!
moreGeneralThan: aNumber | |
	^self generality > aNumber generality
! !
!Number methodsFor: 'Comparison' stamp: ''!
min: aNumber | |
	self < aNumber
	  ifTrue: [^self].
	^aNumber
! !
!Number methodsFor: 'Comparison' stamp: ''!
near: aNumber error: relativeError | |
	 "for testing near equality, e.g. error convergence"
	self = aNumber
	  ifTrue: [^true].
	^self near: aNumber within: (self abs min: aNumber abs) * relativeError
! !
!Number methodsFor: 'Comparison' stamp: ''!
compare: aNumber | | "change to ^(self-aNumber) sign ?"
	self < aNumber
	  ifTrue: [^1].
	self = aNumber
	  ifTrue: [^2].
	^3
! !
!Number methodsFor: 'Comparison' stamp: ''!
max: aNumber | |
	self > aNumber
	  ifTrue: [^self].
	^aNumber
! !
!Number methodsFor: 'Comparison' stamp: ''!
= aNumber | |
	self subclassResponsibility
! !
!Number methodsFor: 'Comparison' stamp: ''!
< aNumber | |
	self subclassResponsibility
! !
!Number methodsFor: 'Comparison' stamp: ''!
≥ aNumber | |
	^self < aNumber ≡ false
! !
!Number methodsFor: 'Comparison' stamp: ''!
≠ aNumber | |
	^self = aNumber ≡ false
! !
!Number methodsFor: 'Comparison' stamp: ''!
within: anInterval | |
	 "return true if I fall within this Interval"
	^anInterval start ≤ self and: [self ≤ anInterval stop]
! !
!Number methodsFor: 'Comparison' stamp: ''!
> n | |
	^n < self
! !
!Number methodsFor: 'Comparison' stamp: ''!
≤ aNumber | |
	^aNumber < self ≡ false
! !
!Number methodsFor: 'Comparison' stamp: ''!
between: min and: max | |
	^self ≥ min and: [self ≤ max]
! !
!Number methodsFor: 'Comparison' stamp: ''!
near: aNumber | |
	^self near: aNumber error: 1.0e-8
! !
!Number methodsFor: 'Comparison' stamp: ''!
near: aNumber within: epsilon | |
	 "test absolute difference"
	^(self - aNumber) abs ≤ epsilon
! !
!Number methodsFor: 'Compatibility' stamp: ''!
isNumber | |
	^true
! !
!Number methodsFor: 'Compatibility' stamp: ''!
sameAs: anObject | | "temporary?"
	^self = anObject
! !
!Number methodsFor: 'Compatibility' stamp: ''!
isLarge | |
	^false
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
radiansToDegrees | |
	^self / radiansPerDegree
! !
!Number methodsFor: 'Conversion' stamp: ''!
to: stop | |
	 "create an Interval"
	^Interval new from: self to: stop by: 1
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
to: stop by: step | |
	 "create an Interval"
	^Interval new from: self to: stop by: step
! !
!Number methodsFor: 'Conversion' stamp: ''!
degreesToRadians | |
	^self * radiansPerDegree
! !
!Number methodsFor: 'Conversion' stamp: ''!
coerce: aNumber | |
	 "must be defined by all Number classes."
	self subclassResponsibility.
	"e.g. for Integer, 
	^ aNumber asInteger"
! !
!Number methodsFor: 'Conversion' stamp: ''!
asDirection | |
	^self cos ⌾ self sin
! !
!Number methodsFor: 'Conversion' stamp: ''!
for: n | |
	 "create an Interval, starting at me"
	^Interval new from: self to: self + (n - 1) by: 1
! !
!Number methodsFor: 'Conversions' stamp: ''!
asPoint | |
	^self ⌾ self "Return a Point with me as both coordinates."
! !
!Number methodsFor: 'Conversions' stamp: ''!
asRectOrigin | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Number methodsFor: 'Conversions' stamp: ''!
asRectCorner | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Number methodsFor: 'Conversions' stamp: ''!
asRectangle | |
	^self ⌾ self "Return a Rectangle with me as all coordinates." rect: self ⌾ self
! !
!Number methodsFor: 'Conversions' stamp: ''!
asPtX | |
	 "pretend to be a Point for Point +-*/"
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
~ x | |
	^Interval new from: self to: x by: 1
! !
!Number methodsFor: 'Intervals, Points' stamp: ''!
⌾ y | |
	^Point new x: self y: y
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
log: base | |
	^self ln / base ln
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
log2 | |
	 "for VirtualMemory"
	^self floorLog2
! !
!Number methodsFor: 'Math functions' stamp: ''!
cos | |
	 "for angles in radians"
	self < 0.0
	  ifTrue: [^(self + halfpi) sin].
	^(halfpi - self) sin
! !
!Number methodsFor: 'Math functions' stamp: ''!
epart: aNumber | |
	"Floor of log"
	^(self log: aNumber) floor
! !
!Number methodsFor: 'Rounding' stamp: ''!
rounded | |
	 "rounds to the nearest integer"
	^(self + (self < 0
	  ifTrue: [0.5]
	  ifFalse: [0.5])) integerPart
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
isIntegral | |
	^false
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
	category: 'Numbers'!
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
printOn: aStream | |
	self < 0
	  ifTrue:
		[self = self minVal
		  ifTrue: [aStream append: (NoteTaker
			  ifTrue: ['-16384']
			  ifFalse: ['-32768'])]
		  ifFalse:
			[aStream append: '-'.
			self negated printOn: aStream base: 10]]
	  ifFalse: [self printOn: aStream base: 10]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
cansubscript: a | |
	^self ≥ 1 and: [self ≤ a length]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asInteger | |
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
intdiv: arg | |
	(arg is: SmallInteger)
	  ifTrue: [^{self / arg , (self \ arg)}].
	(arg is: LargeInteger)
	  ifTrue: [^self asLarge intdiv: arg].
	user notify: 'I give up'
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
eightbitAdd: aNumber | |
	 "treat numbers as unsigned 8-bit quantities. (for Natural)"
	^self + aNumber land: 255
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
bytes | |
	^self "behave like a LargeInteger - negative integers behave like positive naturals"
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
subscripts: a ← val | t |
	(self cansubscript: a)
	  ifTrue:
		[((a class isBytes) and: [t ← val asInteger.  (t<0 or: [t>255])])
		  ifTrue: [user notify: 'Improper store into String']
		  ifFalse: [^a ◦ self ← t]]
	  ifFalse: [user notify: 'Subscript out of bounds: ' + self asString]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
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
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
subscripts: a | |
	(self cansubscript: a)
	  ifTrue: [^a ◦ self].
	user notify: 'Subscript out of bounds: ' + self asString
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asDigit | |
	"Convert to digit 0-9 or A-Z"
	^self + (self>9 ifTrue: [55] ifFalse: [48])
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
unsignedlessthan: y | |
	^self eightbitLessThan: y
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
nomask: b | |
	^0 = (self land: b)
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
isalphanumeric | |
	self isletter
	  ifTrue: [^true].
	 "lower-case"
	^self isdigit
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
even | |
	^(self land: 1) = 0
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
> aNumber | | "primitive"
	^self retry: #> coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
instfield: i | |
	 "small integer gives trouble"
	i = 1
	  ifTrue: [^self].
	user notify: 'arg too big'
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
allmask: b | |
	^b = (self land: b)
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
hash | |
	 "used to find large integers in dictionaries"
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
≤ aNumber | | "primitive"
	^self retry: #≤ coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
recopy | |
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
quo: aNumber | |
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #quo: coercing: aNumber].
	^self / aNumber  "invoke integer divide primitive"
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
inUniqueString | |
	self < 128
	  ifTrue: [^UST1 ◦ (self + 1)].
	^self inString unique
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
coerce: n | | ^n asInteger
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
compare: arg | |
	(arg is: SmallInteger)
	  ifTrue:
		[self < arg
		  ifTrue: [^1].
		self = arg
		  ifTrue: [^2].
		^3]
	  ifFalse: [^self natcompare: arg bytes "4 - (arg bytes natcompare: self)"]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
minVal | |
	NoteTaker
	  ifTrue: [^¬16384].
	^¬32768
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asInt16 | |
	
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
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
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
field: spec | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: right - (spec / 16) + 1 to: right
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
copy | |
	^self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asSmall | |
	
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
length | |
	(self ≥ 256 "behave like a Natural" or: [self ≤ ¬256])
	  ifTrue: [^2].
	^1
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
unsignedadd: y | |
	^self eightbitAdd: y
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asOop | |
	NoteTaker
	  ifTrue:
		[self ≥ 0
		  ifTrue: [^32768 + self].
		 "0...16K-1 --> 32K...48K-1"
		^65536 + self]
	  ifFalse: [ "-16K...-1 --> 48K...64K-1"
		^super asOop]
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
asLarge | me digits |
	 "convert to LargeInteger"
	me ← self bytes.
	digits ← Natural new: me length.
	digits ◦ 1 ← me ◦ 1.
	digits length = 2
	  ifTrue: [digits ◦ 2 ← me ◦ 2].
	^LargeInteger new bytes: digits neg: self neg
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
≠ aNumber | | "primitive"
	aNumber isNumber ifFalse: [^true].
	^self retry: #≠ coercing: aNumber
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
maxVal | |
	NoteTaker
	  ifTrue: [^16383].
	^32767
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
inString | t |
	t ← String new: 1.
	t ◦ 1 ← self.
	^t
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
classInit | |
	 "Initialize the digit buffer"
	digitbuffer ← String new: 16
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asNatural | t |
	t ← Natural new: self length.
	t ◦ 1 ← self ◦ 1.
	t length > 1
	  ifTrue: [t ◦ 2 ← self ◦ 2].
	^t
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
= aNumber | | "primitive"
	aNumber isNumber ifFalse: [^false].
	^self retry: #= coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
hibit | i |
	(1 to: 16) do:
		[:i | (self land: biton ◦ (17 - i)) ≠ 0
		  ifTrue: [^17 - i]].
	^0
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
odd | |
	^(self land: 1) = 1
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
/ arg | | "primitive" <primitive: 10>
	0 = arg
	  ifTrue: [user notify: 'Attempt to divide by 0']
	  ifFalse:
		[arg isLarge
		  ifTrue: [^self asLarge / arg].
		^self / arg asInteger]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
isDigitRadix: radix | |
	^(self - (self ≥ 65 ifTrue: [55] ifFalse: [48]) between: 0 and: radix-1)
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
generality | | ^ 20
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
anymask: b | |
	^0 ≠ (self land: b)
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
last | |
	^self ◦ self length
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
≥ aNumber | | "primitive"
	^self retry: #≥ coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
elementStream | |
	^Stream new of: (Vector new: self)
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
- aNumber | | "primitive"
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge - aNumber].  "overflow"
	^self retry: #- coercing: aNumber 
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
natcompare: arg | i len t4 t5 |
	len ← self length.
	(t4 ← arg length) < len
	  ifTrue: [^3].
	t4 > len
	  ifTrue: [^1].
	(len to: 1 by: ¬1) do:
		[:i | (t5 ← self ◦ i) > (arg ◦ i)
		  ifTrue: [^3].
		t5 < (arg ◦ i)
		  ifTrue: [^1]].
	^2
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
field: spec ← val | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: (right - (spec / 16) + 1) to: right ← val
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
+ aNumber | | "primitive"
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge + aNumber].  "overflow"
	^self retry: #+ coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
lxor: arg | |<primitive: 14>
	^arg lxor: self
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asObjectNoFail | |<primitive: 40>
	^#VanishedObject
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
* aNumber | | "primitive" <primitive: 9>
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge * aNumber].  "overflow"
	^self retry: #* coercing: aNumber
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
\\ aNumber | m |
	m ← self\aNumber.  "invoke integer modulo primitive"
	m = 0
	  ifTrue: [^m].
	(self < 0) ~= (aNumber < 0)
	  ifTrue: [^m - aNumber].
	^m
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
< aNumber | | "primitive"
	^self retry: #< coercing: aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asLowercase | |
	65 ≤ self
	  ifTrue:
		[self ≤ 90
		  ifTrue: [^self + 32]]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asObject | |<primitive: 40>
	user notify: 'This object does not exist!'
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
hex4 | |
	^self base: 16
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
asUppercase | |
	97 ≤ self
	  ifTrue:
		[self ≤ 122
		  ifTrue: [^self - 32]]
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
asFloat | |<primitive: 62>
	user croak
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
eightbitLessThan: aNumber | |
	 "treat numbers as unsigned 8-bit quantities."
	^self < aNumber
! !
!SmallInteger methodsFor: 'As yet unclassified' stamp: ''!
lshift: arg | |<primitive: 13>
	arg class ≡ SmallInteger
	  ifTrue: [^self asLarge lshift: arg].
	^self lshift: arg asSmall
! !
Object subclass: #Time
	instanceVariableNames: 'h m s'
	classVariableNames: ''
	category: 'Numbers'!
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
seconds | |
	^s
! !
!Time methodsFor: 'Aspects' stamp: ''!
minutes | |
	^m
! !
!Time methodsFor: 'Aspects' stamp: ''!
asSeconds | |
	^3600 * h + (60 * m + s)
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
fromSeconds: sec | |
	"seconds since midnight (adjusted for time zone and DST)" 
	sec ← sec asInteger divMod: 3600.
	h ← (sec ◦ 1) asSmall.
	sec ← (sec ◦ 2) asSmall.
	m ← sec // 60.
	s ← sec \\ 60.
! !
!Time methodsFor: 'Setting state' stamp: ''!
default | |
	^user now "right now" ◦ 2
! !
!Time methodsFor: 'Setting state' stamp: ''!
hours: t1 | |
	h ← t1
! !
!Time methodsFor: 'Setting state' stamp: ''!
minutes: t1 | |
	m ← t1
! !
!Time methodsFor: 'Setting state' stamp: ''!
seconds: t1 | |
	s ← t1
! !
