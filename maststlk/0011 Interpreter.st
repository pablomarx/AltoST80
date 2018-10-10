ContextPart subclass: #BlockContext
	instanceVariableNames: 'nargs startpc home'
	classVariableNames: ''
	category: 'Interpreter'!
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
tempAt: index | |
	^home ◦ index
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
receiver | |
	^home receiver
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
fork | |
	^self fork: ↪value
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
eval | save val |
	nargs = 0
	  ifTrue: [^self value].
	nargs = 1
	  ifTrue:
		[save ← startpc "amazing crock for reading ST76 remote variable".
		startpc ← startpc + (self method ◦ startpc < 112
				  ifTrue: [1]
				  ifFalse: [2]).
		nargs ← 0.
		val ← self value.
		startpc ← save.
		nargs ← 1.
		^val].
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm ||
	strm append: '[] in '.
	super printOn: strm
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
sender: t1 pc: t2 stackp: t3 home: t4 startpc: t5 nargs: t6 | |
	sender ← t1.
	pc ← t2.
	stackp ← t3.
	home ← t4.
	startpc ← t5.
	nargs ← t6
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value: arg | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value | |<primitive: 26>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
blockCopy: t1 | |
	nargs ← t1.
	^(BlockContext new: self length) home: home startpc: pc + 2 nargs: nargs
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value← val | |
	 "for compatibility with ST76 remote variables"
	^self value: val
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
erase | |
	home ← nil.
	super erase
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
valueError | |
	user notify: 'Incompatible number of args'
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value: arg1 value: arg2 | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
home: t1 startpc: t2 nargs: t3 | |
	home ← t1.
	startpc ← t2.
	nargs ← t3
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
tempAt: index put: value | |
	^home ◦ index ← value
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
forkWithArgs: args | |
	^self fork: ↪valueWithArgs: withArgs: args
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
method | |
	^home method
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value: arg1 value: arg2 value: arg3 | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
valueWithArgs: vec | t2 |
	(t2 ← vec length) = 0
	  ifTrue: [^self value].
	t2 = 1
	  ifTrue: [^self value: vec ◦ 1].
	t2 = 2
	  ifTrue: [^self value: vec ◦ 1 value: vec ◦ 2].
	t2 = 3
	  ifTrue: [^self value: vec ◦ 1 value: vec ◦ 2 value: vec ◦ 3].
	user notify: 'More than 3 args for value:'
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
home || ^home
! !
!BlockContext methodsFor: 'Scheduling' stamp: ''!
forkWithArgs: args at: priority | |
	^self fork: ↪valueWithArgs: withArgs: args at: priority
! !
!BlockContext methodsFor: 'Scheduling' stamp: ''!
forkAt: priority | |
	^self fork: ↪value at: priority
! !
String subclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: 'largeFrame smallFrame '
	category: 'Interpreter'!
!CompiledMethod methodsFor: 'As yet unclassified' stamp: ''!
beReturnField: field | |
	self flags: 6 nTemps: field nStack: 0 nLits: 0
! !
!CompiledMethod methodsFor: 'As yet unclassified' stamp: ''!
beReturnSelf | |
	self flags: 5 nTemps: 0 nStack: 0 nLits: 0
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
printOn: strm ||
	strm append: 'a CompiledMethod'
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
value | |
	^self
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
key: key | |
	^self objectAt: 1 put: key
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
key | |
	^self objectAt: 1
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
selector | |
	^self key
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
scanLongStore: extension | byte scanStream |
	scanStream ← Stream new of: self from: self initialPC to: self endPC.
	scanStream do:
		[:byte |
		(byte=16r81 or: [byte=16r82]) ifTrue: [scanStream next=extension ifTrue: [^true]]
		  ifFalse: [byte > 16r80 ifTrue:
				[byte < 16rB0 ifTrue:
					[((byte < 16r87) or: [byte > 16r9F]) ifTrue:
						[scanStream next. "long jumps and extentsions"
						(byte=16r84 or: [byte=16r86]) ifTrue:
							[scanStream next] "double extensions"
				]]]]].
	^false
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
scanLongLoad: extension | byte scanStream |
	scanStream ← Stream new of: self from: self initialPC to: self endPC.
	scanStream do:
		[:byte |
		byte=16r80 ifTrue: [scanStream next=extension ifTrue: [^true]]
		  ifFalse: [byte > 16r80 ifTrue:
				[byte < 16rB0 ifTrue:
					[((byte < 16r87) or: [byte > 16r9F]) ifTrue:
						[scanStream next. "long jumps and extentsions"
						(byte=16r84 or: [byte=16r86]) ifTrue:
							[scanStream next] "double extensions"
				]]]]].
	^false
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
symbolic | s d byte i nlits bytenames |
	bytenames← #(
"0:" 'in0' 'in1' 'in2' 'in3' 'in4' 'in5' 'in6' 'in7' 'in8' 'in9' 'in10' 'in11' 'in12' 'in13' 'in14' 'in15'
"10:" 'tp0' 'tp1' 'tp2' 'tp3' 'tp4' 'tp5' 'tp6' 'tp7' 'tp8' 'tp9' 'tp10' 'tp11' 'tp12' 'tp13' 'tp14' 'tp15'
"20:" 'lt0' 'lt1' 'lt2' 'lt3' 'lt4' 'lt5' 'lt6' 'lt7' 'lt8' 'lt9' 'lt10' 'lt11' 'lt12' 'lt13' 'lt14' 'lt15'
"30:" 'lt16' 'lt17' 'lt18' 'lt19' 'lt20' 'lt21' 'lt22' 'lt23' 'lt24' 'lt25' 'lt26' 'lt27' 'lt28' 'lt29' 'lt30' 'lt31'
"40:" 'gl0' 'gl1' 'gl2' 'gl3' 'gl4' 'gl5' 'gl6' 'gl7' 'gl8' 'gl9' 'gl10' 'gl11' 'gl12' 'gl13' 'gl14' 'gl15'
"50:" 'gl16' 'gl17' 'gl18' 'gl19' 'gl20' 'gl21' 'gl22' 'gl23' 'gl24' 'gl25' 'gl26' 'gl27' 'gl28' 'gl29' 'gl30' 'gl31'
"60:" 'si0' 'si1' 'si2' 'si3' 'si4' 'si5' 'si6' 'si7' 'st0' 'st1' 'st2' 'st3' 'st4' 'st5' 'st6' 'st7'
"70:" 'self' 'true' 'false' 'nil' 'cNeg1' 'c0' 'c1' 'c2' 'retSelf' 'retTrue' 'retFalse' 'retNil' 'ret' 'blockRet' 'bad' 'bad'
"80:" 'xLoad1' 'xSto' 'xPopSto' 'xSend' 'xxSend' 'xSuper' 'xxSuper' 'pop' 'dup' 'current' 'bad' 'bad' 'bad' 'bad' 'bad' 'bad'
"90:" 'j1' 'j2' 'j3' 'j4' 'j5' 'j6' 'j7' 'j8' 'b1' 'b2' 'b3' 'b4' 'b5' 'b6' 'b7' 'b8'
"A0:" 'jB' 'jB' 'jB' 'jB' 'jF' 'jF' 'jF' 'jF' 'bT' 'bT' 'bT' 'bT' 'bF' 'bF' 'bF' 'bF'
"B0:" 'sPlus' 'sMinus' 'sLss' 'sGtr' 'sLeq' 'sGeq' 'sEqu' 'sNeq' 'sTimes' 'sOver' 'sRem' 'sMakePt' 'sLshift' 'sLxor' 'sLand' 'sLor'
"C0:" 'sSub' 'sSubGets' 'sLength' 'sNext' 'sNextGets' 'sEnd' 'sEq' 'sClass' 'sBCopy' 'sValue' 'sValue:' 'bad' 'sNew' 'sNew:' 'sX' 'sY'
"D0:" '0s0' '0s1' '0s2' '0s3' '0s4' '0s5' '0s6' '0s7' '0s8' '0s9' '0s10' '0s11' '0s12' '0s13' '0s14' '0s15'
"E0:" '1s0' '1s1' '1s2' '1s3' '1s4' '1s5' '1s6' '1s7' '1s8' '1s9' '1s10' '1s11' '1s12' '1s13' '1s14' '1s15'
"F0:" '2s0' '2s1' '2s2' '2s3' '2s4' '2s5' '2s6' '2s7' '2s8' '2s9' '2s10' '2s11' '2s12' '2s13' '2s14' '2s15').
	self isQuick ifTrue:
		[self isReturnSelf ifTrue: [^'quick ^self'].
		^'quick ^field: ' + self isReturnField asString].
	d ← Stream default.  d cr.
	d append: ' numArgs: '; print: self numArgs; cr.
	d append: ' numTemps: '; print: self numTemps; cr.
	d append: ' numLiterals: '; print: (nlits ← self numLiterals); cr.
	self frameSize > smallFrame ifTrue:
		[d append: 'large frame'; cr].
	self primitive > 0 ifTrue:
		[d append: 'primitive: '; print: self primitive; cr].
	d print: self literals; cr.
	s ← Stream new of: self from: self initialPC to: self endPC.
	s do:
		[:byte | d append: bytenames ◦ (byte + 1).
		d append: '('; append: byte base8; append: ')'; space.
		byte < 128 ifFalse:
			[byte ≥ 176 ifFalse:
				[byte ≤ 130 ifTrue: "long loads, stores"
					[i ← s next.
					d append: #('inst' 'temp' 'lit' 'glob' ) ◦ (i / 64 + 1).
					d print: i \ 64; space]
				  ifFalse:
					[byte ≤ 134 ifTrue: "long sends"
						[byte odd ifTrue:
							[i ← s next.
							d append: '('; print: i / 32; append: ')'.
							d print: i \ 32; space]
						  ifFalse:
							[d append: '('; print: s next; append: ')'.
							d print: s next; space]]
					  ifFalse:
						[(160 ≤ byte and: [byte ≤ 175])
						  ifTrue: "long jumps"
							[d print: byte \ 8 - 4 * 256 + s next; space]]]]]].
	^d contents
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
writesRef: ref | lit |
	(lit ← self literals find: ref) = 0 ?: [^false].
	^self scanLongStore: 16rC0+lit-1
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
writesField: field | |
	field≤8 ?: [(self scanFor: 16r60+field-1) ?: [^true]].
	^self scanLongStore: field-1
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
readsRef: ref | lit |
	(lit ← self literals find: ref) = 0 ?: [^false].
	lit ≤ 32 ?: [^self scanFor: 16r40+lit-1].
	^self scanLongLoad: 16rC0+lit-1
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
readsField: field | |
	field≤16 ?: [^self scanFor: field-1].
	^self scanLongLoad: field-1
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
scanFor: code | byte scanStream | "scans for byte=code, knows about jumps, extensions"
	"Does not know about details of extensions"
	scanStream ← Stream new of: self from: self initialPC to: self endPC.
	scanStream do:
		[:byte | byte=code ifTrue: [^true].
		byte ≥ 16r80 ifTrue:
			[byte < 16rB0 ifTrue:
				[((byte < 16r87) or: [byte > 16r9F]) ifTrue:
					[scanStream next. "long jumps and extentsions"
					(byte=16r84 or: [byte=16r86]) ifTrue:
						[scanStream next] "double extensions"
					]]]].
	^false
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
callsSuper | |
	^(self scanFor: 16r85) or: [self scanFor: 16r86]
"
SymbolTable messages allSuchThat: [:sel | (SymbolTable method: sel) callsSuper]
"
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
primitive | |
	self flags < 7 ifTrue: [^0].  "no prim, or quick prim"
	^(self literalAt: self numLiterals - 1) land: 255 "Prim # is in next-to-last literal"
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
flags | |
	^self ◦ (newFormat ?: [1] ~: [3]) lshift: ¬5
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numTemps | t |
	self isQuick ifTrue: [^0].
	^self ◦ (newFormat ?: [1] ~: [3]) land: 31
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
frameSize | |
	self ◦ (newFormat ?: [2] ~: [4]) < 128 ifTrue: [^smallFrame].
	^largeFrame
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
isQuick | |  "Return non-false iff quick primitive (⇑self or ⇑field)"
	^self flags between: 5 and: 6
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
isReturnField | |
	 "Return field # if code is quick, else false"
	self flags = 6
	  ifTrue: [^self numTemps].
	^false
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numArgs | t1 |
	(t1 ← self flags) ≤ 4 ifTrue: [^self flags].
	t1 < 7 ifTrue: [^0].
	^((self literalAt: self numLiterals - 1) lshift: ¬8) land: 31
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
endPC | |
	(self last between: 120 and: 124)
	  ifTrue: [^self length].
	^self length - 3
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numLiterals | |
	self isQuick ifTrue: [^0].
	^self ◦ (newFormat ?: [2] ~: [4]) / 2 land: 63
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numStack | |
	self isQuick ifTrue: [^0].  "⇑self or ⇑instvar"
	^self frameSize - self numTemps - self numArgs
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
isReturnSelf | |
	 "Return true if code is quick ⇑self"
	^self flags = 5
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
initialPC | |
	self isQuick ifTrue: [^0].
	^self numLiterals * 2 + (newFormat ?: [3] ~: [5])
! !
!CompiledMethod methodsFor: 'Initialization' stamp: ''!
flags: flags nTemps: nTemps nStack: nStack nLits: nLits | |
	self◦(newFormat ?: [1] ~: [3])← flags*32 + nTemps.
	self◦(newFormat ?: [2] ~: [4])← nLits*2 + (nTemps+nStack>smallFrame ?: [128] ~: [0]) + 1
! !
!CompiledMethod methodsFor: 'Initialization' stamp: ''!
classInit | | "CompiledMethod classInit."
	smallFrame ← 12. "Context range for temps+stack"
	largeFrame ← 32
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
objectAt: i | |<primitive: 73>
	user croak
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
literalAt: i put: val | |
	^self objectAt: i + (newFormat ?: [1] ~: [2]) put: val
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
literals | lits nlits i | "Return vector of literals"
	lits← Vector new: (nlits←self numLiterals).
	i← 0.
	[(i← i+1) ≤ nlits] whileTrueDo:
		[lits◦i ← self objectAt: i + (newFormat ?: [1] ~: [2])].
	^lits
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
literalAt: i | |
	^self objectAt: i + (newFormat ?: [1] ~: [2])
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
objectAt: i put: val | |<primitive: 74>
	user croak
! !
!CompiledMethod methodsFor: 'Source Code' stamp: ''!
setSourcePosition: pos inFile: f | loc p1 p |
	 "set last three bytes to be pos in file (0-3)"
	f > 3
	  ifTrue: [user notify: 'invalid file number']
	  ifFalse:
		[loc ← self length - 2.
		self ◦ (loc + 2) ← f * 64 + (p1 ← (p ← pos / 256) / 256).
		p1 > 62
		  ifTrue:
			[user show: 'Source file is getting full!!'.
			user cr].
		self ◦ (loc + 1) ← p land: 255.
		self ◦ loc ← pos land: 255 "into one byte"]
! !
!CompiledMethod methodsFor: 'Source Code' stamp: ''!
fileIndex | |
	(self last between: 120 and: 124) ifTrue: [^false].
	^self last / 64 + 1
! !
!CompiledMethod methodsFor: 'Source Code' stamp: ''!
moveSourceTo: newFile index: index  | len fileNo pos oldFile quote srcLen|
	len ← self length.
	fileNo ← self ◦ len.
	pos ← fileNo land: 63.
	fileNo ← fileNo / 64.
	pos ← pos * 256 + (self ◦ (len - 1)).
	pos ← pos * 256 + (self ◦ (len - 2)).
	pos = 0 ifTrue: [self setSourcePosition: 0 inFile: 0].
	oldFile ← SourceFiles ◦ (fileNo + 1).
	oldFile position← pos.
	quote ← oldFile next.
	quote ≠ 34 ifTrue: [self setSourcePosition: 0 inFile: 0].
	self setSourcePosition: newFile position inFile: index-1.
	newFile next← quote.
	srcLen ← oldFile integerScan.
	newFile print: srcLen; append: (oldFile next: srcLen)

! !
!CompiledMethod methodsFor: 'Source Code' stamp: ''!
getSource | loc file pos char len str |
	 "get source text from file, or decompile"
	SourceFiles≡nil ifTrue:
		[^false].
	loc ← self length.
	file ← self ◦ loc.
	pos ← file land: 63 "high order 6 bits of pos".
	file ← file / 64 "top 2 bits are file index".
	pos ← pos * 256 + (self ◦ (loc - 1)).
	pos ← pos * 256 + (self ◦ (loc - 2)).
	pos = 0 ifTrue: [^false].
	file ← SourceFiles ◦ (file + 1) "get to correct file (should be readonly)".
	file position← pos. "read source text, this should be merged with filin stuff.
		For now, it does not check matching selectors!!"
	char ← file next.
	char ≠ 34
	  ifTrue: [user notify: 'not pointing at length comment'. ^false]
	  ifFalse:
		[len ← file integerScan.
		[(char ← file next) = 91] whileFalseDo:  [ "skip class and category"
			len ← len - 1 "and decrease len of code"].
		len ← len - 2 "subtract for ].<cr> after code".
		str ← String new: len.
		file into: str "get code".
		^str]
! !
!CompiledMethod methodsFor: 'Source Code' stamp: ''!
putSource: text class: class category: category inFile: fileindex | file clname |
	SourceFiles≡nil ifTrue: [^self].
	file ← SourceFiles ◦ fileindex.
	file readwriteshorten.
	file settoend.
	clname ← class title.
	self setSourcePosition: file position inFile: fileindex-1.
	file append: '"' " string lengths plus $''<cr>[].<cr> ".
	file append: (text length + clname length + category length + 8) asString.
	file append: '" ' "put out length".
	file append: clname.
	file append: '$'''.
	file append: category.
	file append: ''''.
	file cr.
	file append: '['.
	file append: text.
	file append: '].'.
	file cr "put out code".
	file readonly "leave readonly for browsing and to keep clean"
! !
Object subclass: #ContextPart
	instanceVariableNames: 'sender pc stackp'
	classVariableNames: ''
	category: 'Interpreter'!
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
sourceCode | mclass selector |
	selector← self receiver class invertMethod: self method inClass: [:mclass].
	^(mclass canunderstand: selector)
		ifTrue: [mclass code: selector]
		ifFalse: [(LADDecompiler new decompile: selector
				in: self receiver class method: self method) asString]
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
releaseFully | | self releaseTo: nil
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
tempNames | |
	^LADParser new parseArgsAndTemps: self sourceCode asStream notifying: false
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
selector | |
	^self receiver class invertMethod: self method inClass: [:ignored]
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
stack | a strm |
	strm ← (Vector new: 20) asStream.
	strm next← a ← self.
	[(a ← a sender) ≡ nil] whileFalseDo:  [strm next← a].
	^strm contents
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
pop | val |
	val ← self ◦ stackp.
	stackp ← stackp - 1.
	^val
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
mclass | mclass |
	self receiver class invertMethod: self method inClass: [:mclass].
	^mclass
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
release | | self releaseTo: nil
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
push: val | |
	^self ◦ (stackp ← stackp + 1) ← val
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
sender | |
	^sender
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm | mclass selector class |
	selector← (class← self receiver class) invertMethod: self method inClass: [:mclass].
	strm append: class title.
	mclass ≡ class
	  ifFalse: [strm append: '('; append: mclass title; append: ')'].
	strm append: '>>'.
	strm append: selector
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
erase | i |
	 "release frames to break cycles"
	(1 to: stackp) do: [:i | self ◦ i ← nil]
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
swapSender: coroutine | oldSender |
	oldSender ← sender.
	sender ← coroutine.
	^oldSender
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
tempframe | i |
	^(1 to: self method numTemps) collect: [:i | self tempAt: i]
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
sender← t1 | |
	sender ← t1.
	^sender
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
releaseTo: caller | c | "release frames to break cycles"
	c ← self.
	[c ≡ nil or: [c≡caller]] whileFalseDo: 
		[c erase.
		c ← c sender]
! !
Object subclass: #Message
	instanceVariableNames: 'selector args'
	classVariableNames: ''
	category: 'Interpreter'!
!Message methodsFor: 'As yet unclassified' stamp: ''!
selector: sel arg: arg | |
	selector ← sel.
	args ← {arg}
! !
!Message methodsFor: 'As yet unclassified' stamp: ''!
selector: sel args: vec | |
	selector ← sel.
	args ← vec
! !
!Message methodsFor: 'As yet unclassified' stamp: ''!
args | |
	^args
! !
!Message methodsFor: 'As yet unclassified' stamp: ''!
selector | |
	^selector
! !
!Message methodsFor: 'As yet unclassified' stamp: ''!
selector: sel | |
	selector ← sel.
	args ← #()
! !
ContextPart subclass: #MethodContext
	instanceVariableNames: 'method receiverMap receiver'
	classVariableNames: ''
	category: 'Interpreter'!
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
tempAt: index | |
	^self ◦ index
! !
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
receiver | |
	^receiver
! !
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
tempAt: index put: value | |
	^self ◦ index ← value
! !
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
method | |
	^method
! !
!MethodContext methodsFor: 'Blocks' stamp: ''!
remoteCopy | |
	^self blockCopy: 0
! !
!MethodContext methodsFor: 'Blocks' stamp: ''!
blockCopy: nargs | |
	^(BlockContext new: self length) home: self startpc: pc + 2 nargs: nargs
! !
!MethodContext methodsFor: 'Debugger' stamp: ''!
erase | |
	receiver ← nil.
	super erase
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
systemRestart | |
	thisContext releaseTo: RestartCode home.
	NormalCursor show.
	RestartCode eval
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
pc: t1 stackp: t2 | |
	pc ← t1.
	stackp ← t2
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
sender: t1 receiver: t2 mclass: ignored method: t4 | |
	sender ← t1.
	receiver ← t2.
	method ← t4
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
send: selector to: rcvr | nargs i mcl context |
	mcl ← rcvr class.
	[mcl md has: selector] whileFalseDo: 
		[mcl ← mcl superclass.
		mcl ≡ nil
		  ifTrue: [user notify: 'Message not understood: ' + selector]].
	method ← mcl method: selector.
	context ← MethodContext new: method frameSize.
	context sender: self receiver: rcvr mclass: mcl method: method.
	nargs ← method numArgs.
	(1 to: nargs) do: [:i | context temp: (nargs - i + 1) ← self pop].
	context pc: method initialPC stackp: nargs + method numTemps.
	^context "
 | NTP. NTP← MethodContext new.  (NTP send: ↪goBaby to: NTP) inspect.
"
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
goBaby | width height |
	width ← 640.
	height ← 480.
	user currentDisplay: (DisplayForm new extent: width⌾height bits:
		(DisplayBitmap new: width/16 * height) offset: nil).
	user restore. dp0 release.
	[true] whileTrueDo: [self run]
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
restart | |
	pc ← method initialPC.
	stackp ← method numArgs + method numTemps
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
setRestart⦂ code | |
	RestartCode ← code
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
run | |
	self setRestart⦂ [(^nil)].
	user restore.
	user run
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
goBaby | height |
	NoteTaker ← true.
	height ← 480.
	user mapDisplay. user restore. dp0 release.
	[true] whileTrueDo: [self run]
! !
