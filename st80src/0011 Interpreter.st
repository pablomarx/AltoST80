ContextPart subclass: #BlockContext
	instanceVariableNames: 'nargs startpc home'
	classVariableNames: ''
	category: 'Interpreter'!
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm ||
	strm append: '[] in '.
	super printOn: strm
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value | |<primitive: 26>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value: arg | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
erase | |
	home ← nil.
	super erase
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
blockCopy: t1 | |
	nargs ← t1.
	^(BlockContext new: self length) home: home startpc: pc + 2 nargs: nargs
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
tempAt: index put: value | |
	^home ◦ index ← value
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
value: arg1 value: arg2 value: arg3 | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
valueError | |
	user notify: 'Incompatible number of args'
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
method | |
	^home method
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
receiver | |
	^home receiver
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
value← val | |
	 "for compatibility with ST76 remote variables"
	^self value: val
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
home: t1 startpc: t2 nargs: t3 | |
	home ← t1.
	startpc ← t2.
	nargs ← t3
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
value: arg1 value: arg2 | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
tempAt: index | |
	^home ◦ index
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
forkWithArgs: args | |
	^self fork: ↪valueWithArgs: withArgs: args
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
home || ^home
! !
!BlockContext methodsFor: 'As yet unclassified' stamp: ''!
fork | |
	^self fork: ↪value
! !
String subclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: 'largeFrame primReturnSelf primReturnField'
	category: 'Interpreter'!
!CompiledMethod methodsFor: 'As yet unclassified' stamp: ''!
moveSourceTo: newFile  | len fileNo pos t5 t6 t7 |
	len ← self length.
	fileNo ← self ◦ len.
	pos ← fileNo land: 63.
	fileNo ← fileNo / 64.
	pos ← pos * 256 + (self ◦ (len - 1)).
	pos ← pos * 256 + (self ◦ (len - 2)).
	pos = 0 ifTrue: [self setSourcePosition: 0 inFile: 0].
	fileNo ← SourceFiles ◦ (fileNo + 1).
	fileNo position← pos.
	t5 ← fileNo next.
	t5 ≠ 34 ifTrue: [self setSourcePosition: 0 inFile: 0].
	self setSourcePosition: newFile position inFile: 0.
	newFile next← t5.
	t6 ← fileNo integerScan.
	newFile print: t6; append: (fileNo next: t6)

! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
printOn: strm ||
	strm append: 'a CompiledMethod for ' + self selector
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
selector | |
	^self key
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
key | |
	^self objectAt: 1
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
value | |
	^self
! !
!CompiledMethod methodsFor: 'Association' stamp: ''!
key: key | |
	^self objectAt: 1 put: key
! !
!CompiledMethod methodsFor: 'ByteCodes' stamp: ''!
symbolic | s d byte i nlits |
	self isQuick
	  ifTrue:
		[self isReturnSelf
		  ifTrue: [^'quick ^self'].
		^'quick ^field: ' + self isReturnField asString]
	  ifFalse:
		[d ← Stream default.
		d cr.
		d append: ' numArgs: '.
		d print: self numArgs.
		d cr.
		d append: ' numTemps: '.
		d print: self numTemps.
		d cr.
		d append: ' numLiterals: '.
		d print: (nlits ← self numLiterals).
		d cr.
		self frameSize > smallFrame
		  ifTrue:
			[d append: 'large frame'.
			d cr].
		self primitive > 0
		  ifTrue:
			[d append: 'primitive: '.
			d print: self primitive.
			d cr].
		d print: self literals.
		d cr.
		s ← Stream new of: self from: self initialPC to: self endPC.
		s do:
			[:byte | d append: byteNames ◦ (byte + 1).
			d append: '('.
			d append: byte base8.
			d append: ')'.
			d space.
			byte < 128
			  ifFalse:
				[byte ≥ 176
				  ifFalse:
					[byte ≤ 130
					  ifTrue:
						[ "long loads, stores"
						i ← s next.
						d append: #('inst' 'temp' 'lit' 'glob' ) ◦ (i / 64 + 1).
						d print: i \ 64.
						d space]
					  ifFalse:
						[byte ≤ 134
						  ifTrue:
							[ "long sends"
							byte even ≡ false
							  ifTrue:
								[i ← s next.
								d append: '('.
								d print: i / 32.
								d append: ')'.
								d print: i \ 32.
								d space]
							  ifFalse:
								[d append: '('.
								d print: s next.
								d append: ')'.
								d print: s next.
								d space]]
						  ifFalse:
							[(160 ≤ byte and: [byte ≤ 175])
							  ifTrue:
								[ "long jumps"
								d print: byte \ 8 - 4 * 256 + s next.
								d space]]]]]].
		^d contents]
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numTemps | t |
	self isQuick
	  ifTrue: [^0].
	^self ◦ 3 land: 31
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numStack | |
	self isQuick
	  ifTrue: [^0].
	 "⇑self or ⇑instvar"
	^self frameSize - self numTemps - self numArgs
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numArgs | t1 |
	(t1 ← self flags) ≤ 4
	  ifTrue: [^self flags].
	t1 < 7
	  ifTrue: [^0].
	^((self literalAt: self numLiterals - 1) lshift: ¬8) land: 31
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
endPC | |
	(self last between: 120 and: [124])
	  ifTrue: [^self length].
	^self length - 3
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
isQuick | |
	 "Return non-false iff quick primitive (⇑self or ⇑field)"
	^self flags between: 5 and: [6]
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
primitive | |
	self flags < 7
	  ifTrue: [^0].
	 "no prim, or quick prim"
	^(self literalAt: self numLiterals - 1) land: 255 "Prim # is in next-to-last literal"
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
isReturnSelf | |
	 "Return true if code is quick ⇑self"
	^self flags = 5
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
isReturnField | |
	 "Return field # if code is quick, else false"
	self flags = 6
	  ifTrue: [^self numTemps].
	^false
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
numLiterals | |
	self isQuick
	  ifTrue: [^0].
	^self ◦ 4 / 2 land: 63
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
frameSize | |
	self ◦ 4 < 128
	  ifTrue: [^smallFrame].
	^largeFrame
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
initialPC | |
	self isQuick
	  ifTrue: [^0].
	^self numLiterals * 2 + 5
! !
!CompiledMethod methodsFor: 'Header' stamp: ''!
flags | |
	^self ◦ 3 lshift: ¬5
! !
!CompiledMethod methodsFor: 'Initialization' stamp: ''!
classInit | |
	 "CompiledMethod classInit."
	smallFrame ← 12.
	largeFrame ← 32 "Context range for temps+stack".
	byteNames ← #('in0' 'in1' 'in2' 'in3' 'in4' 'in5' 'in6' 'in7' 'in8' 'in9' 'in10' 'in11' 'in12' 'in13' 'in14' 'in15' 'tp0' 'tp1' 'tp2' 'tp3' 'tp4' 'tp5' 'tp6' 'tp7' 'tp8' 'tp9' 'tp10' 'tp11' 'tp12' 'tp13' 'tp14' 'tp15' 'lt0' 'lt1' 'lt2' 'lt3' 'lt4' 'lt5' 'lt6' 'lt7' 'lt8' 'lt9' 'lt10' 'lt11' 'lt12' 'lt13' 'lt14' 'lt15' 'lt16' 'lt17' 'lt18' 'lt19' 'lt20' 'lt21' 'lt22' 'lt23' 'lt24' 'lt25' 'lt26' 'lt27' 'lt28' 'lt29' 'lt30' 'lt31' 'gl0' 'gl1' 'gl2' 'gl3' 'gl4' 'gl5' 'gl6' 'gl7' 'gl8' 'gl9' 'gl10' 'gl11' 'gl12' 'gl13' 'gl14' 'gl15' 'gl16' 'gl17' 'gl18' 'gl19' 'gl20' 'gl21' 'gl22' 'gl23' 'gl24' 'gl25' 'gl26' 'gl27' 'gl28' 'gl29' 'gl30' 'gl31' 'si0' 'si1' 'si2' 'si3' 'si4' 'si5' 'si6' 'si7' 'st0' 'st1' 'st2' 'st3' 'st4' 'st5' 'st6' 'st7' 'self' 'true' 'false' 'nil' 'cNeg1' 'c0' 'c1' 'c2' 'retSelf' 'retTrue' 'retFalse' 'retNil' 'ret' 'blockRet' 'bad' 'bad' 'xLoad1' 'xSto' 'xPopSto' 'xSend' 'xxSend' 'xSuper' 'xxSuper' 'pop' 'dup' 'current' 'bad' 'bad' 'bad' 'bad' 'bad' 'bad' 'j1' 'j2' 'j3' 'j4' 'j5' 'j6' 'j7' 'j8' 'b1' 'b2' 'b3' 'b4' 'b5' 'b6' 'b7' 'b8' 'jB' 'jB' 'jB' 'jB' 'jF' 'jF' 'jF' 'jF' 'bT' 'bT' 'bT' 'bT' 'bF' 'bF' 'bF' 'bF' 'sPlus' 'sMinus' 'sLss' 'sGtr' 'sLeq' 'sGeq' 'sEqu' 'sNeq' 'sTimes' 'sOver' 'sRem' 'sMakePt' 'sLshift' 'sLxor' 'sLand' 'sLor' 'sSub' 'sSubGets' 'sLength' 'sNext' 'sNextGets' 'sEnd' 'sEq' 'sClass' 'sBCopy' 'sValue' 'sValue:' 'bad' 'sNew' 'sNew:' 'sX' 'sY' 's0.0' 's0.1' 's0.2' 's0.3' 's0.4' 's0.5' 's0.6' 's0.7' 's0.8' 's0.9' 's0.10' 's0.11' 's0.12' 's0.13' 's0.14' 's0.15' 's1.0' 's1.1' 's1.2' 's1.3' 's1.4' 's1.5' 's1.6' 's1.7' 's1.8' 's1.9' 's1.10' 's1.11' 's1.12' 's1.13' 's1.14' 's1.15' 's2.0' 's2.1' 's2.2' 's2.3' 's2.4' 's2.5' 's2.6' 's2.7' 's2.8' 's2.9' 's2.10' 's2.11' 's2.12' 's2.13' 's2.14' 's2.15' ).
	byteNames length ≠ 256
	  ifTrue: [user notify: 'not 256 byte codes']
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
literals | lit |
	 "Return vector of literals"
	^1 ~ self numLiterals transform⦂ [:lit | lit] to⦂ [(self objectAt: lit + 2)]
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
objectAt: i put: val | |<primitive: 74>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^self word: i ← val asOop]
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
objectAt: i | |<primitive: 73>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^(self word: i) asObject]
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
literalAt: i put: val | |
	^self objectAt: i + 2 put: val
! !
!CompiledMethod methodsFor: 'Literals' stamp: ''!
literalAt: i | |
	^self objectAt: i + 2
! !
!CompiledMethod methodsFor: 'Source Code' stamp: ''!
setSourcePosition: pos inFile: f | loc p1 |
	 "set last three bytes to be pos in file (0-3)"
	f > 3
	  ifTrue: [user notify: 'invalid file number']
	  ifFalse:
		[loc ← self length - 2.
		self ◦ (loc + 2) ← f * 64 + (p1 ← pos / 65536).
		p1 > 62
		  ifTrue:
			[user show: 'Source file is getting full!!'.
			user cr].
		pos ← pos - (p1 * 65536) "into two bytes".
		self ◦ (loc + 1) ← p1 ← pos / 256.
		pos ← pos land: 255 "into one byte".
		self ◦ loc ← pos]
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
!CompiledMethod methodsFor: 'Source Code' stamp: ''!
getSource: class | loc file pos char len str |
	 "get source text from file, or decompile"
	SourceFiles≡nil ifTrue:
		[^LADDecompiler new decompile: self selector in: class method: self].
	loc ← self length.
	file ← self ◦ loc.
	pos ← file land: 63 "high order 6 bits of pos".
	file ← file / 64 "top 2 bits are file index".
	pos ← pos * 256 + (self ◦ (loc - 1)).
	pos ← pos * 256 + (self ◦ (loc - 2)).
	pos = 0 ifTrue: [^LADDecompiler new decompile: self selector in: class method: self].
	file ← SourceFiles ◦ (file + 1) "get to correct file (should be readonly)".
	file position← pos "read source text, this should be merged with filin stuff.
		For now, it does not check matching selectors!!".
	char ← file next.
	char ≠ 34
	  ifTrue: [user notify: 'not pointing at length comment']
	  ifFalse:
		[len ← file integerScan.
		[(char ← file next) = 91] whileFalseDo:  [ "skip class and category"
			len ← len - 1 "and decrease len of code"].
		len ← len - 2 "subtract for ].<cr> after code".
		str ← String new: len.
		file into: str "get code".
		^str asParagraph makeBoldPattern]
! !
Object subclass: #ContextPart
	instanceVariableNames: 'sender pc stackp'
	classVariableNames: ''
	category: 'Interpreter'!
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm | mclass |
	strm append: self receiver class title.
	mclass ← self mclass.
	mclass ≡ self receiver class
	  ifFalse:
		[strm append: '('.
		strm append: mclass title.
		strm append: ')'].
	strm append: '>>'.
	strm append: self method selector
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
releaseFully | | self releaseTo: nil
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
release | | self releaseTo: nil
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
tempNames | mcl code |
	mcl ← self mclass.
	code← (mcl canunderstand: self selector)
		ifTrue: [self mclass code: self selector]
		ifFalse: [(LADDecompiler new
			decompile: self selector in: mcl method: self method) asString].
	^LADParser new parseArgsAndTemps: code asStream notifying: false
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
mclass | mclass sel meth |
	mclass ← self receiver class.
	sel ← self selector.
	meth← self method.
	[mclass superclass ≡ nil or: [(mclass method: sel)≡meth]]
		whileFalseDo:  [mclass ← mclass superclass].
	^mclass
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
releaseTo: caller | c | "release frames to break cycles"
	c ← self.
	[c ≡ nil or: [c≡caller]] whileFalseDo: 
		[c erase.
		c ← c sender]
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
swapSender: coroutine | oldSender |
	oldSender ← sender.
	sender ← coroutine.
	^oldSender
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
push: val | |
	^self ◦ (stackp ← stackp + 1) ← val
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
selector | |
	^self method selector
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
sender | |
	^sender
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
pop | val |
	val ← self ◦ stackp.
	stackp ← stackp - 1.
	^val
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
sender← t1 | |
	sender ← t1.
	^sender
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
erase | i |
	 "release frames to break cycles"
	(1 to: stackp) do: [:i | self ◦ i ← nil]
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
variableNamesInto: dest with: block | n |
	self tempNames transform⦂ [:n | n] to⦂ [(dest identifier: n)]
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
stack | a strm |
	strm ← (Vector new: 20) asStream.
	strm next← a ← self.
	[(a ← a sender) ≡ nil] whileFalseDo:  [strm next← a].
	^strm contents
! !
!ContextPart methodsFor: 'As yet unclassified' stamp: ''!
tempframe | i |
	^1 ~ self method numTemps collect: [:i | self tempAt: i]
! !
Object subclass: #Message
	instanceVariableNames: 'selector args'
	classVariableNames: ''
	category: 'Interpreter'!
!Message methodsFor: 'As yet unclassified' stamp: ''!
selector | |
	^selector
! !
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
selector: sel | |
	selector ← sel.
	args ← #()
! !
ContextPart subclass: #MethodContext
	instanceVariableNames: 'method receiverMap receiver'
	classVariableNames: ''
	category: 'Interpreter'!
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
tempAt: index put: value | |
	^self ◦ index ← value
! !
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
method | |
	^method
! !
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
receiver | |
	^receiver
! !
!MethodContext methodsFor: 'Access to Parts' stamp: ''!
tempAt: index | |
	^self ◦ index
! !
!MethodContext methodsFor: 'Blocks' stamp: ''!
blockCopy: nargs | |
	^(BlockContext new: self length) home: self startpc: pc + 2 nargs: nargs
! !
!MethodContext methodsFor: 'Blocks' stamp: ''!
remoteCopy | |
	^self blockCopy: 0
! !
!MethodContext methodsFor: 'Debugger' stamp: ''!
erase | |
	receiver ← nil.
	super erase
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
goBaby | height |
	NoteTaker ← true.
	externalViews ← Set new vector: 10.
	height ← 480.
	user currentDisplay: (Form new extent: 640 ⌾ height bits: (Bitmap new: 640 / 16 * height) offset: nil).
	user restore. user waitbug.
	[true] whileTrueDo: [self run]
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
systemRestart | |
	thisContext releaseTo: RestartCode home.
	RestartCode eval
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
pc: t1 stackp: t2 | |
	pc ← t1.
	stackp ← t2
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
run | |
	self setRestart⦂ [(^nil)].
	user restore.
	user run
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
sender: t1 receiver: t2 mclass: ignored method: t4 | |
	sender ← t1.
	receiver ← t2.
	method ← t4
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
goBaby | height |
	NoteTaker ← true.
	MethodKeeper ← (Vector new: 10) asStream.
	externalViews ← Set new vector: 10.
	height ← 760 " 480 376 ".
	user currentDisplay: (Form new extent: 640 ⌾ height bits: (Bitmap new: 640 / 16 * height) offset: nil).
	[true] whileTrueDo: [self run]
! !
!MethodContext methodsFor: 'Initialization' stamp: ''!
systemRestart | |
	thisContext releaseTo: RestartCode home.
	NormalCursor topage1.
	RestartCode eval
! !
LADDictionary subclass: #MethodDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Interpreter'!
!MethodDictionary methodsFor: 'As yet unclassified' stamp: ''!
delete: key  | |
	self become: (self copy deleteDangerously: key)
! !
!MethodDictionary methodsFor: 'As yet unclassified' stamp: ''!
deleteDangerously: key  | |
	super delete: key
! !
!MethodDictionary methodsFor: 'As yet unclassified' stamp: ''!
method: sel | |
	^self at: sel ifAbsent⦂ [(self error: sel + ' not found')]
! !
!MethodDictionary methodsFor: 'As yet unclassified' stamp: ''!
methodorfalse: sel | |
	^self at: sel ifAbsent⦂ [(^false)]
! !
!MethodDictionary methodsFor: 'As yet unclassified' stamp: ''!
has: key | probe |
	(probe ← super at: (self find: key)) ≡ nil
	  ifTrue: [^false].
	^probe key = key
! !
