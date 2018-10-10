LADParseNode subclass: #LADAssignmentNode
	instanceVariableNames: 'variable value'
	classVariableNames: ''
	category: 'Compiler'!
!LADAssignmentNode methodsFor: 'Code Generation' stamp: ''!
emitForEffect: stack on: strm | |
	value emitForValue: stack on: strm.
	variable emitStorePop: stack on: strm
! !
!LADAssignmentNode methodsFor: 'Code Generation' stamp: ''!
sizeForValue: encoder | |
	^(value sizeForValue: encoder) + (variable sizeForStore: encoder)
! !
!LADAssignmentNode methodsFor: 'Code Generation' stamp: ''!
emitForValue: stack on: strm | |
	value emitForValue: stack on: strm.
	variable emitStore: stack on: strm
! !
!LADAssignmentNode methodsFor: 'Code Generation' stamp: ''!
sizeForEffect: encoder | |
	^(value sizeForValue: encoder) + (variable sizeForStorePop: encoder)
! !
!LADAssignmentNode methodsFor: 'Initialization' stamp: ''!
variable: var value: exp from: encoder | |
	 "compile"
	(var is: LADMessageNode)
	  ifTrue: [^var store: exp from: encoder].
	variable ← var.
	value ← exp
! !
!LADAssignmentNode methodsFor: 'Initialization' stamp: ''!
variable: var value: exp | |
	variable ← var.
	value ← exp
! !
!LADAssignmentNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level precedence: p | |
	p < 4
	  ifTrue: [strm append: '('].
	self printon: strm indent: level.
	p < 4
	  ifTrue: [strm append: ')']
! !
!LADAssignmentNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | |
	variable printon: strm indent: level.
	strm append: ' ← '.
	value printon: strm indent: level + 2
! !
!LADAssignmentNode methodsFor: 'Simplifying' stamp: ''!
simplify | |
	value ← value simplify
! !
LADParseNode subclass: #LADBlockNode
	instanceVariableNames: 'arguments statements returns nArgsNode size remoteCopyNode'
	classVariableNames: ''
	category: 'Compiler'!
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
sizeForValue: encoder | arg |
	nArgsNode ← encoder encodeLiteral: arguments length.
	remoteCopyNode ← encoder encodeSelector: #blockCopy:.
	size ← (self sizeForEvaluatedValue: encoder) + (self returns
			  ifTrue: [0]
			  ifFalse: [1]) "endBlock".
	arguments do: [:arg | size ← size + (arg sizeForStorePop: encoder)].
	^1 + (nArgsNode sizeForValue: encoder) "current" + (remoteCopyNode size: encoder) + 2 "long jmp" + size
! !
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
emitForEvaluatedValue: stack on: strm | |
	self emitExceptLast: stack on: strm.
	returns
	  ifTrue: [statements last emitForReturn: stack on: strm]
	  ifFalse: [statements last emitForValue: stack on: strm]
! !
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
sizeExceptLast: encoder | i c |
	c ← 0.
	(1 to: statements length - 1) do: [:i | c ← c + (statements ◦ i sizeForEffect: encoder)].
	^c
! !
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
sizeForEvaluatedValue: encoder | |
	^(self sizeExceptLast: encoder) + (returns
	  ifTrue: [statements last sizeForReturn: encoder]
	  ifFalse: [statements last sizeForValue: encoder])
! !
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
emitForValue: stack on: strm | arg oldRemote |
	strm next← LdThisContext.
	stack push: 1.
	nArgsNode emitForValue: stack on: strm.
	remoteCopyNode emit: stack args: 1 on: strm.
	self emitLongJump: size on: strm "jmp-around must be 2 bytes".
	stack push: arguments length.
	arguments reverse do: [:arg | arg emitStorePop: stack on: strm].
	self emitForEvaluatedValue: stack on: strm.
	self returns ifFalse: [strm next← EndRemote].
	stack pop: 1
! !
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
emitExceptLast: stack on: strm | i |
	(1 to: statements length - 1) do: [:i | statements ◦ i emitForEffect: stack on: strm]
! !
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
sizeForEvaluatedEffect: encoder | |
	returns
	  ifTrue: [^self sizeForEvaluatedValue: encoder].
	^(self sizeExceptLast: encoder) + (statements last sizeForEffect: encoder)
! !
!LADBlockNode methodsFor: 'Code Generation' stamp: ''!
emitForEvaluatedEffect: stack on: strm | |
	returns
	  ifTrue:
		[self emitForEvaluatedValue: stack on: strm.
		stack pop: 1]
	  ifFalse:
		[self emitExceptLast: stack on: strm.
		statements last emitForEffect: stack on: strm]
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
default | |
	 "[] argument of missing kwyword in ifTrue: or ifFalse:"
	statements ← NodeNullBlock.
	arguments ← Vector new: 0.
	returns ← false
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
arguments: argNodes statements: stmts returns: returnBool from: encoder | |
	 "compile"
	arguments ← argNodes.
	statements ← (stmts length > 0
			  ifTrue: [stmts]
			  ifFalse:
				[argNodes length > 0
				  ifTrue: [{stmts , arguments last}]
				  ifFalse: [NodeNullBlock]]).
	returns ← returnBool
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
arguments: argNodes | |
	 "decompile"
	arguments ← argNodes
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
mustReturn | |
	returns
	  ifFalse:
		[((statements last is: LADMessageNode) and: [statements last isReturningIf])
		  ifFalse:
			[returns ← true.
			statements ≡ NodeNullBlock
			  ifTrue: [statements ← NodeSelf inVector]
			  ifFalse: [statements ← {statements , NodeSelf}]]]
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
quickCode | |
	(statements length = 1 and: [(statements ◦ 1 is: LADVariableNode)])
	  ifTrue: [^(statements ◦ 1) code].
	^false
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
returnLast | |
	returns ← true
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
returns | |
	^returns
! !
!LADBlockNode methodsFor: 'Initialization' stamp: ''!
statements: stmts returns: returnBool | penult |
	 "decompile"
	statements ← ((stmts length > 1 and: [(penult ← stmts ◦ (stmts length - 1).
			(penult is: LADMessageNode) and: [penult isReturningIf])])
			  ifTrue:
				[returnBool ← false.
				(stmts ◦ (1 to: stmts length - 1)) copy]
			  ifFalse:
				[stmts length = 0
				  ifTrue: [NodeNullBlock]
				  ifFalse: [stmts]]).
	arguments ← Vector new: 0.
	returns ← returnBool
! !
!LADBlockNode methodsFor: 'Printing' stamp: ''!
printArgumentsOn: strm indent: level | arg |
	arguments length = 0
	  ifFalse:
		[arguments reverse do:
			[:arg | strm append: ':'.
			strm append: arg key.
			strm space].
		strm append: '| ']
! !
!LADBlockNode methodsFor: 'Printing' stamp: ''!
printStatementsOn: strm indent: level | i len shown |
	len ← shown ← statements length.
	(level = 1 and: [statements ◦ len ≡ NodeSelf])
	  ifTrue: [shown ← 1 max: shown - 1]
	  ifFalse:
		[(len = 1 and: [statements ◦ 1 ≡ NodeNil])
		  ifTrue: [shown ← shown - 1]].
	(1 to: shown) do:
		[:i | (i = len and: [returns])
		  ifTrue: [strm append: '^'].
		statements ◦ i printon: strm indent: level.
		i < shown
		  ifTrue:
			[strm append: '.'.
			strm crtab: level]]
! !
!LADBlockNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | |
	"statements length ≤ 1
	  ifFalse: [strm crtab: level]."
	strm append: '['.
	self printArgumentsOn: strm indent: level.
	self printStatementsOn: strm indent: level.
	strm append: ']'
! !
!LADBlockNode methodsFor: 'Simplifying' stamp: ''!
justNil | |
	returns
	  ifTrue: [^false].
	^statements length = 1 and: [statements ◦ 1 ≡ NodeNil]
! !
!LADBlockNode methodsFor: 'Simplifying' stamp: ''!
canBeSpecialArgument | |
	 "can I be an argument of (e.g.) ifTrue:?"
	^arguments length = 0
! !
!LADBlockNode methodsFor: 'Simplifying' stamp: ''!
simplify | i |
	(1 to: statements length) do: [:i | statements ◦ i ← (statements ◦ i) simplify]
! !
!LADBlockNode methodsFor: 'Simplifying' stamp: ''!
justFalse | |
	returns
	  ifTrue: [^false].
	^statements length = 1 and: [statements ◦ 1 ≡ NodeFalse]
! !
!LADBlockNode methodsFor: 'Simplifying' stamp: ''!
justTrue | |
	returns
	  ifTrue: [^false].
	^statements length = 1 and: [statements ◦ 1 ≡ NodeTrue]
! !
LADParseNode subclass: #LADCascadeNode
	instanceVariableNames: 'receiver messages'
	classVariableNames: ''
	category: 'Compiler'!
!LADCascadeNode methodsFor: 'Code Generation' stamp: ''!
sizeForValue: encoder | msg size |
	size ← (receiver sizeForValue: encoder) + (messages length - 1 * 2).
	messages do: [:msg | size ← size + (msg sizeForValue: encoder)].
	^size
! !
!LADCascadeNode methodsFor: 'Code Generation' stamp: ''!
emitForValue: stack on: strm | i |
	receiver emitForValue: stack on: strm.
	(1 to: messages length - 1) do:
		[:i | strm next← Dup.
		stack push: 1.
		messages ◦ i emitForValue: stack on: strm.
		strm next← Pop.
		stack pop: 1].
	messages last emitForValue: stack on: strm
! !
!LADCascadeNode methodsFor: 'Initialization' stamp: ''!
receiver: rcvr messages: msgs | |
	 "user show: 'abc'; tab; show: 'abc'; cr"
	receiver ← rcvr.
	messages ← msgs
! !
!LADCascadeNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | i |
	receiver printon: strm indent: level.
	(1 to: messages length) do:
		[:i | messages ◦ i printon: strm indent: level.
		i < messages length
		  ifTrue: [strm append: ';']]
! !
!LADCascadeNode methodsFor: 'Simplifying' stamp: ''!
simplify | msg |
	messages do: [:msg | msg ← msg simplify]
! !
LADParseNode subclass: #LADCollectionNode
	instanceVariableNames: 'elements lengthAsNode elementStreamNode insertIntoNode asArrayNode'
	classVariableNames: ''
	category: 'Compiler'!
!LADCollectionNode methodsFor: 'Code Generation' stamp: ''!
sizeForValue: encoder | element c |
	c ← (lengthAsNode sizeForValue: encoder) + (elementStreamNode size: encoder) + (asArrayNode size: encoder) + (elements length * (insertIntoNode size: encoder)).
	elements do: [:element | c ← c + (element sizeForValue: encoder)].
	^c
! !
!LADCollectionNode methodsFor: 'Code Generation' stamp: ''!
emitForValue: stack on: strm | element |
	lengthAsNode emitForValue: stack on: strm.
	elementStreamNode emit: stack args: 0 on: strm.
	elements do:
		[:element | element emitForValue: stack on: strm.
		insertIntoNode emit: stack args: 1 on: strm].
	asArrayNode emit: stack args: 0 on: strm
! !
!LADCollectionNode methodsFor: 'Initialization' stamp: ''!
elements: vec | |
	 "Decompile via MessageNode simplify"
	elements ← vec
! !
!LADCollectionNode methodsFor: 'Initialization' stamp: ''!
elements: vec from: encoder | |
	 "compile"
	elements ← vec.
	lengthAsNode ← encoder encodeLiteral: vec length.
	elementStreamNode ← encoder encodeSelector: #elementStream.
	insertIntoNode ← encoder encodeSelector: #nextElement:.
	asArrayNode ← encoder encodeSelector: #asArray
! !
!LADCollectionNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | i len |
	len ← elements length.
	strm append: '{'.
	(1 to: len) do:
		[:i | elements ◦ i printon: strm indent: level.
		i < len
		  ifTrue:
			[strm append: ','.
			strm space]].
	strm append: '}'
! !
!LADCollectionNode methodsFor: 'Simplifying' stamp: ''!
simplify | i |
	(1 to: elements length) do: [:i | elements ◦ i ← (elements ◦ i) simplify]
! !
Object subclass: #LADCompiler
	instanceVariableNames: 'sourceStream requestor class instance context failure failureValue huhFlag'
	classVariableNames: ''
	category: 'Compiler'!
!LADCompiler methodsFor: 'Errors' stamp: ''!
notify: string | |
	failure ← true.
	user restoredisplay.
	failureValue ← requestor notify: string at: (sourceStream position min: sourceStream limit) + 1 in: sourceStream
! !
!LADCompiler methodsFor: 'Errors' stamp: ''!
interactive || ^requestor interactive
! !
!LADCompiler methodsFor: 'Private' stamp: ''!
translate: stream noPattern: noPattern | tree |
	tree ← LADParser new parse: stream class: class noPattern: noPattern context: context notifying: self.
	failure ifTrue: [^false].
	^tree
! !
!LADCompiler methodsFor: 'Private' stamp: ''!
from: source class: cls instance: rcvr context: ctxt notifying: req | |
	sourceStream ← source asStream.
	class ← cls.
	instance ← rcvr.
	context ← ctxt.
	requestor ← req.
	failure ← false
! !
!LADCompiler methodsFor: 'Public' stamp: ''!
compile: parag in: cls under: category notifying: t4 | methodNode |
	requestor ← t4.
	self from: parag class: cls instance: false context: false notifying: requestor.
	(methodNode ← user displayoffwhile⦂ [(self translate: sourceStream noPattern: false)])
	  ifTrue: [^methodNode installIn: cls under: category source: parag].
	^false
! !
!LADCompiler methodsFor: 'Public' stamp: ''!
evaluate: stream in: ctxt to: receiver notifying: t4 | methodNode method cls |
	requestor ← t4.
	cls ← (ctxt ifTrue: [ctxt receiver] ifFalse: [receiver]) class.
	self from: stream class: cls instance: receiver context: ctxt notifying: requestor.
	(methodNode ← user displayoffwhile⦂ [(self translate: stream noPattern: true)])
	  ifTrue:
		[method ← methodNode generate.
		context ifTrue: [^instance execute: method with: context].
		^instance execute: method]
	  ifFalse: [^failureValue]
! !
LADParseNode subclass: #LADDecompiler
	instanceVariableNames: 'postfixStream prefixStream here hereType ahead aheadType literals instVars tempVars symbolTables nArgs nLits beginStream nextBegin'
	classVariableNames: 'typeTable baseTable specialTable constantTable ST8'
	category: 'Compiler'!
!LADDecompiler methodsFor: 'Decoding' stamp: ''!
decodeConstant: lowbits | |
	lowbits ≥ 4
	  ifTrue: [^LADLiteralNode new key: ¬1 + lowbits - 4 code: LdMinus1 + lowbits - 4].
	lowbits = 0
	  ifTrue: [^NodeSelf].
	lowbits = 1
	  ifTrue: [^NodeTrue].
	lowbits = 2
	  ifTrue: [^NodeFalse].
	lowbits = 3
	  ifTrue: [^NodeNil].
	self confused: lowbits
! !
!LADDecompiler methodsFor: 'Decoding' stamp: ''!
decodeSelector: which | |
	^LADSelectorNode new key: (literals ◦ (which + 1)) key index: which type: SendType
! !
!LADDecompiler methodsFor: 'Decoding' stamp: ''!
decodeTemporaryVariable: which | |
	^tempVars ◦ (which + 1)
! !
!LADDecompiler methodsFor: 'Decoding' stamp: ''!
decodeInstanceVariable: which | |
	^instVars ◦ (which + 1)
! !
!LADDecompiler methodsFor: 'Decoding' stamp: ''!
decodeLiteral: which | |
	^literals ◦ (which + 1)
! !
!LADDecompiler methodsFor: 'Decoding' stamp: ''!
decodeSpecial: which | |
	^LADSelectorNode new key: specialTable ◦ (which + 1) code: SendPlus + which
! !
!LADDecompiler methodsFor: 'Decoding' stamp: ''!
decodePoolVariable: which | global ref name |
	ref ← (literals ◦ (which + 1)) key.
	symbolTables do:
		[:global | (name ← global invertRef: ref)
		  ifTrue: [^LADVariableNode new name: name key: ref index: which type: LdLitIndType]].
	self error: 'Obsolete global/class/pool variable referenced'
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
message: selector | sel prec rcvr n args i supered |
	 "receiver selector arguments"
	supered ← self match: #:super.
	sel ← selector key.
	n ← sel numArgs.
	args ← Vector new: n.
	(n to: 1 by: ¬1) do: [:i | args ◦ i ← self expression].
	prec ← (sel isinfix ifTrue: [2] ifFalse: [sel iskeyword ifTrue: [3] ifFalse: [1]]).
	hereType ≡ #:pop "last msg of cascade"
	  ifTrue: [^self cascade: (LADMessageNode new receiver: false
			selector: selector arguments: args precedence: prec)].
	rcvr ← (supered
			  ifTrue: [self exactly: NodeSelf. NodeSuper]
			  ifFalse:
				[(self match: #:dup)
				  ifTrue: [false]
				  ifFalse: [ "prior msgs of cascade"
					self expression]]).
	^LADMessageNode new receiver: rcvr
		selector: selector arguments: args precedence: prec
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
method: selOrFalse primitive: primitive literals: lits class: class | method |
	 "pattern block"
	method ← LADMethodNode new selector: (selOrFalse
			  ifTrue: [LADSelectorNode new key: selOrFalse code: nil]
			  ifFalse: [selOrFalse]) arguments: self arguments precedence: (selOrFalse isinfix
			  ifTrue: [2]
			  ifFalse:
				[selOrFalse iskeyword
				  ifTrue: [3]
				  ifFalse: [1]]) temporaries: self temporaries block: self statements encoder: (LADEncoder new initScopeAndLiteralTables nTemps: tempVars length literals: lits class: class) primitive: primitive.
	^method
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
assignment | var |
	var ← self advance.
	self require: #:store.
	^LADAssignmentNode new variable: var value: self expression
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
conditional: withElse | if then else |
	 "if ifTrue: then ifFalse: else"
	withElse
	  ifTrue:
		[else ← self block.
		self require: #:come]
	  ifFalse: [else ← LADBlockNode default].
	then ← self block.
	self require: #:come.
	^LADMessageNode new receiver: self expression selector: (LADSelectorNode new key: #ifTrue:ifFalse: code: #macro) arguments: {then , else} precedence: 3
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
statements | stmts e returns |
	returns ← self match: #:endMethod.
	stmts ← (Vector new: 10) asStream.
	e ← self expression.
	(self match: #:endMethod)
	  ifTrue: [e ← self expression "last return was inaccessible"].
	[e] whileTrueDo:
		[stmts next← e.
		e ← ((self match: #:pop)
				  ifTrue: [self expression]
				  ifFalse: [self control])].
	^LADBlockNode new statements: stmts reverseContents returns: returns
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
control | |
	 "conditional | loop"
	(self match: #:then)
	  ifTrue:
		[(self match: #:else)
		  ifTrue:
			[(self match: #:while)
			  ifTrue: [^self while: false].
			^self conditional: true]
		  ifFalse:
			[(self match: #:while)
			  ifTrue: [^self while: true].
			^self conditional: false]]
	  ifFalse:
		[(self match: #:else)
		  ifTrue: [^self remote].
		^false]
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
while: truth | if then |
	 "if whileTrue: [then]"
	then ← self block.
	self require: #:come.
	truth ≡ false
	  ifTrue: [self require: #:come].
	if ← self block.
	self require: #:begin.
	^LADMessageNode new receiver: if selector: (LADSelectorNode new key: (truth
	  ifTrue: [#whileTrueDo:]
	  ifFalse: [#whileFalseDo:]) code: #macro) arguments: then inVector precedence: 3
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
quickMethod: method selector: selOrFalse | block flags |
	flags ← method flags.
	block ← (flags = 5
		?: [LADBlockNode new default mustReturn]
		~: [flags = 6
			?: [LADBlockNode new statements: (instVars ◦ (method numTemps + 1)) inVector returns: true]
			~: [user notify: 'improper short method']]).
	^LADMethodNode new selector:
		(selOrFalse ?: [LADSelectorNode new key: selOrFalse code: nil] ~: [false])
		arguments: #() precedence:
		(selOrFalse isinfix ?: [2] ~: [selOrFalse iskeyword ?: [3] ~: [1]])
		temporaries: #() block: block encoder: nil primitive: 0
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
expression | e |
	 "assignment | message | variable | literal | control"
	aheadType ≡ #:store
	  ifTrue: [^self assignment].
	(e ← self match: LADVariableNode)
	  ifTrue: [^e].
	(e ← self match: LADSelectorNode)
	  ifTrue: [^self message: e].
	(e ← self match: LADLiteralNode)
	  ifTrue: [^e].
	^self control
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
block | |
	 "ignores for-effect pop"
	self match: #:pop.
	^self statements
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
cascade: lastMessage | messages |
	messages ← lastMessage inVector asStream settoend.
	[self match: #:pop] whileTrueDo: [messages next← self message: (self require: LADSelectorNode)].
	^LADCascadeNode new receiver: self expression messages: messages reverseContents
! !
!LADDecompiler methodsFor: 'Decompiler' stamp: ''!
remote | block args sel |
	 "[{:x} | ...]"
	hereType ≡ #:endMethod
	  ifFalse: [ "endRemote elided"
		self require: #:endRemote].
	block ← self block.
	self require: #:come.
	args ← (Vector new: 2) asStream.
	(self match: #:arguments)
	  ifTrue: [[self match: #:pop] whileTrueDo:
			[args next← self require: LADVariableNode.
			self require: #:store]].
	sel ← self advance.
	sel key = #blockCopy:
	  ifTrue: [self advance].
	self exactly: NodeThisContext.
	^block arguments: args contents
! !
!LADDecompiler methodsFor: 'Errors' stamp: ''!
confused: lowbits | |
	self error: 'unexpected object code'
! !
!LADDecompiler methodsFor: 'Errors' stamp: ''!
confused | |
	self error: 'unexpected object code'
! !
!LADDecompiler methodsFor: 'Initialization' stamp: ''!
decompile: sel in: class | old newBytes tree |
	^self decompile: sel in: class method: (class method: sel)
! !
!LADDecompiler methodsFor: 'Initialization' stamp: ''!
classInit | |  "LADDecompiler classInit."
	typeTable ← "16 groups of 16 byte codes each" 
		#(ldInst: ldTemp: ldLit: ldLit: ldLitInd: ldLitInd: shortStoP: constAndRet:
		sundry: goto: gotoLong: sendSpecial: sendSpecial: send: send: send: ).
	baseTable ← "subtract base of group to get low bits"
		#(0 16 32 32 64 64 96 112
		128 144 160 176 176 208 208 208 ).
	sundryTable ←
		#(longLoad: longSto: longStoPop: longSend: longSend: longSuper: longSuper: pop:
		dup: ldThisContext: ldHome: confused: confused: confused: confused: confused:).
	specialTable ← SpecialOops copy: 10 to: 41
! !
!LADDecompiler methodsFor: 'Initialization' stamp: ''!
decompile: selOrFalse in: class method: method | i primitive nTemps lits |
	instVars ← class instvars.
	1 to: instVars length do:
		[:i | instVars ◦ i ← LADVariableNode new
			name: instVars ◦ i index: i - 1 type: LdInstType].
	method isQuick ifTrue: [^self quickMethod: method selector: selOrFalse].
	 "parse the header"
	nLits ← method numLiterals.
	nTemps ← method numTemps.
	nArgs ← method numArgs.
	(selOrFalse≡#DOIT and: [nArgs=1])
		ifTrue: [selOrFalse← #DOITin:].
	primitive ← method primitive "create symbol tables".
	tempVars ← Vector new: nTemps.
	1 to: nTemps do:
		[:i | tempVars ◦ i ← LADVariableNode new
			name: 't' + i asString index: i - 1 type: LdTempType].
	literals ← Vector new: nLits.
	lits ← method literals.
	1 to: nLits do:
		[:i | literals ◦ i ← LADLiteralNode new
			key: lits ◦ i index: i - 1 type: LdLitType].
	postfixStream ← method asStream.
	postfixStream skip: nLits * 2 + 4.
	postfixStream limit: method endPC. "three on end for file pos"
	symbolTables ← class wholeEnvironment concat: {Smalltalk , Undeclared}.
	self reverse "reverse the bytes".
	^(self method: selOrFalse primitive: primitive literals: lits class: class) simplify
! !
!LADDecompiler methodsFor: 'Reverser' stamp: ''!
reverseTo: limit | |
	[postfixStream position ≥ limit] whileFalseDo:  [self reverseNext]
! !
!LADDecompiler methodsFor: 'Reverser' stamp: ''!
reverse | |
	self backScan "Check for backward jumps".
	nextBegin ← beginStream next.
	prefixStream ← (Vector new: postfixStream limit * 2) asStream.
	self reverseTo: postfixStream limit "Reverse the method".
	prefixStream ← prefixStream reverseContents asStream.
	self advance.
	self advance "Start the scanner"
! !
!LADDecompiler methodsFor: 'Reverser' stamp: ''!
reverseNext | byte group sel thisLoc |
	byte ← postfixStream next "Mark loop beginnings: ".
	[nextBegin and: [postfixStream position = nextBegin]] whileTrueDo:
		[prefixStream last "ST80≡false and⦂" ≡ #:store
		  ifTrue:
			[ "handle for-loop optimization"
			nextBegin ← false "store var test -> store var pop begin var test".
			thisLoc ← postfixStream position.
			postfixStream skip: ¬1.
			self reverseNext.
			prefixStream next← #:pop.
			postfixStream position← thisLoc].
		prefixStream next← #:begin.
		nextBegin ← beginStream next].
	group ← byte / 16 + 1.
	sel ← typeTable ◦ group.
	self perform: sel with: byte - (baseTable ◦ group) "checked in classInit".
	^sel
! !
!LADDecompiler methodsFor: 'Reverser' stamp: ''!
backScan | byte dist start | 
		"quickly locates destinations (begins) of back jumps
		to handle while loops with multi-stmt conditions"
	start ← postfixStream position.
	beginStream ← (Vector new: 2) asStream.
	postfixStream do:
		[:byte | byte < JmpLong
		  ifTrue:
			[byte < 128 ifFalse:
					[byte ≤ 134 ifTrue:
						[postfixStream next "extension bytes".
						(byte = 132 or: [byte = 134])
						  ifTrue: [postfixStream next]]]]
		  ifFalse:
			[byte ≥ SendPlus ifFalse:
				[dist ← byte \ 8 - 4 * 256 + postfixStream next.
				dist ≥ 0 "byte>(JmpLong+3)"
				  ifFalse: [beginStream next← postfixStream position + 1 + dist]]]].
	beginStream ← beginStream contents sort asStream.
	postfixStream position← start
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
longLoad: lowbits | extension type index |
	extension ← postfixStream next.
	type ← extension / 64.
	index ← extension \ 64.
	prefixStream next← (type = 0
	  ifTrue: [self decodeInstanceVariable: index]
	  ifFalse:
		[type = 1
		  ifTrue: [self decodeTemporaryVariable: index]
		  ifFalse:
			[type = 2
			  ifTrue: [self decodeLiteral: index]
			  ifFalse:
				[type = 3
				  ifTrue: [self decodePoolVariable: index]]]])
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
sendSpecial: which | |
	prefixStream next← self decodeSpecial: which
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
ldSelf: lowbits | |
	prefixStream next← #:ldSelf
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
ldLitInd: which | |
	prefixStream next← self decodePoolVariable: which
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
constAndRet: lowbits | |
	lowbits < 8
	  ifTrue: [prefixStream next← self decodeConstant: lowbits]
	  ifFalse:
		[lowbits < 12
		  ifTrue:
			[prefixStream next← self decodeConstant: lowbits - 8.
			self endMethod: lowbits]
		  ifFalse:
			[lowbits = 12
			  ifTrue: [self endMethod: lowbits]
			  ifFalse:
				[lowbits = 13
				  ifTrue: [self endRemote: lowbits]
				  ifFalse: [self confused: lowbits]]]]
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
dup: lowbits | |
	prefixStream next← #:dup
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
gotoLong: nibble | |
	nibble < 8
	  ifTrue: [self jmp: (self long: nibble - 4)]
	  ifFalse: [self bfp: (self long: nibble - 12)]
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
ldThisContext: lowbits | |
	prefixStream next← NodeThisContext
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
sundry: nibble | sel |
	sel ← sundryTable ◦ (nibble + 1).
	sel ◦ 1 = 58 ":"
	  ifTrue: [prefixStream next← sel]
	  ifFalse: [self perform: sel with: nibble]
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
shortStoP: lowbits | |
	prefixStream next← #:store.
	prefixStream next← (lowbits < 8
	  ifTrue: [self decodeInstanceVariable: lowbits]
	  ifFalse: [self decodeTemporaryVariable: lowbits - 8]).
	prefixStream next← #:pop
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
longSuper: lowbits | |
	prefixStream next← #:super.
	prefixStream next← ((lowbits allmask: 1)
	  ifTrue: [self decodeSelector: postfixStream next \ 32]
	  ifFalse:
		[postfixStream next "nargs".
		self decodeSelector: postfixStream next])
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
goto: nibble | |
	nibble < 8
	  ifTrue: [self jmp: nibble + 1]
	  ifFalse: [self bfp: nibble - 7]
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
store: lowbits | |
	prefixStream next← #:store
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
longStoPop: lowbits | |
	self store: lowbits.
	self longLoad: lowbits.
	self pop: lowbits
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
ldInst: which | |
	prefixStream next← self decodeInstanceVariable: which
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
ldLit: which | |
	prefixStream next← self decodeLiteral: which
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
pop: lowbits | |
	prefixStream next← #:pop
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
longSto: lowbits | |
	self store: lowbits.
	self longLoad: lowbits
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
ldTemp: which | |
	prefixStream next← self decodeTemporaryVariable: which
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
endRemote: lowbits | |
	prefixStream next← #:endRemote.
	(postfixStream peek between: JmpLong and: [JmpLong + 3])
	  ifTrue: [postfixStream skip: 2] "skip ST76 back-jmp so it wont make a :while"
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
endMethod: lowbits | |
	prefixStream next← #:endMethod
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
send: lowbits | |
	prefixStream next← self decodeSelector: lowbits \ 16
! !
!LADDecompiler methodsFor: 'Reverser Performs' stamp: ''!
longSend: lowbits | |
	prefixStream next← ((lowbits allmask: 1)
	  ifTrue: [self decodeSelector: postfixStream next \ 32]
	  ifFalse:
		[postfixStream next "nargs".
		self decodeSelector: postfixStream next])
! !
!LADDecompiler methodsFor: 'Reverser Subroutines' stamp: ''!
temporaries | |
	^(tempVars ◦ (nArgs + 1 to: tempVars length)) copy
! !
!LADDecompiler methodsFor: 'Reverser Subroutines' stamp: ''!
long: octal | |
	^octal * 256 + postfixStream next
! !
!LADDecompiler methodsFor: 'Reverser Subroutines' stamp: ''!
abguments | |
	^(tempVars ◦ (1 to: nArgs)) copy
! !
!LADDecompiler methodsFor: 'Reverser Subroutines' stamp: ''!
bfp: dist | |
	self come: dist.
	prefixStream next← #:then
! !
!LADDecompiler methodsFor: 'Reverser Subroutines' stamp: ''!
jmp: dist | nargs argLoc p |
	dist < 0
	  ifTrue: [prefixStream next← #:while]
	  ifFalse:
		[argLoc ← postfixStream position.
		nargs ← 0 "collect arguments to block".
			[(p ← postfixStream peek) = StorePop or:
				[(p between: ShortStoP and: ShortStoP + 15)]]
			whileTrueDo:
				[self reverseNext.
				nargs ← nargs + 1].
		nargs > 0 ifTrue: [prefixStream next← #:arguments].
		self come: dist - (postfixStream position - argLoc).
		prefixStream next← #:else]
! !
!LADDecompiler methodsFor: 'Reverser Subroutines' stamp: ''!
come: dist | |
	prefixStream next← #:come.
	self reverseTo: postfixStream position + dist
! !
!LADDecompiler methodsFor: 'Scanner' stamp: ''!
match: type | |
	hereType ≡ type
	  ifTrue: [^self advance].
	^false
! !
!LADDecompiler methodsFor: 'Scanner' stamp: ''!
exactly: symbol | |
	here ≡ symbol
	  ifTrue: [^self advance].
	self confused
! !
!LADDecompiler methodsFor: 'Scanner' stamp: ''!
advance | old |
	old ← here.
	here ← ahead.
	hereType ← aheadType.
	ahead ← prefixStream next.
	aheadType ← ((ahead is: UniqueString)
			  ifTrue:
				[ahead ◦ 1 = 58
				  ifTrue: [ ":"
					ahead]
				  ifFalse: [#:selector]]
			  ifFalse: [ahead class "VariableNode or LiteralNode"]).
	^old
! !
!LADDecompiler methodsFor: 'Scanner' stamp: ''!
require: type | |
	hereType ≡ type
	  ifTrue: [^self advance].
	self confused
! !
LADParseNode subclass: #LADEncoder
	instanceVariableNames: 'symbolTables scopeTable nTemps supered requestor class literalStream selectorSet litIndSet litSet'
	classVariableNames: ''
	category: 'Compiler'!
!LADEncoder methodsFor: 'Encoding' stamp: ''!
encodeLiteral: object | |
	^self name: object key: object class: LADLiteralNode type: LdLitType set: litSet
! !
!LADEncoder methodsFor: 'Encoding' stamp: ''!
encodeSelector: selector | |
	^self name: selector key: selector class: LADSelectorNode type: SendType set: selectorSet
! !
!LADEncoder methodsFor: 'Encoding' stamp: ''!
litIndex: literal | p |
	p ← literalStream position.
	p = 64
	  ifTrue:
		[requestor notify: 'MORE THAN 64 LITERALS REFERENCED'.
		^0].
	literalStream next← literal.
	^p
! !
!LADEncoder methodsFor: 'Encoding' stamp: ''!
encodeVariable: name | varNode global ref unq |
	(varNode ← scopeTable lookup: name)
	  ifTrue: [^varNode].
	(unq ← name hasBeenUniqued)
	  ifTrue: [symbolTables do:
			[:global | (ref ← global lookupRef: unq)
			  ifTrue: [^self global: ref name: unq]]].
	requestor interactive
	  ifTrue:
		[self notify: 'Undeclared'.
		^0].
	user show: ' (' + name + ' is Undeclared) '.
	unq ← name unique.
	Undeclared declare: unq.
	^self global: (Undeclared ref: unq) name: unq
! !
!LADEncoder methodsFor: 'Errors' stamp: ''!
release | |
	requestor ← false
! !
!LADEncoder methodsFor: 'Errors' stamp: ''!
notify: string | req |
	requestor
	  ifTrue:
		[req ← requestor.
		self release.
		req notify: string]
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
nTemps: n literals: lits class: t3 | |
	 "Decompile"
	class ← t3.
	nTemps ← n.
	(literalStream ← lits asStream) position← lits length
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
newTemp: name | |
	nTemps ← nTemps + 1.
	^LADVariableNode new name: name index: nTemps - 1 type: LdTempType
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
initScopeAndLiteralTables | |
	scopeTable ← Dictionary new copyfrom: stdVariables.
	litSet ← HashSet new copyfrom: stdLiterals.
	selectorSet ← HashSet new copyfrom: stdSelectors.
	litIndSet ← HashSet new init: 16.
	literalStream ← (Vector new: 32) asStream
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
fillDict: dict with: t2 mapping: keys to: codes | cs key |
	class ← t2.
	cs ← codes asStream.
	keys do: [:key | dict insert: key with: (class new name: key key: key code: cs next)]
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
noteSuper | |
	supered ← true
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
bind: name | node |
	 "declare a temporary; error not if a field or class variable"
	((scopeTable has: name) or: [(class classvars has: name)])
	  ifTrue: [self notify: 'Name already used in this class'].
	^self reallyBind: name
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
autoBind: name | node |
	 "declare a block argument as a temp if not already declared"
	(node ← scopeTable lookup: name)
	  ifTrue: [^node].
	^self bind: name
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
fillSet: set with: t2 mapping: keys to: codes | cs key |
	class ← t2.
	cs ← codes asStream.
	keys do: [:key | set insert: (class new key: key code: cs next)]
! !
!LADEncoder methodsFor: 'Initialization' stamp: ''!
init: cls context: ctxt notifying: req | variable node n homeNode indexNode |
	requestor ← req.
	class ← cls.
	nTemps ← 0.
	supered ← false.
	symbolTables ← class wholeEnvironment concat: Smalltalk inVector.
	self initScopeAndLiteralTables.
	n ← ¬1.
	class instvars do:
		[:variable | node ← LADVariableNode new
			name: variable index: (n ← n + 1) type: LdInstType.
		scopeTable insert: variable with: node].
	ctxt ifTrue:
		[homeNode ← self bind: 'homeContext'. "first temp = ctxt passed as arg"
		n ← 0.
		ctxt tempNames do:
			[:variable | indexNode ← self encodeLiteral: (n ← n + 1).
			node ← LADMessageNode new
				receiver: homeNode selector: #tempAt: arguments: indexNode inVector precedence: 3 from: self.
			scopeTable insert: variable with: node]]
! !
!LADEncoder methodsFor: 'Private' stamp: ''!
global: ref name: name | |
	^self name: name key: ref class: LADVariableNode type: LdLitIndType set: litIndSet
! !
!LADEncoder methodsFor: 'Private' stamp: ''!
reallyBind: name | node |
	node ← self newTemp: name.
	scopeTable insert: name with: node.
	^node
! !
!LADEncoder methodsFor: 'Private' stamp: ''!
name: name key: key class: leafNodeClass type: type set: set | t |
	(t ← set lookup: key)
	  ifTrue: [^t].
	t ← leafNodeClass new name: name key: key index: false type: type.
	set insert: t.
	^t
! !
!LADEncoder methodsFor: 'Results' stamp: ''!
literals: primitive nArgs: nArgs | |
	(primitive > 0 or: [nArgs > 4])
	  ifTrue:
		[self litIndex: nArgs * 256 + primitive.
		self litIndex: (Smalltalk ref: class title unique)]
	  ifFalse:
		[supered ifTrue: [self litIndex: (Smalltalk ref: class title unique)]].
	^literalStream contents
! !
!LADEncoder methodsFor: 'Results' stamp: ''!
maxTemp | |
	^nTemps
! !
LADParseNode subclass: #LADLeafNode
	instanceVariableNames: 'key code'
	classVariableNames: ''
	category: 'Compiler'!
!LADLeafNode methodsFor: 'Aspects' stamp: ''!
key | |
	^key
! !
!LADLeafNode methodsFor: 'Aspects' stamp: ''!
= literal | t |
	^key class ≡ literal class and: [(key sameAs: literal)]
! !
!LADLeafNode methodsFor: 'Aspects' stamp: ''!
hash | t |
	^key hash
! !
!LADLeafNode methodsFor: 'Aspects' stamp: ''!
code | |
	^code
! !
!LADLeafNode methodsFor: 'Code Generation' stamp: ''!
sizeForValue: encoder | |
	self reserve: encoder.
	code < 256
	  ifTrue: [^1].
	^2
! !
!LADLeafNode methodsFor: 'Code Generation' stamp: ''!
emitLong: mode on: strm | type index |
	 "emit extended variable access"
	code < 256
	  ifTrue:
		[code < 16
		  ifTrue:
			[type ← 0.
			index ← code]
		  ifFalse:
			[code < 32
			  ifTrue:
				[type ← 1.
				index ← code - 16]
			  ifFalse:
				[code < 96
				  ifTrue:
					[type ← code / 32 + 1.
					index ← code \ 32]
				  ifFalse: [user notify: 'Sends should be handled in SelectorNode']]]]
	  ifFalse:
		[index ← code \ 256.
		type ← code / 256 - 1].
	strm next← mode.
	strm next← type * 64 + index
! !
!LADLeafNode methodsFor: 'Code Generation' stamp: ''!
emitForEffect: stack on: strm | |
	
! !
!LADLeafNode methodsFor: 'Code Generation' stamp: ''!
reserve: encoder | |
	 "if this is a yet unused literal of type -code, reserve it"
	code < 0
	  ifTrue: [code ← self code: (encoder litIndex: key) type: 0 - code]
! !
!LADLeafNode methodsFor: 'Code Generation' stamp: ''!
emitOn: strm | |
	code < 256
	  ifTrue: [strm next← code]
	  ifFalse:
		[strm next← code / 256.
		strm next← code \ 256]
! !
!LADLeafNode methodsFor: 'Code Generation' stamp: ''!
size: encoder | |
	^self sizeForValue: encoder
! !
!LADLeafNode methodsFor: 'Code Generation' stamp: ''!
sizeForEffect: encoder | |
	^0
! !
!LADLeafNode methodsFor: 'Initialization' stamp: ''!
name: literal key: object index: i type: type | |
	self key: object index: i type: type
! !
!LADLeafNode methodsFor: 'Initialization' stamp: ''!
key: object index: i type: type | |
	self key: object code: (self code: i type: type)
! !
!LADLeafNode methodsFor: 'Initialization' stamp: ''!
key: object code: byte | |
	key ← object.
	code ← byte
! !
!LADLeafNode methodsFor: 'Private' stamp: ''!
code: index type: type | |
	index ≡ false
	  ifTrue: [^0 - type].
	CodeLimits ◦ type > index
	  ifTrue: [^CodeBases ◦ type + index].
	^type * 256 + index
! !
LADLeafNode subclass: #LADLiteralNode
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Compiler'!
!LADLiteralNode methodsFor: 'Code Generation' stamp: ''!
emitForValue: stack on: strm | |
	code < 256
	  ifTrue: [strm next← code]
	  ifFalse: [self emitLong: LdInstLong on: strm].
	stack push: 1
! !
!LADLiteralNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | |
	 "wouldn't handle UniqueString literals right"
	((key is: UniqueString) or: [(key is: Vector)])
	  ifTrue: [strm append: '#'].
	key printOn: strm
! !
LADParseNode subclass: #LADMessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes'
	classVariableNames: 'macroTransformers ThenFlag macroEmitters'
	category: 'Compiler'!
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
emitForEffect: stack on: strm | |
	special > 0
	  ifTrue: [self perform: macroEmitters ◦ special with: stack with: strm with: false]
	  ifFalse: [super emitForEffect: stack on: strm]
! !
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
emitWhile: stack on: strm value: forValue | cond stmt stmtSize loopSize |
	 "L1: ... Bfp(L2)|(Bfp(1)Jmp(L2)) ... Jmp(L1) L2:  "
	cond ← receiver.
	stmt ← arguments ◦ 1.
	stmtSize ← sizes ◦ 1.
	loopSize ← sizes ◦ 2.
	cond emitForEvaluatedValue: stack on: strm.
	selector key ≡ #whileFalseDo:
	  ifTrue:
		[self emitBranch: sizes ◦ 3 pop: stack on: strm.
		self emitJump: stmtSize on: strm]
	  ifFalse: [self emitBranch: stmtSize pop: stack on: strm].
	stmt emitForEvaluatedEffect: stack on: strm.
	self emitJump: 0 - loopSize on: strm.
	forValue
	  ifTrue:
		[strm next← LdNil.
		stack push: 1]
! !
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
emitIf: stack on: strm value: forValue | thenExpr thenSize elseExpr elseSize |
	thenSize ← sizes ◦ 1.
	elseSize ← sizes ◦ 2.
	(forValue ≡ false and: [elseSize > 0])
	  ifTrue: [super emitForEffect: stack on: strm]
	  ifFalse:
		[thenExpr ← arguments ◦ 1.
		elseExpr ← arguments ◦ 2.
		receiver emitForValue: stack on: strm.
		self emitBranch: thenSize pop: stack on: strm.
		(forValue ≡ false and: [elseSize = 0])
		  ifTrue: [thenExpr emitForEvaluatedEffect: stack on: strm]
		  ifFalse:
			[thenExpr emitForEvaluatedValue: stack on: strm.
			stack pop: 1.
			thenExpr returns
			  ifFalse: [self emitJump: elseSize on: strm].
			elseExpr emitForEvaluatedValue: stack on: strm]]
! !
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
sizeWhile: encoder value: forValue | cond stmt bfpSize |
	 "L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only)" "justStmt, wholeLoop, justJump"
	cond ← receiver.
	stmt ← arguments ◦ 1.
	sizes ← Vector new: 3.
	sizes ◦ 1 ← (stmt sizeForEvaluatedEffect: encoder) + 2.
	selector key ≡ #whileFalseDo:
	  ifTrue:
		[sizes ◦ 3 ← self sizeJump: sizes ◦ 1.
		bfpSize ← self sizeBranch: sizes ◦ 3]
	  ifFalse:
		[sizes ◦ 3 ← self sizeBranch: sizes ◦ 1.
		bfpSize ← 0].
	sizes ◦ 2 ← (cond sizeForEvaluatedValue: encoder) + bfpSize + (sizes ◦ 3) + (sizes ◦ 1).
	^sizes ◦ 2 + (forValue
	  ifTrue: [1]
	  ifFalse: [0]) "+1 (push nil) for value only"
! !
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
sizeForEffect: encoder | |
	special > 0
	  ifTrue: [^self perform: macroSizers ◦ special with: encoder with: false].
	^super sizeForEffect: encoder
! !
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
sizeIf: encoder value: forValue | thenExpr thenSize elseExpr elseSize |
	thenExpr ← arguments ◦ 1.
	elseExpr ← arguments ◦ 2.
	forValue ≡ false
	  ifTrue:
		[elseExpr justFalse ifTrue:
			[elseSize ← 0.
			thenSize ← thenExpr sizeForEvaluatedEffect: encoder]
		  ifFalse: [^super sizeForEffect: encoder]]
	  ifFalse:
		[elseSize ← elseExpr sizeForEvaluatedValue: encoder.
		thenSize ← (thenExpr sizeForEvaluatedValue: encoder) + (thenExpr returns
				  ifTrue: [0]
				  ifFalse: [self sizeJump: elseSize])].
	sizes ◦ 1 ← thenSize.
	sizes ◦ 2 ← elseSize.
	^(receiver sizeForValue: encoder) + (self sizeBranch: thenSize) + thenSize + elseSize
! !
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
emitForValue: stack on: strm | argument |
	special > 0
	  ifTrue: [self perform: macroEmitters ◦ special with: stack with: strm with: true]
	  ifFalse:
		[receiver
		  ifTrue: [receiver emitForValue: stack on: strm].
		arguments do: [:argument | argument emitForValue: stack on: strm].
		selector emit: stack args: arguments length on: strm super: receiver ≡ NodeSuper]
! !
!LADMessageNode methodsFor: 'Code Generation' stamp: ''!
sizeForValue: encoder | argument c s z |
	special > 0
	  ifTrue: [^self perform: macroSizers ◦ special with: encoder with: true].
	receiver ≡ NodeSuper
	  ifTrue: [selector ← selector copy "only necess for splOops"].
	c ← (selector size: encoder args: arguments length super: receiver ≡ NodeSuper) + (receiver
			  ifTrue: [receiver sizeForValue: encoder]
			  ifFalse: [0]).
	s ← sizes asStream.
	arguments do:
		[:argument | s next← z ← argument sizeForValue: encoder.
		c ← c + z].
	^c
! !
!LADMessageNode methodsFor: 'Initialization' stamp: ''!
cascadeReceiver | rcvr |
	(receiver ≡ NodeSuper or: [special > 0])
	  ifTrue: [^false].
	rcvr ← receiver.
	receiver ← false.
	^rcvr
! !
!LADMessageNode methodsFor: 'Initialization' stamp: ''!
store: expr from: encoder | |
	 "ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment)"
	selector key ≠ #tempAt:
	  ifTrue: [user notify: 'cant transform this message']
	  ifFalse: [^LADMessageNode new receiver: receiver selector: #tempAt:put: arguments: {arguments , expr} precedence: precedence from: encoder]
! !
!LADMessageNode methodsFor: 'Initialization' stamp: ''!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder | |
	 "compile"
	self receiver: rcvr arguments: args precedence: p.
	special ← macroSelectors find: selName.
	(self transform: encoder)
	  ifTrue: [selector ← LADSelectorNode new key: macroSelectors ◦ special code: #macro]
	  ifFalse:
		[selector ← encoder encodeSelector: selName.
		rcvr ≡ NodeSuper
		  ifTrue: [encoder noteSuper]]
! !
!LADMessageNode methodsFor: 'Initialization' stamp: ''!
receiver: rcvr selector: selNode arguments: args precedence: p | |
	 "decompile"
	self receiver: rcvr arguments: args precedence: p.
	special ← macroSelectors find: selNode key.
	selector ← selNode
! !
!LADMessageNode methodsFor: 'Initialization' stamp: ''!
classInit | |  "LADMessageNode classInit."
	macroSelectors ← #(?:  ~:  ?:~:
		ifTrue: ifFalse: ifTrue:ifFalse: and: or: whileFalseDo: whileTrueDo: ).
	macroTransformers ← #(transformIfTrue: transformIfFalse: transformIfTrueIfFalse:
		transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfTrue: transformIfFalse: transformWhile: transformWhile: ).
	macroEmitters ← #(emitIf:on:value: emitIf:on:value: emitIf:on:value:
		emitIf:on:value: emitIf:on:value: emitIf:on:value: emitIf:on:value: emitIf:on:value: emitWhile:on:value: emitWhile:on:value: ).
	macroSizers ← #(sizeIf:value: sizeIf:value: sizeIf:value: 
		sizeIf:value: sizeIf:value: sizeIf:value: sizeIf:value: sizeIf:value: sizeWhile:value: sizeWhile:value: )
! !
!LADMessageNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level precedence: p | parenthesize |
	parenthesize ← precedence > p or: [(p = 3 and: [precedence = 3 "both keywords"])].
	parenthesize
	  ifTrue: [strm append: '('].
	self printon: strm indent: level.
	parenthesize
	  ifTrue: [strm append: ')']
! !
!LADMessageNode methodsFor: 'Printing' stamp: ''!
printIfOn: strm indent: level | |
	(arguments ◦ 2) justNil
	  ifTrue: [^self printKeywords: #?: arguments: (arguments ◦ 1) inVector on: strm indent: level].
	(arguments ◦ 2) justFalse
	  ifTrue: [^self printKeywords: #?: arguments: (arguments ◦ 1) inVector on: strm indent: level].
	(arguments ◦ 1) justNil
	  ifTrue: [^self printKeywords: #~: arguments: (arguments ◦ 2) inVector on: strm indent: level].
	(arguments ◦ 1) justTrue
	  ifTrue: [^self printKeywords: #~: arguments: (arguments ◦ 2) inVector on: strm indent: level].
	self printKeywords: #?:~: arguments: arguments on: strm indent: level
! !
!LADMessageNode methodsFor: 'Printing' stamp: ''!
printKeywords: key arguments: args on: strm indent: level | keywords part prev arg |
	args length = 0
	  ifTrue:
		[strm space.
		strm append: key.
		^self].
	keywords ← key keywords.
	prev ← receiver.
	1 to: args length do:
		[:part | arg ← args ◦ part.
		(((prev is: LADBlockNode) or: [((prev is: LADMessageNode) and: [prev precedence ≥ 3])]) or: [(part > 1 and: [(arg is: LADBlockNode)])])
		  ifTrue: [strm crtab: level + 1]
		  ifFalse: [ "newline after big args"
			strm space].
		key ← keywords ◦ part.
		strm append: key.
		strm space.
		arg printon: strm indent: level + 1 precedence: (precedence = 2
		  ifTrue: [1]
		  ifFalse:
			[key = '←'
			  ifTrue: [4]
			  ifFalse: [precedence]]).
		prev ← arg]
! !
!LADMessageNode methodsFor: 'Printing' stamp: ''!
precedence | |
	^precedence
! !
!LADMessageNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | |
	receiver
	  ifTrue: [receiver printon: strm indent: level precedence: precedence].
	(special between: 1 and: 8)
	  ifTrue: [self printIfOn: strm indent: level]
	  ifFalse: [self printKeywords: selector key arguments: arguments on: strm indent: level]
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
receiver: rcvr arguments: args precedence: p | |
	receiver ← rcvr.
	arguments ← args.
	sizes ← Vector new: arguments length.
	precedence ← p
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
isReturningIf | |
	(special = 3 or: [special=6])
	  ifTrue: [^(arguments ◦ 1) returns and: [(arguments ◦ 2) returns]].
	^false
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
canBeSpecial: node from: encoder | |
	node canBeSpecialArgument
	  ifTrue: [^node is: LADBlockNode].
	^encoder notify: '←Must be a block or a variable'
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
transformWhile: encoder | |
	^(self transformBoolean: encoder) and: [(self canBeSpecial: receiver from: encoder)]
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
transformIfTrueIfFalse: encoder | |
	^(self canBeSpecial: arguments ◦ 1 from: encoder) and: [(self canBeSpecial: arguments ◦ 2 from: encoder)]
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
transformIfFalse: encoder | |
	(self transformBoolean: encoder)
	  ifTrue: [arguments ← {(LADBlockNode new statements: NodeTrue inVector returns: false) , (arguments ◦ 1)}]
	  ifFalse: [^false]
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
transformIfTrue: encoder | |
	(self transformBoolean: encoder)
	  ifTrue: [arguments ← {arguments ◦ 1 , (LADBlockNode new statements: NodeFalse inVector returns: false)}]
	  ifFalse: [^false]
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
transformBoolean: encoder | |
	^self canBeSpecial: arguments ◦ 1 from: encoder
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
transform: encoder | |
	special = 0
	  ifTrue: [^false].
	(self perform: macroTransformers ◦ special with: encoder)
	  ifTrue: [sizes ← Vector new: arguments length]
	  ifFalse:
		[special ← 0.
		^false]
! !
!LADMessageNode methodsFor: 'Private Initialization' stamp: ''!
addLeftArrow: lastArg encoder: encoder | |
	 "for ST76 compatibility"
	selector ← encoder encodeSelector: (selector key + '←') unique.
	arguments ← {arguments , lastArg}
! !
!LADMessageNode methodsFor: 'Simplifying' stamp: ''!
asCollectionElements: length | s |
	 "{1,2,3}"
	(selector key ≡ #elementStream and: [receiver key = length])
	  ifTrue: [^(Vector new: length) asStream].
	(selector key ≡ #nextElement: and: [(s ← receiver asCollectionElements: length + 1)])
	  ifTrue:
		[s next← (arguments ◦ 1) simplify.
		^s].
	^false
! !
!LADMessageNode methodsFor: 'Simplifying' stamp: ''!
simplifyCollection | strm |
	(strm ← receiver asCollectionElements: 0)
	  ifTrue: [^LADCollectionNode new elements: strm contents].
	^false
! !
!LADMessageNode methodsFor: 'Simplifying' stamp: ''!
simplify | n i s |
	((n ← #(asArray ) find: selector key) > 0 and: [(s ← self perform: #(simplifyCollection ) ◦ n)])
	  ifTrue: [^s].
	receiver ifTrue: [receiver ← receiver simplify].
	(1 to: arguments length) do: [:i | arguments ◦ i ← (arguments ◦ i) simplify]
! !
LADParseNode subclass: #LADMethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block literals primitive encoder temporaries'
	classVariableNames: ''
	category: 'Compiler'!
!LADMethodNode methodsFor: 'Code Generation' stamp: ''!
selector | |
	^selectorOrFalse key
! !
!LADMethodNode methodsFor: 'Code Generation' stamp: ''!
quickCode: sel | v quickie |
	sel ≡ false
	  ifTrue: [^false].
	(v ← block quickCode)
	  ifTrue:
		[quickie← CompiledMethod new: (newFormat ?: [2+3] ~: [4+3]).
		newFormat ~: [quickie key: selectorOrFalse key].
		v < 0
		  ifTrue: [^false].
		v = LdSelf
		  ifTrue: [^quickie beReturnSelf].
		v < (CodeBases ◦ LdInstType + (CodeLimits ◦ LdInstType))
		  ifTrue: [^quickie beReturnField: v].
		(v / 256 = 1 and: [v \ 256 < 32])
		  ifTrue: [^quickie beReturnField: v\256].
		^false]
	  ifFalse: [^false]
! !
!LADMethodNode methodsFor: 'Code Generation' stamp: ''!
emitOn: strm | stack limit |
	limit ← strm limit.
	stack ← ParseStack init.
	block emitForValue: stack on: strm.
	strm next← EndMethod.
	strm position = limit
	  ifTrue: [^strm asArray].
	user notify: 'Compiler code size discrepancy'
! !
!LADMethodNode methodsFor: 'Code Generation' stamp: ''!
generate | blkSize method nLits lit stack strm nArgs |
	method ← ((primitive = 0 and: [arguments length = 0])
			  ?: [self quickCode: selectorOrFalse]
			  ~: [false]).
	method ?: [^method].
	nArgs ← arguments length.
	blkSize ← block sizeForEvaluatedValue: encoder.
	literals ← encoder literals: primitive nArgs: nArgs.
	(nLits← literals length) > 63
		?: [^user notify: 'Too many literals referenced'].
	method ← CompiledMethod new: (newFormat ?: [2] ~: [4]) + (2*nLits) + blkSize + 3.
	strm ← method asStream.
	strm position← 2 * literals length + (newFormat ?: [2] ~: [4]).
	stack ← ParseStack init.
	block emitForEvaluatedValue: stack on: strm.
	stack position ≠ 1
		?: [^user notify: 'Compiler stack discrepancy'].
	strm position ≠ (method length - 3)
		?: [^user notify: 'Compiler code size discrepancy'].
	"Header"
	method flags: ((nArgs ≤ 4 and: [primitive = 0]) ?: [nArgs] ~: [7])
		nTemps: encoder maxTemp nStack: stack length nLits: nLits.
	newFormat ~: [method key: selectorOrFalse key].
	1 to: nLits do: [:lit | method literalAt: lit put: literals ◦ lit].
	^strm asArray
! !
!LADMethodNode methodsFor: 'Code Generation' stamp: ''!
installIn: class under: category source: sourceParagraph | selector method |
	selector← selectorOrFalse key.
	method ← self generate.
	class install: method selector: selector under: category source: sourceParagraph.
	method putSource: sourceParagraph text class: class category: category inFile: 2. "changes"
	^selector
! !
!LADMethodNode methodsFor: 'Initialization' stamp: ''!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: t6 primitive: prim | |
	encoder ← t6.
	selectorOrFalse ← selOrFalse.
	precedence ← p.
	arguments ← args.
	temporaries ← temps.
	block ← blk.
	primitive ← prim
! !
!LADMethodNode methodsFor: 'Printing' stamp: ''!
printOn: strm | s args |
	selectorOrFalse
	  ifTrue:
		[(precedence = 1 and: [selectorOrFalse key isarrow ≡ false])
		  ifTrue:
			[ "ST76"
			strm append: selectorOrFalse key.
			strm space]
		  ifFalse:
			[args ← arguments asStream.
			selectorOrFalse key keywords do:
				[:s | strm append: s.
				strm space.
				strm append: args next key.
				strm space]]].
	strm append: ' | '.
	temporaries do:
		[:s | strm append: s key.
		strm space].
	strm append: '|'.
	primitive > 0
	  ifTrue:
		[strm append: '  <primitive: '.
		strm print: primitive.
		strm append: '>'].
	strm crtab: 1.
	block printStatementsOn: strm indent: 1
! !
!LADMethodNode methodsFor: 'Simplifying' stamp: ''!
simplify | |
	block ← block simplify
! !
Object subclass: #LADObject
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Compiler'!
Object subclass: #LADParseNode
	instanceVariableNames: ''
	classVariableNames: 'JmpLong NodeNil Send LdMinus1 StorePop SendLong SendPlus JmpLimit NodeTrue SendType NodeSelf Pop Jmp Bfp Dup stdSelectors LdSelf LdTrue LdNil Store NodeThisContext CodeLimits CodeBases stdLiterals stdVariables ShortStoP SendLimit NodeFalse NodeSuper LdLitType LdTempType LdLitIndType LdInstType NodeNullBlock EndMethod EndRemote LdThisContext LdInstLong LdFalse LdSuper'
	category: 'Compiler'!
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
sizeJump: dist | |
	dist = 0
	  ifTrue: [^0].
	^self sizeShortOrLong: dist
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
sizeForReturn: encoder | |
	^(self sizeForValue: encoder) + 1
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
sizeBranch: dist | |
	dist = 0
	  ifTrue: [^1].
	^self sizeShortOrLong: dist
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
emitLongJump: dist on: strm | |
	 "force a two byte jump"
	self emitLong: dist code: JmpLong on: strm
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
emitForReturn: stack on: strm | |
	self emitForValue: stack on: strm.
	strm next← EndMethod
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
emitJump: dist on: strm | |
	dist = 0
	  ifFalse: [self emitShortOrLong: dist code: Jmp on: strm]
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
emitBranch: dist pop: stack on: strm | |
	stack pop: 1.
	dist = 0
	  ifTrue: [strm next← Pop]
	  ifFalse: [self emitShortOrLong: dist code: Bfp on: strm]
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
emitForEffect: stack on: strm | |
	self emitForValue: stack on: strm.
	strm next← Pop.
	stack pop: 1
! !
!LADParseNode methodsFor: 'Code Generation' stamp: ''!
sizeForEffect: encoder | |
	^(self sizeForValue: encoder) + 1
! !
!LADParseNode methodsFor: 'Encoding' stamp: ''!
encodeSelector: selector | |
	^nil
! !
!LADParseNode methodsFor: 'Initialization' stamp: ''!
classInit | |
	 "LADParseNode classInit.  LADVariableNode classInit." " | x. LADParseNode classvars contents sort transform⦂ x to⦂ x+' '+(LADParseNode classvars ref: x) refct asString " "Smalltalk allCallsOn: (LADParseNode classvars ref: ↪NodeTrueFalse) from: (SystemOrganization superclassOrder: 'S80-Compiler') " "LADParseNode classvars clean."
	.
	LdInstType ← 1.
	LdTempType ← 2.
	LdLitType ← 3.
	LdLitIndType ← 4.
	SendType ← 5.
	CodeBases ← #(0 16 32 64 208 ).
	CodeLimits ← #(16 16 32 32 16 ).
	LdSelf ← 112.
	LdTrue ← 113.
	LdFalse ← 114.
	LdNil ← 115.
	LdMinus1 ← 116.
	LdInstLong ← 128.
	Store ← 129.
	StorePop ← 130.
	ShortStoP ← 96.
	SendLong ← 131.
	LdSuper ← 133.
	Pop ← 135.
	Dup ← 136.
	LdThisContext ← 137.
	EndMethod ← 124.
	EndRemote ← 125.
	Jmp ← 144.
	Bfp ← 152.
	JmpLimit ← 8.
	JmpLong ← 160.
	SendPlus ← 176.
	Send ← 208.
	SendLimit ← 16
! !
!LADParseNode methodsFor: 'Printing' stamp: ''!
printOn: strm | |
	strm append: ''.
	self printon: strm indent: 0.
	strm append: ''
! !
!LADParseNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level precedence: p | |
	self printon: strm indent: level
! !
!LADParseNode methodsFor: 'Private' stamp: ''!
sizeShortOrLong: dist | |
	(1 ≤ dist and: [dist ≤ JmpLimit])
	  ifTrue: [^1].
	^2
! !
!LADParseNode methodsFor: 'Private' stamp: ''!
emitShortOrLong: dist code: shortCode on: strm | |
	(1 ≤ dist and: [dist ≤ JmpLimit])
	  ifTrue: [strm next← shortCode + dist - 1]
	  ifFalse: [self emitLong: dist code: shortCode + 16 on: strm]
! !
!LADParseNode methodsFor: 'Private' stamp: ''!
emitLong: dist code: longCode on: strm | code |
	 "force a two-byte jump"
	code ← longCode.
	dist < 0
	  ifTrue: [dist ← dist + 1024]
	  ifFalse:
		[dist > 1023
		  ifTrue: [dist ← ¬1]
		  ifFalse: [code ← code + 4]].
	dist < 0
	  ifTrue: [user notify: 'A block compiles more than 1K bytes of code']
	  ifFalse:
		[strm next← dist / 256 + code.
		strm next← dist \ 256]
! !
!LADParseNode methodsFor: 'Simplifying' stamp: ''!
canBeSpecialArgument | |
	 "can I be an argument of (e.g.) ifTrue:?"
	^false
! !
!LADParseNode methodsFor: 'Simplifying' stamp: ''!
simplify | |
	
! !
!LADParseNode methodsFor: 'Simplifying' stamp: ''!
asCollectionElements: length | |
	^false
! !
LADScanner subclass: #LADParser
	instanceVariableNames: 'here hereType encoder requestor hereMark'
	classVariableNames: ''
	category: 'Compiler'!
!LADParser methodsFor: 'As yet unclassified' stamp: ''!
pattern | sap |
	 "unarySelector | binarySelector arg | keyword arg {keyword arg} ⇒
			{selector, arguments, precedence}"
	sap ← (hereType ≡ #keyword
			  ifTrue: [self arguments: 999 precedence: 3]
			  ifFalse:
				[(hereType ≡ #binary or: [(hereType ≡ #verticalBar or: [hereType ≡ #comma])])
				  ifTrue: [hereType← #binary.
					self arguments: 1 precedence: 2]
				  ifFalse:
					[hereType ≡ #word
					  ifTrue: [self arguments: 0 precedence: 1]
					  ifFalse: [^false]]]).
	hereType ≡ #leftArrow
	  ifTrue:
		[ "ST76"
		self advance.
		sap ◦ 1 ← encoder encodeSelector: ((sap ◦ 1) key + '←') unique.
		hereType ≡ #word
		  ifTrue:
			[sap ◦ 2 ← {sap ◦ 2 , (encoder bind: self advance)}.
			^sap].
		^self expected: 'argument name']
	  ifFalse: [^sap]
! !
!LADParser methodsFor: 'Errors' stamp: ''!
expected: string | |
	^self notify: string + ' expected'
! !
!LADParser methodsFor: 'Errors' stamp: ''!
interactive || ^requestor interactive
! !
!LADParser methodsFor: 'Errors' stamp: ''!
notify: string | |
	mark ≡ false
	  ifTrue: [^false].
	hereMark ← hereMark +
		(tokenType ≡ #doIt ifTrue: [1] ifFalse: [0]) +
		(hereType ≡ #doIt ifTrue: [1] ifFalse: [0]).
	tokenType ← mark ← false.
	encoder≡nil ifFalse: [encoder release]. "break cycle"
	source skip: hereMark - source position - 1. "**1 too far right now**"
	requestor notify: string.
	here ← 30 inString.
	hereType ← #doIt.
	^false
! !
!LADParser methodsFor: 'Name Parsing for Debugger' stamp: ''!
encodeSelector: sel | |
	^LADSelectorNode new key: sel code: 0
! !
!LADParser methodsFor: 'Name Parsing for Debugger' stamp: ''!
parseArgsAndTemps: sourceStream notifying: req | args temps |
	 "for debugger"
	self init: sourceStream notifying: req.
	encoder ← self.
	args ← self pattern ◦ 2.
	temps ← self temporaries.
	encoder ← nil. "break cycle"
	^args concat: temps
! !
!LADParser methodsFor: 'Name Parsing for Debugger' stamp: ''!
bind: name | |
	^name
! !
!LADParser methodsFor: 'Name Parsing for Debugger' stamp: ''!
unbind: name | |
	
! !
!LADParser methodsFor: 'Parser' stamp: ''!
keywordFreeExpression: cascading | rcvr msg |
	 "argumentFreeExpression {binarySelector argumentFreeExpression} ⇒
			MessageNode"
	(rcvr ← self argumentFreeExpression: cascading)
	  ifTrue:
		[[(hereType ≡ #binary or: [hereType ≡ #verticalBar]) and: [(msg ← self binaryMessage "or⦂ hereType≡↪comma)" "ST76")]] whileTrueDo: [rcvr ← LADMessageNode new receiver: rcvr selector: msg ◦ 1 arguments: msg ◦ 2 precedence: 2 from: encoder].
		^rcvr].
	^false
! !
!LADParser methodsFor: 'Parser' stamp: ''!
more | |
	^hereType ≠ #doIt
! !
!LADParser methodsFor: 'Parser' stamp: ''!
keywordMessage | selector arg args |
	 "{keyword arg} ⇒ {selector, args}"
	selector ← Stream default.
	args ← (Vector new: 4) asStream.
	[hereType ≡ #keyword] whileTrueDo:
		[selector append: self advance.
		(arg ← self keywordFreeExpression: false)
		  ifTrue: [args next← arg]
		  ifFalse: [^self expected: 'argument']].
	^{selector contents unique , args contents}
! !
!LADParser methodsFor: 'Parser' stamp: ''!
primitive | n |
	here ≡ #<
	  ifTrue:
		[self advance.
		here = 'primitive:'
		  ifTrue:
			[self advance.
			((n ← self match: #number) and: [(n Is: Integer)])
			  ifTrue:
				[here ≡ #>
				  ifTrue:
					[self advance.
					^n].
				^self expected: '>']
			  ifFalse: [^self expected: 'Number']]
		  ifFalse: [^self expected: 'primitive:']]
	  ifFalse: [^0]
! !
!LADParser methodsFor: 'Parser' stamp: ''!
statements: argNodes doit: doit | stmts expr returns |
	 " 3. 4. 5. " "{expression '.'} [^]expression ⇒ BlockNode with supplied arguments"
	hereType ≡ #rightBracket
	  ifTrue: [^LADBlockNode new arguments: argNodes statements: #() returns: false from: encoder].
	stmts ← (Vector new: 10) asStream.
	returns ← false.
	[returns ← self match: #upArrow.
	expr ← self expression.
	stmts next← expr
		ifTrue: [expr]
		ifFalse: [encoder encodeVariable: (doit ifTrue: ['nil'] ifFalse: ['self'])].
	returns ≡ false
	  ifTrue: [self match: #period]
	  ifFalse:
		[(hereType ≡ #rightBracket or: [hereType ≡ #doIt])
		  ifTrue: [false]
		  ifFalse: [^self expected: 'end of block']]] whileTrueDo: [].
	^LADBlockNode new arguments: argNodes statements: stmts contents returns: returns from: encoder
! !
!LADParser methodsFor: 'Parser' stamp: ''!
temporaries | vars |
	 "[ '|' {variable} ] ⇒ {variable, ..., variable}"
	(self match: #verticalBar) ifFalse: [^self expected: 'Vertical bar'].
	(self match: #verticalBar) ifTrue: [^#()].
	vars ← (Vector new: 8) asStream.
	[hereType ≡ #word] whileTrueDo: [vars next← encoder bind: self advance].
	(self match: #verticalBar) ifTrue: [^vars contents].
	^self expected: 'Vertical bar'
! !
!LADParser methodsFor: 'Parser' stamp: ''!
simpleExpression: cascading | rcvr msg |
	 "keywordFreeExpression {keyword keywordFreeExpression} ⇒ MessageNode"
	(rcvr ← self keywordFreeExpression: cascading)
	  ifTrue:
		[(hereType ≡ #keyword and: [(msg ← self keywordMessage)])
		  ifTrue: [rcvr ← LADMessageNode new receiver: rcvr selector: msg ◦ 1 arguments: msg ◦ 2 precedence: 3 from: encoder].
		hereType ≡ #leftArrow
		  ifTrue:
			[ "ST76"
			self advance.
			^rcvr addLeftArrow: self expression encoder: encoder].
		^rcvr]
	  ifFalse: [^false]
! !
!LADParser methodsFor: 'Parser' stamp: ''!
method: doit context: ctxt | sap blk prim temps |
	 "sap={selector, arguments, precedence}" "pattern [ | temporaries ] block ⇒ MethodNode"
	doit
	  ifTrue:
		[sap ← (ctxt ≡ false
				  ifTrue: [{(encoder encodeSelector: #DoIt) , #() , 1}]
				  ifFalse: [{(encoder encodeSelector: #DoItIn:) , (encoder encodeVariable: 'homeContext') inVector , 3}]).
		temps ← (hereType ≡ #verticalBar
				  ifTrue: [self temporaries]
				  ifFalse: [#()]).
		(blk ← self statements: #() doit: true)
		  ifTrue: [blk returnLast].
		prim ← 0]
	  ifFalse:
		[(sap ← self pattern)
		  ifTrue:
			[temps ← self temporaries.
			prim ← self primitive.
			blk ← self statements: #() doit: false]].
	(sap and: [blk])
	  ifTrue:
		[self more
		  ifTrue: [^self expected: 'Nothing more'].
		^LADMethodNode new selector: sap ◦ 1 arguments: sap ◦ 2 precedence: sap ◦ 3 temporaries: temps block: blk mustReturn encoder: encoder primitive: prim]
	  ifFalse: [^self expected: 'pattern']
! !
!LADParser methodsFor: 'Parser' stamp: ''!
arguments: maxArgs precedence: p | selector args type |
	 "unarySelector | binarySelector arg | keyword arg {keyword arg} ⇒
		 {selector, arguments, precedence}"
	selector ← Stream default.
	args ← (Vector new: 4) asStream.
	type ← hereType.
	[hereType ≡ type] whileTrueDo:
		[selector append: self advance.
		hereType ≡ #word
		  ifTrue:
			[args position = maxArgs
			  ifTrue: [^self notify: 'Too many arguments'].
			args next← encoder bind: self advance]
		  ifFalse:
			[maxArgs > 0
			  ifTrue: [^self expected: 'argument name']]].
	^{(encoder encodeSelector: selector contents unique) , args contents , p}
! !
!LADParser methodsFor: 'Parser' stamp: ''!
expression | var expr msgs temp rcvr |
	 "[variable '←'] expression ⇒ AssignmentNode
		 rcvr message {; message} ⇒ MessageNode | CascadeNode"
	(hereType ≡ #word and: [tokenType ≡ #leftArrow])
	  ifTrue:
		[var ← encoder encodeVariable: here.
		self advance.
		self advance.
		(expr ← self expression)
		  ifTrue: [^LADAssignmentNode new variable: var value: expr from: encoder].
		^self expected: 'Expression']
	  ifFalse:
		[(expr ← self simpleExpression: false)
		  ifTrue:
			[(hereType ≡ #semicolon and: [(expr is: LADMessageNode)])
			  ifTrue:
				[msgs ← expr inVector asStream settoend.
				rcvr ← expr cascadeReceiver.
				rcvr ≡ false
				  ifTrue: [^self notify: 'cannot cascade super'].
				[hereType ≡ #semicolon] whileTrueDo:
					[expr ← self simpleExpression: true.
					(expr is: LADMessageNode)
					  ifTrue:
						[expr cascadeReceiver
						  ifTrue: [msgs next← expr]
						  ifFalse: [^self notify: 'invalid cascade']]
					  ifFalse: [^self expected: 'Message']].
				^LADCascadeNode new receiver: rcvr messages: msgs contents]
			  ifFalse: [^expr]]
		  ifFalse: [^false]]
! !
!LADParser methodsFor: 'Parser' stamp: ''!
collection | exprs e |
	 "'{' {simpleExpression ','} [simpleExpression] '}' ⇒ CollectionNode"
	(self match: #leftBrace)
	  ifTrue:
		[exprs ← (Vector new: 10) asStream.
		[((e ← self simpleExpression: false) and: [exprs next← e]) and: [(self match: #comma)]] whileTrueDo: [].
		(self match: #rightBrace)
		  ifTrue: [^LADCollectionNode new elements: exprs contents from: encoder].
		^self expected: 'Comma or right brace']
	  ifFalse: [^false]
! !
!LADParser methodsFor: 'Parser' stamp: ''!
block | blk argNodes argument |
	 " '[' {:var.} (:var|statements) ']' ⇒ BlockNode"
	argNodes ← (Vector new: 1) asStream.
	(self match: #leftBracket)
	  ifTrue:
		[[(self match: #colon) and: [((argument ← self match: #word)
		  ifTrue: [argNodes next← encoder autoBind: argument]
		  ifFalse: [^self expected: 'Argument name'])]] whileTrueDo: [].
		(argNodes empty or: [(self match: #verticalBar)]) ≡ false
		  ifTrue: [(self match: #rightBracket) ifTrue: 
					[^LADBlockNode new arguments: argNodes contents
						statements: #() returns: false from: encoder].
				^self expected: 'Vertical bar'].
		((blk ← self statements: argNodes contents doit: false) and: [self match: #rightBracket])
		  ifTrue: [^blk].
		^self expected: 'Period or right bracket']
	  ifFalse: [^self expected: 'Left bracket']
! !
!LADParser methodsFor: 'Parser' stamp: ''!
subExpression | e |
	 "'(' expression ')' ⇒ expression"
	(self match: #leftParenthesis)
	  ifTrue:
		[(e ← self expression)
		  ifTrue:
			[(self match: #rightParenthesis)
			  ifTrue: [^e].
			^self expected: 'Right parenthesis']
		  ifFalse: [^self expected: 'Expression']]
	  ifFalse: [^false]
! !
!LADParser methodsFor: 'Parser' stamp: ''!
primaryExpression: cascading | |
	 "variable ⇒ VariableNode
		 constant ⇒ LiteralNode
		 '{' {expression ','} [expression] '}' ⇒ CollectionNode
		 ';' ⇒ ;  when allowed
		 - number ⇒ ¬number
		 	|  '(' expression ')'  |  block   "
	hereType ≡ #word
	  ifTrue: [^encoder encodeVariable: self advance].
	(hereType ≡ #string or: [(hereType ≡ #number or: [hereType ≡ #literal])])
	  ifTrue: [^encoder encodeLiteral: self advance].
	hereType ≡ #leftParenthesis
	  ifTrue: [^self subExpression].
	hereType ≡ #leftBracket
	  ifTrue: [^self block].
	(hereType ≡ #semicolon and: [cascading])
	  ifTrue: [^self advance].
	hereType ≡ #leftBrace
	  ifTrue: [^self collection].
	((here ≡ #- or: [here ≡ '¬' unique]) and: [tokenType ≡ #number])
	  ifTrue:
		[ "ST76"
		self advance.
		^encoder encodeLiteral: self advance negated].
	^false
! !
!LADParser methodsFor: 'Parser' stamp: ''!
binaryMessage | selector arg |
	 "binarySelector arg ⇒ {selector, {arg}}"
	selector ← self advance.
	(arg ← self argumentFreeExpression: false)
	  ifTrue: [^{selector unique , arg inVector}].
	^self expected: 'argument'
! !
!LADParser methodsFor: 'Parser' stamp: ''!
argumentFreeExpression: cascading | rcvr |
	 "primaryExpression {unarySelector} ⇒ MessageNode"
	(rcvr ← self primaryExpression: cascading)
	  ifTrue:
		[[hereType ≡ #word] whileTrueDo: [rcvr ← LADMessageNode new receiver: rcvr selector: self advance unique arguments: #() precedence: 1 from: encoder].
		^rcvr].
	^false
! !
!LADParser methodsFor: 'Private' stamp: ''!
init: sourceStream notifying: req | |
	requestor ← req.
	super scan: sourceStream.
	self advance
! !
!LADParser methodsFor: 'Public' stamp: ''!
parse: sourceStream class: class noPattern: noPattern context: ctxt notifying: req | m |
	self init: sourceStream notifying: req "noPattern is false for a compile".
	encoder ← LADEncoder new init: class context: ctxt notifying: self.
	m ← self method: noPattern context: ctxt.
	encoder ← nil "break cycle".
	hereType ≡ #doIt
	  ifTrue: [^m].
	self notify: 'Unexpected construct'
! !
!LADParser methodsFor: 'Scanner' stamp: ''!
match: type | |
	hereType ≡ type
	  ifTrue: [^self advance].
	^false
! !
!LADParser methodsFor: 'Scanner' stamp: ''!
advance | t |
	t ← here.
	here ← token.
	hereType ← tokenType.
	hereMark ← mark.
	hereType
	  ifTrue: [self scanToken].
	^t
! !
!LADParser methodsFor: 'Test' stamp: ''!
test: class | s |
	 "LADParser new test: Point."
	user waitnobug.
	#('noOp []' 'max: p2 [^(x>p2 x and: [y>p2 y]) ifTrue: [self] ifFalse: [p2]]' 'coord: p2 [[x>p2 x and: [y>p2 y]] whileTrue: [self past: p2]]' '& z | a b c [a←z. b←z*a max: z/a length/(c←a length+2 min: z)]' 'to: p2 by: d [^Interval new from: self to: p2 by: d]' 'incAll: coll [coll do: [:x. self inc: x with: ''string'']]' 'cascade: a [^a+3 to: 4; from: 5; length]' ) do:
		[:s | user cr.
		user show: (self parse: s class: class noPattern: false context: false notifying: user) simplify asString.
		user cr.
		user waitnobug.
		user waitbug].
	[(s ← user read) length = 0] whileFalseDo: 
		[user cr.
		user show: (self compile: s in: class) simplify asString.
		user cr]
! !
Object subclass: #LADScanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType'
	classVariableNames: 'typeTable'
	category: 'Compiler'!
!LADScanner methodsFor: 'As yet unclassified' stamp: ''!
xLetter | type s |  "form a word or keyword"
	s ← Stream default.
	[(type ← typeTable ◦ hereChar) ≡ #xLetter or: [type ≡ #xDigit]]
		whileTrueDo: [s next← self step].
	type ≡ #colon
	  ifTrue:
		[s next← self step.
		tokenType ← #keyword]
	  ifFalse: [tokenType ← #word].
	token ← s contents
! !
!LADScanner methodsFor: 'Errors' stamp: ''!
error: string | |
	self notify: string
! !
!LADScanner methodsFor: 'Errors' stamp: ''!
notify: string | |
	user notify: string
! !
!LADScanner methodsFor: 'Errors' stamp: ''!
interactive | |
	^false
! !
!LADScanner methodsFor: 'Initialization' stamp: ''!
scan: inputStringOrStream | |
	source ← inputStringOrStream asStream "initialize reader".
	mark ← source position.
	self step.
	self step.
	self scanToken
! !
!LADScanner methodsFor: 'Initialization' stamp: ''!
classInit | |  "LADScanner classInit."
	typeTable ← Vector new: 256.
	(typeTable ◦ (1 to: 256)) all← #xBinary. "default"
	(typeTable ◦ #(9 10 12 13 32)) all← #xDelimiter. "tab lf ff cr space"
	(typeTable ◦ #(15 35)) all← #xLitQuote.
	typeTable ◦ #(26 30 34 35 39) ←
		#(xControlZ doIt xDoubleQuote xLitQuote xSingleQuote).
	typeTable ◦ #(40 41 44 46) ←
		#(leftParenthesis rightParenthesis comma period).
	(typeTable ◦ (48 to: 57)) all← #xDigit.
	(typeTable ◦ #(3 58)) all← #colon.
	typeTable ◦ 59 ← #semicolon.
	(typeTable ◦ ((65 to: 90) concat: (97 to: 122))) all← #xLetter.
	(typeTable ◦ #( 63 126)) all← #xLetter. " ? ~ "
	typeTable ◦ #(91 93 94 95) ←
		#(leftBracket rightBracket upArrow leftArrow).
	typeTable ◦ #(123 124 125) ←
		#(leftBrace verticalBar rightBrace).
	Abbreviated← Dictionary init.
	"Abbreviated insertall: {'<=' unique, '/=' unique, '>=' unique, '==' unique}
		with: #(≤ ≠ ≥ ≡) ** wouldnt compile!"
	Abbreviated insert: '<=' unique with: #≤.
	Abbreviated insert: '/=' unique with: #≠.
	Abbreviated insert: '>=' unique with: #≥.
	Abbreviated insert: '==' unique with: #≡.
! !
!LADScanner methodsFor: 'Main Scanner' stamp: ''!
scanToken | |
	mark ← source position.
	tokenType ← typeTable ◦ hereChar.
	tokenType ◦ 1 = 120  "x as first letter"
	 ?: [self perform: tokenType]  "means perform to compute token & type"
	 ~: [token ← self step inUniqueString]  "else just unique the first char"
! !
!LADScanner methodsFor: 'Mulit-Character Scans' stamp: ''!
xDigit | fracPart exp real radix |
	 "form a number"
	tokenType ← #number.
	real ← false.
	radix ← self convert: (self scanInteger: 10) radix: 10.
	hereChar = 114
	  ifTrue: "<radix>r<number>"
		[self step.
		token ← self convert: (self scanInteger: radix) radix: radix]
	  ifFalse:
		[token ← radix.
		radix ← 10].
	(hereChar = 46 and: [(self testDigit: aheadChar radix: radix)])
	  ifTrue: ".<fraction>"
		[self step.
		real ← true.
		fracPart ← self scanInteger: radix.
		fracPart ← (self convert: fracPart radix: radix) asFloat /
				(radix asFloat ipow: fracPart length).
		token ← token asFloat + fracPart].
	hereChar = 101
	  ifTrue: "e<exponent>"
		[self step.
		exp ← (hereChar = 45 "-"
				  ifTrue:
					[self step.
					0 - (self convert: (self scanInteger: 10) radix: 10)]
				  ifFalse: [self convert: (self scanInteger: 10) radix: 10]).
		token ← token * (real
				  ifTrue: [radix asFloat ipow: exp]
				  ifFalse: [radix ipow: exp])]
! !
!LADScanner methodsFor: 'Mulit-Character Scans' stamp: ''!
xSingleQuote | start s |
	 "string"
	start ← source position.
	self step.
	s ← Stream default.
	[hereChar = 39 and: [(aheadChar = 39
	  ifTrue:
		[self step.
		false]
	  ifFalse: [true])]] whileFalseDo: 
		[s next← self step.
		(hereChar = 30 "doit" and: [source end])
		  ifTrue:
			[ "Ran off end, back up."
			source skip: start - 1 - source position.
			^self error: 'Unmatched string quote']].
	self step.
	token ← s contents.
	tokenType ← #string
! !
!LADScanner methodsFor: 'Mulit-Character Scans' stamp: ''!
xDoubleQuote | |
	 "ignore comment"
	(self skipThrough: hereChar)
	  ifTrue: [self scanToken]
	  ifFalse: [self error: 'Unmatched comment quote']
! !
!LADScanner methodsFor: 'Mulit-Character Scans' stamp: ''!
xControlZ | |  "ignore trailer"
	(self skipThrough: 13)
	  ifTrue: [self scanToken]
	  ifFalse: [self error: 'Missing cr in Bravo trailer']
! !
!LADScanner methodsFor: 'Mulit-Character Scans' stamp: ''!
xDelimiter | |
	 "ignore blanks etc."
	[typeTable ◦ hereChar ≡ #xDelimiter] whileTrueDo: [self step].
	self scanToken
! !
!LADScanner methodsFor: 'Multi-Character Scans' stamp: ''!
xLitQuote | t | "UniqueStrings and Vectors"  " #(1 (4 5) 2 3) #ifTrue:ifFalse: "
	self step "litQuote".
	self scanToken.
	tokenType = #leftParenthesis
	   ifTrue: [self scanLitVec]
	   ifFalse: [(#(word keyword colon) has: tokenType)
				ifTrue: [self scanLitWord]].
	tokenType ← #literal
! !
!LADScanner methodsFor: 'Multi-Character Scans' stamp: ''!
xBinary | | 
	tokenType ← #binary.
	token ← self step inUniqueString.
	typeTable◦hereChar = #xBinary  "double binaries"
	 ?: [token ← (token concat: self step inString) unique.
		"check for <=  /=  >=  == "
		(Abbreviated has: token) ?: [token← Abbreviated◦token]]
! !
!LADScanner methodsFor: 'Subroutines' stamp: ''!
skipThrough: char | start |
	 "⇑false if unmatched."
	start ← source position.
	self step.
	[self step = char] whileFalseDo: 
		[(hereChar = 30 and: [source end "doit"])
		  ifTrue:
			[source skip: start - 1 "Ran off end, back up." - source position.
			^false]].
	^true
! !
!LADScanner methodsFor: 'Subroutines' stamp: ''!
scanLitVec | s |  " #(1 (4 8r31 6)2 (a ?: b) 3) "
	s← (Vector new: 16) asStream.
	[self scanToken.  tokenType = #rightParenthesis] whileFalseDo:
		[tokenType = #leftParenthesis ifTrue: [self scanLitVec].
		(tokenType=#word or: [tokenType=#keyword])
			ifTrue: [self scanLitWord].
		s next← token].
	token ← s contents
! !
!LADScanner methodsFor: 'Subroutines' stamp: ''!
scanLitWord | t | "accumulate keywords and unique the result"
	[typeTable◦hereChar = #xLetter]
		whileTrueDo: [t← token. self xLetter. token← t concat: token].
	token← token unique
! !
!LADScanner methodsFor: 'Subroutines' stamp: ''!
testDigit: char radix: radix | |
	^(char ≥ 48 and: [char < (48 + radix) "0-9"]) or: [(radix > 10 and: [(char ≥ 65 and: [char < (65 - 10 + radix) "A-Z"])])]
! !
!LADScanner methodsFor: 'Subroutines' stamp: ''!
scanInteger: radix | s |
	 "Should be merged with Integer readFrom: "
	s ← Stream default.
	[self testDigit: hereChar radix: radix] whileTrueDo: [s next← self step].
	^s contents
! !
!LADScanner methodsFor: 'Subroutines' stamp: ''!
convert: str radix: radix | |
	^Integer new readFrom: str asStream radix: radix
! !
!LADScanner methodsFor: 'Subroutines' stamp: ''!
step | c |
	c ← hereChar.
	hereChar ← aheadChar.
	(aheadChar ← source next)
	  ifTrue:
		[aheadChar = 0
		  ifTrue: [aheadChar ← 256]]
	  ifFalse: [aheadChar ← 30 "doit"].
	^c
! !
LADLeafNode subclass: #LADSelectorNode
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Compiler'!
!LADSelectorNode methodsFor: 'Code Generation' stamp: ''!
emit: stack args: nArgs on: strm | |
	self emit: stack args: nArgs on: strm super: false
! !
!LADSelectorNode methodsFor: 'Code Generation' stamp: ''!
size: encoder args: nArgs super: supered | index |
	self reserve: encoder.
	(supered ≡ false and: [(code - Send < SendLimit and: [nArgs < 3])])
	  ifTrue: [^1].
	 "short send"
	(supered and: [code < Send])
	  ifTrue: [ "super special:"
		code ← self code: (encoder litIndex: key) type: 5].
	index ← (code < 256
			  ifTrue: [code - Send]
			  ifFalse: [code \ 256]).
	(index < 32 and: [nArgs ≤ 7])
	  ifTrue: [^2].
	 "medium send"
	^3 "long send"
! !
!LADSelectorNode methodsFor: 'Code Generation' stamp: ''!
emit: stack args: nArgs on: strm super: supered | index |
	stack pop: nArgs.
	(supered ≡ false and: [(code - Send < SendLimit and: [nArgs < 3])])
	  ifTrue: [ "short send"
		strm next← (code < Send
		  ifTrue: [code]
		  ifFalse: [ "special"
			nArgs * 16 + code])]
	  ifFalse:
		[index ← (code < 256
				  ifTrue: [code - Send]
				  ifFalse: [code \ 256]).
		(index < 32 and: [nArgs ≤ 7])
		  ifTrue:
			[ "medium send"
			strm next← SendLong + (supered
			  ifTrue: [2]
			  ifFalse: [0]).
			strm next← nArgs * 32 + index]
		  ifFalse:
			[ "long send"
			.
			strm next← SendLong + 1 + (supered
			  ifTrue: [2]
			  ifFalse: [0]).
			strm next← nArgs.
			strm next← index]]
! !
!LADSelectorNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | |
	strm append: key
! !
LADLeafNode subclass: #LADVariableNode
	instanceVariableNames: 'name'
	classVariableNames: ''
	category: 'Compiler'!
!LADVariableNode methodsFor: 'Code Generation' stamp: ''!
sizeForStorePop: encoder | |
	self reserve: encoder.
	(code < 24 and: [(code nomask: 8)])
	  ifTrue: [^1].
	^2
! !
!LADVariableNode methodsFor: 'Code Generation' stamp: ''!
sizeForReturn: encoder | |
	(code ≥ LdSelf and: [code ≤ LdNil])
	  ifTrue: [ "short returns"
		^1].
	^super sizeForReturn: encoder
! !
!LADVariableNode methodsFor: 'Code Generation' stamp: ''!
emitForValue: stack on: strm | |
	code < 256
	  ifTrue:
		[strm next← (code = LdSuper
		  ifTrue: [LdSelf]
		  ifFalse: [code]).
		stack push: 1]
	  ifFalse:
		[self emitLong: LdInstLong on: strm.
		stack push: 1]
! !
!LADVariableNode methodsFor: 'Code Generation' stamp: ''!
emitForReturn: stack on: strm | |
	(code ≥ LdSelf and: [code ≤ LdNil])
	  ifTrue:
		[ "short returns"
		strm next← EndMethod - 4 + (code - LdSelf).
		stack push: 1 "doesnt seem right"]
	  ifFalse: [super emitForReturn: stack on: strm]
! !
!LADVariableNode methodsFor: 'Code Generation' stamp: ''!
emitStorePop: stack on: strm | |
	(code between: 0 and: [7])
	  ifTrue: [strm next← ShortStoP + code "short stopop inst"]
	  ifFalse:
		[(code between: 16 and: [23])
		  ifTrue: [strm next← ShortStoP + 8 + code - 16 "short stopop temp"]
		  ifFalse: [self emitLong: StorePop on: strm]].
	stack pop: 1
! !
!LADVariableNode methodsFor: 'Code Generation' stamp: ''!
sizeForStore: encoder | |
	self reserve: encoder.
	^2
! !
!LADVariableNode methodsFor: 'Code Generation' stamp: ''!
emitStore: stack on: strm | |
	self emitLong: Store on: strm
! !
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
name: string key: object code: byte | |
	name ← string.
	key ← object.
	code ← byte
! !
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
classInit2: encoder | |
	 "LADVariableNode classInit."
	NodeNil ← encoder encodeVariable: 'nil'.
	NodeTrue ← encoder encodeVariable: 'true'.
	NodeFalse ← encoder encodeVariable: 'false'.
	NodeSelf ← encoder encodeVariable: 'self'.
	NodeThisContext ← encoder encodeVariable: 'thisContext'.
	NodeSuper ← encoder encodeVariable: 'super'.
	NodeNullBlock ← NodeNil inVector
! !
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
name: varName key: objRef index: i type: type | |
	name ← varName.
	self key: objRef index: i type: type
! !
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
name: varName index: i type: type | |
	self name: varName key: varName index: i type: type
! !
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
classInit | encoder specials |  "LADVariableNode classInit."
	encoder ← LADEncoder new.
	stdVariables ← Dictionary new init: 16.
	encoder fillDict: stdVariables with: LADVariableNode mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' ) to: {LdSelf , LdThisContext , LdSuper , LdNil , LdFalse , LdTrue}.
	stdSelectors ← HashSet new init: 64.
	specials ← SpecialOops copy: 10 to: SpecialOops length.
	encoder fillSet: stdSelectors with: LADSelectorNode mapping: specials to: (SendPlus to: SendPlus + 31).
	stdLiterals ← HashSet new init: 16.
	encoder fillSet: stdLiterals with: LADLiteralNode mapping: #(¬1 0 1 2 ) to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.
	self classInit2: encoder.
	initted ← true
! !
!LADVariableNode methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | |
	strm append: name
! !
!LADVariableNode methodsFor: 'Simplifying' stamp: ''!
canBeSpecialArgument | |
	 "can I be an argument of (e.g.) ifTrue:?"
	^code < LdNil
! !
Object subclass: #ParseStack
	instanceVariableNames: 'position length'
	classVariableNames: ''
	category: 'Compiler'!
!ParseStack methodsFor: 'Changes' stamp: ''!
push: n | |
	(position ← position + n) > length
	  ifTrue: [length ← position]
! !
!ParseStack methodsFor: 'Changes' stamp: ''!
pop: n | |
	(position ← position - n) < 0
	  ifTrue: [user notify: 'Parse stack underflow']
! !
!ParseStack methodsFor: 'Initialization' stamp: ''!
init | |
	length ← position ← 0
! !
!ParseStack methodsFor: 'Results' stamp: ''!
position | |
	^position
! !
!ParseStack methodsFor: 'Results' stamp: ''!
length | |
	^length
! !
