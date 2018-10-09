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
	key printon: strm
! !
!LADLiteralNode methodsFor: 'Printing' stamp: ''!
printOn: strm indent: level | |
	 "wouldn't handle UniqueString literals right"
	((key is: UniqueString) or: [key is: Vector])
	  ifTrue: [strm append: '#'].
	key printOn: strm
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
sizeForStorePop: encoder | |
	self reserve: encoder.
	(code < 24 and: [(code nomask: 8)])
	  ifTrue: [^1].
	^2
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
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
name: varName index: i type: type | |
	self name: varName key: varName index: i type: type
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
classInit | encoder specials |
	 "LADVariableNode classInit."
	encoder ← LADEncoder new.
	stdVariables ← Dictionary new init: 16.
	encoder fillDict: stdVariables with: LADVariableNode mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' ) to: {LdSelf , LdThisContext , LdSuper , LdNil , LdFalse , LdTrue}.
	stdSelectors ← HashSet new init: 64.
	specials ← (SpecialOops ◦ (10 to: SpecialOops length)) copy.
	specials ◦ (19 ~ 23) ← {#length , #next , 'next←' unique , #end , #≡}.
	encoder fillSet: stdSelectors with: LADSelectorNode mapping: specials to: (SendPlus to: SendPlus + 31).
	stdLiterals ← HashSet new init: 16.
	encoder fillSet: stdLiterals with: LADLiteralNode mapping: #(¬1 0 1 2 ) to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.
	self classInit2: encoder.
	initted ← NoteTaker
! !
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
name: string key: object code: byte | |
	name ← string.
	key ← object.
	code ← byte
! !
!LADVariableNode methodsFor: 'Initialization' stamp: ''!
name: varName key: objRef index: i type: type | |
	name ← varName.
	self key: objRef index: i type: type
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
