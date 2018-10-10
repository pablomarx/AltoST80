Object subclass: #Class
	instanceVariableNames: 'title	"<String> for identification, printing"
				myinstvars "<String> partnames for compiling, printing"
				instsize "<Integer> for storage management"
				messagedict "<MessageDict> for communication, compiling"
				classvars "<Dictionary/nil> compiler checks here"
				superclass "<Class> for execution of inherited behavior"
				environment "<Vector of SymbolTables> for external refs"
				fieldtype'
	classVariableNames: 'lastParagraph lastInstvarClass'
	category: 'Kernel Classes'!
!Class methodsFor: 'Access to parts' stamp: ''!
instsize | | "Return the number of user accessable instance fields"
	^instsize land: 4095
! !
!Class methodsFor: 'Access to parts' stamp: ''!
allSubclasses | strm cl |
	strm ← (Vector new: 5) asStream.
	Class allInstancesDo:
		[:cl | cl superclass ≡ self ifTrue: [strm next ← cl]].
	^strm contents
! !
!Class methodsFor: 'Access to parts' stamp: ''!
isVariable | |
	^instsize allmask: 8r10000
! !
!Class methodsFor: 'Access to parts' stamp: ''!
◦ x | |
	^classvars ◦ x
! !
!Class methodsFor: 'Access to parts' stamp: ''!
md | |
	^messagedict
! !
!Class methodsFor: 'Access to parts' stamp: ''!
◦ x ← val | |
	^classvars ◦ x ← val
! !
!Class methodsFor: 'Access to parts' stamp: ''!
myinstvars | |
	^myinstvars
! !
!Class methodsFor: 'Access to parts' stamp: ''!
isBytes | |
	^instsize nomask: 8r20000
! !
!Class methodsFor: 'Access to parts' stamp: ''!
invertRef: refs | cl env source ref inv sym t |
	 "Refs may be a vector (to allow batching)"
	(refs isnt: Vector)
	  ifTrue: [^(self invert: refs inVector) ◦ 1].
	env ← (self wholeEnvironment concat: {Undeclared , Smalltalk}) asStream.
	source ← Dictionary init.
	^refs transform⦂ [:ref | ref] to⦂ [
		(cl ← self.
		env reset.
		[(sym ← env next) ≡ false
		  ifTrue: [inv ← 'unknown ' concat: ref asOop base8]
		  ifFalse:
			[(cl ≠ nil and: [sym ≡ cl classvars])
			  ifTrue:
				[t ← cl title.
				cl ← cl superclass]
			  ifFalse: [t ← false].
			(inv ← sym invertRef: ref) ≡ false
			  ifTrue: [false]
			  ifFalse:
				[t
				  ifFalse:
					[(t ← source lookup: sym)
					  ifFalse: [source insert: sym with: (t ← Smalltalk invert: sym)]].
				inv ← (t concat: ' ') concat: inv]]] whileFalseDo:  [].
		inv)]
! !
!Class methodsFor: 'Access to parts' stamp: ''!
instspec | | "Return the number of user accessable instance fields"
	^instsize
! !
!Class methodsFor: 'Access to parts' stamp: ''!
Isa: x | |
	 "is x on my superclass chain?"
	superclass ≡ x
	  ifTrue: [^true].
	superclass ≡ nil
	  ifTrue: [^false].
	^superclass Isa: x
! !
!Class methodsFor: 'Access to parts' stamp: ''!
fieldNamesInto: collector | |
	superclass ≡ nil
	  ifFalse: [superclass fieldNamesInto: collector].
	^(Reader new of: myinstvars) readInto: collector
! !
!Class methodsFor: 'Access to parts' stamp: ''!
superclass | |
	^superclass
! !
!Class methodsFor: 'Access to parts' stamp: ''!
title | |
	^title
! !
!Class methodsFor: 'Access to parts' stamp: ''!
isBits | |
	^instsize nomask: 8r40000
! !
!Class methodsFor: 'Access to parts' stamp: ''!
instvars | |
	self ≡ lastInstvarClass
	  ifTrue: [^lastInstvars copy].
	 "cache last computation of instvars"
	lastInstvarClass ← self.
	lastInstvars ← self fieldNamesInto: FieldNameCollector default.
	^lastInstvars copy
! !
!Class methodsFor: 'As yet unclassified' stamp: ''!
recompile: selector | |
	self understands: (self code: selector) asParagraph
! !
!Class methodsFor: 'As yet unclassified' stamp: ''!
usesNewSyntax | |
	self ≡ LADObject
	  ifTrue: [^true].
	superclass ≡ nil
	  ifTrue: [^false].
	^superclass usesNewSyntax
! !
!Class methodsFor: 'As yet unclassified' stamp: ''!
regenerate: sel | old new tree |
	sel ≡ nil
	  ifTrue: [messagedict do: [:sel | user displayoffwhile⦂ [(self regenerate: sel)]]]
	  ifFalse:
		[ "user show: title; space; show: sel; cr."
		.
		tree ← self decompile: sel.
		new ← tree generate.
		old ← self method: sel "user print: old length; space; print: new length; cr.".
		^new]
! !
!Class methodsFor: 'Editing' stamp: ''!
edit: selector para: para formerly: oldpara | |
	NoteTaker
	  ifTrue: [user schedule: (CodeWindow new class: self selector: selector para: para formerly: oldpara)]
	  ifFalse:
		[user leaveTop.
		user restartup: (CodeWindow new class: self selector: selector para: para formerly: oldpara)]
! !
!Class methodsFor: 'Editing' stamp: ''!
ed: selector | c s |
	c ← self code: selector.
	user clearshow: c.
	[s ← user request: 'substitute: '] whileTrueDo:
		[c ← c subst: s for: (user request: 'for: ').
		user clearshow: c].
	self understands: c
! !
!Class methodsFor: 'Editing' stamp: ''!
edit: selector | para s v |
	para ← (selector = #ClassOrganization
			  ifTrue: [self organization asParagraph]
			  ifFalse:
				[(messagedict has: selector)
				  ifTrue: [self code: selector]
				  ifFalse: [nullString asParagraph]]).
	self edit: selector para: para formerly: false
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
printdefon: strm | s |
	 "print my definition on strm"
	strm append: self class title+' new title: #'+title.
	strm cr; tab.
	strm append: 'subclassof: ' + (superclass ≡ nil
	  ifTrue: ['nil']
	  ifFalse: [superclass title]).
	strm cr; tab.
	strm append: 'fields: ' + myinstvars asString.
	strm cr; tab.
	strm append: 'declare: '''.
	classvars contents do:
		[:s | s = #ClassOrganization
		  ifFalse:
			[strm append: s.
			strm space]].
	strm append: ''''.
	(instsize anymask: 4096)
		  ifTrue:
			[strm semicrtab.
			strm append: 'bytesize: '.
			strm print: ((instsize anymask: 8192)
			  ifTrue: [(instsize anymask: ¬16384) ifFalse: [16]]
			  ifFalse: [8])].
	environment ≡ nil
	  ifFalse: [environment do:
			[:s | strm semicrtab.
			strm append: 'sharing: ' + (Smalltalk invert: s)]]
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
paraprinton: strm | para frame s heading org |
	 "Strm is actually a ParagraphPrinter"
	para ← ('"' + title + '"') asParagraph.
	para maskrunsunder: 241 to: 81 "Font ← 5, Bold".
	frame ← strm defaultframe "defeat ST76 optimization".
	strm frame← 15000 ⌾ frame origin y rect: 20000 ⌾ frame corner y.
	strm print: para.
	strm frame← frame.
	strm print: ((self definition + ';
	asFollows') asParagraph maskrunsunder: 241 to: 81).
	org ← self organization.
	strm print: ('
' + org globalComment) asParagraph allItalic.
	org categories do: [:heading | self printCategory: heading on: strm].
	self endChangesOn: strm.
	strm print: ('SystemOrganization classify: #' + title + ' under: ''' + (SystemOrganization invert: title unique) + '''.') asParagraph.
	self ≡ Class
	  ifFalse:
		[(self canunderstand: #classInit)
		  ifTrue: [strm print: (title + ' classInit') asParagraph]]
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
filout | |
	user displayoffwhile⦂ [
		((dp0 file: title + '.st.') filoutclass: self.
		self noChanges)]
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
printoutCategory: cat | |
	(dp0 file: (title + '-' + cat + '.press') asFileName) printout: (self changelist: cat)
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
startCategory: s on: pstrm | |
	pstrm print: (('
' + s) asParagraph maskrunsunder: 241 to: 81) "Font 5, Bold"
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
printCategory: s on: pstrm | sel |
	self startCategory: s on: pstrm.
	(self organization category: s) do: [:sel | self printMethod: sel on: pstrm].
	self endCategoryOn: pstrm
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
filoutOrganization | t1 |
	 "So we can merge separate work on organization"
	user show: title.
	user cr.
	user displayoffwhile⦂ [
		((t1 ← dp0 file: title + '.org.') append: title + ' organization fromParagraph:'.
		t1 cr.
		t1 append: self organization asParagraph text asString.
		t1 append: 'asParagraph'.
		t1 close)]
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
changelist: cat | |
	^{title unique , (self organization category: cat)}
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
definition | strm |
	 "return a string that defines me (Class new title etc.)"
	strm ← (String new: 50) asStream.
	self printdefon: strm.
	^strm contents
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
readfrom: strm format: f | |
	^self new readfrom: strm format: f
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
asFollows | s heading selector p t5 |
	self ≠ self realself
	  ifTrue: [self realself asFollows]
	  ifFalse:
		[heading ← 'As yet unclassified' "handles Bravo or Press (Smalltalk generated) files".
		[(p ← FilinSource nextParagraph) and: [(s ← p text) ≠ '']] whileTrueDo:
			[s ◦ 1 = 13
			  ifTrue: [s ← s copy: 2 to: s length "throw away initial cr before comment and headings"].
			(t5 ← p runs ◦ 2) = 2 "italic"
			  ifTrue: [self organization globalComment← s]
			  ifFalse:
				[t5 = 81 "5, bold"
				  ifTrue: [heading ← s]
				  ifFalse:
					[(self canunderstand: (selector ← self understands: p classified: heading))
					  ifTrue:
						[user show: selector.
						user space]
					  ifFalse: [user show: '(an uncompiled method) ']]]]]
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
readfrom: strm | |
	^self readfrom: strm format: nil
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
printout | |
	user displayoffwhile⦂ [((dp0 file: title + '.press.') printoutclass: self)]
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
filoutCategory: cat | |
	(dp0 file: (title + '-' + cat + '.st') asFileName) filout: (self changelist: cat)
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
noChanges | s t |
	t ← title + ' *'.
	Changes contents do:
		[:s | ((s ◦ 1 = 126 "~" and: [(t match: s ◦ (2 to: s length))]) or: [(t match: s)])
		  ifTrue: [Changes delete: s]]
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
startChangesOn: pstrm | |
	pstrm print: (('
' + title + ' asFollows') asParagraph maskrunsunder: 241 to: 81) "Font 5, Bold"
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
printMethod: sel on: pstrm | |
	pstrm print: (self code: sel)
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
endChangesOn: pstrm | |
	pstrm print: '' asParagraph
! !
!Class methodsFor: 'Filin and Filout' stamp: ''!
endCategoryOn: pstrm | |
	
! !
!Class methodsFor: 'Initialization' stamp: ''!
rename: newtitle | name newname oldclass category oldref |
	name ← title unique.
	newname ← newtitle unique.
	(Smalltalk has: newname)
	  ifTrue:
		[oldclass ← Smalltalk ◦ newname.
		user notify: 'All ' + newtitle + 's will become obsolete if you proceed'.
		oldclass obsolete.
		Smalltalk delete: newname]
	  ifFalse:
		[category ← SystemOrganization invert: name.
		AllClassNames ← AllClassNames insertSorted: newname.
		SystemOrganization classify: newname under: category].
	oldref ← Smalltalk ref: name.
	Smalltalk delete: name.
	AllClassNames ← AllClassNames delete: name.
	SystemOrganization delete: name.
	title ← newtitle.
	(Undeclared has: newname)
		ifTrue: [Smalltalk declare: newname as: self]
		ifFalse: [Smalltalk insert: newname withref: oldref]  "Preserve old ref"
! !
!Class methodsFor: 'Initialization' stamp: ''!
title: t | | 
	self title: (title ← t unique) insystem: Smalltalk
! !
!Class methodsFor: 'Initialization' stamp: ''!
newFieldsForSubClass: t1 | r a b |
	 "list of instance variables" "changing inst fields"
	myinstvars ← t1.
	messagedict ← MethodDictionary init: 4.
	r ← self realself.
	self = r
	  ifTrue: [user notify: 'problem in class redefinition. See coment at end of method']
	  ifFalse:
		[(a ← self instvars) = (b ← r instvars)
		  ifTrue: [user notify: 'problem in class redefinition. See coment at end of method']
		  ifFalse:
			[r someInstance
			  ifTrue: [user cr show: 'All ' + title + 's are obsolete.'].
			classvars ← r classvars.
			messagedict ← r md copy.
			r md init.
			(a length ≤ b length or: [a ◦ (1 to: b length) ≠ b])
			  ifTrue:
				[user cr show: title + ' recompiled.'.
				self compileall].
			self fixSubClassesOf: r.
			r obsolete.
			Smalltalk ◦ title unique ← self.
			self initClass]]
"Regarding the notifys in this method: It is my understanding
		 that this method will only be invoked when the conditions
		 leading to the notifys are false. If I'm available, I'd like to see
		 any case that results in notification.
				Dave Robson"
! !
!Class methodsFor: 'Initialization' stamp: ''!
fields: t1 | r a b s h |
	 "list of instance variables" "just adding new inst fields"
	myinstvars ← t1.
	messagedict ← MethodDictionary init: 4.
	r ← self realself.
	a ← self instvars.
	h ← HashSet init.
	a do:
		[:s | (h has: s)
		  ifTrue: [user notify: s + ' is used already (maybe in superclass)']
		  ifFalse: [h insert: s]].
	self = r
	  ifTrue: [self initClass]
	  ifFalse:
		[a = (b ← r instvars)
		  ifTrue:
			[r environment← nil.
			r myinstvars← myinstvars.
			r subclassof: superclass]
		  ifFalse:
			[r someInstance
			  ifTrue: [user notify: 'All ' + title + 's become obsolete if you proceed...'].
			classvars ← r classvars.
			messagedict ← r md copy.
			(a length ≤ b length or: [a ◦ (1 to: b length) ≠ b])
			  ifTrue:
				[user notify: title + ' methods recompile if you proceed...'.
				self compileall].
			r md init.
			self fixSubClassesOf: r.
			r obsolete.
			Smalltalk ◦ title unique ← self.
			self initClass]]
! !
!Class methodsFor: 'Initialization' stamp: ''!
obsolete | |
	title ← 'AnObsolete' + title.
	classvars ← nil. "recycle class variables"
	messagedict ← MethodDictionary init: 2. "invalidate and recycle local messages"
	self someInstance ifTrue: [environment ← self] "keep me around for old instances"
! !
!Class methodsFor: 'Initialization' stamp: ''!
bytesize: n | | "non-pointer declaration"
	self ≠ self realself
	  ifTrue: [self realself bytesize: n]
	  ifFalse:
		[instsize ← n≡true
			ifTrue: [¬4096]
			ifFalse: [n = 8 ifTrue: [4096] ifFalse: [12288]]]
! !
!Class methodsFor: 'Initialization' stamp: ''!
classInit | | "gets propagated to a dummy instance"
	^self new classInit
! !
!Class methodsFor: 'Initialization' stamp: ''!
title: t subclassof: s fields: f declare: d | |
	t ◦ 1 ≠ (t ◦ 1) asUppercase
	  ifTrue:
		[user notify: 'Please capitalize each word in class title: ' + t.
		^false].
	self title: t.
	self subclassof: s.
	self fields: f.
	self declare: d
! !
!Class methodsFor: 'Initialization' stamp: ''!
fixSubClassesOf: oldClass | n subClass |
	user classNames do:
		[:n | subClass ← Smalltalk ◦ n.
		subClass superclass ≡ oldClass
		  ifTrue: [Class new copyof: subClass subclassof: self]]
! !
!Class methodsFor: 'Initialization' stamp: ''!
copyof: oldClass subclassof: newSubClass | |
	title ← oldClass title.
	self subclassof: newSubClass.
	classvars ← oldClass classvars.
	environment ← oldClass environment.
	self newFieldsForSubClass: oldClass myinstvars
! !
!Class methodsFor: 'Initialization' stamp: ''!
realself | |
	^Smalltalk ◦ title unique "as opposed to possible filin ghost"
! !
!Class methodsFor: 'Initialization' stamp: ''!
abstract | |
	self fields: nullString
! !
!Class methodsFor: 'Initialization' stamp: ''!
subclassof: cl | |
	(cl is: Class)
		ifTrue: [superclass ← cl]
		ifFalse: [user notify: 'Superclass is not yet defined or not a Class']
! !
!Class methodsFor: 'Initialization' stamp: ''!
sharing: table | |
	self ≠ self realself
	  ifTrue: [self realself sharing: table]
	  ifFalse: [environment ← {environment asVector , table}]
! !
!Class methodsFor: 'Initialization' stamp: ''!
environment← t1 | |
	environment ← t1 "for resetting to reread sharing clauses"
! !
!Class methodsFor: 'Initialization' stamp: ''!
myinstvars← t1 | |
	myinstvars ← t1
! !
!Class methodsFor: 'Initialization' stamp: ''!
title: name insystem: system | cl |
	superclass ← Object.
	(system has: name)
	  ifTrue:
		[cl ← (system ◦ name) class.
		cl ≡ self class
		  ifTrue: [^self].
		user notify: name + ' will change from a ' + cl title + ' to a ' + self class title + ' if you proceed...'].
	system declare: name as: self.
	AllClassNames ← AllClassNames insertSorted: name.
	SystemOrganization classify: name under: 'As yet unclassified'
! !
!Class methodsFor: 'Initialization' stamp: ''!
declare: v | var recom |
	self ≠ self realself
	  ifTrue: [self realself declare: v]
	  ifFalse:
		[classvars ≡ nil
		  ifTrue: [classvars ← SymbolTable init].
		(v is: String)
		  ifTrue: [self declare: v asVector]
		  ifFalse:
			[recom ← false.
			(v is: Vector)
			  ifTrue: [v do:
					[:var | ((Smalltalk has: var) or: [(Undeclared has: var)])
					  ifTrue: [recom ← true]]]
			  ifFalse:
				[((Smalltalk has: v) or: [(Undeclared has: v)])
				  ifTrue: [recom ← true]].
			recom
			  ifTrue: [user notify: 'Methods recompile if you proceed, global became local'].
			(v is: Vector)
			  ifTrue: [v do: [:var | classvars insert: var with: nil]]
			  ifFalse: [classvars insert: v with: nil].
			recom
			  ifTrue: [self compileall]]]
! !
!Class methodsFor: 'Initialization' stamp: ''!
veryspecial: n | |
	 "inaccessible fields"
	instsize ← self instvars length + n
! !
!Class methodsFor: 'Initialization' stamp: ''!
initClass | |
	(Vector new: 128) all← self.  "CROCK to stick refct of classes"
	fieldtype ← 16.
	instsize ← self instvars length.
	instsize > 256
	  ifTrue: [user notify: 'too many instance variables']
	  ifFalse:
		[instsize ← instsize + ¬8192.
		self organization]
! !
!Class methodsFor: 'Instance access' stamp: ''!
allInstances | strm x |
	strm← (Vector new: 16) asStream.
	self allInstancesDo: [:x | strm next← x].
	^strm contents
! !
!Class methodsFor: 'Instance access' stamp: ''!
allInstancesDo: aBlock | inst |
	inst← self someInstance.
	inst ifTrue:
		[aBlock value: inst.
		[inst← inst nextInstance]
			whileTrueDo: [aBlock value: inst].
		false class≡self ifTrue: [aBlock value: false]]
! !
!Class methodsFor: 'Instance access' stamp: ''!
recopy: inst | copy i var |
	var← self isVariable
	copy ← var ifTrue: [self new: inst length] ifFalse: [self new].
	(1 to: self instsize) do: [:i | copy instfield: i ← (inst instfield: i) recopy].
	var ifTrue: [(1 to: inst length) do: [:i | copy◦i ← (inst◦i) recopy]].
	^copy
! !
!Class methodsFor: 'Instance access' stamp: ''!
print: inst on: strm | ivars i |
	ivars ← self instvars.
	strm append: '(('+title+' new'.
	self isVariable ifTrue:
		[strm append: ': '; print: inst length].
	strm append: ') '.
	(1 to: instsize) do:
		[:i | strm append: ivars ◦ i.
		strm append: ': '.
		strm print: (inst instfield: i).
		strm space].
	self isVariable ifTrue:
		[(1 to: inst length) do:
			[:i |
			strm append: ' at: '; print: i.
			strm append: ' put: '; print: inst◦i; append: ';'].
		strm append: 'itself'].
	strm append: ')'
! !
!Class methodsFor: 'Instance access' stamp: ''!
copy: inst | copy i var |
	var← self isVariable.
	copy ← var ifTrue: [self new: inst length] ifFalse: [self new].
	(1 to: self instsize) do: [:i | copy instfield: i ← inst instfield: i].
	var ifTrue: [(1 to: inst length) do: [:i | copy◦i ← inst◦i]].
	^copy
! !
!Class methodsFor: 'Instance access' stamp: ''!
new: length | | <primitive: 30>
	self isVariable
	  ifTrue: [user croak]
	  ifFalse: [^self new init: length] "ST76 convention"
! !
!Class methodsFor: 'Instance access' stamp: ''!
someInstance | |<primitive: 84>
	 "return first instance of this class, false if there are none"
	user croak
! !
!Class methodsFor: 'Instance access' stamp: ''!
allInstances⦂ each do⦂ expr | inst |
	inst ← self someInstance.
	inst ≡ false
	  ifFalse:
		[each value← inst.
		expr eval.
		[inst ← inst nextInstance] whileTrueDo:
			[each value← inst.
			expr eval].
		each value← false]
! !
!Class methodsFor: 'Instance access' stamp: ''!
new | |<primitive: 29>
	self isVariable ifTrue: [^self new: 0].
	user croak
! !
!Class methodsFor: 'Instance access' stamp: ''!
init: n | |
	 "init and default get propagated to instances"
	self isVariable
	  ifTrue: [^(self new: n) init].
	^self new init: n
! !
!Class methodsFor: 'Instance access' stamp: ''!
howMany | x n | "how many instances of this class are in use now?"
	n ← 0.
	self allInstancesDo: [:x | n ← n + 1].
	^n
! !
!Class methodsFor: 'Instance access' stamp: ''!
instfield: i | |
	 "prevent user from getting freelist"
	i > Class instsize
	  ifTrue: [user notify: 'arg too big']
	  ifFalse: [^super instfield: i]
! !
!Class methodsFor: 'Instance access' stamp: ''!
init | |
	 "init and default get propagated to instances"
	^self new init
! !
!Class methodsFor: 'Instance access' stamp: ''!
default | |
	^self new default
! !
!Class methodsFor: 'Instance access' stamp: ''!
printOn: strm | |
	strm append: 'Class '.
	strm append: title
! !
!Class methodsFor: 'Message access' stamp: ''!
whosends: selector | senders lit sel |
	senders ← Stream default.
	messagedict do:
		[:sel |
		(messagedict method: sel) literals do:
			[:lit |
			selector ≡ lit ifTrue: [senders append: sel; space]]].
	^senders contents
! !
!Class methodsFor: 'Message access' stamp: ''!
compileall | sel | "does not modify code, just compiles it"
	messagedict do: [:sel | self recompile: sel]

"to recompile the whole system (check out big changes) execute:
	user classNames do:
		[:name |
		user show: name; cr.
		(Smalltalk◦name) compileall.
		Changes init]. "
! !
!Class methodsFor: 'Message access' stamp: ''!
install: method selector: selector under: category source: code | c |
	self organization classify: selector under: category.
	messagedict add: method.
	lastClass ← self.
	lastSelector ← selector.
	lastParagraph ← code.
	Changes insert: (c ← title + ' ' + selector).
	(Changes has: (c ← '~' + c))
	  ifTrue: [Changes delete: c]
! !
!Class methodsFor: 'Message access' stamp: ''!
understands: code classified: heading | | "compile and install method"
	^LADCompiler new compile: code asParagraph in: self under: heading notifying: self
! !
!Class methodsFor: 'Message access' stamp: ''!
code: sel | meth |
	 "last paragraph returned is cached (mainly for NotifyWindows)"
	lastParagraph ← ((sel ≡ lastSelector and: [self ≡ lastClass])
			  ifTrue: [lastParagraph]
			  ifFalse:
				[sel = #ClassOrganization
				  ifTrue: [self organization]
				  ifFalse: [(meth ← messagedict methodorfalse: sel)
					ifTrue: [meth getSource: self]
					ifFalse: ['method not found!']]]) asParagraph.
	lastClass ← self.
	lastSelector ← sel.
	^lastParagraph
! !
!Class methodsFor: 'Message access' stamp: ''!
textLocal | s |
	s ← self organization "makes comment and methods local".
	s globalComment← s globalComment.
	messagedict do: [:s | messagedict code: s ← self code: s]
! !
!Class methodsFor: 'Message access' stamp: ''!
shrink | |
	messagedict ← messagedict shrink
! !
!Class methodsFor: 'Message access' stamp: ''!
copy: sel from: class classified: cat | s code |
	 "Useful when modifying an existing class"
	(sel is: Vector)
	  ifTrue: [sel do: [:s | self copy: s from: class classified: cat]]
	  ifFalse:
		[(sel is: String)
		  ifTrue: [self copy: (class organization category: sel) from: class classified: cat]
		  ifFalse:
			[code ← class code: sel.
			code ≡ nil
			  ifFalse:
				[cat ≡ nil
				  ifTrue: [cat ← class organization invert: sel].
				(messagedict has: sel)
				  ifTrue:
					[code text = (self code: sel) text
					  ifFalse: [user notify: title + ' ' + sel + ' will be redefined if you proceed.']].
				self understands: code classified: cat]]]
! !
!Class methodsFor: 'Message access' stamp: ''!
decompile: sel | |
	^user displayoffwhile⦂ [(LADDecompiler new decompile: sel in: self)]
! !
!Class methodsFor: 'Message access' stamp: ''!
archiveOn: file changesOnly: ch | org m |
	user cr "this should be called only by the system releaser
	(via UserView file:classes:changesOnly:) !!!

	if you want to archive your own classes (useful only if you have stable code
	and intend to clean up afterwards with a vmem write), see Steve.

	write comment and method text on a FileStream for some file.
	ch⇒ [write only changes (non-remote String/Paraagraphs)] write everything".
	user show: title.
	org ← self organization.
	(ch and: [(org globalCommentItself "org globalComment always yields a String, so a small kludge is in order" is: RemoteParagraph)])
	  ifFalse: [org globalComment← (RemoteParagraph new on: file) fromString: org globalComment] "archive in category&alphabetical rather than hash order (messagedict)".
	org do:
		[:m | (ch and: [((messagedict code: m) is: RemoteParagraph)])
		  ifFalse:
			[messagedict code: m ← (RemoteParagraph new on: file) fromParagraph: (self code: m).
			ch
			  ifTrue:
				[user space.
				user show: m]]]
! !
!Class methodsFor: 'Message access' stamp: ''!
canUnderstand: selector | |
	(messagedict has: selector)
	  ifTrue: [^self].
	superclass ≡ nil
	  ifTrue: [^false].
	^superclass canUnderstand: selector
! !
!Class methodsFor: 'Message access' stamp: ''!
copy: sel from: class | |
	self copy: sel from: class classified: nil
! !
!Class methodsFor: 'Message access' stamp: ''!
messages | |
	^{messagedict contents , #ClassOrganization}
! !
!Class methodsFor: 'Message access' stamp: ''!
bytesof: sel | |
	^(messagedict method: sel) asBytes
! !
!Class methodsFor: 'Message access' stamp: ''!
selectors | |
	 "Return a Vector of all my selectors."
	^self messages
! !
!Class methodsFor: 'Message access' stamp: ''!
describe: method on: strm | sel cls |
	 "append mclass and selector"
	cls ← self.
	[cls ≡ nil
	  ifTrue:
		[cls ← self.
		sel ← #?]
	  ifFalse: [sel ← cls md invert: method]] whileFalseDo:  [cls ← cls superclass].
	strm append: cls title.
	strm space.
	strm append: sel
! !
!Class methodsFor: 'Message access' stamp: ''!
derstands: selector | c |
	 "overstands?  undersits? - forget it"
	(selector is: Vector)
	  ifTrue: [selector do: [:c | self derstands: c]]
	  ifFalse:
		[(messagedict has: selector) ≡ false
		  ifFalse:
			[messagedict ← messagedict delete: selector.
			self organization delete: selector.
			lastClass ← lastSelector ← lastParagraph ← nil.
			(Changes has: (c ← title + ' ' + selector))
			  ifTrue: [Changes delete: c].
			Changes insert: (c ← '~' + c).
			^c]]
! !
!Class methodsFor: 'Message access' stamp: ''!
understands: code | selector old |
	 "install method"
	^self understands: code classified: 'As yet unclassified'
! !
!Class methodsFor: 'Message access' stamp: ''!
method: sel | |
	^messagedict methodorfalse: sel
! !
!Class methodsFor: 'Message access' stamp: ''!
notify: errorString at: position in: stream | |
	^self notify: errorString at: position in: stream for: self
! !
!Class methodsFor: 'Message access' stamp: ''!
canunderstand: selector | |
	^messagedict has: selector
! !
!Class methodsFor: 'Message access' stamp: ''!
space | a s |
	s ← 0.
	messagedict do: [:a | s ← s + (messagedict method: a) length].
	^s
! !
!Class methodsFor: 'Message access' stamp: ''!
classified: heading understands: code | |
	"A synonym for replay"
	^self understands: code classified: heading
! !
!Class methodsFor: 'Organization' stamp: ''!
wholeEnvironment | |
	^(classvars asVector concat: environment asVector) concat: (superclass ≡ nil
	  ifTrue: [#()]
	  ifFalse: [superclass wholeEnvironment])
! !
!Class methodsFor: 'Organization' stamp: ''!
clean | name |
	 "release unreferenced classvars"
	classvars do:
		[:name | (name ≠ #ClassOrganization and: [(classvars ref: name) refct = 1])
		  ifTrue: [classvars delete: name]]
! !
!Class methodsFor: 'Organization' stamp: ''!
organization | o |
	classvars ≡ nil
	  ifTrue: [self declare: #ClassOrganization].
	o ← classvars lookup: #ClassOrganization.
	(o is: ClassOrganizer)
	  ifTrue: [^o].
	o ← ClassOrganizer new init: messagedict contents sort.
	classvars insert: #ClassOrganization with: o.
	^o
! !
!Class methodsFor: 'Organization' stamp: ''!
environment | |
	^environment
! !
!Class methodsFor: 'Organization' stamp: ''!
classvars | |
	^classvars
! !
!Class methodsFor: 'System Organization' stamp: ''!
category: cat | |
	(cat is: String)
	  ifTrue: [SystemOrganization add: self title unique under: cat]
	  ifFalse: [user notify: 'Category name must be a String']
! !
!Class methodsFor: 'System Organization' stamp: ''!
moveFromCat: cat1 to: cat2 | |
	((cat1 is: String) and: [(cat2 is: String)])
	  ifTrue: [SystemOrganization move: self title unique from: cat1 to: cat2]
	  ifFalse: [user notify: 'Category name must be a String']
! !
!Class methodsFor: 'System Organization' stamp: ''!
category | |
	^SystemOrganization invert: self title unique
! !
Object subclass: #Error
	instanceVariableNames: 'errorString'
	classVariableNames: ''
	category: 'Kernel Classes'!
!Error methodsFor: 'Error messages' stamp: ''!
notify: anObject | |
	anObject notify: self message
! !
!Error methodsFor: 'Error messages' stamp: ''!
message | |
	(errorString Is: String)
	  ifTrue: [^errorString].
	^'ERROR'
! !
!Error methodsFor: 'Error messages' stamp: ''!
notify | |
	self notify: user
! !
!Error methodsFor: 'Initialization' stamp: ''!
message: aString | |
	errorString ← aString
! !
!Error methodsFor: 'Printing' stamp: ''!
printOn: aStream | |
	aStream append: 'Error: '.
	aStream append: self message
! !
nil subclass: #Object
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Kernel Classes'!
!Object methodsFor: 'As yet unclassified' stamp: ''!
growTo: newLength | |<primitive: 33>
	user croak
! !
!Object methodsFor: 'As yet unclassified' stamp: ''!
become: other | |<primitive: 35>
	user croak
! !
!Object methodsFor: 'As yet unclassified' stamp: ''!
comment | |
	^''
! !
!Object methodsFor: 'As yet unclassified' stamp: ''!
comment: cccc | |
	
! !
!Object methodsFor: 'As yet unclassified' stamp: ''!
do: aBlock || ^self asStream do: aBlock
! !
!Object methodsFor: 'Aspects' stamp: ''!
sizeInWords | class |
	class← self class.
	class isBytes ifTrue: [^self length+1/2].
	class isVariable ifTrue: [^class instsize+self length].
	^class instsize
! !
!Object methodsFor: 'Aspects' stamp: ''!
size | | <primitive: 65>
	user notify: 'Message not understood'
! !
!Object methodsFor: 'Aspects' stamp: ''!
length | | <primitive: 65>
	"Old synonym for size"
	user notify: 'Message not understood'
! !
!Object methodsFor: 'Aspects' stamp: ''!
at: i | | <primitive: 63>
	^i subscripts: self
! !
!Object methodsFor: 'Aspects' stamp: ''!
◦ i | | <primitive: 63>
	"Old synonym for at:"
	^i subscripts: self
! !
!Object methodsFor: 'Aspects' stamp: ''!
instVarAt: anInteger put: anObject | |<primitive: 38>
	user croak
! !
!Object methodsFor: 'Aspects' stamp: ''!
instfield: n | |<primitive: 37>
	"Old synonym for instVarAt:"
	user croak
! !
!Object methodsFor: 'Aspects' stamp: ''!
inspectfield: n | fixedSize |
	 "used by variable panes"
	self class isVariable
	  ifTrue:
		[fixedSize ← self class instsize.
		n > fixedSize
		  ifTrue: [^self ◦ (n - fixedSize)].
		^self instfield: n]
	  ifFalse: [^self instfield: n]
! !
!Object methodsFor: 'Aspects' stamp: ''!
◦ i ← value | | <primitive: 64>
	"Old synonym for at:put:"
	^i subscripts: self ← value
! !
!Object methodsFor: 'Aspects' stamp: ''!
instVarAt: anInteger | |<primitive: 37>
	user croak
! !
!Object methodsFor: 'Aspects' stamp: ''!
at: i put: value | | <primitive: 64>
	^i subscripts: self ← value
! !
!Object methodsFor: 'Aspects' stamp: ''!
error: s | |
	^user notify: s
! !
!Object methodsFor: 'Aspects' stamp: ''!
subError | |
	self error: 'message not defined by subclass'
! !
!Object methodsFor: 'Aspects' stamp: ''!
itself | |
	
! !
!Object methodsFor: 'Aspects' stamp: ''!
fields | |
	self class "Return an Array of all my field names or many of my subscripts." isVariable
	  ifTrue: [^self class instvars concat: (self length ≤ 50
		  ifTrue: [1 to: self length]
		  ifFalse: [(1 to: 20) concat: (self length - 20 to: self length)])].
	^self class instvars
! !
!Object methodsFor: 'Aspects' stamp: ''!
instfields | field |
	self class "Return an Array of all my field values or many of my elements." isVariable
	  ifTrue: [^(1 ~ self class instsize transform⦂ [:field | field] to⦂ [(self instfield: field)]) concat: self ◦ (self length ≤ 50
		  ifTrue: [1 ~ self length]
		  ifFalse: [1 ~ 20 concat: (self length - 20 to: self length)])].
	^1 ~ self class instsize transform⦂ [:field | field] to⦂ [(self instfield: field)]
! !
!Object methodsFor: 'Aspects' stamp: ''!
inspect | |
	NoteTaker
	  ifTrue: [user schedule: (InspectWindow new of: self)]
	  ifFalse:
		[user leaveTop.
		user restartup: (InspectWindow new of: self)]
! !
!Object methodsFor: 'Aspects' stamp: ''!
asOop | |<primitive: 39>
	user croak
! !
!Object methodsFor: 'Aspects' stamp: ''!
canunderstand: selector | |
	^self class canunderstand: selector
! !
!Object methodsFor: 'Aspects' stamp: ''!
instfield: n ← val | |<primitive: 38>
	"Old synonym for instVarAt:put:"
	user croak
! !
!Object methodsFor: 'Aspects' stamp: ''!
title | |
	^self class title + '.' + self asOop base8
! !
!Object methodsFor: 'Aspects' stamp: ''!
hash | |<primitive: 39>
	user croak
! !
!Object methodsFor: 'Classification' stamp: ''!
is: x | |
	^self class ≡ x
! !
!Object methodsFor: 'Classification' stamp: ''!
Is: x | |
	 "Is the class x a superclass or class of self"
	self class ≡ x
	  ifTrue: [^true].
	^self class Isa: x
! !
!Object methodsFor: 'Classification' stamp: ''!
Isnt: x | |
	^(self Is: x) ≡ false
! !
!Object methodsFor: 'Classification' stamp: ''!
isNumber | |
	^false
! !
!Object methodsFor: 'Classification' stamp: ''!
isArray | |
	^false
! !
!Object methodsFor: 'Classification' stamp: ''!
isnt: x | |
	^self class ≡ x ≡ false
! !
!Object methodsFor: 'Classification' stamp: ''!
class | |<primitive: 24>
	user croak
! !
!Object methodsFor: 'Classification' stamp: ''!
species | |
	^self class
! !
!Object methodsFor: 'Comparison' stamp: ''!
≡ x | |<primitive: 78>
	^self ≡ x "In case this is reached by perform:"
! !
!Object methodsFor: 'Comparison' stamp: ''!
≠ x | |
	^self = x ≡ false
! !
!Object methodsFor: 'Comparison' stamp: ''!
≥ x | |
	^self < x ≡ false
! !
!Object methodsFor: 'Comparison' stamp: ''!
≤ x | |
	^self > x ≡ false
! !
!Object methodsFor: 'Comparison' stamp: ''!
= x | |
	^self ≡ x
! !
!Object methodsFor: 'Comparison' stamp: ''!
empty | |
	^self length = 0
! !
!Object methodsFor: 'Comparison' stamp: ''!
sameAs: object | |
	^self ≡ object
! !
!Object methodsFor: 'Compiler Defaults' stamp: ''!
notify: errorString at: position in: stream for: class | syntaxWindow |
	NotifyFlag
	  ifTrue:
		[syntaxWindow ← SyntaxWindow new of: errorString at: position in: stream for: class from: thisContext sender.
		"thisContext sender← nil."
		user restartup: syntaxWindow]
	  ifFalse:
		[user notify: errorString.
		^false]
! !
!Object methodsFor: 'Compiler Defaults' stamp: ''!
ⓢ code | |
	^LADCompiler new evaluate: code asStream in: false to: self notifying: self
! !
!Object methodsFor: 'Compiler Defaults' stamp: ''!
ⓢ code | |
	^(NoteTaker
	  ifTrue: [LADCompiler]
	  ifFalse: [Generator]) new evaluate: code asStream in: false to: self notifying: self
! !
!Object methodsFor: 'Compiler Defaults' stamp: ''!
interactive | |
	^false
! !
!Object methodsFor: 'Compiler Defaults' stamp: ''!
notify: errorString at: position in: stream | |
	^self notify: errorString at: position in: stream for: self class
! !
!Object methodsFor: 'Construction' stamp: ''!
recopy | |
	 "recursively copy whole structure"
	(self is: Object)
	  ifTrue: [^self].
	^self class recopy: self
! !
!Object methodsFor: 'Construction' stamp: ''!
, x | v |
	v ← Vector new: 2.
	v ◦ 1 ← self.
	v ◦ 2 ← x.
	^v
! !
!Object methodsFor: 'Construction' stamp: ''!
asParagraph | |
	^self asString asParagraph
! !
!Object methodsFor: 'Construction' stamp: ''!
inVector | vec |
	vec ← Vector new: 1 "Return me as the sole element of a new Vector.".
	vec ◦ 1 ← self.
	^vec
! !
!Object methodsFor: 'Construction' stamp: ''!
asStream | |
	^self asVector asStream
! !
!Object methodsFor: 'Construction' stamp: ''!
copy | |
	 "create new copy of self"
	(self is: Object)
	  ifTrue: [^self].
	^self class copy: self
! !
!Object methodsFor: 'Construction' stamp: ''!
asVector | v |
	self ≡ nil
	  ifTrue: [^Vector new: 0].
	v ← Vector new: 1.
	v ◦ 1 ← self.
	^v
! !
!Object methodsFor: 'Printing' stamp: ''!
printOn: strm | t |
	strm append: (self ≡ nil
	  ifTrue: ['nil']
	  ifFalse:
		[self ≡ false
		  ifTrue: ['false']
		  ifFalse:
			[self ≡ true
			  ifTrue: ['true']
			  ifFalse:
				[t ← self class title.
				strm append: (('AEIO' has: t ◦ 1)
				  ifTrue: ['an ']
				  ifFalse: ['a ']).
				self class isVariable ifTrue:
					[strm append: t.
					strm append: ' of size '.
					t ← self length asString].
				t]]])
! !
!Object methodsFor: 'Printing' stamp: ''!
asString | strm |
	strm ← (String new: 16) asStream.
	self printOn: strm.
	^strm contents
! !
!Object methodsFor: 'Printing' stamp: ''!
printon: strm indent: level | |
	self printOn: strm
! !
!Object methodsFor: 'Printing' stamp: ''!
filout | file |
	^user displayoffwhile⦂ [
		(file ← dp0 file: self title asFileName.
		self fullprinton: file.
		file close)]
! !
!Object methodsFor: 'Printing' stamp: ''!
asFullString | strm |
	strm ← (String new: 20) asStream.
	self fullprinton: strm.
	^strm contents
! !
!Object methodsFor: 'Printing' stamp: ''!
fullprinton: strm | |
	self ≡ nil
	  ifTrue: [strm append: 'nil']
	  ifFalse:
		[self ≡ false
		  ifTrue: [strm append: 'false']
		  ifFalse:
			[self ≡ true
			  ifTrue: [strm append: 'true']
			  ifFalse: [self class print: self on: strm]]]
! !
!Object methodsFor: 'Printing' stamp: ''!
fullprint | strm |
	strm ← Stream default.
	self fullprinton: strm.
	user show: strm contents
! !
!Object methodsFor: 'Printing' stamp: ''!
print | |
	user show: self asString
! !
!Object methodsFor: 'System Primitives' stamp: ''!
processFor: selector withArgs: args | newProcess |
	newProcess ← self
		processForContext⦂ [self perform: selector withArgs: args.  newProcess terminate]
		priority: Processor activePriority.
	^newProcess
! !
!Object methodsFor: 'System Primitives' stamp: ''!
processForContext⦂ context priority: priority | |
	^Process new forContext: context priority: priority
! !
!Object methodsFor: 'System Primitives' stamp: ''!
fork: selector withArgs: args | |
	Processor resume: (self processFor: selector withArgs: args)
! !
!Object methodsFor: 'System Primitives' stamp: ''!
fork: selector | |
	Processor resume: (self processFor: selector)
! !
!Object methodsFor: 'System Primitives' stamp: ''!
perform: selector withArgs: vec | t3 |
	(t3 ← vec length) = 0
	  ifTrue: [^self perform: selector].
	t3 = 1
	  ifTrue: [^self perform: selector with: vec ◦ 1].
	t3 = 2
	  ifTrue: [^self perform: selector with: vec ◦ 1 with: vec ◦ 2].
	t3 = 3
	  ifTrue: [^self perform: selector with: vec ◦ 1 with: vec ◦ 2 with: vec ◦ 3].
	user notify: 'More than 3 args for perform:'
! !
!Object methodsFor: 'System Primitives' stamp: ''!
perform: selector with: arg1 with: arg2 with: arg3 | |<primitive: 36>
	 "selector had better take 3 args"
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
perform: selector with: arg1 with: arg2 | |<primitive: 36>
	 "selector had better take 2 args"
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
perform: selector with: arg1 | |<primitive: 36>
	 "selector had better take 1 arg"
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
perform: selector | |<primitive: 36>
	 "Send self the message, selector; it had better be unary"
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
doesNotUnderstand: message | selector args |
	selector ← message selector.
	args ← message args.
	user notify: 'Message not understood: ' + selector.
	^self perform: selector withArgs: args
! !
!Object methodsFor: 'System Primitives' stamp: ''!
processFor: selector | newProcess |
	newProcess ← self
		processForContext⦂ [self perform: selector.  newProcess terminate]
		priority: Processor activePriority.
	^newProcess
! !
!Object methodsFor: 'System Primitives' stamp: ''!
execute: method with: arg | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
execute: method with: arg1 with: arg2 with: arg3 | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
execute: method withArgs: vec | t3 |
	(t3 ← vec length) = 0
	  ifTrue: [^self execute: method].
	t3 = 1
	  ifTrue: [^self execute: method with: vec ◦ 1].
	t3 = 2
	  ifTrue: [^self execute: method with: vec ◦ 1 with: vec ◦ 2].
	t3 = 3
	  ifTrue: [^self execute: method with: vec ◦ 1 with: vec ◦ 2 with: vec ◦ 3].
	user notify: 'More than 3 args for execute:'
! !
!Object methodsFor: 'System Primitives' stamp: ''!
execute: method with: arg1 with: arg2 | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
execute: method | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
refct | |<primitive: 34>
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
swap⦂ variable | x |
	 "assign me to variable and return its old value"
	x ← variable value.
	variable value← self.
	^x
! !
!Object methodsFor: 'System Primitives' stamp: ''!
startup | |
	 "loopless scheduling"
	self firsttime
	  ifTrue:
		[[self eachtime] whileTrueDo: [].
		^self lasttime].
	^false
! !
!Object methodsFor: 'System Primitives' stamp: ''!
nextInstance | |<primitive: 82>
	 "return next in enumeration of my class, false if done"
	user croak
! !
!Object methodsFor: 'System Primitives' stamp: ''!
messageNotUnderstood: op withArgs: args from: sender | |
	thisContext sender← sender.
	user notify: 'Message not understood: ' + op
! !
!Object methodsFor: 'System Primitives' stamp: ''!
error | sender op n args i |
	 "after compiling execute: nil installError.  "
	sender ← thisContext sender.
	op ← sender thisop.
	n ← op numArgs.
	args ← Vector new: n.
	(n to: 1 by: ¬1) do: [:i | args ◦ i ← sender pop].
	^self messageNotUnderstood: op withArgs: args from: sender 
! !
Object subclass: #UserView
	instanceVariableNames: 'screenrect "<Rectangle> current screen size"
			vtab "<Integer=0mod2> offset from hardware top"
			htab "<Integer=0mod16> offset from hardware left"
			scale "<Integer=1 or 2> 2 means double bits mode"
			color "<Integer=0 or 1> 1 means reverse field"
			projectWindow "my representative in an overview"
			disp "<dispframe> default message stream"
			sched "<Vector> Windows in this view"'
	classVariableNames: ''
	category: 'Kernel Classes'!
!UserView methodsFor: 'As yet unclassified' stamp: ''!
displayHeight: h | | 
	h ← h | 16.
	DisplayForm refct>3
	  ifTrue: [user croak]
	  ifFalse:
		[user currentDisplay: (Form new extent: 640 ⌾ 16 bits: (String new: 16 * 80) offset: nil).
		user currentDisplay: (Form new extent: 640 ⌾ h bits: (String new: h * 80) offset: nil)].
	user restore
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
primMousePt | |<primitive: 42>
	^self croak
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
primMouseKeys | |<primitive: 46>
	^self croak
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
primCursorLoc← pt | |<primitive: 43>
	^self croak
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
cursorLink: bool | |<primitive: 75>
	^self croak
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
mapDisplay | t1 |
	(DisplayBLTer ← BitBlt init) sourceForm: DisplayForm.
	DisplayBLTer destForm: DisplayForm.
	(t1 ← BitBlt init) sourceForm: currentCursor asForm.
	t1 destForm: DisplayForm.
	t1 sourceRect: (0 ⌾ 0 rect: currentCursor extent).
	t1 effect: oring.
	t1 installDisplay
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
primKbdNext | |<primitive: 45>
	^self croak
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
primKbdPeek | |<primitive: 44>
	^self croak
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
currentDisplay: form | |
	DisplayForm ← form.
	screenrect ← 0 ⌾ 0 rect: form extent.
	vtab ← htab ← mxoffset ← myoffset ← 0.
	self mapDisplay
! !
!UserView methodsFor: 'As yet unclassified' stamp: ''!
primKbdBlock | |<primitive: 47>
	^self croak
! !
!UserView methodsFor: 'Changes' stamp: ''!
changedClasses | titles space str |
	space ← ' ' ◦ 1 "return a vector of the names of classes which have been changed".
	titles ← HashSet new init.
	Changes contents do:
		[:str | titles insert: ((Stream new of: str) upto: space) "class title"].
	^titles contents
! !
!UserView methodsFor: 'Changes' stamp: ''!
changedCategories | titles space str |
	space ← ' ' ◦ 1 "return a vector of the names of class categories which have been changed".
	titles ← HashSet new init.
	Changes contents do: [:str | titles insert: (SystemOrganization invert: ((Stream new of: str) upto: space) unique)].
	^titles contents
! !
!UserView methodsFor: 'Changes' stamp: ''!
noChanges | |
	Changes init
! !
!UserView methodsFor: 'Changes' stamp: ''!
changedMessages | |
	^Changes contents sort
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
newdisploc: origin and: corner | |
	 "for moving disp"
	(disp text "user newdisploc: 8⌾0 and: 150⌾96" frame inset: ¬2 ⌾ ¬2) clear.
	disp text frame← origin rect: corner.
	disp show
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
newdisp | |
	 "for when some class associated with running Dispframe  changed"
	self unschedule: disp.
	disp ← Dispframe new rect: (8 ⌾ 0 rect: 150 ⌾ 96).
	self schedule: disp.
	self clearshow: 'New Dialogue window created.
'
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
print: x | |
	disp print: x.
	disp show
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
ev | |
	disp ev
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
tab | |
	disp tab
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
cr | |
	disp cr
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
frame | |
	 "return rectangle of dialogue window"
	^disp text frame
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
clear | |
	 "clear disp of debris and characters"
	disp clear
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
show | |
	disp outline.
	disp show
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
read | |
	^disp read
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
clearshow: str | |
	disp clear.
	disp append: str.
	disp show
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
next← x | |
	disp cr "simulate a Vector Stream".
	disp print: x.
	disp show
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
show: str | |
	disp append: str.
	disp show
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
request: s | |
	^disp request: s
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
croak | |
	self notify: 'A primitive has failed.'
! !
!UserView methodsFor: 'Dialog Window' stamp: ''!
space | |
	disp space
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
core | n nobjects nwords name class inst bytes | "user core"
	nobjects← nwords← 0.
	self classNames do:
		[:name | class← Smalltalk◦name.
		class isVariable
			ifTrue: [n← 0.
				bytes← class isBytes.
				class allInstancesDo:
					[:inst | n← n+1.
					nwords← nwords
						+(bytes ifTrue: [inst length+1/2] ifFalse: [inst length])]]
			ifFalse: [n← class howMany].
		nobjects← nobjects+n.
		nwords← nwords+(class instsize+2*n)]. "+2 for header"
	^{nobjects , nwords}
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
allImplementors: lit | result cl |
	result ← (Vector new: 10) asStream.
	Class allInstancesDo:
		[:cl| (cl canunderstand: lit) ifTrue: [result next← cl title]].
	^result contents
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
allSenders: lit | result cl lis |
	result ← (Vector new: 10) asStream.
	Class allInstancesDo:
		[:cl| lis ← cl whosends: lit.
		lis length>0 ifTrue: [result next← {cl title, lis}]].
	^result contents
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
systemStartup | |
	 "To do after system flush and installation of new core image"
	Top top.
	Window classInit "The following screen extent seems to really fill the screen in x,
	the Alto Hardware Manual to the contrary notwithstanding.".
	self screenextent: 640 ⌾ 580 tab: 0 ⌾ 50.
	Sources release.
	dp0 release.
	dp1 release.
	self releaseExternalViews.
	E ≡ nil
	  ifFalse: [ "ignore broadcasts"
		E broadcastFilter: true]
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
version | |
	^'Smalltalk-80 December 16' "user version

low level disk address calculations are more general (necessary for 14-sector Dorado/Dolphin file systems)
better error recovery for broken and timed out Leaf connections
AltoFileDirectory disk page allocation/deallocation bugs fixed
miscellaneous printing fixes
Juniper fixes (2)
goodie: again-del-forget.st
Phylum account changes 
	default Leaf connection is logged in to <Smalltalk-User>
	system release uses [Phylum]<Smalltalk-76> instead of [Ivy]<Smalltalk>
see UserView workspace for logging into your account on Phylum, changing default printer

September 3, 5.5j
	duplicate packet fix
	fixes to ether (routing table, name lookup, phylum, Int32), printer names,
		files, UserView time messages, context simulation,
		replace in BitBlt & Paragraph, NotifyWindow cleanup,
		Class code: always decompiles with left shift key, window printing fixes,
		SystemOrganization globalComment contains no nulls
	the following changes files were included:
	[phylum]<small-goodies>
		5.5i.changes.st, notifychange.st, window-print-changes.st
	[phylum]<findit>5.5i.more.changes.st
	[maxc]<dolbec>int32change.st
	[maxc>ingalls>fixes.st
	[ivy]<kaehler>context-simulation.st
	[ivy]<borning>context-changes.st

May 1, 5.5i
	obscure file bugs eliminated; version features added (goody: File-version.st).
	Ifs multiple connections fixed; Ifs error numbers looked up in Ifs.Errors.
	duplicate packets eliminated at lowest level.
	Int32 primitive fix. Juniper retransmit parameters increased
	Integer compare: LargeInteger now works
	CodePane/FilePane 'print' (within a CodeWindow) now prints entire Paragraph
		rather than only part within window
	ScrollBars hide during CodePane again & cancel. cancel saves your old text, so
		an immediate undo will replace the current selection with your previous text.

April 11, 5.5h
	Alto file names limited to 39 characters ('somestring' asFileName will fix
		name, truncating if necessary). other misc. file, ether, simulator fixes.
	BitBlt fixed so that BitRects don't lose their bits
	BitBlt used to speedup reading&writing files, sending Press files to printers
	ParagraphScanner puts underlining into Press files
	printer names updated (PressFile classInit). hashing-changes.st included.
	after font cataclysm, get new version of Fonts.Widths before printing
	system release procedure modified

March 6, 5.5g
	ether, file, vmem writing fixes.  cursor clipping on screen boundary.
	BitBlt used for String growing, copying, replacing
	goodies included: display-off-after-notify.st, CodePane-doit.st,
		context-simfix2.st, ILchanges.st, string-changes.st

see [Phylum]<Smalltalk> for the following files.  () surround an optional prefix or suffix.
	Document.Press
		mini-guide to Smalltalk system and user interface
	VersionHistory
		information about versions up to 5.5g
	ChangedMessages
		a list of  messages which have changed
	xxx.Press
		press file for CrossReference or for system category 'xxx' in current version
		to save paper, consider consulting the LRG alcove copies
	(Xm)Small.Boot(.version)
	(Xm)Smalltalk.Syms(.version)
		older versions of .Boot and .Syms are explicitly named.
	Smalltalk.Sources.version
		all Smalltalk.Sources (including the current one) are explicitly named

[Phylum]<Small-Goodies> contains miscellaneous bug fixes and new features (and even some documentation: goodies.bravo, .press) offered by the community of Smalltalk Users.
"
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
purgealittle | |<primitive: 0>
	
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
workspace | |
	user notify: 'Not meant to be executed' "
XEROX - Learning Research Group
 
user screenextent: 640⌾580 tab: 0⌾50.
NotifyFlag ← true.
Changes init.
user changedMessages
user changedClasses
user changedCategories
Undeclared contents

to set the default printer
PrinterName←'Menlo'.
PrinterName←(PressFile new) selectPrinter: PrinterName.

to change phylum to access your account
user releaseExternalViews. phylum name: 'name' password: 'password'.

dp0 filin: ↪('Changes.st').
(dp0 file: 'changes.st') filout.
(dp0 file: 'xxx') edit.
dp0 pressfilin: ↪('xxx.press').
(dp0 filesMatching: '*.st') sort
dp0 list. dp0 freePages
dp0 delete: 'old'
dp0 rename: 'old' newName: 'new'

for reinitializing Sources and phylum
Sources release. phylum release. Sources reopen.

to make Smalltalk Sources local
 | s. s ← 'Smalltalk.Sources.'.
(phylum asFtpDirectory) retrieve: '<Smalltalk>' + s + user versionName as: s; close.
Sources on: (dp0 file: s).

to switch back to remote Sources
Sources close; on: (phylum file: '<Smalltalk>Smalltalk.Sources.' + user versionName).


to filin a remote Smalltalk file
phylum filin: ↪('<Small-goodies>NotifyChange.st').

to print a remote/local press file
(phylum pressfile: '<Smalltalk>xxx.press') toPrinter.
(dp0 pressfile: 'xxx.press') toPrinter: 'Lilac'.

File noChanges.
BitRect new fromuser; edit.
user schedule: (defaultBitRectEditor newframe).

DocumentEditor new defaultdocument: 'test'.
DocumentEditor new init: (Document new fromPress: 'test.document').


user releaseExternalViews.
E sleep. E kill. E ← nil.
E ← Etherworld new. E broadcastFilter: true. E wakeup.
Sources reopen.

for primary Smalltalk access to file servers and printers at other sites.
substitute yourserver for phylum above, compile this workspace
PrinterName ← 'name-of-your-printer'.
Smalltalk declare: ↪yourserver.
yourserver ← ILFileDirectory new directory: 'name-of-your-server'.
yourserver name: 'Smalltalk-User' password: 'Smalltalk'.
Sources on: (yourserver file: '<Smalltalk>Smalltalk.Sources.' + user versionName).
Changes init.

user Swat.
"
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
coreLeft | | <primitive: 48>
	user croak
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
classNames | classes x c |
	 "an alphabetized Vector of all Smalltalk class titles uniqued"
	AllClassNames ≡ nil
	  ifTrue:
		[classes ← (Vector new: 20) asStream.
		Smalltalk do:
			[:x | c ← Smalltalk ◦ x.
			((c is: Class) or: [(c is: VariableLengthClass)])
			  ifTrue: [classes next← x]
			  ifFalse:
				[]].
		AllClassNames ← classes contents sort]
	  ifFalse:
		[.
		^AllClassNames]
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
release | m |
	(m ← Undeclared contents "prepare to release this version (after editing UserView version)
	and possibly copying Sources file (see writeSources:)") length > 0
	  ifTrue: [user notify: 'Undeclared contains ' + m asString]
	  ifFalse:
		[user displayoffwhile⦂ [
			(m ← Sources directory "either create a new Sources file (write all messages) or append only changes" checkName: '<Smalltalk>Smalltalk.Sources.' + user versionName "for repeated releases in same version.
		should also work for Sources local (if renamed)".
			user writeSources: (m = Sources name
			  ifTrue: [Sources]
			  ifFalse: [Sources directory file: m]) "make workspace local".
			UserView md code: #workspace ← UserView code: #workspace.
			user writeChangedMessages: (phylum file: '<Smalltalk>ChangedMessages'))].
		user noChanges.
		user releaseMessage]
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
file: file classes: classes changesOnly: ch | cl |
	 "called by UserView release to write just changes or entire system on a
	new file.  also, see comment in Class archiveOn:changesOnly:.

	write class comment and message text onto a FileStream (which could refer
	to an AltoFile, ILFile, etc.). either just changes or everything are
	written and replaced with RemoteParagraph references"
	ch
	  ifTrue: [file settoend]
	  ifFalse: [file reset].
	file readwriteshorten.
	classes do: [:cl | Smalltalk ◦ cl archiveOn: file changesOnly: ch].
	file close.
	file readonly
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
systemworkspace3 | |
	 "for system releasers only!!!


8. to update press files for system categories or cross reference listing directly on Phylum, browse or spawn this window.  edit pf to specify a list of system categories to print, usually from step 1, e.g. user changedCategories: ↪('Basic Data Structures' ...) or SystemOrganization categories (for all); delete toPrinter if you don't want the press files printed. edit xref to be user classNames if you want to generate a cross reference listing.
	 | pf xref cat.
	pf ← ↪  ('Text Objects' 'Kernel Classes' 'Press File Support' 'IFS File System' 'Alto File System' 'Panes and Menus' 'Files' 'Juniper' 'Windows' 'Graphical Objects' 'Numbers' 'Basic Data Structures' ).
	xref ← ↪().

	user releaseExternalViews.
	phylum name: 'Smalltalk' password: 'password'.
	for⦂ cat from: pf do⦂ [
		((phylum file: (cat + '.Press') asFileName) asPressPrinter) stamp;
			printclass: (SystemOrganization category: cat); close; toPrinter].
	xref empty⇒ []
	user printCrossReference: xref on: (phylum file: 'CrossReference.Press').
"
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
systemworkspace2 | |
	 "for system releasers only!!!


5. if no surgery or vmem write involved, skip to step 7. start here with an xm version to make a non-xm version.  specify option below:
	1 vmem write (includes xm surgery)
	2 xm surgery
	3 non-xm surgery
to make things totally automatic, edit in your valid Maxc name and password, otherwise Ftp will ask you later (some of the file transfers can be edited out (i.e. if files are already present; also Ramload is superfluous on Dorado). in the case of surgery only, at the end you will have to hit a key after safing.

	| option prefix dir file.
option ← 1.
dir ← phylum asFtpDirectory.
dir directoryName: 'Smalltalk-76'.
prefix ← [option=3⇒ [''] 'Xm'].
for⦂ file from: ↪('Smalltalk.Run' 'Smalltalk.Syms' 'Byterp.Mb') do⦂ [
	dir retrieve: prefix + file as: file].
dir closeThen: ([option=1⇒ ['delete oldsmall.boot;
copy newsmall.boot ← small.boot; '] '']) + 
'ftp maxc Login/c yourname yourpassword directory/c alto retrieve/c packmu.run ramload.run;
Resume small.boot;
Ramload/N Byterp.mb/F 1000/A;
Smalltalk.run'.

option=1⇒ [(VirtualMemory new) giveBirth3. user quit]
Vmem ramwrite: (dp0 oldFile: 'byterp.mb').
Vmem surgery: (dp0 oldFile: 'Smalltalk.run').


6. after a successful vmem write or surgery, execute this (selecting here is tricky or type in a Dispframe)
	user systemStartup.


7. edit lastversion (and Smalltalk password) and execute the following, then close this window (clean up screen for non-xm?), and quit. it then renames old versions of files, stores new versions of files, e.g. remote XmSmall.Boot becomes XmSmall.Boot.5.5g and local Small.Boot becomes remote XmSmall.Boot
	| lastversion dir file remotefile.
	lastversion ← '5.5j'.
	dir ← phylum asFtpDirectory.
	dir login: 'Smalltalk' password: 'password'.
	for⦂ file from: ↪('Small.Boot' 'Smalltalk.Syms') do⦂ [
		remotefile ← ([user hasXM⇒ ['Xm'] '']) + file.
		dir rename: remotefile newName: remotefile + '.' + lastversion;
			store: file as: remotefile].
	(dp0 file: 'rem.cm') append: dir commands; cr; close.
	user releaseMessage.
"
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
systemworkspace1 | |
	 "for system releasers only!!!

this has been partitioned into three workspaces for editing convenience:
	systemworkspace1
		steps 0-4: general comments, handling Sources, creating a release.
	systemworkspace2
		steps 5-7: doing a vmem write or surgery, storing finished files on Phylum
	systemworkspace3
		step 8: after a release, e.g. updating press files


0. This boot file should be named small.boot for vmem writing, surgery, and command file purposes.  If you made changes to the Sources disk be sure to update the current versions of (Xm)Smalltalk.Run, (Xm)Smalltalk.Syms, and (Xm)Byterp.mb on [Ivy]<Smalltalk>. This procedure works best on a Dorado for speed and disk space reasons, and it also can be done on an Alto (double disk O.S. required for vmem write).  Microcode changes (including making non-xm versions) must(?) be done on an Alto.  Step 5 (vmem writing) assumes enough space for another boot file.  To turn off display during execution, hold down left shift key while selecting 'doit'.

For those who want to vmem write their own versions, do not execute steps 4, 7 or 8 without further editing of file and directory names. Underlined items are typical values and normally must be edited to be useful.


1. to create an xm version:  filin changes and selected goodies. Undeclared must be empty for release to work (step 4). copy the categories of classes which have changed to systemworkspace3 (for later printing) and recompile it.
	dp0 filin: ↪('changes.st').
	phylum filin: ↪('<Small-goodies>xx.st').
	Undeclared contents inVector, user changedCategories


2. update version number/letter and comments in UserView version


3. the Sources file will be ordinarily be created in step 4. if only a few changes are involved, it may be somewhat faster to copy the old sources file to the new sources file (this step). then step 4 will only append changes.
	phylum store: Sources reset as: 'Smalltalk.Sources.' + user versionName.


4. checks Undeclared, writes all or appends changed messages to Sources file, updates ChangedMessages, inits Changes, puts up greeting, and sets the default user name & password. note: this is only to be executed for releasing the Smalltalk system itself (supply the proper password!!). if you plan to do a vmem write next, better to do this as first line of step 5.
	user releaseExternalViews.
	phylum name: 'Smalltalk' password: 'password'.
	user release.
	phylum name: 'Smalltalk-User' password: 'Smalltalk'.

	to write out the sources for a private version, specify which directory to use (don't leave Smalltalk as the default) and which categories and/or classes are to be included.
	| c classes. user releaseExternalViews. classes ← (Vector new: 50) asStream.
	phylum name: 'name' password: 'password'.
	for⦂ c from: ↪('category1' class1) do⦂ [
		c is: String⇒ [classes append: (SystemOrganization category: c)]
		classes next ← c].
	user file: (phylum file: '<ddd>xx.Sources.' + user versionName)
		classes: classes contents changesOnly: false.
"
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
releaseMessage | |
	user clearshow: 'Welcome to ' + user version
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
writeChangedMessages: ChangedMessages | class m ms |
	ChangedMessages settoend "append changed messages to a file (usually on [phylum])".
	ChangedMessages cr.
	ChangedMessages cr.
	ChangedMessages asParagraphPrinter stamp.
	class ← ''.
	user changedMessages do:
		[:m | ms ← m asStream.
		(ms upto: 32) = class
		  ifTrue:
			[ChangedMessages append: ', '.
			ChangedMessages append: (ms upto: 32)]
		  ifFalse:
			[ChangedMessages cr.
			ChangedMessages append: m.
			class ← m asStream upto: 32]].
	ChangedMessages close
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
classInit | |
	 " UserView classInit. "
	screenMenu ← Menu new string: 'exit to overview
snapshot
quit
open a subview
open a browser
open a workspace
turtle demo'
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
writeSources: newSources | |
	user file: newSources classes: SystemOrganization changesOnly: newSources end "write a new Sources file (usually on [phylum]Smalltalk.Sources.xxx
		(i.e. xxx = user versionName))
	if it's a new file or empty, write all Sources. otherwise it better be a copy of
	the previous Sources file (only changes will be appended. do the copy with ftp)" ≡ false.
	Sources close.
	Sources ← newSources
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
printCrossReference: classNames on: f | dict m md frame l each s class |
	 "user displayoffwhile⦂ [
	user printCrossReference: user classNames
		on: (dp0 file: 'CrossReference.Press')].

		user classNames
		(SystemOrganization category: 'xyz')
		↪(class1 class2)"
	dict ← Dictionary init: 200.
	(1 to: 32) do: [:m | dict insert: SpecialOops ◦ (9 + m) with: #((Primitives ) () ) copy].
	classNames transform⦂ [:each | each] to⦂ [
		(user show: each.
		user space.
		md ← (Smalltalk ◦ each) md.
		md do:
			[:m |  "Tally all the UniqueString literals"
			(s ← dict lookup: m)
			  ifFalse: [dict insert: m with: (s ← #(() () ) copy)].
			(s ◦ 1 has: each)
			  ifFalse:
				[s ◦ 1 ← {s ◦ 1 , each}.
				(md literals: m) do:
					[:l | (l is: UniqueString)
					  ifTrue:
						[(s ← dict lookup: l)
						  ifFalse: [dict insert: l with: (s ← #(() () ) copy)].
						(s ◦ 2 has: each)
						  ifFalse: [s ◦ 2 ← {s ◦ 2 , (each , m)}]]]]])].
	f ← f asPressPrinter.
	f stamp.
	frame ← f defaultframe "Print the messages out sorted".
	dict contents sort do:
		[:m | user show: m.
		user space.
		f frame← frame.
		md ← dict ◦ m.
		s ← (String new: 200) asStream.
		s append: m.
		s append: ((md ◦ 1) length = 0
		  ifTrue: [' ( - undefined -  ']
		  ifFalse: [' (']).
		(md ◦ 1) sort do:
			[:l | s append: l.
			s append: ', '].
		s skip: ¬2.
		s append: ')'.
		f print: (s contents asParagraph maskrun: 1 to: m length under: 1 to: 1).
		f frame← frame minX + 500 ⌾ frame minY rect: frame corner.
		s reset.
		(md ◦ 1 has: #Primitives)
		  ifTrue:
			[s append: 'untallied.'.
			md ◦ 2 ← #()]
		  ifFalse:
			[(md ◦ 2) length = 0
			  ifTrue: [s append: '- unreferenced -']].
		class ← #-.
		(md ◦ 2) sort do:
			[:l | l ◦ 1 = class
			  ifTrue: [s append: ', ']
			  ifFalse:
				[class ≠ #-
				  ifTrue: [s cr].
				s append: '('.
				s append: l ◦ 1.
				s append: ') '.
				class ← l ◦ 1].
			s append: l ◦ 2].
		f print: s contents asParagraph].
	f close.
	f toPrinter
! !
!UserView methodsFor: 'Misc System Stuff' stamp: ''!
versionName | s |
	s ← self version asStream "skip Smalltalk".
	s skipTo: 32 "return version identification, e.g. 5.5f".
	^s upto: 32
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
cursorloc← pt | |
	^self primCursorLoc← pt - (mxoffset ⌾ myoffset)
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
currentCursor: c | coff p |
	currentCursor ← c.
	coff ← c offset.
	p ← self mp.
	mxoffset ← coff x - htab.
	myoffset ← coff y - vtab.
	self cursorloc← p "Cursor ref pt contin across boundary".
	self mapDisplay
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
mp | |
	^self primMousePt + (mxoffset ⌾ myoffset)
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
buttons | |
	^self primMouseKeys land: 7
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
tabletrelative | |
	mem ◦ 86 ← ¬1
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
waitclickbug | |
	self waitnobug.
	^self waitbug
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
waitnokey | |
	[self keyset = 0] whileFalseDo:  [self rawkbck]
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
tabletbug | |
	^mem ◦ ¬448 < 0
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
tabletabsolute | |
	mem ◦ 86 ← 1
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
redbug | |
	^self buttons = 4
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
anybug | |
	^self buttons > 0
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
mpnext | |
	self redbug "return next mouse point if red button or tablet is down; otherwise false"
	  ifTrue: [^self mp].
	^false
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
kbd | |
	[self rawkbck] whileFalseDo: 
		[NoteTaker
		  ifFalse: [self purgealittle]].
	^kbMap ◦ self rawkbd
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
y | |
	^self mp y
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
x | |
	^self mp x
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
rawkbck | event rdpt stroke |
	 "flush events until key down or no event."
	(NoteTaker and: ['NoEventQ'])
	  ifTrue:
		[[stroke ← user primKbdPeek] whileTrueDo:
			[stroke ≥ 0
			  ifTrue: [^stroke].
			user primKbdNext].
		^false].
	[event ← Events peek] whileTrueDo:
		[event isKbdDown
		  ifTrue: [^event stroke].
		Events next].
	^false
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
kbdnext | event |
	NoteTaker
	  ifTrue:
		[self kbck
		  ifTrue: [^self kbd].
		^false]
	  ifFalse:
		[ "returns next character (mapped) if any; otherwise false"
		[(event ← Events dequeue) or: [(event ← Events primitiveDequeue)]] whileTrueDo:
			[event isKbdDown
			  ifTrue: [^kbMap ◦ event stroke]].
		^false]
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
anykeys | |
	^self keyset > 0
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
bluebug | |
	^self buttons = 1
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
nobug | |
	^self anybug ≡ false
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
yellowbug | |
	^self buttons "From Smalltalk 5.4i on 30 October 1979 at 2:28:57 pm." = 2
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
waitbug | |
	[self anybug] whileFalseDo:  [].
	^self mp
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
kbck | t |
	(t ← self rawkbck)
	  ifTrue: [^kbMap ◦ t].
	NoteTaker
	  ifTrue: [^false].
	self purgealittle.
	^false
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
waitnobug | |
	[self anybug] whileTrueDo: [].
	^self mp
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
rawkbd | stroke |
	(NoteTaker and: ['NoEventQ'])
	  ifTrue:
		[[self rawkbck] whileFalseDo:  [].
		^self primKbdNext].
	[stroke ← self rawkbck] whileFalseDo:  [ "wait for activity"
		].
	Events next.
	^stroke "if key down, return stroke"
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
tablet | |
	^mem ◦ ¬448 ≠ 0
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
leftShiftKey | |
	 "left shift key down?"
	NoteTaker
	  ifTrue: [^false].
	^(mem ◦ ¬482 land: 64) = 0
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
currentCursor | |
	^currentCursor
! !
!UserView methodsFor: 'Mouse, Cursor, Keys' stamp: ''!
keyset | |
	^(self primMouseKeys lshift: ¬3) land: 31
! !
!UserView methodsFor: 'Notify Window' stamp: ''!
notify: errorString | notifyWindow |
	notifyWindow ← self notifier: errorString stack: thisContext sender "Create a notify window looking at the Context stack" interrupt: false.
	notifyWindow
	  ifTrue:
		[NoteTaker
		  ifTrue:
			[self schedule: notifyWindow.
			user run]
		  ifFalse:
			[thisContext sender← nil.
			Top currentPriority = 1
			  ifTrue: [self restartup: notifyWindow]
			  ifFalse:
				[self scheduleOnBottom: notifyWindow.
				Top errorReset]]]
! !
!UserView methodsFor: 'Notify Window' stamp: ''!
notifier: titleString stack: stack interrupt: flag | |
	self restoredisplay "Restore the full display.  Schedule a one-paned window to notify the user that errorString happened.".
	NotifyFlag ≡ false
	  ifTrue:
		[disp cr.
		disp append: 'NotifyFlag is false...'.
		disp cr.
		disp append: ' sender debug shows stack, user restart aborts,'.
		disp cr.
		disp append: ' tempframe shows args, ctrl-d proceeds'.
		disp cr.
		disp append: titleString.
		disp cr.
		disp show.
		stack debug.
		^false].
	^NotifyWindow new of: titleString stack: stack interrupt: flag
! !
!UserView methodsFor: 'Notify Window' stamp: ''!
notifier: titleString level: lev interrupt: flag | |
	self restoredisplay "Restore the full display.  Schedule a one-paned window to notify the user that errorString happened.".
	NotifyFlag ≡ false
	  ifTrue:
		[disp cr.
		disp append: 'NotifyFlag is false...'.
		disp cr.
		disp append: ' top-blank shows stack, user restart aborts,'.
		disp cr.
		disp append: ' tempframe shows args, ctrl-d proceeds'.
		disp cr.
		disp append: titleString.
		disp cr.
		disp show.
		(Top ◦ lev) debug.
		^false].
	^NotifyWindow new of: titleString level: lev interrupt: flag
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
restoredisplay | |
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
displayoffwhile⦂ expr | |
	^expr eval  "Don't bother turning off the display"
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
reconfigure | |<primitive: 75>
	
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
copyIn: p | |
	^UserView new screenrect: screenrect copy vtab: vtab htab: htab scale: scale color: color projectWindow: p disp: disp sched: #()
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
screenrect | |
	^screenrect
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
install | |
	self screenextent: screenrect extent tab: htab ⌾ vtab
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
color: t1 scale: t2 | |
	color ← t1.
	scale ← t2.
	self install
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
bugScreenMenu | n i t3 |
	(t3 ← screenMenu bug) = 1
	  ifTrue:
		[projectWindow ≡ nil
		  ifFalse: [projectWindow runParent]]
	  ifFalse:
		[t3 = 2
		  ifTrue: [user snapshot]
		  ifFalse:
			[t3 = 3
			  ifTrue: [user quit]
			  ifFalse:
				[t3 = 4
				  ifTrue: [self schedule: ProjectWindow init]
				  ifFalse:
					[t3 = 5
					  ifTrue: [self schedule: BrowseWindow default]
					  ifFalse:
						[t3 = 6
						  ifTrue: [self schedule: (CodeWindow new class: UserView selector: #workspace para: (UserView code: #workspace) formerly: false)]
						  ifFalse:
							[t3 = 7
							  ifTrue:
								[n ← Turtle init.
								n color: black.
								n width: 4.
								n inking: oring.
								(1 to: 50) do:
									[:i | n go: i * 4.
									n turn: 89]]]]]]]]
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
projectWindow | |
	projectWindow ≡ nil
	  ifTrue:
		[projectWindow ← ProjectWindow new.
		projectWindow userview: self changes: Changes parent: projectWindow].
	^projectWindow
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
screenextent: extent tab: tab | |
	mem ◦ 53 ← color * 16384 + (scale = 2
	  ifTrue: [¬32768]
	  ifFalse: [0]) + (tab x / 16 * 256) + (extent x / 16 | 2).
	mem ◦ 55 ← extent y * scale / 2.
	mem ◦ 51 ← 1 max: tab y / 2.
	htab ← tab x | 16.
	vtab ← mem ◦ 51 * 2.
	screenrect ← 0 ⌾ 0 rect: extent x | 32 ⌾ (extent y | 2).
	self currentCursor: currentCursor.
	self reconfigure.
	self restore
! !
!UserView methodsFor: 'Screen Views' stamp: ''!
screenrect: t1 vtab: t2 htab: t3 scale: t4 color: t5 projectWindow: t6 disp: t7 sched: t8 | |
	screenrect ← t1.
	vtab ← t2.
	htab ← t3.
	scale ← t4.
	color ← t5.
	projectWindow ← t6.
	disp ← t7.
	sched ← t8
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	InitialContext ← thisContext.
	self snapshotPrimitive.
	self mapDisplay.
	self restore
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
releaseExternalViews | t |
	SourceFiles notNil do: [:t | t close].
	dp0 close.
	dp1 close.
	(externalViews length to: 1 by: ¬1) do: "release (obsolete) known external views"
		[:t | (externalViews ◦ t) release.
		externalViews ◦ t ← nil].
	externalViews reset
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
overlay: fileid | |
	self releaseExternalViews.
	E ≡ nil
	  ifFalse: [E sleep]. "put the ethernet to sleep"
	self InLd: fileid.
	[user keyset > 0] "we start here after a resume"
		whileTrueDo: [user show: 'The keyset is stuck'.
		user cr]
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	InitialContext ← thisContext.
	self snapshotPrimitive.
	self mapDisplay.
	self restore
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
quitFrom: controller | |
	self overlay: #(0 0 0 0 0 ).
	screenrect clear.
	controller restore
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
newSourceFile: f || "user newSourceFile: (dp0 file: 'new.sources').
				then snapshot and copy into st80dec25"
	f append: 'ST80 sources of January 13'; cr. "should be more appropriate header"
	CompiledMethod allInstances do: [:meth | meth moveSourceTo: f].
	f close.
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	InitialContext ← thisContext.
	self snapshotPrimitive.
	self mapDisplay.
	SourceFiles◦2 ← (dp0 file: 'st80changes.v00') readonly. "Avoid remap problem for now"
	self restore
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
backup | |
	 "back up smalltalk on ivy and resume"
	ivy open.
	ivy delete: 'small.boot'.
	ivy store: 'small.boot'.
	ivy close
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
quitThen: str | rem rest |
	rem ← (dp0 file: 'rem.cm') "quit, then have OS execute str" readonly.
	rest ← rem next: rem length.
	rem readwrite.
	rem reset.
	rem append: str.
	rem cr.
	rem append: rest.
	rem close.
	self quit
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
Swat | |<primitive: 75>
	
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
quit | |
	self quitFrom: self "yup"
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
st80FileInit: version | |
	 " user st80FileInit: 'ST80dec25'. "
	(SourceFiles ◦ 1) file rename: version+'.sources'.
	(SourceFiles ◦ 1) readonly.
	(SourceFiles ◦ 2) file rename: version+'.changes'.
	(SourceFiles ◦ 2) readonly
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
st80FileInit | |
	 " user st80FileInit. "
	dp0 release.
	SourceFiles ← Vector new: 4.
	SourceFiles ◦ 1 ← dp0 file: 'ST80Sources.v00'.
	(SourceFiles ◦ 1) readonly.
	SourceFiles ◦ 2 ← dp0 file: 'ST80Changes.v00'.
	(SourceFiles ◦ 2) readonly
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
snapshotPrimitive | |<primitive: 83>
	 "write the OT and Data of this Smalltalk out"
	user croak
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
InLd: fileid | |<primitive: 75>
	 "write out the core image, then load in OS"
	user notify: 'file problem'
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
quitThen: s continue: r | t3 |
	s
	  ifTrue:
		[(t3 ← dp0 oldFile: 'rem.cm.' "something for O.S. to do") settoend.
		t3 append: s.
		t3 append: '; '.
		t3 append: (r
		  ifTrue: ['Resume.~ small.boot']
		  ifFalse: ['Quit.~; Resume.~ small.boot']).
		t3 cr.
		t3 flush].
	self quit
! !
!UserView methodsFor: 'System quit/resume' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	self snapshotPrimitive.
	self mapDisplay.
	self restore
! !
!UserView methodsFor: 'Time' stamp: ''!
timeWordsInto: s | |<primitive: 81>
	 "s length=4!"
	user croak
! !
!UserView methodsFor: 'Time' stamp: ''!
rawtotalsecs | |
	^self rawtotalsecs: self timewords
! !
!UserView methodsFor: 'Time' stamp: ''!
totalsecs | |
	^self totalsecs: self timewords
! !
!UserView methodsFor: 'Time' stamp: ''!
timewords | s |
	s ← String new: 4 "seconds (in GMT) since Jan 1 1901: as a String".
	self timeWordsInto: s.
	^s
! !
!UserView methodsFor: 'Time' stamp: ''!
now | |
	^self dateAndTime: self timewords
! !
!UserView methodsFor: 'Time' stamp: ''!
time | |
	^self now ◦ 2
! !
!UserView methodsFor: 'Time' stamp: ''!
totalsecs: secs | |
	^self convertTime: (self rawtotalsecs: secs) "convert from GMT to local and correct for Daylight Savings" returnSecs: true
! !
!UserView methodsFor: 'Time' stamp: ''!
rawtotalsecs: secs | s |
	s ← Natural new: 4 "secs is a String of 4 characters representing seconds (in GMT) since Jan 1 1901.
	copy (in reverse order) to a Natural string, then return a LargeInteger".
	s ◦ 1 ← secs ◦ 4.
	s ◦ 2 ← secs ◦ 3.
	s ◦ 3 ← secs ◦ 2.
	s ◦ 4 ← secs ◦ 1.
	^LargeInteger new bytes: s neg: false
! !
!UserView methodsFor: 'Time' stamp: ''!
dateAndTime: secs | |
	^self convertTime: (self rawtotalsecs: secs) "secs is a String of 4 characters representing seconds (in GMT) since Jan 1 1901.
	convert it to a LargeInteger (rawtotalsecs:), then return a Vector (Date, Time),
	which is corrected for local time zone and daylight savings" returnSecs: false
! !
!UserView methodsFor: 'Time' stamp: ''!
convertTime: s returnSecs: format | d dd t dfirst dlast m570 m571 |
	 "s is total seconds from midnight Jan 1 1901 GMT (Greenwich mean time).
	see maxc <AltoDocs>AltoTime.Press for details" "time zone specific parameters"
	NoteTaker
	  ifTrue:
		[m570 ← 16505.
		m571 ← 305 "Zap! Youre in Palo Alto!"]
	  ifFalse:
		[m570 ← mem ◦ 376.
		m571 ← mem ◦ 377] "adjust for time zone".
	s ← s + ((m570 ≥ 0
			  ifTrue: [¬1]
			  ifFalse: [ "west" "east"
				1]) * (3600 * (m570 bits: (1 to: 4) "hours") + (60 * (m571 bits: (1 to: 6) "additonal minutes")))).
	t ← s intdiv: 86400 "current day (in local standard time)".
	d ← Date new fromDays: t ◦ 1.
	format
	  ifFalse: [t ← Time new fromSeconds: t ◦ 2] "check for DST. correct DST parameters for nonleap years and
	round to previous Sunday if necessary" "day of the year on or before which DST takes effect".
	dfirst ← m570 land: 511 "bits: (7 to: 15)".
	(dfirst = 366
	  ifTrue: [false]
	  ifFalse:
		[ "DST not in effect"
		(dd ← d day) ≥ (dfirst ← dfirst + d leap - 1)
		  ifTrue:
			[dlast ← (m571 land: 511) "day of the year on or before which DST ends" "bits: (7 to: 15)" + d leap - 1.
			dd < dlast "if false, definitely after" and: [dd < ((Date new day: dlast year: d year) previous: 6) day]]
		  ifFalse: [ "possibly earlier than or at beginning of range"
			dd ≥ ((Date new day: dfirst year: d year) previous: 6) day]])
	  ifTrue:
		[format
		  ifTrue: [ "daylight savings time in effect. add an hour"
			s ← s + 3600]
		  ifFalse:
			[t hours = 23
			  ifTrue:
				[d ← d + 1.
				t hours: 0]
			  ifFalse: [t hours: t hours + 1]]].
	format
	  ifTrue: [^s].
	^{d , t}
! !
!UserView methodsFor: 'Time' stamp: ''!
ticks | |
	 "Return the 38.08-millisecond interval timer"
	^mem ◦ 280
! !
!UserView methodsFor: 'Time' stamp: ''!
time⦂ expr | t |
	t ← self ticks.
	expr eval.
	^self ticks - t
! !
!UserView methodsFor: 'Time' stamp: ''!
today | |
	^self now ◦ 1
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
restore | w |
	screenrect clear.
	projectWindow ≡ nil
	  ifFalse: [projectWindow putTitle].
	disp outline.
	(sched length to: 1 by: ¬1) do: [:w | (sched ◦ w) show]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
restart | i |
	Events ≡ nil
	  ifTrue:
		["Events ← EventQueue init. Top init3.  initialize Event queue and Time interrupt"].
	NormalCursor topage1.
	self restart⦂ [(user run)]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
schedule: window | |
	sched ≡ nil
	  ifTrue: [sched ← window inVector]
	  ifFalse: [sched ← window inVector concat: sched]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
leaveTop | |
	 "leave the top window if there is one"
	sched length = 0
	  ifFalse: [(sched ◦ 1) leave]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
run | |
	self run: false
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
restart⦂ code | u |
	u ← code cleancopy.
	u sender← nil.
	thisContext sender releaseFully.
	thisContext sender← nil.
	code ← nil "release caller chain".
	MessageDict new freeMethods "release held code".
	disp frame flash.
	[true] whileTrueDo: [u eval]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
restartup: window | |
	NoteTaker
	  ifTrue: [ "Equivalent to schedule new window, restart, and redbug in window, except firsttime is already done."
		self schedule: window]
	  ifFalse:
		[thisContext sender releaseFully.
		thisContext sender← nil.
		NormalCursor topage1.
		self schedule: window.
		thisContext tempframe all← nil.
		self run: true]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
topWindow | |
	^sched ◦ 1
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
promote: window | |
	sched promote: window
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
run: topFlag | i w forward |
	 "topFlag means sched◦1 already is awake"
	forward ← (topFlag
			  ifTrue:
				[w ← sched ◦ 1.
				[w eachtime] whileTrueDo: [].
				w lasttime]
			  ifFalse: [true]).
	[true] whileTrueDo:
		[i ← 0.
		[(i ← i + 1) > sched length
		  ifFalse:
			[w ← (forward
					  ifTrue: [sched ◦ i]
					  ifFalse: [sched ◦ (sched length + 1 - i)]).
			w firsttime]] whileFalseDo:  [].
		i > sched length
		  ifTrue:
			[ "check for bug in empty space"
			user yellowbug
			  ifTrue: [self bugScreenMenu]]
		  ifFalse:
			[sched promote: w.
			[w eachtime] whileTrueDo: [].
			forward ← w lasttime]]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
unschedule: window | t |
	0 < (t ← sched find: window)
	  ifTrue: [sched ← sched ◦ (1 to: t - 1) concat: sched ◦ (t + 1 to: sched length)]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
scheduleOnBottom: window | |
	sched ≡ nil
	  ifTrue: [sched ← window asVector]
	  ifFalse: [sched ← sched concat: window asVector]
! !
!UserView methodsFor: 'Window Scheduling' stamp: ''!
sched | |
	^sched
! !
