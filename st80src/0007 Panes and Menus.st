ListPane subclass: #ClassPane
	instanceVariableNames: 'systemPane organizationPane'
	classVariableNames: ''
	category: 'Panes and Menus'!
!ClassPane methodsFor: 'Browser protocol' stamp: ''!
noCode | |
	selection = 0
	  ifTrue: [^systemPane noCode].
	^''
! !
!ClassPane methodsFor: 'Browser protocol' stamp: ''!
dirty | |
	^organizationPane dirty
! !
!ClassPane methodsFor: 'Browser protocol' stamp: ''!
compile: parag | |
	systemPane compile: parag
! !
!ClassPane methodsFor: 'Initialization' stamp: ''!
from: t1 to: t2 | |
	systemPane ← t1.
	organizationPane ← t2
! !
!ClassPane methodsFor: 'Initialization' stamp: ''!
classInit | |
	editmenu ← Menu new string: 'filout
print
forget'
! !
!ClassPane methodsFor: 'ListPane protocol' stamp: ''!
selected | |
	organizationPane class: Smalltalk ◦ (list ◦ selection) "My selection just changed.  Tell organizationPane to display the categories of my newly selected Class."
! !
!ClassPane methodsFor: 'ListPane protocol' stamp: ''!
deselected | |
	organizationPane class: nil "I just lost my selection.  Tell organizationPane to display nothing."
! !
!ClassPane methodsFor: 'Window protocol' stamp: ''!
close | |
	systemPane ← nil.
	super close
! !
!ClassPane methodsFor: 'Window protocol' stamp: ''!
yellowbug | t1 |
	selection = 0 "If there is a selection, let the user choose a command from the menu."
	  ifTrue: [window flash]
	  ifFalse:
		[(t1 ← editmenu bug) = 1
		  ifTrue: [(Smalltalk ◦ (list ◦ selection) "filout") filout]
		  ifFalse:
			[t1 = 2
			  ifTrue: [(Smalltalk ◦ (list ◦ selection) "print") printout]
			  ifFalse:
				[t1 = 3
				  ifTrue: [systemPane forget: list ◦ selection "forget"]]]]
! !
Window subclass: #CodePane
	instanceVariableNames: 'pared class selector selectorPane scrollBar'
	classVariableNames: ''
	category: 'Panes and Menus'!
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
compile: parag in: defaultClass under: category | |
	^LADCompiler new compile: parag in: (class ≡ nil
	  ifTrue: [defaultClass]
	  ifFalse: [class]) under: category notifying: self
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
execute: parseStream in: context to: receiver | cls |
	context
		ifTrue: [cls← context mclass. receiver← context receiver]
		ifFalse: [cls← receiver class].
	^LADCompiler new evaluate: parseStream in: context to: receiver notifying: self
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
selectRange: r | |
	pared selectRange: r.
	pared selectAndScroll
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
dirty | |
	pared formerly
	  ifTrue: [^frame].
	^false
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
contents | |
	^pared contents
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
notify: errorString at: position in: stream | |
	pared fintype.
	pared selectRange: (position to: position).
	pared replace: ('➲' + errorString + '➲.') asParagraph.
	pared selectAndScroll.
	^false
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
compile: parag | |
	 "as my own selectorPane"
	^self compile: parag in: class under: 'As yet unclassified'
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
interactive | |
	^true
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
execute: parseStream for: codePane | |
	 "as my own selectorPane"
	^self execute: parseStream in: false to: nil
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
compile: parag in: defaultClass under: category | |
	^(self compiler: defaultClass) new compile: parag in: (class ≡ nil
	  ifTrue: [defaultClass]
	  ifFalse: [class]) under: category notifying: self
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
oldContents | |
	^pared formerly
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
reflects: selection | |
	 "am I trying to show the code of selectorPaneⓢ selection?"
	^class ≡ nil and: [selection > 0]
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
formerly: oldpara | |
	 "should not be called before 'showing:'"
	pared formerly: oldpara
! !
!CodePane methodsFor: 'Initialization' stamp: ''!
classInit | |
	editmenu ← Menu new string: 'again
copy
cut
paste
doit
compile
undo
cancel
align'
! !
!CodePane methodsFor: 'Initialization' stamp: ''!
from: t1 | |
	selectorPane ← t1
! !
!CodePane methodsFor: 'Initialization' stamp: ''!
showing: paragraph | |
	pared ← TextImage new para: paragraph asParagraph frame: nil.
	pared formerly: false.
	pared fixframe: frame.
	self windowenter.
	scrollBar ← (scrollBar ≡ nil
			  ifTrue: [ScrollBar new]
			  ifFalse: [scrollBar]) on: frame from: pared
! !
!CodePane methodsFor: 'Initialization' stamp: ''!
init | |
	
! !
!CodePane methodsFor: 'Initialization' stamp: ''!
class: t1 selector: t2 para: para | |
	class ← t1.
	selector ← t2
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
doit | s val t4 |
	scrollBar hide "do automatic selection (ESC) on empty selections".
	(s ← pared selectRange) empty
	  ifTrue:
		[pared unselect.
		pared fintype.
		pared complement.
		s ← pared selectRange].
	val ← selectorPane execute: pared selectionAsStream for: self.
	(val ≡ nil or: [s ≠ pared selectRange])
	  ifFalse:
		[ "result is nil or error occurred" "automatically paste result"
		s ← s stop + 1.
		(t4 ← (String new: 100) asStream) space.
		t4 print: val.
		pared Scrap← t4 contents asParagraph.
		pared selectRange: (s to: s).
		pared paste].
	scrollBar show
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
outline | |
	frame outline: 1
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
windowleave | |
	pared ≡ nil
	  ifFalse: [pared leave]
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
windowenter | |
	self outline.
	pared enter
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
close | |
	pared unselect.
	selectorPane ← pared ← nil.
	scrollBar close
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
outside | |
	^scrollBar startup
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
enter | |
	scrollBar show
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
redbug | |
	^pared selecting
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
picked | |
	^frame has: user mp
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
leave | |
	scrollBar hide
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
keyset | |
	^pared keyset
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
kbd | |
	pared kbd
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
hardcopy: pf | t2 |
	selectorPane ≡ self "if this is just part of a CodeWindow, then print entire Paragraph with no frame.
	unfortunately, the test for this is a kludge. otherwise, print clipped"
	  ifTrue:
		[(t2 ← PressPrinter init) press: pf.
		t2 print: pared contents]
	  ifFalse:
		[frame hardcopy: pf thickness: 1.
		pared hardcopy: pf]
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
eachtime | |
	user kbck
	  ifTrue: [^self kbd].
	(frame has: user mp)
	  ifTrue:
		[user anybug
		  ifTrue:
			[user redbug
			  ifTrue: [^self redbug].
			user yellowbug
			  ifTrue: [^self yellowbug].
			user bluebug
			  ifTrue: [^false]]
		  ifFalse:
			[user anykeys
			  ifTrue: [^self keyset]]]
	  ifFalse: [^self outside]
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
frame← t1 | |
	frame ← t1.
	pared ≡ nil "Change my frame and that of my pared (if any)."
	  ifFalse:
		[pared frame← frame.
		scrollBar on: frame from: pared]
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
yellowbug | t1 |
	(t1 ← editmenu bug) = 5
	  ifTrue: [self doit]
	  ifFalse:
		[t1 = 1
		  ifTrue: [scrollBar hidewhile⦂ [(pared again)]]
		  ifFalse:
			[t1 = 2
			  ifTrue: [pared copyselection]
			  ifFalse:
				[t1 = 3
				  ifTrue: [pared cut]
				  ifFalse:
					[t1 = 4
					  ifTrue: [pared paste]
					  ifFalse:
						[t1 = 6
						  ifTrue:
							[pared formerly
							  ifTrue: [scrollBar hidewhile⦂ [
									((selectorPane compile: pared contents)
									  ifTrue: [pared formerly: false])]]
							  ifFalse: [frame flash]]
						  ifFalse:
							[t1 = 7
							  ifTrue: [pared undo]
							  ifFalse:
								[t1 = 8
								  ifTrue:
									[pared formerly
									  ifTrue:
										[pared Deletion← pared contents.
										scrollBar hidewhile⦂ [(self showing: pared formerly)]]
									  ifFalse: [frame flash]]
								  ifFalse:
									[t1 = 9
									  ifTrue: [pared realign]]]]]]]]]
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
show | |
	frame outline.
	pared show
! !
Object subclass: #Menu
	instanceVariableNames: 'str text thisline frame'
	classVariableNames: ''
	category: 'Panes and Menus'!
!Menu methodsFor: 'Initialization' stamp: ''!
rescan | |
	 " | each. Menu allInstances notNil transform⦂ each to⦂ each rescan."
	self string: str "rescan (for new fonts, lineheight)"
! !
!Menu methodsFor: 'Initialization' stamp: ''!
string: t1 | i pt tpara |
	str ← t1.
	str last ≠ 13
	  ifTrue: [str ← str + '
'].
	text ← Textframe new para: (tpara ← str asParagraph) frame: (Rectangle new origin: (pt ← 0 ⌾ 0) corner: 1000 ⌾ 1000).
	pt ← text maxx: str length + 1.
	text frame growto: pt + (4 ⌾ 0).
	text measureall.
	tpara center.
	frame ← text frame inset: ¬2 ⌾ ¬2.
	thisline ← Rectangle new origin: text frame origin corner: text frame corner x ⌾ text lineheight
! !
!Menu methodsFor: 'Initialization' stamp: ''!
stringFromVector: v | s |
	s ← Stream default "DW classInit".
	v do:
		[:v | s append: v.
		s cr].
	self string: s contents
! !
!Menu methodsFor: 'Internal' stamp: ''!
movingsetup | pt bits |
	pt ← user mp - thisline center "center prev item on mouse".
	text frame moveby: pt.
	thisline moveby: pt.
	frame moveby: pt.
	bits ← frame bitsIntoString "save background".
	frame clear: black.
	text displayall.
	^bits
! !
!Menu methodsFor: 'Internal' stamp: ''!
bugit | pt bits rect |
	user nobug
	  ifTrue: [^0].
	 "accidental bug returns 0"
	thisline comp.
	[true] whileTrueDo:
		[(text frame has: (pt ← user mp))
		  ifTrue:
			[user anybug
			  ifTrue:
				[(thisline has: pt)
				  ifFalse:
					[text charofpoint: pt andrect⦂ [:rect | rect].
					pt ← rect origin.
					thisline comp "selection follows mouse".
					thisline moveto: text frame origin x ⌾ pt y.
					thisline comp]]
			  ifFalse: [^1 + (thisline origin y - text frame origin y / text lineheight) "return index"]]
		  ifFalse:
			[thisline comp "he left the menu".
			[text frame has: user mp] whileFalseDo: 
				[user nobug
				  ifTrue: [^0]].
			thisline comp "return 0 for abort"]] "he came back"
! !
!Menu methodsFor: 'User interactions' stamp: ''!
bug | index bits |
	bits ← self movingsetup "set up and save background".
	index ← self bugit "get the index".
	frame bitsFromString: bits "restore background".
	^index "return index"
! !
!Menu methodsFor: 'User interactions' stamp: ''!
wbug | index bits |
	bits ← self movingsetup "save background, display menu" "wait until a mouse button is down".
	[user anybug] whileFalseDo:  [ "get selection (possibly 0)"
		].
	index ← self bugit "restore background".
	frame bitsFromString: bits.
	^index
! !
!Menu methodsFor: 'User interactions' stamp: ''!
has: pt | |
	^text frame has: pt
! !
!Menu methodsFor: 'User interactions' stamp: ''!
clear | |
	frame clear
! !
!Menu methodsFor: 'User interactions' stamp: ''!
frame | |
	^frame
! !
!Menu methodsFor: 'User interactions' stamp: ''!
rebug | |
	user waitbug "wait for button down again".
	^self bug "bugcursor showwhile⦂"
! !
!Menu methodsFor: 'User interactions' stamp: ''!
zbug | index bits |
	bits ← self movingsetup.
	[(index ← self bugit) = 0] whileTrueDo: [].
	frame bitsFromString: bits.
	^index
! !
!Menu methodsFor: 'User interactions' stamp: ''!
moveto: pt | |
	self clear.
	frame moveto: pt.
	text frame moveto: pt + 2.
	thisline moveto: pt + 2
! !
!Menu methodsFor: 'User interactions' stamp: ''!
show | |
	frame clear: black.
	text displayall
! !
!Menu methodsFor: 'User interactions' stamp: ''!
fbug | index |
	index ← self bugit "for fixed menus" "get the index".
	^index "return index"
! !
ListPane subclass: #OrganizationPane
	instanceVariableNames: 'classPane selectorPane class'
	classVariableNames: ''
	category: 'Panes and Menus'!
!OrganizationPane methodsFor: 'As yet unclassified' stamp: ''!
syntaxChoice | |
	 "kludge for new choosing syntax"
	class ≡ nil
	  ifFalse:
		[class usesNewSyntax
		  ifTrue: [^class new]]
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
execute: parag | |
	^class ⓢ parag
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
noCode | |
	class ≡ nil
	  ifTrue: [^classPane noCode].
	selection = 0
	  ifTrue: [^''].
	selection = 1
	  ifTrue: [^class definition].
	selection = 2
	  ifTrue: [^class organization].
	^'Message name and Arguments  | Temporary variables |  "short comment"
	"long comment if necessary"
	Smalltalk
	Statements'
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
compile: parag | sel cat |
	(class ≡ nil or: [selection = 1])
	  ifTrue: [classPane compile: parag]
	  ifFalse:
		[ "new definition"
		selection = 2
		  ifTrue:
			[class organization fromParagraph: parag.
			self class: class]
		  ifFalse:
			[ "new organization"
			cat ← (selection = 0
					  ifTrue: ['As yet unclassified']
					  ifFalse: [list ◦ selection]).
			(sel ← selectorPane compile: parag in: class under: cat)
			  ifTrue:
				[self revise: (self listFor: class) with: cat.
				selection ≠ 0
				  ifTrue: [selectorPane revise: (class organization category: cat) with: sel]]
			  ifFalse: [^false]]]
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
code: selector | |
	^class code: selector
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
forget: selector | cat |
	class derstands: selector.
	cat ← list ◦ selection.
	self revise: (self listFor: class) with: cat.
	selection > 0
	  ifTrue: [selectorPane revise: (class organization category: cat) with: selector]
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
dirty | |
	^selectorPane dirty
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
spawn: selector with: parag formerly: oldparag | |
	selectorPane compselection.
	selectorPane select: 0.
	class edit: selector para: parag formerly: oldparag
! !
!OrganizationPane methodsFor: 'Initialization' stamp: ''!
class: t1 | |
	class ← t1.
	self of: (self listFor: class)
! !
!OrganizationPane methodsFor: 'Initialization' stamp: ''!
classInit | |
	editmenu ← Menu new string: 'filout
print'
! !
!OrganizationPane methodsFor: 'Initialization' stamp: ''!
from: t1 to: t2 | |
	classPane ← t1.
	selectorPane ← t2
! !
!OrganizationPane methodsFor: 'Initialization' stamp: ''!
listFor: t1 | |
	class ← t1.
	^class ≡ nil
	  ifTrue: [Vector new: 0]
	  ifFalse: [#(ClassDefinition ClassOrganization ) concat: class organization categories]
! !
!OrganizationPane methodsFor: 'ListPane protocol' stamp: ''!
selected | |
	selectorPane of: (selection ≤ 2
	  ifTrue: [Vector new: 0]
	  ifFalse: [class organization category: list ◦ selection])
! !
!OrganizationPane methodsFor: 'ListPane protocol' stamp: ''!
deselected | |
	selectorPane of: (Vector new: 0) "I just lost my selection.  Tell selectorPane to display nothing."
! !
!OrganizationPane methodsFor: 'Window protocol' stamp: ''!
close | |
	classPane ← nil.
	super close
! !
!OrganizationPane methodsFor: 'Window protocol' stamp: ''!
yellowbug | t1 |
	selection ≤ 1 "If there is a selection, let the user choose a command from the menu."
	  ifTrue: [window flash]
	  ifFalse:
		[ "Can't filout or print definition by itself"
		(t1 ← editmenu bug) = 1
		  ifTrue:
			[selection = 2 "filout the selected category"
			  ifTrue: [class filoutOrganization]
			  ifFalse: [class filoutCategory: list ◦ selection]]
		  ifFalse:
			[t1 = 2
			  ifTrue:
				[selection = 2 "print the selected category"
				  ifTrue: [window flash]
				  ifFalse: [ "Can't print organization"
					class printoutCategory: list ◦ selection]]]]
! !
Object subclass: #ScrollBar
	instanceVariableNames: 'rect bitstr owner position'
	classVariableNames: ''
	category: 'Panes and Menus'!
!ScrollBar methodsFor: 'Image' stamp: ''!
boxPosition← f | |
	position moveto: rect origin + (9 ⌾ (4 + ((f < 0.0
	  ifTrue: [0.0]
	  ifFalse:
		[f > 1.0
		  ifTrue: [1.0]
		  ifFalse: [f]]) * (rect height - 16)) asInteger ))
! !
!ScrollBar methodsFor: 'Image' stamp: ''!
reposition⦂ expr | |
	self reshow⦂ [
		(expr eval.
		self boxPosition← owner scrollPos)]
! !
!ScrollBar methodsFor: 'Image' stamp: ''!
reshow⦂ expr | r |
	r ← position inset: ¬2.
	expr eval.
	r clear: white.
	position outline
! !
!ScrollBar methodsFor: 'Image' stamp: ''!
hidewhile⦂ expr | v |
	self hide.
	v ← expr eval.
	self show.
	^v
! !
!ScrollBar methodsFor: 'Image' stamp: ''!
hide | |
	 "restore background"
	bitstr ≡ nil
	  ifTrue: [user notify: 'Attempt to hide unshown scrollbar']
	  ifFalse:
		[rect bitsFromString: bitstr.
		bitstr ← nil]
! !
!ScrollBar methodsFor: 'Image' stamp: ''!
show | |
	 "Save background and turn gray"
	bitstr ← rect bitsIntoString.
	rect clear: black.
	(rect inset: 2 ⌾ 2 and: [1 ⌾ 2]) clear: white.
	position outline
! !
!ScrollBar methodsFor: 'Initialization' stamp: ''!
on: frame from: t2 at: f | |
	owner ← t2.
	rect ← Rectangle new origin: frame origin - (32 ⌾ 2) extent: 32 ⌾ (frame height + 4).
	position ← Rectangle new origin: rect origin + (9 ⌾ 4) extent: 16 ⌾ 8.
	self boxPosition← f
! !
!ScrollBar methodsFor: 'Initialization' stamp: ''!
classInit | |
	UpCursor ← Cursor new fromtext: '
1000000000000000
1100000000000000
1110000000000000
1111000000000000
1111100000000000
1111110000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000'.
	DownCursor ← Cursor new fromtext: '
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1100000000000000
1111110000000000
1111100000000000
1111000000000000
1110000000000000
1100000000000000
1000000000000000'.
	JumpCursor ← Cursor new fromtext: '
0111000000000000
1111100000000000
1111100000000000
0111000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000' offset: 2 ⌾ 1
! !
!ScrollBar methodsFor: 'Initialization' stamp: ''!
useBitmap | |
	bitstr ≡ nil
	  ifFalse: [bitstr ← bitstr asBitmap] " | x. ScrollBar allInstances transform⦂ x to⦂ x useBitmap. "
! !
!ScrollBar methodsFor: 'Initialization' stamp: ''!
on: f from: o | |
	self on: f from: o at: o scrollPos
! !
!ScrollBar methodsFor: 'Scheduling' stamp: ''!
firsttime | |
	^rect has: user mp
! !
!ScrollBar methodsFor: 'Scheduling' stamp: ''!
close | |
	owner ← nil
! !
!ScrollBar methodsFor: 'Scheduling' stamp: ''!
slide: p | bug |
	(position has: p)
	  ifTrue:
		[JumpCursor showwhile⦂ [
			(bug ← false.
			[(position has: user mp) and: [bug ≡ false]] whileTrueDo:
				[user redbug
				  ifTrue:
					[bug ← true.
					[user redbug] whileTrueDo: [self reshow⦂ [(position moveto: position origin x ⌾ ((user mp y max: rect origin y + 4) min: rect corner y - 12))]]]])].
		^bug].
	^false
! !
!ScrollBar methodsFor: 'Scheduling' stamp: ''!
lasttime | |
	
! !
!ScrollBar methodsFor: 'Scheduling' stamp: ''!
eachtime | p cx r |
	 "This needs to be restructured"
	(rect has: (p ← user mp))
	  ifTrue:
		[cx ← rect center x - 2.
		p x < cx
		  ifTrue:
			[r ← Rectangle new origin: rect origin corner: cx ⌾ rect maxY.
			DownCursor showwhile⦂ [([r has: (p ← user mp)] whileTrueDo:
					[(self slide: p)
					  ifTrue: [owner scrollTo: (position minY - rect minY - 4) asFloat / (rect height - 12)]
					  ifFalse:
						[user redbug
						  ifTrue: [self reposition⦂ [(owner scrollUp: rect origin y - p y)]]]])]]
		  ifFalse:
			[r ← Rectangle new origin: cx ⌾ rect minY corner: rect corner.
			UpCursor showwhile⦂ [([r has: (p ← user mp)] whileTrueDo:
					[(self slide: p)
					  ifTrue: [owner scrollTo: (position minY - rect minY - 4) asFloat / (rect height - 12)]
					  ifFalse:
						[user redbug
						  ifTrue: [self reposition⦂ [(owner scrollUp: p y - rect origin y)]]]])]]]
	  ifFalse: [^false]
! !
ListPane subclass: #SelectorPane
	instanceVariableNames: 'organizationPane codePane'
	classVariableNames: ''
	category: 'Panes and Menus'!
!SelectorPane methodsFor: 'Browser protocol' stamp: ''!
dirty | |
	^codePane dirty
! !
!SelectorPane methodsFor: 'Browser protocol' stamp: ''!
compile: parag | |
	^organizationPane compile: parag
! !
!SelectorPane methodsFor: 'Browser protocol' stamp: ''!
execute: parseStream for: t2 | |
	codePane ← t2.
	^codePane execute: parseStream in: false to: organizationPane syntaxChoice
! !
!SelectorPane methodsFor: 'Browser protocol' stamp: ''!
compile: parag in: class under: heading | |
	^codePane compile: parag in: class under: heading
! !
!SelectorPane methodsFor: 'Initialization' stamp: ''!
from: t1 to: t2 | |
	organizationPane ← t1.
	codePane ← t2
! !
!SelectorPane methodsFor: 'Initialization' stamp: ''!
classInit | |
	editmenu ← Menu new string: 'spawn
forget'
! !
!SelectorPane methodsFor: 'ListPane protocol' stamp: ''!
selected | |
	codePane showing: (organizationPane code: list ◦ selection)
! !
!SelectorPane methodsFor: 'ListPane protocol' stamp: ''!
deselected | |
	codePane showing: organizationPane noCode
! !
!SelectorPane methodsFor: 'Window protocol' stamp: ''!
close | |
	organizationPane ← nil.
	super close
! !
!SelectorPane methodsFor: 'Window protocol' stamp: ''!
yellowbug | t1 |
	selection = 0
	  ifTrue: [window flash]
	  ifFalse: [scrollBar hidewhile⦂ [
			((t1 ← editmenu bug) = 1
			  ifTrue: [organizationPane spawn: list ◦ selection with: codePane contents formerly: codePane oldContents]
			  ifFalse:
				[t1 = 2
				  ifTrue: [organizationPane forget: list ◦ selection]])]]
! !
ListPane subclass: #SystemPane
	instanceVariableNames: 'mySysOrgVersion classPane'
	classVariableNames: ''
	category: 'Panes and Menus'!
!SystemPane methodsFor: 'Browser protocol' stamp: ''!
forget: className | t2 |
	"user notify: 'Class ' + className + ' will disappear if you proceed...'."
	(t2 ← Smalltalk ◦ className) noChanges.
	t2 obsolete.
	Smalltalk delete: className.
	SystemOrganization delete: className.
	AllClassNames ← AllClassNames delete: className.
	classPane revise: self classes with: className
! !
!SystemPane methodsFor: 'Browser protocol' stamp: ''!
noCode | |
	selection = 0
	  ifTrue: [^''].
	selection = 2
	  ifTrue: [^SystemOrganization].
	^'Class new title: #NameOfClass
	subclassof: Object
	fields: ''names of fields''
	declare: ''names of class variables''' copy
! !
!SystemPane methodsFor: 'Browser protocol' stamp: ''!
classes | |
	 "return a Vector of the classes in my selected category"
	selection = 1
	  ifTrue: [^user classNames].
	selection ≤ 2
	  ifTrue: [^Vector new: 0].
	^SystemOrganization category: list ◦ selection
! !
!SystemPane methodsFor: 'Browser protocol' stamp: ''!
dirty | |
	^classPane dirty
! !
!SystemPane methodsFor: 'Browser protocol' stamp: ''!
compile: parag | class cat className |
	selection = 2
	  ifTrue:
		[SystemOrganization fromParagraph: parag.
		self update]
	  ifFalse:
		[ "new organization"
		cat ← (selection ≤ 1
				  ifTrue: [false]
				  ifFalse: [list ◦ selection]).
		class ← nil ⓢ parag.
		(class Is: Class)
		  ifTrue:
			[className ← class title unique.
			cat
			  ifTrue: [SystemOrganization classify: className under: cat].
			mySysOrgVersion ≡ user classNames
			  ifTrue:
				[selection > 0
				  ifTrue: [classPane of: (cat
					  ifTrue: [SystemOrganization category: cat]
					  ifFalse: [user classNames])]]
			  ifFalse: [self update]]]
! !
!SystemPane methodsFor: 'Initialization' stamp: ''!
update | |
	self of: (#(AllClasses SystemOrganization ) concat: SystemOrganization categories).
	mySysOrgVersion ← user classNames
! !
!SystemPane methodsFor: 'Initialization' stamp: ''!
classInit | |
	sysmenu ← Menu new string: 'filout
print'
! !
!SystemPane methodsFor: 'Initialization' stamp: ''!
to: t1 | |
	classPane ← t1
! !
!SystemPane methodsFor: 'ListPane protocol' stamp: ''!
deselected | |
	classPane of: (Vector new: 0)
! !
!SystemPane methodsFor: 'ListPane protocol' stamp: ''!
selected | |
	classPane of: self classes
! !
!SystemPane methodsFor: 'Window protocol' stamp: ''!
leave | |
	 "I am up to date"
	mySysOrgVersion ← user classNames.
	super leave
! !
!SystemPane methodsFor: 'Window protocol' stamp: ''!
enter | |
	 "be sure I am up to date"
	mySysOrgVersion ≡ user classNames
	  ifTrue: [super enter]
	  ifFalse:
		[window outline.
		self update.
		super enter]
! !
!SystemPane methodsFor: 'Window protocol' stamp: ''!
yellowbug | t1 |
	selection < 3
	  ifTrue: [window flash]
	  ifFalse: [scrollBar hidewhile⦂ [
			((t1 ← sysmenu bug) = 1
			  ifTrue: [SystemOrganization filoutCategory: list ◦ selection]
			  ifFalse:
				[t1 = 2
				  ifTrue: [SystemOrganization printCategory: list ◦ selection]])]]
! !
