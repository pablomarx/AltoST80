PanedWindow subclass: #BrowseWindow
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Windows'!
!BrowseWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	stdTemplates ← {(0 ⌾ 0 rect: 10 ⌾ 14) , (10 ⌾ 0 rect: 18 ⌾ 14) , (18 ⌾ 0 rect: 28 ⌾ 14) , (28 ⌾ 0 rect: 36 ⌾ 14) , (0 ⌾ 14 rect: 36 ⌾ 36)}
! !
!BrowseWindow methodsFor: 'Initialization' stamp: ''!
default | systemPane classPane orgPane selectorPane codePane |
	 "Let the user draw a five-paned window to browse through classes."
	systemPane ← SystemPane new "Create the panes.".
	classPane ← ClassPane new.
	orgPane ← OrganizationPane new.
	selectorPane ← SelectorPane new.
	codePane ← CodePane new "Acquire them.".
	self title: 'Classes' with: {systemPane , classPane , orgPane , selectorPane , codePane} at: stdTemplates.
	self newframe.
	self show "Interconnect them.".
	systemPane to: classPane.
	classPane from: systemPane to: orgPane.
	orgPane from: classPane to: selectorPane.
	selectorPane from: orgPane to: codePane.
	codePane from: selectorPane "Display them.".
	systemPane update
! !
PanedWindow subclass: #CodeWindow
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Windows'!
!CodeWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	stdTemplates ← (0 ⌾ 0 rect: 36 ⌾ 36) inVector
! !
!CodeWindow methodsFor: 'Initialization' stamp: ''!
file: file | filePane |
	filePane ← FilePane new file: file.
	self title: file name with: filePane inVector at: stdTemplates.
	self newframe.
	self show.
	filePane showing: file contents asParagraph.
	filePane from: filePane
! !
!CodeWindow methodsFor: 'Initialization' stamp: ''!
editTitle | |
	titleframe window flash
! !
!CodeWindow methodsFor: 'Initialization' stamp: ''!
hardcopy: p | pane |
	panes do: [:pane | pane hardcopy: p]
! !
!CodeWindow methodsFor: 'Initialization' stamp: ''!
class: class selector: selector para: para formerly: oldpara | codePane |
	codePane ← CodePane new class: class selector: selector para: nil.
	self title: class title + ' ' + selector with: codePane inVector at: stdTemplates.
	self newframe.
	self show.
	codePane showing: para.
	codePane formerly: oldpara.
	codePane from: codePane
! !
PanedWindow subclass: #InspectWindow
	instanceVariableNames: 'variables'
	classVariableNames: ''
	category: 'Windows'!
!InspectWindow methodsFor: 'Initialization' stamp: ''!
of: object | instanceVarPane instanceValuePane safeVec n |
	instanceVarPane ← VariablePane new.
	instanceValuePane ← CodePane new.
	self title: object class title with: {instanceVarPane , instanceValuePane} at: stdTemplates.
	self newframe.
	self show.
	instanceVarPane to: instanceValuePane.
	instanceValuePane from: instanceVarPane.
	safeVec ← Vector new: 2.
	safeVec all← object.
	instanceVarPane names: (#(self ) concat: object fields) values: safeVec wrt: false
! !
!InspectWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	stdTemplates ← {(0 ⌾ 0 rect: 12 ⌾ 36) , (12 ⌾ 0 rect: 36 ⌾ 36)}
! !
!InspectWindow methodsFor: 'Private' stamp: ''!
contents | |
	 "called by of: via Class fieldNamesInto"
! !
!InspectWindow methodsFor: 'Private' stamp: ''!
separator: c | |
	 "called by of: via Class fieldNamesInto"
! !
!InspectWindow methodsFor: 'Private' stamp: ''!
identifier: s | |
	 "called by of: via Class fieldNamesInto"
	variables next← s
! !
!InspectWindow methodsFor: 'Private' stamp: ''!
comment: s | |
	 "called by of: via Class fieldNamesInto"
! !
!InspectWindow methodsFor: 'Private' stamp: ''!
trailer: s | |
	 "called by of: via Class fieldNamesInto"
! !
PanedWindow subclass: #NotifyWindow
	instanceVariableNames: 'enoughpanes'
	classVariableNames: ''
	category: 'Windows'!
!NotifyWindow methodsFor: 'As yet unclassified' stamp: ''!
of: t1 stackPane: stackPane codePane: codePane | contextVarPane contextValuePane instanceVarPane instanceValuePane |
	title ← t1.
	NotifyFlag ← false "Create the remaining four panes.".
	contextVarPane ← VariablePane new.
	contextValuePane ← CodePane new.
	instanceVarPane ← VariablePane new.
	instanceValuePane ← CodePane new "Create the six-paned window.".
	self title: title with: {stackPane , codePane , contextVarPane , contextValuePane , instanceVarPane , instanceValuePane} at: bigTemplates.
	self frame: frame.
	self show "Initialize the six panes.".
	stackPane context: contextVarPane instance: instanceVarPane code: codePane.
	codePane from: stackPane.
	contextVarPane to: contextValuePane.
	contextValuePane from: contextVarPane.
	instanceVarPane to: instanceValuePane.
	instanceValuePane from: instanceVarPane.
	stackPane select: 0.
	stackPane makeParagraph.
	stackPane displayall.
	stackPane deselected.
	enoughpanes ← NotifyFlag ← true
! !
!NotifyWindow methodsFor: 'Initialization' stamp: ''!
of: titleString level: level interrupt: flag | stackPane |
	NotifyFlag ← false.
	stackPane ← StackPane new.
	self title: titleString with: stackPane inVector at: smallTemplates.
	smallFrame moveto: (level > 1
	  ifTrue: [300 ⌾ 50]
	  ifFalse: [user screenrect center - (smallFrame extent / 2)]).
	self frame: (self fixframe: smallFrame).
	self show.
	stackPane context: false at: level instance: false code: false.
	stackPane interrupt: flag.
	stackPane of: (Top ◦ level) inVector.
	NotifyFlag ← true
! !
!NotifyWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	smallTemplates ← (0 ⌾ 0 rect: 36 ⌾ 36) inVector.
	bigTemplates ← {(0 ⌾ 0 rect: 12 ⌾ 18) , (12 ⌾ 0 rect: 36 ⌾ 18) , (0 ⌾ 18 rect: 12 ⌾ 27) , (12 ⌾ 18 rect: 36 ⌾ 27) , (0 ⌾ 27 rect: 12 ⌾ 36) , (12 ⌾ 27 rect: 36 ⌾ 36)}.
	smallFrame ← 204 ⌾ 366 rect: 404 ⌾ 402
! !
!NotifyWindow methodsFor: 'Initialization' stamp: ''!
of: titleString stack: stack interrupt: flag | stackPane |
	NotifyFlag ← false.
	stackPane ← StackPane new.
	self title: titleString with: stackPane inVector at: smallTemplates.
	smallFrame moveto: ((NoteTaker or: [Top currentPriority ≤ 1])
	  ifTrue: [user screenrect center - (smallFrame extent / 2)]
	  ifFalse: [300 ⌾ 50]).
	self frame: (self fixframe: smallFrame).
	self show.
	stackPane context: false instance: false code: false.
	stackPane interrupt: flag.
	stackPane of: stack inVector.
	NotifyFlag ← true
! !
!NotifyWindow methodsFor: 'Window protocol' stamp: ''!
close | |
	super close.
	self erase.
	user unschedule: self.
	thisContext systemRestart
! !
!NotifyWindow methodsFor: 'Window protocol' stamp: ''!
closeNoRestart | |
	^super close
! !
!NotifyWindow methodsFor: 'Window protocol' stamp: ''!
enter | |
	enoughpanes
	  ifTrue: [super enter]
	  ifFalse: [self of: title stackPane: panes ◦ 1 codePane: CodePane new]
! !
!NotifyWindow methodsFor: 'Window protocol' stamp: ''!
aboutToFrame | |
	enoughpanes ← panes length = 6.
	super aboutToFrame
! !
Window subclass: #PanedWindow
	instanceVariableNames: 'panes templates title'
	classVariableNames: ''
	category: 'Windows'!
!PanedWindow methodsFor: 'Initialization' stamp: ''!
title: t1 with: t2 at: t3 | pane |
	 "The instance variable templates is a set of Rectangles for the frames of the panes normalized such that the whole PanedWindow is a frame of 0⌾0 rect: 36⌾36."
	title ← t1.
	panes ← t2.
	templates ← t3.
	self reset.
	panes do: [:pane | pane init]
! !
!PanedWindow methodsFor: 'Pane services' stamp: ''!
vanish | |
	self close.
	self erase.
	user unschedule: self
! !
!PanedWindow methodsFor: 'Private' stamp: ''!
titlerect | |
	^frame origin - (2 ⌾ (DefaultTextStyle lineheight + 4)) rect: frame corner x ⌾ frame origin y + (2 ⌾ 0)
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
title | |
	^title
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
yellowbug | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane yellowbug]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
frame: t1 | templateStream template pane orig ext |
	 "(Re)initialize my frame, and tell my panes their locations."
	frame ← t1.
	templateStream ← templates asStream.
	orig ← frame origin - 1.
	ext ← frame extent + 2.
	panes do:
		[:pane | template ← templateStream next "It would be nice to have parallel fors as in MLISP.".
		pane frame← template * ext / 36 + orig inset: 1]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
fixframe: f | |
	^Rectangle new origin: f origin extent: (f extent max: 160 ⌾ 80)
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
hardcopy: pf | pane |
	self hardcopyTitle: pf "print frame rectangle".
	frame hardcopy: pf "print all panes".
	panes do: [:pane | pane hardcopy: pf "print cursor if it's inside"].
	(frame has: user mp)
	  ifTrue: [user currentCursor hardcopy: pf]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
leave | pane |
	panes do: [:pane | pane windowleave]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
keyset | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane keyset]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
redbug | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane redbug]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
eachtime | pane |
	(frame has: user mp)
	  ifTrue:
		[user bluebug
		  ifTrue: [^self bluebug].
		panes do: [:pane | pane startup]]
	  ifFalse:
		[self outside
		  ifFalse:
			[user anybug
			  ifTrue:
				[(frame has: user mp)
				  ifFalse: [^false]]
			  ifFalse:
				[user kbck
				  ifTrue:
					[user kbd.
					frame flash] "flush typing outside"]]]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
takeCursor | |
	(panes ◦ 1) takeCursor
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
erase | |
	self titlerect clear.
	super erase
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
kbd | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane kbd]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
show | pane |
	super show.
	panes do: [:pane | pane outline]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
hardcopy | p |
	user displayoffwhile⦂ [
		(p ← dp0 pressfile: (self title + '.press') asFileName.
		self hardcopy: p.
		p close.
		p toPrinter)]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
close | pane |
	panes do: [:pane | pane close]
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
hardcopyTitle: pf | |
	self showtitle "refresh title (since it's a class var)" "draw title rectangle".
	titleframe window hardcopy: pf "print title text (make frame larger)".
	titleframe para presson: pf in: (pf transrect: (titleframe frame origin rect: titleframe frame corner + (999 ⌾ 2))) style: titleframe style
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
pickedpane | pane |
	panes do:
		[:pane | pane picked
		  ifTrue: [^pane]].
	frame flash.
	^false
! !
!PanedWindow methodsFor: 'Window protocol' stamp: ''!
enter | pane |
	super show.
	panes do: [:pane | pane windowenter]
! !
Window subclass: #ProjectWindow
	instanceVariableNames: 'userview parent changes'
	classVariableNames: ''
	category: 'Windows'!
!ProjectWindow methodsFor: 'Changing views' stamp: ''!
runParent | |
	 "leave this view by installing the one above"
	parent install
! !
!ProjectWindow methodsFor: 'Changing views' stamp: ''!
putTitle | |
	titlepara ≡ nil
	  ifTrue: [titlepara ← 'Top View' asParagraph allBold].
	titleframe put: titlepara centered: user screenrect extent x / 3 ⌾ 8.
	titleframe outline
! !
!ProjectWindow methodsFor: 'Changing views' stamp: ''!
install | |
	 "Establish this project and its userview as the current screen view"
	Changes ← changes.
	(user ← userview) install.
	self putTitle.
	user restart
! !
!ProjectWindow methodsFor: 'Initialization' stamp: ''!
userview: t1 changes: t2 parent: t3 | |
	 "load state"
	userview ← t1.
	changes ← t2.
	parent ← t3
! !
!ProjectWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	actionMenu ← Menu new string: 'enter'
! !
!ProjectWindow methodsFor: 'Initialization' stamp: ''!
init | |
	 "a new window"
	self userview: (user copyIn: self) changes: HashSet init parent: user projectWindow.
	self newframe.
	self show
! !
!ProjectWindow methodsFor: 'Window behavior' stamp: ''!
yellowbug | |
	actionMenu bug = 1
	  ifTrue: [self install]
! !
!ProjectWindow methodsFor: 'Window behavior' stamp: ''!
close | |
	 "break circular links"
	userview ← parent ← nil
! !
PanedWindow subclass: #SyntaxWindow
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Windows'!
!SyntaxWindow methodsFor: 'As yet unclassified' stamp: ''!
aboutToFrame | nw t s c |
	t ← title.
	s ← panes ◦ 1.
	c ← panes ◦ 2.
	user unschedule: self.
	nw ← NotifyWindow new frame: frame.
	nw of: t stackPane: s codePane: c.
	user restartup: nw
! !
!SyntaxWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	stdTemplates ← {(0 ⌾ 0 rect: 12 ⌾ 36) , (12 ⌾ 0 rect: 36 ⌾ 36)}.
	stdFrame ← 60 ⌾ 320 rect: 570 ⌾ 500
! !
!SyntaxWindow methodsFor: 'Initialization' stamp: ''!
of: errorString at: position in: stream for: class from: context | stackPane codePane |
	stackPane ← StackPane new.
	codePane ← CodePane new class: class selector: nil para: nil.
	self title: class title with: {stackPane , codePane} at: stdTemplates.
	stdFrame moveto: user screenrect center - (stdFrame extent / 2).
	self frame: (self fixframe: stdFrame).
	self show.
	stackPane context: false instance: false code: codePane.
	stackPane of: context inVector.
	codePane showing: stream asArray.
	codePane from: stackPane.
	codePane notify: errorString at: position in: stream
! !
Object subclass: #Window
	instanceVariableNames: 'frame collapsed titlepara growing exitflag '
	classVariableNames: ''
	category: 'Windows'!
!Window methodsFor: 'Default Event responses' stamp: ''!
yellowbug | |
	frame flash
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
print | t1 |
	(t1 ← dp0 pressfile: (self title + '.press.') asFileName) screenout: frame scale: PressScale.
	t1 toPrinter
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
keyset | |
	frame flash
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
leave | |
	
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
kbd | |
	user kbd.
	frame flash
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
outside | |
	(titleframe window has: user mp)
	  ifTrue:
		[user anybug
		  ifTrue: [self editTitle]
		  ifFalse: [^false]]
	  ifFalse: [^false]
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
redbug | |
	frame flash
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
hardcopy | |
	frame flash
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
close | |
	
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
bluebug | t1 |
	(t1 ← windowmenu bug) = 1
	  ifTrue: [^exitflag ← false].
	t1 = 2
	  ifTrue:
		[self newframe.
		self enter]
	  ifFalse:
		[t1 = 3
		  ifTrue:
			[self close.
			self erase.
			user unschedule: self.
			^false].
		t1 = 4
		  ifTrue: [self hardcopy]
		  ifFalse:
			[t1 = 5
			  ifTrue: [self print]]]
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
enter | |
	self show
! !
!Window methodsFor: 'Default Event responses' stamp: ''!
aboutToFrame | |
	 "My frame is about to change.  I dont care."
! !
!Window methodsFor: 'Framing' stamp: ''!
editTitle | pared w |
	pared ← TextImage new para: titlepara frame: nil.
	pared formerly: false.
	pared fixframe: titleframe window + (1 ⌾ 2).
	pared enter.
	w ← titleframe window.
	[user anybug and: [(w has: user mp) ≡ false]] whileFalseDo: 
		[user kbck
		  ifTrue: [pared kbd]
		  ifFalse:
			[user redbug
			  ifTrue:
				[(w has: user mp)
				  ifTrue: [pared selecting]]
			  ifFalse:
				[user yellowbug
				  ifTrue:
					[(w has: user mp)
					  ifTrue: [w flash]]]]].
	titlepara ← pared contents.
	self showtitle
! !
!Window methodsFor: 'Framing' stamp: ''!
moveFrom: oldframe | |
	(oldframe inset: ¬2) clear.
	self show
! !
!Window methodsFor: 'Framing' stamp: ''!
frame: f | |
	frame ← self fixframe: f
! !
!Window methodsFor: 'Framing' stamp: ''!
clearTitle: color | |
	(titleframe window inset: ¬2 ⌾ ¬2) clear: color
! !
!Window methodsFor: 'Framing' stamp: ''!
fixedwidthfromuser: width | a b oldframe |
	user waitnobug.
	frame ≡ nil
	  ifFalse:
		[self aboutToFrame.
		self erase].
	a ← OriginCursor showwhile⦂ [user waitbug].
	growing ← true.
	self frame: (frame ← self fixframe: (a rect: a + (width ⌾ 32))).
	self show.
	CornerCursor showwhile⦂ [([a ← user mpnext] whileTrueDo:
			[a x← frame corner x.
			oldframe ≡ nil
			  ifTrue: [user cursorloc← a max: frame corner].
			oldframe ← frame copy.
			self frame: (frame ← self fixframe: (frame growto: a)).
			self moveFrom: oldframe])].
	growing ← false.
	self takeCursor
! !
!Window methodsFor: 'Framing' stamp: ''!
showtitle | |
	titlepara ≡ nil
	  ifTrue: [titlepara ← Paragraph new text: self title runs: titlerun alignment: 0].
	titleframe put: titlepara at: frame origin + titleloc.
	titleframe outline
! !
!Window methodsFor: 'Framing' stamp: ''!
outline | |
	frame outline "Clear and outline me."
! !
!Window methodsFor: 'Framing' stamp: ''!
title | |
	^'Untitled'
! !
!Window methodsFor: 'Framing' stamp: ''!
fixframe: f | |
	^Rectangle new origin: f origin extent: (f extent max: 32 ⌾ 32)
! !
!Window methodsFor: 'Framing' stamp: ''!
takeCursor | |
	user cursorloc← frame center "Move the cursor to my center."
! !
!Window methodsFor: 'Framing' stamp: ''!
erase | |
	(frame inset: ¬2 ⌾ ¬2) clear.
	self clearTitle: background
! !
!Window methodsFor: 'Framing' stamp: ''!
frame | |
	^frame
! !
!Window methodsFor: 'Framing' stamp: ''!
show | |
	self outline.
	growing
	  ifFalse: [self showtitle]
! !
!Window methodsFor: 'Framing' stamp: ''!
newframe | a oldframe |
	user waitnobug.
	user restoredisplay.
	frame ≡ nil
	  ifFalse:
		[self aboutToFrame.
		self erase].
	a ← OriginCursor showwhile⦂ [user waitbug].
	growing ← true.
	frame ← self fixframe: (a rect: a + 32).
	frame outline.
	CornerCursor showwhile⦂ [([a ← user mpnext] whileTrueDo:
			[oldframe ≡ nil
			  ifTrue: [user cursorloc← a max: frame corner].
			oldframe ← frame copy.
			frame ← self fixframe: (frame growto: a).
			(oldframe inset: ¬2) clear.
			frame outline])].
	self frame: frame.
	growing ← false.
	self takeCursor
! !
!Window methodsFor: 'Initialization' stamp: ''!
reset | |
	exitflag ← true.
	growing ← false
! !
!Window methodsFor: 'Initialization' stamp: ''!
classInit | |
	 "Window classInit"
	border ← 2 ⌾ 2.
	titleframe ← Textframe new para: nil frame: nil.
	titleloc ← 3 ⌾ (¬4 - titleframe lineheight).
	titlerun ← String new: 2.
	titlerun word: 1 ← ¬255.
	windowmenu ← Menu new string: 'under
frame
close
print
printbits
'
! !
!Window methodsFor: 'Scheduling' stamp: ''!
schedule | |
	user restartup: self
! !
!Window methodsFor: 'Scheduling' stamp: ''!
lasttime | |
	self leave.
	^exitflag
! !
!Window methodsFor: 'Scheduling' stamp: ''!
firsttime | |
	(frame has: user mp)
	  ifTrue:
		[self reset.
		^self enter].
	^false
! !
!Window methodsFor: 'Scheduling' stamp: ''!
eachtime | |
	(frame has: user mp)
	  ifTrue:
		[user kbck
		  ifTrue: [^self kbd].
		user anybug
		  ifTrue:
			[user redbug
			  ifTrue: [^self redbug].
			user yellowbug
			  ifTrue: [^self yellowbug].
			user bluebug
			  ifTrue: [^self bluebug]]
		  ifFalse:
			[user anykeys
			  ifTrue: [^self keyset]]]
	  ifFalse:
		[self outside
		  ifFalse:
			[user anybug
			  ifTrue:
				[(frame has: user mp)
				  ifFalse: [^false]]
			  ifFalse:
				[user kbck
				  ifTrue:
					[user kbd.
					frame flash] "flush typing outside"]]]
! !
