ListPane subclass: #ClassPane
	instanceVariableNames: 'systemPane organizationPane'
	classVariableNames: ''
	category: 'Panes and Menus'!
!ClassPane methodsFor: 'Browser protocol' stamp: ''!
compile: parag | |
	systemPane compile: parag
! !
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
!ClassPane methodsFor: 'Window protocol' stamp: ''!
close | |
	systemPane ← nil.
	super close
! !
Window subclass: #CodePane
	instanceVariableNames: 'pared class selector selectorPane scrollBar'
	classVariableNames: ''
	category: 'Panes and Menus'!
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
notify: errorString at: position in: stream | |
	pared fintype.
	pared selectRange: (position to: position).
	pared replace: ('➲' + errorString + '➲.') asParagraph.
	pared selectAndScroll.
	^false
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
interactive | |
	^true
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
execute: parseStream in: context to: receiver | cls |
	context
		ifTrue: [cls← context mclass. receiver← context receiver]
		ifFalse: [cls← receiver class].
	^LADCompiler new evaluate: parseStream in: context to: receiver notifying: self
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
dirty | |
	pared formerly
	  ifTrue: [^frame].
	^false
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
compile: parag in: defaultClass under: category | |
	^LADCompiler new compile: parag in: (class ≡ nil
	  ifTrue: [defaultClass]
	  ifFalse: [class]) under: category notifying: self
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
formerly: oldpara | |
	 "should not be called before 'showing:'"
	pared formerly: oldpara
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
contents | |
	^pared contents
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
execute: parseStream for: codePane | |
	 "as my own selectorPane"
	^self execute: parseStream in: false to: nil
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
compile: parag | |
	 "as my own selectorPane"
	^self compile: parag in: class under: 'As yet unclassified'
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
oldContents | |
	^pared formerly
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
selectRange: r | |
	pared selectRange: r.
	pared selectAndScroll
! !
!CodePane methodsFor: 'Browse/Notify protocol' stamp: ''!
reflects: selection | |
	 "am I trying to show the code of selectorPaneⓢ selection?"
	^class ≡ nil and: [selection > 0]
! !
!CodePane methodsFor: 'Initialization' stamp: ''!
init | |
	
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
from: t1 | |
	selectorPane ← t1
! !
!CodePane methodsFor: 'Initialization' stamp: ''!
class: t1 selector: t2 para: para | |
	class ← t1.
	selector ← t2
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
redbug | |
	^pared selecting
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
outside | |
	^scrollBar startup
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
outline | |
	frame outline: 1
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
windowenter | |
	self outline.
	pared enter
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
picked | |
	^frame has: user mp
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
windowleave | |
	pared ≡ nil
	  ifFalse: [pared leave]
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
show | |
	frame outline.
	pared show
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
close | |
	pared unselect.
	selectorPane ← pared ← nil.
	scrollBar close
! !
!CodePane methodsFor: 'Window protocol' stamp: ''!
enter | |
	scrollBar show
! !
CodePane subclass: #FilePane
	instanceVariableNames: 'file'
	classVariableNames: ''
	category: 'Panes and Menus'!
!FilePane methodsFor: 'As yet unclassified' stamp: ''!
classInit | |
	 "FilePane classInit."
	editmenu ← Menu new string: 'again
copy
cut
paste
doit
put
undo
get
align'
! !
!FilePane methodsFor: 'As yet unclassified' stamp: ''!
file: t1 | |
	file ← t1
! !
!FilePane methodsFor: 'As yet unclassified' stamp: ''!
yellowbug | t1 |
	(t1 ← editmenu bug) = 1
	  ifTrue: [pared again]
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
					[t1 = 5
					  ifTrue: [self doit]
					  ifFalse:
						[t1 = 6
						  ifTrue:
							[pared formerly
							  ifTrue: [user displayoffwhile⦂ [
									(file readwriteshorten.
									file reset.
									file append: pared contents.
									file close.
									pared formerly: false)]]
							  ifFalse: [frame flash]]
						  ifFalse:
							[t1 = 7
							  ifTrue: [pared undo]
							  ifFalse:
								[t1 = 8
								  ifTrue: [user displayoffwhile⦂ [(scrollBar hidewhile⦂ [(self showing: file contents asParagraph)])]]
								  ifFalse:
									[t1 = 9
									  ifTrue: [pared realign]]]]]]]]]
! !
Textframe subclass: #ListPane
	instanceVariableNames: 'list firstShown lastShown selection scrollBar'
	classVariableNames: ''
	category: 'Panes and Menus'!
!ListPane methodsFor: 'Initialization' stamp: ''!
select: lineNum | oldSel |
	oldSel ← selection "Select my non-dummy displayed entry whose subscript is lineNum; highlight it; if it is different from selection, tell me to select.  If there is no such entry, set selection to 0 and if it wasnt 0 before, tell me to deselect.".
	((1 max: firstShown - 1) ≤ lineNum and: [lineNum ≤ (list length min: lastShown - 1)])
	  ifTrue:
		[selection ← lineNum.
		self compselection.
		oldSel ≠ selection
		  ifTrue: [self selected]]
	  ifFalse:
		[selection ← 0.
		oldSel ≠ selection
		  ifTrue: [self deselected]]
! !
!ListPane methodsFor: 'Initialization' stamp: ''!
of: t1 | |
	 "Acquire the specified list and show me scrolled to the top"
	list ← t1.
	selection ← 0.
	self makeParagraph.
	self frame← window.
	self outline.
	self displayall.
	self deselected
! !
!ListPane methodsFor: 'Initialization' stamp: ''!
revise: newlist with: sel | changing |
	 "Acquire newlist. Do not change firstShown. Select sel if in list."
	(changing ← list ≠ newlist)
	  ifTrue:
		[list ← newlist.
		window clear: white.
		self makeParagraph.
		self fill.
		self displayall]
	  ifFalse:
		[selection > 0
		  ifTrue:
			[(changing ← list ◦ selection ≠ sel)
			  ifTrue: [self compselection]]
		  ifFalse: [changing ← true]].
	changing
	  ifTrue:
		[selection ← ¬1.
		self select: (list find: sel)]
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
scrollPos | |
	((firstShown ≡ nil or: [list ≡ nil]) or: [list length = 0])
	  ifTrue: [^0.0].
	^(firstShown - 1) asFloat / list length
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
yellowbug | |
	window flash
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
scrollTo: f | place y |
	^self scrollUp: (f * lastline) asInteger - firstShown + 1 * style lineheight
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
leave | |
	scrollBar hide
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
keyset | c t2 |
	c ← user currentCursor "As long as any keyset keys are down, react to keys 2 and 8 down by scrolling up or down a line at a time.  If key 4 is down as well, scroll faster.".
	self scrollControl⦂ [
		((t2 ← user keyset) = 6
		  ifTrue: [2]
		  ifFalse:
			[t2 = 12
			  ifTrue: [¬2]
			  ifFalse:
				[t2 = 2
				  ifTrue: [1]
				  ifFalse:
					[t2 = 8
					  ifTrue: [¬1]
					  ifFalse: [0]]]])].
	c show
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
lasttime | |
	self leave
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
firsttime | |
	(window has: user mp)
	  ifTrue: [self enter]
	  ifFalse: [^false]
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
kbd | |
	window flash.
	user kbd
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
outside | |
	^scrollBar startup
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
outline | |
	window outline: 1
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
windowenter | |
	 "Refresh my image.  Reaffirm selection."
	self outline.
	self displayall.
	self compselection
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
frame← t1 | |
	 "(Re)initialize my window"
	window ← t1.
	(frame ← window inset: 2 ⌾ 0 and: [0 ⌾ 0]) width← 999.
	self fill.
	scrollBar ← (scrollBar ≡ nil
			  ifTrue: [ScrollBar new]
			  ifFalse: [scrollBar]) on: window from: self
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
redbug | newSel f |
	 "Deselect selection and select cursor item, if any"
	(f ← self locked)
	  ifFalse:
		[self compselection.
		newSel ← ((self lineofy: user mp y) max: 1) - 1.
		XeqCursor showwhile⦂ [(self select: (newSel = selection
			  ifTrue: [0]
			  ifFalse: [newSel]))]].
	[user redbug and: [(window has: user mp)]] whileTrueDo:
		[f
		  ifTrue:
			[f flash.
			self compselection.
			self compselection]]
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
hardcopy: pf | t cr first last lasty lineNum parag left right lineheight |
	window hardcopy: pf thickness: 1.
	para ≡ nil
	  ifTrue: [self makeParagraph].
	parag ← para asParagraph.
	t ← para asStream.
	last ← 0.
	cr ← 13.
	left ← frame minX.
	right ← window maxX.
	lasty ← frame minY.
	lineheight ← self lineheight.
	(firstShown to: lastShown) do:
		[:lineNum | first ← last.
		((t skipTo: cr) or: [lineNum = lastShown])
		  ifTrue: [last ← t position]
		  ifFalse: [user notify: 'not enough lines'].
		(lineNum = selection and: [selection > 0])
		  ifTrue: [(self selectionRect "outline selection; complementing doesn't look good" - (0 ⌾ 1) inset: 0 ⌾ 1) hardcopy: pf thickness: 1].
		(parag copy: first + 1 to: last - 1) presson: pf in: (pf transrect: (left ⌾ lasty rect: right ⌾ (lasty + lineheight + 4))) style: style.
		lasty ← lasty + lineheight]
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
eachtime | |
	(window has: user mp)
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
			  ifTrue: [^false]]
		  ifFalse:
			[user anykeys
			  ifTrue: [^self keyset]]]
	  ifFalse: [^self outside]
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
picked | |
	^window has: user mp
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
windowleave | |
	self compselection.
	self grayselection
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
close | |
	 "Zero my selection so it wont be grayed when I close.  Break cycles."
	selection ← 0.
	scrollBar close
! !
!ListPane methodsFor: 'Pane protocol' stamp: ''!
enter | |
	scrollBar show
! !
!ListPane methodsFor: 'Private' stamp: ''!
fill | |
	firstShown ≡ nil
	  ifTrue: [self makeParagraph]
	  ifFalse: [lastShown ← self lineofy: window maxY - (style lineheight - 1)].
	(self locked or: [selection > 0])
	  ifTrue:
		[selection < firstShown
		  ifTrue: [frame origin y← window minY - (selection * style lineheight) + style lineheight]
		  ifFalse:
			[selection > lastShown
			  ifTrue: [frame origin y← window minY - (selection * style lineheight + style lineheight) + (window maxY - window minY truncateTo: style lineheight) min: window minY]]].
	firstShown ← self lineofy: window minY + (style lineheight - 1).
	lastShown ← self lineofy: window maxY - (style lineheight - 1)
! !
!ListPane methodsFor: 'Private' stamp: ''!
dummy | |
	^'▱▱▱▱▱▱▱'
! !
!ListPane methodsFor: 'Private' stamp: ''!
scrollUp: amount | linetomove |
	self compselection.
	amount ← amount truncateTo: style lineheight.
	linetomove ← (self lineofy: window minY + amount "abs") max: 1.
	amount > 0
	  ifTrue:
		[lastline = lastShown
		  ifFalse:
			[self locked
			  ifTrue:
				[selection + 1 = firstShown
				  ifTrue: [window flash]
				  ifFalse: [self firstShown: (linetomove min: selection + 1)]]
			  ifFalse: [self firstShown: (linetomove min: lastline - (window height / style lineheight) + 1)]]]
	  ifFalse:
		[firstShown = 1
		  ifFalse:
			[self locked
			  ifTrue:
				[selection + 1 = lastShown
				  ifTrue: [window flash]
				  ifFalse: [self lastShown: (lastShown - (firstShown - linetomove) max: selection + 1)]]
			  ifFalse:
				[.
				self lastShown: (lastShown - (firstShown - linetomove) max: (lastline min: window height / style lineheight))]]].
	(window origin x ⌾ ((self yofline: lastline) + style lineheight) rect: window corner) clear: white.
	^self select: selection
! !
!ListPane methodsFor: 'Private' stamp: ''!
compselection | |
	 "If I have a selection, complement its image."
	selection ≠ 0
	  ifTrue: [self selectionRect comp]
! !
!ListPane methodsFor: 'Private' stamp: ''!
selectionRect | newy |
	^(Rectangle new "I have a selection.  Return its highlighting rectangle." origin: frame minX ⌾ (newy ← selection * style lineheight + frame minY) corner: frame maxX ⌾ (newy + style lineheight)) intersect: window
! !
!ListPane methodsFor: 'Private' stamp: ''!
grayselection | |
	selection ≠ 0
	  ifTrue: [self selectionRect color: ltgray mode: oring]
! !
!ListPane methodsFor: 'Private' stamp: ''!
lastShown: new | delta deltaPt |
	delta ← new - lastShown "negative".
	deltaPt ← 0 ⌾ (0 - delta * style lineheight) "positive".
	lastShown ← new.
	firstShown ← firstShown + delta.
	frame moveby: deltaPt.
	(Rectangle new origin: window origin extent: window width ⌾ (window height - deltaPt y truncateTo: style lineheight)) blt: window origin + deltaPt mode: storing.
	self displaylines: firstShown to: (firstShown - delta - 1 min: lastShown)
! !
!ListPane methodsFor: 'Private' stamp: ''!
firstShown: new | delta deltaPt |
	delta ← new - firstShown.
	deltaPt ← 0 ⌾ (0 - delta * style lineheight) "negative".
	firstShown ← new.
	lastShown ← lastShown + delta.
	frame moveby: deltaPt.
	(window origin - deltaPt rect: window corner) blt: window origin mode: storing.
	self displaylines: (firstShown max: lastShown - delta + 1) to: lastShown
! !
!ListPane methodsFor: 'Private' stamp: ''!
displayall | |
	self displaylines: firstShown to: lastShown
! !
!ListPane methodsFor: 'Private' stamp: ''!
makeParagraph | i len s |
	 "Given firstShown, compute lastShown."
	firstShown ≡ nil
	  ifTrue:
		[firstShown ← 1.
		selection ← 0].
	s ← (String new: 200) asStream.
	s append: self dummy.
	s cr.
	list ≡ nil
	  ifFalse: [(1 to: list length) do:
			[:i | list ◦ i printOn: s.
			s cr]].
	s append: self dummy.
	s cr.
	para ← s contents asParagraph.
	self measureall.
	lastShown ← self lineofy: window maxY - (style lineheight - 1).
	frame corner y← (self yofline: lastline) + style lineheight.
	selection > lastShown
	  ifTrue: [selection ← 0]
! !
!ListPane methodsFor: 'Private' stamp: ''!
init | |
	self para: nil frame: nil
! !
!ListPane methodsFor: 'Subclass defaults' stamp: ''!
dirty | |
	 "My subclasses may want to prohibit a change of selection"
	^false
! !
!ListPane methodsFor: 'Subclass defaults' stamp: ''!
locked | |
	 "My subclasses may want to prohibit a change of selection"
	^selection = 0
	  ifTrue: [false]
	  ifFalse: [self dirty]
! !
!ListPane methodsFor: 'Subclass defaults' stamp: ''!
selected | |
	 "A new selection is highlighted.  I dont care, but my subclasses might"
! !
!ListPane methodsFor: 'Subclass defaults' stamp: ''!
deselected | |
	 "I just lost my selection.  I dont care, but my subclasses might."
! !
Object subclass: #Menu
	instanceVariableNames: 'str text thisline frame'
	classVariableNames: ''
	category: 'Panes and Menus'!
!Menu methodsFor: 'Initialization' stamp: ''!
stringFromVector: v | s |
	s ← Stream default "DW classInit".
	v do:
		[:v | s append: v.
		s cr].
	self string: s contents
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
rescan | |
	 " | each. Menu allInstances notNil transform⦂ each to⦂ each rescan."
	self string: str "rescan (for new fonts, lineheight)"
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
wbug | index bits |
	bits ← self movingsetup "save background, display menu" "wait until a mouse button is down".
	[user anybug] whileFalseDo:  [ "get selection (possibly 0)"
		].
	index ← self bugit "restore background".
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
bug | index bits |
	bits ← self movingsetup "set up and save background".
	index ← self bugit "get the index".
	frame bitsFromString: bits "restore background".
	^index "return index"
! !
!Menu methodsFor: 'User interactions' stamp: ''!
zbug | index bits |
	bits ← self movingsetup.
	[(index ← self bugit) = 0] whileTrueDo: [].
	frame bitsFromString: bits.
	^index
! !
!Menu methodsFor: 'User interactions' stamp: ''!
fbug | index |
	index ← self bugit "for fixed menus" "get the index".
	^index "return index"
! !
!Menu methodsFor: 'User interactions' stamp: ''!
rebug | |
	user waitbug "wait for button down again".
	^self bug "bugcursor showwhile⦂"
! !
!Menu methodsFor: 'User interactions' stamp: ''!
frame | |
	^frame
! !
!Menu methodsFor: 'User interactions' stamp: ''!
show | |
	frame clear: black.
	text displayall
! !
!Menu methodsFor: 'User interactions' stamp: ''!
has: pt | |
	^text frame has: pt
! !
!Menu methodsFor: 'User interactions' stamp: ''!
clear | |
	frame clear
! !
ListPane subclass: #OrganizationPane
	instanceVariableNames: 'classPane selectorPane class'
	classVariableNames: ''
	category: 'Panes and Menus'!
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
forget: selector | cat |
	class derstands: selector.
	cat ← list ◦ selection.
	self revise: (self listFor: class) with: cat.
	selection > 0
	  ifTrue: [selectorPane revise: (class organization category: cat) with: selector]
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
execute: parag | |
	^class ⓢ parag
! !
!OrganizationPane methodsFor: 'Browser protocol' stamp: ''!
spawn: selector with: parag formerly: oldparag | |
	selectorPane compselection.
	selectorPane select: 0.
	class edit: selector para: parag formerly: oldparag
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
dirty | |
	^selectorPane dirty
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
!OrganizationPane methodsFor: 'Window protocol' stamp: ''!
close | |
	classPane ← nil.
	super close
! !
Menu subclass: #QuickMenu
	instanceVariableNames: 'mybits'
	classVariableNames: ''
	category: 'Panes and Menus'!
!QuickMenu methodsFor: 'As yet unclassified' stamp: ''!
quicksetup | pt bits |
	pt ← user screenrect center - thisline center "center prev item on screen".
	text frame moveby: pt.
	thisline moveby: pt.
	frame moveby: pt.
	bits ← frame bitsIntoString "save background".
	frame clear: black.
	text displayall.
	^bits
! !
!QuickMenu methodsFor: 'As yet unclassified' stamp: ''!
bug | index bits |
	bits ← self movingsetup. "set up and save background"
	frame bitsFromString: mybits.
	index ← self bugit "get the index".
	frame bitsFromString: bits "restore background".
	^index "return index"
! !
!QuickMenu methodsFor: 'As yet unclassified' stamp: ''!
string: t1 | i pt tpara bits|
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
	thisline ← Rectangle new origin: text frame origin corner: text frame corner x ⌾ text lineheight.
	bits ← self quicksetup. "saved bits"
	mybits ← frame bitsIntoString.
	frame bitsFromString: bits
! !
!QuickMenu methodsFor: 'As yet unclassified' stamp: ''!
movingsetup | pt bits |
	pt ← user mp - thisline center "center prev item on mouse".
	text frame moveby: pt.
	thisline moveby: pt.
	frame moveby: pt.
	bits ← frame bitsIntoString "save background".
	^bits
! !
Object subclass: #ScrollBar
	instanceVariableNames: 'rect bitstr owner position'
	classVariableNames: ''
	category: 'Panes and Menus'!
!ScrollBar methodsFor: 'Image' stamp: ''!
hidewhile⦂ expr | v |
	self hide.
	v ← expr eval.
	self show.
	^v
! !
!ScrollBar methodsFor: 'Image' stamp: ''!
reposition⦂ expr | |
	self reshow⦂ [
		(expr eval.
		self boxPosition← owner scrollPos)]
! !
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
reshow⦂ expr | r |
	r ← position inset: ¬2.
	expr eval.
	r clear: white.
	position outline
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
useBitmap | |
	bitstr ≡ nil
	  ifFalse: [bitstr ← bitstr asBitmap] " | x. ScrollBar allInstances transform⦂ x to⦂ x useBitmap. "
! !
!ScrollBar methodsFor: 'Initialization' stamp: ''!
on: f from: o | |
	self on: f from: o at: o scrollPos
! !
!ScrollBar methodsFor: 'Initialization' stamp: ''!
classInit | |
	UpCursor ← CursorForm new fromtext: '
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
	DownCursor ← CursorForm new fromtext: '
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
	JumpCursor ← CursorForm new fromtext: '
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
!ScrollBar methodsFor: 'Scheduling' stamp: ''!
lasttime | |
	
! !
!ScrollBar methodsFor: 'Scheduling' stamp: ''!
firsttime | |
	^rect has: user mp
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
close | |
	owner ← nil
! !
ListPane subclass: #SelectorPane
	instanceVariableNames: 'organizationPane codePane'
	classVariableNames: ''
	category: 'Panes and Menus'!
!SelectorPane methodsFor: 'Browser protocol' stamp: ''!
execute: parseStream for: t2 | |
	codePane ← t2.
	^codePane execute: parseStream in: false to: nil
! !
!SelectorPane methodsFor: 'Browser protocol' stamp: ''!
compile: parag | |
	^organizationPane compile: parag
! !
!SelectorPane methodsFor: 'Browser protocol' stamp: ''!
dirty | |
	^codePane dirty
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
!SelectorPane methodsFor: 'Window protocol' stamp: ''!
close | |
	organizationPane ← nil.
	super close
! !
ListPane subclass: #StackPane
	instanceVariableNames: 'contextVarPane instanceVarPane codePane variables proceed'
	classVariableNames: ''
	category: 'Panes and Menus'!
!StackPane methodsFor: 'Initialization' stamp: ''!
interrupt: flag | |
	proceed ◦ 1 ← flag
! !
!StackPane methodsFor: 'Initialization' stamp: ''!
context: t1 at: level instance: t3 code: t4 | |
	contextVarPane ← t1.
	instanceVarPane ← t3.
	codePane ← t4.
	variables ← (Vector new: 16) asStream.
	proceed ≡ nil
	  ifTrue: [proceed ← {false , nil , level}]
! !
!StackPane methodsFor: 'Initialization' stamp: ''!
classInit | |
	stackmenu ← Menu new string: 'stack
spawn
proceed
restart'
! !
!StackPane methodsFor: 'Initialization' stamp: ''!
context: t1 instance: t2 code: t3 | |
	contextVarPane ← t1.
	instanceVarPane ← t2.
	codePane ← t3.
	variables ← (Vector new: 16) asStream.
	proceed ≡ nil
	  ifTrue: [proceed ← (NoteTaker
				  ifTrue: [{false , nil , nil}]
				  ifFalse: [{false , nil , Top currentPriority}])]
! !
!StackPane methodsFor: 'ListPane protocol' stamp: ''!
locked | |
	^contextVarPane and: [(selection > 0 and: [self dirty])]
! !
!StackPane methodsFor: 'ListPane protocol' stamp: ''!
selected | context instance code safeVec |
	contextVarPane ifFalse: [^self].
	context ← list ◦ selection.
	instance ← context receiver.
	code ← (list ◦ selection) sourceCode.
	codePane showing: (code ifTrue: [code] ifFalse: ['']).
	contextVarPane names: (#(thisContext ) concat: (self tempNames: code))
			values: {context , context tempframe} wrt: context.
	safeVec ← Vector new: 2.
	safeVec all← instance.
	instanceVarPane names: (#(self ) concat: instance class instvars) values: safeVec wrt: context.
	contextVarPane select: 1
! !
!StackPane methodsFor: 'ListPane protocol' stamp: ''!
deselected | |
	contextVarPane ≡ false
	  ifFalse:
		[codePane showing: ''.
		contextVarPane names: (Vector new: 0) values: #(nil ) wrt: false.
		instanceVarPane names: (Vector new: 0) values: #(nil ) wrt: false]
! !
!StackPane methodsFor: 'NotifyWindow protocol' stamp: ''!
dirty | |
	^codePane and: [codePane dirty]
! !
!StackPane methodsFor: 'NotifyWindow protocol' stamp: ''!
execute: parseStream for: t2 | |
	codePane ← t2.
	^proceed ◦ 2 ← codePane execute: parseStream in: (selection = 0
	  ifTrue: [false]
	  ifFalse: [list ◦ selection]) to: nil
! !
!StackPane methodsFor: 'NotifyWindow protocol' stamp: ''!
compile: parseStream | ctxt selector method mcl |
	ctxt ← list ◦ (selection max: 1).
	mcl ← ctxt mclass.
	proceed ◦ 2 ← selector ← codePane compile: parseStream in: mcl under: 'As yet unclassified'
	  ifTrue:
		[(codePane reflects: selection) ifTrue:
			[method ← mcl md method: selector.
			self releaseAboveSelection.
			ctxt restartWith: method.
			proceed ◦ 1 ← true.
			self of: list ◦ (selection to: list length) copy.
			self select: 1]]
! !
!StackPane methodsFor: 'Private' stamp: ''!
contents | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'Private' stamp: ''!
selector | context |
	context ← list ◦ (selection max: 1).
	^context sender ≡ nil
	  ifTrue: [false]
	  ifFalse: [context selector]
! !
!StackPane methodsFor: 'Private' stamp: ''!
declaration: dummy1 name: string asArg: dummy2 | |
	variables next← string
! !
!StackPane methodsFor: 'Private' stamp: ''!
notify: msg | |
	 "selected context doesnt know its variables"
! !
!StackPane methodsFor: 'Private' stamp: ''!
releaseAboveSelection | |
	selection > 1
	  ifTrue:
		[list ◦ (selection - 1) sender← nil.
		(list ◦ 1) release "Fully"].
	NoteTaker
	  ifFalse: [ "??"
		(list ◦ (selection max: 1)) verifyFrames]
! !
!StackPane methodsFor: 'Private' stamp: ''!
continue: restarting | ctxt | " user notify: '*'. MethodContext howMany 50 40  30"
	 "Close my window and resume my selected context, if any, else my first context.  If interrupted (proceed◦1) or restarting or a recompiled method, don't return a value; otherwise, return proceed◦2."
	selection = 0
	  ifTrue: [selection ← 1].
	ctxt ← list ◦ selection.
	self releaseAboveSelection "release abandoned contexts".
	NoteTaker
	  ifFalse:
		[restarting
		  ifTrue: [ctxt restart]
		  ifFalse:
			[(proceed ◦ 1 and: [selection = 1])
			  ifFalse: [ "resume after interrupt"
				ctxt push: proceed ◦ 2]]].
	list ← false "Inhibit me closing.".
	user topWindow closeNoRestart; erase.
	user unschedule: user topWindow.
	list ← nil.
	NoteTaker
	  ifTrue:
		[thisContext sender releaseTo: ctxt.
		thisContext sender← ctxt.
		^proceed ◦ 2].
	proceed ◦ 3 = 1
	  ifTrue: [thisContext sender release].
	Top run: ctxt at: proceed ◦ 3.
	Top enable: proceed ◦ 3.
	Top wakeup: proceed ◦ 3.
	Top resetCurrent
! !
!StackPane methodsFor: 'Private' stamp: ''!
separator: c | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'Private' stamp: ''!
identifier: s | |
	 "called by selected via Class fieldNamesInto"
	variables next← s
! !
!StackPane methodsFor: 'Private' stamp: ''!
spawn | mclass selector parag oldparag |
	mclass ← (list ◦ (selection max: 1)) mclass.
	selector ← self selector.
	parag ← (codePane
			  ifTrue: [codePane contents]
			  ifFalse:
				[(mclass canunderstand: selector)
				  ifTrue: [mclass code: selector]
				  ifFalse: ['']]).
	oldparag ← (codePane
			  ifTrue: [codePane oldContents]
			  ifFalse: [false]).
	self compselection.
	self select: 0.
	mclass edit: selector para: parag formerly: oldparag
! !
!StackPane methodsFor: 'Private' stamp: ''!
terminate | |
	 "called by parser close during initialization"
! !
!StackPane methodsFor: 'Private' stamp: ''!
comment: s | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'Private' stamp: ''!
trailer: s | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'Private' stamp: ''!
tempNames: code | |
	^LADParser new parseArgsAndTemps: code asStream notifying: false
! !
!StackPane methodsFor: 'Window protocol' stamp: ''!
yellowbug | t1 |
	scrollBar hidewhile⦂ [
		((t1 ← stackmenu bug) = 1
		  ifTrue: [self revise: (list ◦ 1) "show a full backtrace" stack with: (selection = 0
			  ifFalse: [list ◦ selection])]
		  ifFalse:
			[t1 = 2
			  ifTrue: [self spawn "spawn a code editor"]
			  ifFalse:
				[t1 = 3
				  ifTrue: [self continue: false "return to selected context"]
				  ifFalse:
					[t1 = 4
					  ifTrue:
						[NoteTaker
						  ifTrue: [ "restart selected context"
							frame flash]
						  ifFalse: [self continue: true]]]]])]
! !
!StackPane methodsFor: 'Window protocol' stamp: ''!
close | |
	NoteTaker
	  ifTrue: [super close]
	  ifFalse:
		[Top enable: proceed ◦ 3.
		super close.
		list
		  ifTrue: [(list ◦ 1) releaseFully]]
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
dirty | |
	^classPane dirty
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
!SystemPane methodsFor: 'Initialization' stamp: ''!
to: t1 | |
	classPane ← t1
! !
!SystemPane methodsFor: 'Initialization' stamp: ''!
classInit | |
	sysmenu ← Menu new string: 'filout
print'
! !
!SystemPane methodsFor: 'Initialization' stamp: ''!
update | |
	self of: (#(AllClasses SystemOrganization ) concat: SystemOrganization categories).
	mySysOrgVersion ← user classNames
! !
!SystemPane methodsFor: 'ListPane protocol' stamp: ''!
selected | |
	classPane of: self classes
! !
!SystemPane methodsFor: 'ListPane protocol' stamp: ''!
deselected | |
	classPane of: (Vector new: 0)
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
ListPane subclass: #VariablePane
	instanceVariableNames: 'valuePane values context'
	classVariableNames: ''
	category: 'Panes and Menus'!
!VariablePane methodsFor: 'Initialization' stamp: ''!
to: t1 | |
	valuePane ← t1
! !
!VariablePane methodsFor: 'Initialization' stamp: ''!
classInit | |
	varmenu ← Menu new string: 'inspect'
! !
!VariablePane methodsFor: 'Initialization' stamp: ''!
names: vars values: t2 wrt: t3 | |
	values ← t2.
	context ← t3.
	self of: vars
! !
!VariablePane methodsFor: 'ListPane protocol' stamp: ''!
selected | |
	valuePane showing: self value asString
! !
!VariablePane methodsFor: 'ListPane protocol' stamp: ''!
deselected | |
	valuePane showing: ''
! !
!VariablePane methodsFor: 'Notify/Inspect protocol' stamp: ''!
execute: parseStream for: t2 | |
	valuePane ← t2.
	^valuePane execute: parseStream in: context to: values ◦ 1
! !
!VariablePane methodsFor: 'Notify/Inspect protocol' stamp: ''!
compile: parag | |
	window flash.
	^false
! !
!VariablePane methodsFor: 'Private' stamp: ''!
value | |
	selection = 1
	  ifTrue: [^values ◦ 1].
	^values ◦ 2 inspectfield: selection - 1
! !
!VariablePane methodsFor: 'Window protocol' stamp: ''!
yellowbug | |
	selection = 0
	  ifTrue: [window flash]
	  ifFalse: [scrollBar hidewhile⦂ [
			(varmenu bug = 1
			  ifTrue: [self value inspect])]]
! !
