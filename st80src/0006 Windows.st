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
class: class selector: selector para: para formerly: oldpara | codePane |
	codePane ← CodePane new class: class selector: selector para: nil.
	self title: class title + ' ' + selector with: codePane inVector at: stdTemplates.
	self newframe.
	self show.
	codePane showing: para.
	codePane formerly: oldpara.
	codePane from: codePane
! !
!CodeWindow methodsFor: 'Initialization' stamp: ''!
hardcopy: p | pane |
	panes do: [:pane | pane hardcopy: p]
! !
!CodeWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	stdTemplates ← (0 ⌾ 0 rect: 36 ⌾ 36) inVector
! !
Window subclass: #ProjectWindow
	instanceVariableNames: 'userview parent changes'
	classVariableNames: ''
	category: 'Windows'!
!ProjectWindow methodsFor: 'Changing views' stamp: ''!
install | |
	 "Establish this project and its userview as the current screen view"
	Changes ← changes.
	(user ← userview) install.
	self putTitle.
	user restart
! !
!ProjectWindow methodsFor: 'Changing views' stamp: ''!
putTitle | |
	titlepara ≡ nil
	  ifTrue: [titlepara ← 'Top View' asParagraph allBold].
	titleframe put: titlepara centered: user screenrect extent x / 3 ⌾ 8.
	titleframe outline
! !
!ProjectWindow methodsFor: 'Changing views' stamp: ''!
runParent | |
	 "leave this view by installing the one above"
	parent install
! !
!ProjectWindow methodsFor: 'Initialization' stamp: ''!
userview: t1 changes: t2 parent: t3 | |
	 "load state"
	userview ← t1.
	changes ← t2.
	parent ← t3
! !
!ProjectWindow methodsFor: 'Initialization' stamp: ''!
init | |
	 "a new window"
	self userview: (user copyIn: self) changes: HashSet init parent: user projectWindow.
	self newframe.
	self show
! !
!ProjectWindow methodsFor: 'Initialization' stamp: ''!
classInit | |
	actionMenu ← Menu new string: 'enter'
! !
!ProjectWindow methodsFor: 'Window behavior' stamp: ''!
close | |
	 "break circular links"
	userview ← parent ← nil
! !
!ProjectWindow methodsFor: 'Window behavior' stamp: ''!
yellowbug | |
	actionMenu bug = 1
	  ifTrue: [self install]
! !
