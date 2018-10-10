ParagraphPrinter subclass: #BravoPrinter
	instanceVariableNames: 'eject "Eject page before next paragraph if true"
			'
	classVariableNames: ''
	category: 'Printing'!
!BravoPrinter methodsFor: 'Initialization' stamp: ''!
init | |
	super init.
	eject ← false
! !
!BravoPrinter methodsFor: 'Writing' stamp: ''!
nextpage | |
	eject
	  ifTrue: [self eject]
	  ifFalse: [eject ← true]
! !
!BravoPrinter methodsFor: 'Writing' stamp: ''!
eject | |
	strm next← 12.
	strm cr
! !
!BravoPrinter methodsFor: 'Writing' stamp: ''!
print: para | l r |
	para ← para asParagraph.
	eject
	  ifTrue:
		[self eject.
		eject ← false].
	strm append: para text.
	strm next← 26 "^Z".
	l ← frame origin x.
	r ← frame corner x.
	l ≠ self defaultframe origin x
	  ifTrue:
		[strm append: 'l'.
		strm print: l].
	r ≠ self defaultframe corner x
	  ifTrue:
		[strm append: 'z'.
		strm print: r].
	leading ≠ self defaultleading
	  ifTrue:
		[strm append: 'e'.
		strm print: leading].
	para bravoRuns: strm
! !
Object subclass: #ParagraphPrinter
	instanceVariableNames: 'frame "<Rectangle> usable area on page"
			leading "<Integer> paragraph leading"
			style "<TextStyle> for paragraphs"
			strm "<Stream> for output"
			'
	classVariableNames: 'defaultleading'
	category: 'Printing'!
!ParagraphPrinter methodsFor: 'Access to state' stamp: ''!
frame← t1 | |
	frame ← t1
! !
!ParagraphPrinter methodsFor: 'Access to state' stamp: ''!
defaultleading | |
	^defaultleading
! !
!ParagraphPrinter methodsFor: 'Access to state' stamp: ''!
style← t1 | |
	style ← t1
! !
!ParagraphPrinter methodsFor: 'Access to state' stamp: ''!
leading← t1 | |
	leading ← t1
! !
!ParagraphPrinter methodsFor: 'Access to state' stamp: ''!
frame | |
	^frame
! !
!ParagraphPrinter methodsFor: 'Access to state' stamp: ''!
defaultframe | |
	^defaultframe
! !
!ParagraphPrinter methodsFor: 'Class stuff' stamp: ''!
printclass: class | c first |
	(class is: Vector)
	  ifTrue:
		[first ← true.
		class do:
			[:c | first
			  ifTrue: [first ← false]
			  ifFalse: [self nextpage].
			self printclass: c]]
	  ifFalse: [user displayoffwhile⦂ [
			((class is: UniqueString)
			  ifTrue: [class ← Smalltalk ◦ class].
			user cr.
			user show: class title.
			class paraprinton: self)]]
! !
!ParagraphPrinter methodsFor: 'Class stuff' stamp: ''!
printchanges: lis | selector class heading old mes s delFlg |
	 "prints Changes format: ('class message' 'class message' ...)
	or alternate format: (class (message ...) class () ...) or both
	If an element appears in the list of the form '~class message', this puts out a 
	line causing the system to forget that method.  These come after any additons,
	owing to the sort on Changes"
	lis empty
	  ifTrue: [^lis].
	lis ← lis asStream.
	old ← mes ← false.
	[class] whileTrueDo:
		[ "get next class, selector pair"
		delFlg ← false.
		(mes and: [(selector ← mes next)])
		  ifFalse:
			[ "more of alternate form"
			(s ← lis next)
			  ifTrue:
				[(s is: UniqueString)
				  ifTrue:
					[class ← Smalltalk lookup: s.
					mes ← lis next asStream.
					selector ← mes next]
				  ifFalse:
					[ "Changes format"
					s ← s asStream.
					s peek = 126 "~"
					  ifTrue:
						[s next "take it off stream".
						delFlg ← true].
					class ← Smalltalk ◦ (s upto: 32) unique.
					selector ← s upto: 32]]
			  ifFalse: [class ← false]].
		delFlg
		  ifTrue: [old ifTrue:
					[old endCategoryOn: self.
					old endChangesOn: self.
					old← false].
				self printForget: selector class: class]
		  ifFalse:
			[ "same, different or no class"
			old ≡ class
			  ifFalse:
				[old
				  ifTrue:
					[old endCategoryOn: self.
					old endChangesOn: self].
				class ≡ false
				  ifFalse:
					[ "finished"
					user cr.
					user show: class title.
					old ← class.
					class startChangesOn: self.
					heading ← 'As yet unclassified']].
			class ≡ false
			  ifFalse:
				[user space.
				user show: selector.
				s ← class organization invert: (selector ← selector unique).
				s
				  ifTrue:
					[s ≠ heading
					  ifTrue: [class startCategory: (heading ← s) on: self].
					class printMethod: selector on: self]]]]
! !
!ParagraphPrinter methodsFor: 'Class stuff' stamp: ''!
stamp | s t |
	t ← user now "date and time".
	s ← Stream default.
	s append: '''From '.
	s append: user version.
	s append: ' on '.
	s print: t ◦ 1.
	s append: ' at '.
	s print: t ◦ 2.
	s append: '.'''.
	s cr.
	self print: s contents asParagraph
! !
!ParagraphPrinter methodsFor: 'Class stuff' stamp: ''!
printForget: selector class: class | t |
	 "Print a line that causes a message to be forgotten"
	user cr.
	user show: '~' + class title + ' ' + selector.
	t ← selector last=('←'◦1) ifTrue: ['''' + selector + ''' unique'] ifFalse: ['#' + selector].
	self print: (class title + ' derstands: ' + t + '.
') asParagraph
! !
!ParagraphPrinter methodsFor: 'Closing' stamp: ''!
close | |
	strm close
! !
!ParagraphPrinter methodsFor: 'Initialization' stamp: ''!
of: t1 | |
	strm ← t1
! !
!ParagraphPrinter methodsFor: 'Initialization' stamp: ''!
init | |
	self frame← self defaultframe.
	self leading← defaultleading.
	self style← DefaultTextStyle
! !
!ParagraphPrinter methodsFor: 'Initialization' stamp: ''!
classInit | inch |
	inch ← 2540 "1 inch in micas".
	defaultframe ← (0.75 * inch) asInteger ⌾ (1 * inch) rect: (7.75 * inch) asInteger ⌾ (10 * inch).
	defaultleading ← 0
! !
!ParagraphPrinter methodsFor: 'Writing' stamp: ''!
print: para | |
	 "A dummy, subclasses will override"
	strm append: para text
! !
Object subclass: #ParagraphScanner
	instanceVariableNames: 'para "<Paragraph>"
			style "<TextStyle>"
			press "<PressFile> for output"
			runstrm "<Stream> of paragraph runs"
			textstrm "<Stream> of paragraph text"
			font "<WidthTable> current font"
			ascent "<Integer> max ascent"
			descent "<Integer> negative max descent"
			width "<Integer> total width"
			spaces "<Integer> number of spaces"
			rect "<Rectangle> for printing"
			tabpos "<Stream> (text position, new X position) of tabs"
			'
	classVariableNames: ''
	category: 'Printing'!
!ParagraphScanner methodsFor: 'Access' stamp: ''!
width | |
	^width
! !
!ParagraphScanner methodsFor: 'Access' stamp: ''!
position | |
	^textstrm position
! !
!ParagraphScanner methodsFor: 'Initialization' stamp: ''!
init | |
	ascent ← descent ← width ← spaces ← 0.
	tabpos reset
! !
!ParagraphScanner methodsFor: 'Initialization' stamp: ''!
in: t1 | |
	rect ← t1
! !
!ParagraphScanner methodsFor: 'Initialization' stamp: ''!
of: t1 to: t2 style: t3 | |
	para ← t1.
	press ← t2.
	style ← t3.
	textstrm ← '' asStream.
	runstrm ← para runs asStream.
	tabpos ← (Vector new: 10) asStream
! !
!ParagraphScanner methodsFor: 'Printing' stamp: ''!
printfrom: charpos aligned: align skip: n | ybot a b ix iy px xs sp rs len tpos ts ntab rval ifont w ps t21 |
	 "Returns false if goes below bottom"
	(ybot ← rect corner "this code basically writes the EL (entity list) for a line" "bottom of character -- ascent not really ascent but height" y - ascent) < rect origin y
	  ifTrue: [^false].
	 "won't fit"
	a ← charpos + 1.
	b ← textstrm position - n.
	a > b
	  ifFalse:
		[ "No text"
		ts ← tabpos viewer.
		tpos ← ts next.
		px ← false.
		xs ← rect width - width.
		ix ← rect minX + (align = 2 "left margin offset"
				  ifTrue: [xs / 2]
				  ifFalse:
					[align = 4
					  ifTrue: [xs]
					  ifFalse: [0]]) "set baseline of character.  do setx before showchars".
		press sety: (iy ← ybot + descent).
		sp ← font space "kludge?".
		align = 1
		  ifFalse: [ "do setspacex before showchars"
			press setspacex: sp].
		rs ← (para run: a to: b) asStream.
		[len ← rs next] whileTrueDo:
			[press selectfont: (press fontindex: (rval ← rs next) style: style) - 1.
			b ← a + len.
			(rval land: 4) = 0
			  ifFalse:
				[ "no underlining" "unfortunately, we must rescan this part of line to find out how wide it is"
				ifont ← press codefont: rval style: style "a WidthTable".
				ps ← (para ◦ (a to: b - 1)) asStream.
				w ← {true , 0}.
				[w ◦ 1] whileTrueDo:
					[w ← ifont scan: ps until: w ◦ 2 exceeds: rect width.
					(t21 ← w ◦ 1) = 32
					  ifTrue: [w ◦ 2 ← w ◦ 2 + ifont space]
					  ifFalse:
						[t21 = 9
						  ifTrue: [w ◦ 2 ← w ◦ 2 + ifont tab truncateTo: ifont tab]]].
				px
				  ifFalse: [ "use current x position"
					press setx: ix] "change y position to show rectangle, then change y back again".
				press sety: iy - 40.
				press showrectwidth: w ◦ 2 height: 30.
				press sety: iy].
			ntab ← 0.
			[tpos and: [tpos < b]] whileTrueDo:
				[ "Put out tabs"
				tpos = a
				  ifFalse:
					[ "no text between this tab and last" "put out accumulated tabs or initial x"
					ntab > 0
					  ifTrue:
						[press skipchars: ntab.
						press setx: px.
						ntab ← 0]
					  ifFalse:
						[px
						  ifFalse: [press setx: (px ← ix)]].
					press showchars: tpos - a].
				ntab ← ntab + 1.
				px ← ix + ts next.
				a ← tpos + 1.
				tpos ← ts next].
			ntab > 0
			  ifTrue:
				[press skipchars: ntab.
				press setx: px]
			  ifFalse:
				[px
				  ifFalse: [press setx: (px ← ix)]].
			(align = 1 and: [tpos ≡ false])
			  ifTrue:
				[ "Reset space width"
				spaces = 0
				  ifFalse: [press setspacex: xs / spaces + sp].
				align ← 0].
			rs end
			  ifTrue:
				[press showchars: b - a "for more compactness, maybe" skip: n.
				^ybot].
			press showchars: b - a.
			a ← b]].
	n > 0
	  ifTrue: [press skipchars: n "skip over ending blank or carriage return"].
	^ybot
! !
!ParagraphScanner methodsFor: 'Private scanning' stamp: ''!
newrun | len pos |
	(len ← runstrm next)
	  ifTrue:
		[pos ← textstrm position.
		textstrm of: para text from: pos + 1 to: pos + len.
		font ← press codefont: runstrm next style: style]
	  ifFalse: [^false]
! !
!ParagraphScanner methodsFor: 'Scanning' stamp: ''!
backup | |
	textstrm skip: ¬1
! !
!ParagraphScanner methodsFor: 'Scanning' stamp: ''!
tab | |
	spaces ← 0.
	tabpos next← textstrm position.
	tabpos next← width ← width + font tab truncateTo: font tab
! !
!ParagraphScanner methodsFor: 'Scanning' stamp: ''!
scan | maxw sp char t spos slim srunpos sasc sdesc swidth ssp sfont stpos |
	 "Scan up to a zero-width character, back up to last blank if width exceeded" "Save state"
	(textstrm end and: [self newrun ≡ false])
	  ifTrue: [^false].
	maxw ← rect width.
	[ascent ← ascent max: font ascent.
	descent ← descent max: font descent.
	sp ← font space.
	[t ← font scan: textstrm until: width exceeds: maxw.
	(char ← t ◦ 1) ≡ true
	  ifFalse: [width ← t ◦ 2].
	char = 32] whileTrueDo:
		[spos ← textstrm position "Save state".
		slim ← textstrm limit.
		srunpos ← runstrm position.
		stpos ← tabpos position.
		sasc ← ascent.
		sdesc ← descent.
		swidth ← width.
		ssp ← spaces.
		sfont ← font.
		spaces ← spaces + 1.
		width ← width + sp].
	char
	  ifTrue:
		[((char ≡ true and: [nil ≠ spos]) and: [2 * ascent ≤ rect height])
		  ifTrue:
			[textstrm of: para text "Back up to just past last blank (if another line fits)" from: spos + 1 to: slim.
			runstrm position← srunpos.
			tabpos position← stpos.
			ascent ← sasc.
			descent ← sdesc.
			width ← swidth.
			spaces ← ssp.
			font ← sfont.
			^32].
		^char]
	  ifFalse: [self newrun]] whileTrueDo: [].
	^false
! !
