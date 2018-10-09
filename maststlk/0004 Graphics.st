Object subclass: #BitBlt
	instanceVariableNames: 'effect screen dest destRaster destX destY
				width height source sourceRaster sourceX sourceY
				clipX clipY clipWidth clipHeight
				sourceForm destForm screenForm'
	classVariableNames: ''
	category: 'Graphics'!
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
sourceRaster: t1 | |
	 "length of a 'scanline' source in 16-bit words"
	sourceRaster ← t1
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
height: t1 | |
	height ← t1
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
destRaster: t1 | |
	 "length of a 'scanline' destination in 16-bit words"
	destRaster ← t1
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
dest: t1 | |
	dest ← t1
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
effect: anInteger | |
	effect ← #(35 39 38 36 44 45 41 33 51 55 54 52 19 23 22 20 ) ◦
		((anInteger land: 15) + 1)
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
window | |
	^clipX ⌾ clipY rect: clipX + clipWidth ⌾ (clipY + clipHeight)
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
source: t1 | |
	source ← t1
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
screen: t1 | |
	screen ← t1.
	(screen is: HalfTone)  "compatibility mode"
	  ifTrue: [screenForm ← screen. screen ← screenForm bits]
	  ifFalse:  [self screen: (HalfTone new fromOldGray: t1)]
! !
!BitBlt methodsFor: 'Operations' stamp: ''!
copyRect: rect toPoint: point effect: eff screen: halftone | |
	self screen: halftone "careful -- assumes dest, destRaster, source and sourceRaster are set!!".
	self effect: eff.
	self destOrigin: point.
	self sourceRect: rect.
	self callBLT
! !
!BitBlt methodsFor: 'Operations' stamp: ''!
callBLT | |<primitive: 66> "This copies the bits"
	(OldGrays and: [screen is: Bitmap])  "backward compatible halfTone screens"
	  ifTrue: [screen ← screenForm oldGray. self callBLT]
	  ifFalse: [user croak]
! !
!BitBlt methodsFor: 'Operations' stamp: ''!
stringReplace: destString with: sourcestring from: start to: stop and: replacement from: rstart to: rstop | |
	NoteTaker ifTrue: [^false].
	"Works for BitBlt parameters less than 4096. Replaces a subrange of a string.
	Called only by String replace:to:by:from:to:.  Concatenates into destString:
		sourcestring◦(1 to: start - 1)
		replacement◦(rstart to: rstop)
		sourcestring◦(stop + 1 to: sourcestring length).
	assumes String arguments"
	destString length = 0 ifTrue: [^destString].
	(replacement is: String) ifFalse: [^false].
	((stop ≥ 4096 or: [sourcestring length - stop ≥ 4096]) or: [(start + rstop - rstart ≥ 4096 or: [rstart > 4096])])
	  ifTrue: [^false].
	((start < 1 or: [stop > sourcestring length]) or: [(rstart < 1 or: [rstop > replacement length])])
	  ifTrue: [^false].
	destRaster ← destY ← sourceRaster ← sourceY ← 0.
	self effect: 0.
	self screen: black.
	height ← 1.
	dest ← destString.
	width ← start - 1 * 8.
	width = 0 ifFalse:
		[source ← sourcestring.
		sourceX ← destX ← 0.
		self callBLT].
	destX ← width.
	width ← 1 + rstop - rstart * 8.
	width = 0 ifFalse:
		[source ← replacement.
		sourceX ← rstart - 1 * 8.
		self callBLT].
	destX ← destX + width.
	width ← sourcestring length - stop * 8.
	width = 0 ifFalse:
		[source ← sourcestring.
		sourceX ← stop * 8.
		self callBLT].
	destString ◦ 1 ← destString ◦ 1. "mark dirty"
	^destString
! !
!BitBlt methodsFor: 'Operations' stamp: ''!
stringCopy: destString from: start to: stop with: replacement from: rstart to: rstop | |
	NoteTaker ifTrue: [^false].
	"Copies equal subranges from one string to another.  Works for BitBlt parameters up to 4096. maybe too much set up for short strings.  Currently, called by String copy:to:with:from:to:"
	width ← 1 + stop - start.
	width = 0 ifTrue: [^destString].
	((start > 4096 or: [rstart > 4096]) or: [width ≥ 4096]) ifTrue: [^false].
	((width < 0 or: [width ≠ (1 + rstop - rstart)]) or: [((start < 1 or: [stop > destString length]) or: [(rstart < 1 or: [rstop > replacement length])])])
	  ifTrue: [^false].
	destRaster ← destY ← sourceRaster ← sourceY ← 0.
	self effect: 0.
	self screen: black.
	height ← 1.
	width ← width * 8.
	dest ← destString.
	destX ← start - 1 * 8.
	source ← replacement.
	sourceX ← rstart - 1 * 8.
	self callBLT.
	destString ◦ 1 ← destString ◦ 1. "mark dirty"
	^destString
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
clipRect: rect | |
	clipX ← rect minX.
	clipY ← rect minY.
	clipWidth ← rect width.
	clipHeight ← rect height
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
destForm: t1 | |
	destForm ← t1.
	self clipRect: (0 ⌾ 0 rect: destForm extent).
	destRaster ← destForm width + 15 / 16.
	dest ← destForm bits
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
sourceRect: rect | |
	sourceX ← rect minX.
	sourceY ← rect minY.
	width ← rect width.
	height ← rect height
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
destOrigin: destOrigin | |
	destX ← destOrigin x.
	destY ← destOrigin y
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
fromDisplay | |
	self sourceForm: CurrentDisplay
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
toDisplay | |
	self destForm: CurrentDisplay.
	source ≡ nil ifTrue: [self sourceForm: CurrentDisplay.]
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
window: rect | |  "this is public"
	self clipRect: (rect intersect: (0 ⌾ 0 rect: destForm extent))
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
init | |
	self effect: 0.
	self screen: black.
	destX ← destY ← width ← height ← sourceX ← sourceY ← 0
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
extent: extent | |
	width ← extent x.
	height ← extent y
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
classInit | |
	pageOneCursor ← 281 "location of hardware cursor"
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
forCursor | |
	self effect: 0.
	self screen: black.
	dest ← source ← 281.
	width ← height ← 16.
	destRaster ← sourceRaster ← 1.
	destX ← destY ← sourceX ← sourceY ← 0
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
sourceForm: t1 | |
	sourceForm ← t1.
	sourceRaster ← sourceForm width + 15 / 16.
	source ← sourceForm bits
! !
String subclass: #Bitmap
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Graphics'!
!Bitmap methodsFor: 'As yet unclassified' stamp: ''!
fromString: s | i |
	(1 to: self length) do: [:i | self ◦ i ← s ◦ (i + i) + (s ◦ (i + i - 1) lshift: 8)]
! !
!Bitmap methodsFor: 'As yet unclassified' stamp: ''!
fromStream: strm | i hi lo |
	(1 to: self length) do:
		[:i | hi ← strm next.
		lo ← strm next.
		self ◦ i ← (hi lshift: 8) + lo]
! !
!Bitmap methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm | |
	strm append: 'a Bitmap of length '; print: self length
! !
!Bitmap methodsFor: 'As yet unclassified' stamp: ''!
toStream: strm | i |
	(1 to: self length) do:
		[:i | strm next← self ◦ i lshift: ¬8.
		strm next← self ◦ i land: 255]
! !
Form subclass: #CursorForm
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Graphics'!
!CursorForm methodsFor: 'Initialization' stamp: ''!
fromString: t1 | |
	bits ← t1.
	self fromString: bits  offset: 0 ⌾ 0
! !
!CursorForm methodsFor: 'Initialization' stamp: ''!
fromString: t1 offset: t2 | |
	extent ← 16⌾16.
	bits ← t1.
	offset ← t2
! !
!CursorForm methodsFor: 'Initialization' stamp: ''!
fromtext: str offset: t2 | i s n c |
	extent ← 16⌾16.
	offset ← t2.
	bits ← Bitmap new: 16 "Not great, but compatible with printon.".
	s ← str asStream.
	s next.
	(1 to: 16) do:
		[:i | n ← 0.
		[(c ← s next) = 48 or: [c = 49]] whileTrueDo: [n ← n + n + (c - 48)].
		bits ◦ i ← n]
! !
!CursorForm methodsFor: 'Initialization' stamp: ''!
fromtext: str | |
	self fromtext: str offset: 0 ⌾ 0
! !
!CursorForm methodsFor: 'Initialization' stamp: ''!
asForm || ^self
! !
!CursorForm methodsFor: 'Printing' stamp: ''!
printOn: strm | i |
	strm append: 'CursorForm new fromtext: '''.
	(1 to: 16) do:
		[:i | strm cr.
		bits ◦ i printOn: strm base: 2].
	strm append: ''' offset: '.
	strm print: offset.
	strm append: '.'
! !
!CursorForm methodsFor: 'Printing' stamp: ''!
hardcopy: pf | |
	self hardcopy: pf at: user mp "use current cursor position" - offset
! !
!CursorForm methodsFor: 'Printing' stamp: ''!
hardcopy: pf at: loc | rect |
	rect ← loc extent: 16 ⌾ 16 "print cursor image at some point location into a presssfile".
	pf setp: (pf transrect: rect) origin.
	pf bitmap: rect bits: bits
! !
!CursorForm methodsFor: 'Showing' stamp: ''!
showwhile⦂ expr | oldcursor value |
	oldcursor ← user currentCursor.
	self show.
	value ← expr eval.
	oldcursor show.
	^value
! !
!CursorForm methodsFor: 'Showing' stamp: ''!
show | |
	user currentCursor: self
! !
!CursorForm methodsFor: 'Showing' stamp: ''!
beCursor || <primitive: 96>
	user print: 'new cursor'
! !
Bitmap subclass: #DisplayBitmap
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Graphics'!

Form subclass: #DisplayForm
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Graphics'!
!DisplayForm methodsFor: 'As yet unclassified' stamp: ''!
beDisplay || <primitive: 97>
	user print: 'new display'
! !
Object subclass: #Form
	instanceVariableNames: 'extent bits offset figure ground'
	classVariableNames: ''
	category: 'Graphics'!
!Form methodsFor: 'As yet unclassified' stamp: ''!
extent: t1 bits: t2 offset: t3 | |
	extent ← t1.
	bits ← t2.
	offset ← t3
! !
!Form methodsFor: 'DISPLAY' stamp: ''!
displayat: path effect: effect clippedBy: cliprect | r i clippedrect |
	(path is: Point) "basic form display primitive"
	  ifTrue:
		[r ← Rectangle new origin: path extent: self extent.
		r bitsFromString: bits mode: effect clippedBy: cliprect.
		aurorarunning
		  ifTrue: [user displayoffwhile⦂ [
				(clippedrect ← r intersect: user screenrect.
				aurora destination: clippedrect.
				aurora source: clippedrect.
				aurora figure: figure.
				aurora ground: ground.
				aurora function: 1103 "AoverB".
				aurora doit.
				aurora function: 0.
				aurora doit)]]]
	  ifFalse:
		[(path is: Path)
		  ifTrue: [(1 to: path length) do: [:i | self displayat: path ◦ i effect: effect clippedBy: cliprect]]]
! !
!Form methodsFor: 'EDITING' stamp: ''!
blinkbrush: parentimage | pt |
	pt ← parentimage mp "to show current position of brush in the form.".
	brush displayat: parentimage rectangle origin + pt effect: 2 clippedBy: user screenrect.
	brush displayat: parentimage rectangle origin + pt effect: 2 clippedBy: user screenrect.
	^parentimage rectangle origin + pt
! !
!Form methodsFor: 'EDITING' stamp: ''!
arc: parentimage | pt1 pt2 pt3 path pt |
	BlankCursor show "arc tool for forms.".
	[user redbug] whileFalseDo:  [pt1 ← self blinkbrush: parentimage].
	brush displayat: pt1 effect: color clippedBy: user screenrect.
	[user nobug] whileFalseDo:  [pt2 ← self blinkbrush: parentimage].
	brush displayat: pt2 effect: color clippedBy: user screenrect.
	[user redbug] whileFalseDo:  [pt3 ← self blinkbrush: parentimage].
	brush displayat: pt3 effect: color clippedBy: user screenrect.
	path ← Path new init.
	path addarcfrom: pt1 via: pt2 to: pt3.
	path do: [:pt | brush displayat: pt effect: color clippedBy: user screenrect].
	NormalCursor show
! !
!Form methodsFor: 'EDITING' stamp: ''!
setground: parentimage | |
	ground ← ground + 1 "for now just increment the ground color by 1 \ 14" \ 14.
	self displayat: parentimage origin effect: 0 clippedBy: user screenrect
! !
!Form methodsFor: 'EDITING' stamp: ''!
setfigure: parentimage | |
	figure ← figure + 1 "for now just increment the figure color by 1 \ 14" \ 14.
	self displayat: parentimage origin effect: 0 clippedBy: user screenrect
! !
!Form methodsFor: 'EDITING' stamp: ''!
resize: superimage | pt f |
	superimage boxcomp.
	CornerCursor show.
	user waitbug.
	[user nobug] whileFalseDo: 
		[superimage reverse.
		superimage reverse.
		pt ← superimage superimage mp + superimage superimage rectangle origin.
		superimage corner← pt max: superimage origin + (16 ⌾ 16)].
	f ← Form new fromrectangle: superimage rectangle.
	bits ← f bits.
	extent ← f extent.
	offset ← 0 ⌾ 0.
	superimage white.
	superimage resize.
	superimage display.
	superimage boxcomp.
	NormalCursor show
! !
!Form methodsFor: 'EDITING' stamp: ''!
edit: parentimage | pt f c file t6 |
	[false] whileFalseDo: 
		[ "Simple Form editor for now." "forever for now"
		pt ← parentimage mp " blink the current brush".
		BlankCursor show.
		self blinkbrush: parentimage.
		user redbug
		  ifTrue:
			[(parentimage contains: (pt ← self blinkbrush: parentimage))
			  ifTrue:
				[brush displayat: pt effect: color clippedBy: parentimage rectangle.
				[user redbug] whileTrueDo:
					[brush displayat: (self blinkbrush: parentimage) effect: color clippedBy: parentimage rectangle]]
			  ifFalse:
				[NormalCursor show.
				bits ← parentimage rectangle bitsIntoString.
				^self]]
		  ifFalse:
			[user kbck
			  ifTrue:
				[c ← user kbd.
				c = 120
				  ifTrue:
					[user clearshow: 'x gridding is '.
					parentimage xgrid print.
					user cr.
					parentimage xgrid: (user request: 'x gridding . . . ') asInteger]
				  ifFalse:
					[c = 121
					  ifTrue:
						[user clearshow: 'y gridding is '.
						parentimage ygrid print.
						user cr.
						parentimage ygrid: (user request: 'y gridding . . . ') asInteger]
					  ifFalse:
						[c = 114
						  ifTrue:
							[file ← user request: 'filename of Form . . .'.
							brush ← Form new read: file.
							brush figure: 1.
							brush ground: 0]]]]
			  ifFalse:
				[user yellowbug
				  ifTrue:
					[NormalCursor show.
					(t6 ← formmenu bug) = 1
					  ifTrue: [self newbrush: parentimage]
					  ifFalse:
						[ "get a new brush"
						t6 = 2
						  ifTrue:
							[color ← 1]
						  ifFalse:
							[ "set the color of the brush to black"
							t6 = 3
							  ifTrue:
								[color ← 3]
							  ifFalse:
								[ "set the color of the brush to white"
								t6 = 4
								  ifTrue: [self line: parentimage]
								  ifFalse:
									[t6 = 5
									  ifTrue: [self arc: parentimage]
									  ifFalse:
										[t6 = 6
										  ifTrue:
											[self white.
											parentimage display]
										  ifFalse:
											[ "erase the whole form"
											t6 = 7
											  ifTrue: [self resize: parentimage]
											  ifFalse:
												[ "change size"
												t6 = 8
												  ifTrue: [self setfigure: parentimage]
												  ifFalse:
													[t6 = 9
													  ifTrue: [self setground: parentimage]]]]]]]]]]
				  ifFalse:
					[user bluebug
					  ifTrue:
						[bits ← parentimage rectangle bitsIntoString.
						NormalCursor show.
						^self] "exit back to the parentimage"]]]]
! !
!Form methodsFor: 'EDITING' stamp: ''!
line: parentimage | pt1 pt2 path pt |
	BlankCursor show "line tool for forms.".
	[user redbug] whileFalseDo:  [pt1 ← self blinkbrush: parentimage].
	brush displayat: pt1 effect: color clippedBy: user screenrect.
	[user nobug] whileFalseDo:  [pt2 ← self blinkbrush: parentimage].
	brush displayat: pt2 effect: color clippedBy: user screenrect.
	path ← Path new init.
	path addlinefrom: pt1 to: pt2.
	path do: [:pt | brush displayat: pt effect: color clippedBy: user screenrect].
	NormalCursor show
! !
!Form methodsFor: 'EDITING' stamp: ''!
newbrush: superimage | pt rect |
	OriginCursor show.
	user waitbug.
	pt ← superimage mp + superimage rectangle origin.
	rect ← pt rect: pt.
	CornerCursor show.
	[user nobug] whileFalseDo: 
		[rect reverse.
		rect reverse.
		pt ← superimage mp + superimage rectangle origin.
		rect corner← rect origin max: pt].
	brush ← Form new fromrectangle: rect.
	NormalCursor show
! !
!Form methodsFor: 'FILING' stamp: ''!
write: filename | t2 |
	(t2 ← dp0 file: filename "Saves the Form in the format width,height,bits.") nextword← self width.
	t2 nextword← self height.
	t2 append: bits.
	t2 close
! !
!Form methodsFor: 'FILING' stamp: ''!
read: filename | f strip w h form stripheight leftoverlines i |
	f ← dp0 oldFile: filename "Reads the Form from the disk in the format width,height,bits.".
	f readonly.
	w ← f nextword.
	h ← f nextword.
	extent ← w ⌾ h.
	w * h < 64000
	  ifTrue:
		[bits ← (Form new extent: extent) bits.
		f into: bits.
		f close]
	  ifFalse:
		[f close.
		user notify: 'too many bits to be a Form']
! !
!Form methodsFor: 'INIT' stamp: ''!
fromImage: image | |
	self extent: image extent "creates a virtual bit map with width = (image width) and height = (image height) with the bits in image.".
	bits ← image rectangle bitsIntoString
! !
!Form methodsFor: 'INIT' stamp: ''!
fromrectangle: r | |
	self extent: r extent "creates a virtual bit map with width = (r width) and height = (r height) with the bits in r.".
	bits ← r bitsIntoString
! !
!Form methodsFor: 'INIT' stamp: ''!
fromuser | r |
	r ← Rectangle new "create a new Form whose rectangle is specified by the user. " fromuser.
	self extent: r extent.
	bits ← r bitsIntoString
! !
!Form methodsFor: 'INIT' stamp: ''!
fromuserevenword | r |
	r ← Rectangle new "create a new Form whose rectangle is specified by the user,
		truncated to nearest multiple of 16 (for Spruce printing). " fromuserevenword.
	self extent: r extent.
	bits ← r bitsIntoString
! !
!Form methodsFor: 'INIT' stamp: ''!
extent: t1 | |
	extent ← t1.
	self extent: extent figure: 0 ground: 1 offset: 0 ⌾ 0 "creates a virtual bit map with width = (extent x) and height = (extent y) with the bits all 1."
! !
!Form methodsFor: 'INIT' stamp: ''!
classInit | |
	blankcursor ← nil. "Form classInit."
	black ← 0 - 1 "sets up colors and effects for BITBLT.".
	white ← 0.
	over ← 0.
	under ← 1.
	reverse ← 2.
	brush ← Form new extent: 5 ⌾ 5.
	brush black.
	color ← 1.
	formmenu ← Menu new string: 'brush
black
white
line
arc
erase
size
figure
ground
'.
	dotsetter ← BitBlt new init " a BitBlt for pattern access.".
	dotsetter extent: 1 ⌾ 1.
	aurorarunning ← false.
	aurora ← nil "Aurora new"
! !
!Form methodsFor: 'INIT' stamp: ''!
close | |
	
! !
!Form methodsFor: 'INIT' stamp: ''!
extent: t1 figure: t2 ground: t3 offset: t4 | |
	extent ← t1.
	figure ← t2.
	ground ← t3.
	offset ← t4.
	bits ← Bitmap new: extent y "creates a virtual bit map with width = (extent x) and height = (extent y) with the bits all 1." * (extent x + 15 / 16)
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
figure: t1 | |
	figure ← t1 "set the figure ( color assiciated with black) for the form "
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
length | |
	^bits length
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
width | |
	^extent x "return the width of the Form"
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
extent | |
	^extent "return the extent (width⌾height) of the Form"
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
offset: t1 | |
	offset ← t1.
	^self "set the offset of the form "
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
ground | |
	^ground "return the ground ( color assiciated with white) for the form "
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
figure | |
	^figure "return the figure( color assiciated with black) for the form "
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
ground: t1 | |
	ground ← t1 "set the ground ( color assiciated with white) for the form "
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
offset | |
	offset ≡ nil
	  ifTrue: [^0 ⌾ 0].
	^offset
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
height | |
	^extent y "return the height of the Form"
! !
!Form methodsFor: 'PATTERN ACCESS' stamp: ''!
black | i |
	(1 to: bits length "sets all bits in the form to black ( to ones)") do: [:i | bits ◦ i ← 8r177777]
! !
!Form methodsFor: 'PATTERN ACCESS' stamp: ''!
bits: t1 | |
	bits ← t1 "reset the string containing the bits)"
! !
!Form methodsFor: 'PATTERN ACCESS' stamp: ''!
bits | |
	^bits "return the string containing the bits)"
! !
!Form methodsFor: 'PATTERN ACCESS' stamp: ''!
gray | i |
	(1 to: bits length "sets all bits in the form to gray ( to gray)") do: [:i | bits ◦ i ← 10922]
! !
!Form methodsFor: 'PATTERN ACCESS' stamp: ''!
black: pt | |
	(0 ⌾ 0 "sets the bit at pt in the  form to black ( to one)" ≤ pt and: [pt ≤ extent])
	  ifTrue:
		[dotsetter dest: bits.
		dotsetter destRaster: extent x + 15 / 16.
		dotsetter destOrigin: pt.
		dotsetter screen: black.
		dotsetter effect: 12.
		dotsetter callBLT]
! !
!Form methodsFor: 'PATTERN ACCESS' stamp: ''!
white | i |
	(1 to: bits length "sets all bits in the form to white ( to zeros)") do: [:i | bits ◦ i ← 0]
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
fromPress: press value: s | nbytes |
	extent ← s nextPoint.
	offset ← s nextPoint.
	figure ← s nextword.
	ground ← s nextword.
	nbytes ← 2 * extent y * (extent x + 15 / 16).
	press data skip: 0 - nbytes.
	bits ← press data next: nbytes
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
hideData: complete | s t3 |
	(t3 ← Stream new "a Form does not split across page boundaries") of: (s ← String new: 12).
	t3 nextPoint← extent.
	t3 nextPoint← offset.
	t3 nextword← figure.
	t3 nextword← ground.
	^s
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
asInstance | s |
	s ← Stream new default.
	s nextPoint← extent.
	s nextPoint← offset.
	s nextword← figure.
	s nextword← ground.
	s nextString← bits.
	^s contents
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
presson: press in: r | hs y |
	(hs ← press scale * self height) > r height
	  ifTrue: [^self].
	 "not enough room left on current page.
		assume for now that it will at least fit on an entire page"
	press setp: r origin x ⌾ (y ← r corner y - hs).
	press bitmap: self bits: bits.
	^y
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
hidePress: press complete: c | |
	press skipcode: self pressCode data: (self hideData: c)
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
copy | t |
	t ← Form new "return a copy of myself" extent: extent.
	t bits: bits copy.
	^t
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
fromInstance: file | |
	extent ← file nextPoint.
	offset ← file nextPoint.
	figure ← file nextword.
	ground ← file nextword.
	bits ← file nextString.
	^self
! !
!Form methodsFor: 'SYSTEM' stamp: ''!
pressCode | |
	^5
! !
Form subclass: #HalfTone
	instanceVariableNames: 'oldGray'
	classVariableNames: ''
	category: 'Graphics'!
!HalfTone methodsFor: 'As yet unclassified' stamp: ''!
fromOldGray: gray | i j line |
	oldGray← gray.
	self extent: 16 ⌾ 16.
	"1 to: 4 do:
		[:i | line ← (gray land: 15) * 16r1111.
		0 to: 3 do: [:j | bits ◦ (j * 4 + i) ← line].
		gray ← gray lshift: ¬4]" " this crashed the system! "
	bits← ((0⌾0 rect: 16⌾16) clear: gray) bitsIntoString  "this doesnt"
"
background← HalfTone new fromOldGray: background.
dkgray← HalfTone new fromOldGray: dkgray.
ltgray← HalfTone new fromOldGray: ltgray.
gray← HalfTone new fromOldGray: gray.
black← HalfTone new fromOldGray: black.
white← HalfTone new fromOldGray: white.
"
" (Bitmap new: 2)◦1←0  crashes! "
! !
!HalfTone methodsFor: 'As yet unclassified' stamp: ''!
oldGray | |
	^oldGray
! !
Object subclass: #Point
	instanceVariableNames: 'x y'
	classVariableNames: ''
	category: 'Graphics'!
!Point methodsFor: 'Access to parts' stamp: ''!
hash | |
	^(x lshift: 2) lxor: y
! !
!Point methodsFor: 'Access to parts' stamp: ''!
x← t1 | |
	x ← t1
! !
!Point methodsFor: 'Access to parts' stamp: ''!
y← t1 | |
	y ← t1
! !
!Point methodsFor: 'Access to parts' stamp: ''!
theta | tan theta |
	 "return the angle the point makes with origin.  right is 0; down is 90."
	x = 0
	  ifTrue:
		[y ≥ 0
		  ifTrue: [^90.0].
		^270.0]
	  ifFalse:
		[.
		tan ← y asFloat / x asFloat.
		theta ← tan arctan.
		x ≥ 0
		  ifTrue:
			[y ≥ 0
			  ifTrue: [^theta].
			^360.0 + theta]
		  ifFalse:
			[.
			^180.0 + theta]]
! !
!Point methodsFor: 'Access to parts' stamp: ''!
y | |
	^y
! !
!Point methodsFor: 'Access to parts' stamp: ''!
x | |
	^x
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
translate: delta | |
	x ← x + delta x "increment self by delta".
	y ← y + delta y
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
normal | n |
	 "unit vector rotated 90 deg clockwise"
	n ← y asFloat neg ⌾ x asFloat.
	^n / n length
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
max: t | |
	^Point new x: (x max: t x) y: (y max: t y)
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
normalize | |
	self x← 0 "set selt to zero".
	self y← 0
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
length | |
	^(x asFloat * x asFloat + (y asFloat * y asFloat)) sqrt
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
/ scale | |
	^Point new "Return a Point that is the quotient of me and scale (which is a Point or Number)" x: x / scale asPtX y: y / scale asPtY
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
* scale | |
	^Point new "Return a Point that is the product of me and scale (which is a Point or Number)" x: x * scale asPtX y: y * scale asPtY
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
= pt | |
	^x = pt x and: [y = pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
≥ pt | |
	^x ≥ pt x and: [y ≥ pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
≤ pt | |
	^x ≤ pt x and: [y ≤ pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
> pt | |
	^x > pt x and: [y > pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
< pt | |
	^x < pt x and: [y < pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
- delta | |
	^Point new "Return a Point that is the difference of me and delta (which is a Point or Number)" x: x - delta asPtX y: y - delta asPtY
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
+ delta | |
	^Point new "Return a Point that is the sum of me and delta (which is a Point or Number)" x: x + delta asPtX y: y + delta asPtY
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
abs | |
	 "absolute value of a point"
	^Point new x: x abs y: y abs
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
min: t | |
	^Point new x: (x min: t x) y: (y min: t y)
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
truncateTo: grid | |
	^Point new x: (x truncateTo: grid) y: (y truncateTo: grid)
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
dist: pt | t |
	 "distance (Manhattan norm) between pt and self"
	t ← (pt - self) abs.
	^t x + t y
! !
!Point methodsFor: 'Conversion' stamp: ''!
asPoint | |
	 "Return self."
! !
!Point methodsFor: 'Conversion' stamp: ''!
corner | |
	^self + (1 ⌾ 1)
! !
!Point methodsFor: 'Conversion' stamp: ''!
width | |
	^1
! !
!Point methodsFor: 'Conversion' stamp: ''!
extent | |
	^1 ⌾ 1
! !
!Point methodsFor: 'Conversion' stamp: ''!
printOn: strm | |
	strm print: x.
	strm append: '⌾'.
	strm print: y
! !
!Point methodsFor: 'Conversion' stamp: ''!
asRectCorner | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Point methodsFor: 'Conversion' stamp: ''!
asRectOrigin | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Point methodsFor: 'Conversion' stamp: ''!
origin | |
	^self
! !
!Point methodsFor: 'Conversion' stamp: ''!
asPtX | |
	^x
! !
!Point methodsFor: 'Conversion' stamp: ''!
asRectangle | |
	^self rect: self "Return a Rectangle with me as both origin and corner."
! !
!Point methodsFor: 'Conversion' stamp: ''!
rect: p | |
	 "infix creation of rectangles"
	^Rectangle new origin: self corner: p
! !
!Point methodsFor: 'Conversion' stamp: ''!
extent: p | |
	 "infix creation of rectangles"
	^Rectangle new origin: self extent: p
! !
!Point methodsFor: 'Conversion' stamp: ''!
asPtY | |
	^y
! !
!Point methodsFor: 'Conversion' stamp: ''!
height | |
	^1
! !
!Point methodsFor: 'Initialization' stamp: ''!
x: t1 y: t2 | |
	x ← t1.
	y ← t2
! !
!Point methodsFor: 'Initialization' stamp: ''!
copy | |
	^x ⌾ y
! !
!Point methodsFor: 'SYSTEM' stamp: ''!
pressCode | |
	^7
! !
!Point methodsFor: 'SYSTEM' stamp: ''!
fromPress: press value: s | |
	x ← s nextword.
	y ← s nextword
! !
!Point methodsFor: 'SYSTEM' stamp: ''!
hideData: complete | s t3 |
	(t3 ← Stream new) of: (s ← String new: 4).
	t3 nextPoint← self.
	^s
! !
!Point methodsFor: 'SYSTEM' stamp: ''!
hidePress: press complete: c | |
	press skipcode: self pressCode data: (self hideData: c)
! !
Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	category: 'Graphics'!
!Rectangle methodsFor: 'Altering' stamp: ''!
translate: pt | |
	origin ← origin + pt.
	corner ← corner + pt
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
usersize | |
	self usersize: user screenrect
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
usermove: bound | m lim |
	lim ← bound corner - self extent.
	self bordercomp.
	m ← user mp.
	[true] whileTrueDo:
		[user redbug
		  ifTrue:
			[self bordercomp.
			self moveto: (bound origin max: ((m ← user mp) min: lim)).
			self bordercomp].
		[user anybug and: [m = user mp]] whileTrueDo: [].
		user bluebug
		  ifTrue:
			[user waitnobug.
			^self bordercomp]]
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
usermove | |
	self usermove: user screenrect
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
translateto: pt | |
	self translate: pt - origin
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
usersize: bound | m lim |
	self origin ≡ nil
	  ifTrue:
		[origin ← user mp.
		self extent← 16].
	self bordercomp.
	m ← user mp.
	[true] whileTrueDo:
		[lim ← bound corner - self extent.
		user redbug
		  ifTrue:
			[self bordercomp.
			self moveto: (bound origin max: ((m ← user mp) min: lim)).
			self bordercomp].
		user yellowbug
		  ifTrue:
			[self bordercomp.
			corner ← m ← (user mp min: bound corner) max: origin.
			self bordercomp].
		[user anybug and: [m = user mp]] whileTrueDo: [].
		user bluebug
		  ifTrue:
			[user waitnobug.
			^self bordercomp]]
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
growto: t1 | |
	corner ← t1
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
moveto: pt | |
	corner ← corner + pt - origin.
	origin ← pt
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
growby: pt | |
	corner ← corner + pt
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
dragto: dest | v i |
	self blt: dest mode: storing.
	v ← dest rect: dest + self extent.
	(self minus: v) do: [:i | i clear].
	origin ← dest.
	corner ← v corner
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
maxstretch: bound | bx by boundr selfr |
	bx ← (bound corner - origin) x.
	by ← (bound corner - origin) y.
	boundr ← bx asFloat / by.
	selfr ← self width asFloat / self height.
	selfr > boundr
	  ifTrue: [self extent← bx ⌾ (bx asFloat / selfr) asInteger]
	  ifFalse: [self extent← (by asFloat * selfr) asInteger ⌾ by]
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
moveby: pt | |
	origin ← origin + pt.
	corner ← corner + pt
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
nearest: pt | |
	^((origin x max: pt x) min: corner x) ⌾ ((origin y max: pt y) min: corner y)
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
include: r | |
	 "Returns the merge with an adjacent rectangle."
	^(origin min: r origin) rect: (corner max: r corner)
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
minus: r | s yorg ycor |
	 "return Vector of Rectangles comprising
				the part of me not intersecting r "
	 "Make sure the intersection is non-empty"
	(origin ≤ r corner and: [r origin ≤ corner])
	  ifFalse: [^self inVector].
	s ← (Vector new: 4) asStream.
	r origin y > origin y
	  ifTrue: [s next← origin rect: corner x ⌾ (yorg ← r origin y)]
	  ifFalse: [yorg ← origin y].
	r corner y < corner y
	  ifTrue: [s next← origin x ⌾ (ycor ← r corner y) rect: corner]
	  ifFalse: [ycor ← corner y].
	r origin x > origin x
	  ifTrue: [s next← origin x ⌾ yorg rect: r origin x ⌾ ycor].
	r corner x < corner x
	  ifTrue: [s next← r corner x ⌾ yorg rect: corner x ⌾ ycor].
	^s contents
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
max: rect | |
	^Rectangle new origin: (origin min: rect origin) corner: (corner max: rect corner)
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
has: pt | |
	^origin ≤ pt and: [pt < corner]
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
empty | |
	^origin < corner ≡ false
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
intersect: r | |
	^Rectangle new origin: (origin max: r origin) corner: (corner min: r corner)
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
isWithin: rect | |
	 "am I equal to or contained within rect"
	^origin ≥ rect origin and: [corner ≤ rect corner]
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
intersects: r | |
	^(origin max: r origin) < (corner min: r corner)
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
/ scale | |
	^Rectangle new "Return a Rectangle which is the quotient of me and scale (which is a Rectangle, Point, or Number)" origin: origin / scale asRectOrigin corner: corner / scale asRectCorner
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
* scale | |
	^Rectangle new "Return a Rectangle which is the product of me and scale (which is a Rectangle, Point, or Number)" origin: origin * scale asRectOrigin corner: corner * scale asRectCorner
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
= r | |
	^origin = r origin and: [corner = r corner]
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
sideNearest: pt | d dmin i imin |
	dmin ← 32767.
	(0 to: 3) do:
		[:i | dmin > (d ← self side: i distanceTo: pt) abs
		  ifTrue:
			[dmin ← d.
			imin ← i]].
	^imin
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
side: side distanceTo: pt | |
	side = 0
	  ifTrue: [^pt y - origin y].
	side = 1
	  ifTrue: [^pt x - origin x].
	side = 2
	  ifTrue: [^corner y - pt y].
	side = 3
	  ifTrue: [^corner x - pt x].
	.
	user notify: 'Invalid side'
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
- delta | |
	^Rectangle new "Return a Rectangle which is the difference of me and delta (which is a Rectangle, Point, or Number)" origin: origin - delta asRectOrigin corner: corner - delta asRectCorner
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
+ delta | |
	^Rectangle new "Return a Rectangle which is the sum of me and delta (which is a Rectangle, Point, or Number)" origin: origin + delta asRectOrigin corner: corner + delta asRectCorner
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
inset: p1 and: p2 | |
	^origin + p1 rect: corner - p2
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
inset: p1 | |
	^origin + p1 rect: corner - p1
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
center | |
	^origin + corner / 2
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
bottom | |
	^corner y
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
corner | |
	^corner
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
width← w | |
	 "change my right x to make my width w"
	corner x← origin x + w
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
top | |
	^origin y
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
leftside | |
	^origin x
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
corner← t1 | |
	corner ← t1
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
height | |
	^corner y - origin y
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
width | |
	^corner x - origin x
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
extent | |
	^corner - origin
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
hash | |
	^super hash
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
size | |
	^corner - origin
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
origin | |
	^origin
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
origin← t1 | |
	origin ← t1
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
rightside | |
	^corner x
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
withEdge: side at: coord | |
	 "Returns a rectangle with one side moved."
	side = 0
	  ifTrue: [^origin x ⌾ coord rect: corner].
	side = 1
	  ifTrue: [^coord ⌾ origin y rect: corner].
	side = 2
	  ifTrue: [^origin rect: corner x ⌾ coord].
	side = 3
	  ifTrue: [^origin rect: coord ⌾ corner y].
	.
	user notify: 'Invalid side'
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
withSide: side at: pt | |
	 "Returns a rectangle with one side moved."
	side = 0
	  ifTrue: [^origin x ⌾ pt y rect: corner].
	side = 1
	  ifTrue: [^pt x ⌾ origin y rect: corner].
	side = 2
	  ifTrue: [^origin rect: corner x ⌾ pt y].
	side = 3
	  ifTrue: [^origin rect: pt x ⌾ corner y].
	.
	user notify: 'Invalid side'
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
edge: side | |
	 "Returns one side as a number." "Sides are numbered 0-3.  +1 goes counterclockwise.  lxor: 2 gets opposite side."
	side = 0
	  ifTrue: [^origin y "top"].
	side = 1
	  ifTrue: [^origin x "left"].
	side = 2
	  ifTrue: [^corner y "bottom"].
	side = 3
	  ifTrue: [^corner x "right"].
	.
	user notify: 'Invalid side'
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
side: side | |
	 "Returns one side as a rectangle." "Sides are numbered 0-3.  +1 goes counterclockwise.  Xor: 2 gets opposite side."
	side = 0
	  ifTrue: [^origin rect: corner x "top" ⌾ origin y].
	side = 1
	  ifTrue: [^origin rect: origin x "left" ⌾ corner y].
	side = 2
	  ifTrue: [^origin x "bottom" ⌾ corner y rect: corner].
	side = 3
	  ifTrue: [^corner x "right" ⌾ origin y rect: corner].
	.
	user notify: 'Invalid side'
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
height← h | |
	 "change my bottom y to make my height h"
	corner y← origin y + h
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
area | |
	^self width * self height
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
extent← extent | |
	corner ← origin + extent.
	^extent
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
maxY | |
	^corner y
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
maxX | |
	^corner x
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
minY | |
	^origin y
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
minX | |
	^origin x
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
corners | v |
	v ← Vector new: 4.
	v ◦ 1 ← origin.
	v ◦ 2 ← corner x ⌾ origin y.
	v ◦ 3 ← corner.
	v ◦ 4 ← origin x ⌾ corner y.
	^v
! !
!Rectangle methodsFor: 'Border' stamp: ''!
outline | |
	 "default border is two thick"
	self outline: 2
! !
!Rectangle methodsFor: 'Border' stamp: ''!
boxcomp | t1 |
	 "paints a border withoud disturbing interior"
	(t1 ← Rectangle new origin: origin - (2 ⌾ 2) corner: corner x + 2 ⌾ origin y) color: black mode: xoring.
	t1 moveto: origin x - 2 ⌾ corner y.
	t1 color: black mode: xoring.
	t1 origin← corner x ⌾ (origin y - 2).
	t1 color: black mode: xoring.
	t1 moveto: origin - (2 ⌾ 2).
	t1 color: black mode: xoring
! !
!Rectangle methodsFor: 'Border' stamp: ''!
outline: thick | t |
	t ← ¬1 ⌾ ¬1 * thick.
	(self inset: t) clear: black.
	self clear: white
! !
!Rectangle methodsFor: 'Border' stamp: ''!
border: thick color: color | t3 |
	 "paints a border withoud disturbing interior"
	(t3 ← Rectangle new origin: origin - (thick ⌾ thick) corner: corner x + thick ⌾ origin y) clear: color.
	t3 moveto: origin x - thick ⌾ corner y.
	t3 clear: color.
	t3 origin← corner x ⌾ (origin y - thick).
	t3 clear: color.
	t3 moveto: origin - (thick ⌾ thick).
	t3 clear: color
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsFromStream: strm | rec s |
	rec ← origin rect: origin + (self width ⌾ (16 min: self height)).
	s ← rec bitmap.
	[rec maxY ≤ corner y] whileTrueDo:
		[s fromStream: strm.
		rec bitsFromString: s.
		rec moveby: 0 ⌾ 16].
	rec minY < corner y
	  ifTrue:
		[rec corner y← corner y.
		s ← nil.
		s ← rec bitmap.
		s fromStream: strm.
		rec bitsFromString: s]
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsIntoString: bitmap mode: mode clippedBy: clipRect | t5 |
	"Store the screen bits within my area into bitmap.  If clipRect is not nil,
	then store only those bits within both myself and clipRect,
	leaving alone the other bits in bitmap"
	(t5 ← BitBlt new fromDisplay window: clipRect) effect: mode.
	t5 destForm: (Form new extent: corner - origin bits: bitmap offset: nil).
	t5 destOrigin: 0 ⌾ 0.
	t5 sourceRect: (origin rect: corner).
	t5 callBLT
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsIntoString: str mode: mode | t3 |
	(t3 ← BitBlt new fromDisplay) effect: mode.
		t3 destForm: (Form new extent: corner - origin bits: str offset: nil).
		t3 destOrigin: 0 ⌾ 0.
		t3 sourceRect: (origin rect: corner).
		t3 callBLT
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitmap | extent |
	extent ← corner - origin.
	^Bitmap new: extent y * (extent x + 15 / 16)
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
asRectangle | |
	 "Return self."
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsIntoString | str |
	str ← self bitmap.
	self bitsIntoString: str mode: storing.
	^str
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
printOn: strm | |
	strm print: origin.
	strm append: ' rect: '.
	strm print: corner
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
hardcopy: pf | |
	self hardcopy: pf thickness: 2
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsOntoStream: strm | rec s |
	rec ← origin rect: origin + (self width ⌾ (16 min: self height)).
	(s ← self bitmap) all← 0.
	[rec maxY ≤ corner y] whileTrueDo:
		[rec bitsIntoString: s.
		rec moveby: 0 ⌾ 16.
		s toStream: strm].
	rec minY < corner y
	  ifTrue:
		[rec height← corner y - rec miny.
		s ← rec bitsIntoString.
		s toStream: strm]
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsFromString: str | |
	self bitsFromString: str mode: storing "default stores bits onto display"
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
hardcopy: pf thickness: th | r |
	((self inset: 0 - th) minus: self) do: [:r | pf showrect: r color: 0]
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
asRectCorner | |
	^corner
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
asRectOrigin | |
	^origin
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitmapLength | extent |
	extent ← corner - origin.
	^extent y * (extent x + 15 / 16)
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsIntoString: str | |
	self bitsIntoString: str mode: storing "default stores bits into the string"
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitStringLength | extent |
	extent ← corner - origin.
	^2 * extent y * (extent x + 15 / 16)
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsFromString: str mode: mode | t3 |
	(t3 ← BitBlt new toDisplay) effect: mode.
		t3 sourceForm: (Form new extent: corner - origin bits: str offset: nil).
		t3 destOrigin: origin.
		t3 sourceRect: (0 ⌾ 0 rect: corner - origin).
		t3 callBLT
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsFromString: bitmap mode: mode clippedBy: clipRect | t5 |
	"Load the screen bits within my area from those stored in bitmap.  If clipRect is not nil, then load only those bits within both  myself and clipRect"
	(t5 ← BitBlt new toDisplay window: clipRect) effect: mode.
	t5 sourceForm: (Form new extent: corner - origin bits: bitmap offset: nil).
	t5 destOrigin: origin.
	t5 sourceRect: (0 ⌾ 0 rect: corner - origin).
	t5 callBLT
! !
!Rectangle methodsFor: 'Image' stamp: ''!
fillin: color mode: mode | T bits p s dirs i which |
	 "Rectangle new fromuser fillin: gray"
	T ← Turtle init.
	p ← origin + (self width ⌾ 0).
	s ← Rectangle new origin: p extent: self extent.
	dirs ← {1 ⌾ 0 , (¬1 ⌾ 0) , (0 ⌾ 1) , (0 ⌾ ¬1)}.
	bits ← s bitsIntoString.
	self blt: p mode: storing "s ← self".
	user waitbug.
	T place: user mp.
	T pendn.
	[user anybug] whileTrueDo: [ "draw seed in self"
		T goto: user mp].
	self blt: p mode: xoring "s ← seed only".
	s blt: origin mode: xoring "take seed out of self".
	user waitbug.
	[user anybug] whileTrueDo: [(0 to: 2 by: 2) do:
			[:which | (1 to: 2) "smear seed around" do: [:i | s blt: dirs ◦ (which + i) + p mode: oring].
			self blt: p mode: erasing]].
	s brush: origin mode: mode color: color "then clip to outline" "paint it in".
	s bitsFromString: bits "restore background to s"
! !
!Rectangle methodsFor: 'Image' stamp: ''!
color: color mode: mode | |
	DisplayBLTer copyRect: self toPoint: origin effect: 12 + (mode land: 3) screen: color
! !
!Rectangle methodsFor: 'Image' stamp: ''!
brush: dest mode: mode color: color clippedBy: clipRect | |
	 "Brush the screen bits within my area to the rectangle whose
		origin is dest and whose extent is the same as mine.
		If clipRect is not nil, then brush only those bits within both
		the destination rectangle and clipRect"
	DisplayBLTer copy window: clipRect;
		copyRect: self toPoint: dest effect: 8 + (mode land: 3) screen: color
! !
!Rectangle methodsFor: 'Image' stamp: ''!
blt: dest mode: mode clippedBy: clipRect | |
	 "Copy the screen bits within my area to the rectangle whose
		origin is dest and whose extent is the same as mine.
		If clipRect is not nil, then copy only those bits within both
		the destination rectangle and clipRect"
	DisplayBLTer copy window: clipRect;
		copyRect: self toPoint: dest effect: (mode land: 3) screen: black
! !
!Rectangle methodsFor: 'Image' stamp: ''!
flash | |
	self comp.
	self comp
! !
!Rectangle methodsFor: 'Image' stamp: ''!
bltcomp: dest mode: mode | |
	DisplayBLTer copyRect: self toPoint: dest effect: 4 + (mode land: 3) screen: black
! !
!Rectangle methodsFor: 'Image' stamp: ''!
rotate | size maskr spt mpt tpt data temp atab btab i unit |
	 "(0⌾0 rect: 128⌾128) rotate."
	size ← self extent x.
	spt ← size ⌾ size "size must be a power of 2".
	data ← Rectangle new origin: origin extent: spt.
	maskr ← Rectangle new origin: (mpt ← origin + (0 ⌾ size)) extent: spt.
	temp ← Rectangle new origin: (tpt ← mpt + (size ⌾ 0)) extent: spt.
	atab ← {0 ⌾ 0 , (1 ⌾ 0) , (0 ⌾ 0) , (0 ⌾ 1) , (1 ⌾ 1) , (0 ⌾ 1) , (1 ⌾ 0) , (¬1 ⌾ 0) , (1 ⌾ 0)}.
	btab ← {0 ⌾ 0 , (1 ⌾ 1) , (0 ⌾ 0) , (1 ⌾ 1) , (¬1 ⌾ ¬1) , (1 ⌾ 1)}.
	unit ← size / 2.
	maskr clear: white.
	(Rectangle new origin: mpt extent: unit ⌾ unit) clear: black.
	[unit < 1] whileFalseDo: 
		[(1 to: 3) do:
			[:i |  "flip left and right halves"
			temp clear: white.
			maskr blt: atab ◦ i * unit + tpt mode: storing.
			maskr blt: atab ◦ (3 + i) * unit + tpt mode: oring.
			data bltcomp: tpt mode: erasing.
			temp blt: atab ◦ (6 + i) * unit + origin mode: xoring].
		(1 to: 3) do:
			[:i |  "flip diagonals"
			temp clear: white.
			maskr blt: btab ◦ i * unit + tpt mode: storing.
			data bltcomp: tpt mode: erasing.
			temp blt: btab ◦ (3 + i) * unit + origin mode: xoring].
		(unit ← unit / 2) < 1
		  ifFalse:
			[maskr blt: 0 ⌾ unit + mpt mode: erasing.
			maskr blt: unit ⌾ 0 + mpt mode: erasing.
			maskr blt: unit * 2 ⌾ 0 + mpt mode: oring.
			maskr blt: 0 ⌾ (2 * unit) + mpt mode: oring]]
! !
!Rectangle methodsFor: 'Image' stamp: ''!
reverse | |
	self color: black mode: xoring
! !
!Rectangle methodsFor: 'Image' stamp: ''!
clear: color | |
	self color: color mode: storing
! !
!Rectangle methodsFor: 'Image' stamp: ''!
blt: dest mode: mode | |
	DisplayBLTer copyRect: self toPoint: dest effect: (mode land: 3) screen: black
! !
!Rectangle methodsFor: 'Image' stamp: ''!
comp | |
	self color: black mode: xoring
! !
!Rectangle methodsFor: 'Image' stamp: ''!
blowup: at by: scale spacing: spacing | extent z inc sinc slice width height dest i j spread |
	extent ← self extent.
	scale ← scale asPoint.
	spacing ← spacing asPoint.
	dest ← Rectangle new origin: at extent: extent * scale.
	z ← 1 ⌾ 0.
	width ← extent x.
	height ← 0 ⌾ extent y.
	spread ← (scale - spacing) x.
	(1 to: 2) do:
		[:i |  "first do horiz, then vert"
		inc ← z * ¬1.
		sinc ← z * scale.
		slice ← Rectangle new origin: z * width + (i = 1
				  ifTrue: [self origin]
				  ifFalse: [at]) extent: z + height.
		dest ← at + (z * (scale * width)).
		(1 to: width) do:
			[:j |  "slice it up"
			dest ← dest - sinc.
			slice moveby: inc.
			slice blt: dest mode: storing].
		slice ← Rectangle new origin: at + z extent: height + (z * (scale - 1)).
		(1 to: width) do:
			[:j |  "clear slice source"
			slice clear: white.
			slice moveby: sinc].
		slice ← Rectangle new origin: at extent: height + (z * (scale * width - 1)).
		(1 to: spread - 1) do: [:j |  "spread it out"
			slice blt: at + z mode: oring].
		z ← 0 ⌾ 1 "flip to do vertical".
		width ← extent y.
		height ← (scale * extent) x ⌾ 0.
		spread ← (scale - spacing) y]
! !
!Rectangle methodsFor: 'Image' stamp: ''!
blowup: at by: scale | z dest |
	dest ← Rectangle new origin: at extent: self extent * scale.
	((dest has: origin) or: [(dest has: corner)])
	  ifTrue:
		[z ← self bitsIntoString.
		dest outline.
		self moveto: dest origin.
		self bitsFromString: z]
	  ifFalse: [dest outline].
	self blowup: at by: scale spacing: 1
! !
!Rectangle methodsFor: 'Image' stamp: ''!
comp: color | |
	self color: color mode: xoring
! !
!Rectangle methodsFor: 'Image' stamp: ''!
brush: dest mode: mode color: color | |
	DisplayBLTer copyRect: self toPoint: dest effect: 8 + (mode land: 3) screen: color
! !
!Rectangle methodsFor: 'Image' stamp: ''!
clear | |
	 "default is backround"
	self color: background mode: storing
! !
!Rectangle methodsFor: 'Initialization' stamp: ''!
origin: t1 corner: t2 | |
	origin ← t1.
	corner ← t2
! !
!Rectangle methodsFor: 'Initialization' stamp: ''!
origin: t1 extent: extent | |
	origin ← t1.
	corner ← origin + extent
! !
!Rectangle methodsFor: 'Initialization' stamp: ''!
fromuser | t |
	origin ← OriginCursor showwhile⦂ [ "Show the origin cursor until the user presses a mouse button,
		then get my origin"
				(user waitbug)] "Show the corner cursor and complement me until the user presses
		a button again.  The loop is arranged so 
		that complementing stays on for a little while.".
	t ← origin.
	CornerCursor showwhile⦂ [([corner ← t.
		t ← user mpnext] whileTrueDo:
			[self comp.
			t ← t max: origin.
			self comp])]
! !
!Rectangle methodsFor: 'Initialization' stamp: ''!
fromuserevenword | t |
	origin ← OriginCursor showwhile⦂ [ "Show the origin cursor until the user presses a mouse button,
		then get my origin"
				(user waitbug)] "Show the corner cursor and complement me until the user presses
		a button again.  The loop is arranged so 
		that complementing stays on for a little while.".
	t ← origin.
	CornerCursor showwhile⦂ [([corner ← t.
		t ← user mpnext] whileTrueDo:
			[self comp.
			t ← (t x + 15 truncateTo: 16) ⌾ t y max: origin.
			self comp])]
! !
!Rectangle methodsFor: 'Initialization' stamp: ''!
copy | |
	^origin copy "new rectangle" rect: corner copy
! !
BitBlt subclass: #Turtle
	instanceVariableNames: 'frame location direction pen penDown inking'
	classVariableNames: ''
	category: 'Graphics'!
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
penup | |
	penDown ← false
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
place: t1 | |
	location ← t1
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
go: dist | |
	self goto: direction degreesToRadians asDirection * dist + location
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
turn: degrees | |
	direction ← direction + degrees
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
frame: t1 | |
	frame ← t1
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
width | |
	^width
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
drawLoopX: xDelta Y: yDelta | dx dy px py P i |<primitive: 77>
	 "This is the Bresenham plotting algorithm (IBM Systems Journal Vol 4 No. 1, 1965).
	It chooses a principal direction, and maintains a potential, P.
	When P's sign changes, it is time to move in the minor direction as well."
	dx ← xDelta sign.
	dy ← yDelta sign.
	px ← yDelta abs.
	py ← xDelta abs.
	super callBLT "first point".
	py > px
	  ifTrue:
		[ "more horizontal"
		P ← py / 2.
		(1 to: py) do:
			[:i | destX ← destX + dx.
			(P ← P - px) < 0
			  ifTrue:
				[destY ← destY + dy.
				P ← P + py].
			super callBLT]]
	  ifFalse:
		[ "more vertical"
		P ← px / 2.
		(1 to: px) do:
			[:i | destY ← destY + dy.
			(P ← P - py) < 0
			  ifTrue:
				[destX ← destX + dx.
				P ← P + px].
			super callBLT]]
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
pen: t1 | |
	pen ← t1
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
black | |
	self screen: black
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
init | |
	"source ← sourceRaster ← nil."
	super init.
	super toDisplay.
	frame ← user screenrect.
	self pendn.
	self color: black.
	self width: 1.
	self inking: storing.
	self home.
	self up
" | n i |
	n←Turtle init. n color: gray; width: 4; inking: oring.
	1 to: 50 do: [:i | n go: i*4; turn: 89]. "
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
erase | |
	super window clear: white
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
frame | |
	^frame
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
drawfrom: p1 to: p2 | offset delta dx dy px py P i |
	(pen Is: Integer)
	  ifTrue:
		[self effect: 16 + 12 + (inking land: 3).
		width ← height ← pen.
		offset ← pen / 2 ⌾ (pen / 2)]
	  ifFalse:
		[(pen is: Form)
		  ifTrue:
			[self effect: 16 + (inking land: 3).
			super sourceForm: pen.
			width ← pen width.
			height ← pen height.
			offset ← pen offset]
		  ifFalse: [user notify: 'pen must be a Form or integer']].
	destX ← (p1 x - offset x) asInteger.
	destY ← (p1 y - offset y) asInteger.
	self drawLoopX: (p2 x - p1 x) asInteger Y: (p2 y - p1 y) asInteger.
	(pen is: Form)
	  ifTrue: [super release: pen]
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
goto: p | old |
	old ← location.
	location ← p.
	penDown
	  ifTrue: [self drawfrom: old to: location]
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
up | |
	direction ← 270.0
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
white | |
	self screen: white
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
home | |
	location ← frame center
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
width: w | |
	self pen: w
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
color: color | |
	self screen: color
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
pendn | |
	penDown ← true
! !
!Turtle methodsFor: 'As yet unclassified' stamp: ''!
inking: t1 | |
	inking ← t1
! !
