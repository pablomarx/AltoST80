Object subclass: #BitBlt
	instanceVariableNames: 'effect screen dest destRaster destX destY
				width height source sourceRaster sourceX sourceY
				clipX clipY clipWidth clipHeight
				sourceForm destForm screenForm'
	classVariableNames: ''
	category: 'Graphics'!
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
	(screen is: Form)
	  ifTrue:
		[screenForm ← screen.
		screen ← screenForm bits]
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
height: t1 | |
	height ← t1
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
effect: anInteger | |
	NoteTaker
	  ifTrue: [effect ← #(35 39 38 36 44 45 41 33 51 55 54 52 19 23 22 20 ) ◦ ((anInteger land: 15) + 1)]
	  ifFalse: [effect ← anInteger]
! !
!BitBlt methodsFor: 'Access to Parts' stamp: ''!
sourceRaster: t1 | |
	 "length of a 'scanline' source in 16-bit words"
	sourceRaster ← t1
! !
!BitBlt methodsFor: 'Operations' stamp: ''!
stringCopy: destString from: start to: stop with: replacement from: rstart to: rstop | |
	NoteTaker
	  ifTrue: [^false].
	 "Copies equal subranges from one string to another.  Works for BitBlt parameters up to 4096. maybe too much set up for short strings.  Currently, called by String copy:to:with:from:to:"
	width ← 1 + stop - start.
	width = 0
	  ifTrue: [^destString].
	((start > 4096 or: [rstart > 4096]) or: [width ≥ 4096])
	  ifTrue: [^false].
	((width < 0 or: [width ≠ (1 + rstop - rstart)]) or: [((start < 1 or: [stop > destString length]) or: [(rstart < 1 or: [rstop > replacement length])])])
	  ifTrue: [user notify: 'illegal range or subscript']
	  ifFalse:
		[destRaster ← destY ← sourceRaster ← sourceY ← 0.
		self effect: 0.
		self screen: black.
		height ← 1.
		width ← width * 8.
		dest ← destString lock.
		destX ← start - 1 * 8.
		source ← replacement lock.
		sourceX ← rstart - 1 * 8.
		self callBLT.
		replacement unlock "mark dirty".
		destString ◦ 1 ← destString ◦ 1.
		destString unlock.
		^destString]
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
stringReplace: destString with: sourcestring from: start to: stop and: replacement from: rstart to: rstop | slock |
	NoteTaker
	  ifTrue: [^false].
	 "Works for BitBlt parameters less than 4096. Replaces a subrange of a string.  Called only by String replace:to:by:from:to:.  Concatenates into destString:
		sourcestring◦(1 to: start - 1)
		replacement◦(rstart to: rstop)
		sourcestring◦(stop + 1 to: sourcestring length).
	assumes String arguments"
	destString length = 0
	  ifTrue: [^destString].
	(replacement is: String) ≡ false
	  ifTrue: [^false].
	((stop ≥ 4096 or: [sourcestring length - stop ≥ 4096]) or: [(start + rstop - rstart ≥ 4096 or: [rstart > 4096])])
	  ifTrue: [^false].
	((start < 1 or: [stop > sourcestring length]) or: [(rstart < 1 or: [rstop > replacement length])])
	  ifTrue: [user notify: 'illegal subscript']
	  ifFalse:
		[destRaster ← destY ← sourceRaster ← sourceY ← 0.
		self effect: 0.
		self screen: black.
		height ← 1.
		dest ← destString lock.
		source ← slock ← sourcestring lock.
		width ← start - 1 * 8.
		width = 0
		  ifFalse:
			[sourceX ← destX ← 0.
			self callBLT].
		destX ← width.
		width ← 1 + rstop - rstart * 8.
		width = 0
		  ifFalse:
			[sourceX ← rstart - 1 * 8.
			source ← replacement lock.
			self callBLT.
			replacement unlock].
		destX ← destX + width.
		width ← sourcestring length - stop * 8.
		width = 0
		  ifFalse:
			[source ← slock.
			sourceX ← stop * 8.
			self callBLT].
		destString ◦ 1 ← destString ◦ 1.
		sourcestring unlock.
		destString unlock.
		^destString]
! !
!BitBlt methodsFor: 'Operations' stamp: ''!
callBLT | |<primitive: 66>
	(screen is: Form)
	  ifTrue:
		[screen ← screen halftoneInteger.
		self callBLT]
	  ifFalse: [user croak]
! !
!BitBlt methodsFor: 'Operations' stamp: ''!
installDisplay | |<primitive: 67>
	user croak "set up source=cursor, dest=display"
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
sourceForm: t1 | |
	sourceForm ← t1.
	sourceRaster ← sourceForm width + 15 / 16.
	source ← sourceForm bits
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
toDisplay | |
	self destForm: DisplayForm.
	source ≡ nil ifTrue: [self sourceForm: DisplayForm]
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
fromDisplay | |
	self sourceForm: DisplayForm
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
init | |
	self effect: 0.
	self screen: black.
	destX ← destY ← width ← height ← sourceX ← sourceY ← 0
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
window: rect | |
	 "this is public"
	NoteTaker
	  ifTrue: [self clipRect: (rect intersect: (0 ⌾ 0 rect: destForm extent))]
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
destForm: t1 | |
	destForm ← t1.
	self clipRect: (0 ⌾ 0 rect: destForm extent).
	destRaster ← destForm width + 15 / 16.
	dest ← (NoteTaker
			  ifTrue: [destForm bits]
			  ifFalse: [destForm bits lock])
! !
!BitBlt methodsFor: 'Setup' stamp: ''!
clipRect: rect | |
	clipX ← rect minX.
	clipY ← rect minY.
	clipWidth ← rect width.
	clipHeight ← rect height
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
toStream: strm | i |
	(1 to: self length) do:
		[:i | strm next← self ◦ i lshift: ¬8.
		strm next← self ◦ i land: 255]
! !
!Bitmap methodsFor: 'As yet unclassified' stamp: ''!
◦ x | |<primitive: 17>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^self instfield: x]
! !
!Bitmap methodsFor: 'As yet unclassified' stamp: ''!
fromStream: strm | i hi lo |
	(1 to: self length) do:
		[:i | hi ← strm next.
		lo ← strm next.
		self ◦ i ← (hi lshift: 8) + lo]
! !
!Bitmap methodsFor: 'As yet unclassified' stamp: ''!
◦ x ← val | |<primitive: 18>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^self instfield: x ← val]
! !
Object subclass: #Cursor
	instanceVariableNames: 'bitstr offset'
	classVariableNames: ''
	category: 'Graphics'!
!Cursor methodsFor: 'Access to Parts' stamp: ''!
offset: t1 | |
	offset ← t1
! !
!Cursor methodsFor: 'Access to Parts' stamp: ''!
extent | |
	^16 ⌾ 16
! !
!Cursor methodsFor: 'Access to Parts' stamp: ''!
bits | |
	^bitstr
! !
!Cursor methodsFor: 'Access to Parts' stamp: ''!
offset | |
	^offset
! !
!Cursor methodsFor: 'Initialization' stamp: ''!
fromString: t1 | |
	bitstr ← t1.
	self fromString: bitstr offset: 0 ⌾ 0
! !
!Cursor methodsFor: 'Initialization' stamp: ''!
fromtext: str | |
	self fromtext: str offset: 0 ⌾ 0
! !
!Cursor methodsFor: 'Initialization' stamp: ''!
fromString: t1 offset: t2 | |
	bitstr ← t1.
	offset ← t2
! !
!Cursor methodsFor: 'Initialization' stamp: ''!
asForm | |
	^Form new extent: self extent bits: bitstr offset: offset
! !
!Cursor methodsFor: 'Initialization' stamp: ''!
fromtext: str offset: t2 | i s n c |
	offset ← t2.
	bitstr ← Bitmap new: 16 "Not great, but compatible with printon.".
	s ← str asStream.
	s next.
	(1 to: 16) do:
		[:i | n ← 0.
		[(c ← s next) = 48 or: [c = 49]] whileTrueDo: [n ← (n lshift: 1) + (c - 48)].
		bitstr ◦ i ← n]
! !
!Cursor methodsFor: 'Printing' stamp: ''!
printOn: strm | i |
	strm append: 'Cursor new fromtext: '''.
	(1 to: 16) do:
		[:i | strm cr.
		bitstr ◦ i printOn: strm base: 2].
	strm append: ''' offset: '.
	strm print: offset.
	strm append: '.'
! !
!Cursor methodsFor: 'Printing' stamp: ''!
hardcopy: pf at: loc | rect |
	rect ← loc extent: 16 ⌾ 16 "print cursor image at some point location into a presssfile".
	pf setp: (pf transrect: rect) origin.
	pf bitmap: rect bits: bitstr
! !
!Cursor methodsFor: 'Printing' stamp: ''!
hardcopy: pf | |
	self hardcopy: pf at: user mp "use current cursor position" - offset
! !
!Cursor methodsFor: 'Showing' stamp: ''!
topage1 | |
	self show
! !
!Cursor methodsFor: 'Showing' stamp: ''!
showwhile⦂ expr | oldcursor value |
	oldcursor ← user currentCursor.
	self show.
	value ← expr eval.
	oldcursor show.
	^value
! !
!Cursor methodsFor: 'Showing' stamp: ''!
show | |
	user currentCursor: self
! !
Object subclass: #Form
	instanceVariableNames: 'extent bits offset figure ground'
	classVariableNames: 'blankcursor brush aurora aurorarunning SPARE over color formmenu under dotsetter black white'
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
edit: parentimage | pt f c file t6 |
	[false] whileFalseDo: 
		[ "Simple Form editor for now." "forever for now"
		pt ← parentimage mp " blink the current brush".
		BlankCursor topage1.
		self blinkbrush: parentimage.
		user redbug
		  ifTrue:
			[(parentimage contains: (pt ← self blinkbrush: parentimage))
			  ifTrue:
				[brush displayat: pt effect: color clippedBy: parentimage rectangle.
				[user redbug] whileTrueDo:
					[brush displayat: (self blinkbrush: parentimage) effect: color clippedBy: parentimage rectangle]]
			  ifFalse:
				[NormalCursor topage1.
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
					[NormalCursor topage1.
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
						NormalCursor topage1.
						^self] "exit back to the parentimage"]]]]
! !
!Form methodsFor: 'EDITING' stamp: ''!
resize: superimage | pt f |
	superimage boxcomp.
	CornerCursor topage1.
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
	NormalCursor topage1
! !
!Form methodsFor: 'EDITING' stamp: ''!
arc: parentimage | pt1 pt2 pt3 path pt |
	BlankCursor topage1 "arc tool for forms.".
	[user redbug] whileFalseDo:  [pt1 ← self blinkbrush: parentimage].
	brush displayat: pt1 effect: color clippedBy: user screenrect.
	[user nobug] whileFalseDo:  [pt2 ← self blinkbrush: parentimage].
	brush displayat: pt2 effect: color clippedBy: user screenrect.
	[user redbug] whileFalseDo:  [pt3 ← self blinkbrush: parentimage].
	brush displayat: pt3 effect: color clippedBy: user screenrect.
	path ← Path new init.
	path addarcfrom: pt1 via: pt2 to: pt3.
	path do: [:pt | brush displayat: pt effect: color clippedBy: user screenrect].
	NormalCursor topage1
! !
!Form methodsFor: 'EDITING' stamp: ''!
line: parentimage | pt1 pt2 path pt |
	BlankCursor topage1 "line tool for forms.".
	[user redbug] whileFalseDo:  [pt1 ← self blinkbrush: parentimage].
	brush displayat: pt1 effect: color clippedBy: user screenrect.
	[user nobug] whileFalseDo:  [pt2 ← self blinkbrush: parentimage].
	brush displayat: pt2 effect: color clippedBy: user screenrect.
	path ← Path new init.
	path addlinefrom: pt1 to: pt2.
	path do: [:pt | brush displayat: pt effect: color clippedBy: user screenrect].
	NormalCursor topage1
! !
!Form methodsFor: 'EDITING' stamp: ''!
newbrush: superimage | pt rect |
	OriginCursor topage1.
	user waitbug.
	pt ← superimage mp + superimage rectangle origin.
	rect ← pt rect: pt.
	CornerCursor topage1.
	[user nobug] whileFalseDo: 
		[rect reverse.
		rect reverse.
		pt ← superimage mp + superimage rectangle origin.
		rect corner← rect origin max: pt].
	brush ← Form new fromrectangle: rect.
	NormalCursor topage1
! !
!Form methodsFor: 'EDITING' stamp: ''!
blinkbrush: parentimage | pt |
	pt ← parentimage mp "to show current position of brush in the form.".
	brush displayat: parentimage rectangle origin + pt effect: 2 clippedBy: user screenrect.
	brush displayat: parentimage rectangle origin + pt effect: 2 clippedBy: user screenrect.
	^parentimage rectangle origin + pt
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
!Form methodsFor: 'FILING' stamp: ''!
write: filename | t2 |
	(t2 ← dp0 file: filename "Saves the Form in the format width,height,bits.") nextword← self width.
	t2 nextword← self height.
	t2 append: bits.
	t2 close
! !
!Form methodsFor: 'INIT' stamp: ''!
fromuserevenword | r |
	r ← Rectangle new "create a new Form whose rectangle is specified by the user,
		truncated to nearest multiple of 16 (for Spruce printing). " fromuserevenword.
	self extent: r extent.
	bits ← r bitsIntoString
! !
!Form methodsFor: 'INIT' stamp: ''!
fromuser | r |
	r ← Rectangle new "create a new Form whose rectangle is specified by the user. " fromuser.
	self extent: r extent.
	bits ← r bitsIntoString
! !
!Form methodsFor: 'INIT' stamp: ''!
close | |
	
! !
!Form methodsFor: 'INIT' stamp: ''!
extent: t1 | |
	extent ← t1.
	self extent: extent figure: 0 ground: 1 offset: 0 ⌾ 0 "creates a virtual bit map with width = (extent x) and height = (extent y) with the bits all 1."
! !
!Form methodsFor: 'INIT' stamp: ''!
classInit | |
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
fromHalftone: halftone | i j line |
	self extent: 16 ⌾ 16.
	(1 to: 4) do:
		[:i | line ← (halftone land: 15) * 4369.
		(0 to: 3) do: [:j | bits ◦ (j * 4 + i) ← line].
		halftone ← halftone lshift: ¬4]
! !
!Form methodsFor: 'INIT' stamp: ''!
halftoneInteger | screen i |
	screen ← 0.
	(1 to: 4) do: [:i | screen ← (screen lshift: 4) lor: (bits ◦ i land: 15)].
	^screen
! !
!Form methodsFor: 'INIT' stamp: ''!
fromrectangle: r | |
	self extent: r extent "creates a virtual bit map with width = (r width) and height = (r height) with the bits in r.".
	bits ← r bitsIntoString
! !
!Form methodsFor: 'INIT' stamp: ''!
fromImage: image | |
	self extent: image extent "creates a virtual bit map with width = (image width) and height = (image height) with the bits in image.".
	bits ← image rectangle bitsIntoString
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
offset: t1 | |
	offset ← t1.
	^self "set the offset of the form "
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
height | |
	^extent y "return the height of the Form"
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
extent | |
	^extent "return the extent (width⌾height) of the Form"
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
offset | |
	offset ≡ nil
	  ifTrue: [^0 ⌾ 0].
	^offset
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
ground: t1 | |
	ground ← t1 "set the ground ( color assiciated with white) for the form "
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
ground | |
	^ground "return the ground ( color assiciated with white) for the form "
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
width | |
	^extent x "return the width of the Form"
! !
!Form methodsFor: 'MODULE ACCESS' stamp: ''!
figure | |
	^figure "return the figure( color assiciated with black) for the form "
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
white | i |
	(1 to: bits length "sets all bits in the form to white ( to zeros)") do: [:i | bits ◦ i ← 0]
! !
!Form methodsFor: 'PATTERN ACCESS' stamp: ''!
black | i |
	(1 to: bits length "sets all bits in the form to black ( to ones)") do: [:i | bits ◦ i ← 0 - 1]
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
fromInstance: file | |
	extent ← file nextPoint.
	offset ← file nextPoint.
	figure ← file nextword.
	ground ← file nextword.
	bits ← file nextString.
	^self
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
pressCode | |
	^5
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
Set subclass: #Image
	instanceVariableNames: ' origin rectangle path form superimage xgrid ygrid figure ground'
	classVariableNames: 'under screen black white aurora over'
	category: 'Graphics'!
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
ground: t1 | |
	ground ← t1 "change the ground color (color associated with white) for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
figure: t1 | |
	figure ← t1 "change the figure color (color associated with black) for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
ground | |
	^ground "return the ground color (color associated with white) for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
figure | |
	^figure "return the figure color (color associated with black) for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
ygrid: t1 | |
	ygrid ← t1 "set the y gridding module for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
ygrid | |
	^ygrid "return the y gridding module for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
path: t1 | |
	path ← t1 "change the path for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
form: t1 | |
	form ← t1 "change the form for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
xgrid | |
	^xgrid "return the x gridding module for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
xgrid: t1 | |
	xgrid ← t1 "set the x gridding module for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
hash | |
	^rectangle hash
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
superimage: t1 | |
	superimage ← t1
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
path | |
	^path "return the path for this Image"
! !
!Image methodsFor: 'ACCESS TO PARTS' stamp: ''!
form | |
	^form "return the form for this Image"
! !
!Image methodsFor: 'BUILDING IMAGES' stamp: ''!
add: p and: i | s |
	rectangle ← rectangle include: ((Rectangle new "add p (set or point) and i ( Image or Form ) and expand the
		 bounding rectangle of this image." origin: p origin + origin extent: i size) include: (Rectangle new origin: p corner + origin extent: i size)).
	s ← Set default.
	s add: p.
	s add: i.
	self add: s
! !
!Image methodsFor: 'BUILDING IMAGES' stamp: ''!
addpath: p andform: f | r |
	rectangle ← rectangle include: (r ← (Rectangle new "add p (set or point) and f ( Form ) and expand the
		 bounding rectangle of this image." origin: p origin extent: f extent "+ origin") include: (Rectangle new origin: p corner - (1 ⌾ 1) extent: f extent "+ origin")).
	self add: (Image new origin: 0 ⌾ 0 rectangle: r path: p form: f figure: 1 ground: 0 xgrid: 1 ygrid: 1)
! !
!Image methodsFor: 'BUILDING IMAGES' stamp: ''!
addform: f andpath: p | r |
	self addpath: p andform: f "add p (set or point) and f ( Form ) and expand the
		 bounding rectangle of this image."
! !
!Image methodsFor: 'BUILDING IMAGES' stamp: ''!
addimage: i | r |
	rectangle ← rectangle include: (Rectangle new "add the Image i (as a subimage) and expand the
		 bounding rectangle of this image." origin: i origin + origin extent: i extent).
	self add: i
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
subimageswithin: rect | image topleft fittedimage t |
	image ← Image new "return an image containing my subimages that are within rect, 		otherwise return false." origin: rect origin extent: rect extent.
	self do:
		[:t | (t rectangle isWithin: rect)
		  ifTrue: [image addimage: (t translate: 0 ⌾ 0 - rect origin)]].
	image length = 0
	  ifTrue: [^false].
	topleft ← (image ◦ 1) rectangle origin.
	image do:
		[:t | t rectangle origin < topleft
		  ifTrue: [topleft ← t rectangle origin]].
	fittedimage ← Image new origin: topleft + rect origin extent: 1 ⌾ 1.
	image do: [:t | fittedimage addimage: (t translate: 0 ⌾ 0 - topleft)].
	^fittedimage
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
comment | |
	 "see class Set for operations (deletion,replacement,insertion etc.) on subimages ( elements)."
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
outlinesubimage: i | r |
	r ← Rectangle new "draw an outline(reversed boarder 2 units thick) about the ith subimage ." origin: self origin + (self ◦ i) origin extent: (self ◦ i) extent.
	r comp
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
smallestsubimageat: pt | i smallest slf sml |
	smallest ← false "return the index of the smallest subimage which contains pt(relative to self origin)
		otherwise return false.".
	(1 to: self length) do:
		[:i | ((self ◦ i) rectangle has: pt)
		  ifTrue:
			[smallest
			  ifTrue:
				[slf ← (self ◦ i) rectangle.
				sml ← (self ◦ smallest) rectangle.
				slf area < sml area
				  ifTrue: [smallest ← i]]
			  ifFalse: [smallest ← i]]].
	smallest
	  ifTrue: [^self ◦ smallest].
	^smallest
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
indexofsubimagebelow: yvalue | i subimage |
	(1 to: self length "return the index of the first subimage below yvalue otherwise return false.") do:
		[:i | (self ◦ i) top ≥ yvalue
		  ifTrue: [^i]].
	^false
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
replaceimage: image with: newimage | i |
	i ← self findbyrect: image "replace image with newimage in self.".
	self replaceI: i value: newimage
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
appendimage: newimage after: image | i |
	i ← self findbyrect: image "append newimage into the image after image.".
	self insertI: i + 1 value: newimage
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
indexofsubimageat: pt | i subimage |
	(1 to: self length "return the index of the subimage which contains pt(relative to self origin)
		otherwise return false.") do:
		[:i | subimage ← self ◦ i.
		(subimage rectangle has: pt)
		  ifTrue: [^i]].
	^false
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
edit: t1 | blackdot pt indexofsubimage subimage |
	superimage ← t1.
	nil ≡ form "eventually a general Image manipulator for now
			just passes control to its subimages."
	  ifTrue:
		[user waitnobug.
		[1 = 2] whileFalseDo: 
			[ "until bug occurs outside rectangle"
			(false = (rectangle has: user mp) and: [user anybug])
			  ifTrue: [^self].
			user kbck
			  ifTrue: [self kbd]
			  ifFalse:
				[user redbug
				  ifTrue:
					[indexofsubimage ← self smallestsubimageat: user mp - self rectangle origin.
					indexofsubimage
					  ifTrue:
						[subimage ← self ◦ indexofsubimage.
						subimage translate: self origin.
						subimage edit: self.
						subimage translate: 0 ⌾ 0 - self origin]]
				  ifFalse:
					[user yellowbug
					  ifTrue: [self yellowbug]]]]]
	  ifFalse:
		[.
		form ← form edit: self.
		(form is: Form)
		  ifFalse: [ "origin ←  (form origin) copy."
			rectangle ← form frame copy].
		^self]
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
highlite | r i |
	(1 to: position) do:
		[:i |  "reverse the ith subimage ."
		r ← Rectangle new origin: self origin + (self ◦ i) origin extent: (self ◦ i) extent.
		r comp]
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
substitute: form1 for: form2 | i |
	(1 to: self length "everywhere in the imagesubstitute form1 for form2") do:
		[:i | self ◦ i ◦ 2 ≡ form2
		  ifTrue: [self ◦ i ◦ 2 ← form1]]
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
subimageat: pt | i |
	i ← self indexofsubimageat: pt "return the  subimage which contains pt (relative to self origin)
		otherwise return false.".
	i
	  ifTrue: [^self ◦ i].
	^false
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
subimage: i | sub s |
	sub ← self ◦ i "return the ith subimage." s← Image new at: self origin + (sub ◦ 1) origin.
	s add: 0 ⌾ 0 and: [sub ◦ 2].
	^s
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
deletesubimage: i | |
	self deleteindex: i "delete the ith subimage."
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
deleteimage: i | subimage |
	subimage ← self ◦ i "delete the i th subimage and recompute the bounding rectangle of the Image".
	self deleteI: i.
	(subimage rectangle isWithin: rectangle)
	  ifFalse: [self resize "recompute bounding rectangle"]
! !
!Image methodsFor: 'CHANGING IMAGES' stamp: ''!
findbyrect: image | i |
	(1 to: position) do:
		[:i | (array ◦ i) rectangle = image rectangle
		  ifTrue: [^i]].
	^0
! !
!Image methodsFor: 'DISPLAY' stamp: ''!
blink | |
	self display: 2 "blink the image".
	self display: 2
! !
!Image methodsFor: 'DISPLAY' stamp: ''!
quickDisplayAt: pt scale: scal offset: delta | i rect x1 y1 x2 y2 |
	x1 ← (scal * (rectangle minX "outline me and all of the subimages in this image in given scale" + pt x) + delta x) asInteger.
	y1 ← (scal * (rectangle minY + pt y) + delta y) asInteger.
	x2 ← (scal * (rectangle maxX + pt x) + delta x) asInteger.
	y2 ← (scal * (rectangle maxY + pt y) + delta y) asInteger.
	rect ← x1 ⌾ y1 rect: x2 ⌾ y2.
	rect outline.
	pt ← pt + origin.
	(1 to: self length) do:
		[:i | rect ← (self ◦ i) rectangle.
		x1 ← (scal * (rect minX + pt x) + delta x) asInteger.
		y1 ← (scal * (rect minY + pt y) + delta y) asInteger.
		x2 ← (scal * (rect maxX + pt x) + delta x) asInteger.
		y2 ← (scal * (rect maxY + pt y) + delta y) asInteger.
		rect ← x1 ⌾ y1 rect: x2 ⌾ y2.
		rect color: gray mode: oring]
! !
!Image methodsFor: 'DISPLAY' stamp: ''!
display: effect | |
	self displayat: 0 ⌾ 0 "display all of the forms in the image on the screen 
	effect = 0 ⇒ store
	effect = 1 ⇒ or
	effect = 2 ⇒ xor
	effect = 3 ⇒ and complement
" effect: effect clippedBy: user screenrect
! !
!Image methodsFor: 'DISPLAY' stamp: ''!
displayat: pt effect: effect clippedBy: cliprect | i |
	 "display all of the subimages in this image "
	nil ≡ form
	  ifFalse: [form displayat: path + pt + origin effect: effect clippedBy: cliprect].
	(1 to: self length) do: [:i | self ◦ i displayat: pt + origin effect: effect clippedBy: cliprect]
! !
!Image methodsFor: 'DISPLAY' stamp: ''!
display | |
	self displayat: 0 ⌾ 0 "display all of the forms in the image on the screen " effect: 0 clippedBy: user screenrect
! !
!Image methodsFor: 'Fist and last' stamp: ''!
close | im |
	array ≡ nil
	  ifFalse: [self asArray notNil do: [:im | im close]].
	superimage ← nil.
	form ← nil.
	self vector: 0
! !
!Image methodsFor: 'INIT' stamp: ''!
rectanglefromuser | f pt r |
	r ← Rectangle new "create a  Rectangle  specified by the user and origin and corner are gridded. ".
	f ← Form new extent: xgrid ⌾ ygrid.
	f black.
	user waitnobug.
	[user anybug] whileFalseDo: 
		[r origin← self blink: f].
	[user nobug] whileFalseDo: 
		[r corner← self mp + rectangle origin max: r origin + f extent.
		r reverse.
		r reverse].
	^r
! !
!Image methodsFor: 'INIT' stamp: ''!
blink: t1 | pt |
	form ← t1.
	pt ← self mp "to show current gridded position of the form... returns abs position.".
	form displayat: rectangle origin + pt effect: 2 clippedBy: user screenrect.
	form displayat: rectangle origin + pt effect: 2 clippedBy: user screenrect.
	^self rectangle origin + pt
! !
!Image methodsFor: 'INIT' stamp: ''!
fromuser | |
	rectangle ← Rectangle new "create a new Image whose rectangle is specified by the user. " fromuser.
	self origin: rectangle origin rectangle: rectangle path: rectangle origin form: (Form new fromrectangle: rectangle) figure: 1 ground: 0 xgrid: 1 ygrid: 1
! !
!Image methodsFor: 'INIT' stamp: ''!
classInit | |
	black ← 0 - 1 "sets up black and white as colors and over ,under and reverse as modes
	also initializes the name screen as an image the size of the display".
	white ← 0.
	over ← 0.
	under ← 1.
	reverse ← 2.
	screen ← Image new origin: user screenrect origin extent: user screenrect extent.
	aurora ← nil "Aurora new".
	aurorarunning ← false
! !
!Image methodsFor: 'INIT' stamp: ''!
origin: t1 extent: extent | |
	origin ← t1.
	self origin: origin copy "create a new Image at origin with extent (width⌾height). " rectangle: (Rectangle new origin: origin extent: extent) path: nil form: nil figure: 1 ground: 0 xgrid: 1 ygrid: 1
! !
!Image methodsFor: 'INIT' stamp: ''!
origin: t1 rectangle: t2 path: t3 form: t4 | |
	origin ← t1.
	rectangle ← t2.
	path ← t3.
	form ← t4.
	self origin: origin rectangle: rectangle path: path form: form figure: 1 ground: 0 xgrid: 1 ygrid: 1 "basic message to create a new instance."
! !
!Image methodsFor: 'INIT' stamp: ''!
origin: t1 rectangle: t2 path: t3 form: t4 figure: t5 ground: t6 xgrid: t7 ygrid: t8 | |
	origin ← t1.
	rectangle ← t2.
	path ← t3.
	form ← t4.
	figure ← t5.
	ground ← t6.
	xgrid ← t7.
	ygrid ← t8.
	self default "basic message to create a new instance."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
rectangle | |
	^rectangle "return the rectangle that bounds the Image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
leftside | |
	^rectangle origin "return the leftmost x of the bounding rectangle of theImage." x
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
width | |
	^rectangle extent "return the width of the Image." x
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
corner← pt | |
	rectangle corner← pt "modify the corner of the Image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
top | |
	^rectangle origin "return the top y of the bounding rectangle of theImage." y
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
resize | i |
	 " Recompute the bounding rectangle of the Image"
	nil ≡ form
	  ifTrue: [rectangle ← Rectangle new origin: origin extent: 1 ⌾ 1]
	  ifFalse: [rectangle ← Rectangle new origin: origin extent: form extent].
	(1 to: self length) do: [:i | rectangle ← rectangle include: (self ◦ i) rectangle]
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
center | |
	^rectangle center "return the center of the Image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
origin: t1 | |
	origin ← t1 "change the origin of the image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
bottom | |
	^rectangle corner "return the bottom y of the bounding rectangle of theImage." y
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
height | |
	^rectangle extent "return the height of the Image." y
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
corner | |
	^rectangle corner "return the corner of the Image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
origin | |
	^origin "return the origin of the image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
extent | |
	^rectangle extent "return the extent (width,height) of the Image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
rectangle: r | |
	rectangle ← r "redefine rectangle that bounds the Image."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
contains: pt | |
	^rectangle has: pt "return true if the  bounding rectangle for the Image contains pt.."
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
rightside | |
	^rectangle corner "return the rightmost x of the bounding rectangle of theImage." x
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
= image | |
	^rectangle = image rectangle
! !
!Image methodsFor: 'MODULE ACCESS' stamp: ''!
superimage | |
	^superimage "return the superimage (Image containing) of this Image."
! !
!Image methodsFor: 'PATTERN ACCESS' stamp: ''!
color: color effect: effect | |
	rectangle color: color mode: effect "basic rectangle call to blt.".
	aurorarunning
	  ifTrue: [user displayoffwhile⦂ [
			(aurora destination: rectangle.
			aurora source: rectangle.
			aurora figure: figure.
			aurora ground: ground.
			aurora function: 1103 "AoverB".
			aurora doit)]]
! !
!Image methodsFor: 'PATTERN ACCESS' stamp: ''!
boxcomp | |
	rectangle comp "border without disturbing the interior."
! !
!Image methodsFor: 'PATTERN ACCESS' stamp: ''!
reverse | |
	self color: black effect: 2 "reverse  the image (black to white and white to black)"
! !
!Image methodsFor: 'PATTERN ACCESS' stamp: ''!
gray | |
	self color: gray effect: storing "gray out the image"
! !
!Image methodsFor: 'PATTERN ACCESS' stamp: ''!
white | |
	(form is: BorderedText) "white out the image"
	  ifTrue: [(rectangle inset: ¬1 ⌾ ¬1 and: [¬1 ⌾ ¬1]) clear: 0]
	  ifFalse:
		[self color: white effect: over]
! !
!Image methodsFor: 'PATTERN ACCESS' stamp: ''!
black | |
	self color: black effect: 0 "black out the image"
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
presson: press in: r | yvalue t h rect |
	(self length > 0 and: [r height < (h ← press scale * self height)])
	  ifTrue: [^self].
	 "try on next page"
	self hidePress: press complete: ¬1.
	self do:
		[:t | yvalue ← t presson: press in: r "if subimage didn't fit, print version will be clipped,
		but entire subimage will be stored".
		t hidePress: press complete: ((yvalue Is: Integer)
		  ifTrue: [0]
		  ifFalse: [1])].
	form ≡ nil
	  ifTrue: [^r corner y - h].
	rect ← r copy.
	rect corner y← rect corner y - (path y * press scale) "form will be hidden by Image presson:in: or PressPrinter print:in:".
	.
	^form presson: press in: rect
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
printOn: strm | t |
	strm append: 'an Image: '.
	(array is: String)
	  ifTrue: [strm space append: self]
	  ifFalse: [self do: [:t | strm space print: t]]
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
fromPress: press value: s | numberofsubimages i code t |
	self default "retrieves and builds an instance of class Image from a press file".
	numberofsubimages ← s nextword.
	origin ← s nextPoint.
	i ← s nextPoint.
	rectangle ← i rect: s nextPoint.
	xgrid ← s nextword.
	ygrid ← s nextword.
	figure ← s nextword.
	ground ← s nextword.
	form ← s next.
	path ← s next.
	(1 to: numberofsubimages) do:
		[:i | s ← press nextControl asStream.
		code ← s next.
		t ← Image new.
		code = t pressCode
		  ifTrue: [self addimage: (t fromPress: press value: s)]
		  ifFalse: [user notify: 'subimage not Image']].
	form = 0
	  ifTrue: [form ← nil]
	  ifFalse:
		[s ← press nextControl asStream.
		code ← s next.
		form ← (code = 4
				  ifTrue: [TextImage new]
				  ifFalse:
					[code = 5
					  ifTrue: [Form new]
					  ifFalse:
						[code = 6
						  ifTrue: [BorderedText new]
						  ifFalse: [false]]]).
		(form and: [code = form pressCode])
		  ifTrue:
			[code = 4
			  ifTrue: [form frame← rectangle copy].
			code = 6
			  ifTrue: [form frame← rectangle copy].
			form fromPress: press value: s]
		  ifFalse: [user notify: 'illegal form']].
	path = 0
	  ifTrue: [path ← nil]
	  ifFalse:
		[s ← press nextControl asStream.
		code ← s next.
		path ← (code = 6
				  ifTrue: [Path new]
				  ifFalse:
					[code = 7
					  ifTrue: [Point new]
					  ifFalse: [false]]).
		(path and: [code = path pressCode])
		  ifTrue: [path fromPress: press value: s]
		  ifFalse: [user notify: 'illegal path']]
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
hidePress: press complete: c | |
	c ≥ 0
	  ifTrue:
		[ "called from PressPrinter print:in:"
		form ≡ nil
		  ifFalse: [ "already done"
			form hidePress: press complete: c].
		path ≡ nil
		  ifFalse: [path hidePress: press complete: c]]
	  ifFalse: [ "¬1. called from Image presson:in:"
		press skipcode: self pressCode data: (self hideData: c)]
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
pressCode | |
	^1
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
hideData: complete | s t3 |
	(t3 ← Stream new "stores an instance of class Image on a press file. ignore complete") of: (s ← String new: 24).
	t3 nextword← self length "number of subimages".
	t3 nextPoint← origin.
	t3 nextPoint← rectangle origin.
	t3 nextPoint← rectangle corner.
	t3 nextword← xgrid.
	t3 nextword← ygrid.
	t3 nextword← figure.
	t3 nextword← ground.
	t3 next← (form ≡ nil
	  ifTrue: [0]
	  ifFalse: [1]).
	t3 next← (path ≡ nil
	  ifTrue: [0]
	  ifFalse: [1]).
	^s
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
kbd | |
	user kbd " default response for Images.".
	self reverse.
	self reverse
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
copy | im i |
	im ← Image new origin: origin copy rectangle: rectangle copy path: path copy form: form copy figure: figure copy ground: ground copy xgrid: xgrid copy ygrid: ygrid copy.
	(1 to: self length) do: [:i | im add: (self ◦ i) copy].
	^im
! !
!Image methodsFor: 'SYSTEM' stamp: ''!
mp | p |
	p ← user mp " returns a gridded point relative to my rectangle.".
	p x← p x - rectangle origin x | xgrid.
	p y← p y - rectangle origin y | ygrid.
	^p
! !
!Image methodsFor: 'TRANSFORMATIONS' stamp: ''!
griddedpoint: pt | |
	^pt x | xgrid ⌾ (pt y | ygrid)
! !
!Image methodsFor: 'TRANSFORMATIONS' stamp: ''!
translateto: pt | |
	self translate: pt - origin "move the Image to pt."
! !
!Image methodsFor: 'TRANSFORMATIONS' stamp: ''!
translate: delta | |
	rectangle ← rectangle translate: delta "translate the origin and bounding rectangle of the Image.".
	origin translate: delta
! !
!Image methodsFor: 'TRANSFORMATIONS' stamp: ''!
normalize | delta i |
	 "recompute origin, rectangle and path so that: path origin = 0⌾0."
	nil ≡ path
	  ifFalse:
		[delta ← path origin copy.
		path normalize.
		origin translate: delta].
	(1 to: self length) do: [:i | (self ◦ i) normalize]
! !
Object subclass: #Point
	instanceVariableNames: 'x y'
	classVariableNames: ''
	category: 'Graphics'!
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
hash | |
	^(x lshift: 2) lxor: y
! !
!Point methodsFor: 'Access to parts' stamp: ''!
y | |
	^y
! !
!Point methodsFor: 'Access to parts' stamp: ''!
x | |
	^x
! !
!Point methodsFor: 'Access to parts' stamp: ''!
x← t1 | |
	x ← t1
! !
!Point methodsFor: 'Access to parts' stamp: ''!
y← t1 | |
	y ← t1
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
normal | n |
	 "unit vector rotated 90 deg clockwise"
	n ← y asFloat neg ⌾ x asFloat.
	^n / n length
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
min: t | |
	^Point new x: (x min: t x) y: (y min: t y)
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
translate: delta | |
	x ← x + delta x "increment self by delta".
	y ← y + delta y
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
max: t | |
	^Point new x: (x max: t x) y: (y max: t y)
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
abs | |
	 "absolute value of a point"
	^Point new x: x abs y: y abs
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
dist: pt | t |
	 "distance (Manhattan norm) between pt and self"
	t ← (pt - self) abs.
	^t x + t y
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
| grid | |
	^Point new x: x | grid y: y | grid
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
≥ pt | |
	^x ≥ pt x and: [y ≥ pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
> pt | |
	^x > pt x and: [y > pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
≤ pt | |
	^x ≤ pt x and: [y ≤ pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
= pt | |
	^x = pt x and: [y = pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
- delta | |
	^Point new "Return a Point that is the difference of me and delta (which is a Point or Number)" x: x - delta asPtX y: y - delta asPtY
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
< pt | |
	^x < pt x and: [y < pt y]
! !
!Point methodsFor: 'Arithmetic' stamp: ''!
+ delta | |
	^Point new "Return a Point that is the sum of me and delta (which is a Point or Number)" x: x + delta asPtX y: y + delta asPtY
! !
!Point methodsFor: 'Conversion' stamp: ''!
printOn: strm | |
	strm print: x.
	strm append: '⌾'.
	strm print: y
! !
!Point methodsFor: 'Conversion' stamp: ''!
asPoint | |
	 "Return self."
! !
!Point methodsFor: 'Conversion' stamp: ''!
extent: p | |
	 "infix creation of rectangles"
	^Rectangle new origin: self extent: p
! !
!Point methodsFor: 'Conversion' stamp: ''!
height | |
	^1
! !
!Point methodsFor: 'Conversion' stamp: ''!
corner | |
	^self + (1 ⌾ 1)
! !
!Point methodsFor: 'Conversion' stamp: ''!
origin | |
	^self
! !
!Point methodsFor: 'Conversion' stamp: ''!
extent | |
	^1 ⌾ 1
! !
!Point methodsFor: 'Conversion' stamp: ''!
asRectOrigin | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Point methodsFor: 'Conversion' stamp: ''!
asRectCorner | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Point methodsFor: 'Conversion' stamp: ''!
asRectangle | |
	^self rect: self "Return a Rectangle with me as both origin and corner."
! !
!Point methodsFor: 'Conversion' stamp: ''!
width | |
	^1
! !
!Point methodsFor: 'Conversion' stamp: ''!
asPtY | |
	^y
! !
!Point methodsFor: 'Conversion' stamp: ''!
rect: p | |
	 "infix creation of rectangles"
	^Rectangle new origin: self corner: p
! !
!Point methodsFor: 'Conversion' stamp: ''!
asPtX | |
	^x
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
fromPress: press value: s | |
	x ← s nextword.
	y ← s nextword
! !
!Point methodsFor: 'SYSTEM' stamp: ''!
hidePress: press complete: c | |
	press skipcode: self pressCode data: (self hideData: c)
! !
!Point methodsFor: 'SYSTEM' stamp: ''!
pressCode | |
	^7
! !
!Point methodsFor: 'SYSTEM' stamp: ''!
hideData: complete | s t3 |
	(t3 ← Stream new) of: (s ← String new: 4).
	t3 nextPoint← self.
	^s
! !
Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	category: 'Graphics'!
!Rectangle methodsFor: 'Altering' stamp: ''!
translateto: pt | |
	self translate: pt - origin
! !
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
usermove | |
	self usermove: user screenrect
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
moveby: pt | |
	origin ← origin + pt.
	corner ← corner + pt
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
moveto: pt | |
	corner ← corner + pt - origin.
	origin ← pt
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
growto: t1 | |
	corner ← t1
! !
!Rectangle methodsFor: 'Altering' stamp: ''!
growby: pt | |
	corner ← corner + pt
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
center | |
	^origin + corner / 2
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
inset: p1 | |
	^origin + p1 rect: corner - p1
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
inset: p1 and: p2 | |
	^origin + p1 rect: corner - p2
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
intersect: r | |
	^Rectangle new origin: (origin max: r origin) corner: (corner min: r corner)
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
- delta | |
	^Rectangle new "Return a Rectangle which is the difference of me and delta (which is a Rectangle, Point, or Number)" origin: origin - delta asRectOrigin corner: corner - delta asRectCorner
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
+ delta | |
	^Rectangle new "Return a Rectangle which is the sum of me and delta (which is a Rectangle, Point, or Number)" origin: origin + delta asRectOrigin corner: corner + delta asRectCorner
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
empty | |
	^origin < corner ≡ false
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
max: rect | |
	^Rectangle new origin: (origin min: rect origin) corner: (corner max: rect corner)
! !
!Rectangle methodsFor: 'Arithmetic' stamp: ''!
has: pt | |
	^origin ≤ pt and: [pt < corner]
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
height | |
	^corner y - origin y
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
corner | |
	^corner
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
extent | |
	^corner - origin
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
origin | |
	^origin
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
width← w | |
	 "change my right x to make my width w"
	corner x← origin x + w
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
area | |
	^self width * self height
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
width | |
	^corner x - origin x
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
extent← extent | |
	corner ← origin + extent.
	^extent
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
corner← t1 | |
	corner ← t1
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
origin← t1 | |
	origin ← t1
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
height← h | |
	 "change my bottom y to make my height h"
	corner y← origin y + h
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
!Rectangle methodsFor: 'Aspects' stamp: ''!
bottom | |
	^corner y
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
maxX | |
	^corner x
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
maxY | |
	^corner y
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
size | |
	^corner - origin
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
hash | |
	^super hash
! !
!Rectangle methodsFor: 'Aspects' stamp: ''!
rightside | |
	^corner x
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
outline | |
	 "default border is two thick"
	self outline: 2
! !
!Rectangle methodsFor: 'Border' stamp: ''!
outline: thick | t |
	t ← ¬1 ⌾ ¬1 * thick.
	(self inset: t) clear: black.
	self clear: white
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
bitsFromString: bitmap mode: mode clippedBy: clipRect | t5 |
	"Load the screen bits within my area from those stored in bitmap.  If clipRect is not nil, then load only those bits within both  myself and clipRect"
	(t5 ← BitBlt new toDisplay window: clipRect) effect: mode.
	t5 sourceForm: (Form new extent: corner - origin bits: bitmap offset: nil).
	t5 destOrigin: origin.
	t5 sourceRect: (0 ⌾ 0 rect: corner - origin).
	t5 callBLT
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
printOn: strm | |
	strm print: origin.
	strm append: ' rect: '.
	strm print: corner
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsIntoString: str mode: mode | t3 |
	NoteTaker
	  ifTrue:
		[(t3 ← BitBlt new fromDisplay) effect: mode.
		t3 destForm: (Form new extent: corner - origin bits: str offset: nil).
		t3 destOrigin: 0 ⌾ 0.
		t3 sourceRect: (origin rect: corner).
		t3 callBLT]
	  ifFalse: [self ALTObitsIntoString: str mode: mode]
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsFromString: str mode: mode | t3 |
	NoteTaker
	  ifTrue:
		[(t3 ← BitBlt new toDisplay) effect: mode.
		t3 sourceForm: (Form new extent: corner - origin bits: str offset: nil).
		t3 destOrigin: origin.
		t3 sourceRect: (0 ⌾ 0 rect: corner - origin).
		t3 callBLT]
	  ifFalse: [self ALTObitsFromString: str mode: mode]
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsFromString: str | |
	self bitsFromString: str mode: storing "default stores bits onto display"
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsIntoString: str | |
	self bitsIntoString: str mode: storing "default stores bits into the string"
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitsIntoString | str |
	str ← self bitmap.
	self bitsIntoString: str mode: storing.
	^str
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
bitStringLength | extent |
	extent ← corner - origin.
	^2 * extent y * (extent x + 15 / 16)
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
asRectOrigin | |
	^origin
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
asRectCorner | |
	^corner
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
asRectangle | |
	 "Return self."
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitmap | extent |
	extent ← corner - origin.
	^Bitmap new: extent y * (extent x + 15 / 16)
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
hardcopy: pf | |
	self hardcopy: pf thickness: 2
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
hardcopy: pf thickness: th | r |
	((self inset: 0 - th) minus: self) do: [:r | pf showrect: r color: 0]
! !
!Rectangle methodsFor: 'Conversion' stamp: ''!
bitmapLength | extent |
	extent ← corner - origin.
	^extent y * (extent x + 15 / 16)
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
		copyRect: self toPoint: dest effect: (mode land: 3) screen: 0
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
clear: color | |
	self color: color mode: storing
! !
!Rectangle methodsFor: 'Image' stamp: ''!
color: color mode: mode | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: origin effect: 12 + (mode land: 3) screen: color]
	  ifFalse: [self ALTOcolor: color mode: mode]
! !
!Rectangle methodsFor: 'Image' stamp: ''!
bltcomp: dest mode: mode | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: dest effect: 4 + (mode land: 3) screen: 0]
	  ifFalse: [self ALTObltcomp: dest mode: mode]
! !
!Rectangle methodsFor: 'Image' stamp: ''!
blt: dest mode: mode | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: dest effect: (mode land: 3) screen: 0]
	  ifFalse: [self ALTOblt: dest mode: mode]
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
brush: dest mode: mode color: color | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: dest effect: 8 + (mode land: 3) screen: color]
	  ifFalse: [self ALTObrush: dest mode: mode color: color]
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
flash | |
	self comp.
	self comp
! !
!Rectangle methodsFor: 'Image' stamp: ''!
comp: color | |
	self color: color mode: xoring
! !
!Rectangle methodsFor: 'Image' stamp: ''!
clear | |
	 "default is backround"
	self color: background mode: storing
! !
!Rectangle methodsFor: 'Image' stamp: ''!
comp | |
	self color: black mode: xoring
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
			t ← t x + 15 | 16 ⌾ t y max: origin.
			self comp])]
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
copy | |
	^origin copy "new rectangle" rect: corner copy
! !
