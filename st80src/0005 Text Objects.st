Object subclass: #CharLine
	instanceVariableNames: 'starti stopi spaces padwidth'
	classVariableNames: ''
	category: 'Text Objects'!
!CharLine methodsFor: 'Access to Parts' stamp: ''!
starti | |
	^starti
! !
!CharLine methodsFor: 'Access to Parts' stamp: ''!
spaces | |
	^spaces
! !
!CharLine methodsFor: 'Access to Parts' stamp: ''!
stopi | |
	^stopi
! !
!CharLine methodsFor: 'Access to Parts' stamp: ''!
padwidth | |
	^padwidth
! !
!CharLine methodsFor: 'Access to Parts' stamp: ''!
stopi← t1 | |
	stopi ← t1
! !
!CharLine methodsFor: 'Initialization' stamp: ''!
starti: t1 stopi: t2 spaces: t3 padwidth: t4 | |
	starti ← t1.
	stopi ← t2.
	spaces ← t3.
	padwidth ← t4
! !
!CharLine methodsFor: 'Operations' stamp: ''!
= line | |
	^(starti = line starti and: [stopi = line stopi]) and: [(spaces = line spaces and: [padwidth = line padwidth])]
! !
!CharLine methodsFor: 'Operations' stamp: ''!
slide: delta | |
	starti ← starti + delta.
	stopi ← stopi + delta
! !
Stream subclass: #Dispframe
	instanceVariableNames: 'text'
	classVariableNames: ''
	category: 'Text Objects'!
!Dispframe methodsFor: 'Access to Parts' stamp: ''!
text: t1 | |
	text ← t1
! !
!Dispframe methodsFor: 'Access to Parts' stamp: ''!
text | |
	^text
! !
!Dispframe methodsFor: 'Access to Parts' stamp: ''!
frame | |
	^text frame
! !
!Dispframe methodsFor: 'Dialog' stamp: ''!
read | t |
	 "false if ctrl-d, all input since prompt if "
	self next← prompt.
	self show.
	[user kbck
	  ifTrue: [t ← self kbd]
	  ifFalse: [false]] whileFalseDo:  [].
	t ≡ nil
	  ifTrue: [^false].
	^t
! !
!Dispframe methodsFor: 'Dialog' stamp: ''!
prompt | |
	self cr.
	self next← prompt.
	self show
! !
!Dispframe methodsFor: 'Dialog' stamp: ''!
ev | t |
	[self cr.
	t ← self request: ''] whileTrueDo:
		[self space.
		self print: nil ⓢ t].
	^false
! !
!Dispframe methodsFor: 'Dialog' stamp: ''!
request: s | |
	 "false if ctrl-d, all input since prompt if "
	self append: s.
	^self read
! !
!Dispframe methodsFor: 'Dialog' stamp: ''!
kbd | n t |
	 "false if user pauses, nil if ctrl-d, all input since prompt if "
	[user kbck] whileTrueDo:
		[t ← user kbd.
		t = 132
		  ifTrue:
			[self append: 'done.'.
			self show.
			^nil].
		 "ctl-d for done"
		t = 8
		  ifTrue:
			[self last = prompt
			  ifFalse: [self skip: ¬1]]
		  ifFalse:
			[ "backspace"
			t = 30
			  ifTrue:
				[n ← array ◦ (position to: 1 by: ¬1) find: prompt.
				n = 0
				  ifTrue:
					[self append: 'lost beginning'.
					self prompt]
				  ifFalse:
					[t ← self last: n - 1.
					self next← doit.
					self show.
					^t]]
			  ifFalse:
				[ "do-it (LF)"
				t = 145
				  ifTrue:
					[self last = prompt
					  ifFalse:
						[self skip: ¬1 "ctl-w for backspace word".
						[position > 0 and: [self last tokenish]] whileTrueDo: [self skip: ¬1]]]
				  ifFalse:
					[t = 151
					  ifTrue:
						[self reset.
						self prompt]
					  ifFalse: [ "ctl-x clears frame"
						self next← t]]]]].
	self show.
	^false
! !
!Dispframe methodsFor: 'Image' stamp: ''!
outline | |
	text window outline: 2
! !
!Dispframe methodsFor: 'Image' stamp: ''!
clear | |
	self reset.
	self show
! !
!Dispframe methodsFor: 'Image' stamp: ''!
show | t |
	text show: self contents asParagraph.
	[text lastshown ≥ position] whileFalseDo: 
		[position < (t ← text scrolln: 1)
		  ifFalse:
			[t ← array copy: t + 1 to: position.
			text show: t asParagraph.
			position ← 0.
			self append: t "self dequeue: (text scrolln: 1).
		text show: self contents"]]
! !
!Dispframe methodsFor: 'Image' stamp: ''!
moveto: pt | |
	(text window inset: ¬2 ⌾ ¬2) dragto: pt - (¬2 ⌾ ¬2)
! !
!Dispframe methodsFor: 'Initialization' stamp: ''!
frame← r | |
	text para: nil frame: r
! !
!Dispframe methodsFor: 'Initialization' stamp: ''!
init | |
	text ← Textframe new.
	self of: (String new: 16)
! !
!Dispframe methodsFor: 'Initialization' stamp: ''!
classInit | |
	prompt ← '' ◦ 1.
	doit ← '' ◦ 1
! !
!Dispframe methodsFor: 'Initialization' stamp: ''!
rect: r | |
	self init.
	self frame← r.
	self clear
! !
!Dispframe methodsFor: 'Scheduler' stamp: ''!
firsttime | |
	(text window has: user mp)
	  ifTrue:
		[self outline.
		self prompt]
	  ifFalse: [^false]
! !
!Dispframe methodsFor: 'Scheduler' stamp: ''!
leave | |
	
! !
!Dispframe methodsFor: 'Scheduler' stamp: ''!
lasttime | |
	self last = prompt
	  ifTrue:
		[self skip: ¬2.
		self show].
	^user bluebug ≡ false
! !
!Dispframe methodsFor: 'Scheduler' stamp: ''!
eachtime | t |
	(text window has: user mp)
	  ifTrue:
		[user kbck
		  ifTrue:
			[(t ← self kbd)
			  ifTrue:
				[t ≡ nil
				  ifFalse:
					[self space.
					self print: nil ⓢ t].
				self prompt]]
		  ifFalse:
			[user bluebug
			  ifTrue: [^false]]]
	  ifFalse:
		[user anybug
		  ifTrue: [^false]]
! !
TokenCollector subclass: #FieldNameCollector
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Text Objects'!
!FieldNameCollector methodsFor: 'Invalid fields' stamp: ''!
next← value | |
	user notify: 'Invalid field name: ' + value asString
! !
!FieldNameCollector methodsFor: 'Invalid fields' stamp: ''!
rightparen | |
	self next← ')' "just for error message"
! !
!FieldNameCollector methodsFor: 'Invalid fields' stamp: ''!
leftparen | |
	self next← '(' "just for error message"
! !
!FieldNameCollector methodsFor: 'Valid fields' stamp: ''!
identifier: s | |
	sink next← s
! !
Object subclass: #Font
	instanceVariableNames: 'name minascii maxascii maxwidth length ascent descent xoffset raster glyphs xtable'
	classVariableNames: ''
	category: 'Text Objects'!
!Font methodsFor: 'Access to Parts' stamp: ''!
spacewidth | w |
	w ← self widthof: 32.
	w = 0
	  ifTrue: [^4].
	^w
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
maxwidth | |
	^maxwidth
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
charForm: ascii | |
	 "should return a Form copied out of glyphs"
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
widthof: glyph | |
	^xtable ◦ (glyph + 2) - (xtable ◦ (glyph + 1))
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
name | |
	^name
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
height | |
	^ascent + descent "height of font"
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
xtable | |
	^xtable
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
raster | |
	^raster
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
glyphs | |
	^glyphs
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
ascent | |
	^ascent
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
minascii | |
	^minascii
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
maxascii | |
	^maxascii
! !
!Font methodsFor: 'Access to Parts' stamp: ''!
descent | |
	^descent
! !
!Font methodsFor: 'Initialization' stamp: ''!
fromStrike: t1 | strike i |
	 "Build instance from strike file."
	name ← t1.
	strike ← dp0 oldFile: name + '.strike.'.
	strike nextword "skip header".
	minascii ← strike nextword.
	maxascii ← strike nextword.
	maxwidth ← strike nextword.
	length ← strike nextword.
	ascent ← strike nextword.
	descent ← strike nextword.
	xoffset ← strike nextword "If (horrors!) we should ever kern.".
	raster ← strike nextword.
	glyphs ← (Bitmap new: raster * self height) fromStream: strike.
	xtable ← (Vector new: maxascii + 3) all← 0.
	(minascii + 1 to: maxascii + 3) do: [:i | xtable ◦ i ← strike nextword]
! !
Object subclass: #FontSet
	instanceVariableNames: 'fonts names height ascent descent'
	classVariableNames: ''
	category: 'Text Objects'!
!FontSet methodsFor: 'Access' stamp: ''!
names | |
	^names
! !
!FontSet methodsFor: 'Access' stamp: ''!
baseline | |
	^ascent
! !
!FontSet methodsFor: 'Access' stamp: ''!
size: n | s c size |
	 "return size from fontname"
	names ◦ n ≡ nil
	  ifTrue: [n ← 1].
	size ← 0.
	s ← (names ◦ n) asStream.
	[(c ← s next) isletter] whileTrueDo: [].
	[size ← size * 10 + (c - 48).
	c ← s next] whileTrueDo: [].
	^size
! !
!FontSet methodsFor: 'Access' stamp: ''!
descent | |
	^descent
! !
!FontSet methodsFor: 'Access' stamp: ''!
height | |
	^height
! !
!FontSet methodsFor: 'Access' stamp: ''!
family: n | s char |
	 "return the family name taken out of names"
	names ◦ n ≡ nil
	  ifTrue: [n ← 1].
	s ← Stream default.
	names ◦ n do:
		[:char | char isletter
		  ifTrue: [s next← char]
		  ifFalse: [^s contents]]
! !
!FontSet methodsFor: 'Access' stamp: ''!
◦ n | |
	(n > (fonts length - 1) or: [n < 0])
	  ifTrue: [user notify: 'fontset offset < 0 or > 15 illegal']
	  ifFalse:
		[(fonts ◦ (n + 1) Is: Font)
		  ifTrue: [^fonts ◦ (n + 1)].
		(fonts ◦ 1 Is: Font)
		  ifTrue: [^fonts ◦ 1].
		user notify: 'No valid fonts in this FontSet']
! !
!FontSet methodsFor: 'Access' stamp: ''!
ascent | |
	^ascent
! !
!FontSet methodsFor: 'Access' stamp: ''!
◦ n ← font | |
	(n > (fonts length - 1) or: [n < 0])
	  ifTrue: [user notify: 'fontset offset < 0 or > 15 illegal']
	  ifFalse:
		[names ◦ (n + 1) ← font name asUppercase.
		fonts ◦ (n + 1) ← font.
		ascent < font ascent
		  ifTrue: [ascent ← font ascent].
		descent < font descent
		  ifTrue: [descent ← font descent].
		height ← ascent + descent]
! !
!FontSet methodsFor: 'Initialization' stamp: ''!
init | |
	fonts ← Vector new: 16.
	names ← Vector new: 16.
	height ← ascent ← descent ← 0
! !
Array subclass: #Paragraph
	instanceVariableNames: 'text runs alignment'
	classVariableNames: ''
	category: 'Text Objects'!
!Paragraph methodsFor: 'Bravo conversions' stamp: ''!
bravoRuns: s | i old len dif new bit bits |
	 "Encode the runs in a Bravo paragraph trailer onto a Stream"
	s append:  "assume Ctrl-Z is already there"
	(alignment = 1
	  ifTrue: ['j\g']
	  ifFalse:
		[alignment = 2
		  ifTrue: ['c\g']
		  ifFalse: ['\g']]).
	runs ≡ nil
	  ifFalse:
		[len ← 0.
		old ← 256.
		bits ← #(1 2 4 ).
		(1 to: runs length by: 2) do:
			[:i | dif ← old lxor: (new ← runs ◦ (i + 1)).
			(dif land: 247) = 0
			  ifTrue: [ "No changes"
				len ← len + (runs ◦ i)]
			  ifFalse:
				[i = 1
				  ifFalse: [len printOn: s].
				(1 to: 3) do:
					[:bit | (dif land: bits ◦ bit) = 0
					  ifFalse: [s next← ((new land: bits ◦ bit) ≠ 0
						  ifTrue: ['biu']
						  ifFalse: ['BIU']) ◦ bit]].
				(dif land: 240) ≠ 0
				  ifTrue:
					[ "Font change"
					s append: 'f'.
					s print: (new lshift: ¬4).
					s space].
				old ← new.
				len ← runs ◦ i]]].
	s cr
! !
!Paragraph methodsFor: 'Bravo conversions' stamp: ''!
toBravo | s |
	s ← (String new: text length * 2) asStream.
	s append: text.
	s next← 26.
	self bravoRuns: s.
	^s contents asParagraph
! !
!Paragraph methodsFor: 'Bravo conversions' stamp: ''!
applyBravo: s at: i to: j | v ch t bslash cr |
	s ← s asStream "Alter runs of characters i through j according to trailer.
	see Ibis<Bravo>Trailer.Memo for further info.
	some functions may not be implemented, thus parsed and ignored.

	paragraph looks.
	implemented: justification (j), centering (c).
	ignored: left margin (l), first line left margin (d), right margin (z),
	line leading (x), paragraph leading (e), vertical tab (y), keep (k), profile (q),
	tab tables ( () )".
	cr ← 13.
	bslash ← '\' ◦ 1.
	[(ch ← s next) = bslash] whileFalseDo: 
		[(ch ≡ false or: [ch = cr])
		  ifTrue: [^self].
		 "no more"
		(t ← 'jcq' find: ch) > 0
		  ifTrue:
			[t = 1
			  ifTrue: [self justify]
			  ifFalse:
				[t = 2
				  ifTrue: [self center]]]
		  ifFalse:
			[(t ← '(ldzxeyk' find: ch) > 0
			  ifTrue:
				[t = 1
				  ifTrue: [s skipTo: ')' ◦ 1]
				  ifFalse: [s integerScan]]] "character looks.
	implemented: font (f), bold (bB), italic (iI), underline (uU).
	ignored: graphic (g), visible (v), overstrike (s), superscript (o), tabcolor (t)"].
	[(ch ← s next) and: [ch ≠ cr]] whileTrueDo:
		[ "run length"
		((ch ≥ 48 and: [ch ≤ 57 "isdigit"])
		  ifTrue: [s skip: ¬1]
		  ifFalse: [ch = 32])
		  ifTrue: [i ← i + s integerScan]
		  ifFalse:
			[(t ← 'bBiIuU' find: ch) > 0
			  ifTrue: [self maskrun: i to: j under: #(1 1 2 2 4 4 ) ◦ t to: #(1 0 2 0 4 0 ) ◦ t]
			  ifFalse:
				[(t ← 'fot' find: ch) > 0
				  ifTrue:
					[v ← s integerScan "new value follows".
					t = 1
					  ifTrue: [self maskrun: i to: j under: 240 to: (v lshift: 4)]]]]]
! !
!Paragraph methodsFor: 'Bravo conversions' stamp: ''!
fromBravo | newpara newtext loc i j |
	 "Find Bravo trailers and return a copy of self with them applied"
	newpara ← self copy.
	loc ← 1.
	[(i ← (newtext ← newpara text) find: 26) ≠ 0] whileTrueDo:
		[j ← newtext ◦ (i + 1 to: newtext length) find: 13.
		newpara applyBravo: newtext ◦ (i + 1 to: i + j) at: loc to: i - 1.
		newpara replace: i to: (i + j = newtext length
		  ifTrue: [i + j]
		  ifFalse: [i + j - 1]) by: ''.
		loc ← i + 1].
	^newpara
! !
!Paragraph methodsFor: 'Filing' stamp: ''!
readFrom: file | |
	text ← file nextString.
	runs ← file nextString.
	alignment ← file next.
	runs empty
	  ifTrue: [runs ← nil]
! !
!Paragraph methodsFor: 'Filing' stamp: ''!
storeOn: file | |
	file nextString← text.
	runs ≡ nil
	  ifTrue: [file next← 0]
	  ifFalse: [file nextString← runs].
	file next← alignment
! !
!Paragraph methodsFor: 'Initialization of parts' stamp: ''!
text: t1 | |
	text ← t1.
	alignment ← 0
! !
!Paragraph methodsFor: 'Initialization of parts' stamp: ''!
text: t1 runs: t2 alignment: t3 | |
	text ← t1.
	runs ← t2.
	alignment ← t3
! !
!Paragraph methodsFor: 'Initialization of parts' stamp: ''!
copy | |
	^self class new text: text runs: runs alignment: alignment
! !
!Paragraph methodsFor: 'Initialization of parts' stamp: ''!
text: t1 alignment: t2 | |
	text ← t1.
	alignment ← t2
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
makeBoldPattern | s i c |
	s ← text asStream.
	i ← 0.
	[(c ← s next)
	  ifTrue:
		[ " scan to bracket, bar or comment "
		c = 91
		  ifTrue: [true]
		  ifFalse:
			[c = 124
			  ifTrue: [true]
			  ifFalse:
				[c = 34
				  ifTrue: [true]
				  ifFalse:
					[c = 25
					  ifTrue: [true]
					  ifFalse: [false]]]]]
	  ifFalse: [true]] whileFalseDo:  [ "end"
		i ← i + 1].
	self maskrun: 1 to: i under: 1 to: 1
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
runAndVal: index | run t |
	 "length and value at index"
	runs ≡ nil
	  ifTrue: [^{text length - index + 1 , 0}].
	run ← 1.
	[(t ← index - (runs ◦ run)) > 0] whileTrueDo:
		[index ← t.
		run ← run + 2].
	^{runs ◦ run - index + 1 , (runs ◦ (run + 1))}
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
allFont: n | |
	(n is: String)
	  ifTrue: [n ← (self textStyle fontnames find: n) - 1].
	self maskrunsunder: 240 to: n * 16
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
maskrunsunder: m to: val | |
	self maskrun: 1 to: text length under: m to: val
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
runfind: index | run t |
	 "index into run"
	run ← 1.
	[(t ← index - (runs ◦ run)) > 0] whileTrueDo:
		[index ← t.
		run ← run + 2].
	^{run , index}
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
maskrun: i to: j under: m to: val | r k |
	 "Alter my runs so that the bits selected by m become val." "Maybe merge this with mergestyle"
	r ← self run: i to: j.
	(2 to: r length by: 2) do: [:k | r ◦ k ← (r ◦ k land: 255 - m) + val].
	runs ← self runcat: (self run: 1 to: i - 1) and: [r] and: [(self run: j + 1 to: text length)]
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
runcat: r1 and: r2 and: r3 | i r olen len oc c nr |
	nr ← Stream new "concatenate and compact 3 runs" of: (String new: 30).
	oc ← false.
	(1 to: 3) do:
		[:i | r ← (i = 1
				  ifTrue: [r1]
				  ifFalse:
					[i = 2
					  ifTrue: [r2]
					  ifFalse: [r3]]).
		r length = 0
		  ifFalse:
			[r ← r asStream.
			[len ← r next] whileTrueDo:
				[c ← r next.
				len = 0
				  ifFalse:
					[ "ignore empty runs (shouldn't be any)"
					oc = c
					  ifTrue:
						[(olen ← olen + len) ≤ 255
						  ifFalse:
							[nr next← 255.
							nr next← oc.
							olen ← olen - 255]]
					  ifFalse:
						[oc
						  ifTrue:
							[nr next← olen.
							nr next← oc] "first time thru".
						olen ← len.
						oc ← c]]]]].
	oc
	  ifTrue:
		[nr next← olen "leftovers".
		nr next← oc].
	^nr contents
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
runs | |
	 "return runs or default if none"
	runs ≡ nil
	  ifTrue: [^self makerun: text length val: 0].
	^runs
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
allItalic | |
	self maskrunsunder: 2 to: 2
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
allBold | |
	self maskrunsunder: 1 to: 1
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
run: a to: b | c |
	 "subrange of run"
	a > b
	  ifTrue: [^nullString].
	runs ≡ nil
	  ifTrue: [^self makerun: 1 + b - a val: 0].
	a ← self runfind: a.
	b ← self runfind: b.
	c ← runs copy: a ◦ 1 to: b ◦ 1 + 1 "copy the sub-run".
	a ◦ 1 = (b ◦ 1)
	  ifTrue: [c ◦ 1 ← 1 + (b ◦ 2) - (a ◦ 2)]
	  ifFalse:
		[c ◦ 1 ← 1 + (runs ◦ (a ◦ 1)) - (a ◦ 2) "trim the end lengths".
		c ◦ (c length - 1) ← b ◦ 2].
	^c
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
runcat: x to: y | |
	^self runcat: x and: [y] and: ['']
! !
!Paragraph methodsFor: 'Manipulation of format runs' stamp: ''!
makerun: len val: val | str i |
	 "Make up a solid run of value val"
	len = 0
	  ifTrue: [^nullString].
	str ← String new: len - 1 / 255 + 1 * 2.
	(1 to: str length by: 2) do:
		[:i | str ◦ i ← (len > 255
		  ifTrue: [255]
		  ifFalse: [len]).
		str ◦ (i + 1) ← val.
		len ← len - 255].
	^str
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
subst: x for: y | |
	 "runs are not supported yet here"
	^text subst: x for: y
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
textStyle | |
	^DefaultTextStyle
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
asStream | |
	^text asStream
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
findString: str startingAt: start | |
	^text findString: str startingAt: start
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
length | |
	^text length
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
asVector | |
	^text asVector
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
text | |
	^text
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
copy: a to: b | |
	 "Return a copied subrange of this paragraph"
	^self class new text: (text copy: a to: b) runs: (self run: a to: b) alignment: alignment
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
asParagraph | |
	^self
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
replace: a to: b by: c | |
	 "alters self - doesnt copy"
	(runs ≡ nil and: [(c isnt: self class)])
	  ifFalse: [runs ← self runcat: (self run: 1 to: a - 1) and: [((c is: self class)
				  ifTrue: [c runs]
				  ifFalse: [self makerun: c length val: (runs empty
					  ifTrue: [0]
					  ifFalse: [runs ◦ ((self runfind: b) ◦ 1 + 1)])])] and: [(self run: b + 1 to: text length)]].
	text ← text replace: a to: b by: ((c is: self class)
			  ifTrue: [c text]
			  ifFalse: [c])
! !
!Paragraph methodsFor: 'Normal access' stamp: ''!
◦ x | |
	^text ◦ x
! !
!Paragraph methodsFor: 'Press printing' stamp: ''!
fromPress: press value: s | len x |
	s next = 0
	  ifTrue:
		[len ← s nextword "text is in DL" "amount to skip from where we are now to end of text".
		x ← (s limit > 255
				  ifTrue: [s limit "control info came from DL"]
				  ifFalse: [ "from EL"
					0]).
		press data skip: 0 - x - len.
		text ← press data next: len.
		press data skip: x]
	  ifFalse: [text ← s nextString].
	runs ← s nextString.
	alignment ← s next.
	runs empty
	  ifTrue: [runs ← nil]
! !
!Paragraph methodsFor: 'Press printing' stamp: ''!
presson: press in: r style: style | char pos s3 y chop |
	(text length "Output paragraph inside rectangle (page coordinates)" "probably ParagraphScanner should handle this" > 0 and: [text ◦ 1 = 12])
	  ifTrue: [^self copy: 2 to: text length "formfeed --> page break"].
	y ← r corner y "We change corner y later".
	s3 ← ParagraphScanner new of: self to: press style: style.
	s3 init in: r.
	pos ← s3 position.
	chop ← (alignment = 1
			  ifTrue: [0]
			  ifFalse: [alignment]).
	[y and: [(char ← s3 scan)]] whileTrueDo:
		[char = 9
		  ifTrue: [s3 tab]
		  ifFalse:
			[(char = 32 or: [char = 13])
			  ifTrue:
				[(y ← s3 printfrom: pos aligned:  "carriage return or exceeded max width and backed up to blank"
						(char = 32
						  ifTrue: [alignment]
						  ifFalse: [chop]) skip: 1)
				  ifTrue:
					[r corner y← y.
					s3 init in: r.
					pos ← s3 position]]
			  ifFalse:
				[char ≡ true
				  ifTrue:
					[s3 backup "exceeded max width with no blanks in line".
					(y ← s3 printfrom: pos aligned: 0 skip: 0)
					  ifTrue:
						[r corner y← y.
						s3 init in: r.
						pos ← s3 position]] "user notify: 'unimplemented control char'"]] "Put out trailing text if any"].
	(y and: [(pos = s3 position or: [(y ← s3 printfrom: pos aligned: chop skip: 0)])])
	  ifTrue:
		[press append: text.
		^y].
	press append: text ◦ (1 to: pos).
	^self copy: pos + 1 to: text length
! !
!Paragraph methodsFor: 'Press printing' stamp: ''!
presson: press in: r | |
	^self presson: press in: r style: self textStyle
! !
!Paragraph methodsFor: 'Press printing' stamp: ''!
hidePress: press complete: c | |
	press skipcode: self pressCode "not called by Form-Path-Image, but probably by Class printout" data: (self hideData: c)
! !
!Paragraph methodsFor: 'Press printing' stamp: ''!
pressCode | |
	^99
! !
!Paragraph methodsFor: 'Press printing' stamp: ''!
hideData: complete | s |
	s ← Stream new of: (String new: 150).
	s next← complete.
	complete = 0
	  ifTrue: [s nextword← text length]
	  ifFalse: [s nextString← text].
	s nextString← (runs ≡ nil
	  ifTrue: [nullString]
	  ifFalse: [runs]).
	s next← alignment.
	^s contents
! !
!Paragraph methodsFor: 'Text alignment' stamp: ''!
center | |
	alignment ← 2
! !
!Paragraph methodsFor: 'Text alignment' stamp: ''!
alignment← t1 | |
	alignment ← t1
! !
!Paragraph methodsFor: 'Text alignment' stamp: ''!
alignment | |
	^alignment
! !
!Paragraph methodsFor: 'Text alignment' stamp: ''!
flushleft | |
	alignment ← 0
! !
!Paragraph methodsFor: 'Text alignment' stamp: ''!
justify | |
	alignment ← 1
! !
!Paragraph methodsFor: 'Text alignment' stamp: ''!
flushright | |
	alignment ← 4
! !
Object subclass: #Reader
	instanceVariableNames: 'source collector token nextchar typetbl '
	classVariableNames: 'typetable'
	category: 'Text Objects'!
!Reader methodsFor: 'Initialization' stamp: ''!
of: s | |
	typetbl ← typetable.
	token ← Stream default.
	source ← s asStream.
	self step
! !
!Reader methodsFor: 'Initialization' stamp: ''!
classInit | strm type first last i |
	 "Initialize the type and mask tables"
	typetable ← String new: 256.
	strm ← Stream new of: #(5 0 255 1 65 90 1 97 122 2 48 57 3 58 58 3 3 3 4 9 10 4 12 13 4 32 32 6 34 34 6 25 25 7 39 39 8 21 21 9 26 26 10 30 30 11 40 41 ) "(initialize)" "upper and lower case letters" "digits" "colon, open colon" "TAB, LF, FF, CR, blank" "5 is one-char tokens" "comment quote and ➲" "string quote" "high-minus" "^Z (format trailer)" "DOIT" "open and close paren".
	[type ← strm next] whileTrueDo:
		[first ← strm next.
		last ← strm next.
		(first + 1 to: last + 1) do: [:i | typetable ◦ i ← type]]
! !
!Reader methodsFor: 'Internal readers' stamp: ''!
upto: char | start |
	 "Knows about doubled ' in strings"
	start ← source position.
	token reset.
	[nextchar ← source next] whileTrueDo:
		[nextchar = char
		  ifTrue:
			[self step.
			char ≠ 39
			  ifTrue: [^false].
			nextchar ≠ 39
			  ifTrue: [^false]].
		token next← nextchar "Ran off end, back up."].
	source skip: start - 1 - source position.
	^true
! !
!Reader methodsFor: 'Internal readers' stamp: ''!
rdint: char | |
	 "Read an integer, allow char as first char"
	token reset.
	nextchar = char
	  ifTrue:
		[token next← char.
		self step].
	[nextchar] whileTrueDo:
		[nextchar < 48
		  ifTrue: [^token contents].
		nextchar > 57
		  ifTrue:
			[ "allow ABCDEFGH for hex"
			nextchar > 72
			  ifTrue: [^token contents].
			nextchar < 65
			  ifTrue: [^token contents].
			token next← nextchar.
			nextchar ← source next]
		  ifFalse:
			[.
			token next← nextchar.
			nextchar ← source next]].
	^token contents
! !
!Reader methodsFor: 'Internal readers' stamp: ''!
step | |
	nextchar ← source next
! !
!Reader methodsFor: 'Main reader' stamp: ''!
readInto: t1 | x |
	collector ← t1.
	[nextchar] whileTrueDo:
		[x ← typetbl ◦ (nextchar + 1) "See classInit for the meanings of the type codes".
		x = 4
		  ifTrue:
			[collector separator: nextchar.
			nextchar ← source next]
		  ifFalse:
			[x = 1
			  ifTrue: [self readatom: 0]
			  ifFalse:
				[x = 5
				  ifTrue:
					[self binary]
				  ifFalse:
					[x = 6
					  ifTrue:
						[(self upto: nextchar)
						  ifTrue: [collector notify: 'Unmatched comment quote']
						  ifFalse: [collector comment: token contents]]
					  ifFalse:
						[x = 2
						  ifTrue: [self readnum]
						  ifFalse:
							[x = 11
							  ifTrue:
								[nextchar = 40
								  ifTrue: [collector leftparen]
								  ifFalse: [collector rightparen].
								nextchar ← source next]
							  ifFalse:
								[x = 7
								  ifTrue:
									[(self upto: nextchar)
									  ifTrue: [collector notify: 'Unmatched string quote']
									  ifFalse: [collector string: token contents]]
								  ifFalse:
									[x = 8
									  ifTrue: [self readnum]
									  ifFalse:
										[x = 9
										  ifTrue:
											[(self upto: 13)
											  ifTrue: [collector notify: '^Z without CR']
											  ifFalse: [collector trailer: token contents]]
										  ifFalse:
											[x = 10
											  ifTrue: [^collector contents].
											x = 3
											  ifTrue: [self readatom: 1]]]]]]]]]]].
	^collector contents
! !
!Reader methodsFor: 'Main reader' stamp: ''!
binary | char1 s |
	char1 ← nextchar.
	nextchar ← source next.
	(nextchar and: [typetbl ◦ (nextchar + 1) = 5])
	  ifTrue: "binary"
		[s ← '??'.
		s◦1 ← char1.
		s◦2 ← nextchar.
		nextchar ← source next.
		collector otheratom: s]
	  ifFalse: 
		[collector onechar: char1]
! !
!Reader methodsFor: 'Main reader' stamp: ''!
readatom: ncolons | type s t4 |
	token reset.
	[token next← nextchar.
	(nextchar ← source next)
	  ifTrue: [(type ← typetbl ◦ (nextchar + 1)) ≤ 3]
	  ifFalse: [false]] whileTrueDo:
		[type = 3
		  ifTrue: [ncolons ← ncolons + 1]].
	s ← token contents.
	ncolons = 0
	  ifTrue: [collector identifier: s]
	  ifFalse:
		[ncolons > 1
		  ifTrue: [collector otheratom: s]
		  ifFalse:
			[.
			s length = 1
			  ifTrue: [collector otheratom: s]
			  ifFalse:
				[ ": or ⦂ alone"
				(t4 ← s ◦ s length) = 58
				  ifTrue: [collector keyword: s]
				  ifFalse:
					[t4 = 3
					  ifTrue: [collector keyword: s]
					  ifFalse:
						[.
						collector otheratom: s "Colon wasn't last character"]]]]]
! !
!Reader methodsFor: 'Main reader' stamp: ''!
readnum | val d e |
	val ← self rdint: 21.
	nextchar = 46
	  ifTrue:
		[ "check for decimal point"
		self step.
		(nextchar ≡ false or: [nextchar isdigit ≡ false])
		  ifTrue:
			[collector integer: val.
			collector onechar: 46]
		  ifFalse:
			[ "was <Integer> .  "
			d ← self rdint: ¬1 "fraction part".
			nextchar = 101
			  ifTrue:
				[ "check for e<exponent> "
				self step.
				e ← self rdint: 21]
			  ifFalse: [e ← ''].
			collector float: val fraction: d exp: e]]
	  ifFalse: [collector integer: val]
! !
!Reader methodsFor: 'Main reader' stamp: ''!
read | |
	^self readInto: TokenCollector default
! !
Object subclass: #RemoteParagraph
	instanceVariableNames: 'file hipos lowpos'
	classVariableNames: ''
	category: 'Text Objects'!
!RemoteParagraph methodsFor: 'As yet unclassified' stamp: ''!
asParagraph | |
	file≡nil ifTrue: [^'an inaccessible RemoteParagraph' asParagraph].
	file position← self position.
	^Paragraph new readFrom: file
! !
!RemoteParagraph methodsFor: 'As yet unclassified' stamp: ''!
on: t1 | |
	 "Refer me to a specific file"
	file ← t1
! !
!RemoteParagraph methodsFor: 'As yet unclassified' stamp: ''!
asString | |
	^self asParagraph text
! !
!RemoteParagraph methodsFor: 'As yet unclassified' stamp: ''!
fromString: s | |
	self fromParagraph: s asParagraph
! !
!RemoteParagraph methodsFor: 'As yet unclassified' stamp: ''!
position← p | |
	p ← p intdiv: 2000.
	hipos ← (p ◦ 1) asInteger - 1000.
	lowpos ← (p ◦ 2) asInteger - 1000
! !
!RemoteParagraph methodsFor: 'As yet unclassified' stamp: ''!
position | |
	^hipos + 1000 * 2000 + (lowpos + 1000)
! !
!RemoteParagraph methodsFor: 'As yet unclassified' stamp: ''!
fromParagraph: p | |
	self position← file position "write me (only once!) on file".
	p storeOn: file
! !
Object subclass: #StyleSheet
	instanceVariableNames: 'fontset linelead lineheight baseline tabsize spacesize effect'
	classVariableNames: ''
	category: 'Text Objects'!
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
spacesize | |
	^spacesize
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
baseline | |
	^baseline
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
lineheight | |
	^lineheight
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
fontsize: fontindex | |
	^fontset size: fontindex
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
fontfamily: fontindex | |
	^fontset family: fontindex
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
effect | |
	^effect
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
linelead | |
	^linelead
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
tabsize | |
	^tabsize
! !
!StyleSheet methodsFor: 'Access to Parts' stamp: ''!
font: fontindex | |
	^fontset ◦ fontindex
! !
!StyleSheet methodsFor: 'As yet unclassified' stamp: ''!
NoteTakerize | x |
	(fontset ◦ 0) NTformat
! !
!StyleSheet methodsFor: 'Initialization' stamp: ''!
fontset: t1 | |
	fontset ← t1.
	self fontset: fontset lineheight: fontset height linelead: 0 baseline: fontset baseline tabsize: 32 spacesize: 4 effect: storing
! !
!StyleSheet methodsFor: 'Initialization' stamp: ''!
fontset: t1 lineheight: t2 linelead: t3 baseline: t4 tabsize: t5 spacesize: t6 effect: t7 | |
	fontset ← t1.
	lineheight ← t2.
	linelead ← t3.
	baseline ← t4.
	tabsize ← t5.
	spacesize ← t6.
	effect ← t7
! !
Object subclass: #Textframe
	instanceVariableNames: 'frame window para style lines lastline'
	classVariableNames: ''
	category: 'Text Objects'!
!Textframe methodsFor: 'Access to Parts' stamp: ''!
lineheight | |
	^style lineheight
! !
!Textframe methodsFor: 'Access to Parts' stamp: ''!
frame | |
	^frame
! !
!Textframe methodsFor: 'Access to Parts' stamp: ''!
window | |
	^window
! !
!Textframe methodsFor: 'Access to Parts' stamp: ''!
frame← t1 | |
	frame ← t1.
	window ← frame "Change my frame and window."
! !
!Textframe methodsFor: 'Access to Parts' stamp: ''!
para | |
	^para
! !
!Textframe methodsFor: 'Conversion' stamp: ''!
makeParagraph | |
	para ≡ nil "simulate ListPane for hardcopy"
	  ifTrue: [para ← 'NIL !' asParagraph]
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
put: t1 at: pt | |
	para ← t1.
	self put: para at: pt centered: false
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
erase | |
	(window inset: ¬2 ⌾ ¬2) clear
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
displaylines: startline to: stopline | liney line P lineheight boty leftx |
	 "display a range, windowed"
	lineheight ← style lineheight.
	startline ← startline max: (self lineofy: window minY).
	stopline ← stopline min: (self lineofy: window maxY - lineheight).
	liney ← self yofline: startline.
	boty ← liney + lineheight.
	P ← self printer: true "P should do left and right clearing, since it has clip rect".
	(startline to: stopline) do:
		[:startline | line ← lines ◦ startline.
		para alignment > 1
		  ifTrue:
			[leftx ← frame minX + (para alignment = 4
					  ifTrue: [line padwidth]
					  ifFalse: [line padwidth / 2]).
			(window minX ⌾ liney rect: leftx ⌾ boty) clear: white].
		P scanline: line at: liney stopx: window maxX stopchar: line stopi.
		(para alignment nomask: 1)
		  ifTrue: [(P rightx ⌾ liney rect: window maxX ⌾ boty) clear: white].
		liney ← boty.
		boty ← boty + lineheight].
	^liney
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
displayall | liney |
	 "display all the lines in window"
	liney ← self displaylines: 1 to: lastline.
	self clearfrom: liney "self outline"
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
clearfrom: y | |
	((frame origin x ⌾ y rect: frame corner) intersect: window) clear: white
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
put: t1 centered: pt | |
	para ← t1.
	self put: para at: pt centered: true
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
put: t1 at: pt centered: center | rect |
	para ← t1.
	window ← frame ← pt rect: 1000 ⌾ 1000.
	para ← para asParagraph.
	self measureall.
	self pointofchar: para length + 1 andrect⦂ [:rect | rect].
	window growto: rect corner.
	center
	  ifTrue: [window moveby: pt - window center].
	window ← window inset: ¬3 ⌾ ¬2.
	window clear: white.
	self show
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
clear | |
	(frame intersect: window) clear
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
show | |
	self measureall.
	self displayall
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
outline | |
	 "from TextFrame"
	window border: 2 color: black
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
printer: printing | |
	^TextScanner new frame: frame window: window para: para style: style printing: printing
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
comp | |
	window comp
! !
!Textframe methodsFor: 'Displaying' stamp: ''!
show: t1 | |
	para ← t1.
	self measureall.
	self displayall
! !
!Textframe methodsFor: 'Editing' stamp: ''!
replace: starti to: stopi with: insert | i j k startline stopline oldlastline newline range stopped oldlines begini oldy newy moveRect |
	 "cut or paste"
	para replace: starti to: stopi by: insert "do the replacement in the string".
	lastline = 0
	  ifTrue:
		[self measureall.
		self displayall]
	  ifFalse:
		[ "if lines have never been set up,
									measure them and display all the
									lines falling in the window"
		oldlines ← lines copy.
		oldlastline ← lastline.
		i ← startline ← self lineofchar: starti "find the starting and stopping lines".
		stopline ← self lineofchar: stopi.
		range ← insert length - (stopi - starti + 1) "how many characters being
										inserted or deleted" "If the starting line is not also the first line, then measuring must commence from line preceding the one in which starti appears.  For example, deleting a line with only a carriage return may move characters following the deleted portion of text into the line preceding the deleted line.".
		begini ← (lines ◦ i) starti.
		startline > 1
		  ifTrue:
			[newline ← self measurefrom: (lines ◦ (startline - 1)) starti to: para length.
			lines ◦ (startline - 1) = newline
			  ifFalse:
				[ "no backwrap" "start in line preceding that containing the starting character"
				startline ← startline - 1.
				self line: startline ← newline.
				begini ← newline stopi + 1]].
		begini > para length
		  ifTrue:
			[self trimLinesTo: (i - 1 max: 0) "nil lines after cut--remeasure last line below".
			para length = 0
			  ifTrue:
				[ "cut entire paragraph--clear and return"
				window clear: white.
				^self]].
		stopped ← false.
		j ← stopline.
		[stopped or: [begini > para length]] whileFalseDo: 
			[self line: i ← newline ← self measurefrom: begini to: para length.
			[(j > oldlastline or: [newline stopi "no more old line to compare with" < (stopi ← (oldlines ◦ j) stopi + range)]) or: [stopped]] whileFalseDo: 
				[stopi = newline stopi
				  ifTrue:
					[ "got the match"
					oldy ← self yofline: j +  "get source and destination y's for moving the unchanged lines"
							(range < 0
							  ifTrue: [0]
							  ifFalse: [1]).
					newy ← self yofline: i + (range < 0
							  ifTrue: [0]
							  ifFalse: [1]).
					stopline ← i.
					stopped ← true "fill in the new line vector with the old unchanged lines.
				Update their starting and stopping indices on the way.".
					((j ← j + 1) to: oldlastline) do: [:k | self line: (i ← i + 1) ← oldlines ◦ k slide: range].
					self trimLinesTo: i "trim off obsolete lines, if any"]
				  ifFalse: [j ← j + 1]].
			begini ← newline stopi + 1.
			i ← i + 1 "Now the lines are up to date.  What remains is the move the 'unchanged' lines and display those which have changed."].
		begini > para length
		  ifTrue: [ "If at the end of previous lines simply display lines
						from the line in which the first character of the
						replacement occured through the end of the paragraph."
			self displaylines: startline to: (stopline ← i min: lastline)]
		  ifFalse:
			[ "Otherwise prepare to move the unchanged lines.  moveRect defines the portion of the window containing the lines which may be move en masse." "Deletion -- moving 'up' the screen or ..."
			moveRect ← window minX ⌾ (oldy max: window minY) rect: window corner "Insertion -- moving 'down' the screen.
					Shorten moveRect by height of insertion or ...".
			oldy ≤ newy
			  ifTrue: [moveRect corner← window corner + (0 ⌾ (oldy - newy))]
			  ifFalse:
				[ "Deletion, and top of moveRect will fall above top of window.
		Increase the origin of moveRect by the amount that would fall above
		(hence outside) the window."
				newy < window minY
				  ifTrue: [moveRect origin← window minX ⌾ (oldy + window minY - newy)]] "Move it.".
			moveRect blt: window minX ⌾ (newy max: window minY) mode: storing "Display the new lines.".
			self displaylines: startline to: stopline "A deletion may have 'pulled' previously undisplayed  lines into the window.
		If so, display them.".
			(newy < oldy and: [(self yofline: oldlastline + 1) > window maxY])
			  ifTrue: [self displaylines: (self lineofy: window maxY - (oldy - newy)) to: (stopline ← self lineofy: window maxY)]] "If we have done a deletion, obsolete material may remain at the bottom of the window.  If so, clear it out.".
		oldlastline ≥ lastline
		  ifTrue: [self clearfrom: (self yofline: lastline + 1)]]
! !
!Textframe methodsFor: 'Image' stamp: ''!
style | |
	^style
! !
!Textframe methodsFor: 'Initialization' stamp: ''!
style: t1 | |
	style ← t1
! !
!Textframe methodsFor: 'Initialization' stamp: ''!
para: t1 frame: t2 | |
	para ← t1.
	frame ← t2.
	self frame: frame window: frame para: para style: DefaultTextStyle
! !
!Textframe methodsFor: 'Initialization' stamp: ''!
para: t1 frame: t2 style: t3 | |
	para ← t1.
	frame ← t2.
	style ← t3.
	self frame: frame window: frame para: para style: style
! !
!Textframe methodsFor: 'Initialization' stamp: ''!
frame: t1 window: t2 para: t3 style: t4 | |
	frame ← t1.
	window ← t2.
	para ← t3.
	style ← t4.
	(NoteTaker or: [frame ≡ nil])
	  ifFalse: [frame ← window ← frame intersect: user screenrect].
	lines ← Vector new: 32.
	lastline ← 0.
	(para ≡ nil or: [frame ≡ nil])
	  ifFalse: [self measureall]
! !
!Textframe methodsFor: 'Initialization' stamp: ''!
classInit | |
	space ← 32.
	cr ← 13.
	tab ← 9
! !
!Textframe methodsFor: 'Line management' stamp: ''!
line: i ← line | |
	 "store a line, track last, and grow lines if necessary"
	i > lastline
	  ifTrue: [lastline ← i].
	lastline > lines length
	  ifTrue: [lines ← lines grow].
	^lines ◦ i ← line
! !
!Textframe methodsFor: 'Line management' stamp: ''!
lineofchar: index | i |
	 "Return line number (may be 0)"
	(1 to: lastline) do:
		[:i | index ≤ (lines ◦ i) stopi
		  ifTrue: [^i]].
	^lastline
! !
!Textframe methodsFor: 'Line management' stamp: ''!
adjustLines: i by: delta | |
	 "add to indices of charlines beginning with stopi in i"
	lines ◦ i stopi← (lines ◦ i) stopi + delta.
	(i + 1 to: lastline) do: [:i | lines ◦ i slide: delta]
! !
!Textframe methodsFor: 'Line management' stamp: ''!
trimLinesTo: last | i |
	(last + 1 to: lastline) do: [:i | lines ◦ i ← nil].
	(lastline ← last) < (lines length / 2)
	  ifTrue: [lines ← lines growby: 0 - (lines length / 2)]
! !
!Textframe methodsFor: 'Line management' stamp: ''!
yofline: line | |
	 "top y of given line"
	^frame minY + (line - 1 * style lineheight)
! !
!Textframe methodsFor: 'Line management' stamp: ''!
lineofy: y | |
	 "Return line number for a given y (may be out of range!)"
	^y - frame minY / style lineheight + 1 min: lastline
! !
!Textframe methodsFor: 'Measuring' stamp: ''!
maxx: char | i rect maxx |
	self measureall.
	maxx ← 0.
	(1 to: lastline) do:
		[:i | self pointofchar: (lines ◦ i) stopi andrect⦂ [:rect | rect].
		rect corner x > maxx
		  ifTrue: [maxx ← rect corner x]].
	^maxx ⌾ rect corner y
! !
!Textframe methodsFor: 'Measuring' stamp: ''!
measurefrom: starti to: stopi | line chari spacecount rightbits P stopx |
	 "measure and return a line"
	line ← CharLine new starti: starti stopi: stopi spaces: 0 padwidth: 0.
	P ← self printer: false.
	P scanline: line at: 0 stopx: (stopx ← frame maxX) stopchar: stopi.
	chari ← P chari.
	spacecount ← P spacecount.
	(chari = stopi or: [para ◦ chari = cr])
	  ifTrue:
		[ "CR or string end"
		rightbits ← stopx - P rightx.
		spacecount ← 0]
	  ifFalse:
		[spacecount > 0
		  ifTrue:
			[ "Back up to space"
			chari ← P spacei.
			rightbits ← stopx - P spacex.
			(chari > starti and: [para ◦ (chari - 1) = space])
			  ifTrue:
				[spacecount ← spacecount - 1.
				rightbits ← rightbits + P rightx - P leftx]
			  ifFalse:
				[(chari < stopi and: [para ◦ (chari + 1) = space])
				  ifTrue: [chari ← chari + 1]] "double spaces".
			spacecount ← 1 max: spacecount - 1]
		  ifFalse:
			[chari ← chari - 1.
			rightbits ← stopx - P leftx]] "Back 1 char".
	^line starti: starti stopi: chari spaces: spacecount padwidth: rightbits
! !
!Textframe methodsFor: 'Measuring' stamp: ''!
measureall | starti stopi linei |
	 "Set up lines for text displayer"
	starti ← linei ← 1.
	stopi ← para length.
	[starti > stopi] whileFalseDo: 
		[self line: linei ← self measurefrom: starti to: stopi.
		starti ← (lines ◦ linei) stopi + 1.
		linei ← linei + 1].
	self trimLinesTo: linei - 1
! !
!Textframe methodsFor: 'Measuring' stamp: ''!
scrolln: n | ignored |
	^self charofpoint: frame corner x ⌾ (frame origin y + (n * style lineheight)) andrect⦂ [:ignored | ignored]
! !
!Textframe methodsFor: 'Measuring' stamp: ''!
lastshown | ignored |
	^self charofpoint: window corner andrect⦂ [:ignored | ignored]
! !
!Textframe methodsFor: 'Measuring' stamp: ''!
pointofchar: index andrect⦂ rect | linei y P orig |
	 "Return origin of character index"
	linei ← self lineofchar: index.
	y ← self yofline: linei.
	index > para length
	  ifTrue:
		[(para length = 0 or: [para ◦ para length = cr])
		  ifTrue: [orig ← frame minX ⌾ (y + style lineheight)]
		  ifFalse:
			[P ← self printer: false.
			P scanline: lines ◦ linei at: y stopx: 9999 stopchar: index - 1.
			orig ← P rightx ⌾ y]]
	  ifFalse:
		[.
		P ← self printer: false.
		P scanline: lines ◦ linei at: y stopx: 9999 stopchar: index.
		orig ← P leftx ⌾ y].
	rect value← Rectangle new origin: orig extent: 0 ⌾ style lineheight.
	^orig
! !
!Textframe methodsFor: 'Measuring' stamp: ''!
charofpoint: point andrect⦂ rect | linei leftx rightx y chari P orig r |
	 "Return character index of character at point" "TD enter."
	linei ← 0 max: (self lineofy: point y).
	y ← self yofline: linei.
	linei = 0
	  ifTrue:
		[chari ← 1.
		orig ← frame minX ⌾ (y + style lineheight)]
	  ifFalse:
		[point y > (y + style lineheight)
		  ifTrue:
			[self pointofchar: para length + 1 andrect⦂ [:r | r].
			rect value← r.
			^para length + 1].
		P ← self printer: false.
		P scanline: lines ◦ linei at: y stopx: point x stopchar: (lines ◦ linei) stopi.
		chari ← P chari.
		rightx ← P rightx.
		leftx ← P leftx.
		(point x ≤ (leftx + rightx / 2) or: [para ◦ chari = cr])
		  ifTrue: [orig ← leftx ⌾ y]
		  ifFalse:
			[chari ← chari + 1.
			orig ← rightx ⌾ y]].
	rect value← Rectangle new origin: orig extent: 0 ⌾ style lineheight.
	^chari
! !
!Textframe methodsFor: 'Printing' stamp: ''!
hardcopy: pf | first last lasty len parag left right top bottom rect |
	para ≡ nil
	  ifTrue: [self makeParagraph].
	parag ← para asParagraph.
	frame = window
	  ifTrue: [parag presson: pf in: (pf transrect: window) style: style]
	  ifFalse:
		[left ← frame minX max: window minX.
		right ← window maxX min: frame maxX.
		bottom ← window maxY min: frame maxY.
		top ← window minY max: frame minY.
		lasty ← top + 4 "slop for char finding and making print rect larger".
		first ← self charofpoint: left + 1 ⌾ lasty andrect⦂ [:rect | rect].
		len ← parag length.
		(frame minX ≥ left and: [frame maxX ≤ right])
		  ifTrue: [(parag copy: first to: len) "paragraph is inset and may be scrolled" presson: pf in: (pf transrect: (left ⌾ top rect: right ⌾ (bottom + 4))) style: style]
		  ifFalse: [ "yuk, frame extends left or right so do it a line at a time for clipping"
			[first < len and: [lasty < bottom]] whileTrueDo:
				[last ← (self charofpoint: right - 1 ⌾ lasty andrect⦂ [:rect | rect]) min: len.
				lasty ← lasty + rect height.
				(parag copy: first to: last) presson: pf in: (pf transrect: (left ⌾ rect minY rect: right ⌾ lasty)) style: style.
				first ← last + 1]]]
! !
!Textframe methodsFor: 'Printing' stamp: ''!
hardcopy | pf |
	user displayoffwhile⦂ [
		(pf ← dp0 pressfile: 'frame.press'.
		window hardcopy: pf.
		self hardcopy: pf.
		pf close.
		pf toPrinter)]
! !
!Textframe methodsFor: 'Scheduling' stamp: ''!
takeCursor | |
	user cursorloc← window center "Move the cursor to the center of my window."
! !
!Textframe methodsFor: 'Scheduling' stamp: ''!
aboutToFrame | |
	 "My frame is about to change.  I dont care."
! !
!Textframe methodsFor: 'Testing' stamp: ''!
Initializations | |
	 "
Smalltalk declare: Undeclared contents from: Undeclared.
Smalltalk declare: ↪(Cream10 STBig TestSet TestStyle TestPara TD TDFlag NoteTaker).

NoteTaker ← false.
(dp0 file: 'Cream10.strike') append: DefaultTextStyle fonts◦1; close.
TextDisplayer classInit.
Cream10 ← Font new fromStrike: 'Cream10'.
STBig ← Font new fromStrike: 'STBig'.
TestSet ← FontSet init. TestSet ◦ 0 ← Cream10. TestSet◦1 ← STBig.
TestStyle ← StyleSheet new fontset: TestSet.
TestPara ← Paragraph new text:
'again
copy
cut
paste
doit
compile
undo
cancel
align
'.
TestPara text: TestPara text runs: TestPara runs alignment: 0.
TextEditor classInit.
TD ←TDTextImage new.
user schedule: (TD ← TDCodePane new showing: TestPara).

| r. TD frame: (r ← Rectangle new fromuser) window: r para: TestPara style: TestStyle.
TD measureall.
"
! !
!Textframe methodsFor: 'Testing' stamp: ''!
Testing | |
	 "

Stuff to do:
Fast type-in
Try out line buffer

TD measureall. TD enter.
TimesRoman10 fromStrike: 'TimesRoman10'.
user time⦂ [TD align; align; align; align.] 575
until⦂ user anybug do⦂ [TD align].
 | p. user print: (p←user waitbug); cr.
	user print: (p←TD charofpoint: p); cr.
	user print: (TD pointofchar: p◦1); cr.
TD pointofchar: 68 
TD displayall; nselect: user waitbug.
TD clearfrom: user mp y a TDTextImage
TD reversefrom: 1 to: 20.
 | i. spy every: 10; on⦂ [for⦂ i to: 100 do⦂
[TD displayall ]]; report: 'show.spy'; close.
spy every: 10; on⦂ [TD select: user waitbug]; report: 'select.spy'; close.
"
! !
Textframe subclass: #TextImage
	instanceVariableNames: 'c1 c2 begintypein superimage oldEntity sel'
	classVariableNames: 'Deletion on esc off Scrap bs scrap paragraphmenu cut'
	category: 'Text Objects'!
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
Scrap← s | |
	Scrap ← s
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
formerly | |
	^oldEntity
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
frame← f | |
	self fixframe: f
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
c2← t1 | |
	c2 ← t1
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
c1← t1 | |
	c1 ← t1
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
Deletion← s | |
	Deletion ← s
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
height | rect |
	self pointofchar: para length + 1 andrect⦂ [:rect | rect].
	^rect corner y - frame minY
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
contents | |
	^para
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
begintypein← t1 | |
	begintypein ← t1
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
frameoffset | |
	^frame minY "a useful number" - window minY
! !
!TextImage methodsFor: 'ACCESS TO PARTS' stamp: ''!
formerly: t1 | |
	oldEntity ← t1
! !
!TextImage methodsFor: 'DISPLAY' stamp: ''!
enter | |
	begintypein ← false.
	self show.
	self select
! !
!TextImage methodsFor: 'DISPLAY' stamp: ''!
leave | |
	self complement: off
! !
!TextImage methodsFor: 'DISPLAY' stamp: ''!
show | |
	lastline = 0
	  ifTrue: [super show]
	  ifFalse: [self displayall].
	sel ← off
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
fintype | |
	begintypein
	  ifTrue:
		[begintypein < c1
		  ifTrue:
			[Scrap ← para copy: begintypein to: c1 - 1.
			c1 ← begintypein].
		begintypein ← false]
	  ifFalse: [^false]
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
realign | |
	self align.
	sel ← on
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
paste | |
	self fintype "paste the Scrap over the current selection and redisplay the paragraph.".
	self unselect.
	self replace: Scrap.
	self selectAndScroll
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
replace: t | |
	oldEntity
	  ifFalse:
		[oldEntity ← para.
		para ← para copy].
	begintypein
	  ifFalse: [Deletion ← self selection].
	self replace: c1 to: c2 - 1 with: t.
	c2 ← c1 + t length
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
align | |
	para alignment← #(1 2 4 0 0 ) ◦ (1 + para alignment).
	self displayall.
	self reversefrom: c1 to: c2
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
undo | |
	self fintype.
	self replace: Deletion.
	self complement
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
fit | rect |
	(window intersect: frame) "make the bounding rectangle of the TextImage contain all the textwhile not changing the width of the TextImage." white.
	frame extent← frame width ⌾ 1000.
	self pointofchar: para length + 1 andrect⦂ [:rect | rect].
	frame extent← frame width ⌾ (rect corner y - frame origin y).
	self show.
	frame border: 1 color: ¬1.
	self reversefrom: c1 to: c2
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
checklooks | t val mask range |
	t ← #(166 150 137 151 230 214 201 215 135 159 144 143 128 127 129 131 180 149 199 223 208 207 192 191 240 226 ) find: user kbck.
	t = 0
	  ifTrue: [^false].
	user kbd.
	oldEntity
	  ifFalse:
		[oldEntity ← para.
		para ← para copy].
	t = 25
	  ifTrue: [para ← para toBravo]
	  ifFalse:
		[ "ctl-T"
		t = 26
		  ifTrue: [para ← para fromBravo]
		  ifFalse:
			[ "ctl-F"
			.
			val ← #(1 2 4 256 ¬1 ¬2 ¬4 256 0 16 32 48 64 80 96 112 128 144 160 176 192 208 224 240 ) ◦ t "ctl-b i - x   B I ¬ X" "ctl-0 1 ... 9" "ctl-shift-0 1 ... 5".
			val = 256
			  ifTrue:
				[mask ← 255.
				val ← 0]
			  ifFalse:
				[ "reset all"
				val < 0
				  ifTrue:
					[mask ← 0 - val.
					val ← 0]
				  ifFalse:
					[ "reset emphasis"
					(val > 0 and: [val < 16])
					  ifTrue: [mask ← val]
					  ifFalse: [ "set emphasis"
						mask ← 240]]] "set font".
			para maskrun: c1 to: c2 - 1 under: mask to: val "fix paragraph".
			mask = 240
			  ifTrue: [self replace: c1 to: c2 - 1 "remeasure if changing fonts" with: (para copy: c1 to: c2 - 1)]
			  ifFalse: [ "otherwise just redisplay lines"
				self displaylines: (self lineofchar: c1) to: (self lineofchar: c2)].
			self select]]
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
cut | |
	self fintype "cut out the current selection and redisplay the paragraph.".
	self replace: nullString.
	self selectAndScroll.
	Scrap ← Deletion
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
copyselection | |
	Scrap ← self selection "copy the current selection and store it in the Scrap."
! !
!TextImage methodsFor: 'EDITING' stamp: ''!
fixframe: f | dy |
	dy ← (frame ≡ nil
			  ifTrue: [0]
			  ifFalse: [self frameoffset]).
	window ← f copy.
	frame ← Rectangle new origin: window origin + (2 ⌾ dy) extent: window width - 4 ⌾ 9999.
	self measureall.
	^window
! !
!TextImage methodsFor: 'Editing' stamp: ''!
again | many |
	many ← user leftShiftKey.
	self fintype
	  ifTrue:
		[Scrap ← Scrap text.
		self select].
	many
	  ifTrue: [[self againOnce] whileTrueDo: []]
	  ifFalse:
		[self againOnce
		  ifFalse: [frame flash]]
! !
!TextImage methodsFor: 'Editing' stamp: ''!
againOnce | t |
	t ← para findString: Deletion startingAt: c2.
	t = 0
	  ifTrue: [^false].
	self unselect.
	c1 ← t.
	c2 ← c1 + Deletion length.
	self replace: Scrap.
	self selectAndScroll
! !
!TextImage methodsFor: 'INIT' stamp: ''!
paragraph: t1 frame: t2 style: t3 | |
	para ← t1.
	frame ← t2.
	style ← t3.
	nil ≡ para
	  ifTrue: [para ← nullString].
	c1 ← c2 ← begintypein ← 1.
	self para: para frame: frame style: style
! !
!TextImage methodsFor: 'INIT' stamp: ''!
close | |
	superimage ← nil
! !
!TextImage methodsFor: 'INIT' stamp: ''!
classInit | |
	bs ← 8.
	ctlw ← 145.
	esc ← 160.
	cut ← 173.
	paste ← 158.
	Scrap ← Deletion ← nullString.
	on ← 1.
	off ← 0.
	paragraphmenu ← Menu new string: 'again
copy
cut
paste
doit
compile
undo
cancel
align
fit
resize
'
! !
!TextImage methodsFor: 'PARAGRAPH EDITOR' stamp: ''!
scrollTo: f | |
	self scrollUp: self frameoffset + (f * self height) asInteger - 4
! !
!TextImage methodsFor: 'PARAGRAPH EDITOR' stamp: ''!
scrollUp: n | |
	self scrollby: n / self lineheight
! !
!TextImage methodsFor: 'PARAGRAPH EDITOR' stamp: ''!
scrollPos | t |
	t ← self height - self lineheight.
	t = 0
	  ifTrue: [^0.0].
	^0.0 - self frameoffset / t
! !
!TextImage methodsFor: 'Public Messages' stamp: ''!
kbd | more char |
	 "key struck on the keyboard"
	(c1 < c2 and: [self checklooks])
	  ifTrue: [^self show complement].
	more ← Set new string: 16.
	begintypein
	  ifFalse:
		[Deletion ← self selection.
		begintypein ← c1].
	[char ← user kbdnext] whileTrueDo:
		[char = bs
		  ifTrue:
			[more empty "backspace"
			  ifTrue: [begintypein ← begintypein min: (c1 ← 1 max: c1 - 1)]
			  ifFalse: [more skip: ¬1]]
		  ifFalse:
			[char = cut
			  ifTrue:
				[self fintype.
				c1 = c2
				  ifTrue: [c2 ← c1 + 1 min: para length + 1].
				self replace: nullString.
				self complement.
				Scrap ← Deletion.
				^self].
			char = paste
			  ifTrue: [^self paste].
			char = ctlw
			  ifTrue:
				[ "ctl-w for backspace word"
				more empty
				  ifFalse:
					[self replace: more.
					more reset.
					c1 ← c2].
				c1 ← 1 max: c1 - 1.
				[c1 > 1 and: [(para ◦ (c1 - 1)) tokenish]] whileTrueDo: [c1 ← c1 - 1].
				begintypein ← begintypein min: c1]
			  ifFalse:
				[char = esc
				  ifTrue:
					[ "select previous type-in"
					more empty
					  ifTrue: [self unselect]
					  ifFalse:
						[self replace: more.
						c1 ← c2].
					self fintype.
					c1 ← c2 - Scrap length.
					^self complement].
				 "just a normal character"
				(char between: 11 and: 12)
					ifTrue: [more append: (char=11 ifTrue: ['ifTrue: '] ifFalse: ['ifFalse: '])]
					ifFalse: [more next← char]]]].
	self replace: more.
	c1 ← c2.
	self selectAndScroll
! !
!TextImage methodsFor: 'SCROLLING' stamp: ''!
scrollby: n | oldw topline |
	(n > 0 and: [n + (topline ← self lineofy: window minY) > lastline])
	  ifTrue: [n ← lastline - topline].
	n ← n * self lineheight max: self frameoffset.
	frame moveby: 0 ⌾ (0 - n).
	n abs ≥ window height
	  ifTrue:
		[self show.
		self select]
	  ifFalse:
		[ "need only to reshow part of window"
		oldw ← window.
		window ← (n < 0
				  ifTrue: [window inset: 0 ⌾ 0 and: [0 ⌾ (0 - n)]]
				  ifFalse: [window inset: 0 ⌾ n and: [0 ⌾ 0]]).
		window blt: window origin - (0 ⌾ n) mode: storing.
		n < 0
		  ifTrue: [window corner y← window origin y - n]
		  ifFalse: [window origin y← self yofline: (self lineofy: window corner y - n)].
		self displayall.
		self select.
		window ← oldw]
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
select: t | |
	self complement: off.
	c1 ← c2 ← t.
	self selectAndScroll
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
select | |
	sel ← off.
	c1 ≡ nil
	  ifTrue: [c1 ← c2 ← 1].
	self complement: on
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
unselect | |
	self complement: off
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
selectionAsStream | |
	^Stream new of: para text from: c1 to: c2 - 1
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
complementfrom: hair1 to: hair2 | temprect |
	hair1 minY "Complement the screen dots corresponding to the lines and part-lines of the paragraph between hair1 inclusive and hair2 exclusive.  If hair1 = hair2, this is a no-op.  If hair1 > hair2, they are reversed. This complementing happens in three parts, A, B, and C, between points 1 and 2, according to the following illustration:
					1AAA
					BBBB
					BBBB
					BBBB
					CCC2
unless there is just one line involved, as in:
					1DD2
" "one line case" = hair2 minY
	  ifTrue: [(((hair1 minX ≤ hair2 minX
		  ifTrue: [hair1 origin rect: hair2 corner]
		  ifFalse: [hair2 origin rect: hair1 corner]) intersect: frame) intersect: window) comp]
	  ifFalse:
		[hair1 minY > hair2 minY
		  ifTrue:
			[temprect ← hair1.
			hair1 ← hair2.
			hair2 ← temprect].
		temprect ← frame minX ⌾ hair1 maxY rect: frame maxX ⌾ hair2 minY.
		(((hair1 origin rect: temprect maxX ⌾ temprect minY) intersect: frame) intersect: window) comp.
		((temprect intersect: frame) intersect: window) comp.
		(((temprect minX ⌾ temprect maxY rect: hair2 corner) intersect: frame) intersect: window) comp]
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
selectRange: r | |
	self complement: off.
	c1 ← r start.
	c2 ← r stop.
	self complement: on
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
selectRange | |
	^c1 to: c2 - 1
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
complement | |
	self complement: on
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
selection | |
	para text empty
	  ifTrue: [^para copy].
	^para copy: c1 to: c2 - 1
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
complement: nsel | |
	nsel = sel
	  ifFalse:
		[ "already that way"
		(nsel = on and: [(user rawkbck or: [user redbug])])
		  ifFalse:
			[ "slippage"
			sel ← nsel.
			self reversefrom: c1 to: c2]]
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
reversefrom: char1 to: char2 | h1 h2 rect |
	self pointofchar: char1 andrect⦂ [:rect | rect] "Complement the dots corresponding to the the lines and part-lines of the paragraph between the left edge of char1 and the left edge of char2.  If char1 = char2, this is sort of a no-op.  If char1 > char2, this is undefined.".
	h1 ← rect.
	char2 = char1
	  ifTrue: [h2 ← h1 + (1 ⌾ 0)]
	  ifFalse:
		[self pointofchar: char2 andrect⦂ [:rect | rect].
		h2 ← rect].
	self complementfrom: h1 to: h2
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
selectAndScroll | l dy c1y ignored |
	l ← self lineheight.
	self select.
	c1y ← (self pointofchar: c1 andrect⦂ [:ignored | ignored]) y.
	dy ← c1y - window minY.
	dy ≥ 0
	  ifTrue: [dy ← c1y + l - 1 - window maxY max: 0].
	dy ≠ 0
	  ifTrue: [self scrollby: dy abs + l - 1 / l * dy sign]
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
selectword | a b dir t level open close s slen t10 |
	a ← b ← dir ← ¬1 "Select bracketed or word range, as a result of double-bug.".
	s ← para text.
	slen ← s length.
	level ← 1.
	open ← '([{<''"
'.
	close ← ')]}>''"
'.
	c1 ≤ 1
	  ifTrue:
		[dir ← 1.
		t ← c1]
	  ifFalse:
		[c1 > slen
		  ifTrue: [t ← c1 - 1]
		  ifFalse:
			[t ← open find: (a ← para ◦ (c1 - 1)).
			t > 0
			  ifTrue:
				[ "delim on left"
				dir ← 1.
				b ← close ◦ t.
				t ← c1 - 1]
			  ifFalse:
				[ "match to the right"
				t ← close find: (a ← para ◦ c1).
				t > 0
				  ifTrue:
					[ "delim on right"
					dir ← ¬1.
					b ← open ◦ t.
					t ← c1]
				  ifFalse:
					[ "match to the left"
					a ← ¬1.
					t ← c1]]]] "no delims - select a token".
	[level = 0 or: [(dir = 1
	  ifTrue: [t ≥ slen]
	  ifFalse: [t ≤ 1])]] whileFalseDo: 
		[(t10 ← s ◦ (t ← t + dir)) = b
		  ifTrue: [level ← level - 1]
		  ifFalse:
			[ "leaving nest"
			t10 = a
			  ifTrue: [level ← level + 1]
			  ifFalse:
				[ "entering nest"
				.
				a = ¬1
				  ifTrue:
					[(s ◦ t) tokenish
					  ifTrue:
						[ "token check goes left "
						t = 1
						  ifTrue:
							[c1 ← dir ← 1.
							t ← c2]]
					  ifFalse:
						[dir = ¬1
						  ifTrue:
							[c1 ← t + 1.
							dir ← 1.
							t ← c2 - 1]
						  ifFalse: [ "then right"
							level ← 0]]]]]].
	level ≠ 0
	  ifTrue: [t ← t + dir].
	dir = 1
	  ifTrue: [c2 ← t min: slen + 1]
	  ifFalse: [c1 ← t + 1]
! !
!TextImage methodsFor: 'SELECTION' stamp: ''!
selecting | pt t h1 h2 c h drag2 selection |
	t ← self charofpoint: (pt ← user mp) andrect⦂ [:h1 | h1].
	self complement: off.
	self fintype.
	(t = c1 and: [c1 = c2])
	  ifTrue:
		[[ "bugged hairline - maybe double-bug"
		user redbug and: [t = (self charofpoint: user mp andrect⦂ [:h | h])]] whileTrueDo: [ "wait for unclick or drawing selection"
			].
		user redbug ≡ false
		  ifTrue:
			[self selectword.
			self select.
			^true]].
	sel ← on "draw out and record (c1 and c2) a selection".
	c1 ← c2 ← t.
	h2 ← h1 + (1 ⌾ 0).
	self complementfrom: h1 to: h2.
	selection ← true.
	[pt ← user mpnext] whileTrueDo:
		[c ← self charofpoint: pt andrect⦂ [:h | h].
		c1 = c2
		  ifTrue: [drag2 ← c ≥ c2].
		drag2
		  ifTrue:
			[c < c1
			  ifTrue: [self pointofchar: (c ← c1) andrect⦂ [:h | h]].
			self complementfrom: h to: h2.
			c2 ← c.
			h2 ← h]
		  ifFalse:
			[c > c2
			  ifTrue: [self pointofchar: (c ← c2) andrect⦂ [:h | h]].
			self complementfrom: h1 to: h.
			c1 ← c.
			h1 ← h].
		h1 = h2
		  ifTrue: [self complementfrom: h1 to: (h2 ← h1 + (1 ⌾ 0))]].
	drag2
	  ifFalse: [ "get rid of extra line in backwards select"
		self complementfrom: h2 - (1 ⌾ 0) to: h2]
! !
!TextImage methodsFor: 'SYSTEM' stamp: ''!
presson: press in: r | |
	^para presson: press in: r
! !
!TextImage methodsFor: 'SYSTEM' stamp: ''!
copy | t |
	t ← TextImage new paragraph: para copy frame: frame copy style: style copy.
	t c1← c1.
	t c2← c2.
	t begintypein← begintypein.
	^t
! !
BitBlt subclass: #TextScanner
	instanceVariableNames: 'printing chari stopx xtable exceptions spacecount spacei spacex charpad text spacesize style para font fontno minascii maxascii glyphs frame looktype'
	classVariableNames: 'cr defaultExceptions'
	category: 'Text Objects'!
!TextScanner methodsFor: 'Access to parts' stamp: ''!
chari | |
	^chari "index of last char scanned"
! !
!TextScanner methodsFor: 'Access to parts' stamp: ''!
spacex | |
	^spacex
! !
!TextScanner methodsFor: 'Access to parts' stamp: ''!
spacei | |
	^spacei
! !
!TextScanner methodsFor: 'Access to parts' stamp: ''!
rightx | |
	^destX
! !
!TextScanner methodsFor: 'Access to parts' stamp: ''!
spacecount | |
	^0 - spacecount "because counted down from zero"
! !
!TextScanner methodsFor: 'Access to parts' stamp: ''!
leftx | |
	^destX - width
! !
!TextScanner methodsFor: 'Initialization' stamp: ''!
frame: t1 window: w para: t3 style: t4 printing: t5 | |
	frame ← t1.
	para ← t3.
	style ← t4.
	printing ← t5.
	super init.
	self screen: white.
	self toDisplay.
	exceptions ← defaultExceptions.
	fontno ← ¬1 "indicates font not set".
	text ← (para ← para asParagraph) text.
	NoteTaker
	  ifTrue: [self window: w]
! !
!TextScanner methodsFor: 'Initialization' stamp: ''!
exceptionTable: t1 | |
	exceptions ← t1
! !
!TextScanner methodsFor: 'Initialization' stamp: ''!
setfont: newfont liney: liney | |
	newfont = fontno
	  ifTrue: [ "Just bump y if same font"
		destY ← liney + style baseline - font ascent]
	  ifFalse:
		[font ← style font: (fontno ← newfont).
		xtable ← font xtable.
		spacesize ← font spacewidth.
		minascii ← font minascii.
		maxascii ← font maxascii.
		glyphs ← font glyphs.
		printing
		  ifTrue:
			[sourceRaster ← font raster.
			destY ← liney + style baseline - font ascent.
			height ← font height.
			NoteTaker
			  ifTrue: [source ← glyphs]]]
! !
!TextScanner methodsFor: 'Initialization' stamp: ''!
classInit | |
	 "TextScanner classInit."
	space ← 32.
	tab ← 9.
	cr ← 13.
	(defaultExceptions ← Vector new: 256) all← 0.
	defaultExceptions ◦ (space + 1) ← 12.
	defaultExceptions ◦ (tab + 1) ← 13.
	defaultExceptions ◦ (cr + 1) ← 4
! !
!TextScanner methodsFor: 'Scanning' stamp: ''!
type: ascii | |
	 "For incremental typing in a line"
	exceptions ◦ (ascii + 1) ≠ 0
	  ifTrue: [^exceptions ◦ (ascii + 1)].
	(ascii < minascii or: [ascii > maxascii])
	  ifTrue: [ascii ← maxascii + 1].
	stopx ← frame maxX.
	sourceX ← xtable ◦ (ascii + 1).
	(ascii ≠ space and: [ascii ≠ tab])
	  ifTrue: [width ← xtable ◦ (ascii + 2) - sourceX]
	  ifFalse:
		[ascii = space
		  ifTrue: [width ← spacesize]
		  ifFalse: [width ← style tabsize]].
	(destX + width > stopx or: [ascii = cr])
	  ifTrue: [^1].
	(destX ⌾ destY rect: stopx - width ⌾ (destY + height)) blt: destX + width ⌾ destY mode: storing "slide the rest of the line over".
	(ascii ≠ space and: [ascii ≠ tab])
	  ifTrue: [super effect: 16]
	  ifFalse: [super effect: 28].
	self callBLT.
	destX ← destX + width.
	^0
! !
!TextScanner methodsFor: 'Scanning' stamp: ''!
scanline: line at: y stopx: t3 stopchar: stopchar | ascii runx reason emphasis newrun endrun padwidth relx tabsize spacepad t15 |
	stopx ← t3.
	chari ← line starti - 1.
	spacecount ← line spaces.
	padwidth ← line padwidth.
	destX ← spacex ← runx ← frame minX.
	emphasis ← charpad ← spacepad ← 0.
	tabsize ← style tabsize.
	spacei ← ¬1.
	(t15 ← para alignment) = 0
	  ifFalse:
		[ "LeftFlush"
		t15 = 1
		  ifTrue:
			[spacecount > 0
			  ifTrue:
				[ "Justified"
				charpad ← padwidth / (line stopi - chari + spacecount).
				charpad > 0
				  ifTrue:
					[padwidth ← padwidth - (charpad * (line stopi - chari - 2 - spacecount)).
					printing
					  ifTrue: [(frame minX ⌾ y rect: frame maxX ⌾ (y + style lineheight)) clear: white]].
				spacepad ← padwidth / spacecount.
				spacecount ← spacecount - (padwidth \ spacecount)]]
		  ifFalse:
			[t15 = 2
			  ifTrue: [destX ← destX + (padwidth / 2)]
			  ifFalse:
				[ "Centered"
				t15 = 4
				  ifTrue: [destX ← destX + padwidth]]]] "RightFlush".
	reason ← 10.
	[reason < 10] whileFalseDo: 
		[ "2=>stopx, 3=>stopchar, 4=>CR, 
			10=>end of run, 11=>ascii out of range, 12-254=>exception chars"
		reason = 10
		  ifTrue:
			[reason ← 255.
			chari ≥ stopchar
			  ifTrue: [reason ← 3]
			  ifFalse:
				[ "new run"
				printing
				  ifTrue: [self emphasize: emphasis leftx: runx rightx: destX liney: y].
				runx ← destX.
				chari ← chari + 1.
				newrun ← para runAndVal: chari.
				emphasis ← newrun ◦ 2 land: 15.
				endrun ← newrun ◦ 1 + chari - 1 min: stopchar.
				self setfont: (newrun ◦ 2 lshift: ¬4) liney: y]]
		  ifFalse:
			[reason ← self scanword: endrun.
			reason > 10
			  ifTrue:
				[reason = 12
				  ifTrue:
					[spacecount ← spacecount - 1 "space".
					spacecount = 0
					  ifTrue: [spacepad ← spacepad + 1].
					spacei ← chari.
					spacex ← destX.
					width ← spacesize + spacepad.
					printing
					  ifTrue:
						[super effect: 28.
						self callBLT]]
				  ifFalse:
					[reason = 13
					  ifTrue:
						[relx ← destX - frame minX "tab".
						width ← (para alignment > 0
								  ifTrue: [tabsize + charpad]
								  ifFalse: [relx + tabsize | tabsize - relx]).
						printing
						  ifTrue:
							[super effect: 28.
							self callBLT]]
					  ifFalse:
						[reason = 11
						  ifTrue: [ascii ← maxascii + 1].
						sourceX ← xtable ◦ (ascii + 1).
						width ← xtable ◦ (ascii + 2) - sourceX.
						printing
						  ifTrue:
							[super effect: 16.
							self callBLT].
						width ← width + charpad]].
				(destX ← destX + width) > stopx
				  ifTrue: [reason ← 2]
				  ifFalse:
					[chari ≥ endrun
					  ifTrue: [reason ← 10]
					  ifFalse: [chari ← chari + 1]]]
			  ifFalse:
				[reason = 4
				  ifTrue: [width ← 0] "CR"]]].
	printing
	  ifTrue: [self emphasize: emphasis leftx: runx rightx: destX liney: y]
! !
!TextScanner methodsFor: 'Scanning' stamp: ''!
scanword: endrun | ascii |<primitive: 76>
	 "Scan or print text until terminated by x-value, special character or new format run.
	Returns an Integer as follows:
		2=>stopx, 10=>end of run, 11=>ascii out of range,
		else exceptions◦chari (4=CR, 12=space, 13=tab)"
	super effect: 16 "exceptions and xtable are Vectors of Integers, printing is Boolean, rest are Integers" "becomes: super effect: 3 in new encoding".
	[chari > endrun] whileFalseDo: 
		[ascii ← text ◦ chari.
		exceptions ◦ (ascii + 1) ≠ 0
		  ifTrue: [^exceptions ◦ (ascii + 1)].
		(ascii < minascii or: [ascii > maxascii])
		  ifTrue: [^11].
		sourceX ← xtable ◦ (ascii + 1).
		width ← xtable ◦ (ascii + 2) - sourceX.
		printing
		  ifTrue: [self callBLT].
		width ← width + charpad.
		destX ← destX + width.
		destX > stopx
		  ifTrue: [^2].
		chari ← chari + 1].
	chari ← chari - 1.
	^10
! !
!TextScanner methodsFor: 'Scanning' stamp: ''!
emphasize: emphasis leftx: leftx rightx: rightx liney: liney | y y2 |
	 "Should eventually use self callBLT, to get windowing and go faster"
	(font ≡ nil or: [font height = style lineheight])
	  ifFalse:
		[font ascent < style baseline
		  ifTrue:
			[y ← liney + (style baseline - font ascent).
			(leftx ⌾ liney rect: rightx ⌾ y) clear: white].
		font descent < (style lineheight - style baseline)
		  ifTrue:
			[y ← liney + (style baseline + font descent).
			y2 ← y + (style lineheight - (style baseline + font descent)).
			(leftx ⌾ y rect: rightx ⌾ y2) clear: white]].
	emphasis = 0
	  ifFalse:
		[emphasis ≥ 8
		  ifTrue:
			[emphasis ← emphasis - 8 "Strike-out".
			y ← liney + (style baseline / 2).
			(leftx ⌾ y rect: rightx ⌾ (y + 1)) clear: black].
		emphasis ≥ 4
		  ifTrue:
			[emphasis ← emphasis - 4 "Underline".
			y ← liney + style baseline.
			(leftx ⌾ y rect: rightx ⌾ (y + 1)) clear: black].
		emphasis ≥ 2
		  ifTrue:
			[emphasis ← emphasis - 2 "Itallic".
			y ← liney + style lineheight - 4.
			[y ≤ liney] whileFalseDo: 
				[(leftx ⌾ liney rect: rightx - 1 ⌾ y) blt: leftx + 1 ⌾ liney mode: storing.
				(leftx ⌾ liney rect: leftx + 1 ⌾ y) clear: white.
				y ← y - 4]].
		emphasis ≥ 1
		  ifTrue:
			[emphasis ← emphasis - 1 "Bold".
			(leftx ⌾ liney rect: rightx ⌾ (liney + style lineheight)) blt: leftx + 1 ⌾ liney mode: oring]]
! !
Object subclass: #TokenCollector
	instanceVariableNames: 'sink parenstack'
	classVariableNames: ''
	category: 'Text Objects'!
!TokenCollector methodsFor: 'Constructors' stamp: ''!
separator: c | |
	
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
identifier: s | |
	self next← s unique
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
float: i fraction: f exp: e | |
	self next← (i + '.' + f + 'e' + e) asFloat
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
string: s | |
	self next← s
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
otheratom: s | |
	self next← s unique
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
rightparen | |
	parenstack empty
	  ifFalse:
		[ "Error will be caught elsewhere"
		parenstack last next← sink contents.
		sink ← parenstack pop]
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
leftparen | |
	parenstack next← sink.
	sink ← (Vector new: 10) asStream
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
integer: s | |
	self next← s asInteger
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
keyword: s | |
	self next← s unique
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
onechar: c | x |
	x ← String new: 1.
	x ◦ 1 ← c.
	self next← x unique
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
trailer: s | |
	
! !
!TokenCollector methodsFor: 'Constructors' stamp: ''!
comment: s | |
	
! !
!TokenCollector methodsFor: 'Finalization' stamp: ''!
next← obj | |
	sink next← obj "subclasses can override easily"
! !
!TokenCollector methodsFor: 'Finalization' stamp: ''!
contents | |
	 "Close all parentheses first"
	[parenstack empty] whileFalseDo:  [self rightparen].
	^sink contents
! !
!TokenCollector methodsFor: 'Finalization' stamp: ''!
notify: errorString | |
	user notify: errorString
! !
!TokenCollector methodsFor: 'Initialization' stamp: ''!
to: v | |
	 "Initialize"
	sink ← v asStream.
	parenstack ← (Vector new: 5) asStream
! !
!TokenCollector methodsFor: 'Initialization' stamp: ''!
default | |
	self to: (Vector new: 20)
! !
