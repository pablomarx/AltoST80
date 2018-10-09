!AbstractInteger methodsFor: 'undefined' stamp: ''!
factorial | |
	self = 0
	  ifTrue: [^1].
	self < 0
	  ifTrue: [self error: 'factorial invalid for: ' + self asString]
	  ifFalse: [^self * (self - 1) factorial]
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
/ aNumber | | "temporary"
	 "/ means div: when used between integers for now"
	(aNumber differentGenerality: self)
	  ifTrue: [^self retry: #/ coercing: aNumber].
	^self//aNumber
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
% aNumber | |  "create a Fraction"
	^self over: aNumber
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
withDenominator: denom | |
	^self asFraction withDenominator: denom
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
readFrom: aStream radix: radix | char digit value none neg |
	 "returns the next Integer or LargeInteger contained in aStream up to the first invalid character
	constants:
		('0'◦1) = 48
		('A'◦1) = 65
		('A'◦1) - 10 = 55"

	neg ← (aStream ∢ 45 "-" ifTrue: [true] ifFalse: [aStream ∢ 21 "¬"]).
	value ← 0.
	none ← true.
	aStream do:
		[:char | digit ← char - (char ≥ 65
				  ifTrue: [55]
				  ifFalse: [48]).
		(digit < 0 or: [digit ≥ radix])
		  ifTrue:
			[(none or: [digit between: 0 and: 9])
			  ifTrue: [^Error new message: 'ILLEGAL/MISSING DIGIT'].
			aStream skip: -1.
			neg
			  ifTrue: [^value negated asInteger "catch 0 minVal"].
			^value]
		  ifFalse:
			[value ← value * radix + digit.
			none ← false]].
	none
	  ifTrue: [^Error new message: 'INCOMPLETE NUMBER'].
	neg
	  ifTrue: [^value negated asInteger "catch 0 minVal"].
	^value
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
readFrom: aStream | radix |
	 "doesn't actually alter self; returns a new Integer or LargeInteger"
	aStream ← aStream asStream.
	radix ← self readFrom: aStream radix: 10.
	(radix is: Error) ifTrue: [^radix].
	aStream ∢ 114 "r"
	  ifTrue:
		[ "<radix>r<integer>"
		radix ≤ 1
		  ifTrue: [^Error new message: 'INVALID RADIX'].
		^self readFrom: aStream radix: radix]
	  ifFalse: [^radix] "no radix--this is the number"
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
numerator | |
	^self
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
denominator | |
	^1
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
asFraction | |
	^self
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
neg | |
	 "for LargeInteger compatibility"
	self < 0
	  ifTrue: [^true].
	^false
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
base8 | strm |
	strm ← Stream default.
	strm append: '8r'.
	self printOn: strm base: 8.
	^strm contents
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
base: b | strm |
	strm ← Stream default.
	self printOn: strm base: b.
	^strm contents
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
over: denom | |
	^(Fraction new numerator: self denominator: denom) reduced
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
radix: radix | strm |
	strm ← Stream default.
	radix printOn: strm.
	strm append: 'r'.
	self printOn: strm base: radix.
	^strm contents
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	self printOn: strm base: 10 "default print radix"
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
truncated | |
	^self
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
rounded | |
	^self
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
floor | |
	^self
! !
!AbstractInteger methodsFor: 'undefined' stamp: ''!
ceiling | |
	^self
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
updateLeader: page | s time lastwrite |
	time ← user timewords "see <Alto>AltoFileSys.D, (p.3 leader page) for further info".
	s ← page asStream.
	(type anymask: write)
	  ifTrue:
		[directory flush "set creation/write/read date and file name".
		lastwrite ← time.
		s append: time.
		s append: time.
		s append: time.
		name empty
		  ifFalse: [s nextString← name]]
	  ifFalse:
		[ "remember creation, skip write, update read date"
		lastwrite ← s next: 4.
		s skip: 4.
		s append: time].
	self Write: page.
	^lastwrite
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
dskprim: diskNumber address: a command: com page: string | |<primitive: 79>
	 "0/1" "starting Alto disk address" "disk command (usually CCR, CCW, CWW)" "string containing label and data"
	error ← ¬1 "if disk routine encounters an error,
	error ← (DCB status, to be interpreted by errorString:).
	⇑false" "if other error occurs, e.g. nil instead of Integer...".
	^false
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
leader: t1 | |
	leader ← t1
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
pageAddresses: t1 | |
	pageAddresses ← t1
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
leader | |
	^leader
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
errorString: status | t s |
	status = ¬1 "see Alto hardware manual for details on error word format"
	  ifTrue: [^'primitive failure, bad args?'].
	s ← Stream default.
	s append: #('' 'hardware error or sector overflow' 'check error' 'disk command specified illegal sector' ) ◦ (1 + (status land: 3)).
	(1 to: 6) do:
		[:t | (status allmask: (128 lshift: 1 - t))
		  ifTrue:
			[s space.
			s append: #('seek failed, possible illegal track' 'seek in progress' 'disk unit not ready' 'hardware late' 'hardware not transferring' 'checksum' ) ◦ t]].
	s space.
	s append: status base8.
	^s contents
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
close | |
	type ← self updateLeader: (self read: 0) "to look at at reopen"
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
open | |
	type ← read "don't find last page immediately.  for later close"
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
entryClass | |
	^AltoFilePage
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
readFrom: s | |
	serialNumber ← s next: 4 "read file description from SysDir".
	s skip: 4 "self version: s nextword. s skip: 2".
	leader ← directory virtualToReal: s nextword.
	name ← s nextString.
	s padNext
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
init | |
	super init.
	pageAddresses ← AltoFileAddressTable new
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
fileSize | |
	^11 + (name length "sn, version, fn, leader, name" lor: 1)
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
storeOn: s | |
	s append: serialNumber.
	s nextword← 1.
	s nextword← 0.
	s nextword← directory realToVirtual: leader.
	s nextString← name.
	s padNext← 0
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
sameFile | page s |
	(page ← self newPage: 0) address: leader.

	"if any of following  tests fail, File will be reinitialized"
	(((page doCommand: CCR error: false) "serial number match"
		and: [page pageNumber "correct page number" = 0])
		and: [s ← page asStream. "last write was by us"
			type = (s next: 4) and: [s skip: 8. "same name"
			(name compare: s nextString) = 2]])
	ifTrue: 
		["check last page (if known)"
		(lastpn and: [pageAddresses])
			ifTrue: [page pageNumber: lastpn; address: pageAddresses◦lastpn.
				^(page doCommand: CCR error: false) and: [page lastPage]].
		^true]
	ifFalse: [^false]
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
findLastPage | |
	self read: 20000.
	^lastpn
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
classInit | |
	AltoFilePool declare: #(CRR CCR CCW CWW ) as: #(18496 18512 18520 18536 ) "before filing in:
		Smalltalk declare: ↪AltoFilePool as: (SymbolTable new init: 32)" "disk commands".
	AltoFilePool declare: #(dfmask boffset dirname ) as: #(1024 32 'SysDir.' ) "bit means active directory entry" "byte offset of bit table in DiskDescriptor".
	AltoFilePool declare: #(nextp backp numch pagen vn ) as: #(1 2 4 5 6 )
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
Get: page | p pn |
	pn ← page pageNumber.
	(self Read: page)
	  ifTrue: [^page].
	 "page now contains last page"
	(lastpn to: pn - 1) do:
		[:p | page pageNumber: p.
		page length: page dataLength "this writes current and allocates next (empty) page".
		page ← self Write: page].
	^page
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
Read: page | pn p palen |
	pn ← page pageNumber.
	pageAddresses
	  ifTrue: [palen ← pageAddresses length]
	  ifFalse:
		[pn = 0
		  ifTrue: [palen ← 0]
		  ifFalse: [^false]].
	((palen min: pn) to: pn) do:
		[:p | page pageNumber: p "set up page for checking" "zeroed by machine code
			header: nextp ← [p < palen⇒ [pageAddresses◦(p+1)] 0];
			header: backp ← [p=0⇒ [0]; =1⇒[leader] pageAddresses◦(p-1)];
			length: [p < palen⇒ [page dataLength] 0];".
		page address: (p = 0
		  ifTrue: [leader]
		  ifFalse: [pageAddresses ◦ p]).
		page doCommand: CCR error: 'readPage:'.
		page lastPage
		  ifTrue:
			[(lastpn ← p) < pn
			  ifTrue: [^false]]
		  ifFalse:
			[(p ≥ palen and: [pageAddresses])
			  ifTrue: [pageAddresses ◦ (p + 1) ← page header: nextp] "no need to store if already known or no page table"]].
	^page
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
endFile: page | nextPage pn |
	page ≡ false
	  ifTrue: [pn ← ¬1 "free all of file"]
	  ifFalse:
		[page full
		  ifTrue:
			[nextPage ← self Write: page "if page was a full last page, next is an empty (and now last) page".
			nextPage lastPage
			  ifTrue: [^nextPage].
			page ← self read: page pageNumber + 1.
			page empty
			  ifTrue: [^page].
			page length: 0].
		page header: nextp ← 0.
		self Write: page "free rest of file".
		pn ← page pageNumber].
	lastpn ← false "reset by readPage:".
	[lastpn ≡ false and: [(nextPage ← self read: (pn ← pn + 1))]] whileTrueDo:
		[nextPage init.
		nextPage freePage.
		nextPage doCommand: CWW error: 'endFile:'.
		directory deallocate: nextPage].
	page
	  ifTrue: [pageAddresses position← lastpn ← page pageNumber].
	^page
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
Write: page | nextPage labelDirty returnPage |
	((labelDirty ← page lastPage) and: [page full])
	  ifTrue:
		[returnPage ← nextPage ← self newPage "last page can't be full, so glue on another page".
		directory allocate: nextPage after: (directory realToVirtual: page address).
		nextPage init.
		nextPage header: backp ← page address.
		nextPage pageNumber: (lastpn ← page pageNumber + 1).
		nextPage serialNumber: serialNumber.
		nextPage doCommand: CWW error: 'writePage: (allocate)' "link to current page".
		page header: nextp ← nextPage address.
		pageAddresses
		  ifTrue: [pageAddresses ◦ lastpn ← nextPage address] "growSmalltalkBy:"]
	  ifFalse: [returnPage ← page] "whenever a last (or second last) page is written, write label also".
	self doCommand: (labelDirty
	  ifTrue: [CWW]
	  ifFalse: [CCW]) page: page error: 'writePage:'.
	type ← read + write.
	^returnPage
! !
!AltoFile methodsFor: 'undefined' stamp: ''!
doCommand: com page: page error: e | |
	error ← nullString.
	(self dskprim: directory diskNumber address: page address command: com page: page page)
	  ifTrue: [^page].
	error ← self errorString: error "set by dskprim:...".
	^self error: e
! !
!AltoFileAddressTable methodsFor: 'undefined' stamp: ''!
position← p | l |
	 "shortens (for file shorten)"
	p > max
	  ifTrue: [user notify: 'invalid extension']
	  ifFalse:
		[max ← p.
		(l ← starts findSorted: max) < starts length
		  ifTrue:
			[starts ← starts copy: 1 to: l.
			values ← values copy: 1 to: l]]
! !
!AltoFileAddressTable methodsFor: 'undefined' stamp: ''!
◦ i | base |
	base ← super ◦ i.
	^dp0 virtualToReal: base + offset
! !
!AltoFileAddressTable methodsFor: 'undefined' stamp: ''!
◦ i ← val | virt |
	virt ← dp0 realToVirtual: val.
	starts ≡ nil
	  ifTrue:
		[super ◦ i ← virt.
		^val].
	super ◦ i ← virt - i + starts last "superclass tries for constant runs".
	offset > 0
	  ifTrue: [^val].
	 "OK if same run"
	values last← virt.
	^val "else fix new run value base"
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
deallocate: page | index ch m |
	dirFile ≡ nil ifTrue: [self open].
	index ← self realToVirtual: page address. "character position"
	bitsFile position← index / 8 + boffset.
	ch ← bitsFile next "bit position".
	m ← 128 lshift: 0 - (index land: 7) "make page free by turning off bit in DiskDescriptor".
	(ch land: m) = m
	  ifTrue:
		[bitsFile skip: ¬1.
		bitsFile next← ch - m]
	  ifFalse:
		[user cr.
		user show: 'page already free (dealloc:)']
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
filesMatching: pattern | files v i |
	files ← self match: (pattern last = ('.' ◦ 1)
			  ifTrue: [pattern]
			  ifFalse: [pattern + '.']).
	v ← Vector new: files length.
	(1 to: v length) do: [:i | v ◦ i ← (files ◦ i) name].
	^v
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
freePages | npages ch i |
	self open.
	bitsFile position← boffset.
	npages ← 0.
	(1 to: totalPages by: 8) do:
		[:i | (ch ← bitsFile next) = 255
		  ifFalse:
			[ "all used" "possibly up to 8 unused"
			npages ← npages + 8.
			[ch = 0] whileFalseDo: 
				[npages ← npages - (ch land: 1).
				ch ← ch lshift: ¬1]]].
	^npages
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
allocate: nextPage after: address | index stop ch m vadr |
	index ← false.
	[true] whileTrueDo:
		[ "go around bittable from address to end, and beginning to address.
		we start over again if the table appears full or bitsFile is out of sync"
		(index and: [stop ≥ totalPages])
		  ifTrue:
			[stop ← address "wrap around to where we started".
			index ← 0]
		  ifFalse:
			[index ≡ false
			  ifFalse: [ "first time or bitsFile out of sync" "disk probabbly full"
				user quitThen: '//   YOUR DISK IS FULL - Please make some space available.
//   Then resume Smalltalk and interrupt or continue as desired...'].
			self open "index by bits rather than bytes? close enough for now".
			index ← address land: ¬8.
			stop ← totalPages].
		bitsFile position← index / 8 + boffset.
		[index and: [(index ← index + 8) ≤ stop]] whileTrueDo:
			[(ch ← bitsFile next) = 255
			  ifFalse:
				[ "8 full" "check that bitsFile position is correct --
				possibly out of sync with index if  growSmalltalkBy: occurred?"
				bitsFile position ≠ (index / 8 + boffset)
				  ifTrue: [index ← false]
				  ifFalse:
					[m ← 128.
					(index - 8 to: index - 1) do:
						[:vadr | (ch land: m) "nomask:" = 0
						  ifTrue:
							[bitsFile skip: ¬1 "page appears free. first update DiskDescriptor".
							bitsFile next← ch ← ch lor: m "then check if page is really free".
							vadr = 0
							  ifFalse:
								[ "O.S. boot"
								(nextPage init.
								nextPage freePage.
								nextPage address: (self virtualToReal: vadr).
								nextPage doCommand: CCR error: false)
								  ifTrue: [^vadr] "page not really free"]] "page not free according to bit".
						m ← m lshift: ¬1]]]]]
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
diskNumber | |
	^directory "directory is: Integer⇒ [" "] ⇑directory diskNumber"
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
checkName: fname fixing: fixing | x copy special |
	fname empty
	  ifTrue:
		[fixing
		  ifTrue: [^'$'].
		 "empty name"
		^false]
	  ifFalse:
		[fname length > 38
		  ifTrue:
			[fixing
			  ifTrue: [fname ← fname ◦ (1 to: 38)]
			  ifFalse: [ "name too long"
				^false]].
		copy ← (String new: fname length + 1) asStream.
		special ← '.-+$!?'.
		fname do:
			[:x | (x isletter "check characters: alphanumeric or 6 special" or: [((special has: x) or: [x isdigit])])
			  ifTrue: [copy next← x]
			  ifFalse:
				[fixing
				  ifTrue: [copy next← special ◦ 2]
				  ifFalse: [ "illegal character"
					^false]]].
		fixing
		  ifTrue:
			[fname last = (special ◦ 1)
			  ifTrue: [copy skip: ¬1]]
		  ifFalse:
			[fname last ≠ (special ◦ 1)
			  ifTrue: [copy next← special ◦ 1]].
		^copy contents]
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
deleteEntry: file | p |
	p ← dirFile position "called only by Delete: and rename:newName:
	read and save".
	self nextEntry: file.
	dirFile position← p "delete it from directory (turn off bit in entry length word)".
	p ← dirFile nextword land: dfmask - 1.
	dirFile skip: ¬2.
	dirFile readwrite.
	dirFile nextword← p.
	dirFile readonly.
	dirFile skip: ¬2.
	^file
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
addEntry: file | entrysize holesize |
	 "called only by Insert: and rename:newName:"
	(holesize ← dirFile nextword)
	  ifTrue:
		[holesize ← holesize land: dfmask - 1 "either a deleted entry or rename entry".
		dirFile skip: ¬2] "at end".
	entrysize ← self entrySize: file.
	dirFile readwrite.
	dirFile nextword← entrysize + dfmask.
	file storeOn: dirFile.
	(holesize and: [entrysize < holesize])
	  ifTrue: [dirFile nextword← holesize - entrysize "mark remaining hole"].
	dirFile readonly.
	bitsFile flush
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
diskID | f u |
	(f ← self oldFile: 'sys.boot' "return user name and disk name installed in O.S.") readonly.
	f position← 512.
	u ← f nextString.
	f padNext.
	u ← {u , f nextString}.
	f close.
	^u
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
configure: s | nDisks nHeads nTracks |
	nDisks ← s nextword "read disk configuration from a Stream:
		either leader page of SysDir or beginning of DiskDescriptor".
	nTracks ← s nextword.
	nHeads ← s nextword.
	nSectors ← s nextword.
	diskPages ← nTracks * nHeads * nSectors.
	totalPages ← nDisks * diskPages
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
flush | |
	bitsFile ≡ nil
	  ifFalse: [bitsFile flush]
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
close | |
	self obsolete
	  ifFalse:
		[dirFile close.
		bitsFile ≡ nil
		  ifFalse: [ "an interrupted open?"
			bitsFile close].
		super close]
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
Insert: file | sn page |
	file serialNumber: (sn ← self allocateSN: file) "allocate a new page (more success after O.S. stuff, bittable etc.)".
	self allocate: (page ← file newPage) after: 800 "write 0th -- leader, in the process filling it in and then creating first page".
	page init.
	page serialNumber: sn.
	page length: page dataLength.
	file leader: page address.
	file type: write.
	file updateLeader: page.
	self addEntry: file
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
Delete: file | t2 |
	(t2 ← self deleteEntry: file) open.
	t2 endFile: false.
	bitsFile flush
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
reset | |
	self obsolete
	  ifTrue: [self open]
	  ifFalse: [self flush].
	dirFile readonly.
	dirFile reset
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
entrySize: file | |
	^1 + (file fileSize "entry size in words" / 2)
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
release | |
	dirFile ← bitsFile ← nil
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
open | f s a page len elen type |
	nil ≠ dirFile
	  ifFalse:
		[ "assume some defaults in case DSHAPE is not in SysDir leader page.
	these should only be needed if the disk is old (and not scavenged).
	they will not work if a 14 sector system is missing DSHAPE (unlikely) since addresses of first page of directory and of DiskDescriptor might be computed incorrectly.
	in a Smalltalk-76 system, nSectors, diskPages had better eventually match:
		| a. a ← Vmem specialLocs◦13. mem◦(a+5), (mem◦(a+6))
	"
		nSectors ← 12.
		diskPages ← 812 * nSectors.
		totalPages ← 2 * diskPages "read SysDir leader page to find out file system configuration.  see AltoFileSys.D".
		f ← self find: dirname "to prevent address of page 1 from being stored".
		f pageAddresses: false "length of property list, in words".
		page ← f read: 0.
		len ← page ◦ 494.
		len ≠ 210
		  ifFalse:
			[ "scan file properties for DSHAPE"
			s ← page asStream.
			s skipwords: page ◦ 493.
			[len > 0] whileTrueDo:
				[type ← s next.
				type = 0
				  ifTrue: [len ← 0 "0 terminates list.  property not found. try to read if from DiskDescriptor"]
				  ifFalse:
					[elen ← s next.
					(type = 1 and: [elen = 5])
					  ifTrue:
						[self configure: s "DSHAPE. read property" "set flags so configure and loop are not done again".
						s ← false.
						len ← 0]
					  ifFalse:
						[ "skip over other property"
						len ← len - elen.
						s skipwords: elen - 1]]]] "now, with the correct (or default) file system configuration,
	store the virtual address of next page (1), and create a FileStream on SysDir".
		a ← AltoFileAddressTable new.
		a ◦ 1 ← page header: nextp.
		f pageAddresses: a.
		(dirFile ← f asStream) readonly.
		(bitsFile ← self oldFile: 'DiskDescriptor') readwrite.
		s
		  ifTrue: [self configure: bitsFile "configuration not read from SysDir. this will work for 12 sector systems.
		14 sector systems should have had the DSHAPE property"].
		super open]
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
obsolete | |
	^dirFile ≡ nil
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
entryClass | |
	^AltoFile
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
Position← entry | name elen s holepos holesize entrysize nlen sk |
	name ← entry name "entry format
		1	n (length in words, including this one) + undeleted bit (dfmask)
		2-3	serialNumber
		4	version
		5	0?
		6	virtual address of page 0
		7-n name as Bcpl string (extra 0 if length even)".
	(dirFile ≡ nil and: [(name compare: dirname) = 2])
	  ifTrue: [^true].
	self reset.
	holepos ← false.
	holesize ← dfmask.
	nlen ← name length.
	entrysize ← self entrySize: entry "desired entry size".
	[s ← dirFile nextword] whileTrueDo:
		[elen ← s land: dfmask - 1 "entry length in words".
		entrysize > elen
		  ifTrue: [sk ← ¬2 "entry too small"]
		  ifFalse:
			[s = elen
			  ifTrue:
				[sk ← ¬2 "deleted entry. check hole size for later inserting or renaming".
				elen < holesize
				  ifTrue:
					[holesize ← elen "hole is the smallest so far".
					holepos ← dirFile position]]
			  ifFalse:
				[ "normal entry, big enough"
				dirFile skip: 10.
				nlen ≠ dirFile next
				  ifTrue: [sk ← ¬13 "name wrong size"]
				  ifFalse:
					[sk ← ¬13 - nlen.
					(name compare: (dirFile next: nlen)) = 2
					  ifTrue:
						[dirFile skip: sk "name match, position back to beginning of entry".
						^entry]]]] "sk is the character offset from the entry header word to the next entry".
		dirFile skip: elen * 2 + sk].
	holepos
	  ifTrue: [dirFile position← holepos - 2] "at end of dirFile".
	^false
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
nextEntry: file | s elen |
	dirFile ≡ nil
	  ifTrue:
		[(file name compare: dirname) = 2
		  ifTrue:
			[file serialNumber: {¬32768 , 100} "return system directory file. known serialNumber and leader".
			file leader: 4096.
			^file].
		self error: 'directory not open']
	  ifFalse:
		[ "return the next file entry, ignore deleted entries,
	and leave dirFile positioned before next entry"
		[s ← dirFile nextword] whileTrueDo:
			[elen ← s land: dfmask - 1.
			(s allmask: dfmask)
			  ifTrue:
				[file readFrom: dirFile.
				dirFile skip: elen * 2 - (file fileSize + 2).
				^file].
			 "deleted entry, again"
			dirFile skipwords: elen - 1].
		^false]
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
rename: file newName: newName | holesize pos |
	(newName ← self checkName: newName)
	  ifTrue:
		[self position← newName
		  ifTrue: [self error: 'new name already exists: ' + newName]
		  ifFalse: [ "a possible insertion place"
			pos ← dirFile position]]
	  ifFalse: [self error: 'illegal new name: ' + newName].
	(self Find: (file ← self makeEntry: file))
	  ifTrue:
		[holesize ← dirFile nextword land: dfmask - 1.
		dirFile skip: ¬2.
		file name: newName.
		(self entrySize: file) "new size of entry" ≤ holesize
		  ifTrue:
			[pos ← dirFile position "new entry will fit in current entry" "read and save entry".
			self nextEntry: file]
		  ifFalse: [ "delete and save entry"
			self deleteEntry: file] "position to same entry or hole discovered earlier".
		dirFile position← pos.
		self addEntry: (file name: newName).
		(file type Is: Integer)
		  ifTrue: [file type: write "file is open. defer leader page change until someone closes it"]
		  ifFalse:
			[ "close file: updating name in leader page"
			file type: write.
			file close]]
	  ifFalse: [file error: 'rename: old name does not exist']
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
checkName: s | |
	^self checkName: s fixing: false
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
realToVirtual: adr | |
	^(adr lshift: ¬12) "see virtualToReal:.
	Alto address format is
	bits
	0-3	sector number (0 - 015, i.e. 12 or 14 sectors)
	4-12	cylinder number (0 - 0312, Model 31; 0-0625, Model 44)
	13		head number (0-1)
	14		disk number	(0-1)
	15		restore bit.

	in a system with two separable disks, addresses on disk 1 have a 0 disk bit, which is complemented by the disk primitive" "sector: field" + (nSectors * ((adr land: 4092) "cylinder and head: field*" lshift: ¬2)) +  "disk: field*pages per disk"
	((adr land: 2) = 2
	  ifTrue: [diskPages]
	  ifFalse: [0]) "diskPages*(adr land: 2)/2" "vadr < 0 or⦂ vadr ≥ totalPages⇒ [
		self error: 'illegal disk address']"
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
allocateSN: file | sn |
	bitsFile position← 8.
	sn ← bitsFile next: 4.
	sn word: 2 ← (sn word: 2) + 1 = 0
	  ifTrue: [sn word: 1 ← (sn word: 1) "overflow" + 1].
	bitsFile skip: ¬4.
	bitsFile append: sn.
	^sn
! !
!AltoFileDirectory methodsFor: 'undefined' stamp: ''!
virtualToReal: vadr | t2 d |
	 "inverse of realToVirtual:" "vadr < 0 or⦂ vadr ≥ totalPages⇒ [
		self error: 'illegal virtual address']" "faster to do /\ for normal Integers" "t ← vadr intdiv: diskPages.
	sec ← t◦2 intdiv: nSectors"
	vadr < diskPages
	  ifTrue:
		[d ← 0.
		t2 ← vadr]
	  ifFalse:
		[d ← 2.
		t2 ← vadr \ diskPages].
	^(t2 \ nSectors "sector" lshift: 12) + (t2 / nSectors "cylinder & head" lshift: 2) + d "disk" "(vadr / diskPages) lshift: 1"
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
freePage | |
	page fill: 11 to: 16 with: 255 "label for a free page: version, sn1, sn2 = ¬1"
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
pageNumber: pn | |
	page word: pagen ← pn "self header:"
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
address: t1 | |
	address ← t1
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
pageNumber | |
	^page word: pagen "self header:"
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
serialNumber: sn | |
	page copy: 13 to: 16 with: sn from: 1 to: 4 "page◦(13 to: 16) ← sn" "self header:".
	page word: vn ← 1 "fixed version"
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
lastPage | |
	^(page word: nextp) "self header:" = 0
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
length | |
	^page word: numch "self header:"
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
headerLength | |
	^16
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
length: len | |
	page word: numch ← len "self header:"
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
address | |
	^address
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
init | |
	page ≡ nil
	  ifTrue: [super init]
	  ifFalse: [ "nextp, backp, lnused, numch, pn"
		page fill: 1 to: 10 with: 0]
! !
!AltoFilePage methodsFor: 'undefined' stamp: ''!
serialNumber | |
	^page ◦ (13 to: 16)
! !
!Array methodsFor: 'undefined' stamp: ''!
hash | |
	 "make sure = arrays hash =ly"
	self length = 0
	  ifTrue: [^17171].
	^(self ◦ 1) hash + (self ◦ self length) hash
! !
!Array methodsFor: 'undefined' stamp: ''!
isArray | |
	
! !
!Array methodsFor: 'undefined' stamp: ''!
species | |
	^Vector
! !
!Array methodsFor: 'undefined' stamp: ''!
isIntervalBy1 | |
	^false
! !
!Array methodsFor: 'undefined' stamp: ''!
sumTo: subTotal | x |
	 "add all my elements to this subTotal (usually 0 or 0.0)"
	self do: [:x | subTotal ← subTotal + x].
	^subTotal
! !
!Array methodsFor: 'undefined' stamp: ''!
transform⦂ each to⦂ expr | s i |
	 "a copy of me with each element transformed"
	s ← (self species new: self length) asStream.
	(1 to: self length) do:
		[:i | each value← self ◦ i.
		s next← expr eval].
	^s asArray
! !
!Array methodsFor: 'undefined' stamp: ''!
frequencies | d x |
	 "return a sorted vector ((freq item) (freq item) ...)"
	d ← Dictionary new init: 64.
	self do: [:x | d tally: x].
	^d asInvertedVector sort
! !
!Array methodsFor: 'undefined' stamp: ''!
asSet | |
	^Set new of: self to: self length
! !
!Array methodsFor: 'undefined' stamp: ''!
sum | |
	^self sumTo: 0
! !
!Array methodsFor: 'undefined' stamp: ''!
viewer | |
	^SetReader new of: self
! !
!Array methodsFor: 'undefined' stamp: ''!
asStream | |
	^Stream new of: self
! !
!Array methodsFor: 'undefined' stamp: ''!
do: aBlock | i len |
	len← self length.  i←0.
	[(i←i+1) ≤ len] whileTrueDo: [aBlock value: self◦i]
! !
!Array methodsFor: 'undefined' stamp: ''!
collect: aBlock | i len result |
	result← self species new: (len← self length).
	i←0.
	[(i←i+1) ≤ len]
		whileTrueDo: [result◦i← aBlock value: self◦i].
	^result
! !
!Array methodsFor: 'undefined' stamp: ''!
insertSorted: x | a c i |
	 "self is assumed to be sorted"
	i ← self findSorted: x.
	c ← (a ← self species new: self length + 1) asStream.
	self ◦ (1 to: i) copyto: c.
	c next← x.
	self ◦ (i + 1 to: self length) copyto: c.
	^a
! !
!Array methodsFor: 'undefined' stamp: ''!
copy: a to: b to: t | i s me |
	s ← t asStream.
	me ← Stream new of: self from: a to: b.
	(a to: b) do: [:i |  "general code wont stop at false"
		s next← me next].
	^t
! !
!Array methodsFor: 'undefined' stamp: ''!
concat: arg | x s |
	x ← self species new: self length + arg length.
	self copyto: (s ← x asStream).
	arg copyto: s.
	^x
! !
!Array methodsFor: 'undefined' stamp: ''!
without: index | s me i |
	 "if index in range, return self without ◦index"
	(index cansubscript: self)
	  ifTrue:
		[s ← (self species new: self length - 1) asStream.
		me ← self asStream.
		(1 to: self length) do:
			[:i | i = index
			  ifTrue: [me next]
			  ifFalse: [s next← me next]].
		^s asArray]
! !
!Array methodsFor: 'undefined' stamp: ''!
copy: a to: b | |
	^self copy: a to: b to: (self species new: b - a + 1)
! !
!Array methodsFor: 'undefined' stamp: ''!
replace: a to: b by: s | x xs |
	x ← self species new: self length + s length - (1 + b - a).
	xs ← x asStream.
	self copy: 1 to: a - 1 to: xs.
	s copy: 1 to: s length to: xs.
	self copy: b + 1 to: self length to: xs.
	^x
! !
!Array methodsFor: 'undefined' stamp: ''!
notNil | t i |
	 "copy self (which contains no falses) removing all nils"
	t ← (self species new: self length - (self count: nil)) asStream.
	self do:
		[:i | i ≡ nil
		  ifFalse: [t next← i]].
	^t asArray
! !
!Array methodsFor: 'undefined' stamp: ''!
copyto: t | |
	^self copy: 1 to: self length to: t
! !
!Array methodsFor: 'undefined' stamp: ''!
insertNonDescending: x | |
	 "self is assumed to be sorted"
	^self insertSorted: x
! !
!Array methodsFor: 'undefined' stamp: ''!
grow | |
	^self growto: (4 max: self length * 2)
! !
!Array methodsFor: 'undefined' stamp: ''!
delete: obj | s each |
	s ← (self species new: self length) asStream.
	self do:
		[:each | obj = each
		  ifFalse: [s next← each]].
	^s contents
! !
!Array methodsFor: 'undefined' stamp: ''!
growto: n | |
	^self copy: 1 to: self length "copyto:" to: (self species new: n)
! !
!Array methodsFor: 'undefined' stamp: ''!
copy | |
	^self copy: 1 to: self length
! !
!Array methodsFor: 'undefined' stamp: ''!
growby: n | |
	^self growto: self length + n
! !
!Array methodsFor: 'undefined' stamp: ''!
+ arg | |
	^self concat: arg
! !
!Array methodsFor: 'undefined' stamp: ''!
subscripts: x ← val | |
	 "subrange replacement"
	self length ≠ val length
	  ifTrue: [user notify: 'lengths not commensurate']
	  ifFalse:
		[val copyto: (Substring new data: x map: self).
		^val]
! !
!Array methodsFor: 'undefined' stamp: ''!
subscripts: x | |
	 "subarrays"
	^Substring new data: x map: self
! !
!Array methodsFor: 'undefined' stamp: ''!
cansubscript: a | i |
	self do:
		[:i | (i cansubscript: a)
		  ifFalse: [^false]]
! !
!Array methodsFor: 'undefined' stamp: ''!
swap: i with: j | t |
	t ← self ◦ i.
	self ◦ i ← self ◦ j.
	self ◦ j ← t
! !
!Array methodsFor: 'undefined' stamp: ''!
promote: t | n |
	n ← self find: t.
	n = 0
	  ifFalse:
		[self ◦ (n to: 2 by: ¬1) ← self ◦ (n - 1 to: 1 by: ¬1).
		self ◦ 1 ← t]
! !
!Array methodsFor: 'undefined' stamp: ''!
sort: i to: j | di dij dj tt ij k l n |
	(n ← j + 1 "Sort elements i through j of self to be nondescending." "The prefix d means the data at." - i) ≤ 1
	  ifFalse:
		[ "Nothing to sort." "Sort di,dj."
		di ← self ◦ i.
		dj ← self ◦ j.
		di > dj
		  ifTrue:
			[self swap: i with: j.
			tt ← di.
			di ← dj.
			dj ← tt].
		n = 2
		  ifFalse:
			[ "They are the only two elements."
			ij ← i + j lshift: ¬1 "ij is the midpoint of i and j." "Sort di,dij,dj.  Make dij be their median.".
			dij ← self ◦ ij.
			di > dij
			  ifTrue:
				[self swap: i with: ij.
				dij ← di]
			  ifFalse:
				[dj < dij
				  ifTrue:
					[self swap: j with: ij.
					dij ← dj]].
			n = 3
			  ifFalse:
				[ "They are the only three elements." "Find k>i and l<j such that dk,dij,dl are in reverse order.  Swap k and l.  Repeat this procedure until j and k pass each other."
				k ← i.
				l ← j.
				[[self ◦ (l ← l - 1) > dij] whileTrueDo: [].
				[self ◦ (k ← k + 1) < dij] whileTrueDo: [].
				k ≤ l] whileTrueDo: [self swap: k with: l "Now l<k (either 1 or 2 less), and di through dl are all less than dk through dj.  Sort those two segments."].
				self sort: i to: l.
				self sort: k to: j]]]
! !
!Array methodsFor: 'undefined' stamp: ''!
permutationToSort | |
	^(self ◦ (1 to: self length "Return a Vector, permutation, such that self◦permutation is sorted nondescending.  Do not alter self.") copy sort: 1 to: self length) map
! !
!Array methodsFor: 'undefined' stamp: ''!
reverse | |
	^Substring new data: self map: (self length to: 1 by: ¬1)
! !
!Array methodsFor: 'undefined' stamp: ''!
sort | |
	self sort: 1 to: self length "Permute my elements so they are sorted nondescending.  Note: if I am a substring, only my map will be permuted.  In certain situations, this may not be what you expect."
! !
!Array methodsFor: 'undefined' stamp: ''!
first | |
	^self ◦ 1
! !
!Array methodsFor: 'undefined' stamp: ''!
all← val | i |
	(1 to: self length) do: [:i | self ◦ i ← val]
! !
!Array methodsFor: 'undefined' stamp: ''!
last | |
	^self ◦ self length
! !
!Array methodsFor: 'undefined' stamp: ''!
last← val | |
	^self ◦ self length ← val
! !
!Array methodsFor: 'undefined' stamp: ''!
= arg | x |
	arg isArray
	  ifTrue:
		[self length ≠ arg length
		  ifTrue: [^false].
		(1 to: self length) do:
			[:x | self ◦ x = (arg ◦ x)
			  ifFalse: [^false]].
		^true]
	  ifFalse: [^false]
! !
!Array methodsFor: 'undefined' stamp: ''!
< v | |
	 "for sorting vectors by first element"
	^self ◦ 1 < (v ◦ 1)
! !
!Array methodsFor: 'undefined' stamp: ''!
> v | |
	 "for sorting vectors by first element"
	^self ◦ 1 > (v ◦ 1)
! !
!Array methodsFor: 'undefined' stamp: ''!
first⦂ x suchThat⦂ predicate | i |
	(1 to: self length) do:
		[:i | x value← self ◦ i.
		predicate eval
		  ifTrue: [^self ◦ i]].
	^false
! !
!Array methodsFor: 'undefined' stamp: ''!
all⦂ variable suchThat⦂ expr | s i x |
	 "a copy of some of me"
	s ← (self species new: self length) asStream.
	(1 to: self length) do:
		[:i | x ← self ◦ i.
		variable value← x.
		expr eval
		  ifTrue: [s next← x]].
	^s contents
! !
!Array methodsFor: 'undefined' stamp: ''!
find⦂ x suchThat⦂ predicate | i |
	(1 to: self length) do:
		[:i | x value← self ◦ i.
		predicate eval
		  ifTrue: [^i]].
	^0
! !
!Array methodsFor: 'undefined' stamp: ''!
findSorted: x | lo mid hi |
	 " returns index of largest element ≤ x "
	hi ← self length + 1.
	lo ← 1.
	[lo < hi] whileTrueDo:
		[ "binary search; self must be sorted"
		self ◦ (mid ← lo + hi / 2) > x
		  ifTrue: [hi ← mid]
		  ifFalse: [lo ← mid + 1]].
	^hi - 1 " 0≤result≤length "
! !
!Array methodsFor: 'undefined' stamp: ''!
findnon: x | i |
	(1 to: self length) do:
		[:i | self ◦ i ≠ x
		  ifTrue: [^i]].
	^0
! !
!Array methodsFor: 'undefined' stamp: ''!
count: x | i n |
	n ← 0.
	(1 to: self length) do:
		[:i | x = (self ◦ i)
		  ifTrue: [n ← n + 1]].
	^n
! !
!Array methodsFor: 'undefined' stamp: ''!
find: x | i |
	(1 to: self length) do:
		[:i | self ◦ i = x
		  ifTrue: [^i]].
	^0
! !
!Array methodsFor: 'undefined' stamp: ''!
has: x | |
	^(self find: x) ≠ 0
! !
!Association methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm print: key.
	strm append: '->'.
	strm print: value
! !
!Association methodsFor: 'undefined' stamp: ''!
value | |
	^value
! !
!Association methodsFor: 'undefined' stamp: ''!
key | |
	^key
! !
!Association methodsFor: 'undefined' stamp: ''!
value← val | |
	^value ← val
! !
!Association methodsFor: 'undefined' stamp: ''!
key: t1 value: t2 | |
	key ← t1.
	value ← t2
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
destRaster: t1 | |
	 "length of a 'scanline' destination in 16-bit words"
	destRaster ← t1
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
dest: t1 | |
	dest ← t1
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
window | |
	^clipX ⌾ clipY rect: clipX + clipWidth ⌾ (clipY + clipHeight)
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
source: t1 | |
	source ← t1
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
screen: t1 | |
	screen ← t1.
	(screen is: Form)
	  ifTrue:
		[screenForm ← screen.
		screen ← screenForm bits]
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
height: t1 | |
	height ← t1
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
effect: anInteger | |
	NoteTaker
	  ifTrue: [effect ← #(35 39 38 36 44 45 41 33 51 55 54 52 19 23 22 20 ) ◦ ((anInteger land: 15) + 1)]
	  ifFalse: [effect ← anInteger]
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
sourceRaster: t1 | |
	 "length of a 'scanline' source in 16-bit words"
	sourceRaster ← t1
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
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
!BitBlt methodsFor: 'undefined' stamp: ''!
copyRect: rect toPoint: point effect: eff screen: halftone | |
	self screen: halftone "careful -- assumes dest, destRaster, source and sourceRaster are set!!".
	self effect: eff.
	self destOrigin: point.
	self sourceRect: rect.
	self callBLT
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
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
!BitBlt methodsFor: 'undefined' stamp: ''!
callBLT | |<primitive: 66>
	(screen is: Form)
	  ifTrue:
		[screen ← screen halftoneInteger.
		self callBLT]
	  ifFalse: [user croak]
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
installDisplay | |<primitive: 67>
	user croak "set up source=cursor, dest=display"
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
sourceForm: t1 | |
	sourceForm ← t1.
	sourceRaster ← sourceForm width + 15 / 16.
	source ← sourceForm bits
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
toDisplay | |
	self destForm: DisplayForm.
	source ≡ nil ifTrue: [self sourceForm: DisplayForm]
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
extent: extent | |
	width ← extent x.
	height ← extent y
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
classInit | |
	pageOneCursor ← 281 "location of hardware cursor"
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
fromDisplay | |
	self sourceForm: DisplayForm
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
init | |
	self effect: 0.
	self screen: black.
	destX ← destY ← width ← height ← sourceX ← sourceY ← 0
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
forCursor | |
	self effect: 0.
	self screen: black.
	dest ← source ← 281.
	width ← height ← 16.
	destRaster ← sourceRaster ← 1.
	destX ← destY ← sourceX ← sourceY ← 0
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
window: rect | |
	 "this is public"
	NoteTaker
	  ifTrue: [self clipRect: (rect intersect: (0 ⌾ 0 rect: destForm extent))]
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
sourceRect: rect | |
	sourceX ← rect minX.
	sourceY ← rect minY.
	width ← rect width.
	height ← rect height
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
destOrigin: destOrigin | |
	destX ← destOrigin x.
	destY ← destOrigin y
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
destForm: t1 | |
	destForm ← t1.
	self clipRect: (0 ⌾ 0 rect: destForm extent).
	destRaster ← destForm width + 15 / 16.
	dest ← (NoteTaker
			  ifTrue: [destForm bits]
			  ifFalse: [destForm bits lock])
! !
!BitBlt methodsFor: 'undefined' stamp: ''!
clipRect: rect | |
	clipX ← rect minX.
	clipY ← rect minY.
	clipWidth ← rect width.
	clipHeight ← rect height
! !
!BitImage methodsFor: 'undefined' stamp: ''!
displayat: t1 effect: effect clippedBy: cliprect | |
	path ← t1.
	super displayat: path effect: effect clippedBy: cliprect
! !
!BitImage methodsFor: 'undefined' stamp: ''!
show | |
	super displayat: 0 ⌾ 0 effect: 0 clippedBy: user screenrect
! !
!BitImage methodsFor: 'undefined' stamp: ''!
verticalsymmetry | r f i |
	user clear "vertical symmetry tool".
	user show: 'Define rectangle. Reflection will be around right-hand edge'.
	r ← Rectangle new fromuser.
	XeqCursor show.
	(1 to: r width) do:
		[:i | f ← Form new fromrectangle: (Rectangle new origin: r origin x + r width - i ⌾ r top extent: 1 ⌾ r height).
		f displayat: r corner x + i ⌾ r top effect: 0 clippedBy: user screenrect].
	NormalCursor show
! !
!BitImage methodsFor: 'undefined' stamp: ''!
blinkbrush | pt |
	pt ← self mp "to show current position of brush in the BitImage." + rectangle origin.
	brush displayat: pt effect: 2 clippedBy: user screenrect.
	brush displayat: pt effect: 2 clippedBy: user screenrect.
	^pt
! !
!BitImage methodsFor: 'undefined' stamp: ''!
horizontalsymmetry | r f i |
	user clear "horizontal symmetry tool".
	user show: 'Define rectangle. Reflection will be around lower edge'.
	r ← Rectangle new fromuser.
	XeqCursor show.
	(1 to: r height) do:
		[:i | f ← Form new fromrectangle: (Rectangle new origin: r origin x ⌾ (r bottom - i) extent: r width ⌾ 1).
		f displayat: r origin x ⌾ (r bottom + i) effect: 0 clippedBy: user screenrect].
	NormalCursor show
! !
!BitImage methodsFor: 'undefined' stamp: ''!
pastebrush | pt1 |
	user waitnobug "one-copy tool for forms.".
	OriginCursor show.
	user waitbug.
	[user redbug] whileTrueDo: [pt1 ← self blinkbrush].
	XeqCursor show.
	brush displayat: pt1 effect: dotsetter tool mode clippedBy: user screenrect.
	NormalCursor show
! !
!BitImage methodsFor: 'undefined' stamp: ''!
newbrush | pt rect |
	OriginCursor topage1.
	user waitbug.
	pt ← self mp + rectangle origin.
	rect ← pt rect: pt.
	CornerCursor topage1.
	[user nobug] whileFalseDo: 
		[rect reverse.
		rect reverse.
		pt ← self mp + rectangle origin.
		rect corner← rect origin max: pt].
	brush ← Form new fromrectangle: rect.
	NormalCursor topage1
! !
!BitImage methodsFor: 'undefined' stamp: ''!
setground | t |
	ground ← ground + 1 "for now just increment the ground color by 1 \ 12" \ 12.
	self do: [:t | t form ground: ground].
	self display
! !
!BitImage methodsFor: 'undefined' stamp: ''!
line | pt1 pt2 p pt |
	BlankCursor topage1 "line tool for forms.".
	[user redbug] whileFalseDo:  [pt1 ← self blinkbrush].
	brush displayat: pt1 effect: color clippedBy: user screenrect.
	[user nobug] whileFalseDo:  [pt2 ← self blinkbrush].
	brush displayat: pt2 effect: color clippedBy: user screenrect.
	p ← Path new init.
	p addlinefrom: pt1 to: pt2.
	p do: [:pt | brush displayat: pt effect: color clippedBy: user screenrect].
	NormalCursor topage1
! !
!BitImage methodsFor: 'undefined' stamp: ''!
setfigure | t |
	figure ← figure + 1 "for now just increment the figure color by 1 \ 12" \ 12.
	self do: [:t | t form figure: figure].
	self display
! !
!BitImage methodsFor: 'undefined' stamp: ''!
edit: t1 | bits |
	superimage ← t1.
	XeqCursor show "uses the BitRect toolbox editor".
	dotsetter ← BitRectEditor new picture: self.
	dotsetter firsttime.
	NormalCursor show.
	[true] whileTrueDo:
		[ "forever"
		(dotsetter lostMouse and: [user anybug])
		  ifTrue:
			[dotsetter outside
			  ifFalse:
				[XeqCursor show.
				dotsetter lasttime.
				bits ← self fromrectangle: rectangle.
				NormalCursor show.
				^bits]]
		  ifFalse: [dotsetter eachtime]]
! !
!BitImage methodsFor: 'undefined' stamp: ''!
rotate | r f i j |
	r ← Rectangle new "90 degree rotation tool" fromuser.
	(1 to: r width) do: [:i | (1 to: r height) do:
			[:j | f ← Form new fromrectangle: (Rectangle new origin: r origin x + i ⌾ (r top + j) extent: 1 ⌾ 1).
			f displayat: r corner x + j ⌾ (r top + i) effect: 0 clippedBy: user screenrect]]
! !
!BitImage methodsFor: 'undefined' stamp: ''!
yellowbug | t1 |
	(t1 ← bitimagemenu bug) = 1
	  ifTrue: [self resize: superimage]
	  ifFalse:
		[ "change size"
		t1 = 2
		  ifTrue: [self setfigure]
		  ifFalse:
			[t1 = 3
			  ifTrue: [self setground]
			  ifFalse:
				[t1 = 4
				  ifTrue: [self newbrush]
				  ifFalse:
					[t1 = 5
					  ifTrue: [self pastebrush]
					  ifFalse:
						[t1 = 6
						  ifTrue: [self arc]
						  ifFalse:
							[t1 = 7
							  ifTrue: [Rectangle new fromuser fillin: dotsetter tool tone mode: dotsetter tool mode]
							  ifFalse:
								[t1 = 8
								  ifTrue: [dotsetter tool shade]
								  ifFalse:
									[t1 = 9
									  ifTrue: [self verticalsymmetry]
									  ifFalse:
										[t1 = 10
										  ifTrue: [self horizontalsymmetry]
										  ifFalse:
											[t1 = 11
											  ifTrue: [self rotate]]]]]]]]]]]
! !
!BitImage methodsFor: 'undefined' stamp: ''!
resize: t1 | pt f |
	superimage ← t1.
	dotsetter leave.
	CornerCursor topage1.
	user waitbug.
	[user nobug] whileFalseDo: 
		[self reverse.
		self reverse.
		pt ← superimage mp + superimage rectangle origin.
		self corner← pt max: self origin + (16 ⌾ 16)].
	self fromrectangle: rectangle.
	self white.
	self display.
	NormalCursor topage1.
	self edit: superimage
! !
!BitImage methodsFor: 'undefined' stamp: ''!
arc | pt1 pt2 pt3 p pt |
	BlankCursor topage1 "arc tool for forms.".
	user clear.
	user print: 'Redbug 3 points'.
	user cr.
	user print: 'Paints using current brush.'.
	user waitnobug.
	[user redbug] whileFalseDo:  [pt1 ← self blinkbrush].
	brush displayat: pt1 effect: color clippedBy: user screenrect.
	user waitnobug.
	[user redbug] whileFalseDo:  [pt2 ← self blinkbrush].
	brush displayat: pt2 effect: color clippedBy: user screenrect.
	user waitnobug.
	[user redbug] whileFalseDo:  [pt3 ← self blinkbrush].
	brush displayat: pt3 effect: color clippedBy: user screenrect.
	XeqCursor show.
	p ← Path new init.
	p addarcfrom: pt1 via: pt2 to: pt3.
	p do: [:pt | brush displayat: pt effect: color clippedBy: user screenrect].
	NormalCursor show
! !
!BitImage methodsFor: 'undefined' stamp: ''!
grayEdit | a b c i d p r v bits |
	 "edit up a gray pattern and return it"
	r ← Rectangle new "first a rectangle for it.  Then redbug is black, yellow is white,
	blue terminates" fromuser.
	bits ← 0.
	a ← r extent.
	a ← (a x max: a y) | 4.
	a ← a ⌾ a.
	b ← r origin.
	r extent← a.
	r color: white mode: storing.
	r moveby: 0 ⌾ (0 - a y).
	c ← a / 4.
	d ← b rect: b + c.
	[user bluebug] whileFalseDo: 
		[user redbug
		  ifTrue:
			[p ← user mp - b / c.
			i ← p y * 4 + p x + 1.
			(i < 1 or: [i > 16])
			  ifTrue: [r flash]
			  ifFalse:
				[d moveto: b + (c * p).
				d color: black mode: storing.
				bits ← bits lor: (1 lshift: 16 - i).
				r color: bits mode: storing.
				user waitnobug]]
		  ifFalse:
			[user yellowbug
			  ifTrue:
				[p ← user mp - b / c.
				i ← p y * 4 + p x + 1.
				(i < 1 or: [i > 16])
				  ifTrue: [r flash]
				  ifFalse:
					[d moveto: b + (c * p).
					d color: white mode: storing.
					bits ← (¬1 lxor: (1 lshift: 16 - i)) land: bits.
					r color: bits mode: storing.
					user waitnobug]]]].
	^bits "aa grayEdit base8 ."
! !
!BitImage methodsFor: 'undefined' stamp: ''!
read: filename | file subimage strip yposition i |
	self origin: 0 ⌾ 0 "Reads the Image in the format nstrips , Form(1) , Form(2) , Form(3) . . .
		Form(nstips). Where each Form is saved as width,height then bits.  " extent: 1 ⌾ 1.
	yposition ← 0.
	file ← (dp0 oldFile: filename) readonly.
	nstrips ← file nextword.
	(1 to: nstrips) do:
		[:i | strip ← Form new fromInstance: file.
		self addform: strip andpath: 0 ⌾ yposition.
		yposition ← yposition + strip height].
	file close
! !
!BitImage methodsFor: 'undefined' stamp: ''!
write: filename | file subimage |
	file ← dp0 file: filename "Saves the Form in the format nstrips , Form(1) , Form(2) , Form(3) . . .
		Form(nstips). Where each Form is saved as width,height then bits.  ".
	file nextword← nstrips.
	self do:
		[:subimage | file append: subimage form asInstance].
	file close
! !
!BitImage methodsFor: 'undefined' stamp: ''!
fromuserevenword | r |
	r ← Rectangle new "create a new BitImage whose rectangle is specified by the user,
		truncated to nearest multiple of 16 (for Spruce printing). " fromuserevenword.
	self fromrectangle: r
! !
!BitImage methodsFor: 'undefined' stamp: ''!
fromuser | r |
	r ← Rectangle new "create a new Form whose rectangle is specified by the user. " fromuser.
	self fromrectangle: r
! !
!BitImage methodsFor: 'undefined' stamp: ''!
classInit | |
	black ← 0 - 1 "sets up colors and effects for BITBLT.".
	white ← 0.
	over ← 0.
	under ← 1.
	reverse ← 2.
	erase ← 3.
	brush ← Form new extent: 5 ⌾ 5.
	brush black.
	color ← 1.
	stripheight ← 20.
	bitimagemenu ← Menu new string: 'size
figure
ground
newform
pasteform
arc
areafill
shade
vertical
horizontal
rotate
'.
	aurora ← nil "Aurora new"
! !
!BitImage methodsFor: 'undefined' stamp: ''!
fromrectangle: rect | r i leftover image yposition |
	super origin: rect origin "creates a virtual bit map with width = (r width) , height = (r height)
	 and the bits in rect. The Image is  made up of forms that are stripheight high." extent: rect extent.
	nstrips ← rect height + (stripheight - 1) / stripheight.
	yposition ← 0.
	leftover ← rect height \ stripheight.
	leftover = 0
	  ifTrue: [leftover ← stripheight].
	r ← Rectangle new origin: rect origin extent: rect width ⌾ stripheight.
	(1 to: nstrips) do:
		[:i | i = nstrips
		  ifTrue: [r extent← rect width ⌾ leftover].
		image ← Image new origin: 0 ⌾ 0 extent: rect extent.
		image form: (Form new fromrectangle: r).
		image path: 0 ⌾ yposition.
		self addimage: image.
		yposition ← yposition + stripheight.
		r translate: 0 ⌾ stripheight]
! !
!BitImage methodsFor: 'undefined' stamp: ''!
fromImage: image | |
	self fromrectangle: image rectangle "creates a virtual bit map with width = (image width) and height = (image height) with the bits in image."
! !
!BitImage methodsFor: 'undefined' stamp: ''!
comment | |
	 "see class Image"
! !
!BitImage methodsFor: 'undefined' stamp: ''!
frame | |
	^rectangle
! !
!BitImage methodsFor: 'undefined' stamp: ''!
moveto: pt | |
	self translateto: pt
! !
!BitImage methodsFor: 'undefined' stamp: ''!
strips | |
	^strips "return the set of Forms making up this BitImage)"
! !
!BitImage methodsFor: 'undefined' stamp: ''!
saveScreenBits | |
	
! !
!BitImage methodsFor: 'undefined' stamp: ''!
nstrips | |
	^nstrips
! !
!BitImage methodsFor: 'undefined' stamp: ''!
erase | i |
	(1 to: nstrips) do: [:i |  "sets all bits in the BitImage to white ( to zeros)"
		(strips ◦ i) white]
! !
!BitImage methodsFor: 'undefined' stamp: ''!
nstrips: t1 | |
	nstrips ← t1
! !
!BitImage methodsFor: 'undefined' stamp: ''!
strips: t1 | |
	strips ← t1
! !
!BitImage methodsFor: 'undefined' stamp: ''!
copy | t i |
	t ← BitImage new "return a copy of myself" origin: origin copy extent: self extent copy.
	(1 to: self length) do: [:i | t add: (self ◦ i) copy].
	^t
! !
!BitImage methodsFor: 'undefined' stamp: ''!
title | |
	^''
! !
!BitImage methodsFor: 'undefined' stamp: ''!
pressCode | |
	^3
! !
!BitRect methodsFor: 'undefined' stamp: ''!
title | |
	^title
! !
!BitRect methodsFor: 'undefined' stamp: ''!
data | |
	^data
! !
!BitRect methodsFor: 'undefined' stamp: ''!
copyBitsFrom: other | clippedStrip i j myStrips otherStrips myStrip otherStrip bb |
	myStrips ← self strips "copy all bits from other that are within my area".
	otherStrips ← other strips.
	(1 to: myStrips length) do: [:i | (1 to: otherStrips length) do:
			[:j | myStrip ← myStrips ◦ i.
			otherStrip ← otherStrips ◦ j.
			clippedStrip ← myStrip intersect: otherStrip.
			clippedStrip empty
			  ifFalse:
				[bb ← BitBlt init.
				bb function← 0.
				bb destbase← data ◦ i.
				bb destraster← myStrip width + 15 / 16.
				bb dest← clippedStrip origin - myStrip origin.
				bb extent← clippedStrip extent.
				bb sourcebase← other data ◦ j.
				bb sourceraster← otherStrip width + 15 / 16.
				bb source← clippedStrip origin - otherStrip origin.
				bb bb callBLT]]]
! !
!BitRect methodsFor: 'undefined' stamp: ''!
edit | a |
	user leaveTop.
	a ← BitRectEditor new picture: self.
	a takeCursor.
	a enter.
	user restartup: a
! !
!BitRect methodsFor: 'undefined' stamp: ''!
filin: t1 | f i x y rect strips |
	 "read bits from a file"
	title ← t1.
	f ← dp0 oldFile: (title concat: '.pic.').
	f readonly.
	f end
	  ifTrue:
		[f close.
		user notify: 'no data']
	  ifFalse:
		[x ← f nextword.
		y ← f nextword.
		rect ← Rectangle new origin: ((origin is: Point)
				  ifTrue: [origin]
				  ifFalse: [0 ⌾ 0]) extent: x ⌾ y.
		self title: title in: rect.
		stripheight ≠ f nextword
		  ifTrue: [user notify: 'strip heights dont match']
		  ifFalse:
			[strips ← self strips.
			(1 to: strips length) do: [:i | f into: data ◦ i].
			f close]]
! !
!BitRect methodsFor: 'undefined' stamp: ''!
filout | f i |
	 "write bits on a file"
	f ← dp0 file: (title concat: '.pic.').
	f nextword← self extent x.
	f nextword← self extent y.
	f nextword← stripheight.
	data do: [:i | i toStream: f].
	f close
! !
!BitRect methodsFor: 'undefined' stamp: ''!
fromuser | |
	self title: 'BitRect' in: Rectangle new fromuser.
	self saveScreenBits
! !
!BitRect methodsFor: 'undefined' stamp: ''!
classInit | |
	defaultpic ← BitRect new "the default picture is a gray rectangle" filin: 'defaultpic'
! !
!BitRect methodsFor: 'undefined' stamp: ''!
title: t1 in: rect | nStrips i strips |
	title ← t1.
	origin ← rect origin.
	corner ← rect corner "the strip height is chosen so that each bitstring is about 2048 bytes".
	stripheight ← 1023 / (self extent x + 15 / 16).
	nStrips ← self extent y + stripheight - 1 / stripheight.
	data ← Vector new: nStrips.
	strips ← self strips.
	(1 to: nStrips) do: [:i | data ◦ i ← (strips ◦ i) bitmap]
! !
!BitRect methodsFor: 'undefined' stamp: ''!
default | |
	^defaultpic recopy
! !
!BitRect methodsFor: 'undefined' stamp: ''!
origin: t1 corner: t2 title: t3 stripheight: t4 data: t5 | |
	origin ← t1.
	corner ← t2.
	title ← t3.
	stripheight ← t4.
	data ← t5
! !
!BitRect methodsFor: 'undefined' stamp: ''!
presson: press in: r | w h hs scale w16 y |
	scale ← press scale.
	h ← self height.
	(hs ← scale * h) > r height
	  ifTrue: [^self].
	 "not enough room left on current page.
		assume for now that it will at least fit on an entire page"
	w ← self width.
	w16 ← w + 15 | 16 "width to next word boundary" "with w, prints on viola but not on spruce.
	with w16, prints on spruce with garbage on end".
	press setp: 0 ⌾ (y ← r corner y - hs).
	press dots⦂ [
		(press setcoding: 1 dots: w16 lines: h "bitmap".
		press setmode: 3 "to right and to bottom".
		press setsizewidth: scale * w16 height: hs.
		press setwindowwidth: w16 height: h.
		press dotsfollow.
		self bitsOntoStream: press data)].
	^y
! !
!BitRect methodsFor: 'undefined' stamp: ''!
length | |
	^self bitmapLength * 2
! !
!BitRect methodsFor: 'undefined' stamp: ''!
width← w | |
	self growby: w - self extent x ⌾ 0
! !
!BitRect methodsFor: 'undefined' stamp: ''!
bitsOntoStream: strm | bits |
	data do: [:bits | bits toStream: strm]
! !
!BitRect methodsFor: 'undefined' stamp: ''!
growto: x | |
	self growby: x - corner
! !
!BitRect methodsFor: 'undefined' stamp: ''!
hash | |<primitive: 39>
	user croak
! !
!BitRect methodsFor: 'undefined' stamp: ''!
growby: change | old |
	old ← BitRect new origin: origin corner: corner title: title stripheight: stripheight data: data.
	self title: title in: (origin rect: corner + change).
	self copyBitsFrom: old
! !
!BitRect methodsFor: 'undefined' stamp: ''!
= x | |
	^self ≡ x
! !
!BitRect methodsFor: 'undefined' stamp: ''!
extent← x | |
	self growby: x - self extent
! !
!BitRect methodsFor: 'undefined' stamp: ''!
corner← x | |
	self growby: x - corner
! !
!BitRect methodsFor: 'undefined' stamp: ''!
height← h | |
	self growby: 0 ⌾ (h - self extent y)
! !
!BitRect methodsFor: 'undefined' stamp: ''!
strips | nStrips strips stripOrigin stripExtent i |
	 "return a vector of strips (Rectangles)"
	(nStrips ← data length) = 1
	  ifTrue: [^self inVector].
	strips ← Vector new: nStrips.
	stripOrigin ← origin.
	stripExtent ← self width ⌾ stripheight.
	(1 to: nStrips - 1) do:
		[:i | strips ◦ i ← Rectangle new origin: stripOrigin extent: stripExtent.
		stripOrigin ← stripOrigin + (0 ⌾ stripheight)].
	strips ◦ nStrips ← Rectangle new origin: stripOrigin corner: corner.
	^strips
! !
!BitRect methodsFor: 'undefined' stamp: ''!
saveScreenBits | strips i |
	strips ← self strips.
	(1 to: strips length) do: [:i | strips ◦ i bitsIntoString: data ◦ i mode: storing clippedBy: nil]
! !
!BitRect methodsFor: 'undefined' stamp: ''!
show | strips i |
	strips ← self strips.
	(1 to: strips length) do: [:i | strips ◦ i bitsFromString: data ◦ i]
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
classInit | t i |
	t ← Vector new: 6.
	(1 to: t length) do: [:i | t ◦ i ← BitRectTool new init].
	tools ← RadioButtons new vec: t at: 0 ⌾ 0 width: 20.
	windowmenu ← Menu new string: 'under
move
grow
close
filout
printbits' "actionpic←BitRect new filin: 'actionpic'.
	toolpic←BitRect new filin: 'toolpic'.".
	self initmenu1
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
actionpic: a | |
	actionpic ← a
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
initmenu1 | s z |
	s ← Vector new: 5.
	z ← 20.
	s ◦ 1 ← RadioButtons new vec: #(setbrush paint block draw line blowup ) at: 0 ⌾ 0 height: z "action".
	s ◦ 2 ← RadioButtons new vec: {black , dkgray , gray , ltgray , white} at: 0 ⌾ 0 height: z "tone".
	s ◦ 3 ← RadioButtons new vec: {0 , 1 , 2 , 3} at: 0 ⌾ 0 height: z "mode".
	s ◦ 4 ← RadioButtons new vec: {1 , 2 , 4 , 8} at: 0 ⌾ 0 height: z "width".
	s ◦ 5 ← RadioButtons new vec: {1 , 2 , 4 , 8 , 16 , 32} at: 0 ⌾ 0 height: z "grid".
	actionbuttons ← s
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
toolpic: a | |
	toolpic ← a
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
picture: t1 | |
	picture ← t1.
	tool ← tools push: 1.
	self frame: (picture origin rect: picture corner)
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
showtitle | |
	 "The BitRectEditor have a menu where the title used to be"
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
title | |
	^picture title
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
grow | oldframe newframe pt r |
	self leave.
	newframe ← picture origin rect: picture corner.
	CornerCursor showwhile⦂ [
		(pt ← user mp + 16.
		[user nobug] whileTrueDo:
			[newframe corner← pt.
			newframe comp.
			pt ← user mp + 16.
			newframe comp].
		[user anybug] whileTrueDo:
			[newframe corner← pt.
			newframe comp.
			pt ← user mp + 16.
			newframe comp])] "clear unused areas from old picture to background,
		and clear new picture areas to white".
	oldframe ← picture inset: ¬2 ⌾ ¬2 "¬2 is for erasing old border".
	(oldframe minus: newframe) do: [:r | r clear: background].
	(newframe minus: picture) do: [:r | r clear: white].
	picture title: picture title in: newframe.
	picture saveScreenBits.
	self frame: newframe.
	self show.
	self takeCursor.
	self enter
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
bluebug | t1 |
	(picture is: BitImage)
	  ifTrue: [^picture fromrectangle: picture rectangle].
	(t1 ← windowmenu bug) = 1
	  ifTrue:
		[self leave.
		^exitflag ← false].
	 "under"
	t1 = 2
	  ifTrue:
		[self leave.
		self newframe.
		self enter]
	  ifFalse:
		[ "move"
		t1 = 3
		  ifTrue: [self grow "grow"]
		  ifFalse:
			[t1 = 4
			  ifTrue:
				[self leave.
				self erase "close".
				user unschedule: self.
				^false].
			t1 = 5
			  ifTrue:
				[self leave.
				picture filout.
				self enter]
			  ifFalse:
				[ "filout"
				t1 = 6
				  ifTrue: [self print] "press file"]]]
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
yellowbug | |
	(picture is: BitImage)
	  ifTrue: [picture yellowbug]
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
outside | pt |
	(toolpic has: (pt ← user mp))
	  ifTrue:
		[user redbug
		  ifTrue:
			[tool ← tools bug: pt.
			tool frame: frame.
			tool showon: actionbuttons]]
	  ifFalse:
		[(actionpic has: pt)
		  ifTrue:
			[user redbug
			  ifTrue: [tool setfrom: actionbuttons]]
		  ifFalse: [^false]]
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
lostMouse | |
	^(frame has: user mp) ≡ false
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
enter | start pt b t4 |
	super show "Periodically check if the mouse is still in the frame.
		If not, stop showing the picture".
	self lostMouse
	  ifTrue: [^false].
	picture show.
	dirty ← false.
	self lostMouse
	  ifTrue: [^false].
	actionbuttons do: [:b | b reset "show action menu above the picture"].
	start ← frame origin - 1.
	pt ← start - (0 ⌾ actionpic extent y).
	actionpic moveto: pt.
	saveActionPic ← actionpic bitsIntoString.
	self lostMouse
	  ifTrue: [^false].
	 "last point I can return before having to restore bits under menus"
	actionpic show.
	pt ← actionbuttons ◦ 1 moveto: pt "action".
	pt ← actionbuttons ◦ 3 moveto: pt "mode".
	pt ← actionbuttons ◦ 4 moveto: pt "width" "show the next bank of action buttons".
	pt ← start - (0 ⌾ (actionpic extent y + 1 / 2)).
	pt ← actionbuttons ◦ 2 moveto: pt "tone".
	pt ← actionbuttons ◦ 5 moveto: pt "grid".
	tool brushpt: (pt ← pt + (7 ⌾ 7)).
	(t4 ← tool brush) moveto: pt.
	t4 show "show the tool pic".
	pt ← start - (toolpic extent x ⌾ 0).
	toolpic moveto: pt.
	saveToolPic ← toolpic bitsIntoString.
	toolpic show.
	tools moveto: pt.
	tools setvalue: tool.
	tool frame: frame.
	tool showon: actionbuttons
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
fixframe: r | |
	picture moveto: r origin.
	r corner← picture corner.
	^r
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
tool | |
	^tool
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
leave | |
	nil ≡ saveActionPic
	  ifFalse:
		[actionpic bitsFromString: saveActionPic.
		saveActionPic ← nil].
	nil ≡ saveToolPic
	  ifFalse:
		[toolpic bitsFromString: saveToolPic.
		saveToolPic ← nil].
	dirty
	  ifTrue:
		[picture saveScreenBits.
		dirty ← false].
	frame border: 3 color: white
! !
!BitRectEditor methodsFor: 'undefined' stamp: ''!
redbug | |
	dirty ← true.
	tool redbug
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
init | |
	(pencil ← Turtle new) init.
	pencil color: black.
	pencil width: 2.
	(brush ← BitRect new) title: 'brush' in: (0 ⌾ 0 rect: 16 ⌾ 16).
	tone ← black.
	mode ← 0.
	grid ← 1.
	action ← #draw
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
classInit | rect saveBits t i |
	blowupScale ← 4 "make a vector of gray pens".
	rect ← 0 ⌾ 0 rect: 9 ⌾ 9.
	saveBits ← rect bitsIntoString.
	t ← Turtle init.
	graypens ← Vector new: 8.
	(1 to: 8) do:
		[:i | t width: i.
		rect clear: white.
		t place: 4 ⌾ 4.
		t go: 0.
		graypens ◦ i ← BitRect new title: 'graypen' in: rect.
		(graypens ◦ i) saveScreenBits].
	rect bitsFromString: saveBits
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
blowup | smallRect bigRectFrame |
	smallRect ← self getRectangle.
	bigRectFrame ← Rectangle new origin: smallRect corner extent: 4 ⌾ 4 + (smallRect extent * blowupScale).
	(smallRect empty or: [bigRectFrame bitStringLength > 4000])
	  ifTrue:
		[pencil frame flash.
		^nil].
	.
	(user screenrect has: bigRectFrame corner)
	  ifFalse:
		[bigRectFrame moveto: smallRect origin - bigRectFrame extent.
		(user screenrect has: bigRectFrame origin)
		  ifFalse:
			[ "can't find a space for blown up image"
			pencil frame flash.
			^nil]].
	self blowup: smallRect to: bigRectFrame
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
line | start end width |
	start ← end ← self mpOnGrid - pencil frame origin.
	width ← pencil width.
	pencil xor.
	pencil width: 1.
	[user redbug] whileTrueDo:
		[end ← self mpOnGrid - pencil frame origin.
		pencil xor.
		pencil place: start.
		pencil goto: end.
		pencil place: start.
		pencil goto: end].
	tone = white
	  ifTrue: [pencil white]
	  ifFalse: [pencil black].
	pencil width: width.
	pencil place: start.
	pencil goto: end
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
draw | |
	(tone = white or: [tone = black])
	  ifTrue:
		[pencil place: self mpOnGrid - pencil frame origin.
		grid = 1
		  ifTrue: [ "make drawing with grid 1 fast"
			[user redbug] whileTrueDo: [pencil goto: user mp - pencil frame origin]]
		  ifFalse: [[user redbug] whileTrueDo: [pencil goto: self mpOnGrid - pencil frame origin]]]
	  ifFalse: [self brush: graypens ◦ pencil width]
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
blowup: smallRect to: bigRectFrame | bigRect box pt i turt flag bits |
	bits ← bigRectFrame bitsIntoString.
	bigRect ← bigRectFrame inset: 2 ⌾ 2.
	smallRect blowup: bigRect origin by: blowupScale.
	turt ← Turtle init.
	box ← 0 ⌾ 0 rect: blowupScale - 1 ⌾ (blowupScale - 1) "keep editing in blowup mode until the user presses a button
		outside the big rect".
	[flag] whileTrueDo:
		[(bigRect has: (pt ← user mp))
		  ifTrue:
			[box moveto: bigRect origin + (i ← pt - bigRect origin | blowupScale).
			turt place: smallRect origin + (i / blowupScale).
			user redbug
			  ifTrue:
				[box color: black mode: storing.
				turt black.
				turt go: 0]
			  ifFalse:
				[user yellowbug
				  ifTrue:
					[box color: white mode: storing.
					turt white.
					turt go: 0]
				  ifFalse:
					[user bluebug
					  ifTrue: [bigRect flash]]]]
		  ifFalse:
			[user anybug
			  ifTrue:
				[((bigRect inset: ¬5 ⌾ ¬5) has: pt)
				  ifTrue: [bigRect flash]
				  ifFalse: [ "quit"
					flag ← false]]]].
	bigRectFrame bitsFromString: bits
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
getRectangle | rect newrect start t |
	 "rect must be in my frame"
	start ← self mpOnGrid "the rect-newrect stuff is so that the complementing stays
		on for a while".
	rect ← newrect ← (Rectangle new origin: start corner: start) intersect: self frame "move the cursor slightly so that the user will notice the rectangle
		being complemented".
	user cursorloc← start + 4.
	[user anybug] whileTrueDo:
		[rect ← newrect.
		rect comp.
		t ← self mpOnGrid.
		newrect ← (Rectangle new origin: (start min: t) corner: (start max: t)) intersect: self frame.
		rect comp].
	^rect
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
mode | |
	^mode
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
shade | p1 p2 a b t p r vs |
	 "down on redbug is black place.
	up on redbug is white place.  Subsequent redbugs
	paint a shade of gray depending on position between
	black and white (and beyond white to black again).
	Yellow or blue bug terminates."
	[user redbug] whileFalseDo:  [p1 ← user mp "black"].
	[user nobug] whileFalseDo:  [p2 ← user mp "white"].
	vs ← #(¬1 ¬1025 ¬1089 ¬585 ¬4681 ¬6731 ¬22058 ¬27031 ¬26986 ¬31191 ¬32108 5160 5128 8321 1025 1 0 ).
	r ← 0 ⌾ 0 rect: 10 ⌾ 10.
	b ← p1 - p2.
	b ← b x asFloat ⌾ b y asFloat.
	a ← b x * b x + (b y * b y) / 16.0.
	[user yellowbug or: [user bluebug]] whileFalseDo: 
		[user redbug
		  ifTrue:
			[p ← user mp.
			t ← b * (p - p2).
			t ← (t x + t y / a) asInteger abs min: 16.
			brush brush: p mode: mode color: vs ◦ (17 - t)]]
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
tone | |
	^tone
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
paint | |
	self brush: brush
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
block | |
	self getRectangle color: tone mode: mode
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
mpOnGrid | |
	 "return mouse point rounded to grid"
	^user mp + (grid / 2) | grid
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
setbrush | rect |
	rect ← self getRectangle.
	(rect empty or: [50 ⌾ 50 < rect extent])
	  ifTrue: [pencil frame flash]
	  ifFalse:
		[.
		brush color: white mode: storing.
		brush title: 'brush' in: rect.
		brush saveScreenBits.
		brush moveto: brushpt.
		brush show.
		action ← #paint]
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
brush | |
	^brush
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
redbug | |
	self perform: action
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
brush: sourceRect | minpt maxpt pt offset |
	 "use the bits in the BitRect sourceRect as a brush"
	sourceRect moveto: brushpt "The inner painting loop should be fast - all the extra foliage below
		is to move tests outside of the inner loop".
	sourceRect show.
	minpt ← self frame origin.
	maxpt ← self frame corner - sourceRect extent.
	offset ← sourceRect extent / 2 "If mode is storing or oring, use brush command, otherwise blt.
		Use the unclipped form of brushing  and grid=1 when possible".
	(mode < xoring and: [grid = 1])
	  ifTrue: [[user redbug] whileTrueDo:
			[(minpt ≤ (pt ← user mp - offset) and: [pt ≤ maxpt])
			  ifTrue: [sourceRect brush: pt mode: mode color: tone]
			  ifFalse: [sourceRect brush: pt mode: mode color: tone clippedBy: self frame]]]
	  ifFalse:
		[(mode ≥ xoring and: [grid = 1])
		  ifTrue: [[user redbug] whileTrueDo:
				[(minpt ≤ (pt ← user mp - offset) and: [pt ≤ maxpt])
				  ifTrue: [sourceRect blt: pt mode: mode]
				  ifFalse: [sourceRect blt: pt mode: mode clippedBy: self frame]]]
		  ifFalse:
			[mode < xoring
			  ifTrue: [ "grid is > 1"
				[user redbug] whileTrueDo:
					[(minpt ≤ (pt ← self mpOnGrid - offset) and: [pt ≤ maxpt])
					  ifTrue: [sourceRect brush: pt mode: mode color: tone]
					  ifFalse: [sourceRect brush: pt mode: mode color: tone clippedBy: self frame]]]
			  ifFalse: [ "grid is > 1 and mode≥xoring"
				[user redbug] whileTrueDo:
					[(minpt ≤ (pt ← self mpOnGrid - offset) and: [pt ≤ maxpt])
					  ifTrue: [sourceRect blt: pt mode: mode]
					  ifFalse: [sourceRect blt: pt mode: mode clippedBy: self frame]]]]]
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
showon: butvec | |
	butvec ◦ 1 setvalue: action.
	butvec ◦ 2 setvalue: tone.
	butvec ◦ 3 setvalue: mode.
	butvec ◦ 4 setvalue: pencil width.
	butvec ◦ 5 setvalue: grid
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
frame: f | |
	pencil frame: f
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
frame | |
	^pencil frame
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
setfrom: butvec | pt |
	(butvec ◦ 1 has: (pt ← user mp))
	  ifTrue: [action ← butvec ◦ 1 bug: pt]
	  ifFalse:
		[(butvec ◦ 2 has: pt)
		  ifTrue:
			[tone ← butvec ◦ 2 bug: pt.
			tone = white
			  ifTrue: [pencil white]
			  ifFalse: [pencil black]]
		  ifFalse:
			[(butvec ◦ 3 has: pt)
			  ifTrue: [mode ← butvec ◦ 3 bug: pt]
			  ifFalse:
				[(butvec ◦ 4 has: pt)
				  ifTrue: [pencil width: (butvec ◦ 4 bug: pt)]
				  ifFalse:
					[(butvec ◦ 5 has: pt)
					  ifTrue: [grid ← butvec ◦ 5 bug: pt]]]]]
! !
!BitRectTool methodsFor: 'undefined' stamp: ''!
brushpt: pt | |
	 "set the point at which the current brush will be shown"
	brushpt ← pt
! !
!Bitmap methodsFor: 'undefined' stamp: ''!
fromString: s | i |
	(1 to: self length) do: [:i | self ◦ i ← s ◦ (i + i) + (s ◦ (i + i - 1) lshift: 8)]
! !
!Bitmap methodsFor: 'undefined' stamp: ''!
toStream: strm | i |
	(1 to: self length) do:
		[:i | strm next← self ◦ i lshift: ¬8.
		strm next← self ◦ i land: 255]
! !
!Bitmap methodsFor: 'undefined' stamp: ''!
◦ x | |<primitive: 17>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^self instfield: x]
! !
!Bitmap methodsFor: 'undefined' stamp: ''!
fromStream: strm | i hi lo |
	(1 to: self length) do:
		[:i | hi ← strm next.
		lo ← strm next.
		self ◦ i ← (hi lshift: 8) + lo]
! !
!Bitmap methodsFor: 'undefined' stamp: ''!
◦ x ← val | |<primitive: 18>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^self instfield: x ← val]
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
printOn: strm ||
	strm append: '[] in '.
	super printOn: strm
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
value | |<primitive: 26>
	self valueError
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
value: arg | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
erase | |
	home ← nil.
	super erase
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
blockCopy: t1 | |
	nargs ← t1.
	^(BlockContext new: self length) home: home startpc: pc + 2 nargs: nargs
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
tempAt: index put: value | |
	^home ◦ index ← value
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
eval | save val |
	nargs = 0
	  ifTrue: [^self value].
	nargs = 1
	  ifTrue:
		[save ← startpc "amazing crock for reading ST76 remote variable".
		startpc ← startpc + (self method ◦ startpc < 112
				  ifTrue: [1]
				  ifFalse: [2]).
		nargs ← 0.
		val ← self value.
		startpc ← save.
		nargs ← 1.
		^val].
	self valueError
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
value: arg1 value: arg2 value: arg3 | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
valueError | |
	user notify: 'Incompatible number of args'
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
method | |
	^home method
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
receiver | |
	^home receiver
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
valueWithArgs: vec | t2 |
	(t2 ← vec length) = 0
	  ifTrue: [^self value].
	t2 = 1
	  ifTrue: [^self value: vec ◦ 1].
	t2 = 2
	  ifTrue: [^self value: vec ◦ 1 value: vec ◦ 2].
	t2 = 3
	  ifTrue: [^self value: vec ◦ 1 value: vec ◦ 2 value: vec ◦ 3].
	user notify: 'More than 3 args for value:'
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
value← val | |
	 "for compatibility with ST76 remote variables"
	^self value: val
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
home: t1 startpc: t2 nargs: t3 | |
	home ← t1.
	startpc ← t2.
	nargs ← t3
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
value: arg1 value: arg2 | |<primitive: 27>
	self valueError
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
tempAt: index | |
	^home ◦ index
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
sender: t1 pc: t2 stackp: t3 home: t4 startpc: t5 nargs: t6 | |
	sender ← t1.
	pc ← t2.
	stackp ← t3.
	home ← t4.
	startpc ← t5.
	nargs ← t6
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
forkWithArgs: args | |
	^self fork: ↪valueWithArgs: withArgs: args
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
home || ^home
! !
!BlockContext methodsFor: 'undefined' stamp: ''!
fork | |
	^self fork: ↪value
! !
!BorderedText methodsFor: 'undefined' stamp: ''!
displayat: pt effect: effect clippedBy: cliprect | origin corner t6 |
	super displayat: pt effect: effect clippedBy: cliprect "display text and border around it ".
	origin ← frame origin.
	corner ← frame corner.
	(t6 ← Rectangle new origin: origin - (1 ⌾ 1) corner: corner x + 1 ⌾ origin y) color: ¬1 mode: effect.
	t6 moveto: origin x - 1 ⌾ corner y.
	t6 color: ¬1 mode: effect.
	t6 origin← corner x ⌾ (origin y - 1).
	t6 color: ¬1 mode: effect.
	t6 moveto: origin - (1 ⌾ 1).
	t6 color: ¬1 mode: effect
! !
!BorderedText methodsFor: 'undefined' stamp: ''!
presson: press in: r | scale |
	scale ← press scale.
	press setp: r origin x - scale ⌾ (r corner y - (3 * scale)).
	press showrectwidth: scale * (2 + self width) height: scale.
	press setp: r origin x - scale ⌾ (r corner y - (self height + 5 * scale)).
	press showrectwidth: scale * (2 + self width) height: scale.
	press setp: r origin x - scale ⌾ (r corner y - (scale * (self height + 4))).
	press showrectwidth: scale height: scale * (self height + 2).
	press setp: r origin x + (scale * self width) ⌾ (r corner y - (scale * (self height + 4))).
	press showrectwidth: scale height: scale * (self height + 2).
	^para presson: press in: r
! !
!BorderedText methodsFor: 'undefined' stamp: ''!
copy | t |
	t ← BorderedText new paragraph: para copy frame: frame copy style: style copy.
	t c1← c1.
	t c2← c2.
	t begintypein← begintypein.
	^t
! !
!BorderedText methodsFor: 'undefined' stamp: ''!
pressCode | |
	^6
! !
!BravoPrinter methodsFor: 'undefined' stamp: ''!
init | |
	super init.
	eject ← false
! !
!BravoPrinter methodsFor: 'undefined' stamp: ''!
nextpage | |
	eject
	  ifTrue: [self eject]
	  ifFalse: [eject ← true]
! !
!BravoPrinter methodsFor: 'undefined' stamp: ''!
eject | |
	strm next← 12.
	strm cr
! !
!BravoPrinter methodsFor: 'undefined' stamp: ''!
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
!BrowseWindow methodsFor: 'undefined' stamp: ''!
classInit | |
	stdTemplates ← {(0 ⌾ 0 rect: 10 ⌾ 14) , (10 ⌾ 0 rect: 18 ⌾ 14) , (18 ⌾ 0 rect: 28 ⌾ 14) , (28 ⌾ 0 rect: 36 ⌾ 14) , (0 ⌾ 14 rect: 36 ⌾ 36)}
! !
!BrowseWindow methodsFor: 'undefined' stamp: ''!
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
!CharLine methodsFor: 'undefined' stamp: ''!
starti | |
	^starti
! !
!CharLine methodsFor: 'undefined' stamp: ''!
spaces | |
	^spaces
! !
!CharLine methodsFor: 'undefined' stamp: ''!
stopi | |
	^stopi
! !
!CharLine methodsFor: 'undefined' stamp: ''!
padwidth | |
	^padwidth
! !
!CharLine methodsFor: 'undefined' stamp: ''!
stopi← t1 | |
	stopi ← t1
! !
!CharLine methodsFor: 'undefined' stamp: ''!
starti: t1 stopi: t2 spaces: t3 padwidth: t4 | |
	starti ← t1.
	stopi ← t2.
	spaces ← t3.
	padwidth ← t4
! !
!CharLine methodsFor: 'undefined' stamp: ''!
= line | |
	^(starti = line starti and: [stopi = line stopi]) and: [(spaces = line spaces and: [padwidth = line padwidth])]
! !
!CharLine methodsFor: 'undefined' stamp: ''!
slide: delta | |
	starti ← starti + delta.
	stopi ← stopi + delta
! !
!Class methodsFor: 'undefined' stamp: ''!
instsize | | "Return the number of user accessable instance fields"
	^instsize land: 4095
! !
!Class methodsFor: 'undefined' stamp: ''!
allSubclasses | strm cl |
	strm ← (Vector new: 5) asStream.
	Class allInstancesDo:
		[:cl | cl superclass ≡ self ifTrue: [strm next ← cl]].
	^strm contents
! !
!Class methodsFor: 'undefined' stamp: ''!
isVariable | |
	^instsize allmask: 8r10000
! !
!Class methodsFor: 'undefined' stamp: ''!
◦ x | |
	^classvars ◦ x
! !
!Class methodsFor: 'undefined' stamp: ''!
md | |
	^messagedict
! !
!Class methodsFor: 'undefined' stamp: ''!
◦ x ← val | |
	^classvars ◦ x ← val
! !
!Class methodsFor: 'undefined' stamp: ''!
myinstvars | |
	^myinstvars
! !
!Class methodsFor: 'undefined' stamp: ''!
isBytes | |
	^instsize nomask: 8r20000
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
instspec | | "Return the number of user accessable instance fields"
	^instsize
! !
!Class methodsFor: 'undefined' stamp: ''!
Isa: x | |
	 "is x on my superclass chain?"
	superclass ≡ x
	  ifTrue: [^true].
	superclass ≡ nil
	  ifTrue: [^false].
	^superclass Isa: x
! !
!Class methodsFor: 'undefined' stamp: ''!
fieldNamesInto: collector | |
	superclass ≡ nil
	  ifFalse: [superclass fieldNamesInto: collector].
	^(Reader new of: myinstvars) readInto: collector
! !
!Class methodsFor: 'undefined' stamp: ''!
superclass | |
	^superclass
! !
!Class methodsFor: 'undefined' stamp: ''!
title | |
	^title
! !
!Class methodsFor: 'undefined' stamp: ''!
isBits | |
	^instsize nomask: 8r40000
! !
!Class methodsFor: 'undefined' stamp: ''!
instvars | |
	self ≡ lastInstvarClass
	  ifTrue: [^lastInstvars copy].
	 "cache last computation of instvars"
	lastInstvarClass ← self.
	lastInstvars ← self fieldNamesInto: FieldNameCollector default.
	^lastInstvars copy
! !
!Class methodsFor: 'undefined' stamp: ''!
recompile: selector | |
	self understands: (self code: selector) asParagraph
! !
!Class methodsFor: 'undefined' stamp: ''!
usesNewSyntax | |
	self ≡ LADObject
	  ifTrue: [^true].
	superclass ≡ nil
	  ifTrue: [^false].
	^superclass usesNewSyntax
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
edit: selector para: para formerly: oldpara | |
	NoteTaker
	  ifTrue: [user schedule: (CodeWindow new class: self selector: selector para: para formerly: oldpara)]
	  ifFalse:
		[user leaveTop.
		user restartup: (CodeWindow new class: self selector: selector para: para formerly: oldpara)]
! !
!Class methodsFor: 'undefined' stamp: ''!
ed: selector | c s |
	c ← self code: selector.
	user clearshow: c.
	[s ← user request: 'substitute: '] whileTrueDo:
		[c ← c subst: s for: (user request: 'for: ').
		user clearshow: c].
	self understands: c
! !
!Class methodsFor: 'undefined' stamp: ''!
edit: selector | para s v |
	para ← (selector = #ClassOrganization
			  ifTrue: [self organization asParagraph]
			  ifFalse:
				[(messagedict has: selector)
				  ifTrue: [self code: selector]
				  ifFalse: [nullString asParagraph]]).
	self edit: selector para: para formerly: false
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
filout | |
	user displayoffwhile⦂ [
		((dp0 file: title + '.st.') filoutclass: self.
		self noChanges)]
! !
!Class methodsFor: 'undefined' stamp: ''!
printoutCategory: cat | |
	(dp0 file: (title + '-' + cat + '.press') asFileName) printout: (self changelist: cat)
! !
!Class methodsFor: 'undefined' stamp: ''!
startCategory: s on: pstrm | |
	pstrm print: (('
' + s) asParagraph maskrunsunder: 241 to: 81) "Font 5, Bold"
! !
!Class methodsFor: 'undefined' stamp: ''!
printCategory: s on: pstrm | sel |
	self startCategory: s on: pstrm.
	(self organization category: s) do: [:sel | self printMethod: sel on: pstrm].
	self endCategoryOn: pstrm
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
changelist: cat | |
	^{title unique , (self organization category: cat)}
! !
!Class methodsFor: 'undefined' stamp: ''!
definition | strm |
	 "return a string that defines me (Class new title etc.)"
	strm ← (String new: 50) asStream.
	self printdefon: strm.
	^strm contents
! !
!Class methodsFor: 'undefined' stamp: ''!
readfrom: strm format: f | |
	^self new readfrom: strm format: f
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
readfrom: strm | |
	^self readfrom: strm format: nil
! !
!Class methodsFor: 'undefined' stamp: ''!
printout | |
	user displayoffwhile⦂ [((dp0 file: title + '.press.') printoutclass: self)]
! !
!Class methodsFor: 'undefined' stamp: ''!
filoutCategory: cat | |
	(dp0 file: (title + '-' + cat + '.st') asFileName) filout: (self changelist: cat)
! !
!Class methodsFor: 'undefined' stamp: ''!
noChanges | s t |
	t ← title + ' *'.
	Changes contents do:
		[:s | ((s ◦ 1 = 126 "~" and: [(t match: s ◦ (2 to: s length))]) or: [(t match: s)])
		  ifTrue: [Changes delete: s]]
! !
!Class methodsFor: 'undefined' stamp: ''!
startChangesOn: pstrm | |
	pstrm print: (('
' + title + ' asFollows') asParagraph maskrunsunder: 241 to: 81) "Font 5, Bold"
! !
!Class methodsFor: 'undefined' stamp: ''!
printMethod: sel on: pstrm | |
	pstrm print: (self code: sel)
! !
!Class methodsFor: 'undefined' stamp: ''!
endChangesOn: pstrm | |
	pstrm print: '' asParagraph
! !
!Class methodsFor: 'undefined' stamp: ''!
endCategoryOn: pstrm | |
	
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
title: t | | 
	self title: (title ← t unique) insystem: Smalltalk
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
obsolete | |
	title ← 'AnObsolete' + title.
	classvars ← nil. "recycle class variables"
	messagedict ← MethodDictionary init: 2. "invalidate and recycle local messages"
	self someInstance ifTrue: [environment ← self] "keep me around for old instances"
! !
!Class methodsFor: 'undefined' stamp: ''!
bytesize: n | | "non-pointer declaration"
	self ≠ self realself
	  ifTrue: [self realself bytesize: n]
	  ifFalse:
		[instsize ← n≡true
			ifTrue: [¬4096]
			ifFalse: [n = 8 ifTrue: [4096] ifFalse: [12288]]]
! !
!Class methodsFor: 'undefined' stamp: ''!
classInit | | "gets propagated to a dummy instance"
	^self new classInit
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
fixSubClassesOf: oldClass | n subClass |
	user classNames do:
		[:n | subClass ← Smalltalk ◦ n.
		subClass superclass ≡ oldClass
		  ifTrue: [Class new copyof: subClass subclassof: self]]
! !
!Class methodsFor: 'undefined' stamp: ''!
copyof: oldClass subclassof: newSubClass | |
	title ← oldClass title.
	self subclassof: newSubClass.
	classvars ← oldClass classvars.
	environment ← oldClass environment.
	self newFieldsForSubClass: oldClass myinstvars
! !
!Class methodsFor: 'undefined' stamp: ''!
realself | |
	^Smalltalk ◦ title unique "as opposed to possible filin ghost"
! !
!Class methodsFor: 'undefined' stamp: ''!
abstract | |
	self fields: nullString
! !
!Class methodsFor: 'undefined' stamp: ''!
subclassof: cl | |
	(cl is: Class)
		ifTrue: [superclass ← cl]
		ifFalse: [user notify: 'Superclass is not yet defined or not a Class']
! !
!Class methodsFor: 'undefined' stamp: ''!
sharing: table | |
	self ≠ self realself
	  ifTrue: [self realself sharing: table]
	  ifFalse: [environment ← {environment asVector , table}]
! !
!Class methodsFor: 'undefined' stamp: ''!
environment← t1 | |
	environment ← t1 "for resetting to reread sharing clauses"
! !
!Class methodsFor: 'undefined' stamp: ''!
myinstvars← t1 | |
	myinstvars ← t1
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
veryspecial: n | |
	 "inaccessible fields"
	instsize ← self instvars length + n
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
allInstances | strm x |
	strm← (Vector new: 16) asStream.
	self allInstancesDo: [:x | strm next← x].
	^strm contents
! !
!Class methodsFor: 'undefined' stamp: ''!
allInstancesDo: aBlock | inst |
	inst← self someInstance.
	inst ifTrue:
		[aBlock value: inst.
		[inst← inst nextInstance]
			whileTrueDo: [aBlock value: inst].
		false class≡self ifTrue: [aBlock value: false]]
! !
!Class methodsFor: 'undefined' stamp: ''!
recopy: inst | copy i var |
	var← self isVariable
	copy ← var ifTrue: [self new: inst length] ifFalse: [self new].
	(1 to: self instsize) do: [:i | copy instfield: i ← (inst instfield: i) recopy].
	var ifTrue: [(1 to: inst length) do: [:i | copy◦i ← (inst◦i) recopy]].
	^copy
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
copy: inst | copy i var |
	var← self isVariable.
	copy ← var ifTrue: [self new: inst length] ifFalse: [self new].
	(1 to: self instsize) do: [:i | copy instfield: i ← inst instfield: i].
	var ifTrue: [(1 to: inst length) do: [:i | copy◦i ← inst◦i]].
	^copy
! !
!Class methodsFor: 'undefined' stamp: ''!
new: length | | <primitive: 30>
	self isVariable
	  ifTrue: [user croak]
	  ifFalse: [^self new init: length] "ST76 convention"
! !
!Class methodsFor: 'undefined' stamp: ''!
someInstance | |<primitive: 84>
	 "return first instance of this class, false if there are none"
	user croak
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
new | |<primitive: 29>
	self isVariable ifTrue: [^self new: 0].
	user croak
! !
!Class methodsFor: 'undefined' stamp: ''!
init: n | |
	 "init and default get propagated to instances"
	self isVariable
	  ifTrue: [^(self new: n) init].
	^self new init: n
! !
!Class methodsFor: 'undefined' stamp: ''!
howMany | x n | "how many instances of this class are in use now?"
	n ← 0.
	self allInstancesDo: [:x | n ← n + 1].
	^n
! !
!Class methodsFor: 'undefined' stamp: ''!
instfield: i | |
	 "prevent user from getting freelist"
	i > Class instsize
	  ifTrue: [user notify: 'arg too big']
	  ifFalse: [^super instfield: i]
! !
!Class methodsFor: 'undefined' stamp: ''!
init | |
	 "init and default get propagated to instances"
	^self new init
! !
!Class methodsFor: 'undefined' stamp: ''!
default | |
	^self new default
! !
!Class methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm append: 'Class '.
	strm append: title
! !
!Class methodsFor: 'undefined' stamp: ''!
whosends: selector | senders lit sel |
	senders ← Stream default.
	messagedict do:
		[:sel |
		(messagedict method: sel) literals do:
			[:lit |
			selector ≡ lit ifTrue: [senders append: sel; space]]].
	^senders contents
! !
!Class methodsFor: 'undefined' stamp: ''!
compileall | sel | "does not modify code, just compiles it"
	messagedict do: [:sel | self recompile: sel]

"to recompile the whole system (check out big changes) execute:
	user classNames do:
		[:name |
		user show: name; cr.
		(Smalltalk◦name) compileall.
		Changes init]. "
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
understands: code classified: heading | | "compile and install method"
	^LADCompiler new compile: code asParagraph in: self under: heading notifying: self
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
textLocal | s |
	s ← self organization "makes comment and methods local".
	s globalComment← s globalComment.
	messagedict do: [:s | messagedict code: s ← self code: s]
! !
!Class methodsFor: 'undefined' stamp: ''!
shrink | |
	messagedict ← messagedict shrink
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
decompile: sel | |
	^user displayoffwhile⦂ [(LADDecompiler new decompile: sel in: self)]
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
canUnderstand: selector | |
	(messagedict has: selector)
	  ifTrue: [^self].
	superclass ≡ nil
	  ifTrue: [^false].
	^superclass canUnderstand: selector
! !
!Class methodsFor: 'undefined' stamp: ''!
copy: sel from: class | |
	self copy: sel from: class classified: nil
! !
!Class methodsFor: 'undefined' stamp: ''!
messages | |
	^{messagedict contents , #ClassOrganization}
! !
!Class methodsFor: 'undefined' stamp: ''!
bytesof: sel | |
	^(messagedict method: sel) asBytes
! !
!Class methodsFor: 'undefined' stamp: ''!
selectors | |
	 "Return a Vector of all my selectors."
	^self messages
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
understands: code | selector old |
	 "install method"
	^self understands: code classified: 'As yet unclassified'
! !
!Class methodsFor: 'undefined' stamp: ''!
method: sel | |
	^messagedict methodorfalse: sel
! !
!Class methodsFor: 'undefined' stamp: ''!
notify: errorString at: position in: stream | |
	^self notify: errorString at: position in: stream for: self
! !
!Class methodsFor: 'undefined' stamp: ''!
canunderstand: selector | |
	^messagedict has: selector
! !
!Class methodsFor: 'undefined' stamp: ''!
space | a s |
	s ← 0.
	messagedict do: [:a | s ← s + (messagedict method: a) length].
	^s
! !
!Class methodsFor: 'undefined' stamp: ''!
classified: heading understands: code | |
	"A synonym for replay"
	^self understands: code classified: heading
! !
!Class methodsFor: 'undefined' stamp: ''!
wholeEnvironment | |
	^(classvars asVector concat: environment asVector) concat: (superclass ≡ nil
	  ifTrue: [#()]
	  ifFalse: [superclass wholeEnvironment])
! !
!Class methodsFor: 'undefined' stamp: ''!
clean | name |
	 "release unreferenced classvars"
	classvars do:
		[:name | (name ≠ #ClassOrganization and: [(classvars ref: name) refct = 1])
		  ifTrue: [classvars delete: name]]
! !
!Class methodsFor: 'undefined' stamp: ''!
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
!Class methodsFor: 'undefined' stamp: ''!
environment | |
	^environment
! !
!Class methodsFor: 'undefined' stamp: ''!
classvars | |
	^classvars
! !
!Class methodsFor: 'undefined' stamp: ''!
category: cat | |
	(cat is: String)
	  ifTrue: [SystemOrganization add: self title unique under: cat]
	  ifFalse: [user notify: 'Category name must be a String']
! !
!Class methodsFor: 'undefined' stamp: ''!
moveFromCat: cat1 to: cat2 | |
	((cat1 is: String) and: [(cat2 is: String)])
	  ifTrue: [SystemOrganization move: self title unique from: cat1 to: cat2]
	  ifFalse: [user notify: 'Category name must be a String']
! !
!Class methodsFor: 'undefined' stamp: ''!
category | |
	^SystemOrganization invert: self title unique
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
globalComment← t1 | |
	 "String or RemoteParagraph"
	globalComment ← t1
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
deleteCategory: index | |
	groupVector ← groupVector without: index.
	commentVector ← commentVector without: index
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
insert: heading | di dgroup hi |
	 "force default category to end, delete if empty"
	(di ← commentVector find: default) > 0
	  ifTrue: [dgroup ← groupVector ◦ di].
	commentVector ← {(commentVector without: di) , heading}.
	groupVector ← {(groupVector without: di) , (Vector new: 0)}.
	hi ← commentVector length.
	(di = 0 or: [dgroup length = 0])
	  ifTrue: [^hi].
	commentVector ← {commentVector , default}.
	groupVector ← {groupVector , dgroup}.
	^hi
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
categories | |
	^commentVector
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
globalComment | |
	^globalComment asParagraph text
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
category: str | i |
	i ← commentVector find: str.
	i = 0
	  ifTrue: [user notify: 'No such category: ' + str]
	  ifFalse: [^groupVector ◦ i]
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
has: sel | t |
	groupVector do:
		[:t | (t has: sel)
		  ifTrue: [^true]].
	^false
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
invert: selector | i |
	(1 to: groupVector length) do:
		[:i | (groupVector ◦ i has: selector)
		  ifTrue: [^commentVector ◦ i]].
	^false
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
globalCommentItself | |
	^globalComment "used only by Class archiveOn:changesOnly:"
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
classify: selector under: heading | s h n |
	(selector is: Vector)
	  ifTrue: [selector do: [:s | self classify: s under: heading]]
	  ifFalse:
		[s ← commentVector find: heading.
		(s > 0 and: [(groupVector ◦ s has: selector)])
		  ifTrue: [^self].
		(h ← self invert: selector)
		  ifTrue:
			[heading = default
			  ifTrue: [^self].
			n ← commentVector find: h.
			groupVector ◦ n ← groupVector ◦ n delete: selector].
		s = 0
		  ifTrue: [s ← self insert: heading].
		groupVector ◦ s ← groupVector ◦ s insertSorted: selector.
		n ← commentVector find: default.
		(n > 0 and: [(groupVector ◦ n) length = 0])
		  ifTrue: [self deleteCategory: n]]
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
asStream | v t |
	v ← Stream new of: (Vector new: 200).
	groupVector do: [:t | v append: t].
	^v contents asStream
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
delete: selector | i |
	 "delete this from all categories"
	(1 to: groupVector length) do:
		[:i | (groupVector ◦ i has: selector)
		  ifTrue:
			[groupVector ◦ i ← groupVector ◦ i delete: selector.
			((groupVector ◦ i) length = 0 and: [commentVector ◦ i = default])
			  ifTrue: [self deleteCategory: i]]]
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
asParagraph | s i |
	s ← Stream default.
	s print: self globalComment.
	(1 to: commentVector length) do:
		[:i | s cr.
		s print: ((commentVector ◦ i) inVector concat: groupVector ◦ i)].
	^s contents asParagraph
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
fromParagraph: para | t i j g |
	user displayoffwhile⦂ [
		(t ← para asVector.
		self globalComment← t ◦ 1.
		commentVector ← Vector new: t length - 1.
		groupVector ← Vector new: t length - 1.
		(1 to: t length - 1) do:
			[:i | g ← t ◦ (i + 1).
			commentVector ◦ i ← g ◦ 1.
			[0 = (j ← g find: #←)] whileFalseDo:  [ "reconstitute ← suffixes"
				g ← g replace: j - 1 to: j by: (g ◦ (j - 1) + '←') unique inVector].
			groupVector ◦ i ← (g copy: 2 to: g length) sort])]
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
init: sortedVec | |
	self globalComment← 'This class has not yet been commented'.
	commentVector ← 'As yet unclassified' inVector.
	groupVector ← sortedVec inVector
! !
!ClassOrganizer methodsFor: 'undefined' stamp: ''!
classInit | |
	default ← 'As yet unclassified'
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
noCode | |
	selection = 0
	  ifTrue: [^systemPane noCode].
	^''
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
dirty | |
	^organizationPane dirty
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
compile: parag | |
	systemPane compile: parag
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
from: t1 to: t2 | |
	systemPane ← t1.
	organizationPane ← t2
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
classInit | |
	editmenu ← Menu new string: 'filout
print
forget'
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
selected | |
	organizationPane class: Smalltalk ◦ (list ◦ selection) "My selection just changed.  Tell organizationPane to display the categories of my newly selected Class."
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
deselected | |
	organizationPane class: nil "I just lost my selection.  Tell organizationPane to display nothing."
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
close | |
	systemPane ← nil.
	super close
! !
!ClassPane methodsFor: 'undefined' stamp: ''!
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
!CodePane methodsFor: 'undefined' stamp: ''!
compile: parag in: defaultClass under: category | |
	^LADCompiler new compile: parag in: (class ≡ nil
	  ifTrue: [defaultClass]
	  ifFalse: [class]) under: category notifying: self
! !
!CodePane methodsFor: 'undefined' stamp: ''!
execute: parseStream in: context to: receiver | cls |
	context
		ifTrue: [cls← context mclass. receiver← context receiver]
		ifFalse: [cls← receiver class].
	^LADCompiler new evaluate: parseStream in: context to: receiver notifying: self
! !
!CodePane methodsFor: 'undefined' stamp: ''!
selectRange: r | |
	pared selectRange: r.
	pared selectAndScroll
! !
!CodePane methodsFor: 'undefined' stamp: ''!
dirty | |
	pared formerly
	  ifTrue: [^frame].
	^false
! !
!CodePane methodsFor: 'undefined' stamp: ''!
contents | |
	^pared contents
! !
!CodePane methodsFor: 'undefined' stamp: ''!
notify: errorString at: position in: stream | |
	pared fintype.
	pared selectRange: (position to: position).
	pared replace: ('➲' + errorString + '➲.') asParagraph.
	pared selectAndScroll.
	^false
! !
!CodePane methodsFor: 'undefined' stamp: ''!
compile: parag | |
	 "as my own selectorPane"
	^self compile: parag in: class under: 'As yet unclassified'
! !
!CodePane methodsFor: 'undefined' stamp: ''!
interactive | |
	^true
! !
!CodePane methodsFor: 'undefined' stamp: ''!
execute: parseStream for: codePane | |
	 "as my own selectorPane"
	^self execute: parseStream in: false to: nil
! !
!CodePane methodsFor: 'undefined' stamp: ''!
compile: parag in: defaultClass under: category | |
	^(self compiler: defaultClass) new compile: parag in: (class ≡ nil
	  ifTrue: [defaultClass]
	  ifFalse: [class]) under: category notifying: self
! !
!CodePane methodsFor: 'undefined' stamp: ''!
oldContents | |
	^pared formerly
! !
!CodePane methodsFor: 'undefined' stamp: ''!
reflects: selection | |
	 "am I trying to show the code of selectorPaneⓢ selection?"
	^class ≡ nil and: [selection > 0]
! !
!CodePane methodsFor: 'undefined' stamp: ''!
formerly: oldpara | |
	 "should not be called before 'showing:'"
	pared formerly: oldpara
! !
!CodePane methodsFor: 'undefined' stamp: ''!
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
!CodePane methodsFor: 'undefined' stamp: ''!
from: t1 | |
	selectorPane ← t1
! !
!CodePane methodsFor: 'undefined' stamp: ''!
showing: paragraph | |
	pared ← TextImage new para: paragraph asParagraph frame: nil.
	pared formerly: false.
	pared fixframe: frame.
	self windowenter.
	scrollBar ← (scrollBar ≡ nil
			  ifTrue: [ScrollBar new]
			  ifFalse: [scrollBar]) on: frame from: pared
! !
!CodePane methodsFor: 'undefined' stamp: ''!
init | |
	
! !
!CodePane methodsFor: 'undefined' stamp: ''!
class: t1 selector: t2 para: para | |
	class ← t1.
	selector ← t2
! !
!CodePane methodsFor: 'undefined' stamp: ''!
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
!CodePane methodsFor: 'undefined' stamp: ''!
outline | |
	frame outline: 1
! !
!CodePane methodsFor: 'undefined' stamp: ''!
windowleave | |
	pared ≡ nil
	  ifFalse: [pared leave]
! !
!CodePane methodsFor: 'undefined' stamp: ''!
windowenter | |
	self outline.
	pared enter
! !
!CodePane methodsFor: 'undefined' stamp: ''!
close | |
	pared unselect.
	selectorPane ← pared ← nil.
	scrollBar close
! !
!CodePane methodsFor: 'undefined' stamp: ''!
outside | |
	^scrollBar startup
! !
!CodePane methodsFor: 'undefined' stamp: ''!
enter | |
	scrollBar show
! !
!CodePane methodsFor: 'undefined' stamp: ''!
redbug | |
	^pared selecting
! !
!CodePane methodsFor: 'undefined' stamp: ''!
picked | |
	^frame has: user mp
! !
!CodePane methodsFor: 'undefined' stamp: ''!
leave | |
	scrollBar hide
! !
!CodePane methodsFor: 'undefined' stamp: ''!
keyset | |
	^pared keyset
! !
!CodePane methodsFor: 'undefined' stamp: ''!
kbd | |
	pared kbd
! !
!CodePane methodsFor: 'undefined' stamp: ''!
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
!CodePane methodsFor: 'undefined' stamp: ''!
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
!CodePane methodsFor: 'undefined' stamp: ''!
frame← t1 | |
	frame ← t1.
	pared ≡ nil "Change my frame and that of my pared (if any)."
	  ifFalse:
		[pared frame← frame.
		scrollBar on: frame from: pared]
! !
!CodePane methodsFor: 'undefined' stamp: ''!
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
!CodePane methodsFor: 'undefined' stamp: ''!
show | |
	frame outline.
	pared show
! !
!CodeWindow methodsFor: 'undefined' stamp: ''!
file: file | filePane |
	filePane ← FilePane new file: file.
	self title: file name with: filePane inVector at: stdTemplates.
	self newframe.
	self show.
	filePane showing: file contents asParagraph.
	filePane from: filePane
! !
!CodeWindow methodsFor: 'undefined' stamp: ''!
editTitle | |
	titleframe window flash
! !
!CodeWindow methodsFor: 'undefined' stamp: ''!
class: class selector: selector para: para formerly: oldpara | codePane |
	codePane ← CodePane new class: class selector: selector para: nil.
	self title: class title + ' ' + selector with: codePane inVector at: stdTemplates.
	self newframe.
	self show.
	codePane showing: para.
	codePane formerly: oldpara.
	codePane from: codePane
! !
!CodeWindow methodsFor: 'undefined' stamp: ''!
hardcopy: p | pane |
	panes do: [:pane | pane hardcopy: p]
! !
!CodeWindow methodsFor: 'undefined' stamp: ''!
classInit | |
	stdTemplates ← (0 ⌾ 0 rect: 36 ⌾ 36) inVector
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
moveSourceTo: newFile  | len fileNo pos t5 t6 t7 |
	len ← self length.
	fileNo ← self ◦ len.
	pos ← fileNo land: 63.
	fileNo ← fileNo / 64.
	pos ← pos * 256 + (self ◦ (len - 1)).
	pos ← pos * 256 + (self ◦ (len - 2)).
	pos = 0 ifTrue: [self setSourcePosition: 0 inFile: 0].
	fileNo ← SourceFiles ◦ (fileNo + 1).
	fileNo position← pos.
	t5 ← fileNo next.
	t5 ≠ 34 ifTrue: [self setSourcePosition: 0 inFile: 0].
	self setSourcePosition: newFile position inFile: 0.
	newFile next← t5.
	t6 ← fileNo integerScan.
	newFile print: t6; append: (fileNo next: t6)

! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
printOn: strm ||
	strm append: 'a CompiledMethod for ' + self selector
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
selector | |
	^self key
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
key | |
	^self objectAt: 1
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
value | |
	^self
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
key: key | |
	^self objectAt: 1 put: key
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
symbolic | s d byte i nlits |
	self isQuick
	  ifTrue:
		[self isReturnSelf
		  ifTrue: [^'quick ^self'].
		^'quick ^field: ' + self isReturnField asString]
	  ifFalse:
		[d ← Stream default.
		d cr.
		d append: ' numArgs: '.
		d print: self numArgs.
		d cr.
		d append: ' numTemps: '.
		d print: self numTemps.
		d cr.
		d append: ' numLiterals: '.
		d print: (nlits ← self numLiterals).
		d cr.
		self frameSize > smallFrame
		  ifTrue:
			[d append: 'large frame'.
			d cr].
		self primitive > 0
		  ifTrue:
			[d append: 'primitive: '.
			d print: self primitive.
			d cr].
		d print: self literals.
		d cr.
		s ← Stream new of: self from: self initialPC to: self endPC.
		s do:
			[:byte | d append: byteNames ◦ (byte + 1).
			d append: '('.
			d append: byte base8.
			d append: ')'.
			d space.
			byte < 128
			  ifFalse:
				[byte ≥ 176
				  ifFalse:
					[byte ≤ 130
					  ifTrue:
						[ "long loads, stores"
						i ← s next.
						d append: #('inst' 'temp' 'lit' 'glob' ) ◦ (i / 64 + 1).
						d print: i \ 64.
						d space]
					  ifFalse:
						[byte ≤ 134
						  ifTrue:
							[ "long sends"
							byte even ≡ false
							  ifTrue:
								[i ← s next.
								d append: '('.
								d print: i / 32.
								d append: ')'.
								d print: i \ 32.
								d space]
							  ifFalse:
								[d append: '('.
								d print: s next.
								d append: ')'.
								d print: s next.
								d space]]
						  ifFalse:
							[(160 ≤ byte and: [byte ≤ 175])
							  ifTrue:
								[ "long jumps"
								d print: byte \ 8 - 4 * 256 + s next.
								d space]]]]]].
		^d contents]
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
numTemps | t |
	self isQuick
	  ifTrue: [^0].
	^self ◦ 3 land: 31
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
numStack | |
	self isQuick
	  ifTrue: [^0].
	 "⇑self or ⇑instvar"
	^self frameSize - self numTemps - self numArgs
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
numArgs | t1 |
	(t1 ← self flags) ≤ 4
	  ifTrue: [^self flags].
	t1 < 7
	  ifTrue: [^0].
	^((self literalAt: self numLiterals - 1) lshift: ¬8) land: 31
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
endPC | |
	(self last between: 120 and: [124])
	  ifTrue: [^self length].
	^self length - 3
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
isQuick | |
	 "Return non-false iff quick primitive (⇑self or ⇑field)"
	^self flags between: 5 and: [6]
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
primitive | |
	self flags < 7
	  ifTrue: [^0].
	 "no prim, or quick prim"
	^(self literalAt: self numLiterals - 1) land: 255 "Prim # is in next-to-last literal"
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
isReturnSelf | |
	 "Return true if code is quick ⇑self"
	^self flags = 5
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
isReturnField | |
	 "Return field # if code is quick, else false"
	self flags = 6
	  ifTrue: [^self numTemps].
	^false
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
numLiterals | |
	self isQuick
	  ifTrue: [^0].
	^self ◦ 4 / 2 land: 63
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
frameSize | |
	self ◦ 4 < 128
	  ifTrue: [^smallFrame].
	^largeFrame
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
initialPC | |
	self isQuick
	  ifTrue: [^0].
	^self numLiterals * 2 + 5
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
flags | |
	^self ◦ 3 lshift: ¬5
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
classInit | |
	 "CompiledMethod classInit."
	smallFrame ← 12.
	largeFrame ← 32 "Context range for temps+stack".
	byteNames ← #('in0' 'in1' 'in2' 'in3' 'in4' 'in5' 'in6' 'in7' 'in8' 'in9' 'in10' 'in11' 'in12' 'in13' 'in14' 'in15' 'tp0' 'tp1' 'tp2' 'tp3' 'tp4' 'tp5' 'tp6' 'tp7' 'tp8' 'tp9' 'tp10' 'tp11' 'tp12' 'tp13' 'tp14' 'tp15' 'lt0' 'lt1' 'lt2' 'lt3' 'lt4' 'lt5' 'lt6' 'lt7' 'lt8' 'lt9' 'lt10' 'lt11' 'lt12' 'lt13' 'lt14' 'lt15' 'lt16' 'lt17' 'lt18' 'lt19' 'lt20' 'lt21' 'lt22' 'lt23' 'lt24' 'lt25' 'lt26' 'lt27' 'lt28' 'lt29' 'lt30' 'lt31' 'gl0' 'gl1' 'gl2' 'gl3' 'gl4' 'gl5' 'gl6' 'gl7' 'gl8' 'gl9' 'gl10' 'gl11' 'gl12' 'gl13' 'gl14' 'gl15' 'gl16' 'gl17' 'gl18' 'gl19' 'gl20' 'gl21' 'gl22' 'gl23' 'gl24' 'gl25' 'gl26' 'gl27' 'gl28' 'gl29' 'gl30' 'gl31' 'si0' 'si1' 'si2' 'si3' 'si4' 'si5' 'si6' 'si7' 'st0' 'st1' 'st2' 'st3' 'st4' 'st5' 'st6' 'st7' 'self' 'true' 'false' 'nil' 'cNeg1' 'c0' 'c1' 'c2' 'retSelf' 'retTrue' 'retFalse' 'retNil' 'ret' 'blockRet' 'bad' 'bad' 'xLoad1' 'xSto' 'xPopSto' 'xSend' 'xxSend' 'xSuper' 'xxSuper' 'pop' 'dup' 'current' 'bad' 'bad' 'bad' 'bad' 'bad' 'bad' 'j1' 'j2' 'j3' 'j4' 'j5' 'j6' 'j7' 'j8' 'b1' 'b2' 'b3' 'b4' 'b5' 'b6' 'b7' 'b8' 'jB' 'jB' 'jB' 'jB' 'jF' 'jF' 'jF' 'jF' 'bT' 'bT' 'bT' 'bT' 'bF' 'bF' 'bF' 'bF' 'sPlus' 'sMinus' 'sLss' 'sGtr' 'sLeq' 'sGeq' 'sEqu' 'sNeq' 'sTimes' 'sOver' 'sRem' 'sMakePt' 'sLshift' 'sLxor' 'sLand' 'sLor' 'sSub' 'sSubGets' 'sLength' 'sNext' 'sNextGets' 'sEnd' 'sEq' 'sClass' 'sBCopy' 'sValue' 'sValue:' 'bad' 'sNew' 'sNew:' 'sX' 'sY' 's0.0' 's0.1' 's0.2' 's0.3' 's0.4' 's0.5' 's0.6' 's0.7' 's0.8' 's0.9' 's0.10' 's0.11' 's0.12' 's0.13' 's0.14' 's0.15' 's1.0' 's1.1' 's1.2' 's1.3' 's1.4' 's1.5' 's1.6' 's1.7' 's1.8' 's1.9' 's1.10' 's1.11' 's1.12' 's1.13' 's1.14' 's1.15' 's2.0' 's2.1' 's2.2' 's2.3' 's2.4' 's2.5' 's2.6' 's2.7' 's2.8' 's2.9' 's2.10' 's2.11' 's2.12' 's2.13' 's2.14' 's2.15' ).
	byteNames length ≠ 256
	  ifTrue: [user notify: 'not 256 byte codes']
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
literals | lit |
	 "Return vector of literals"
	^1 ~ self numLiterals transform⦂ [:lit | lit] to⦂ [(self objectAt: lit + 2)]
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
objectAt: i put: val | |<primitive: 74>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^self word: i ← val asOop]
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
objectAt: i | |<primitive: 73>
	NoteTaker
	  ifTrue: [user croak]
	  ifFalse: [^(self word: i) asObject]
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
literalAt: i put: val | |
	^self objectAt: i + 2 put: val
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
literalAt: i | |
	^self objectAt: i + 2
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
setSourcePosition: pos inFile: f | loc p1 |
	 "set last three bytes to be pos in file (0-3)"
	f > 3
	  ifTrue: [user notify: 'invalid file number']
	  ifFalse:
		[loc ← self length - 2.
		self ◦ (loc + 2) ← f * 64 + (p1 ← pos / 65536).
		p1 > 62
		  ifTrue:
			[user show: 'Source file is getting full!!'.
			user cr].
		pos ← pos - (p1 * 65536) "into two bytes".
		self ◦ (loc + 1) ← p1 ← pos / 256.
		pos ← pos land: 255 "into one byte".
		self ◦ loc ← pos]
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
putSource: text class: class category: category inFile: fileindex | file clname |
	SourceFiles≡nil ifTrue: [^self].
	file ← SourceFiles ◦ fileindex.
	file readwriteshorten.
	file settoend.
	clname ← class title.
	self setSourcePosition: file position inFile: fileindex-1.
	file append: '"' " string lengths plus $''<cr>[].<cr> ".
	file append: (text length + clname length + category length + 8) asString.
	file append: '" ' "put out length".
	file append: clname.
	file append: '$'''.
	file append: category.
	file append: ''''.
	file cr.
	file append: '['.
	file append: text.
	file append: '].'.
	file cr "put out code".
	file readonly "leave readonly for browsing and to keep clean"
! !
!CompiledMethod methodsFor: 'undefined' stamp: ''!
getSource: class | loc file pos char len str |
	 "get source text from file, or decompile"
	SourceFiles≡nil ifTrue:
		[^LADDecompiler new decompile: self selector in: class method: self].
	loc ← self length.
	file ← self ◦ loc.
	pos ← file land: 63 "high order 6 bits of pos".
	file ← file / 64 "top 2 bits are file index".
	pos ← pos * 256 + (self ◦ (loc - 1)).
	pos ← pos * 256 + (self ◦ (loc - 2)).
	pos = 0 ifTrue: [^LADDecompiler new decompile: self selector in: class method: self].
	file ← SourceFiles ◦ (file + 1) "get to correct file (should be readonly)".
	file position← pos "read source text, this should be merged with filin stuff.
		For now, it does not check matching selectors!!".
	char ← file next.
	char ≠ 34
	  ifTrue: [user notify: 'not pointing at length comment']
	  ifFalse:
		[len ← file integerScan.
		[(char ← file next) = 91] whileFalseDo:  [ "skip class and category"
			len ← len - 1 "and decrease len of code"].
		len ← len - 2 "subtract for ].<cr> after code".
		str ← String new: len.
		file into: str "get code".
		^str asParagraph makeBoldPattern]
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
printOn: strm | mclass |
	strm append: self receiver class title.
	mclass ← self mclass.
	mclass ≡ self receiver class
	  ifFalse:
		[strm append: '('.
		strm append: mclass title.
		strm append: ')'].
	strm append: '>>'.
	strm append: self method selector
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
releaseFully | | self releaseTo: nil
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
release | | self releaseTo: nil
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
tempNames | mcl code |
	mcl ← self mclass.
	code← (mcl canunderstand: self selector)
		ifTrue: [self mclass code: self selector]
		ifFalse: [(LADDecompiler new
			decompile: self selector in: mcl method: self method) asString].
	^LADParser new parseArgsAndTemps: code asStream notifying: false
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
mclass | mclass sel meth |
	mclass ← self receiver class.
	sel ← self selector.
	meth← self method.
	[mclass superclass ≡ nil or: [(mclass method: sel)≡meth]]
		whileFalseDo:  [mclass ← mclass superclass].
	^mclass
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
releaseTo: caller | c | "release frames to break cycles"
	c ← self.
	[c ≡ nil or: [c≡caller]] whileFalseDo: 
		[c erase.
		c ← c sender]
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
swapSender: coroutine | oldSender |
	oldSender ← sender.
	sender ← coroutine.
	^oldSender
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
push: val | |
	^self ◦ (stackp ← stackp + 1) ← val
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
selector | |
	^self method selector
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
sender | |
	^sender
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
pop | val |
	val ← self ◦ stackp.
	stackp ← stackp - 1.
	^val
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
sender← t1 | |
	sender ← t1.
	^sender
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
erase | i |
	 "release frames to break cycles"
	(1 to: stackp) do: [:i | self ◦ i ← nil]
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
variableNamesInto: dest with: block | n |
	self tempNames transform⦂ [:n | n] to⦂ [(dest identifier: n)]
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
stack | a strm |
	strm ← (Vector new: 20) asStream.
	strm next← a ← self.
	[(a ← a sender) ≡ nil] whileFalseDo:  [strm next← a].
	^strm contents
! !
!ContextPart methodsFor: 'undefined' stamp: ''!
tempframe | i |
	^1 ~ self method numTemps collect: [:i | self tempAt: i]
! !
!Cursor methodsFor: 'undefined' stamp: ''!
offset: t1 | |
	offset ← t1
! !
!Cursor methodsFor: 'undefined' stamp: ''!
extent | |
	^16 ⌾ 16
! !
!Cursor methodsFor: 'undefined' stamp: ''!
bits | |
	^bitstr
! !
!Cursor methodsFor: 'undefined' stamp: ''!
offset | |
	^offset
! !
!Cursor methodsFor: 'undefined' stamp: ''!
fromString: t1 | |
	bitstr ← t1.
	self fromString: bitstr offset: 0 ⌾ 0
! !
!Cursor methodsFor: 'undefined' stamp: ''!
fromtext: str | |
	self fromtext: str offset: 0 ⌾ 0
! !
!Cursor methodsFor: 'undefined' stamp: ''!
fromString: t1 offset: t2 | |
	bitstr ← t1.
	offset ← t2
! !
!Cursor methodsFor: 'undefined' stamp: ''!
asForm | |
	^Form new extent: self extent bits: bitstr offset: offset
! !
!Cursor methodsFor: 'undefined' stamp: ''!
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
!Cursor methodsFor: 'undefined' stamp: ''!
printOn: strm | i |
	strm append: 'Cursor new fromtext: '''.
	(1 to: 16) do:
		[:i | strm cr.
		bitstr ◦ i printOn: strm base: 2].
	strm append: ''' offset: '.
	strm print: offset.
	strm append: '.'
! !
!Cursor methodsFor: 'undefined' stamp: ''!
hardcopy: pf at: loc | rect |
	rect ← loc extent: 16 ⌾ 16 "print cursor image at some point location into a presssfile".
	pf setp: (pf transrect: rect) origin.
	pf bitmap: rect bits: bitstr
! !
!Cursor methodsFor: 'undefined' stamp: ''!
hardcopy: pf | |
	self hardcopy: pf at: user mp "use current cursor position" - offset
! !
!Cursor methodsFor: 'undefined' stamp: ''!
topage1 | |
	self show
! !
!Cursor methodsFor: 'undefined' stamp: ''!
showwhile⦂ expr | oldcursor value |
	oldcursor ← user currentCursor.
	self show.
	value ← expr eval.
	oldcursor show.
	^value
! !
!Cursor methodsFor: 'undefined' stamp: ''!
show | |
	user currentCursor: self
! !
!Date methodsFor: 'undefined' stamp: ''!
= date | |
	^day = date day and: [year = date year]
! !
!Date methodsFor: 'undefined' stamp: ''!
< date | |
	year = date year
	  ifTrue: [^day < date day].
	^year < date year
! !
!Date methodsFor: 'undefined' stamp: ''!
- date | |
	(date is: Date)
	  ifTrue:
		[year = date year
		  ifTrue: [^day - date day].
		^year - 1 / 4 - (date year / 4) + day + date daysleft + (year - 1 - date year * 365)]
	  ifFalse: [^self + (0 - date)]
! !
!Date methodsFor: 'undefined' stamp: ''!
+ days | t |
	days ← day + days.
	t ← Date new.
	(days > 0 and: [days < 366])
	  ifTrue:
		[t day← days "same year".
		t year← year.
		^t].
	^t day: days year: year
! !
!Date methodsFor: 'undefined' stamp: ''!
previous: di | |
	^self + (0 - (7 + self weekdayIndex "e.g. previous: 6 (Sunday) returns Date which is previous closest Sunday.
	note: di=self weekdayIndex returns self+0" - di \ 7))
! !
!Date methodsFor: 'undefined' stamp: ''!
weekdayIndex | a d |
	day ≤ (self monthday: 3)
	  ifTrue:
		[a ← year - 1.
		d ← 306]
	  ifFalse:
		[a ← year.
		d ← ¬59 - self leap] "Tuesday=1,..., Monday=7".
	^d + day + a + (a // 4) + (a // 400) - (a // 100) \\ 7 + 1
! !
!Date methodsFor: 'undefined' stamp: ''!
daysinmonth | |
	^self daysinmonth: self month
! !
!Date methodsFor: 'undefined' stamp: ''!
dayinyear | |
	^day
! !
!Date methodsFor: 'undefined' stamp: ''!
dayinmonth | |
	^day - (self monthday: self month)
! !
!Date methodsFor: 'undefined' stamp: ''!
day | |
	^day
! !
!Date methodsFor: 'undefined' stamp: ''!
leap | |
	year \ 4 = 0
	  ifTrue:
		[year \ 100 = 0
		  ifTrue:
			[year \ 400 = 0
			  ifTrue: [^1].
			^0]
		  ifFalse: [^1]]
	  ifFalse: [^0]
! !
!Date methodsFor: 'undefined' stamp: ''!
year | |
	^year
! !
!Date methodsFor: 'undefined' stamp: ''!
weekday | |
	^#(Tuesday Wednesday Thursday Friday Saturday Sunday Monday ) ◦ self weekdayIndex
! !
!Date methodsFor: 'undefined' stamp: ''!
daysleft | |
	^self daysinyear - day
! !
!Date methodsFor: 'undefined' stamp: ''!
day← t1 | |
	day ← t1
! !
!Date methodsFor: 'undefined' stamp: ''!
month | m leap |
	leap ← self leap.
	(12 to: 1 by: ¬1) do:
		[:m | #(0 31 59 90 120 151 181 212 243 273 304 334 ) ◦ m + (m > 2
		  ifTrue: [leap]
		  ifFalse: [0]) "self monthday: m" < day
		  ifTrue: [^m]].
	user notify: 'illegal month'
! !
!Date methodsFor: 'undefined' stamp: ''!
year← t1 | |
	year ← t1
! !
!Date methodsFor: 'undefined' stamp: ''!
hash | |
	^(year lshift: 3) lxor: day
! !
!Date methodsFor: 'undefined' stamp: ''!
monthname | |
	^monthnames ◦ self month
! !
!Date methodsFor: 'undefined' stamp: ''!
asSeconds | |
	 "Seconds since the beginning of time (local time)"
	^secsinday * (self - (Date new day: 1 year: 1901))
! !
!Date methodsFor: 'undefined' stamp: ''!
whichmonth: m | a |
	 "M may be a (partial) month name, or a number.  Return the month number, or false"
	(m Is: String)
	  ifTrue:
		[m ← m + '*'.
		(1 to: 12) do:
			[:a | (m match: monthnames ◦ a "first partial match")
			  ifTrue: [^a]].
		^false].
	^m ≥ 1 and: [m ≤ 12]
! !
!Date methodsFor: 'undefined' stamp: ''!
daysinyear | |
	^365 + self leap
! !
!Date methodsFor: 'undefined' stamp: ''!
monthday: m | |
	 "Return first day-in-year of m'th month"
	^#(0 31 59 90 120 151 181 212 243 273 304 334 ) ◦ m + (m > 2
	  ifTrue: [self leap]
	  ifFalse: [0])
! !
!Date methodsFor: 'undefined' stamp: ''!
daysinmonth: m | |
	^#(31 28 31 30 31 30 31 31 30 31 30 31 ) ◦ m + (m = 2
	  ifTrue: [self leap]
	  ifFalse: [0])
! !
!Date methodsFor: 'undefined' stamp: ''!
classInit | |
	monthnames ← #(January February March April May June July August September October November December ).
	secsinday ← 24 * 60 * 60
! !
!Date methodsFor: 'undefined' stamp: ''!
printOn: strm format: f | i m t5 t6 |
	"f is print format.
	1-3	positions to print day,month,year respectively
	4	character separator
	5	month format (1 month #, 2 first 3 chars, 3 entire name)
	6	year format (1 year #, 2 year # m\\ 100)"
	
	m ← self month.
	 (1 to: 3) do:
		[:i | (t5 ← f ◦ i) = 1
		  ifTrue: [day - (self monthday: m) printOn: strm]
		  ifFalse:
			[t5 = 2
			  ifTrue:
				[(t6 ← f ◦ 5) = 1
				  ifTrue: [m printOn: strm]
				  ifFalse:
					[t6 = 2
					  ifTrue: [strm append: monthnames ◦ m ◦ (1 to: 3)]
					  ifFalse: [strm append: monthnames ◦ m]]]
			  ifFalse:
				[t5 = 3
				  ifTrue: [(f ◦ 6 = 1
					  ifTrue: [year]
					  ifFalse: [year \\ 100]) printOn: strm]]].
		i < 3
		  ifTrue:
			[f ◦ 4 ≠ 0
			  ifTrue: [strm next← f ◦ 4 "separator"]]]
! !
!Date methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	self printOn: strm format: #(1 2 3 32 3 1 )
! !
!Date methodsFor: 'undefined' stamp: ''!
from: s | |
	self readfrom: s asVector "asSet" viewer format: nil
! !
!Date methodsFor: 'undefined' stamp: ''!
format: f | strm |
	strm ← Stream default.
	self printon: strm format: f.
	^strm contents
! !
!Date methodsFor: 'undefined' stamp: ''!
fromDays: d | |
	"d = days since Jan 1 1901. There are 1461 days in a 4-year cycle.
	2000 is a leap year, so no extra correction is necessary.
	day:year: will fix things up"
	d ← d asInteger divMod: 1461.
	self day: 1 + (d ◦ 2) asSmall year: 1901 + ((d ◦ 1) asSmall * 4)
! !
!Date methodsFor: 'undefined' stamp: ''!
default | |
	^user now "today" ◦ 1
! !
!Date methodsFor: 'undefined' stamp: ''!
day: t1 month: month year: t3 | |
	day ← t1.
	year ← t3.
	year < 100
	  ifTrue: [year ← 1900 + year].
	(month ← self whichmonth: month) ≡ false
	  ifTrue: [user notify: 'illegal month']
	  ifFalse:
		[(day < 1 or: [day > (self daysinmonth: month)])
		  ifTrue: [user notify: 'illegal day in month']
		  ifFalse: [day ← day + (self monthday: month)]]
! !
!Date methodsFor: 'undefined' stamp: ''!
day: t1 year: t2 | d |
	day ← t1.
	year ← t2.
	[day > (d ← self daysinyear)] whileTrueDo:
		[year ← year + 1.
		day ← day - d].
	[day ≤ 0] whileTrueDo:
		[year ← year - 1.
		day ← day + self daysinyear]
! !
!Dict methodsFor: 'undefined' stamp: ''!
list | |
	self list: '*'
! !
!Dict methodsFor: 'undefined' stamp: ''!
exists: entry | |
	^self Find: (self makeEntry: entry) "doesn't initialize too much"
! !
!Dict methodsFor: 'undefined' stamp: ''!
read: entry | |
	^self find: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
create: entry | |
	^self insert: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
contents | |
	^self match: '*'
! !
!Dict methodsFor: 'undefined' stamp: ''!
write: entry | |
	 "replace or insert"
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue: [self Replace: entry]
	  ifFalse: [self Insert: entry].
	^entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
match: entries | set |
	set ← Set new vector: 50.
	self match: entries to: set.
	^set contents
! !
!Dict methodsFor: 'undefined' stamp: ''!
replace: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue:
		[self Replace: entry.
		^entry].
	^self error: 'not replaced (not found)' entry: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
delete: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue:
		[self Delete: entry.
		^entry].
	^self error: 'not deleted (not found)' entry: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
find: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue: [^self found: entry].
	^self error: 'not found' entry: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
insert: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue: [^self error: 'not inserted (already found)' entry: entry].
	self Insert: entry.
	^entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
get: entry | |
	(self Find: (entry ← self makeEntry: entry "find or insert"))
	  ifTrue: [^self found: entry].
	self Insert: entry.
	^entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
match: entries to: strm | entry nentries |
	 "return a Set of entries which match those in entries
	(can include exact values and patterns and ranges)"
	((entries is: Vector) or: [(entries Is: Set)])
	  ifFalse: [entries ← entries inVector].
	nentries ← Set new vector: entries length.
	entries do: [:entry | nentries next← self makeEntry: entry].
	^self Match: nentries to: strm
! !
!Dict methodsFor: 'undefined' stamp: ''!
store: entry | |
	^self write: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
found: entry | |
	^self nextEntry: entry "found, fill it in from dictionary"
! !
!Dict methodsFor: 'undefined' stamp: ''!
rename: entry newName: name | nentry |
	(self Find: (nentry ← self makeEntry: name "not tested"))
	  ifTrue: [^self error: 'already exists' error: nentry].
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue:
		[self Rename: entry from: nentry.
		^entry].
	^self error: 'not found' entry: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
retrieve: entry | |
	^self find: entry "match:?"
! !
!Dict methodsFor: 'undefined' stamp: ''!
list: entries | |
	self match: entries to: user
! !
!Dict methodsFor: 'undefined' stamp: ''!
newEntry | t1 |
	^(t1 ← self entryClass new) dictionary: self.
	t1 init
! !
!Dict methodsFor: 'undefined' stamp: ''!
entryClass | |
	self subError "a subclass of DictionaryEntry"
! !
!Dict methodsFor: 'undefined' stamp: ''!
nextEntry: entry | |
	^entry "return next name and value in entry, or false.
	if insert or delete occurs after previous next, may be problem"
! !
!Dict methodsFor: 'undefined' stamp: ''!
makeEntry: entry | cl |
	 "entry or name"
	cl ← self entryClass.
	(cl ≡ false or: [(entry Is: cl)])
	  ifTrue: [^entry].
	 "entry should not be converted or is the correct type" "convert entry from a name to an entry with that name"
	^self newEntry name: entry
! !
!Dict methodsFor: 'undefined' stamp: ''!
Replace: entry | |
	self subError "entry found (next), replace it's value"
! !
!Dict methodsFor: 'undefined' stamp: ''!
Insert: entry | |
	self subError "entry not found, insert it (next)"
! !
!Dict methodsFor: 'undefined' stamp: ''!
Delete: entry | |
	self subError "entry found (next), delete it"
! !
!Dict methodsFor: 'undefined' stamp: ''!
error: e entry: entry | |
	^false "entry error: e"
! !
!Dict methodsFor: 'undefined' stamp: ''!
Find: entry | |
	^self Position← entry "is entry in dictionary?"
! !
!Dict methodsFor: 'undefined' stamp: ''!
entrySize: entry | |
	self subError "storage size of entry, constant or variable"
! !
!Dict methodsFor: 'undefined' stamp: ''!
Match: entries to: strm | entry pat ents |
	self reset "default (unordered) is to compare entire dictionary with entries".
	self do:
		[:entry | ents ← entries asStream.
		[ents and: [(pat ← ents next)]] whileTrueDo:
			[(pat match: entry)
			  ifTrue:
				[ents ← false.
				strm next← entry]]]
! !
!Dict methodsFor: 'undefined' stamp: ''!
Rename: entry from: nentry | |
	self Delete: entry.
	self Insert: (entry name: nentry name)
! !
!Dict methodsFor: 'undefined' stamp: ''!
reopen | |
	self open "reinitialize, especially if a File is involved"
! !
!Dict methodsFor: 'undefined' stamp: ''!
close | |
	self obsolete
	  ifFalse:
		[ "possible cleanup before a release"
		self file
		  ifTrue: [self file close].
		self release]
! !
!Dict methodsFor: 'undefined' stamp: ''!
open | |
	
! !
!Dict methodsFor: 'undefined' stamp: ''!
release | |
	 "obsolete and deallocate storage, especially if connected to an external view,
	e.g. a File"
	self file
	  ifTrue: [self file release]
! !
!Dict methodsFor: 'undefined' stamp: ''!
obsolete | |
	self file "is my information obsolete (should I regenerate it)?"
	  ifTrue: [^self file obsolete].
	^false
! !
!Dict methodsFor: 'undefined' stamp: ''!
file | |
	^false "return my file"
! !
!Dict methodsFor: 'undefined' stamp: ''!
init: initialSize | |
	 "default is to ignore"
! !
!Dict methodsFor: 'undefined' stamp: ''!
replace: name with: value | |
	^self replace: (self newEntry name: name value: value)
! !
!Dict methodsFor: 'undefined' stamp: ''!
insert: name with: value | |
	^self insert: (self newEntry name: name value: value)
! !
!Dict methodsFor: 'undefined' stamp: ''!
lookup: name | |
	^self ◦ name
! !
!Dict methodsFor: 'undefined' stamp: ''!
◦ name | entry |
	(entry ← self find: name "find")
	  ifTrue: [^entry value].
	^false
! !
!Dict methodsFor: 'undefined' stamp: ''!
◦ name ← value | |
	^self write: (self newEntry "replace or insert" name: name value: value)
! !
!Dict methodsFor: 'undefined' stamp: ''!
position | |
	self subError "current position (name)"
! !
!Dict methodsFor: 'undefined' stamp: ''!
reset | |
	self subError "position to beginning"
! !
!Dict methodsFor: 'undefined' stamp: ''!
asStream | |
	 "leave position where it is"
! !
!Dict methodsFor: 'undefined' stamp: ''!
next | |
	^self nextEntry: self newEntry "return next entry or false"
! !
!Dict methodsFor: 'undefined' stamp: ''!
append: dict | entry |
	dict do: [:entry | self write: entry]
! !
!Dict methodsFor: 'undefined' stamp: ''!
Position← entry | |
	^self position← entry name "position to name, or position to insert place and return false if not found.
	subclass had better define position← or Position← (preferably)
	otherwise circularity results!!!"
! !
!Dict methodsFor: 'undefined' stamp: ''!
position← name | |
	^self Position← self makeEntry: name
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
init: size | |
	values ← Vector new: size.
	super init: size
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
copyfrom: dict | |
	self objects← dict objects copy.
	values ← dict values copy
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
clean | name |
	 "release unreferenced entries"
	self do:
		[:name |  "slick, huh"
		(self ◦ name) refct = 1
		  ifTrue: [self delete: name]]
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
insert: name with: value | |
	self insert: name.
	values ◦ (self findorerror: name) ← value
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
insertall: names with: vals | i |
	 "insert many entries"
	(1 to: names length) do: [:i | self insert: names ◦ i with: vals ◦ i]
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
insertall: names | |
	 "default value is nil"
	self insertall: names with: (Vector new: names length)
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
with: names values: vals | i |
	(1 to: names length) do: [:i | self insert: names ◦ i with: vals ◦ i]
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
tally: name | x |
	(x ← self find: name)
	  ifTrue: [^values ◦ x ← values ◦ x + 1].
	self insert: name with: 1.
	^1
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
delete: name | |
	(name is: Vector)
	  ifTrue: [super delete: name]
	  ifFalse:
		[values ◦ (self findorerror: name) ← nil.
		super delete: name]
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
invert: obj | i |
	(1 to: values length) do:
		[:i | values ◦ i = obj
		  ifTrue: [^objects ◦ i]].
	^false
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
invert | |
	^self invertto: (Dictionary new init: objects length)
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
invertto: dict | i |
	(1 to: objects length) do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [dict insert: values ◦ i with: objects ◦ i]].
	^dict
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
asInvertedVector | s i v |
	 "in form ((value, object), ...)"
	s ← (Vector new: objects length) asStream.
	(1 to: objects length) do:
		[:i | objects ◦ i ≡ nil
		  ifFalse:
			[v ← Vector new: 2.
			v ◦ 1 ← values ◦ i.
			v ◦ 2 ← objects ◦ i.
			s next← v]].
	^s contents
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
rehash | i copy |
	copy ← Dictionary new init: self size "create a copy".
	(1 to: objects length) "hash each entry into it" do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [copy insert: objects ◦ i with: values ◦ i]].
	self copyfrom: copy
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
swap: i with: j | |
	values swap: i with: j.
	super swap: i with: j
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
values | |
	^values
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
growto: size | name copy |
	copy ← self class new init: size "create a copy of the new size".
	self do: [:name | copy insert: name with: self ◦ name].
	self copyfrom: copy "hash each entry into it"
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
lookup: name | x |
	(x ← self find: name)
	  ifTrue: [^values ◦ x].
	^false
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
◦ name | |
	^values ◦ (self findorerror: name)
! !
!Dictionary methodsFor: 'undefined' stamp: ''!
◦ name ← val | |
	^values ◦ (self findorerror: name) ← val
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
fileSize | |
	self subError "size in characters for filing"
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
readFrom: file | |
	self subError "inverse of storeOn:"
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
storeOn: file | |
	self subError "store self as fileSize characters on file"
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
name: name | |
	
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
dictionary: dict | |
	
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
init | |
	
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
match: entry | |
	self subError "does self (some kind of pattern) match entry?"
! !
!DictionaryEntry methodsFor: 'undefined' stamp: ''!
dictionary | |
	^false "what dictionary did I come from?"
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
text: t1 | |
	text ← t1
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
text | |
	^text
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
frame | |
	^text frame
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
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
!Dispframe methodsFor: 'undefined' stamp: ''!
prompt | |
	self cr.
	self next← prompt.
	self show
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
ev | t |
	[self cr.
	t ← self request: ''] whileTrueDo:
		[self space.
		self print: nil ⓢ t].
	^false
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
request: s | |
	 "false if ctrl-d, all input since prompt if "
	self append: s.
	^self read
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
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
!Dispframe methodsFor: 'undefined' stamp: ''!
outline | |
	text window outline: 2
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
clear | |
	self reset.
	self show
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
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
!Dispframe methodsFor: 'undefined' stamp: ''!
moveto: pt | |
	(text window inset: ¬2 ⌾ ¬2) dragto: pt - (¬2 ⌾ ¬2)
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
frame← r | |
	text para: nil frame: r
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
init | |
	text ← Textframe new.
	self of: (String new: 16)
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
classInit | |
	prompt ← '' ◦ 1.
	doit ← '' ◦ 1
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
rect: r | |
	self init.
	self frame← r.
	self clear
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
firsttime | |
	(text window has: user mp)
	  ifTrue:
		[self outline.
		self prompt]
	  ifFalse: [^false]
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
leave | |
	
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
lasttime | |
	self last = prompt
	  ifTrue:
		[self skip: ¬2.
		self show].
	^user bluebug ≡ false
! !
!Dispframe methodsFor: 'undefined' stamp: ''!
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
!Document methodsFor: 'undefined' stamp: ''!
bubbleinsert: image | delta i k |
	i ← self findindex: image "insert image into the document keeping the document y-sorted and adding images extent y to all subimages below it.".
	self insertI: i value: image.
	delta ← image extent y.
	(i + 1 to: self length) do: [:k | self ◦ k translate: 0 ⌾ delta]
! !
!Document methodsFor: 'undefined' stamp: ''!
findindex: image | y guess top bottom |
	position = 0 "binary search on the origins of the rectangles surrounding my subimages
		returns the index of the subimage just below image."
	  ifTrue: [^1].
	top ← 1.
	bottom ← position.
	y ← image rectangle origin y.
	y ≤ (self ◦ 1) rectangle origin y
	  ifTrue: [^1].
	.
	y ≥ (self ◦ position) rectangle origin y
	  ifTrue: [^position + 1].
	guess ← position / 2.
	[bottom = (top + 1)] whileFalseDo: 
		[(self ◦ guess) rectangle origin y ≥ y
		  ifTrue: [bottom ← guess]
		  ifFalse: [top ← guess].
		guess ← bottom + top / 2].
	^bottom
! !
!Document methodsFor: 'undefined' stamp: ''!
insert: image | i |
	i ← self findindex: image "insert image into the document keeping the document y-sorted.".
	self insertI: i value: image
! !
!Document methodsFor: 'undefined' stamp: ''!
bubbledelete: image | delta i k |
	i ← self find: image "delete image from the document  and subtracting images extent y from all subimages below it.".
	i
	  ifTrue:
		[self deleteI: i.
		delta ← image extent y.
		(i to: self length) do: [:k | self ◦ k translate: 0 ⌾ (0 - delta)]]
! !
!Document methodsFor: 'undefined' stamp: ''!
resize | delta t |
	 "make sure the document does not have subimages that have negative y values and resize the document"
	position ≥ 1
	  ifTrue:
		[(delta ← (self ◦ 1) top) ≤ 0
		  ifTrue: [self do: [:t | t translate: 0 ⌾ (0 - delta)]]].
	super resize
! !
!Document methodsFor: 'undefined' stamp: ''!
edit | |
	DocumentEditor new "Documents are edited with a DocumentEditor" init: self
! !
!Document methodsFor: 'undefined' stamp: ''!
delete: image | i |
	i ← self find: image "delete image from the document and leave its space. ".
	i
	  ifTrue:
		[self deleteI: i]
! !
!Document methodsFor: 'undefined' stamp: ''!
name | |
	^displayorder "returns the name of the document ( displayorder is currently used for name... note that name is a string."
! !
!Document methodsFor: 'undefined' stamp: ''!
classInit | |
	micasperinch ← 2540
! !
!Document methodsFor: 'undefined' stamp: ''!
name: t1 | |
	displayorder ← t1 "sets the name of the document ( displayorder is currently used for name... note that name is a string."
! !
!Document methodsFor: 'undefined' stamp: ''!
hardcopy | p i press bottoms rect pressscale pageheight pagewidth lastrect currentrect oldytop oldybottom |
	oldytop ← 11 * micasperinch.
	pageheight ← 11 * micasperinch.
	pagewidth ← 8 * micasperinch.
	user displayoffwhile⦂ [
		(press ← dp0 pressfile: displayorder + '.doc'.
		pressscale ← press scale.
		self hidePress: press complete: ¬1.
		p ← PressPrinter init.
		p press: press.
		p frame← 1 * micasperinch "in micas" ⌾ (1 * micasperinch) rect: pagewidth - micasperinch ⌾ (pageheight - micasperinch).
		lastrect ← (self ◦ 1) rectangle * pressscale.
		rect ← lastrect leftside ⌾ (1 * micasperinch) rect: lastrect rightside ⌾ (pageheight - lastrect top).
		oldybottom ← p print: self ◦ 1 in: rect.
		(2 to: self length) do:
			[:i | oldybottom > oldytop
			  ifTrue:
				[oldytop ← pageheight - micasperinch "page break"].
			currentrect ← (self ◦ i) rectangle * pressscale.
			currentrect top > lastrect bottom
			  ifTrue: [oldytop ← oldybottom + (lastrect bottom - currentrect top) "no overlap"]
			  ifFalse: [oldytop ← oldytop + (lastrect top - currentrect top) "overlap"].
			rect ← currentrect leftside ⌾ (1 * micasperinch) rect: currentrect rightside ⌾ oldytop.
			oldybottom ← p print: self ◦ i in: rect.
			lastrect ← (self ◦ i) rectangle * pressscale].
		press close.
		press toPrinter "send over ethernet to printer")]
! !
!Document methodsFor: 'undefined' stamp: ''!
copy | im i |
	im ← Document new origin: origin copy rectangle: rectangle copy path: path copy form: form copy figure: figure copy ground: ground copy xgrid: xgrid copy ygrid: ygrid copy.
	(1 to: self length) do: [:i | im add: (self ◦ i) copy].
	im name: self name copy.
	^im
! !
!Document methodsFor: 'undefined' stamp: ''!
pressCode | |
	^0
! !
!Document methodsFor: 'undefined' stamp: ''!
hideData: complete | s |
	s ← Stream new "stores an instance of class Document from a press file" of: (String new: 100).
	s nextword← self length "number of subimages".
	s nextPoint← origin.
	s nextPoint← rectangle origin.
	s nextPoint← rectangle corner.
	s nextword← xgrid.
	s nextword← ygrid.
	s nextString← displayorder.
	^s contents
! !
!Document methodsFor: 'undefined' stamp: ''!
fromPress: press value: s | numberofsubimages t t1 i code |
	numberofsubimages ← s nextword "builds an instance of class Document from a press file".
	origin ← s nextPoint.
	t ← s nextPoint.
	t1 ← s nextPoint.
	rectangle ← t rect: t1.
	xgrid ← s nextword.
	ygrid ← s nextword.
	displayorder ← s nextString.
	(1 to: numberofsubimages) do:
		[:i | s ← press nextControl asStream.
		code ← s next.
		t ← (code = 1
				  ifTrue: [Image new]
				  ifFalse:
					[code = 2
					  ifTrue: [Heading new]
					  ifFalse:
						[code = 3
						  ifTrue: [BitImage new]
						  ifFalse: [false]]]).
		(t and: [code = t pressCode])
		  ifTrue: [self addimage: (t fromPress: press value: s)]
		  ifFalse: [user notify: 'illegal code or code mismatch']].
	^self
! !
!Document methodsFor: 'undefined' stamp: ''!
fromPress: t1 | press s |
	displayorder ← t1.
	user displayoffwhile⦂ [
		 "retrieves an instance of class Document from a press file"
		(self default.
		press ← dp0 pressfile: displayorder.
		press open.
		s ← press nextControl asStream.
		s next = self pressCode
		  ifTrue: [self fromPress: press value: s]
		  ifFalse:
			[user notify: 'error in pressfile'])]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
close | |
	XeqCursor show.
	document close.
	document ← nil.
	screenimage ← Vector new: 0.
	NormalCursor show
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
enter | |
	selection ← false.
	self show
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
leave | |
	document ≡ nil
	  ifFalse:
		[selection
		  ifTrue: [selection highlite].
		self update.
		self buildscreenimage.
		selection ← false]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
redbug | pt rect newrect start t |
	pt ← user mp.
	start ← pt.
	rect ← newrect ← Rectangle new origin: start corner: start.
	selection
	  ifTrue:
		[selection highlite.
		self deselect.
		selection ← false].
	[user anybug] whileTrueDo:
		[rect ← newrect.
		rect comp.
		t ← user mp.
		newrect ← Rectangle new origin: (start min: t) corner: (start max: t).
		rect comp].
	rect width < 10
	  ifTrue:
		[selection ← screenimage smallestsubimageat: pt - screenimage origin.
		selection
		  ifTrue:
			[selection translate: screenimage origin.
			selection edit: screenimage.
			selection translate: 0 ⌾ 0 - screenimage origin.
			selection ← false]]
	  ifFalse:
		[rect origin← screenimage griddedpoint: rect origin.
		selection ← screenimage subimageswithin: (rect translate: 0 ⌾ 0 - screenimage origin).
		selection
		  ifTrue:
			[selection translate: screenimage origin.
			selection highlite]]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
kbd | c x y |
	c ← user kbd.
	c = 120
	  ifTrue:
		[user clearshow: 'x gridding is '.
		document xgrid print.
		user cr.
		document xgrid: (x ← (user request: 'x gridding . . . ') asInteger).
		screenimage xgrid: x]
	  ifFalse:
		[c = 121
		  ifTrue:
			[user clearshow: 'y gridding is '.
			document ygrid print.
			user cr.
			document ygrid: (y ← (user request: 'y gridding . . . ') asInteger).
			screenimage ygrid: y]]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
print | |
	document hardcopy "write a press file and hardcopy this document"
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
hardcopy | |
	self leave "write a press file and hardcopy this document".
	self top.
	document hardcopy
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
yellowbug | pt t2 |
	(t2 ← documentmenu bug) = 1
	  ifTrue: [self move]
	  ifFalse:
		[t2 = 2
		  ifTrue: [self delete]
		  ifFalse:
			[t2 = 3
			  ifTrue: [self place]
			  ifFalse:
				[t2 = 4
				  ifTrue: [self cut]
				  ifFalse:
					[t2 = 5
					  ifTrue: [self paste]
					  ifFalse:
						[t2 = 6
						  ifTrue: [self copyselection]
						  ifFalse:
							[t2 = 7
							  ifTrue: [self top]
							  ifFalse:
								[t2 = 8
								  ifTrue: [self bottom]
								  ifFalse:
									[t2 = 9
									  ifTrue: [self jump]
									  ifFalse:
										[t2 = 10
										  ifTrue: [self addspace]
										  ifFalse:
											[t2 = 11
											  ifTrue: [self deletespace]
											  ifFalse:
												[t2 = 12
												  ifTrue:
													[self deselect.
													selection ← false.
													self show]]]]]]]]]]]]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
closeScrap | |
	scrap ≡ nil
	  ifFalse: [scrap close]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
top | i delta |
	selection
	  ifTrue:
		[ "translate the current selection to the top of the window and update the document to reflect any changes in the subimages which are scrolled out of the screenimage."
		delta ← selection rectangle origin - screenimage rectangle origin.
		self update.
		selection ← false.
		documentwindow translate: 0 ⌾ delta y "move window on document".
		self buildscreenimage.
		self show "reconstruct screen image, including reestablishing  first and last indices"]
	  ifFalse:
		[self update.
		documentwindow ← Rectangle new origin: document rectangle origin extent: document width ⌾ self frame height.
		self buildscreenimage.
		self show]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
bottom | i delta |
	selection
	  ifTrue:
		[ "translate the current selection to the bottom of the window and update the document to reflect any changes in the subimages which are scrolled out of the screenimage."
		delta ← selection rectangle corner - screenimage rectangle corner.
		self update.
		selection ← false.
		documentwindow translate: 0 ⌾ delta y "move window on document".
		self buildscreenimage.
		self show "reconstruct screen image, including reestablishing  first and last indices"]
	  ifFalse:
		[self update.
		documentwindow ← Rectangle new origin: document rectangle corner - self frame height extent: document width ⌾ self frame height.
		self buildscreenimage.
		self show]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
delete | t |
	selection
	  ifTrue:
		[ "delete subimage (paragraph) from the screenimage and save it in the scrap"
		XeqCursor show.
		self closeScrap.
		scrap ← selection.
		selection highlite.
		selection display: 3.
		selection do: [:t | screenimage delete: t].
		selection ← false.
		NormalCursor show]
	  ifFalse: [frame flash]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
deletespace | image i k r delta |
	self update "delete whitespace from the document  .".
	selection ← false.
	r ← document rectanglefromuser.
	i ← document indexofsubimagebelow: r top - screenimage top + documentwindow top.
	i
	  ifTrue:
		[delta ← r height.
		(i to: document length) do: [:k | document ◦ k translate: 0 ⌾ (0 - delta)].
		document resize].
	self buildscreenimage.
	self show
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
paste | pt t |
	 "add the subimage (paragraph) in the scrap to the screenimage."
	selection
	  ifTrue: [selection highlite].
	self update.
	selection ← scrap copy.
	OriginCursor showwhile⦂ [
		(user waitbug.
		[user nobug] whileFalseDo: 
			[pt ← screenimage mp + screenimage rectangle origin.
			selection translateto: pt.
			selection blink].
		selection displayat: 0 ⌾ 0 effect: 1 clippedBy: self frame)].
	self deselect.
	selection do:
		[:t | document bubbleinsert: (t translate: documentwindow origin)].
	selection ← false.
	self buildscreenimage.
	self show
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
cut | t |
	selection
	  ifTrue:
		[ "delete subimage (paragraph) from the screenimage and save it in the scrap"
		XeqCursor show.
		self closeScrap.
		scrap ← selection.
		self update.
		selection do:
			[:t | document bubbledelete: t].
		selection ← false.
		self buildscreenimage.
		self show.
		NormalCursor show]
	  ifFalse: [frame flash]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
update | i |
	XeqCursor topage1 "update the document to reflect any changes in the subimages .".
	selection
	  ifTrue: [self deselect].
	document deleteI: firstindex to: lastindex "update document".
	(1 to: screenimage length) do:
		[:i | document insert: (screenimage ◦ i translate: 0 ⌾ documentwindow origin y)].
	document resize.
	NormalCursor topage1
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
copy | |
	self copyselection "copy the selection and put it in scrap"
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
addspace | image i k r delta |
	self update "add whitespace to the document  .".
	selection ← false.
	r ← document rectanglefromuser.
	i ← document indexofsubimagebelow: r top - screenimage top + documentwindow top.
	i
	  ifTrue:
		[delta ← r height.
		(i to: document length) do: [:k | document ◦ k translate: 0 ⌾ delta].
		document resize].
	self buildscreenimage.
	self show
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
jump | y deltay yprime deltayprime rect pt newY scal r |
	user waitnobug.
	XeqCursor show.
	self update.
	y ← document height.
	yprime ← frame height.
	deltay ← documentwindow origin y - document origin y.
	scal ← yprime asFloat / y.
	deltayprime ← (scal * deltay) asInteger.
	pt ← screenimage leftside + (1.0 - scal * frame width / 2) ⌾ (screenimage top + deltayprime).
	document quickDisplayAt: 0 ⌾ 0 scale: scal offset: frame minX + (1.0 - scal * frame width / 2) ⌾ frame minY.
	rect ← 0 ⌾ 0 rect: 1 ⌾ 1.
	rect origin← pt.
	rect corner x← pt x + (scal * frame width) asInteger.
	rect corner y← pt y + (scal * frame height) asInteger.
	user cursorloc← pt.
	OriginCursor show.
	user waitbug.
	user redbug
	  ifTrue:
		[rect comp.
		[user redbug] whileTrueDo:
			[r ← rect copy.
			newY ← user mp y.
			newY < (frame minY - rect height)
			  ifTrue: [newY ← frame minY - rect height].
			newY > frame maxY
			  ifTrue: [newY ← frame maxY].
			rect translateto: pt x ⌾ newY.
			r comp.
			rect comp].
		rect comp.
		XeqCursor show.
		deltayprime ← newY - frame origin y.
		deltay ← y * deltayprime / yprime.
		documentwindow translateto: 0 ⌾ deltay.
		selection ← false].
	self buildscreenimage.
	self show.
	NormalCursor show
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
copyselection | |
	selection
	  ifTrue:
		[ "copy the selection and put it in scrap"
		XeqCursor show.
		self closeScrap.
		scrap ← selection copy.
		NormalCursor show]
	  ifFalse: [frame flash]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
move | pt t |
	selection
	  ifTrue:
		[ "used to place subimages (paragraphs) in the Image."
		user waitnobug.
		OriginCursor show.
		user waitbug.
		selection highlite.
		selection displayat: 0 ⌾ 0 effect: 3 clippedBy: frame.
		[user redbug] whileTrueDo:
			[pt ← screenimage mp + screenimage rectangle origin.
			selection translateto: pt.
			selection blink].
		XeqCursor show.
		selection displayat: 0 ⌾ 0 effect: 1 clippedBy: frame.
		self deselect.
		selection ← false.
		NormalCursor show]
	  ifFalse: [frame flash]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
place | pt tempimage t |
	 "add the image in the scrap to the screenimage."
	selection
	  ifTrue: [selection highlite].
	self deselect.
	selection ← scrap copy.
	OriginCursor showwhile⦂ [
		(user waitbug.
		[user nobug] whileFalseDo: 
			[pt ← screenimage mp + screenimage rectangle origin.
			selection translateto: pt.
			selection blink])].
	selection displayat: 0 ⌾ 0 effect: 1 clippedBy: self frame.
	self deselect.
	selection do:
		[:t | screenimage add: t].
	selection ← false
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
deselect | t |
	selection
	  ifTrue:
		[selection translate: 0 ⌾ 0 - screenimage origin.
		selection do: [:t | t translate: selection origin]]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
editTitle | |
	titlepara ← document name asParagraph.
	super editTitle.
	document name: titlepara text
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
newframe | |
	self update.
	self fixedwidthfromuser: document width.
	self buildscreenimage.
	self show
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
title | |
	^document name
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
show | |
	XeqCursor show.
	self outline.
	growing
	  ifFalse:
		[titleframe put: (Paragraph new text: self title runs: titlerun alignment: 0) at: frame origin + titleloc.
		titleframe window outline.
		titleframe show.
		screenimage white.
		screenimage displayat: 0 ⌾ 0 effect: 1 clippedBy: self frame.
		selection
		  ifTrue: [selection boxcomp]
		  ifFalse:
			[.
			NormalCursor show]]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
classInit | |
	 "  DocumentEditor classInit.    "
	documentmenu ← Menu new string: 'move
erase
place
cut
paste
copy
top
bottom
jump
addspace
deletespace
show
'.
	jumpcursor ← Cursor new fromtext: '
1111111111111111
1111111111111111
0000000000000000
0000000000000000
0000000000000000
0000000000000000
0000001110000000
0000011111000000
0000011111000000
0000001110000000
0000000000000000
0000000000000000
0000000000000000
0000000000000000
1111111111111111
1111111111111111' offset: 2 ⌾ 1
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
defaultdocument: name | defaultdocument run r textimage f im dot heading head text char b image row |
	defaultdocument ← Document new " name is a string" origin: 0 ⌾ 0 extent: user screenrect extent.
	defaultdocument name: name.
	defaultdocument xgrid: DefaultTextStyle tab.
	defaultdocument ygrid: DefaultTextStyle lineheight.
	textimage ← BorderedText new.
	textimage text: 'Text that is bordered' width: 200.
	defaultdocument addform: textimage andpath: 0 ⌾ 0.
	textimage ← TextImage new.
	textimage text: 'This is a paragraph' width: 600.
	defaultdocument addform: textimage andpath: 0 ⌾ 0.
	b ← BitImage new fromrectangle: (Rectangle new origin: 0 ⌾ 200 extent: 100 ⌾ 100).
	defaultdocument insert: b.
	head ← Set new default.
	text ← 'HEADING'.
	text do: [:char | head add: char].
	heading ← Heading new origin: 0 ⌾ 400 index: 9 charactercodes: head currentcharacter: 0.
	defaultdocument insert: heading "defaultdocument insert: CurveIdiom new init.".
	self init: defaultdocument
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
defaultdocument | |
	self defaultdocument: 'document'
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
buildscreenimage | i r delta |
	screenimage ← Image new "This function copies the subimages intersecting the document window
		into the screen image." origin: self frame origin extent: self frame extent.
	screenimage xgrid: document xgrid.
	screenimage ygrid: document ygrid.
	delta ← documentwindow origin y.
	firstindex ← 1 " find the index of the first subimage that intersects the document window.".
	[firstindex ≥ document length or: [(document ◦ firstindex) rectangle bottom > documentwindow top]] whileFalseDo:  [firstindex ← firstindex + 1].
	lastindex ← firstindex.
	(firstindex to: document length) do:
		[:i | (document ◦ i) rectangle top < documentwindow bottom
		  ifTrue:
			[lastindex ← i.
			screenimage add: (document ◦ i translate: 0 ⌾ 0 "copy" - (0 ⌾ delta))]
		  ifFalse: [^lastindex]]
! !
!DocumentEditor methodsFor: 'undefined' stamp: ''!
init: t1 | i |
	document ← t1.
	self fixedwidthfromuser: document width "This is the paragraph (subimage) level document editor.".
	documentwindow ← Rectangle new origin: document rectangle origin extent: document width ⌾ self frame height.
	self buildscreenimage.
	selection ← false.
	user topWindow leave.
	self takeCursor.
	self enter.
	user restartup: self
! !
!EFTPSender methodsFor: 'undefined' stamp: ''!
net: n host: h | |
	super net: n host: h soc: 16 "Each instance of an EFTPSender has a unique lclSocket, but
		always goes to socket 020 of the receiver; unlike plain sockets, we only want acks from this dest.".
	filterInput ← true.
	self retransmit: 5 every: 180.
	outPac ← false.
	transaction ← 0.
	ackType ← 25
! !
!EFTPSender methodsFor: 'undefined' stamp: ''!
process: packet | error |
	 "The printer is trying to tell me something"
	packet pupType = 27
	  ifTrue:
		[error ← packet dataString "error 33!!!".
		self freePacket: packet.
		user show: 'remote server aborted: '.
		user show: error ◦ (3 to: error length).
		abortTransfer ← true]
! !
!Error methodsFor: 'undefined' stamp: ''!
notify: anObject | |
	anObject notify: self message
! !
!Error methodsFor: 'undefined' stamp: ''!
message | |
	(errorString Is: String)
	  ifTrue: [^errorString].
	^'ERROR'
! !
!Error methodsFor: 'undefined' stamp: ''!
notify | |
	self notify: user
! !
!Error methodsFor: 'undefined' stamp: ''!
message: aString | |
	errorString ← aString
! !
!Error methodsFor: 'undefined' stamp: ''!
printOn: aStream | |
	aStream append: 'Error: '.
	aStream append: self message
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
packet | |
	^page
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
pupType← p | |
	^page pupType← p
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
pupType | |
	^page pupType
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
header: n ← v | |
	^page word: (12 + n) ← v
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
header: n | |
	^page word: 12 + n "for accessing information after pup header, e.g. file commands and parameters.
	n = 1 to (self headerLength-24)/2"
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
trailerLength | |
	^2 "checksum"
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
length | |
	^page pupLength - (self headerLength - 2)
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
headerLength | |
	^44 "ethernet encap.(4), pup header(20), file label (20=default)"
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
dataString | |
	^page pupString
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
length: len | |
	page pupLength← len + self headerLength - 2
! !
!EtherFilePage methodsFor: 'undefined' stamp: ''!
init | |
	page ≡ nil
	  ifTrue: [page ← file allocatePage "self page:"].
	self length: 0
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
classInit | |
	Smalltalk declare: #(E ) "if this needs to be filed in again, execute this first
	Smalltalk declare: ↪EtherPool as: (SymbolTable new init: 32).

access variables from outside with (for example) with EtherPool◦↪ethAwake".
	EtherPool declare: #(ethInPacNext checkIncomingCS IntProcLevel InputProcLevel ethIntBits etherState ethAwake ethAsleep ethDead ) as: {false , false , 14 , 13 , 16 , 0 , 3 , 1 , 0}.
	EtherPool declare: #(NETNUM ALTONUM freeQ justArrivedQ sockeTable routingTable routingHopCount routingUpdateUser IntProc InputProc broadcastFilter IntLight InputLight OutputLight )
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
etherStart | |
	 "allows ether to start running again"
	etherState = ethAwake "makes sure the interrupt is on, and kicks the device"
	  ifTrue:
		[mem ◦ 385 = 0
		  ifTrue: [mem ◦ 385 ← ethIntBits].
		self SIO: 3 "forces it to wake up again"]
	  ifFalse:
		[.
		self notify: 'Attempt to etherStart when not awake!!.']
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
etherStop | |
	 "temporarily shuts off the ether stuff"
	Top critical⦂ [
		(mem ◦ 385 ← 0.
		self SIO: 3.
		mem ◦ 384 ← 0)]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
sleep | socket |
	 "be sure to do this before a user quit"
	etherState = ethDead
	  ifTrue: [self Init]
	  ifFalse:
		[ "that is, go from dead to asleep"
		etherState = ethAsleep
		  ifFalse:
			[ "already asleep"
			etherState = ethAwake
			  ifTrue:
				[sockeTable values "try to shut down gracefully" do:
					[:socket | socket ≡ nil
					  ifFalse: [socket sleep] "warn the sockets, leaves them in table"].
				self etherStop.
				etherState ← ethAsleep "when next we wake up, may be on a new machine/net"]
			  ifFalse:
				[ "when next we wake up, may be on a new machine/net"
				]]]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
Init | i |
	 "move from state ethDead to ethAsleep"
	 "if we were already running, bring it all down, just in case!!"
	etherState = ethDead
	  ifFalse: [self kill] "now sure we are ethDead".
	NETNUM ← ALTONUM ← 0 "may get reset later".
	self setLights.
	(justArrivedQ ← SafeQ new of: (Vector new: 20)) enable.
	freeQ
	  ifTrue:
		[(freeQ ← SafeQ new of: (Vector new: 20)) enable.
		(1 to: 10) do: [:i | freeQ next← Pacbuf init]]
	  ifFalse: [justArrivedQ disable].
	ethInPacNext ← self freePacket.
	sockeTable ← Dictionary new init: 10.
	routingTable ← String new: 255.
	routingTable all← 0 "1-255, 0 is special".
	routingHopCount ← String new: 255.
	routingHopCount all← 8.
	routingUpdateUser ← RoutingUpdater init.
	self installIntProc.
	self installInputProc.
	IntProc enable.
	InputProc enable.
	etherState ← ethAsleep "we are still asleep, must do a wakeup to get numbers, start, etc."
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
setLights | |
	IntLight ← Rectangle new origin: 576 ⌾ 0 extent: 16 ⌾ 16.
	InputLight ← Rectangle new origin: 592 ⌾ 0 extent: 16 ⌾ 16.
	OutputLight ← Rectangle new origin: 576 ⌾ 16 extent: 16 ⌾ 16
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
kill | socket |
	 "shuts down ethernet and PUP world completely"
	etherState = ethDead "Should free up all of the storage, etc.....
		Would need to wakeup or Init, to get started again.
		Device may have been running"
	  ifFalse:
		[ "do nothing"
		etherState = ethAwake
		  ifTrue: [self sleep].
		sockeTable values do:
			[:socket | socket ≡ nil
			  ifFalse: [socket kill]].
		Top terminate: IntProcLevel.
		Top terminate: InputProcLevel.
		ethInPacNext
		  ifTrue:
			[ethInPacNext ≡ nil
			  ifFalse:
				[ethInPacNext locked
				  ifTrue: [ethInPacNext unlock]]].
		ethInPacNext ← false "Release the PQueues to avoid circular data structures".
		(freeQ and: [freeQ ≠ nil])
		  ifTrue:
			[freeQ release.
			freeQ ← nil].
		justArrivedQ ≠ nil
		  ifTrue:
			[justArrivedQ release.
			justArrivedQ ← nil].
		routingUpdateUser ≡ nil
		  ifFalse: [routingUpdateUser release].
		routingUpdateUser ← routingTable ← routingHopCount ← nil.
		etherState ← ethDead]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
wakeup | socket |
	 "Try to get everything up and running"
	etherState = ethAwake
	  ifTrue: [self etherStart]
	  ifFalse:
		[ "do nothing, kick the receiver"
		.
		etherState = ethDead
		  ifTrue: [self sleep].
		etherState = ethAsleep
		  ifTrue:
			[ALTONUM ← self getMachineID "this is the tricky one, need to get our machine # and routing table.
		may have come up on a different network and host, assume the worst".
			self setMachineID: ALTONUM.
			NETNUM ← 0.
			sockeTable values do:
				[:socket | socket ≡ nil
				  ifFalse: [socket setOutAddBlock]].
			etherState ← ethAwake.
			self etherStart.
			routingUpdateUser update.
			NETNUM = 0
			  ifTrue:
				[etherState ← ethAsleep.
				user notify: 'no routing tables']
			  ifFalse:
				[ "tell leftover sockets current net&host, and that we are awake again"
				sockeTable values do:
					[:socket | socket ≡ nil
					  ifFalse:
						[socket setOutAddBlock.
						socket wakeup]]]]
		  ifFalse:
			[.
			self notify: 'In wakeup, found Ethernet in some unknown state.']]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
installInputProc | inBuf destSoc |
	InputProc ← Top install⦂ [([true] whileTrueDo:
					[InputLight comp "infinite loop for process in scheduler".
					[inBuf ← justArrivedQ next] whileTrueDo:
						[(checkIncomingCS and: [inBuf checksumOK "process each incoming buffer, know it's a PUP" "verify the incoming checksum" ≡ false])
						  ifTrue: [self freePacket: inBuf "reject it, done"]
						  ifFalse:
							[ "To be honest, we should check the destNet and destHost,
				but they generally have to be OK.....
				OK to pass the packet on"
							(destSoc ← sockeTable lookup: inBuf destSocNum)
							  ifTrue: [destSoc acceptPacbuf: inBuf]
							  ifFalse:
								[ "couldn't find a socket for it, done"
								.
								self freePacket: inBuf]]].
					InputLight comp.
					InputProc sleep "last action in the loop"])] at: InputProcLevel
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
installIntProc | |
	IntProc ← Top install⦂ [([true] whileTrueDo:
					[IntLight comp "infinite loop for process in scheduler.
			Interrupt just happened, running at a high level, interface off.
			Something just happened, do the common cases first.
			Input is wired down below; only comes here if OK.
			Note: we can only come here if last action was to start the rec!!" "copy out the packet first".
					ethInPacNext
					  ifTrue: [self copyinput: ethInPacNext pupString]
					  ifFalse:
						[ "user cr; show: 'warning, no packet pre-fetched. tell John'."
						(ethInPacNext ← self freePacket)
						  ifTrue: [self copyinput: ethInPacNext pupString] "user cr; show: 'input lost'"] "start the receiver".
					self SIO: 2.
					ethInPacNext
					  ifTrue:
						[justArrivedQ next← ethInPacNext "now process this input".
						ethInPacNext ← self freePacket.
						Top wakeup: InputProcLevel "all done"].
					IntLight comp.
					IntProc sleep "last action in the loop"])] at: IntProcLevel
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
copyinput: string | |<primitive: 89>
	user croak
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
sendOutput: ethOutPac | post |
	 "This is the one and only place from which we  send output.
			Only one packet gets passed in to us at a time.
			For performance, we wait here for the transmitter to post!!!!
			Nominally, we are running at level 0;  thus, this must be run
			at a Top critical, to protect from multiple calls."
	etherState ≠ ethAwake
	  ifTrue:
		[self wakeup.
		user show: 'starting Ethernet...'].
	Top critical⦂ [
		OutputLight comp.
		(post ← self doOutput: ethOutPac pupString words: ethOutPac totLengthWords) ≠ 511
		  ifTrue:
			[user cr.
			user show: 'Warning, bad output post: ' + post base8].
		OutputLight comp] "end of the critical part"
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
doOutput: string words: words | |<primitive: 91>
	user croak
	
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
broadcastFilterSet: val | |<primitive: 90>
	user croak
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
freePacket: p | |
	 "put a used packet into free queue"
	(freeQ and: [p])
	  ifTrue: [freeQ next← p].
	^false
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
freePacket | p |
	freeQ
	  ifTrue:
		[ "get a packet"
		(p ← freeQ next)
		  ifTrue: [^p].
		user show: 'Warning, empty freeQ, in Etherworld'.
		^false]
	  ifFalse: [^Pacbuf new init]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
broadcastFilter: val | |
	val
	  ifTrue:
		[broadcastFilter ← true.
		self broadcastFilterSet: 1]
	  ifFalse:
		[broadcastFilter ← false.
		self broadcastFilterSet: 0]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
awake | |
	^etherState = ethAwake
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
printOn: s | |
	etherState = ethDead
	  ifTrue: [s append: 'Etherworld,  etherState = ethDead.']
	  ifFalse:
		[s append: 'Etherworld running on '.
		s print: NETNUM.
		s append: '#' + ALTONUM base8 + '#'.
		s cr.
		freeQ
		  ifTrue:
			[s print: freeQ length.
			s append: ' Pacbufs in freeQ']
		  ifFalse: [s append: 'no freeQ'].
		s cr.
		s append: 'etherState = '.
		etherState = ethAsleep
		  ifTrue: [s append: 'etherAsleep']
		  ifFalse:
			[etherState = ethAwake
			  ifTrue: [s append: 'etherAwake']
			  ifFalse: [s print: etherState]]]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
printRoutingTable | i |
	(1 to: 255) do:
		[:i | routingTable ◦ i ≠ 0
		  ifTrue:
			[user cr.
			user show: 'To net ' + i asString + ' via host ' + (routingTable ◦ i) asString + ', hop count = ' + (routingHopCount ◦ i) asString]].
	user cr
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
error: str | |
	user cr.
	user show: str
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
SIO: sioArg | | <primitive: 87>
	user croak
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
printSocketTable | i |
	sockeTable ≡ nil
	  ifTrue:
		[user cr.
		user show: 'no socketTable']
	  ifFalse: [sockeTable objects do:
			[:i | i ≡ nil
			  ifFalse:
				[user cr.
				user print: i.
				user show: ', '.
				user print: sockeTable ◦ i]]]
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
setMachineID: ID | |
	mem ◦ 392 ← ID
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
getMachineID | |
	^(self SIO: 0) \ 256
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
notify: strng | |
	 "turn off the Ethernet before doing a user notify"
	self etherStop.
	user show: ' Etherworld stopped'.
	Top currentPriority ≠ 1
	  ifTrue:
		[user cr.
		user show: 'priority is ' + Top currentPriority asString].
	user notify: strng
! !
!Etherworld methodsFor: 'undefined' stamp: ''!
fill | outstanding |
	 "I want to replenish the freeQ"
	(freeQ ≡ false or: [freeQ ≡ nil])
	  ifFalse:
		[outstanding ← Pacbuf howMany - freeQ length.
		user cr.
		user show: outstanding asString + ' packets outstanding'.
		[freeQ length = 10] whileFalseDo:  [freeQ next← Pacbuf init]]
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
do⦂ pCode onTrapDo⦂ pTrapCode | |
	 "...executes the set of statements in the block pCode and, conditionally, the set of statements in the block pTrapCode.  If 'trap' or 'trap:' is invoked during the execution of pCode, the execution is interrupted and pTrapCode is started.  When pTrapCode completes, pCode is either continued, restarted, or aborted, depending on which of 'continue', 'restart', and 'abort' is sent during the execution of pTrapCode.  A notify window is created if 'trap' or 'trap:' is sent when pCode is not active, or if 'do⦂onTrapDo⦂' is sent again before the first invocation completes."
	fStatusIndicator ≠ nil
	  ifTrue:
		[user notify: 'ExceptionHandler is active'] "1".
	fStatusIndicator ← 1 "2".
	fCode ← pCode "3".
	fTrapCode ← pTrapCode "4".
	fDoContext ← thisContext "5".
	fTrapCondition ← nil "6".
	fResult ← true "7".
	fCode eval "8".
	self release "9".
	^fResult "10" "
1. If fStatusIndicator ≠ nil, then the previous invocation of 'do⦂onTrapDo⦂' has not completed properly.  Create a notify window in this case.
2. Set fStatusIndicator to indicate pCode is active.
3. Save pCode for execution and restart.
4. Save pTrapCode for later execution.
5. Save the context of 'do⦂onTrapDo⦂' for 'restart' and 'abort'.
6. Initialize the trap condition to nil.
7. Initialize the result of 'do⦂onTrapDo⦂ ' to true.
8. Execute pCode.
9. Set fields to nil to release saved contexts.
10. Return fResult (which may have been modified during the execution of pCode or pTrapCode).
"
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
implementationNotes | |
	 "
FIELDS
fDoContext : a Context that controls the execution of the message 'do⦂onTrapDo⦂'.
fCode : a Context that controls the execution of the statements passed as the first parameter to 'do⦂onTrapDo⦂'.
fTrapCode : a Context that controls the execution of the statements passed as the second parameter to 'do⦂onTrapDo⦂'.
fTrapCondition : the object passed as the parameter to 'trap:'.
fResult : the object that is the result of 'do⦂onTrapDo⦂'.
fPostTrapIndicator : an Integer specifying the action to be taken after fTrapCode is executed.
fStatusIndicator : an Integer specifying the status of 'do⦂onTrapDo⦂'; nil means 'do⦂onTrapDo⦂' has not been invoked, 1 means fCode is active, 0 means fTrapCode is active.
"
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
description | |
	 "
This class provides some exception handling capabilities that can be used in a variety of applications.  It is intended to be used in the following way:

1. An ExceptionHandler object is created to supervise the execution of a block of statements.
	EH ← ExceptionHandler new.
2. Each object that is capable of detecting an exception is informed of the existence of the ExceptionHandler object.
	dpj exceptionHandler: EH.
3. The ExceptionHandler object is then sent the message 'do⦂ pCode onTrapDo⦂ pTrapCode'.  The parameter pCode is a block of statements that is to be supervised by the ExceptionHandler object.  The parameter pTrapCode is a block of statements that is to be executed whenever an exception is detected.
	EH do⦂ [ ... ] onTrapDo⦂ [ ... ].
4. When an exception is detected during the execution of one of the statements in pCode the message 'trap' can be sent to the ExceptionHandler object which will then take control.  If information about the exception is available, it can be relayed by sending the message 'trap:' with the information specified as the parameter.
	EH trap.  (or)  EH trap: information.
5. When the ExceptionHandler object gets control it saves the parameter if one was sent, then executes pTrapCode.  The saved parameter can be retrieved by sending the message 'trapCondition'.  The statements in pTrapCode should take any actions necessary to handle the exception. Messages can be included in pTrapCode that tell the ExceptionHandler object what to do when pTrapCode completes.  The available messages are:
	5.1 'restart' which restarts the execution of pCode,
	5.2 'continue' which continues pCode at the point at which 'trap' was sent,
	5.3 'abort' which aborts the execution of pCode and returns control to the statement following 'do⦂ pCode onTrapDo⦂ pTrapCode'.
	EH restart.  (or)  EH continue.  (or)  EH abort.
6. The message 'do⦂onTrapDo⦂' returns a result which can be specified during the execution of either  pCode or pTrapCode by the message 'result ← pResult'.  This result value can be retrieved anytime after it has been specified by sending the message 'result'.
	EH result ← true.
	[ EH result ⇒ [ ... ] ].
7. If pCode is terminated abnormally from a user notify window, the message 'release' should be sent to return the ExceptionHandler object to its initial state.
	EH release.
8. Special error checking is included in the ExceptionHandler object to detect calls to pTrapCode during the execution of pTrapCode and other invalid conditions.
"
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
loopTest | guard i |
	 "ExceptionHandler new loopTest."
	guard ← ExceptionHandler new.
	user clear.
	i ← 0.
	guard do⦂ [
		(user show: i asString.
		i ← i + 1.
		guard trap: i.
		user show: ' end')] onTrapDo⦂ [
		(i < 10
		  ifTrue: [guard restart]
		  ifFalse: [guard continue])].
	user show: ' done.'.
	user cr.
	^guard result
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
juniperTest | e i max f |
	 "ExceptionHandler new juniperTest."
	e ← ExceptionHandler new.
	i ← 0.
	max ← 5.
	dpj exceptionHandler: e.
	dpj open.
	f ← dpj file: 'test.test'.
	e do⦂ [
		(f reset.
		[f end] whileFalseDo: 
			[user show: f next inString])] onTrapDo⦂ [
		(user cr.
		user show: 'Juniper not responding'.
		f release.
		dpj release.
		i ← i + 1.
		i < max
		  ifTrue:
			[e restart]
		  ifFalse: [e abort])].
	user cr.
	user show: 'done'.
	f close.
	dpj close
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
trapCondition | |
	 "...returns the condition that was passed as the parameter to 'trap:'."
	^fTrapCondition
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
result← pResult | |
	 "...sets the result value of 'do⦂onTrapDo⦂' to pResult."
	fResult ← pResult
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
result | |
	 "...returns the result set by 'result←'."
	^fResult
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
release | |
	 "...resets the exception handler to its initial condition."
	fCode ← nil.
	fTrapCode ← nil.
	fDoContext ← nil.
	fStatusIndicator ← nil
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
abortCode | tDoContextCaller |
	 "...releases the context chain and returns fResult to the caller of 'do⦂onTrapDo⦂'."
	tDoContextCaller ← fDoContext caller "1".
	thisContext caller releaseTo: tDoContextCaller "2".
	self release "3".
	tDoContextCaller push: fResult "4".
	Top run: tDoContextCaller at: Top currentPriority "5" "
1. Get the caller of 'do⦂onTrapDo⦂'.
2. Release the context chain from the caller of thisContext up to the caller of 'do⦂onTrapDo⦂'.
3. Set fields to nil to clean up pointers to unwanted contexts.
4. Return fResult to the caller of 'do⦂onTrapDo⦂'.
5. Run the caller of 'do⦂onTrapDo⦂'.
"
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
restartCode | |
	 "...releases the context chain and restarts the block of statements passed as the first parameter to 'do⦂onTrapDo⦂'."
	thisContext caller releaseTo: fCode "1".
	fCode restart "2".
	fCode push: fDoContext "3".
	Top run: fCode at: Top currentPriority "4" "
1. Release the context chain from the caller of thisContext up to fCode.
2. Resets the pc and stack pointer of fCode.
3. Push the return context of fCode onto its tempframe.
4. Run fCode.
"
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
continue | |
	 "...specifies that the code interrupted by 'trap' or 'trap:' will be continued when the trap code completes."
	fPostTrapIndicator ← 2
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
abort | |
	 "...specifies that the code interrupted by 'trap' or 'trap:' will be aborted when the trap code completes.  Control will return to the statement that follows the invocation of 'do⦂onTrapDo⦂'."
	fPostTrapIndicator ← 1
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
restart | |
	 "...specifies that the code interrupted by 'trap' or 'trap:' will be restarted when the trap code completes."
	fPostTrapIndicator ← 0
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
testTransaction | e i max f |
	 "ExceptionHandler new testTransaction."
	e ← ExceptionHandler new.
	i ← 0.
	max ← 5.
	dpj exceptionHandler: e.
	dpj open.
	f ← dpj file: 'test.test'.
	e do⦂ [
		(f reset.
		[f end] whileFalseDo: 
			[user show: f next inString])] onTrapDo⦂ [
		(user cr.
		user show: 'Juniper not responding'.
		f release.
		dpj release.
		i ← i + 1.
		i < max
		  ifTrue:
			[e restart]
		  ifFalse: [e abort])].
	user cr.
	user show: 'done'.
	f close.
	dpj close
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
test | guard i |
	 "ExceptionHandler new test."
	guard ← ExceptionHandler new.
	user clear.
	i ← 0.
	guard do⦂ [
		(user show: i asString.
		i ← i + 1.
		guard trap: i.
		user show: ' end')] onTrapDo⦂ [
		(i < 10
		  ifTrue: [guard restart]
		  ifFalse: [guard continue])].
	user show: ' done.'.
	user cr.
	^guard result
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
trap: pTrapCondition | tTrapCode i |
	 "...interrupts the execution of pCode and begins pTrapCode (from 'do⦂ pCode onTrapDo⦂ pTrapCode').  The trap condition is set to pTrapCondition."
	fStatusIndicator ≡ nil
	  ifTrue: [user notify: 'ExceptionHandler is not active']
	  ifFalse:
		[ "1"
		fStatusIndicator = 0
		  ifTrue: [user notify: 'ExceptionHandler trap code is already active'] "2"].
	fStatusIndicator ← 0 "3".
	fTrapCondition ← pTrapCondition "4".
	fPostTrapIndicator ← 0 "5".
	tTrapCode ← fTrapCode cleancopy "6".
	tTrapCode eval "7".
	(1 to: tTrapCode totalPT) "8" do:
		[:i | fTrapCode setPT: i to: (tTrapCode getPT: i)].
	fStatusIndicator ← 1 "9".
	fPostTrapIndicator = 0 "10"
	  ifTrue:
		[self restartCode]
	  ifFalse:
		[fPostTrapIndicator = 1
		  ifTrue:
			[self abortCode]
		  ifFalse:
			[fPostTrapIndicator = 2]] "
1. If fStatusIndicator ≡ nil, then 'do⦂onTrapDo⦂' has not been invoked and there is no trap code to execute.
2. if fStatusIndicator = 0, then fTrapCode is active.  This is an invalid state.
3. Set fStatusIndicator to indicate fTrapCode is active.
4. Set the trap condition to pTrapCondition.
5. Set the default post trap action to 'restart'.
6. Make a new copy of the context, fTrapCode.
7. Execute the new copy of fTrapCode.
8. Set the parameters and temporaries in the context, fTrapCode to those of the executed copy.  This is done so that fCode will see any changes made to these variables by fTrapCode.
9. Set fStatusIndicator to indicate fTrapCode is not active.
10. Select the terminating action from the value of fPostTrapIndicator.
"
! !
!ExceptionHandler methodsFor: 'undefined' stamp: ''!
trap | |
	 "...interrupts the execution of pCode and begins pTrapCode (from 'do⦂ pCode onTrapDo⦂ pTrapCode')."
	self trap: nil
! !
!FieldNameCollector methodsFor: 'undefined' stamp: ''!
next← value | |
	user notify: 'Invalid field name: ' + value asString
! !
!FieldNameCollector methodsFor: 'undefined' stamp: ''!
rightparen | |
	self next← ')' "just for error message"
! !
!FieldNameCollector methodsFor: 'undefined' stamp: ''!
leftparen | |
	self next← '(' "just for error message"
! !
!FieldNameCollector methodsFor: 'undefined' stamp: ''!
identifier: s | |
	sink next← s
! !
!File methodsFor: 'undefined' stamp: ''!
reopen | |
	self sameFile
	  ifFalse: [ "init and directory access"
		directory get: self init].
	self open
! !
!File methodsFor: 'undefined' stamp: ''!
Find: page | |
	^page pageNumber ≤ lastpn
! !
!File methodsFor: 'undefined' stamp: ''!
release | |
	
! !
!File methodsFor: 'undefined' stamp: ''!
open | |
	self findLastPage "compute lastpn"
! !
!File methodsFor: 'undefined' stamp: ''!
found: page | |
	self subError "read an existing page"
! !
!File methodsFor: 'undefined' stamp: ''!
entryClass | |
	self subError "a subclass of FilePage"
! !
!File methodsFor: 'undefined' stamp: ''!
makeEntry: page | t2 |
	(page is: self entryClass)
	  ifTrue:
		[page init.
		page serialNumber: serialNumber.
		^page].
	^(t2 ← self entryClass new) dictionary: self.
	t2 init.
	t2 pageNumber: page.
	t2 serialNumber: serialNumber
! !
!File methodsFor: 'undefined' stamp: ''!
close | |
	
! !
!File methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm append: name
! !
!File methodsFor: 'undefined' stamp: ''!
name | |
	^name
! !
!File methodsFor: 'undefined' stamp: ''!
name: t1 | |
	name ← t1
! !
!File methodsFor: 'undefined' stamp: ''!
dictionary: t1 | |
	directory ← t1
! !
!File methodsFor: 'undefined' stamp: ''!
dictionary | |
	^directory
! !
!File methodsFor: 'undefined' stamp: ''!
match: entry | |
	^self name match: entry name
! !
!File methodsFor: 'undefined' stamp: ''!
init | |
	lastpn ← false.
	error ← nullString.
	serialNumber ← String new: 4
! !
!File methodsFor: 'undefined' stamp: ''!
help | |
	 "

A common way to access a File is through a FileStream.
	to create a FileStream on either an old or new file:
		<FileStream> ← <FileDirectory> file: <String>. (see also oldFile: and newFile:)
	e.g. f ← dp0 file: 'test'.

	The default access mode (readwriteshorten) allows you to read or write, and
	automatically shorten a File (to its current position) upon closing).  If you want to
	only read a file, readonly mode is faster and safer.

Some common ways to access a FileStream (see Stream and FileStream):
	reading a character (an Integer between 0 and 255)
		next, ◦
	reading a String of characters
		upto:	, next:, nextString, contents
	reading other kinds of objects
		nextword, word:, nextNumber:, nextParagraph

	writing characters
		next←, ◦←
	writing a String of characters
		append:, nextString←
	writing other kinds of objects
		nextword, word:←, print:

	finding position
		position, wordposition, length, end, positionSize:

	changing position (besides reading/writing)
		position←, skip:, skipTo:, reset, settoend, wordposition←, position:size:

When finished with a FileStream, <FileStream> close.

For information about using or creating other views of file organizations (Btree, file-based object dictionaries, Findit), about WFS and Juniper files, and general file problems, see Steve Weyer.
"
! !
!File methodsFor: 'undefined' stamp: ''!
lastPage: t1 | |
	 "for those who know what they're doing"
	lastpn ← t1
! !
!File methodsFor: 'undefined' stamp: ''!
findLastPage | |
	^lastpn ← self pageFrom: self length "the default definitions for findLastPage and length are circular.
	at least one of them must be defined by a subclass"
! !
!File methodsFor: 'undefined' stamp: ''!
pageFrom: len | |
	^(len - 1 "compute page number for a character index" / self entryClass new dataLength) asSmall + 1
! !
!File methodsFor: 'undefined' stamp: ''!
lastPage | |
	lastpn
	  ifTrue: [ "length in pages"
		^lastpn].
	^self findLastPage
! !
!File methodsFor: 'undefined' stamp: ''!
endFile: page | |
	self subError "make File end with this FilePage. false means delete all of File"
! !
!File methodsFor: 'undefined' stamp: ''!
lastFullPage | |
	(self read: self lastPage) full
	  ifTrue: [^lastpn].
	^lastpn - 1
! !
!File methodsFor: 'undefined' stamp: ''!
length | page |
	page ← self read: self lastPage "length in characters" "defeat ST76 optimization".
	.
	^lastpn - 1 * page dataLength + page length
! !
!File methodsFor: 'undefined' stamp: ''!
type | |
	^type
! !
!File methodsFor: 'undefined' stamp: ''!
delete | |
	^directory delete: self
! !
!File methodsFor: 'undefined' stamp: ''!
directory: t1 | |
	directory ← t1
! !
!File methodsFor: 'undefined' stamp: ''!
directory | |
	^directory
! !
!File methodsFor: 'undefined' stamp: ''!
type: t1 | |
	 "used by different Files in different ways, e.g. read/write mode"
	type ← t1
! !
!File methodsFor: 'undefined' stamp: ''!
rename: newName | |
	^directory rename: self newName: newName
! !
!File methodsFor: 'undefined' stamp: ''!
error: e | t2 |
	e
	  ifTrue:
		[e ← ((t2 ← Stream default) append: name.
				t2 append: ' in '.
				t2 append: e.
				t2 append: ', '.
				t2 append: error.
				t2 contents).
		error ← nullString.
		^super error: e].
	^false
! !
!File methodsFor: 'undefined' stamp: ''!
Read: page | |
	self subError "return page or false"
! !
!File methodsFor: 'undefined' stamp: ''!
newPage: pn | |
	^self makeEntry: pn
! !
!File methodsFor: 'undefined' stamp: ''!
newPage | |
	^self makeEntry: 0
! !
!File methodsFor: 'undefined' stamp: ''!
Write: page | |
	self subError "update lastpn, write page and return result (maybe next page)"
! !
!File methodsFor: 'undefined' stamp: ''!
doCommand: com page: page error: s | |
	self subError "execute a File command on page. if an error occurs, include
	error ← 'some error message'.
	⇑self error: s
	if s is false, returns false.
	otherwise s is passed to an error routine"
! !
!File methodsFor: 'undefined' stamp: ''!
read: pn | |
	^self Read: (self makeEntry: pn)
! !
!File methodsFor: 'undefined' stamp: ''!
Get: page | p pn |
	pn ← page pageNumber.
	(p ← self Read: page)
	  ifTrue: [^p].
	 "current last page of the file is assumed full"
	(lastpn + 1 to: pn - 1) "return an empty last page which is not written yet" do:
		[:p | page pageNumber: p.
		page ← self Write: page].
	page pageNumber: pn.
	page length: 0.
	^page
! !
!File methodsFor: 'undefined' stamp: ''!
error | |
	^error
! !
!File methodsFor: 'undefined' stamp: ''!
get: pn | |
	^self Get: (self makeEntry: pn)
! !
!File methodsFor: 'undefined' stamp: ''!
asStream | |
	^FileStream new on: (self open.
	self get: 1)
! !
!File methodsFor: 'undefined' stamp: ''!
sameFile | |
	 "is File's current internal representation the same as what is stored externally? if so, usually can avoid some initialization, directory lookup"
	^false
! !
!File methodsFor: 'undefined' stamp: ''!
classInit | |
	FilePool declare: #(read write shorten ) as: #(1 2 4 ) "subclasses of File may want to share variables in pools.
	execute before filin:
		Smalltalk declare: ↪XFilePool as: (SymbolTable new init: 16).
	in classInit: XFilePool declare: ↪() as: ↪()"
! !
!File methodsFor: 'undefined' stamp: ''!
serialNumber: s | |
	(s is: String) "stored as a String of 4 characters rather than as various Numbers"
	  ifTrue: [serialNumber ← s]
	  ifFalse:
		[(s is: Substring)
		  ifTrue: [serialNumber ← s copy]
		  ifFalse:
			[(s Is: Integer)
			  ifTrue:
				[serialNumber word: 1 ← 0.
				serialNumber word: 2 ← s]
			  ifFalse:
				[ "Vector of Integers"
				serialNumber word: 1 ← s ◦ 1.
				serialNumber word: 2 ← s ◦ 2]]]
! !
!File methodsFor: 'undefined' stamp: ''!
serialNumber | |
	^serialNumber
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm append: (self obsolete
	  ifTrue: ['a closed ']
	  ifFalse: ['an open ']).
	strm append: self class title.
	strm append: ' on '.
	self server printOn: strm
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
close | |
	self obsolete
	  ifFalse:
		[externalViews delete: self.
		self release]
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
checkName: s | |
	(s empty "default behavior is to get rid of ending period.
	subclasses can do any kind of checking they want and
	return false if name is no good" or: [s last ≠ ('.' ◦ 1)])
	  ifTrue: [^s].
	^s copy: 1 to: s length - 1
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
error: e entry: file | |
	^file error: e
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
Find: file | name |
	(name ← self checkName: file name)
	  ifTrue:
		[file name: name.
		^self Position← file].
	file error: 'illegal name'
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
insert: file | old |
	file ← self makeEntry: file "note: this changes the default behavior found in Dict.
	this creates a new version rather than generating an error if the name exists".
	self versionNumbers
	  ifTrue: [file ← self makeEntry: (file name "ignore explicit version and directory will create a next version" asStream upto: '!' ◦ 1)]
	  ifFalse:
		[(self Find: file)
		  ifTrue:
			[old ← self makeEntry: file name + '$' "otherwise, if the file already exists,
		rename it to name$, deleting that file first if it exists".
			(self Find: old)
			  ifTrue: [self Delete: old].
			self rename: file name newName: old name "reposition to original name".
			(self Find: file)
			  ifTrue: [self error: 'insert/rename ??' entry: file]] "file didn't exist"].
	self Insert: file.
	^file
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
open | |
	externalViews insert: self
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
entryClass | |
	self subError "a subclass of File"
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
dictionary: t1 | |
	directory ← t1
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
dictionary | |
	^directory
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
asFtpDirectory | ftp |
	(ftp ← FtpDirectory new "to allow convenient (kludgey) access to file servers (e.g. phylum, dpj) via Ftp") server: self server.
	ftp open.
	ftp userName empty
	  ifTrue: [ftp login: self userName password: self userPassword].
	^ftp
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
server | |
	^directory
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
store: s1 as: s2 | f t4 |
	(s1 is: FileStream)
	  ifTrue: [f ← s1]
	  ifFalse:
		[(dp0 exists: s1)
		  ifTrue: [f ← dp0 oldFile: s1]
		  ifFalse: [^false]].
	f readonly.
	(t4 ← self file: s2) append: f.
	t4 close.
	f close
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
retrieve: s1 as: s2 | f t4 |
	(self exists: s1)
	  ifTrue: [f ← self oldFile: s1]
	  ifFalse: [^false].
	f readonly.
	(t4 ← ((s2 is: FileStream)
			  ifTrue: [s2]
			  ifFalse: [dp0 file: s2])) append: f.
	t4 close.
	f close
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
userPassword | |
	^currentProfile ≡ nil
	  ifTrue: ['']
	  ifFalse: [currentProfile userPassword: self server]
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
userName | |
	^currentProfile ≡ nil
	  ifTrue: ['']
	  ifFalse: [currentProfile userName: self server]
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
login: name password: pw | |
	self subError
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
login: name | |
	^self login: name password: '' "or prompt?"
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
store: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self store: t as: t]]
	  ifFalse: [^self store: s as: s]
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
server: t1 | |
	directory ← t1
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
retrieve: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self retrieve: t as: t]]
	  ifFalse: [^self retrieve: s as: s]
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
versionNumbers | |
	^false "generally, version numbers are not supported"
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
directory | |
	^directory
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
newPage | |
	^(self makeEntry: nullString) "return a dummy FilePage from a dummy File" newPage
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
allocateSN: file | |
	self subError "allocate a new serial number for a File"
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
filin: s format: ft | |
	user displayoffwhile⦂ [
		 "read Class definitions or Changes from FileStreams or PressFiles
	ft: 1 (FileStream=Bravo), 2 (Press)"
		((s is: Vector)
		  ifTrue: [s do: [:s | self filin: s format: ft]]
		  ifFalse:
			[ "special case for Alto and patterns"
			((s is: String) and: [((s has: '*' ◦ 1) or: [(s has: '#' ◦ 1)])])
			  ifTrue: [self filin: (self filesMatching: s) format: ft]
			  ifFalse:
				[(s is: UniqueString)
				  ifTrue: [s ← s +  "Class name"
							(ft = 1
							  ifTrue: ['.st']
							  ifFalse: ['.press'])].
				(ft = 1
				  ifTrue: [self oldFile: s]
				  ifFalse: [self pressfile: s]) filin]])]
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
filin: s | |
	self filin: s format: 1
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
pressfile: name | |
	^PressFile new of: (self file: name)
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
oldFile: name | |
	^(self find: name) asStream
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
newFile: name | |
	^(self insert: name) asStream
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
file: name | |
	^(self get: name) asStream
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
pressfilin: s | |
	self filin: s format: 2
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
directory: t1 | |
	directory ← t1
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
exceptionHandler: eh | |
	 "default is to do nothing"
! !
!FileDirectory methodsFor: 'undefined' stamp: ''!
closeTransaction | |
	 "default is to do nothing"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
reopen | |
	file reopen.
	file makeEntry: self "self may have been released"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
asStream | |
	^self asStream: Stream new
! !
!FilePage methodsFor: 'undefined' stamp: ''!
◦ i | |
	^page ◦ (self checkIndex: i)
! !
!FilePage methodsFor: 'undefined' stamp: ''!
◦ i ← v | |
	^page ◦ (self checkIndex: i) ← v
! !
!FilePage methodsFor: 'undefined' stamp: ''!
init | |
	page ≡ nil
	  ifTrue: [page ← String new: self pageLength "self page:"].
	self length: 0 "not sure who depends on this besides FileStream read:"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
name: sp | |
	self init.
	self serialNumber: sp ◦ 1.
	self pageNumber: sp ◦ 2
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dictionary: t1 | |
	file ← t1
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dictionary | |
	^file
! !
!FilePage methodsFor: 'undefined' stamp: ''!
read: pn | |
	self pageNumber: pn "recycle self".
	self length: 0.
	^file Read: self
! !
!FilePage methodsFor: 'undefined' stamp: ''!
endFile | |
	^file endFile: self
! !
!FilePage methodsFor: 'undefined' stamp: ''!
get: pn | |
	self pageNumber: pn "recycle self".
	self length: 0.
	^file Get: self
! !
!FilePage methodsFor: 'undefined' stamp: ''!
write | |
	^file Write: self "some files, e.g. AltoFile, will return a last empty page instead of a full one"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
file | |
	^file
! !
!FilePage methodsFor: 'undefined' stamp: ''!
doCommand: com error: s | |
	^file doCommand: com page: self error: s
! !
!FilePage methodsFor: 'undefined' stamp: ''!
file: t1 | |
	file ← t1
! !
!FilePage methodsFor: 'undefined' stamp: ''!
page: t1 | |
	page ← t1
! !
!FilePage methodsFor: 'undefined' stamp: ''!
pageNumber: pn | |
	self subError
! !
!FilePage methodsFor: 'undefined' stamp: ''!
header: n | |
	^page word: n "return n-th header word"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
pageNumber | |
	self subError
! !
!FilePage methodsFor: 'undefined' stamp: ''!
serialNumber: sn | |
	self subError
! !
!FilePage methodsFor: 'undefined' stamp: ''!
serialNumber | |
	^file serialNumber
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dataEnd: pos | |
	self length: pos - self headerLength
! !
!FilePage methodsFor: 'undefined' stamp: ''!
address: a | |
	self subError
! !
!FilePage methodsFor: 'undefined' stamp: ''!
word: i ← v | |
	^page word: (self headerLength "no bounds checking" / 2 + i) ← v
! !
!FilePage methodsFor: 'undefined' stamp: ''!
lastPage | |
	^self pageNumber "is this last page in file?" ≥ file lastPage
! !
!FilePage methodsFor: 'undefined' stamp: ''!
length: len | |
	self subError
! !
!FilePage methodsFor: 'undefined' stamp: ''!
address | |
	self subError "page address, e.g. on a disk"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dataEnd | |
	^self headerLength "logical end of data in page" + self length
! !
!FilePage methodsFor: 'undefined' stamp: ''!
checkIndex: i | |
	(i > 0 and: [i ≤ self length])
	  ifTrue: [^i + self headerLength].
	self error: 'illegal index'
! !
!FilePage methodsFor: 'undefined' stamp: ''!
full | |
	^self length = self dataLength
! !
!FilePage methodsFor: 'undefined' stamp: ''!
word: i | |
	^page word: self headerLength "no bounds checking" / 2 + i
! !
!FilePage methodsFor: 'undefined' stamp: ''!
length | |
	self subError "logical length of data in page"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
header: n ← v | |
	^page word: n ← v "set and return n-th header word"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dataString | |
	^page
! !
!FilePage methodsFor: 'undefined' stamp: ''!
trailerLength | |
	^0 "length of stuff after data ends in page"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
pageLength | |
	^self headerLength "physical size of page" + self dataLength + self trailerLength
! !
!FilePage methodsFor: 'undefined' stamp: ''!
page | |
	^page
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dataMaxEnd | |
	^self headerLength "physical end of data in page" + self dataLength
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dataBeginning | |
	^self headerLength
! !
!FilePage methodsFor: 'undefined' stamp: ''!
headerLength | |
	^0 "length of stuff before data begins in page"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
asStream: s | offset |
	offset ← self headerLength.
	^s of: self dataString from: offset + 1 to: offset + self length "self dataEnd"
! !
!FilePage methodsFor: 'undefined' stamp: ''!
dataLength | |
	^512 "physical length of data in page. default"
! !
!FilePane methodsFor: 'undefined' stamp: ''!
file: t1 | |
	file ← t1
! !
!FilePane methodsFor: 'undefined' stamp: ''!
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
!FilePane methodsFor: 'undefined' stamp: ''!
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
!FileStream methodsFor: 'undefined' stamp: ''!
readonly | |
	self setMode: read
! !
!FileStream methodsFor: 'undefined' stamp: ''!
setMode: m | |
	rwmode = m
	  ifFalse:
		[ "don't flush if first time or not write mode or continuing write mode"
		(rwmode ≡ nil or: [((rwmode nomask: write) or: [(m anymask: write)])])
		  ifFalse: [self flush].
		rwmode ← m]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
writing | |
	rwmode ≡ nil
	  ifTrue: [^self readwriteshorten "default mode. true"].
	^(rwmode land: write) = write
! !
!FileStream methodsFor: 'undefined' stamp: ''!
readwrite | |
	self setMode: read + write "allow read and write but don't automatically shorten"
! !
!FileStream methodsFor: 'undefined' stamp: ''!
readwriteshorten | |
	self setMode: read + write "allow read and write and shorten File upon closing" + shorten
! !
!FileStream methodsFor: 'undefined' stamp: ''!
writeshorten | |
	self setMode: write + shorten "allow write and shorten File upon closing. in general, this would be faster for overwriting Files since pages might not have to be read first. at present, treated same as readwriteshorten"
! !
!FileStream methodsFor: 'undefined' stamp: ''!
edit | |
	NoteTaker
	  ifTrue: [user schedule: (CodeWindow new file: self)]
	  ifFalse: [user restartup: (CodeWindow new file: self)]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
reopen | pos |
	dirty
	  ifTrue:
		[ "self obsolete"
		pos ← position "reopen to current position".
		(self read: page pageNumber)
		  ifTrue: [position ← pos min: limit]
		  ifFalse:
			[ "if that page doesn't exist, go to last one that does.
		note that settoend would be recursive"
			(self read: self file lastPage)
			  ifTrue: [position ← limit]
			  ifFalse: [self error: 'cannot reopen or settoend']]]
	  ifFalse: [^false]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
release | |
	self obsolete
	  ifFalse:
		[dirty ← limit ← 0.
		self file release]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
close | |
	self obsolete
	  ifFalse:
		[self writing
		  ifTrue:
			[(rwmode anymask: shorten)
			  ifTrue: [self shorten]
			  ifFalse: [self flush]].
		dirty ← limit ← 0.
		self file close.
		externalViews delete: self]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
obsolete | |
	^dirty
! !
!FileStream methodsFor: 'undefined' stamp: ''!
file | |
	^page file
! !
!FileStream methodsFor: 'undefined' stamp: ''!
readPages: n | charsLeft len s |
	len ← n * page dataLength "read n pages of characters".
	s ← String new: len "charsRead ← self into: s endError: false.".
	charsLeft ← self readString: s from: 1 to: len.
	charsLeft = 0
	  ifTrue: [^s].
	 "read len chars" "return characters read only before end of file"
	^s copy: 1 to: len - charsLeft
! !
!FileStream methodsFor: 'undefined' stamp: ''!
writeString: s from: start to: stop | len charsLeft maxLimit |
	 "for writing a subrange of a large String onto a file (quickly, if BitBlt is used);
	called by FileStream append:"
	self writing
	  ifFalse: [self error: 'read only!'].
	self reopen.
	start ← start - 1.
	charsLeft ← stop - start.
	maxLimit ← page dataMaxEnd "keep going until all of the requested characters are copied".
	[charsLeft > 0] whileTrueDo:
		[ "end of current page?"
		position = maxLimit
		  ifTrue: [self nextPage].
		len ← maxLimit - position min: charsLeft.
		charsLeft ← charsLeft - len "copy subrange of String into page".
		array copy: position + 1 to: position + len with: s from: start + 1 to: start + len "update source and destination pointers".
		start ← start + len.
		position ← position + len.
		position > limit
		  ifTrue: [limit ← position]].
	^s
! !
!FileStream methodsFor: 'undefined' stamp: ''!
writeFile: fs for: charsLeft | start len maxLimit |
	 "for copying part or all of one file to another (quickly, if BitBlt is used);
	charsLeft ≡ nil means copy until end, otherwise a number of characters.
	called by FileStream append:, next:from:"
	self writing
	  ifFalse: [self error: 'read only!'].
	self reopen.
	fs readonly.
	fs reopen.
	maxLimit ← page dataMaxEnd "keep going until all of the requested characters are copied or
	until end of file. if end of current page only, next page is read.".
	[(charsLeft ≡ nil or: [charsLeft > 0]) and: [fs end ≡ false]] whileTrueDo:
		[ "end of current destination page?"
		position = maxLimit
		  ifTrue: [self nextPage].
		start ← fs streamPosition.
		len ← maxLimit - position min: fs limit - start.
		charsLeft ≡ nil
		  ifFalse:
			[len ← len min: charsLeft.
			charsLeft ← charsLeft - len] "copy subrange of source page into destination page".
		array copy: position + 1 to: position + len with: fs asArray from: start + 1 to: start + len "update source and destination pointers".
		fs streamPosition← start + len.
		position ← position + len.
		position > limit
		  ifTrue: [limit ← position]].
	^charsLeft ≡ nil
	  ifTrue: [0]
	  ifFalse: [charsLeft]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
streamPosition← t1 | |
	position ← t1.
	^position
! !
!FileStream methodsFor: 'undefined' stamp: ''!
streamPosition | |
	^position
! !
!FileStream methodsFor: 'undefined' stamp: ''!
readString: s from: start to: stop | len charsLeft |
	self readonly "for reading a subrange of a large String from a file (quickly, if BitBlt is used);
	called by FileStream into:endError:".
	self reopen.
	start ← start - 1.
	charsLeft ← stop - start "keep going until all of the requested characters are copied or
	until end of file. if end of current page only, next page is read.".
	[charsLeft > 0 and: [self end ≡ false]] whileTrueDo:
		[len ← limit - position "len = # characters of current page that will fit in String" min: charsLeft.
		charsLeft ← charsLeft - len "copy subrange of page into String".
		s copy: start + 1 to: start + len with: array from: position + 1 to: position + len "update source and destination pointers".
		position ← position + len.
		start ← start + len "return the number of characters not read"].
	^charsLeft
! !
!FileStream methodsFor: 'undefined' stamp: ''!
nextPage | |
	^self read: page pageNumber + 1
! !
!FileStream methodsFor: 'undefined' stamp: ''!
directory | |
	^self file directory
! !
!FileStream methodsFor: 'undefined' stamp: ''!
position: objpos size: size | len pn c pos |
	len ← page dataLength "set the current character position and the current page
	from the position of an object of a given size (see positionSize:)".
	size = len
	  ifTrue:
		[pn ← objpos + 1 "page size".
		c ← 0]
	  ifFalse:
		[pos ← objpos.
		size = 1
		  ifFalse:
			[len \ size = 0
			  ifTrue: [len ← len / size "page length is a multiple of size"]
			  ifFalse:
				[pos ← objpos * size.
				size ← 1]] "obtain quotient (page) and remainder (position)".
		pos ← pos intdiv: len.
		pn ← 1 + (pos ◦ 1) asSmall.
		c ← size * (pos ◦ 2) asSmall].
	(self positionPage: pn character: c)
	  ifTrue: [^objpos].
	self error: 'cannot read page ' + pn asString
! !
!FileStream methodsFor: 'undefined' stamp: ''!
positionSize: size | len pos |
	len ← page dataLength "compute the position for an object of a given size,
		e.g. characters (1), words (2), fixed length (n),
	from the current character position and the current page".
	(size = 1 or: [len \ size ≠ 0])
	  ifTrue:
		[pos ← page pageNumber - 1 * len + (position - page dataBeginning).
		size = 1
		  ifTrue: [^pos].
		^pos / size]
	  ifFalse: [ "page length is a multiple of size"
		^page pageNumber - 1 * (len / size) + (position - page dataBeginning / size)]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
name | |
	^self file name
! !
!FileStream methodsFor: 'undefined' stamp: ''!
fixEnd | |
	(self writing and: [position > page dataEnd])
	  ifTrue: [page dataEnd: (limit ← position) "fix the end of page"]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
page | |
	^page
! !
!FileStream methodsFor: 'undefined' stamp: ''!
pad: size with: val | rem |
	rem ← ( "pad to next boundary of size and return how many characters padded"
			(page dataLength \ size = 0
			  ifTrue: [position - page dataBeginning]
			  ifFalse: [self position]) \ size) asSmall.
	rem = 0
	  ifTrue: [^0].
	self next: (size - rem) ← val.
	^size - rem
! !
!FileStream methodsFor: 'undefined' stamp: ''!
pad: size | rem |
	rem ← ( "skip to next boundary of size and return how many characters skipped"
			(page dataLength \ size = 0
			  ifTrue: [position - page dataBeginning]
			  ifFalse: [self position]) \ size) asSmall.
	rem = 0
	  ifTrue: [^0].
	self skip: size - rem.
	^size - rem
! !
!FileStream methodsFor: 'undefined' stamp: ''!
positionPage: pn character: c | |
	(self read: pn) "normally accessed by position:size:, skip:"
	  ifTrue:
		[position ← position + c "c assumed between 0 and page dataLength. position, limit were set in on:".
		(position ≤ limit or: [self writing])
		  ifTrue: [^true].
		position ← limit.
		^false]
	  ifFalse:
		[c = 0
		  ifTrue: [^self positionPage: pn - 1 "try end of previous page" character: page dataLength].
		^false]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
read: pn | p |
	pn < 1 "normally accessed by nextPage, position:size:, reopen, reset, settoend"
	  ifTrue: [^false].
	self obsolete
	  ifTrue:
		[page reopen "reopen the file, (re)read the page".
		(p ← page read: pn)
		  ifTrue: [self on: p]
		  ifFalse: [^false]]
	  ifFalse:
		[(pn = page pageNumber and: [(page length > 0 or: [position > page dataBeginning])])
		  ifTrue:
			[self fixEnd.
			page asStream: self]
		  ifFalse:
			[ "current page has wrong page number or is empty (possibly from error)"
			self writing
			  ifTrue:
				[((pn > page pageNumber and: [page full ≡ false])
				  ifTrue: [position ← page dataMaxEnd "fill up last page when positioning past it"]
				  ifFalse: [ "otherwise, fixEnd"
					position > page dataEnd])
				  ifTrue: [page dataEnd: (limit ← position)].
				p ← page write.
				p pageNumber = pn
				  ifFalse: [ "already have next page, e.g. at end of AltoFile" "read it or create it"
					p ← page get: pn]]
			  ifFalse: [p ← page read: pn].
			p
			  ifTrue: [(page ← p) asStream: self]
			  ifFalse: [^false]]]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
shorten | |
	self on:  "normally called by close and not directly by user"
	(page dataEnd: (limit ← position).
	page endFile).
	position ← limit
! !
!FileStream methodsFor: 'undefined' stamp: ''!
flush | |
	self obsolete
	  ifTrue: [^page].
	self fixEnd.
	^page write
! !
!FileStream methodsFor: 'undefined' stamp: ''!
settopage: p char: c | |
	(self read: p asSmall "mainly for compatibility, since page sizes may vary.
	in general, use position←, wordposition←")
	  ifTrue: [self skip: c asSmall]
	  ifFalse: [self error: 'no page']
! !
!FileStream methodsFor: 'undefined' stamp: ''!
asParagraphPrinter | |
	^BravoPrinter init "default format for filout etc." of: self
! !
!FileStream methodsFor: 'undefined' stamp: ''!
backup | |
	self directory "assume ivy open" ≡ dp0
	  ifTrue: [ivy replace: self name]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
filoutclass: class | t2 |
	(t2 ← self asParagraphPrinter) stamp.
	t2 printclass: class.
	t2 close
! !
!FileStream methodsFor: 'undefined' stamp: ''!
nextParagraph | text |
	self end "Bravo format paragraph (or self contents if no trailer)"
	  ifTrue: [^false].
	text ← self upto: 26 "ctrl-z".
	^text asParagraph applyBravo: self at: 1 to: text length
! !
!FileStream methodsFor: 'undefined' stamp: ''!
filout | |
	self filout: Changes contents sort
! !
!FileStream methodsFor: 'undefined' stamp: ''!
filout: source | t2 |
	(t2 ← self asParagraphPrinter) stamp.
	t2 printchanges: source.
	t2 close
! !
!FileStream methodsFor: 'undefined' stamp: ''!
filin | p |
	user cr.
	self readonly.
	self end
	  ifTrue: [self file error: 'empty file']
	  ifFalse:
		[[p ← self nextParagraph] whileTrueDo:
			[FilinSource ← self.
			user print: nil ⓢ p text.
			user space].
		self close.
		FilinSource ← nil]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
on: t1 | |
	 "some page from a File, usually page 1, or another FileStream"
	page ← t1.
	(page is: FileStream)
	  ifTrue: [page ← page page].
	page asStream: self.
	externalViews insert: self "obsolete flag".
	dirty ← false
! !
!FileStream methodsFor: 'undefined' stamp: ''!
asPressPrinter | |
	^PressPrinter init "default format for printt etc." of: self
! !
!FileStream methodsFor: 'undefined' stamp: ''!
printoutclass: class | t2 |
	(t2 ← self asPressPrinter) stamp.
	t2 printclass: class.
	t2 close.
	t2 toPrinter
! !
!FileStream methodsFor: 'undefined' stamp: ''!
toPrinter | pp p |
	user displayoffwhile⦂ [
		 "print an unformatted or Bravo file as a press file"
		(pp ← (self directory file: self name + 'Press') asPressPrinter.
		self readonly.
		[p ← self nextParagraph] whileTrueDo: [pp print: p].
		self close)].
	pp close.
	pp toPrinter
! !
!FileStream methodsFor: 'undefined' stamp: ''!
printout: source | t2 |
	(t2 ← self asPressPrinter) stamp.
	t2 printchanges: source.
	t2 close.
	t2 toPrinter
! !
!FileStream methodsFor: 'undefined' stamp: ''!
length | |
	self reopen; fixEnd.
	page lastPage
	  ifTrue: [^page pageNumber - 1 * page dataLength + page length].
	^self file length
! !
!FileStream methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	super printOn: strm.
	strm append: ' on '.
	self file printOn: strm
! !
!FileStream methodsFor: 'undefined' stamp: ''!
wordposition | |
	^self positionSize: 2
! !
!FileStream methodsFor: 'undefined' stamp: ''!
reset | |
	(self read: 1) "self position ← 0"
	  ifFalse: [self error: 'reset']
! !
!FileStream methodsFor: 'undefined' stamp: ''!
position | |
	^self positionSize: 1
! !
!FileStream methodsFor: 'undefined' stamp: ''!
pastend | |
	(self reopen or: [(page lastPage ≡ false and: [self nextPage])])
	  ifTrue: [^self next].
	^false
! !
!FileStream methodsFor: 'undefined' stamp: ''!
pastend← v | |
	self writing
	  ifTrue:
		[self reopen
		  ifTrue: [^self next← v].
		(limit < page dataMaxEnd or: [(self nextPage
		  ifTrue: [position = limit]
		  ifFalse: [self error: 'could not get page'])])
		  ifTrue: [limit ← page dataMaxEnd].
		^self next← v]
	  ifFalse: [self error: 'no writing allowed']
! !
!FileStream methodsFor: 'undefined' stamp: ''!
into: s endError: err | charsRead len t |
	len ← s length.
	len > 80
	  ifTrue: [charsRead ← len - (self readString: s from: 1 to: len)]
	  ifFalse:
		[ "in line: super into: s endError: err"
		charsRead ← 0 "read until count or stream is exhausted".
		[charsRead < len and: [(t ← self next)]] whileTrueDo: [s ◦ (charsRead ← charsRead + 1) ← t]].
	err
	  ifTrue:
		[charsRead = len
		  ifTrue: [^s].
		user notify: 'only read first ' + charsRead asString]
	  ifFalse: [^charsRead]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
settoend | |
	self reopen "self position ← self length" "make sure file is open so lastPage is correct" "when writing on the last page, lastPage may be too small".
	(self read: (self file lastPage max: page pageNumber))
	  ifTrue: [position ← limit]
	  ifFalse: [self error: 'settoend???']
! !
!FileStream methodsFor: 'undefined' stamp: ''!
skip: n | p plen |
	n = 0
	  ifFalse:
		[self reopen.
		p ← position + n.
		(n > 0
		  ifTrue: [p ≥ limit]
		  ifFalse:
			[self fixEnd "important on last page".
			p < page dataBeginning])
		  ifTrue:
			[plen ← page dataLength "simply: self position ← self position + n.
		however, since we are incurable optimizers..." "assume p is not Large, otherwise use intdiv:".
			p ← p - page dataBeginning.
			(self positionPage: page pageNumber + (n < 0
			  ifTrue: [p + 1 / plen - 1]
			  ifFalse: [p / plen]) character: p \ plen)
			  ifFalse: [self error: 'cannot skip ' + n asString]]
		  ifFalse: [ "same page"
			position ← p]]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
word: i ← v | |
	self wordposition← i - 1.
	^self nextword← v
! !
!FileStream methodsFor: 'undefined' stamp: ''!
contents | s |
	self readonly "read all of a File".
	self reset.
	s ← self next: self length.
	self close.
	^s
! !
!FileStream methodsFor: 'undefined' stamp: ''!
word: i | |
	self wordposition← i - 1.
	^self nextword
! !
!FileStream methodsFor: 'undefined' stamp: ''!
next: n from: strm | |
	(n > 80 and: [(strm is: FileStream)])
	  ifTrue: [self writeFile: strm for: n]
	  ifFalse: [^super next: n from: strm]
! !
!FileStream methodsFor: 'undefined' stamp: ''!
append: s | |
	 "try to make some special cases go much faster"
	(s is: String)
	  ifTrue:
		[s length > 80
		  ifTrue:
			[self writeString: s from: 1 to: s length.
			^s]]
	  ifFalse:
		[(s is: Stream)
		  ifTrue:
			[(s limit - s position > 80 and: [(s asArray is: String)])
			  ifTrue:
				[self writeString: s asArray from: s position + 1 to: s limit.
				^s]]
		  ifFalse:
			[(s is: FileStream)
			  ifTrue:
				[self writeFile: s for: nil.
				^s]]].
	^super append: s
! !
!FileStream methodsFor: 'undefined' stamp: ''!
end | |
	self reopen.
	position < limit
	  ifTrue: [^false].
	(self read: page pageNumber + 1)
	  ifTrue: [^position = limit "page empty"].
	^true
! !
!FileStream methodsFor: 'undefined' stamp: ''!
position← p | |
	^self position: p size: 1
! !
!FileStream methodsFor: 'undefined' stamp: ''!
wordposition← w | |
	^self position: w size: 2
! !
!FileStream methodsFor: 'undefined' stamp: ''!
◦ i | |
	self position← i - 1.
	^self next
! !
!FileStream methodsFor: 'undefined' stamp: ''!
◦ i ← v | |
	self position← i - 1.
	^self next← v
! !
!Float methodsFor: 'undefined' stamp: ''!
≠ aNumber | |<primitive: 56>
	aNumber isNumber ≡ false
	  ifTrue: [^true].
	^self retry: #≠ coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
= aNumber | |<primitive: 55>
	aNumber isNumber ≡ false
	  ifTrue: [^false].
	^self retry: #= coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
reciprocal | |
	^1.0 / self
! !
!Float methodsFor: 'undefined' stamp: ''!
≤ aNumber | |<primitive: 53>
	^self retry: #≤ coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
≥ aNumber | |<primitive: 54>
	^self retry: #≥ coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
* aNumber | |<primitive: 57>
	^self retry: #* coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
- aNumber | |<primitive: 50>
	^self retry: #- coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
+ aNumber | |<primitive: 49>
	^self retry: #+ coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
< aNumber | |<primitive: 51>
	^self retry: #< coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
hash | |
	^self instfield: 2  "High bits of fraction"
! !
!Float methodsFor: 'undefined' stamp: ''!
| arg | |
	 "By analogy with integers"
	^(self / arg) ipart * arg
! !
!Float methodsFor: 'undefined' stamp: ''!
/ aNumber | |<primitive: 58>
	^self retry: #/ coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
> aNumber | |<primitive: 52>
	^self retry: #> coercing: aNumber
! !
!Float methodsFor: 'undefined' stamp: ''!
truncated | |<primitive: 60>
	 "Return an Integer = self integerPart"
	^(self / 10000.0) truncated * 10000 + (self \ 10000.0) truncated
! !
!Float methodsFor: 'undefined' stamp: ''!
readFrom: aStream radix: radix | value exp int neg |
	aStream ← aStream asStream.
	radix ← radix asFloat.
	value ← 0.0.
	exp ← 0.
	neg ← (aStream ∢ 45 "-" ifTrue: [true] ifFalse: [aStream ∢ 21 "¬"]).
	[aStream end or: [(aStream peek isDigitRadix: radix) ≡ false]] whileFalseDo:  [value ← value * radix + (aStream next - 48)].
	aStream ∢ 46 "."
	  ifTrue: [[aStream end or: [(aStream peek isDigitRadix: radix) ≡ false]] whileFalseDo: 
			[value ← value * radix + (aStream next - 48).
			exp ← exp - 1]].
	aStream ∢ 101 "e"
	  ifTrue:
		[int ← Integer new readFrom: aStream radix: 10.  "exp is always base 10"
		(int is: Error)
		  ifTrue: [^int].
		exp ← exp + int].
	neg
	  ifTrue: [value ← value negated].
	exp = 0
	  ifTrue: [^value].
	exp > 0
	  ifTrue: [^value * (radix ipow: exp)].
	^value / (radix ipow: exp negated)
! !
!Float methodsFor: 'undefined' stamp: ''!
minVal | |
	^self maxVal negated
! !
!Float methodsFor: 'undefined' stamp: ''!
maxMag | |
	self error: 'hash problem'
! !
!Float methodsFor: 'undefined' stamp: ''!
asFraction | denom |
	 "only use 5 significant figures"
	denom ← (10.0 ipow: 4 - (self abs floorLog: 10.0)) asInteger.
	^(Fraction new numerator: (self * denom) rounded denominator: denom) reduced
! !
!Float methodsFor: 'undefined' stamp: ''!
asFloat | |
	^self
! !
!Float methodsFor: 'undefined' stamp: ''!
minMag | |
	self error: 'hash problem'
! !
!Float methodsFor: 'undefined' stamp: ''!
asInteger | |<primitive: 60>
	 "Return an Integer = self integerPart"
	^(self / 10000.0) asInteger * 10000 + (self \ 10000.0) asInteger
! !
!Float methodsFor: 'undefined' stamp: ''!
fractionalPart | |<primitive: 59>
	user croak
! !
!Float methodsFor: 'undefined' stamp: ''!
coerce: aNumber | |
	^aNumber asFloat
! !
!Float methodsFor: 'undefined' stamp: ''!
generality | |
	^80
! !
!Float methodsFor: 'undefined' stamp: ''!
maxVal | |
	^self maxMag
! !
!Float methodsFor: 'undefined' stamp: ''!
readFrom: aStream | value  |
	value ← Number new readFrom: aStream.
	(value is: Error) ifTrue: [^value].
	^value asFloat
! !
!Float methodsFor: 'undefined' stamp: ''!
ipart | |
	 "Returns a Float with zero fractional part"
	^self - self fpart
! !
!Float methodsFor: 'undefined' stamp: ''!
recopy | |
	^self
! !
!Float methodsFor: 'undefined' stamp: ''!
asLarge | me digits nat i |
	 "convert to LargeInteger"
	self < 0
	  ifTrue: [^(0.0 - self) asLarge negated].
	digits ← Stream default.
	self = 0.0
	  ifTrue: [digits next← 0]
	  ifFalse:
		[me ← self ipart.
		[me ≥ 1] whileTrueDo:
			[digits next← (me \ 256.0) asInteger.
			me ← me / 256.0]].
	digits ← digits contents.
	nat ← Natural new: digits length.
	(1 to: digits length) do: [:i | nat ◦ i ← digits ◦ i].
	^LargeInteger new bytes: nat neg: false
! !
!Float methodsFor: 'undefined' stamp: ''!
copy | |
	^self
! !
!Float methodsFor: 'undefined' stamp: ''!
fpart | |<primitive: 59>
	user croak
! !
!Float methodsFor: 'undefined' stamp: ''!
printOn: strm digits: digits | |
	 "print me using digits significant figures"
	self > 0.0
	  ifTrue: [self absPrintOn: strm digits: digits]
	  ifFalse:
		[self = 0.0
		  ifTrue: [strm append: '0.0']
		  ifFalse:
			[strm append: '-'.
			self negated absPrintOn: strm digits: digits]]
! !
!Float methodsFor: 'undefined' stamp: ''!
absPrintOn: strm digits: digits | fuzz x exp q i |
	 "print me using digits significant figures"
	"x is myself normalized to [1.0, 10.0), exp is my exponent"
	exp ← (self < 1.0
		  	ifTrue: [(10.0/self floorLog: 10.0) negated]
			ifFalse: [self floorLog: 10.0]).
	x ← self / (10.0 ipow: exp). 
	fuzz ← 10.0 ipow: 1 - digits. 	"round the last digit to be printed"
	x ← 0.5 * fuzz + x.
	x ≥ 10.0					"check if rounding has unnormalized x"
	  ifTrue:
		[x ← x / 10.0.
		exp ← exp + 1].
	(exp < 6 and: [exp > -4])
	  ifTrue:					"decimal notation"
		[q ← 0. 
		exp < 0
		  ifTrue: [strm append: '0.0000' ◦ (1 to: 1 - exp)]]
	  ifFalse:					"scientific notation" 
		[q ← exp.
		exp ← 0].
	[x >= fuzz] whileTrueDo: 		"use fuzz to track significance"
		[i ← x truncated.
		strm next← 48 + i.
		x ← (x - i) * 10.0.
		fuzz ← fuzz * 10.0.
		exp ← exp - 1.
		exp = -1
		  ifTrue: [strm append: '.']].
	[exp ≥ -1] whileTrueDo:
		[strm next← 48.
		exp ← exp - 1.
		exp = -1
		  ifTrue: [strm append: '.']].
	q ≠ 0
	  ifTrue:
		[strm append: 'e'.
		strm print: q]
! !
!Float methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	self printOn: strm digits: 8
! !
!Font methodsFor: 'undefined' stamp: ''!
spacewidth | w |
	w ← self widthof: 32.
	w = 0
	  ifTrue: [^4].
	^w
! !
!Font methodsFor: 'undefined' stamp: ''!
maxwidth | |
	^maxwidth
! !
!Font methodsFor: 'undefined' stamp: ''!
charForm: ascii | |
	 "should return a Form copied out of glyphs"
! !
!Font methodsFor: 'undefined' stamp: ''!
widthof: glyph | |
	^xtable ◦ (glyph + 2) - (xtable ◦ (glyph + 1))
! !
!Font methodsFor: 'undefined' stamp: ''!
name | |
	^name
! !
!Font methodsFor: 'undefined' stamp: ''!
height | |
	^ascent + descent "height of font"
! !
!Font methodsFor: 'undefined' stamp: ''!
xtable | |
	^xtable
! !
!Font methodsFor: 'undefined' stamp: ''!
raster | |
	^raster
! !
!Font methodsFor: 'undefined' stamp: ''!
glyphs | |
	^glyphs
! !
!Font methodsFor: 'undefined' stamp: ''!
ascent | |
	^ascent
! !
!Font methodsFor: 'undefined' stamp: ''!
minascii | |
	^minascii
! !
!Font methodsFor: 'undefined' stamp: ''!
maxascii | |
	^maxascii
! !
!Font methodsFor: 'undefined' stamp: ''!
descent | |
	^descent
! !
!Font methodsFor: 'undefined' stamp: ''!
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
!FontSet methodsFor: 'undefined' stamp: ''!
names | |
	^names
! !
!FontSet methodsFor: 'undefined' stamp: ''!
baseline | |
	^ascent
! !
!FontSet methodsFor: 'undefined' stamp: ''!
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
!FontSet methodsFor: 'undefined' stamp: ''!
descent | |
	^descent
! !
!FontSet methodsFor: 'undefined' stamp: ''!
height | |
	^height
! !
!FontSet methodsFor: 'undefined' stamp: ''!
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
!FontSet methodsFor: 'undefined' stamp: ''!
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
!FontSet methodsFor: 'undefined' stamp: ''!
ascent | |
	^ascent
! !
!FontSet methodsFor: 'undefined' stamp: ''!
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
!FontSet methodsFor: 'undefined' stamp: ''!
init | |
	fonts ← Vector new: 16.
	names ← Vector new: 16.
	height ← ascent ← descent ← 0
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
setdescent: descentdelta | updatedfont descent space |
	descent ← font word: 7 "descent delta".
	descent + descentdelta < 0
	  ifTrue: [descentdelta ← 0 - descent].
	descentdelta > 0
	  ifTrue:
		[space ← String new: 2 * fontraster * descentdelta.
		space all← 0.
		updatedfont ← font ◦ (1 to: fontxtabl - 1 * 2) concat: space.
		updatedfont ← self appendxtable: updatedfont]
	  ifFalse:
		[updatedfont ← font ◦ (1 to: fontxtabl - 1 * 2 + (fontraster * descentdelta * 2)).
		updatedfont ← self appendxtable: updatedfont].
	updatedfont word: 7 ← descent + descentdelta "reset descent word in font".
	self setfont: updatedfont "updatedfont now font of interest".
	self updateseglength: font raster: fontraster
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
setascent: ascentdelta | updatedfont ascent |
	ascent ← font word: 6 "ascent delta".
	ascent + ascentdelta < 0
	  ifTrue: [ascentdelta ← 0 - ascent].
	ascentdelta > 0
	  ifTrue:
		[updatedfont ← String new: 2 * fontraster * ascentdelta "grow".
		updatedfont all← 0 "fill with white".
		updatedfont ← font ◦ (1 to: 18) "add oldfont header and new space together" concat: updatedfont ◦ (1 to: updatedfont length).
		updatedfont ← updatedfont concat: font ◦ (19 to: font length "now add on rest of old font")]
	  ifFalse:
		[updatedfont ← font ◦ (1 to: 18) concat: font ◦ (19 + (0 - (2 * fontraster "shrink" * ascentdelta)) to: font length)].
	updatedfont word: 6 ← ascent + ascentdelta "reset ascent word in font".
	self setfont: updatedfont "updatedfont now font of interest".
	self updateseglength: font raster: fontraster
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
setwidth: delta | fontrightx newraster newxtabl newmaxwidth updatedfont i |
	delta ← delta - charwid "change in width".
	delta = 0
	  ifTrue:
		[self show.
		^false].
	.
	fontrightx ← font word: fontxtabl + ((font word: 3) - (font word: 2)) + 2.
	newraster ← (fontrightx + 15 / 16 ≠ (i ← fontrightx + delta + 15 / 16)
			  ifTrue: [i]
			  ifFalse: [fontraster]).
	newxtabl ← newraster * fontht + 9 "header" + 1 "for 0 addressing".
	XeqCursor showwhile⦂ [
		(updatedfont ← String new: 9 + (newraster * fontht) "header" "bits" * 2 "grow/shrink the bits".
		(1 to: 8) "fill in header of new font" do: [:i | updatedfont word: i ← font word: i].
		updatedfont word: 9 ← newraster "set raster in new font" "copy the xtable".
		updatedfont ← self appendxtable: updatedfont "set up to copy up to old bits of char".
		bitsetter destraster← newraster.
		bitsetter destx← 0.
		bitsetter desty← 0.
		bitsetter sourcex← 0.
		bitsetter sourcey← 0.
		bitsetter width← charx + charwid.
		bitsetter height← fontht.
		bitsetter sourceraster← fontraster.
		bitsetter destbase← updatedfont.
		bitsetter sourcebase← font.
		bitsetter sstrike← true.
		bitsetter dstrike← true.
		bitsetter copy: storing.
		delta < 0 "if char grown, clean out right side of char"
		  ifFalse:
			[bitsetter destx← charx + charwid.
			bitsetter width← delta.
			bitsetter fill: storing color: 0] "now copy remainder of font".
		bitsetter destx← charx + charwid + delta.
		bitsetter width← fontrightx - charx - charwid.
		bitsetter sourcex← charx + charwid.
		bitsetter copy: storing "shift x-vals".
		(char + 1 to: 2 + (updatedfont word: 3) - (updatedfont word: 2) "max") do: [:i | updatedfont word: (newxtabl + i) ← delta + (updatedfont word: newxtabl + i)].
		clearframe clear "clear out old version of character".
		self setfont: updatedfont "set up the new copy of the font".
		self updateseglength: font raster: fontraster.
		self updatemaxwidth)]
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
setbit: bitpoint color: color | x y |
	 "turn bits on, off"
	bitpoint ← bitpoint - frame origin.
	x ← (0 max: charwid - 1) min: bitpoint x / scale.
	y ← (0 max: fontht - 1) min: bitpoint y / scale.
	boxer moveto: frame origin + (scale * x ⌾ (scale * y)).
	boxer color: color mode: storing "turn bit on/off in blowup".
	bitsetter destraster← fontraster "set up bitblt table.".
	bitsetter destx← charx + x.
	bitsetter desty← y.
	bitsetter destbase← font.
	bitsetter dstrike← true "lock font and get core ptr".
	bitsetter fill: storing color: color "turn bit on/off in font"
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
setwidth | newextentx outlineframe |
	outlineframe ← clearframe inset: 1 ⌾ 1 "get new size" and: [0 ⌾ 1].
	OriginCursor showwhile⦂ [
		(user waitbug
		  ifTrue:
			[[user anybug] whileTrueDo:
				[outlineframe growto: clearframe origin x + 2 + (newextentx ← user mp x - clearframe origin x + 2 | scale) ⌾ outlineframe corner y.
				outlineframe border: 2 color: black.
				outlineframe border: 2 color: background]]
		  ifFalse:
			[])].
	outlineframe border: 2 color: black.
	self setwidth: newextentx / scale
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
setchar: t1 | |
	char ← t1.
	charstr ◦ 1 ← char.
	char ← (font word: 3) - (font word: 2) + 1 "char out of range".
	(font word: 2) ≤ char
	  ifTrue:
		[char ≤ (font word: 3)
		  ifTrue: [char ← char - (font word: 2)]].
	charx ← font word: fontxtabl + char.
	charwid ← (font word: fontxtabl + char + 1) - charx.
	clearframe clear.
	frame extent← charwid ⌾ fontht.
	clearframe ← frame inset: ¬2 ⌾ ¬2 "for clearing everything including outline" and: [charwid - (charwid * scale + 2) ⌾ (fontht - (fontht * scale + 2))].
	self show "FontWindow regenerate: ↪setchar:"
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
setfont: t1 | |
	font ← t1.
	altostyle fonts ◦ fontnumber ← font.
	fontraster ← font word: 9.
	fontht ← (font word: 6) + (font word: 7) "ascent + descent".
	fontxtabl ← fontraster * fontht + 9 "header" + 1 "for 0 addressing".
	bitsetter width← 1.
	bitsetter height← 1.
	self setchar: charstr ◦ 1
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
help | |
	 "
**sysFontWindow is declared in the Smalltalk dictionary, and bound to the font window displayed on the screen of most system releases -- intended to provide an easy way to play around with the font editor.
**to create a window for editing default font 0 at middle-click:
	 user schedule: (sysFontWindow ← FontWindow new
		altostyle: DefaultTextStyle
		fontnumber: 1
		at: (OriginCursor showwhile⦂
			[user waitbug ⇒[user mp]])).
**to create a new font
	yourfont ← FontWindow new newfont: 16 maxcharwidth: 16 min: 0
			max: 177 ascent: 12 kern: 0.

**to edit newly created font
	yourtextstyle setfont: n name: yourfont.	**insert it into a TextStyle
	**now create a window as above with yourtextstyle and appropriate
		fontnumber
	

**examples of manual manipulation of yourfontwindow:
	sysFontWindow setascent: 2.	**Deltas -- for entire font**
	sysFontWindow setascent: ¬3.
	sysFontWindow setdescent: 2.
	sysFontWindow setdescent: ¬2.
	sysFontWindow setchar: 046.
	sysFontWindow setwidth: 5.	**Absolute--for char in window. 
	Useful for characters of zero width.**
"
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
frame | |
	clearframe clear.
	frame moveto: (OriginCursor showwhile⦂ [
		(user waitbug
		  ifTrue: [user mp])]).
	self setchar: char
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
show | tempframe showrun showpara |
	 "refresh window"
	showrun ← String new: 2.
	showrun word: 1 ← 16 * (fontnumber - 1) + ¬256.
	showpara ← Paragraph new text: charstr runs: showrun alignment: 0.
	tempframe ← Textframe new para: showpara frame: frame style: altostyle.
	tempframe show.
	frame blowup: frame origin by: scale
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
altostyle: t1 fontnumber: t2 at: origin | |
	altostyle ← t1.
	fontnumber ← t2.
	 "set up an instance"
	fontmenu ≡ nil
	  ifTrue: [self init].
	scale ← 9.
	charstr ← String new: 1.
	char ← 65.
	charstr ◦ 1 ← char.
	bitsetter ← BitBlt init.
	boxer ← Rectangle new origin: 0 ⌾ 0 extent: scale - 1 ⌾ (scale - 1).
	frame ← Rectangle new origin: origin extent: scale ⌾ 0.
	clearframe ← Rectangle new origin: origin extent: scale ⌾ 0.
	self setfont: altostyle fonts ◦ fontnumber
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
classInit | |
	fontmenu ← Menu new string: 'strike
set width
debug
move
close'
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
firsttime | |
	 "upon entry"
	(clearframe has: user mp)
	  ifTrue: [self show]
	  ifFalse: [^false]
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
lasttime | |
	 "upon exit"
	
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
eachtime | t1 |
	 "while active"
	(clearframe has: user mp)
	  ifTrue:
		[user redbug
		  ifTrue: [self setbit: user mp color: black]
		  ifFalse:
			[ "make dot black"
			user yellowbug
			  ifTrue: [self setbit: user mp color: white]
			  ifFalse:
				[ "make dot white"
				user bluebug
				  ifTrue:
					[(t1 ← fontmenu bug) = 1
					  ifTrue: [self strike]
					  ifFalse:
						[ "put strike of font in dialogue window"
						t1 = 2
						  ifTrue: [self setwidth]
						  ifFalse:
							[ "grow character"
							t1 = 3
							  ifTrue:
								[self updateseglength: font raster: fontraster.
								self updatemaxwidth "clean things up".
								user notify: 'font debugging']
							  ifFalse:
								[t1 = 4
								  ifTrue: [self frame]
								  ifFalse:
									[ "move fontwindow"
									t1 = 5
									  ifTrue:
										[clearframe clear.
										self updateseglength: font raster: fontraster.
										self updatemaxwidth "clean things up".
										user unschedule: self.
										^false]]]]]]
				  ifFalse:
					[user kbck
					  ifTrue:
						[char ← user kbd.
						self setchar: char]]]]]
	  ifFalse:
		[user anybug
		  ifTrue: [^false]]
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
makecu: name scale: cuscale | f svscale svchar bitwidth i bitmover bits |
	 "Put out font in Carnegie-Mellon format"
	f ← dp0 file: name + '.cu.'.
	self updateseglength: font raster: fontraster.
	self updatemaxwidth.
	svscale ← scale.
	scale ← cuscale.
	svchar ← char.
	self cufixup.
	f nextword← fontht * scale.
	f nextword← (bitwidth ← font word: 4) * scale + 15 / 16.
	bits ← String new: fontht * scale * (bitwidth * scale + 15 / 16) * 2.
	bitmover ← BitBlt init.
	bitmover destbase← bits.
	bitmover destraster← bitwidth * scale + 15 / 16.
	bitmover destx← 0.
	bitmover desty← 0.
	bitmover fromDisplay.
	bitmover sourcex← frame origin x.
	bitmover sourcey← frame origin y.
	((font word: 2) to: (font word: 3) by: 1) do:
		[:i | self setchar: i.
		f nextword← i.
		f nextword← charwid * scale.
		bitmover width← frame extent x * scale.
		bitmover height← frame extent y * scale.
		bits all← 0.
		bitmover copy: storing.
		f append: bits].
	f close.
	scale ← svscale.
	self cufixup.
	self setchar: svchar
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
appendxtable: thefont | |
	thefont ← thefont concat: font ◦ (fontxtabl * 2 "put fontⓢxtable on end of a grown/shrunk font" - 1 to: font length).
	^thefont
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
updateseglength: newfont raster: newraster | |
	newfont word: 5 ← 5 + (newraster * fontht) "compute new segment length for a font" "length, ascent, descent, kern, and raster" "bits" + ((font word: 3) "max" - (font word: 2) "min" + 2) "xtabl"
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
updatemaxwidth | newmaxwidth i |
	newmaxwidth ← 0 "update max width".
	(fontxtabl to: fontxtabl + ((font word: 3) - (font word: 2) + 1) by: 1) do: [:i | newmaxwidth ← newmaxwidth max: (font word: i + 1) - (font word: i)].
	font word: 4 ← newmaxwidth
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
strike | i showstr |
	 "Put a strike of font into dialogue window"
	showstr ← String new: 128.
	(1 to: 128) do: [:i | showstr ◦ i ← i].
	user clearshow: showstr
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
newfont: t1 maxcharwidth: maxcharwidth min: min max: max ascent: ascent kern: kern | raster i x |
	fontht ← t1.
	XeqCursor showwhile⦂ [
		(raster ← 2 + max - min * maxcharwidth + 15 / 16.
		font ← String new: 3 + max - min + (fontht * raster) + 9 * 2.
		font word: 1 ← ¬32768 "format: strike, simple, varwidth".
		font word: 2 ← min "min ascii code".
		font word: 3 ← max "max ascii code".
		font word: 4 ← maxcharwidth "max char width".
		font word: 5 ← 2 + max - min + 5 + (fontht * raster) "segment length".
		font word: 6 ← ascent "bits above baseline".
		font word: 7 ← fontht - ascent "bits below baseline".
		font word: 8 ← kern "kerning offset".
		font word: 9 ← raster "#words per scan-line in bitmap".
		font ◦ (18 + 1 to: 2 * raster * fontht + 18) all← 0 "chars all white".
		ascent ← ascent min: fontht - 1 "keep baseline within char".
		font ◦ (2 * raster * ascent + 18 + 1 to: ascent + 1 * raster * 2 + 18) all← 255 "put in a black baseline".
		x ← 0.
		(raster * fontht + 9 + 1 to: raster * fontht + 9 + 3 + max - min by: 1) do:
			[:i | font word: i ← x.
			x ← x + maxcharwidth "table of left x"])].
	^font
! !
!FontWindow methodsFor: 'undefined' stamp: ''!
cufixup | |
	 "Carnegie-Mellon fixup for scale compatibility"
	boxer extent← scale - 1 ⌾ (scale - 1).
	frame extent← scale ⌾ 0.
	clearframe extent← scale ⌾ 0
! !
!Form methodsFor: 'undefined' stamp: ''!
extent: t1 bits: t2 offset: t3 | |
	extent ← t1.
	bits ← t2.
	offset ← t3
! !
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
setground: parentimage | |
	ground ← ground + 1 "for now just increment the ground color by 1 \ 14" \ 14.
	self displayat: parentimage origin effect: 0 clippedBy: user screenrect
! !
!Form methodsFor: 'undefined' stamp: ''!
setfigure: parentimage | |
	figure ← figure + 1 "for now just increment the figure color by 1 \ 14" \ 14.
	self displayat: parentimage origin effect: 0 clippedBy: user screenrect
! !
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
blinkbrush: parentimage | pt |
	pt ← parentimage mp "to show current position of brush in the form.".
	brush displayat: parentimage rectangle origin + pt effect: 2 clippedBy: user screenrect.
	brush displayat: parentimage rectangle origin + pt effect: 2 clippedBy: user screenrect.
	^parentimage rectangle origin + pt
! !
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
write: filename | t2 |
	(t2 ← dp0 file: filename "Saves the Form in the format width,height,bits.") nextword← self width.
	t2 nextword← self height.
	t2 append: bits.
	t2 close
! !
!Form methodsFor: 'undefined' stamp: ''!
fromuserevenword | r |
	r ← Rectangle new "create a new Form whose rectangle is specified by the user,
		truncated to nearest multiple of 16 (for Spruce printing). " fromuserevenword.
	self extent: r extent.
	bits ← r bitsIntoString
! !
!Form methodsFor: 'undefined' stamp: ''!
fromuser | r |
	r ← Rectangle new "create a new Form whose rectangle is specified by the user. " fromuser.
	self extent: r extent.
	bits ← r bitsIntoString
! !
!Form methodsFor: 'undefined' stamp: ''!
close | |
	
! !
!Form methodsFor: 'undefined' stamp: ''!
extent: t1 | |
	extent ← t1.
	self extent: extent figure: 0 ground: 1 offset: 0 ⌾ 0 "creates a virtual bit map with width = (extent x) and height = (extent y) with the bits all 1."
! !
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
fromHalftone: halftone | i j line |
	self extent: 16 ⌾ 16.
	(1 to: 4) do:
		[:i | line ← (halftone land: 15) * 4369.
		(0 to: 3) do: [:j | bits ◦ (j * 4 + i) ← line].
		halftone ← halftone lshift: ¬4]
! !
!Form methodsFor: 'undefined' stamp: ''!
halftoneInteger | screen i |
	screen ← 0.
	(1 to: 4) do: [:i | screen ← (screen lshift: 4) lor: (bits ◦ i land: 15)].
	^screen
! !
!Form methodsFor: 'undefined' stamp: ''!
fromrectangle: r | |
	self extent: r extent "creates a virtual bit map with width = (r width) and height = (r height) with the bits in r.".
	bits ← r bitsIntoString
! !
!Form methodsFor: 'undefined' stamp: ''!
fromImage: image | |
	self extent: image extent "creates a virtual bit map with width = (image width) and height = (image height) with the bits in image.".
	bits ← image rectangle bitsIntoString
! !
!Form methodsFor: 'undefined' stamp: ''!
extent: t1 figure: t2 ground: t3 offset: t4 | |
	extent ← t1.
	figure ← t2.
	ground ← t3.
	offset ← t4.
	bits ← Bitmap new: extent y "creates a virtual bit map with width = (extent x) and height = (extent y) with the bits all 1." * (extent x + 15 / 16)
! !
!Form methodsFor: 'undefined' stamp: ''!
offset: t1 | |
	offset ← t1.
	^self "set the offset of the form "
! !
!Form methodsFor: 'undefined' stamp: ''!
height | |
	^extent y "return the height of the Form"
! !
!Form methodsFor: 'undefined' stamp: ''!
extent | |
	^extent "return the extent (width⌾height) of the Form"
! !
!Form methodsFor: 'undefined' stamp: ''!
offset | |
	offset ≡ nil
	  ifTrue: [^0 ⌾ 0].
	^offset
! !
!Form methodsFor: 'undefined' stamp: ''!
ground: t1 | |
	ground ← t1 "set the ground ( color assiciated with white) for the form "
! !
!Form methodsFor: 'undefined' stamp: ''!
figure: t1 | |
	figure ← t1 "set the figure ( color assiciated with black) for the form "
! !
!Form methodsFor: 'undefined' stamp: ''!
length | |
	^bits length
! !
!Form methodsFor: 'undefined' stamp: ''!
ground | |
	^ground "return the ground ( color assiciated with white) for the form "
! !
!Form methodsFor: 'undefined' stamp: ''!
width | |
	^extent x "return the width of the Form"
! !
!Form methodsFor: 'undefined' stamp: ''!
figure | |
	^figure "return the figure( color assiciated with black) for the form "
! !
!Form methodsFor: 'undefined' stamp: ''!
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
!Form methodsFor: 'undefined' stamp: ''!
bits: t1 | |
	bits ← t1 "reset the string containing the bits)"
! !
!Form methodsFor: 'undefined' stamp: ''!
bits | |
	^bits "return the string containing the bits)"
! !
!Form methodsFor: 'undefined' stamp: ''!
gray | i |
	(1 to: bits length "sets all bits in the form to gray ( to gray)") do: [:i | bits ◦ i ← 10922]
! !
!Form methodsFor: 'undefined' stamp: ''!
white | i |
	(1 to: bits length "sets all bits in the form to white ( to zeros)") do: [:i | bits ◦ i ← 0]
! !
!Form methodsFor: 'undefined' stamp: ''!
black | i |
	(1 to: bits length "sets all bits in the form to black ( to ones)") do: [:i | bits ◦ i ← 0 - 1]
! !
!Form methodsFor: 'undefined' stamp: ''!
fromPress: press value: s | nbytes |
	extent ← s nextPoint.
	offset ← s nextPoint.
	figure ← s nextword.
	ground ← s nextword.
	nbytes ← 2 * extent y * (extent x + 15 / 16).
	press data skip: 0 - nbytes.
	bits ← press data next: nbytes
! !
!Form methodsFor: 'undefined' stamp: ''!
fromInstance: file | |
	extent ← file nextPoint.
	offset ← file nextPoint.
	figure ← file nextword.
	ground ← file nextword.
	bits ← file nextString.
	^self
! !
!Form methodsFor: 'undefined' stamp: ''!
asInstance | s |
	s ← Stream new default.
	s nextPoint← extent.
	s nextPoint← offset.
	s nextword← figure.
	s nextword← ground.
	s nextString← bits.
	^s contents
! !
!Form methodsFor: 'undefined' stamp: ''!
presson: press in: r | hs y |
	(hs ← press scale * self height) > r height
	  ifTrue: [^self].
	 "not enough room left on current page.
		assume for now that it will at least fit on an entire page"
	press setp: r origin x ⌾ (y ← r corner y - hs).
	press bitmap: self bits: bits.
	^y
! !
!Form methodsFor: 'undefined' stamp: ''!
hidePress: press complete: c | |
	press skipcode: self pressCode data: (self hideData: c)
! !
!Form methodsFor: 'undefined' stamp: ''!
copy | t |
	t ← Form new "return a copy of myself" extent: extent.
	t bits: bits copy.
	^t
! !
!Form methodsFor: 'undefined' stamp: ''!
pressCode | |
	^5
! !
!Form methodsFor: 'undefined' stamp: ''!
hideData: complete | s t3 |
	(t3 ← Stream new "a Form does not split across page boundaries") of: (s ← String new: 12).
	t3 nextPoint← extent.
	t3 nextPoint← offset.
	t3 nextword← figure.
	t3 nextword← ground.
	^s
! !
!FormSet methodsFor: 'undefined' stamp: ''!
replace: t1 with: form | |
	formindex ← t1.
	self checkindex "Replace form in set.  Check incoming form for compatibility with formset,
	and insert form into formset.".
	form width ≠ self width
	  ifTrue: [self changewidthof: formindex to: form width].
	form displayat: 0 ⌾ 0 effect: 0 clippedBy: user screenrect "copy bits of form into formset".
	bitmover destraster← self wordwidth.
	bitmover destx← self originx.
	bitmover desty← 0.
	bitmover sourcex← 0.
	bitmover sourcey← 0.
	bitmover width← self width.
	bitmover height← self ascent + self descent.
	bitmover fromDisplay.
	bitmover destbase← strike.
	bitmover dstrike← true.
	bitmover copy: storing
! !
!FormSet methodsFor: 'undefined' stamp: ''!
makecu: name scale: scale | f i iform bits drast |
	user displayoffwhile⦂ [
		 "Put out strike in Carnegie-Mellon format.
		A typical call might look like:
			yourset ←
				FormSet new style: DefaultTextStyle styleindex: 0.

				yourset makecu: 'cream12' scale: 1.			
			"
		f ← dp0 file: name + '.cu.'.
		f nextword← self height * scale.
		f nextword← self maxwidth * scale + 15 / 16.
		bits ← String new: self height * scale * (self maxwidth * scale + 15 / 16) * 2.
		drast ← self maxwidth * scale + 15 / 16.
		(self first to: self last by: 1) do:
			[:i | iform ← self asForm: i "self copy: i to: 0⌾0".
			iform displayat: 0 ⌾ 0 effect: 0 clippedBy: user screenrect.
			scale > 1
			  ifTrue: [iform blowup: 0 ⌾ 0 by: scale].
			f nextword← i.
			f nextword← self width * scale.
			bitmover destbase← bits.
			bitmover destraster← drast.
			bitmover destx← 0.
			bitmover desty← 0.
			bitmover fromDisplay.
			bitmover sourcex← 0.
			bitmover sourcey← 0.
			bitmover dstrike← false.
			bitmover sstrike← true.
			bitmover width← iform width * scale.
			bitmover height← iform height * scale.
			bits all← 0.
			bitmover copy: storing.
			f append: bits].
		f close]
! !
!FormSet methodsFor: 'undefined' stamp: ''!
copy: t1 to: pt effect: effect | f |
	formindex ← t1.
	self checkindex "copies the form indexed by formindex to pt." "f ← Image new size: (self width) ⌾ (self height) at: pt.".
	bitmover toDisplay.
	bitmover destx← pt x.
	bitmover desty← pt y.
	bitmover sourcex← self originx.
	bitmover sourcey← 0.
	bitmover width← self width.
	bitmover height← self ascent + self descent.
	bitmover sourceraster← self wordwidth.
	bitmover sourcebase← strike.
	bitmover strike← true.
	bitmover copy: effect
! !
!FormSet methodsFor: 'undefined' stamp: ''!
copy: t1 to: pt | f |
	formindex ← t1.
	self checkindex "copies the form indexed by formindex to pt." "f ← Image new size: (self width) ⌾ (self height) at: pt.".
	bitmover toDisplay.
	bitmover destx← pt x.
	bitmover desty← pt y.
	bitmover sourcex← self originx.
	bitmover sourcey← 0.
	bitmover width← self width.
	bitmover height← self ascent + self descent.
	bitmover sourceraster← self wordwidth.
	bitmover sourcebase← strike.
	bitmover strike← true.
	bitmover copy: oring.
	^self widthof: formindex
! !
!FormSet methodsFor: 'undefined' stamp: ''!
from: t1 | |
	strike ← t1.
	self classInit "Make a formset out of string in strike format".
	self install: strike
! !
!FormSet methodsFor: 'undefined' stamp: ''!
asForm: t1 | f |
	formindex ← t1.
	self checkindex "returns the form indexed by formindex .".
	f ← Form new extent: self width ⌾ self height.
	bitmover destraster← f width + 15 / 16.
	bitmover destx← 0.
	bitmover desty← 0.
	bitmover sourcex← self originx.
	bitmover sourcey← 0.
	bitmover width← self width.
	bitmover height← self ascent + self descent.
	bitmover sourceraster← self wordwidth.
	bitmover destbase← f bits.
	bitmover sourcebase← strike.
	bitmover dstrike← false.
	bitmover sstrike← true.
	bitmover copy: storing.
	^f
! !
!FormSet methodsFor: 'undefined' stamp: ''!
show | |
	space outline "show all the forms in the set".
	space show
! !
!FormSet methodsFor: 'undefined' stamp: ''!
newspace | |
	space frame← Rectangle new "let user reshape/position space" fromuser.
	self show
! !
!FormSet methodsFor: 'undefined' stamp: ''!
widthof: t1 | |
	formindex ← t1.
	^self width "return width of from at formindex"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
spaceorigin: t1 | |
	spaceorigin ← t1.
	space erase "reposition the space".
	space frame origin← spaceorigin.
	self show
! !
!FormSet methodsFor: 'undefined' stamp: ''!
spaceframe | |
	^space frame "return frame of space"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
height | |
	^self ascent "return height of fromset" + self descent
! !
!FormSet methodsFor: 'undefined' stamp: ''!
initspaceat: t1 | i run para |
	spaceorigin ← t1.
	image ← String new: 256 "make a space for formset viewing".
	image all← 0.
	(self first to: self last + 1) do: [:i | image ◦ (i + 1) ← i].
	run ← String new: 2.
	run word: 1 ← 16 * styleindex + ¬256.
	para ← Paragraph new text: image runs: run alignment: 0.
	space ≡ nil
	  ifFalse: [space erase].
	space ← Textframe new para: para frame: (Rectangle new origin: spaceorigin extent: self last - self first * self maxwidth / 8 ⌾ (self ascent + self descent * 6)) style: style
! !
!FormSet methodsFor: 'undefined' stamp: ''!
fromstyle: t1 styleindex: t2 | |
	style ← t1.
	styleindex ← t2.
	self classInit "Make a formset out of string in strike format".
	self install: style fonts ◦ (styleindex + 1)
! !
!FormSet methodsFor: 'undefined' stamp: ''!
copyrange: start to: stop from: sourceset startingat: deststart | savebackground savebits i f |
	user displayoffwhile⦂ [
		 "copy a range of forms from one set to another"
		((sourceset is: FormSet)
		  ifFalse:
			[(sourceset is: String)
			  ifTrue: [sourceset ← FormSet new from: sourceset]
			  ifFalse: [user notify: 'Illegal sourceset -- not String or Formset.']].
		savebackground ← Form new size: sourceset maxwidth by: sourceset height.
		savebackground translate: 0 ⌾ 0.
		savebackground scale: 1.
		savebits ← savebackground bitsIntoString.
		(start to: stop) do:
			[:i | f ← sourceset copy: i to: 0 ⌾ 0.
			self include: deststart with: f.
			deststart ← deststart + 1].
		savebackground bitsFromString: savebits)]
! !
!FormSet methodsFor: 'undefined' stamp: ''!
classInit | |
	bitmover ← BitBlt init "Just initialize the bitmover for now."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
currentformorigin | imageindex |
	imageindex ← image find: formindex "return index in image of form last touched".
	^space ptofchar: imageindex
! !
!FormSet methodsFor: 'undefined' stamp: ''!
changewidthof: t1 to: width | |
	formindex ← t1.
	self checkindex "new width for form at index".
	self deltawidthof: formindex by: width - self width
! !
!FormSet methodsFor: 'undefined' stamp: ''!
fromspace: pt to: dest | |
	formindex ← image ◦ ((space charofpt: pt) "get form selected from space" min: 256).
	^self copy: formindex to: dest
! !
!FormSet methodsFor: 'undefined' stamp: ''!
currentformindex | |
	^formindex "return index of form last touched"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
include: t1 with: form | newoffsettable newstrike i j |
	formindex ← t1.
	(formindex > self first "Put a form into the formset" and: [formindex < self last])
	  ifTrue: [self replace: formindex with: form]
	  ifFalse:
		[formindex < 0
		  ifTrue: [user notify: 'Formindex < 0 illegal for formset.']
		  ifFalse:
			[formindex > 255
			  ifTrue: [user notify: 'Formindex > 255 illegal for formset.']
			  ifFalse:
				[formindex < self first
				  ifTrue:
					[newoffsettable ← String new: self first - formindex + self abslength * 2.
					newoffsettable all← 0.
					j ← self first - formindex + 1.
					(j to: newoffsettable length / 2) do: [:i | newoffsettable word: i ← strike word: offsettable + (i - j)]]
				  ifFalse:
					[newoffsettable ← String new: self abslength + formindex - self last * 2.
					newoffsettable all← 0.
					(0 to: self length - 1) do: [:i | newoffsettable word: (i + 1) ← strike word: offsettable + i].
					(self length to: newoffsettable length / 2) do: [:i | newoffsettable word: i ← strike word: offsettable + self length].
					newoffsettable word: (newoffsettable length / 2) ← strike word: offsettable + self length + 1].
				newstrike ← String new: 9 + (self wordwidth "header" * (self ascent + self descent)) "bits" * 2 "new space for bits".
				(1 to: 9) "fill in header of new font" do: [:i | newstrike word: i ← strike word: i].
				bitmover destraster← self wordwidth.
				bitmover destx← 0.
				bitmover desty← 0.
				bitmover sourcex← 0.
				bitmover sourcey← 0.
				bitmover width← self strikerightx.
				bitmover height← self ascent + self descent.
				bitmover sourceraster← self wordwidth.
				bitmover destbase← newstrike.
				bitmover sourcebase← strike.
				bitmover strike← true.
				bitmover copy: storing "copy the xtable".
				newstrike ← newstrike concat: newoffsettable ◦ (1 to: newoffsettable length).
				self install: newstrike.
				formindex < self first
				  ifTrue: [self first: formindex]
				  ifFalse: [self last: formindex].
				self replace: formindex with: form]]]
! !
!FormSet methodsFor: 'undefined' stamp: ''!
changedescentto: newdescent | |
	self deltadescent: newdescent - self descent "new ascent for FormSet"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
changeascentto: newascent | |
	self deltaascent: newascent - self ascent "new ascent for FormSet"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
space | |
	^space "return textframe that is space of formset"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
from: first to: last ascent: ascent descent: descent style: t5 styleindex: t6 name: name | |
	style ← t5.
	styleindex ← t6.
	self classInit "Make an empty formset.".
	offsettable ← String new: last - first + 3 * 2.
	offsettable all← 0.
	offsettable word: (last - first + 3) ← 4 "width of illegal form".
	strike ← String new: 9 + ascent "header" + descent "space for illegal form" * 2.
	strike all← 0.
	self type: ¬32768 "for the outside world".
	self first: first.
	self last: last.
	self wordwidth: 1 "only illegal form".
	self ascent: ascent.
	self descent: descent.
	self maxwidth: 4 "width of illegal form".
	strike ← strike concat: offsettable ◦ (1 to: offsettable length) "mash in bits of illegal form" "leftside".
	bitmover destraster← self wordwidth.
	bitmover destx← 0.
	bitmover desty← 0.
	bitmover width← 1.
	bitmover height← self ascent + self descent.
	bitmover destbase← strike.
	bitmover dstrike← true.
	bitmover fill: storing color: black "rightside".
	bitmover destx← 3.
	bitmover dstrike← true.
	bitmover fill: storing color: black "top".
	bitmover width← 4.
	bitmover height← 1.
	bitmover destx← 0.
	bitmover dstrike← true.
	bitmover fill: storing color: black "bottom".
	bitmover desty← self ascent + self descent - 1.
	bitmover dstrike← true.
	bitmover fill: storing color: black.
	self install: strike.
	self updateseglength.
	style ≡ nil
	  ifFalse: [style setfont: styleindex name: name fromstring: strike]
! !
!FormSet methodsFor: 'undefined' stamp: ''!
width | |
	^(strike word: offsettable + (formindex - self first "Return width of form at formindex") + 1) - (strike word: offsettable + (formindex - self first))
! !
!FormSet methodsFor: 'undefined' stamp: ''!
updatemaxwidth | newmaxwidth i |
	newmaxwidth ← 0 "update max width".
	(offsettable to: offsettable + (self last - self first + 1)) do: [:i | newmaxwidth ← newmaxwidth max: (strike word: i + 1) - (strike word: i)].
	self maxwidth: newmaxwidth
! !
!FormSet methodsFor: 'undefined' stamp: ''!
originx | |
	^strike word: offsettable + formindex "Return origin x  of form at formindex" - self first
! !
!FormSet methodsFor: 'undefined' stamp: ''!
install: t1 | |
	strike ← t1.
	 "set up a new or refreshed strike"
	style ≡ nil
	  ifFalse: [style fonts ◦ (styleindex + 1) ← strike].
	offsettable ← self wordwidth * (self ascent + self descent) + 9 "header" + 1 "for 0 addressing".
	^strike
! !
!FormSet methodsFor: 'undefined' stamp: ''!
abslength | |
	^self last "Return absolute length of formset, i.e. number of forms in set
		plus space for illegal character and its rightx" - self first + 3
! !
!FormSet methodsFor: 'undefined' stamp: ''!
updateseglength | |
	strike word: 5 ← 5 + (self wordwidth "compute new segment length for formset" "length, ascent, descent, kern, and raster" * (self ascent + self descent)) "bits" + (self last "max" - self first "min" + 2) "xtabl"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
deltawidthof: index by: delta | newwordwidth newoffsettable newstrike normalizedindex normalizedlast i |
	 "change width of form at index"
	delta < 0
	  ifTrue:
		[delta abs > self width
		  ifTrue: [delta ← 0 - self width]].
	newwordwidth ← (self strikerightx + 15 / 16 ≠ (i ← self strikerightx + delta + 15 / 16)
			  ifTrue: [i]
			  ifFalse: [self wordwidth]).
	newoffsettable ← newwordwidth * (self ascent + self descent) "height" + 9 "header" + 1 "for 0 addressing".
	XeqCursor showwhile⦂ [
		(newstrike ← String new: 9 + (newwordwidth * (self ascent "header" + self descent)) "bits" * 2 "grow/shrink the bits".
		newstrike all← 0.
		(1 to: 8) "fill in header of new font" do: [:i | newstrike word: i ← strike word: i].
		newstrike word: 9 ← newwordwidth "set raster in new font" "copy the xtable".
		newstrike ← newstrike concat: strike ◦ (offsettable * 2 - 1 to: strike length) "set up to copy up to old bits of form in formset".
		bitmover destraster← newwordwidth.
		bitmover destx← 0.
		bitmover desty← 0.
		bitmover sourcex← 0.
		bitmover sourcey← 0.
		bitmover width← self originx + self width.
		bitmover height← self ascent + self descent.
		bitmover sourceraster← self wordwidth.
		bitmover destbase← newstrike.
		bitmover sourcebase← strike.
		bitmover sstrike← true.
		bitmover dstrike← true.
		bitmover copy: storing "now copy remainder of font".
		bitmover destx← self originx + self width + delta.
		bitmover width← self strikerightx - self originx - self width.
		bitmover sourcex← self originx + self width.
		bitmover copy: storing "shift x-vals".
		normalizedindex ← formindex - self first.
		normalizedlast ← self last - self first.
		(normalizedindex + 1 to: normalizedlast + 2 "max") do: [:i | newstrike word: (newoffsettable + i) ← delta + (newstrike word: newoffsettable + i)].
		self install: newstrike "set up the updated copy of the formset".
		self updatemaxwidth.
		self updateseglength)]
! !
!FormSet methodsFor: 'undefined' stamp: ''!
deltadescent: delta | newstrike somespace |
	 "descent delta"
	self descent + delta < 0
	  ifTrue: [delta ← 0 - self descent].
	delta > 0
	  ifTrue:
		[somespace ← String new: 2 * self wordwidth * delta.
		somespace all← 0.
		newstrike ← strike ◦ (1 to: offsettable - 1 * 2) concat: somespace]
	  ifFalse:
		[newstrike ← strike ◦ (1 to: offsettable - 1 * 2 + (self wordwidth * delta * 2))] "copy the xtable".
	newstrike ← newstrike concat: strike ◦ (offsettable * 2 - 1 to: strike length).
	newstrike word: 7 ← self descent + delta "reset descent word in font".
	self install: newstrike "updatedfont now font of interest".
	self updatemaxwidth.
	self updateseglength.
	style ≡ nil
	  ifFalse:
		[style maxdescent < self descent
		  ifTrue: [style maxdescent: self descent]]
! !
!FormSet methodsFor: 'undefined' stamp: ''!
deltaascent: delta | newstrike |
	 "ascent delta"
	self ascent + delta < 0
	  ifTrue: [delta ← 0 - self ascent].
	delta > 0
	  ifTrue:
		[newstrike ← String new: 2 * self wordwidth "grow" * delta.
		newstrike all← 0 "fill with white".
		newstrike ← strike ◦ (1 to: 18) "add oldfont header and new space together" concat: newstrike ◦ (1 to: newstrike length).
		newstrike ← newstrike concat: strike ◦ (19 to: strike length "now add on rest of old font")]
	  ifFalse:
		[ "shrink"
		newstrike ← strike ◦ (1 to: 18) concat: strike ◦ (19 + (0 - (2 * self wordwidth * delta)) to: strike length)].
	newstrike word: 6 ← self ascent + delta "reset ascent word in font".
	self install: newstrike "newstrike now font of interest".
	self updateseglength.
	style ≡ nil
	  ifFalse:
		[style maxascent < self ascent
		  ifTrue: [style maxascent: self ascent]]
! !
!FormSet methodsFor: 'undefined' stamp: ''!
checkindex | |
	 "check formindex for legality and make into number if necessary"
	(formindex is: String)
	  ifTrue:
		[formindex length > 1
		  ifTrue: [user notify: 'formindex out of range for FormSet.']
		  ifFalse: [formindex ← formindex ◦ 1]].
	(formindex < self first or: [formindex > (self last + 1)])
	  ifTrue: [user notify: 'formindex out of range for this FormSet.']
! !
!FormSet methodsFor: 'undefined' stamp: ''!
length | |
	^self last "Return length of formset, i.e. number of forms in set" - self first + 1
! !
!FormSet methodsFor: 'undefined' stamp: ''!
kern | |
	^strike word: 8 "When form set treated as characters, describes distance this form is
	to intrude into space of preceding character."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
type | |
	^strike word: 1 "**BEWARE -- outside world has ideas about this value."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
ascent | |
	^strike word: 6 "When form set treated as characters, describes distance from top of form
	to baseline."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
wordwidth: wordwidth | |
	strike word: 9 ← wordwidth "Also know as the raster of the formset.
	The width in alto words of the bits of the formset.  When the display of a 
	form is desired, the word and bit address of the bits of the form must
	be discovered.  Adding the wordwidth to the word portion of that value,
	produces the word address of the second line of the bits of the form, and
	so on until the height of the form is spanned."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
type: type | |
	strike word: 1 ← type "**BEWARE -- outside world has ideas about this value."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
ascent: ascent | |
	strike word: 6 ← ascent "When form set treated as characters, describes distance from top of form
	to baseline."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
maxwidth | |
	^strike word: 4 "All forms in this set ≤ to this value"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
descent: descent | |
	strike word: 7 ← descent "When form set treated as characters, describes distance from bottom of
	form to baseline."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
strikerightx | |
	^strike word: offsettable + (self last "Corner x of last form in form set" - self first) + 2
! !
!FormSet methodsFor: 'undefined' stamp: ''!
segmentlength | |
	^strike word: 5 "Amount of space allocated for form set - 4"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
first: first | |
	strike word: 2 ← first "Heritage from the world of fonts" "minimum formindex (ascii) in the strike"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
first | |
	^strike word: 2 "Heritage from the world of fonts" "minimum formindex (ascii) in the strike"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
maxwidth: maxwidth | |
	strike word: 4 ← maxwidth "All forms in this set ≤ to this value"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
last: last | |
	strike word: 3 ← last "Heritage from the world of fonts" "maximum formindex (ascii) in the strike"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
last | |
	^strike word: 3 "Heritage from the world of fonts" "maximum formindex (ascii) in the strike"
! !
!FormSet methodsFor: 'undefined' stamp: ''!
wordwidth | |
	^strike word: 9 "Also know as the raster of the formset.
	The width in alto words of the bits of the formset.  When the display of a 
	form is desired, the word and bit address of the bits of the form must
	be discovered.  Adding the wordwidth to the word portion of that value,
	produces the word address of the second line of the bits of the form, and
	so on until the height of the form is spanned."
! !
!FormSet methodsFor: 'undefined' stamp: ''!
descent | |
	^strike word: 7 "When form set treated as characters, describes distance from bottom of
	form to baseline."
! !
!Fraction methodsFor: 'undefined' stamp: ''!
= frac | |
	frac isNumber ≡ false
	  ifTrue: [^false].
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #= coercing: frac].
	frac numerator = 0
	  ifTrue: [^numerator = 0].
	^self - frac = 0
! !
!Fraction methodsFor: 'undefined' stamp: ''!
reciprocal | |
	^Fraction new numerator: denominator denominator: numerator
! !
!Fraction methodsFor: 'undefined' stamp: ''!
+ frac | com |
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #+ coercing: frac].
	denominator = frac denominator
	  ifTrue: [^Fraction new numerator: numerator + frac numerator denominator: denominator].
	com ← denominator lcm: frac denominator. "common denominator"
	^((self withDenominator: com) + (frac withDenominator: com)) reduced
! !
!Fraction methodsFor: 'undefined' stamp: ''!
* frac | |
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #* coercing: frac].
	^(Fraction new numerator: numerator * frac numerator denominator: denominator * frac denominator) reduced
! !
!Fraction methodsFor: 'undefined' stamp: ''!
compare: frac | diff |
	diff ← self - frac.
	diff < 0
	  ifTrue: [^1].
	diff = 0
	  ifTrue: [^2].
	^3
! !
!Fraction methodsFor: 'undefined' stamp: ''!
< frac | |
	(frac moreGeneralThan: self)
	  ifTrue: [^self retry: #< coercing: frac].
	frac numerator = 0
	  ifTrue: [^numerator < 0].
	^self - frac < 0
! !
!Fraction methodsFor: 'undefined' stamp: ''!
negated | |
	^Fraction new numerator: numerator negated denominator: denominator
! !
!Fraction methodsFor: 'undefined' stamp: ''!
numerator | |
	^numerator
! !
!Fraction methodsFor: 'undefined' stamp: ''!
denominator | |
	^denominator
! !
!Fraction methodsFor: 'undefined' stamp: ''!
readFrom: strm | num denom |
	strm ← strm asStream.
	num ← Integer new readFrom: strm radix: 10.
	(num is: Error) ifTrue: [^num].
	denom ← (strm ∢ ('%' ◦ 1)
			  ifTrue: [Integer new nextInteger: strm base: 10]
			  ifFalse: [1]).
	(denom is: Error) ifTrue: [^denom].
	^(Fraction new numerator: num denominator: denom) reduced
! !
!Fraction methodsFor: 'undefined' stamp: ''!
asFraction | |
	^self
! !
!Fraction methodsFor: 'undefined' stamp: ''!
asInteger | |
	^self asFloat asInteger
! !
!Fraction methodsFor: 'undefined' stamp: ''!
generality | |
	^60
! !
!Fraction methodsFor: 'undefined' stamp: ''!
coerce: aNumber | |
	^aNumber asFraction
! !
!Fraction methodsFor: 'undefined' stamp: ''!
asFloat | |
	^numerator asFloat / denominator asFloat
! !
!Fraction methodsFor: 'undefined' stamp: ''!
numerator: n denominator: d | |
	d = 0
	  ifTrue: [self error: 'denominator cannot be zero']
	  ifFalse:
		[numerator ← n asInteger.
		denominator ← d asInteger abs "keep sign in numerator".
		d < 0
		  ifTrue: [numerator ← numerator negated]]
! !
!Fraction methodsFor: 'undefined' stamp: ''!
withDenominator: denom | f |
	denom ← denom asInteger.
	(denom\\denominator) = 0
	  ifTrue: [f ← self]
	  ifFalse:
		[f ← self reduced.
		(denom\\f denominator) ≠ 0
		  ifTrue: [^self error: 'unable to normalize fraction']].
	^Fraction new numerator: f numerator * (denom//f denominator) denominator: denom
! !
!Fraction methodsFor: 'undefined' stamp: ''!
reduced | gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd ← numerator gcd: denominator.
	numer ← numerator//gcd.
	denom ← denominator//gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction new numerator: numer denominator: denom
! !
!Fraction methodsFor: 'undefined' stamp: ''!
normalized: denom | f |
	 "use least multiple of denom for denominator"
	denom ← denom asInteger.
	(denom\\denominator) = 0
	  ifTrue: [f ← self]
	  ifFalse:
		[f ← self reduced.
		denom ← denom lcm: f denominator].
	^Fraction new numerator: f numerator * (denom // f denominator) denominator: denom
! !
!Fraction methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm append: '('.
	strm print: numerator.
	strm append: '%'.
	strm print: denominator.
	strm append: ')'
! !
!Fraction methodsFor: 'undefined' stamp: ''!
truncated | |
	^numerator quo: denominator
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
open | |
	command ← Stream new of: (String new: 100).
	command append: 'Ftp '.
	command append: directory.
	self login: self userName password: self userPassword
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
login: name password: pw | |
	name empty
	  ifFalse:
		[command append: ' Login/C '.
		command append: name.
		command space.
		command append: pw]
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
store: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self store: t]]
	  ifFalse:
		[command append: ' Store/C '.
		command append: (self checkName: s)]
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
close | |
	self closeThen: 'Resume. Small.Boot'
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
rename: oldName newName: newName | |
	command append: ' Rename/C '.
	command append: (self checkName: oldName).
	command space.
	command append: (self checkName: newName)
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
closeThen: s | |
	command append: '; '.
	command append: s.
	user quitThen: command contents.
	self open
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
directoryName: name | |
	command append: ' Directory/C ' "this message should be directory:, but until rewriting...".
	command append: name
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
retrieve: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self retrieve: t]]
	  ifFalse:
		[command append: ' Retrieve/C '.
		command append: (self checkName: s)]
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
connect: name password: pw | |
	command append: ' Connect/C '.
	command append: name.
	command space.
	command append: pw
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
store: local as: remote | |
	command append: ' Store/S '.
	command append: (self checkName: local).
	command space.
	command append: (self checkName: remote)
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
retrieve: remote as: local | |
	command append: ' Retrieve/S '.
	command append: (self checkName: remote).
	command space.
	command append: (self checkName: local)
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
replace: name | s |
	s ← self checkName: name.
	(directory compare: 'maxc') = 2
	  ifTrue:
		[self delete: s.
		self store: s]
	  ifFalse:
		[ "store as highest version (ifs only)"
		command append: ' Store/S '.
		command append: s.
		command space.
		command append: s.
		command append: '!H']
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
delete: name | |
	command append: ' Delete/C '.
	command append: (self checkName: name)
! !
!FtpDirectory methodsFor: 'undefined' stamp: ''!
commands | |
	^command contents
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
doFile | str i s2 r y skipsum |
	str ← String new: pixelsPerLine.
	r ← 0 ⌾ 0 rect: pixelsPerLine * outpix / inpix ⌾ 1.
	r moveto: rect origin copy.
	s2 ← String new: 1 + (pixelsPerLine * outpix / (8 * inpix)).
	vect ← Vector new: lines.
	strm reset.
	strm position← 2048 + (inset y * npix) "crop top".
	i ← 1.
	y ← 0 - outpix.
	skipsum ← 0.
	[i ≤ lines] whileTrueDo:
		[skipsum ← skipsum + inset x "inset left".
		strm skip: skipsum.
		skipsum ← 0 "do all tallied skips prior to next read".
		strm into: str endError: true.
		r bitsFromString: (self decode: str using: s2).
		skipsum ← skipsum + npix - (pixelsPerLine + inset x).
		r origin y← r origin y + 1.
		r corner y← r corner y + 1.
		(y ← y + inpix) ≥ 0
		  ifTrue:
			[ "next line?"
			i ← i + 1.
			y ← y - outpix.
			[y ≥ 0 and: [i ≤ lines]] whileTrueDo:
				[i ← i + 1.
				y ← y - outpix.
				skipsum ← skipsum + npix]]
		  ifFalse: [skipsum ← skipsum - npix] "not next line"].
	strm close
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
decode: str using: s | i j k x cascadeRight cascadeDiag val error r msk masks |<primitive: 75>
	masks ← #(128 64 32 16 8 4 2 1 ) "Change 8-bit grey from str filling s".
	cascadeRight ← 0.
	cascadeDiag ← errorString ◦ 1.
	i ← msk ← j ← k ← 1.
	x ← 0 - outpix.
	s ◦ 1 ← 0.
	(1 to: pixelsPerLine) do:
		[:i | [x < 0] whileTrueDo:
			[val ← str ◦ i - black.
			(error ← cascadeRight - val) ≥ 0
			  ifTrue:
				[s ◦ j ← masks ◦ msk "print Black" + (s ◦ j).
				error > white
				  ifTrue: [error ← white]]
			  ifFalse:
				[ "print White"
				(error ← error + white) < 0
				  ifTrue: [error ← 0]].
			error ← error / 2.
			val ← error / 2.
			errorString ◦ k ← cascadeDiag + val.
			cascadeRight ← errorString ◦ (k + 1) + error.
			cascadeDiag ← val.
			(msk ← msk + 1) > 8
			  ifTrue:
				[msk ← 1.
				j ← j + 1.
				s ◦ j ← 0].
			x ← x + inpix.
			k ← k + 1].
		x ← x - outpix].
	^s
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
intoPress: p file: f | outrect |
	 "Creates an external file reference"
	outrect ← p transrect: rect.
	p setp: outrect origin.
	p dots⦂ [
		(p setcoding: 8 dots: npix lines: nlines "byte samples".
		p setmode: 3 "to right and to bottom of page".
		p setwindowwidth: pixelsPerLine height: lines skipdots: inset x skiplines: inset y.
		p setsizewidth: outrect width height: outrect height.
		p dotsfromAIS: f)] "
	|p. p←dp0 pressfile: 'pix.press'.
	p pictureinit. (HalfToner new test) intoPress: p file: 'Rolfup.AIS'. p close.
"
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
rect | |
	^rect
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
test | files |
	files ← (dp0 filesMatching: '*.ais.') sort.
	files empty
	  ifTrue: [user notify: 'no .ais files on disk']
	  ifFalse:
		[strm ← dp0 file: files ◦ (Menu new stringFromVector: files) zbug.
		strm readonly.
		rect ← Rectangle new usersize.
		self setup: strm.
		self doFile] "
HalfToner new test.
"
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
strm | |
	^strm
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
setup: t1 | inrect croprect |
	strm ← t1.
	strm readonly.
	((strm word: 2) ≠ 1024 or: [(strm word: 9) ≠ 8])
	  ifTrue: [user notify: 'bad file']
	  ifFalse:
		[nlines ← lines ← strm word: 4.
		npix ← pixelsPerLine ← strm nextword.
		black ← 0.
		white ← 255.
		inrect ← 0 ⌾ 0 rect: pixelsPerLine ⌾ lines.
		inrect moveto: rect origin.
		inrect usermove.
		inrect comp "show whole".
		croprect ← rect copy.
		croprect moveto: inrect origin copy.
		croprect maxstretch: inrect.
		croprect userstretch: inrect.
		inrect comp.
		inset ← croprect origin - inrect origin.
		pixelsPerLine ← croprect width.
		lines ← pixelsPerLine * rect height / rect width.
		rect width > pixelsPerLine
		  ifTrue:
			[inpix ← 32 "blowup".
			outpix ← 32 * rect width / pixelsPerLine]
		  ifFalse:
			[ "shrink"
			outpix ← 32.
			inpix ← 32 * pixelsPerLine / rect width].
		errorString ← String new: pixelsPerLine * outpix / inpix + 2.
		errorString all← 0]
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
nlines | |
	^nlines
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
rect← t1 | |
	rect ← t1
! !
!HalfToner methodsFor: 'undefined' stamp: ''!
npix | |
	^npix
! !
!HashSet methodsFor: 'undefined' stamp: ''!
size | |
	^objects length
! !
!HashSet methodsFor: 'undefined' stamp: ''!
contents | obj strm |
	strm ← (Vector new: objects length) asStream.
	objects do:
		[:obj | obj ≡ nil
		  ifFalse: [strm next← obj]].
	^strm contents
! !
!HashSet methodsFor: 'undefined' stamp: ''!
asStream | |
	^self contents asStream
! !
!HashSet methodsFor: 'undefined' stamp: ''!
lookup: obj | i |
	 "⇑object if found, else false (object=obj but may not ≡obj)"
	i ← self findornil: obj.
	objects ◦ i = obj
	  ifTrue: [^objects ◦ i].
	^false
! !
!HashSet methodsFor: 'undefined' stamp: ''!
shrink | table oldtable |
	oldtable ← self.
	table ← oldtable growto: (2 max: oldtable size / 2).
	[table size = oldtable size] whileFalseDo: 
		[(oldtable size - table size) print.
		user show: ' '.
		oldtable ← table.
		table ← oldtable growto: (2 max: oldtable size / 2)].
	^table
! !
!HashSet methodsFor: 'undefined' stamp: ''!
packprobes | tot n l i obj t |
	 "⇑(fullness, avg #probes)"
	tot ← n ← 0.
	l ← objects length.
	(1 to: l) do:
		[:i | (obj ← objects ◦ i) ≡ nil
		  ifFalse:
			[t ← obj hash \ l.
			tot ← tot + (i < t
					  ifTrue: [l - t + i]
					  ifFalse: [i - t]).
			n ← n + 1]].
	n = 0
	  ifTrue: [^{1 , 1}].
	^{n asFloat / l , (tot asFloat / n)} "Class md packprobes(0.4921875 2.53968255 )"
! !
!HashSet methodsFor: 'undefined' stamp: ''!
copy | |
	 "⇑ a copy of me"
	^self class new copyfrom: self
! !
!HashSet methodsFor: 'undefined' stamp: ''!
init: size | |
	objects ← Vector new: (size max: 2)
! !
!HashSet methodsFor: 'undefined' stamp: ''!
init | |
	self init: 4
! !
!HashSet methodsFor: 'undefined' stamp: ''!
copyfrom: hset | |
	 "take on state of hset"
	objects ← hset objects copy
! !
!HashSet methodsFor: 'undefined' stamp: ''!
insert: obj | i |
	self findorinsert: obj.
	^obj
! !
!HashSet methodsFor: 'undefined' stamp: ''!
insertall: objs | x |
	objs do: [:x | self insert: x]
! !
!HashSet methodsFor: 'undefined' stamp: ''!
delete: obj | i j l |
	(obj is: Vector)
	  ifTrue: [obj do: [:i | self delete: i]]
	  ifFalse:
		[i ← self findorerror: obj.
		objects ◦ i ← nil.
		l ← objects length.
		[objects ◦ (i ← (i = l
				  ifTrue: [1]
				  ifFalse: [i + 1])) ≡ nil] whileFalseDo: 
			[i = (j ← self findornil: objects ◦ i)
			  ifFalse: [self swap: i with: j]]]
! !
!HashSet methodsFor: 'undefined' stamp: ''!
rawinsert: t1 | t2 |
	 "assumes there is room for the new one"
	t2 ← self findornil: t1.
	objects ◦ t2 ← t1.
	^t2
! !
!HashSet methodsFor: 'undefined' stamp: ''!
findornil: obj | i loc |
	 "⇑index if found or available slot"
	loc ← obj hash \ objects length.
	(1 to: objects length) do:
		[:i | loc ← (loc = objects length
				  ifTrue: [1]
				  ifFalse: [loc + 1]).
		objects ◦ loc ≡ nil
		  ifTrue: [^loc].
		objects ◦ loc = obj
		  ifTrue: [^loc]].
	^1 "table full - caller must check for hit"
! !
!HashSet methodsFor: 'undefined' stamp: ''!
findorinsert: obj | i |
	 "insert if not found, "
	i ← self findornil: obj.
	objects ◦ i = obj
	  ifTrue: [^i].
	 "found it"
	self sparse
	  ifTrue:
		[objects ◦ i ← obj.
		^i].
	 "insert if room"
	self growto: objects length * 2 "grow".
	^self findorinsert: obj "and insert"
! !
!HashSet methodsFor: 'undefined' stamp: ''!
swap: i with: j | |
	objects swap: i with: j
! !
!HashSet methodsFor: 'undefined' stamp: ''!
sparse | i n |
	n ← objects length "⇑true if (1 max: 1/4 of table) is nil".
	(1 to: objects length) do:
		[:i | objects ◦ i ≡ nil
		  ifTrue:
			[(n ← n - 4) ≤ 0
			  ifTrue: [^true]]].
	^false
! !
!HashSet methodsFor: 'undefined' stamp: ''!
rehash | i copy |
	copy ← HashSet new init: self size "create a copy".
	(1 to: objects length) "hash each entry into it" do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [copy insert: objects ◦ i]].
	objects ← copy objects
! !
!HashSet methodsFor: 'undefined' stamp: ''!
objects← t1 | |
	objects ← t1
! !
!HashSet methodsFor: 'undefined' stamp: ''!
objects | |
	^objects
! !
!HashSet methodsFor: 'undefined' stamp: ''!
growto: t1 | t2 t3 |
	 "faster insert for growing"
	t2 ← self class new init: t1.
	t1 < objects length
	  ifTrue: [self do: [:t3 | t2 insert: t3]]
	  ifFalse: [self do: [:t3 | t2 rawinsert: t3]].
	objects ← t2 objects
! !
!HashSet methodsFor: 'undefined' stamp: ''!
find: obj | i |
	 "⇑index if found, else false"
	i ← self findornil: obj.
	objects ◦ i = obj
	  ifTrue: [^i].
	^false
! !
!HashSet methodsFor: 'undefined' stamp: ''!
has: obj | |
	^objects ◦ (self findornil: obj) = obj
! !
!HashSet methodsFor: 'undefined' stamp: ''!
findorerror: name | i |
	i ← self findornil: name.
	objects ◦ i = name
	  ifTrue: [^i].
	 "allow the user to put a correct value into i"
	user notify: name asString + ' cannot be found'.
	^i
! !
!Heading methodsFor: 'undefined' stamp: ''!
newfont | w char charcount delta i |
	index ← (user request: 'index of new font . .  ') asInteger.
	formset ← FormSet new fromstyle: DefaultTextStyle styleindex: index.
	self white.
	(1 to: position) do: [:i | self ◦ i ← nil].
	position ← 0.
	w ← 0.
	charcount ← 0.
	delta ← origin copy.
	self translate: 0 ⌾ 0 - delta.
	charactercodes do:
		[:char | nil ≡ char
		  ifFalse:
			[char ← formset asForm: char.
			char displayat: delta + (w ⌾ 0) effect: 0 clippedBy: user screenrect.
			self addpath: w ⌾ 0 andform: char.
			w ← w + char width.
			charcount ← charcount + 1]].
	self resize.
	self translate: delta.
	^self
! !
!Heading methodsFor: 'undefined' stamp: ''!
typein | w char charcount delta i |
	self white.
	(1 to: position) do: [:i | self ◦ i ← nil].
	position ← 0.
	w ← 0.
	charcount ← 0.
	charactercodes ← Set new default.
	delta ← origin copy.
	self translate: 0 ⌾ 0 - delta.
	[(char ← user kbd) = 13] whileFalseDo: 
		[char = 8
		  ifTrue:
			[ "back space"
			charcount ≠ 0
			  ifTrue:
				[w ← w - (self ◦ charcount) width.
				(self ◦ charcount) white.
				self deleteimage: charcount.
				charactercodes deleteI: charcount.
				charcount ← charcount - 1]]
		  ifFalse:
			[charactercodes add: char.
			char ← formset asForm: char.
			char displayat: delta + (w ⌾ 0) effect: 0 clippedBy: user screenrect.
			self addpath: w ⌾ 0 andform: char.
			w ← w + char width.
			charcount ← charcount + 1]].
	self resize.
	self translate: delta.
	^self
! !
!Heading methodsFor: 'undefined' stamp: ''!
edit: parentimage | pt t3 |
	self display: 0 "Simple Heading (line) editor for now.".
	[1 = 2] whileFalseDo: 
		[ "forever for now"
		user kbck
		  ifTrue: [self typein]
		  ifFalse:
			[user yellowbug
			  ifTrue:
				[(t3 ← headingmenu bug) = 1
				  ifTrue: [self right]
				  ifFalse:
					[ "move current character right one bit"
					t3 = 2
					  ifTrue: [self left]
					  ifFalse:
						[ "move current character left one bit"
						t3 = 3
						  ifTrue: [self up]
						  ifFalse:
							[ "move current character up one bit"
							t3 = 4
							  ifTrue: [self down]
							  ifFalse:
								[ "move current character down one bit"
								t3 = 5
								  ifTrue: [self newfont] "change fonts"]]]]]
			  ifFalse:
				[user redbug
				  ifTrue:
					[(rectangle has: (pt ← user mp))
					  ifTrue:
						[pt ← pt - rectangle origin.
						currentcharacter ← self indexofsubimageat: pt.
						currentcharacter
						  ifTrue:
							[self ◦ currentcharacter displayat: self origin effect: 2 clippedBy: user screenrect.
							self ◦ currentcharacter displayat: self origin effect: 2 clippedBy: user screenrect]]
					  ifFalse: [^self]]
				  ifFalse:
					[user bluebug
					  ifTrue: [^self] "exit back to the parentimage"]]]]
! !
!Heading methodsFor: 'undefined' stamp: ''!
left | i delta |
	self boxcomp "move the current character and all those to the right of it to the left one bit.".
	(currentcharacter to: self length) do: [:i | self ◦ i translate: 0 ⌾ 0 - (1 ⌾ 0)].
	delta ← origin copy.
	self translate: 0 ⌾ 0 - delta.
	self resize.
	self translate: delta.
	self white.
	self display: 1.
	self boxcomp
! !
!Heading methodsFor: 'undefined' stamp: ''!
down | delta |
	self boxcomp "move the current character down one bit.".
	self ◦ currentcharacter translate: 0 ⌾ 1.
	delta ← origin copy.
	self translate: 0 ⌾ 0 - delta.
	self resize.
	self translate: delta.
	self white.
	self display: 1.
	self boxcomp
! !
!Heading methodsFor: 'undefined' stamp: ''!
right | i delta |
	self boxcomp "move the current character and all those to the right of it to the right one bit.".
	(currentcharacter to: self length) do: [:i | self ◦ i translate: 1 ⌾ 0].
	delta ← origin copy.
	self translate: 0 ⌾ 0 - delta.
	self resize.
	self translate: delta.
	self white.
	self display: 1.
	self boxcomp
! !
!Heading methodsFor: 'undefined' stamp: ''!
up | delta |
	self boxcomp "move the current character up one bit.".
	self ◦ currentcharacter translate: 0 ⌾ 0 - (0 ⌾ 1).
	delta ← origin copy.
	self translate: 0 ⌾ 0 - delta.
	self resize.
	self translate: delta.
	self white.
	self display: 1.
	self boxcomp
! !
!Heading methodsFor: 'undefined' stamp: ''!
origin: t1 formset: t2 currentcharacter: t3 | |
	origin ← t1.
	formset ← t2.
	currentcharacter ← t3.
	 "initilization of a Heading (used in copy)"
	(formset Is: Integer)
	  ifTrue:
		[formset ← FormSet new fromstyle: DefaultTextStyle styleindex: formset].
	self origin: origin extent: 200 ⌾ formset height
! !
!Heading methodsFor: 'undefined' stamp: ''!
classInit | |
	headingmenu ← Menu new "menu for the Heading edits." string: 'right
left
up
down
font
'
! !
!Heading methodsFor: 'undefined' stamp: ''!
origin: t1 index: t2 charactercodes: t3 currentcharacter: t4 | char w delta |
	origin ← t1.
	index ← t2.
	charactercodes ← t3.
	currentcharacter ← t4.
	formset ← FormSet new "initilization of a Heading (used in copy)" fromstyle: DefaultTextStyle styleindex: index.
	nil ≡ charactercodes
	  ifTrue: [charactercodes ← Set new default].
	self origin: origin extent: 200 ⌾ formset height.
	w ← 0.
	delta ← origin copy.
	self translate: 0 ⌾ 0 - delta.
	charactercodes do:
		[:char | char ← formset asForm: char.
		self addpath: w ⌾ 0 andform: char.
		w ← w + char width].
	self translate: delta.
	^self
! !
!Heading methodsFor: 'undefined' stamp: ''!
fromPress: press value: s | numberofcharacters i |
	numberofcharacters ← s nextword "retrieves and builds an instance of class Heading from a press file".
	origin ← s nextPoint.
	index ← s nextword.
	charactercodes ← Set new default.
	(1 to: numberofcharacters) do: [:i | charactercodes add: s nextword].
	^self origin: origin index: index charactercodes: charactercodes currentcharacter: 1
! !
!Heading methodsFor: 'undefined' stamp: ''!
copy | h i |
	h ← Heading new origin: rectangle origin copy index: index charactercodes: charactercodes copy currentcharacter: currentcharacter copy.
	h rectangle: rectangle copy.
	(1 to: self length) do: [:i | h add: (self ◦ i) copy].
	^h
! !
!Heading methodsFor: 'undefined' stamp: ''!
pressCode | |
	^2
! !
!Heading methodsFor: 'undefined' stamp: ''!
hideData: complete | s i |
	s ← Stream new "stores an instance of class Heading on a press file" of: (String new: 100).
	s nextword← self length "number of characters".
	s nextPoint← origin.
	s nextword← index.
	(1 to: self length) do: [:i | s nextword← charactercodes ◦ i].
	^s contents
! !
!Heading methodsFor: 'undefined' stamp: ''!
presson: press in: r | hs y t i pressscale |
	(hs ← press scale * self height) > r height
	  ifTrue: [^self].
	 "not enough room left on current page.
		assume for now that it will at least fit on an entire page"
	self hidePress: press complete: ¬1.
	pressscale ← press scale.
	press selectfont: (press fontindex: 16 * index style: DefaultTextStyle) - 1.
	(1 to: self length) do:
		[:i | press setx: r leftside + ((self ◦ i) leftside * pressscale).
		press sety: r bottom - ((self ◦ i) top * pressscale).
		press showchar: charactercodes ◦ i].
	^r bottom - (self height * pressscale)
! !
!ILFile methodsFor: 'undefined' stamp: ''!
entryClass | |
	^ILFilePage
! !
!ILFile methodsFor: 'undefined' stamp: ''!
close: e | p | "close file, possibly ignoring errors"
	type ← read. "for next open"
	(directory obsolete and: [e=false]) ifTrue: [^self "dont bother to reopen"].

	"shorten header block to first 2 words: command&length,  file handle"
	p ← self newPage.
	p length: ¬6.
	self doCommand: Close page: p error: e
! !
!ILFile methodsFor: 'undefined' stamp: ''!
release | |
	self close: false
! !
!ILFile methodsFor: 'undefined' stamp: ''!
open | |
	 "do nothing. lastpn set by ILFileDirectory Find:"
! !
!ILFile methodsFor: 'undefined' stamp: ''!
Write: page | pn |
	(pn ← page pageNumber) > (lastpn + 1)
	  ifTrue: [self error: 'illegal page number']
	  ifFalse:
		[ "make sure we can write"
		self writeMode: page.
		page mode: (pn ≥ lastpn
		  ifTrue: [SetEof]
		  ifFalse: [NoHoles]).
		self doCommand: Write page: page error: 'Write:' "file possibly extended".
		pn = (lastpn + 1)
		  ifTrue: [lastpn ← pn].
		^page]
! !
!ILFile methodsFor: 'undefined' stamp: ''!
findLastPage | |
	^lastpn "already known from open"
! !
!ILFile methodsFor: 'undefined' stamp: ''!
writeMode: page | |
	type = write "make sure we can write on file"
	  ifFalse:
		[ "turn on writing by closing and reopening file. eventually there might be
	a simpler way to change file mode"
		self close.
		type ← write.
		(directory Find: self)
		  ifTrue: [page serialNumber: serialNumber "file handle changed"]
		  ifFalse: [self error: 'file failed to reopen']]
! !
!ILFile methodsFor: 'undefined' stamp: ''!
errorString: errorCode | ef ename errorString notfound dollar cr t8 |
	(errorCode is: String)
	  ifTrue:
		[errorString ← errorCode.
		errorCode ← ((errorString ◦ 1) isdigit
				  ifTrue: [errorString asInteger]
				  ifFalse: [0])]
	  ifFalse: [errorString ← errorCode asString].
	ename ← '<System>Ifs.Errors'.
	(self name compare: ename) = 2
	  ifTrue: [^errorString + ' (cannot access Ifs.Errors !!!)' "recursion"].
	notfound ← errorString + '	(error code not found)'.
	errorCode ≤ 0
	  ifTrue: [^notfound].
	dollar ← '$' ◦ 1.
	cr ← 13.
	ef ← directory oldFile: ename.
	ef readonly.
	errorString ← false "scan through the errors file looking for lines of the form:
$$nn	some message
".
	[errorString] whileFalseDo: 
		[(ef skipTo: dollar)
		  ifTrue:
			[ef next = dollar
			  ifTrue:
				[(t8 ← ef integerScan "valid line") > errorCode
				  ifTrue: [errorString ← notfound "since errors are ordered"]
				  ifFalse:
					[t8 = errorCode
					  ifTrue:
						[errorString ← (String new: 200) asStream.
						errorString print: errorCode.
						[ef peek = dollar or: [ef peek = cr]] whileFalseDo: 
							[errorString append: (ef upto: cr).
							errorString space].
						errorString ← errorString contents]]]]
		  ifFalse: [ "end of file"
			errorString ← notfound]].
	ef close.
	^errorString
! !
!ILFile methodsFor: 'undefined' stamp: ''!
doCommand: com page: page error: e | in ecode t6 |
	page command: com.
	[true] whileTrueDo:
		[directory open "make sure connection is open".
		error ← nullString.
		(in ← directory socket sendPacket: page packet)
		  ifTrue:
			[in ◦ 11 = BrokenConnection
			  ifTrue: [ecode ← ¬1]
			  ifFalse:
				[ "turn packet into a ILFilePage"
				in ← ((t6 ← self entryClass new) dictionary: self.
						t6 page: in) "check if answer is of same type as request".
				((page header: 1) land: RequestBits) + AnswerBit = ((in header: 1) land: RequestBits)
				  ifTrue: [^in].
				ecode ← in header: 2]]
		  ifFalse: [ "no response?"
			ecode ← false] "some kind of problem".
		com = Quit
		  ifTrue: [^false].
		 "ignore"
		(ecode ≡ false or: [ecode = ¬1])
		  ifTrue: [directory release "make new connection"].
		(ecode = ¬1 or: [ecode = 1011])
		  ifTrue:
			[self reopen "try again after some reinitializing" "reopen file" "init page with new handle only -- don't lose mode, length, etc.".
			page serialNumber: serialNumber]
		  ifFalse:
			[error ← (ecode
					  ifTrue:
						[e
						  ifTrue: [self errorString: ecode]
						  ifFalse: [ecode asString]]
					  ifFalse: [directory directory + ' not responding']).
			e
			  ifTrue: [self error: e "proceeding tries again"]
			  ifFalse: [^false]]]
! !
!ILFile methodsFor: 'undefined' stamp: ''!
close | |
	self close:  "ignore errors if file was readonly"
	(type = read
	  ifTrue: [false]
	  ifFalse: ['close'])
! !
!ILFile methodsFor: 'undefined' stamp: ''!
classInit | i sym names |
	ILFilePool declare: #NotFound as: '207' "ILFile classInit.".
	names ← #(1011 (BadLeafHandle ) 1024 (AnswerBit ) ¬1024 (RequestBits ) 176 (LeafType ) 0 (Error Open Close Delete ) 6 (Read Write Reset ) 96 (SetEof ) 128 (NoExtend ) 64 (NoHoles ) ¬6400 (WriteOld ) ¬30976 (ReadOld ) ¬4224 (CreateNew ) 0 (Data Ack Noop ) 5 (OpenConnection ) 9 (DestroyConnection Dally Quit BrokenConnection ) ) asStream "5-bit operations for left field command block word" "read/write modes" "no holes, set eof" "don't extend file on read or write" "for writing past end" "open file modes" "read, write, extend, any explicit, highest" "read, any explicit, highest" "read, write, extend, create, any explicit, next" "control codes for left half of pupID1 field" "if Reset".
	names do: [:i | names next do:
			[:sym | ILFilePool declare: sym as: i.
			i ← i + 1]]
! !
!ILFile methodsFor: 'undefined' stamp: ''!
Read: page | |
	page pageNumber > lastpn
	  ifTrue: [^false].
	 "no page" "don't extend beyond eof. length of page is 0, but request is for 512 bytes"
	page mode: NoExtend.
	page header: 5 ← 512 "self dataLength".
	^self doCommand: Read page: page error: 'Read:'
! !
!ILFile methodsFor: 'undefined' stamp: ''!
endFile: page | |
	self writeMode: page "make sure we can write" "update length. this will end file with this page".
	lastpn ← page pageNumber.
	^self Write: page
! !
!ILFile methodsFor: 'undefined' stamp: ''!
allocatePage | |
	^directory allocatePage
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
release | t1 |
	self obsolete
	  ifFalse:
		[ "shorten header block to 0 words.
	DestroyConnection. after this the connection is gone"
		(t1 ← self newPage) length: ¬10.
		t1 doCommand: Quit error: false.
		isocket close.
		isocket ← nil]
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
open | page t2 |
	isocket ≡ nil "self obsolete"
	  ifTrue:
		[(isocket ← ILSocket new hostName: directory "name of IFS/Leaf server")
		  ifFalse:
			[isocket ← nil.
			user notify: directory + ' (name not found)'].
		super open.
		page ← self newPage "treat packet in page as a parameter block".
		(t2 ← ILParameterBlock new) packet← page packet.
		t2 nextword← 0 "host number (0 = this, ¬1 = all)".
		t2 nextString← self userName.
		t2 nextString← self userPassword.
		(page doCommand: Reset error: false)
		  ifFalse:
			[isocket ← nil.
			user notify: 'open (reset)']]
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
obsolete | |
	^isocket ≡ nil
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
versionNumbers | |
	^true
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
Insert: file | |
	file type: write.
	(self openFile: file mode: CreateNew)
	  ifTrue: [^file].
	file error: 'Insert: cannot create'
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
Delete: file | t2 |
	(t2 ← file newPage "delete a file (highest version)
	shorten header block to 2 words: command&length,  file handle") length: ¬6.
	t2 doCommand: Delete error: 'Delete:'
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
entryClass | |
	^ILFile
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
Find: file | |
	^self openFile: file mode:  "since there can be many readers but only one writer, default mode is for
	read only. writing will cause file to be closed and reopened for writing"
	(file type = write
	  ifTrue: [WriteOld]
	  ifFalse: [ReadOld])
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
userPassword | |
	(userPassword ≡ nil or: [userPassword empty])
	  ifTrue: [^super userPassword].
	^userPassword
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
openFile: file mode: m | page t4 |
	self open "open bit modes
	read
	write
	extend
	multiple (0)
	create name
	explicit version in name (2b)
		no, old, next or old, any
	default (if no version specified) (2b)
		no, lowest, highest, next
	unused (7b)".
	page ← file newPage "treat packet in page as a parameter block".
	(t4 ← ILParameterBlock new) packet← page packet.
	t4 nextword← 0 "file handle".
	t4 nextword← m "modes".
	t4 nextString← self userName.
	t4 nextString← self userPassword.
	t4 nextString← '' "connect name".
	t4 nextString← '' "connect password".
	t4 nextString← self checkName: file name "answer bits (in page header: 5)
	same (5b)
	version (4b)
		bad, default lowest, default highest, default next, !*, !L, !H, !N, explicit old, explicit lowest, explicit highest, explicit next, explicit new, explicit-less, explicit between, explicit greater".
	(page ← file doCommand: Open page: page error: false)
	  ifTrue:
		[file serialNumber: page serialNumber "open returns file length".
		file lastPage: (page pageNumber - (((page header: 4) land: 511) = 0
		  ifTrue: [1]
		  ifFalse: [ "full last page"
			0]) max: 1)]
	  ifFalse:
		[file error = NotFound
		  ifTrue: [^false].
		^file error: 'open ' + (file errorString: file error)]
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
userName | |
	(userName ≡ nil or: [userName empty])
	  ifTrue: [^super userName].
	^userName
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
noAck | |
	isocket noAck "an optimization for intense ether activity"
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
name: t1 password: t2 | |
	userName ← t1.
	userPassword ← t2
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
socket | |
	^isocket
! !
!ILFileDirectory methodsFor: 'undefined' stamp: ''!
allocatePage | |
	isocket ≡ nil
	  ifTrue: [self open].
	^isocket freePacket
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
pageNumber: pn | |
	pn ← pn - 1 "inverse of pageNumber. set 5 mode bits to 0".
	self header: 3 ← pn / 128 "lshift: ¬7, except for large pn".
	self header: 4 ← pn lshift: 9
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
pageNumber | |
	^((self header: 3) "extract page number from 27-bit byte address. ignore high 5 mode bits.
	dividing byte adress (which may be a LargeInteger) by self dataLength (512)
	is the correct but slow way to do this. byte address 0 = page 1" land: 2047) * 128 "lshift: 7, maybe large" + ((self header: 4) lshift: ¬9) + 1
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
length | |
	^self header: 5 "valid for read or write"
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
headerLength | |
	^34 "4+20+10"
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
length: len | |
	self header: 1 ← 10 + len "10 bytes in a normal header block. ILParameterBlock can change things.
	also negative lengths can shorten it and change pupLength" "length of command block, including data" "number of data bytes to read/write".
	self header: 5 ← len "set pupLength".
	super length: len
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
serialNumber: sn | |
	self header: 2 ← sn word: 2
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
serialNumber | sn |
	sn ← String new: 4.
	sn ◦ 1 ← sn ◦ 2 ← 0.
	sn word: 2 ← self header: 2.
	^sn
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
command: com | |
	page pupType← LeafType "operation word (header 1)
		0-4
			opcode
		5
			0 request
			1 answer
		6-15
			inclusive byte length of operation block" "make operation a request, preserve length set earlier".
	page ◦ 25 ← (page ◦ 25 land: 3) + (com lshift: 3) "left half pupID1".
	page ◦ 11 ← (com = Reset
	  ifTrue: [OpenConnection]
	  ifFalse:
		[com = Quit
		  ifTrue: [DestroyConnection]
		  ifFalse: [Data]])
! !
!ILFilePage methodsFor: 'undefined' stamp: ''!
mode: m | |
	page ◦ 29 ← (page ◦ 29 "current meaning of 5 read/write bits (from high to low):
		0-1
			0 read or write anywhere
			1 no holes (read or write) zeros past end
			2 don't extend on read or write
			3 check extend (Error)
		2
			1 set eof
		3-4
			0 undefined" "m is a byte which is already shifted" land: 7) + m
! !
!ILParameterBlock methodsFor: 'undefined' stamp: ''!
position: t1 | |
	position ← t1.
	packet pupLength← position - 2 "changing length of data in packet" "set inclusive byte length in first command word; rest set later".
	packet word: 13 ← position - 24
! !
!ILParameterBlock methodsFor: 'undefined' stamp: ''!
nextString← s | strm |
	strm ← packet pupString asStream.
	strm skip: position.
	strm nextword← s length.
	strm append: s.
	strm padNext← 0.
	self position: strm position
! !
!ILParameterBlock methodsFor: 'undefined' stamp: ''!
nextword← w | strm |
	strm ← packet pupString asStream.
	strm skip: position.
	strm nextword← w.
	self position: strm position
! !
!ILParameterBlock methodsFor: 'undefined' stamp: ''!
packet← t1 | |
	packet ← t1.
	self position: 26 "default is an empty (data) packet, except first header word"
! !
!ILSocket methodsFor: 'undefined' stamp: ''!
net: n host: h | |
	seqNum ← 0 "usually called by hostName:".
	super net: n host: h soc: 35.
	self retransmit: 8 every: (n = NETNUM
	  ifTrue: [400]
	  ifFalse: [ "same net"
		1800]).
	self setAck
! !
!ILSocket methodsFor: 'undefined' stamp: ''!
setAck | |
	outAck ← self freePacket "create acknowledgement packet".
	outAck pupType← LeafType.
	outAck dataString← '' "control code".
	outAck ◦ 11 ← Ack.
	self setAddresses: outAck
! !
!ILSocket methodsFor: 'undefined' stamp: ''!
noAck | |
	outAck ← false "if acknowledgements are not needed"
! !
!ILSocket methodsFor: 'undefined' stamp: ''!
sendPacket: t1 | nseq |
	outPac ← t1.
	result ← abortTransfer ← false "send a packet, wait for result, and acknowledge.
	RetransmitSocket setAddressesAndComplete: sets timer
	ILSocket socProcess: receives answers
	ILSocket timerFired handles retransmissions" "alloc, receiver seq".
	outPac pupID0← outPac ◦ 12 ← seqNum "command, send seq" "pupID1 ← (outPac pupID1 land: 0177400) +".
	self setAddressesAndComplete: outPac "while waiting for packet to arrive, set up ack".
	nseq ← seqNum + 1 \ 256.
	outPac ◦ 11 = DestroyConnection
	  ifTrue:
		[ "assume reply will be Dally. send Quit to shut down connection a little faster"
		outAck
		  ifFalse:
			[ "create ack"
			self setAck.
			outAck pupID0← outAck ◦ 12 ← seqNum].
		outAck ◦ 11 ← Quit.
		retransMax ← 0 "same sequence numbers as previous"]
	  ifFalse:
		[outAck
		  ifTrue: [outAck pupID0← outAck ◦ 12 ← nseq "acknowledgement for Open & Data"] "in rapid interchanges, the next request is an implicit ack, so it's faster not to send one. however, packets unacknowledged for ~5 secs. at the server end can cause degraded performance"] "now wait for socProcess: to set result, or timerFired to set abortTransfer".
	[result or: [abortTransfer]] whileFalseDo:  [].
	result
	  ifTrue:
		[seqNum ← nseq.
		outAck
		  ifTrue: [self completePup: outAck "send ack"]].
	^result
! !
!ILSocket methodsFor: 'undefined' stamp: ''!
socProcess: Ipac | |
	(Ipac ◦ 12 "check if this is a packet we want. normally left half of pupID1 = Data (0).
	in case of DestroyConnection, control code might be Dallying (10)" "pupID1" = seqNum and: [Ipac pupType = LeafType])
	  ifTrue:
		[self timerOff "turn off timer, save result".
		result ← Ipac]
	  ifFalse: [ "recycle packet"
		self freePacket: Ipac]
! !
!ILSocket methodsFor: 'undefined' stamp: ''!
timerFired | |
	(result or: [abortTransfer]) "timer has fired -- retransmit or abort"
	  ifFalse:
		[ "bug?--timer may not have been disabled yet"
		self timerOn
		  ifTrue: [self completePup: outPac "retransmit"]
		  ifFalse: [ "abort"
			abortTransfer ← true]]
! !
!Image methodsFor: 'undefined' stamp: ''!
ground: t1 | |
	ground ← t1 "change the ground color (color associated with white) for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
figure: t1 | |
	figure ← t1 "change the figure color (color associated with black) for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
ground | |
	^ground "return the ground color (color associated with white) for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
figure | |
	^figure "return the figure color (color associated with black) for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
ygrid: t1 | |
	ygrid ← t1 "set the y gridding module for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
ygrid | |
	^ygrid "return the y gridding module for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
path: t1 | |
	path ← t1 "change the path for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
form: t1 | |
	form ← t1 "change the form for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
xgrid | |
	^xgrid "return the x gridding module for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
xgrid: t1 | |
	xgrid ← t1 "set the x gridding module for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
hash | |
	^rectangle hash
! !
!Image methodsFor: 'undefined' stamp: ''!
superimage: t1 | |
	superimage ← t1
! !
!Image methodsFor: 'undefined' stamp: ''!
path | |
	^path "return the path for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
form | |
	^form "return the form for this Image"
! !
!Image methodsFor: 'undefined' stamp: ''!
add: p and: i | s |
	rectangle ← rectangle include: ((Rectangle new "add p (set or point) and i ( Image or Form ) and expand the
		 bounding rectangle of this image." origin: p origin + origin extent: i size) include: (Rectangle new origin: p corner + origin extent: i size)).
	s ← Set default.
	s add: p.
	s add: i.
	self add: s
! !
!Image methodsFor: 'undefined' stamp: ''!
addpath: p andform: f | r |
	rectangle ← rectangle include: (r ← (Rectangle new "add p (set or point) and f ( Form ) and expand the
		 bounding rectangle of this image." origin: p origin extent: f extent "+ origin") include: (Rectangle new origin: p corner - (1 ⌾ 1) extent: f extent "+ origin")).
	self add: (Image new origin: 0 ⌾ 0 rectangle: r path: p form: f figure: 1 ground: 0 xgrid: 1 ygrid: 1)
! !
!Image methodsFor: 'undefined' stamp: ''!
addform: f andpath: p | r |
	self addpath: p andform: f "add p (set or point) and f ( Form ) and expand the
		 bounding rectangle of this image."
! !
!Image methodsFor: 'undefined' stamp: ''!
addimage: i | r |
	rectangle ← rectangle include: (Rectangle new "add the Image i (as a subimage) and expand the
		 bounding rectangle of this image." origin: i origin + origin extent: i extent).
	self add: i
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
comment | |
	 "see class Set for operations (deletion,replacement,insertion etc.) on subimages ( elements)."
! !
!Image methodsFor: 'undefined' stamp: ''!
outlinesubimage: i | r |
	r ← Rectangle new "draw an outline(reversed boarder 2 units thick) about the ith subimage ." origin: self origin + (self ◦ i) origin extent: (self ◦ i) extent.
	r comp
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
indexofsubimagebelow: yvalue | i subimage |
	(1 to: self length "return the index of the first subimage below yvalue otherwise return false.") do:
		[:i | (self ◦ i) top ≥ yvalue
		  ifTrue: [^i]].
	^false
! !
!Image methodsFor: 'undefined' stamp: ''!
replaceimage: image with: newimage | i |
	i ← self findbyrect: image "replace image with newimage in self.".
	self replaceI: i value: newimage
! !
!Image methodsFor: 'undefined' stamp: ''!
appendimage: newimage after: image | i |
	i ← self findbyrect: image "append newimage into the image after image.".
	self insertI: i + 1 value: newimage
! !
!Image methodsFor: 'undefined' stamp: ''!
indexofsubimageat: pt | i subimage |
	(1 to: self length "return the index of the subimage which contains pt(relative to self origin)
		otherwise return false.") do:
		[:i | subimage ← self ◦ i.
		(subimage rectangle has: pt)
		  ifTrue: [^i]].
	^false
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
highlite | r i |
	(1 to: position) do:
		[:i |  "reverse the ith subimage ."
		r ← Rectangle new origin: self origin + (self ◦ i) origin extent: (self ◦ i) extent.
		r comp]
! !
!Image methodsFor: 'undefined' stamp: ''!
substitute: form1 for: form2 | i |
	(1 to: self length "everywhere in the imagesubstitute form1 for form2") do:
		[:i | self ◦ i ◦ 2 ≡ form2
		  ifTrue: [self ◦ i ◦ 2 ← form1]]
! !
!Image methodsFor: 'undefined' stamp: ''!
subimageat: pt | i |
	i ← self indexofsubimageat: pt "return the  subimage which contains pt (relative to self origin)
		otherwise return false.".
	i
	  ifTrue: [^self ◦ i].
	^false
! !
!Image methodsFor: 'undefined' stamp: ''!
subimage: i | sub s |
	sub ← self ◦ i "return the ith subimage." s← Image new at: self origin + (sub ◦ 1) origin.
	s add: 0 ⌾ 0 and: [sub ◦ 2].
	^s
! !
!Image methodsFor: 'undefined' stamp: ''!
deletesubimage: i | |
	self deleteindex: i "delete the ith subimage."
! !
!Image methodsFor: 'undefined' stamp: ''!
deleteimage: i | subimage |
	subimage ← self ◦ i "delete the i th subimage and recompute the bounding rectangle of the Image".
	self deleteI: i.
	(subimage rectangle isWithin: rectangle)
	  ifFalse: [self resize "recompute bounding rectangle"]
! !
!Image methodsFor: 'undefined' stamp: ''!
findbyrect: image | i |
	(1 to: position) do:
		[:i | (array ◦ i) rectangle = image rectangle
		  ifTrue: [^i]].
	^0
! !
!Image methodsFor: 'undefined' stamp: ''!
blink | |
	self display: 2 "blink the image".
	self display: 2
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
display: effect | |
	self displayat: 0 ⌾ 0 "display all of the forms in the image on the screen 
	effect = 0 ⇒ store
	effect = 1 ⇒ or
	effect = 2 ⇒ xor
	effect = 3 ⇒ and complement
" effect: effect clippedBy: user screenrect
! !
!Image methodsFor: 'undefined' stamp: ''!
displayat: pt effect: effect clippedBy: cliprect | i |
	 "display all of the subimages in this image "
	nil ≡ form
	  ifFalse: [form displayat: path + pt + origin effect: effect clippedBy: cliprect].
	(1 to: self length) do: [:i | self ◦ i displayat: pt + origin effect: effect clippedBy: cliprect]
! !
!Image methodsFor: 'undefined' stamp: ''!
display | |
	self displayat: 0 ⌾ 0 "display all of the forms in the image on the screen " effect: 0 clippedBy: user screenrect
! !
!Image methodsFor: 'undefined' stamp: ''!
close | im |
	array ≡ nil
	  ifFalse: [self asArray notNil do: [:im | im close]].
	superimage ← nil.
	form ← nil.
	self vector: 0
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
blink: t1 | pt |
	form ← t1.
	pt ← self mp "to show current gridded position of the form... returns abs position.".
	form displayat: rectangle origin + pt effect: 2 clippedBy: user screenrect.
	form displayat: rectangle origin + pt effect: 2 clippedBy: user screenrect.
	^self rectangle origin + pt
! !
!Image methodsFor: 'undefined' stamp: ''!
fromuser | |
	rectangle ← Rectangle new "create a new Image whose rectangle is specified by the user. " fromuser.
	self origin: rectangle origin rectangle: rectangle path: rectangle origin form: (Form new fromrectangle: rectangle) figure: 1 ground: 0 xgrid: 1 ygrid: 1
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
origin: t1 extent: extent | |
	origin ← t1.
	self origin: origin copy "create a new Image at origin with extent (width⌾height). " rectangle: (Rectangle new origin: origin extent: extent) path: nil form: nil figure: 1 ground: 0 xgrid: 1 ygrid: 1
! !
!Image methodsFor: 'undefined' stamp: ''!
origin: t1 rectangle: t2 path: t3 form: t4 | |
	origin ← t1.
	rectangle ← t2.
	path ← t3.
	form ← t4.
	self origin: origin rectangle: rectangle path: path form: form figure: 1 ground: 0 xgrid: 1 ygrid: 1 "basic message to create a new instance."
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
rectangle | |
	^rectangle "return the rectangle that bounds the Image."
! !
!Image methodsFor: 'undefined' stamp: ''!
leftside | |
	^rectangle origin "return the leftmost x of the bounding rectangle of theImage." x
! !
!Image methodsFor: 'undefined' stamp: ''!
width | |
	^rectangle extent "return the width of the Image." x
! !
!Image methodsFor: 'undefined' stamp: ''!
corner← pt | |
	rectangle corner← pt "modify the corner of the Image."
! !
!Image methodsFor: 'undefined' stamp: ''!
top | |
	^rectangle origin "return the top y of the bounding rectangle of theImage." y
! !
!Image methodsFor: 'undefined' stamp: ''!
resize | i |
	 " Recompute the bounding rectangle of the Image"
	nil ≡ form
	  ifTrue: [rectangle ← Rectangle new origin: origin extent: 1 ⌾ 1]
	  ifFalse: [rectangle ← Rectangle new origin: origin extent: form extent].
	(1 to: self length) do: [:i | rectangle ← rectangle include: (self ◦ i) rectangle]
! !
!Image methodsFor: 'undefined' stamp: ''!
center | |
	^rectangle center "return the center of the Image."
! !
!Image methodsFor: 'undefined' stamp: ''!
origin: t1 | |
	origin ← t1 "change the origin of the image."
! !
!Image methodsFor: 'undefined' stamp: ''!
bottom | |
	^rectangle corner "return the bottom y of the bounding rectangle of theImage." y
! !
!Image methodsFor: 'undefined' stamp: ''!
height | |
	^rectangle extent "return the height of the Image." y
! !
!Image methodsFor: 'undefined' stamp: ''!
corner | |
	^rectangle corner "return the corner of the Image."
! !
!Image methodsFor: 'undefined' stamp: ''!
origin | |
	^origin "return the origin of the image."
! !
!Image methodsFor: 'undefined' stamp: ''!
extent | |
	^rectangle extent "return the extent (width,height) of the Image."
! !
!Image methodsFor: 'undefined' stamp: ''!
rectangle: r | |
	rectangle ← r "redefine rectangle that bounds the Image."
! !
!Image methodsFor: 'undefined' stamp: ''!
contains: pt | |
	^rectangle has: pt "return true if the  bounding rectangle for the Image contains pt.."
! !
!Image methodsFor: 'undefined' stamp: ''!
rightside | |
	^rectangle corner "return the rightmost x of the bounding rectangle of theImage." x
! !
!Image methodsFor: 'undefined' stamp: ''!
= image | |
	^rectangle = image rectangle
! !
!Image methodsFor: 'undefined' stamp: ''!
superimage | |
	^superimage "return the superimage (Image containing) of this Image."
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
boxcomp | |
	rectangle comp "border without disturbing the interior."
! !
!Image methodsFor: 'undefined' stamp: ''!
reverse | |
	self color: black effect: 2 "reverse  the image (black to white and white to black)"
! !
!Image methodsFor: 'undefined' stamp: ''!
gray | |
	self color: gray effect: storing "gray out the image"
! !
!Image methodsFor: 'undefined' stamp: ''!
white | |
	(form is: BorderedText) "white out the image"
	  ifTrue: [(rectangle inset: ¬1 ⌾ ¬1 and: [¬1 ⌾ ¬1]) clear: 0]
	  ifFalse:
		[self color: white effect: over]
! !
!Image methodsFor: 'undefined' stamp: ''!
black | |
	self color: black effect: 0 "black out the image"
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
printOn: strm | t |
	strm append: 'an Image: '.
	(array is: String)
	  ifTrue: [strm space append: self]
	  ifFalse: [self do: [:t | strm space print: t]]
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
pressCode | |
	^1
! !
!Image methodsFor: 'undefined' stamp: ''!
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
!Image methodsFor: 'undefined' stamp: ''!
kbd | |
	user kbd " default response for Images.".
	self reverse.
	self reverse
! !
!Image methodsFor: 'undefined' stamp: ''!
copy | im i |
	im ← Image new origin: origin copy rectangle: rectangle copy path: path copy form: form copy figure: figure copy ground: ground copy xgrid: xgrid copy ygrid: ygrid copy.
	(1 to: self length) do: [:i | im add: (self ◦ i) copy].
	^im
! !
!Image methodsFor: 'undefined' stamp: ''!
mp | p |
	p ← user mp " returns a gridded point relative to my rectangle.".
	p x← p x - rectangle origin x | xgrid.
	p y← p y - rectangle origin y | ygrid.
	^p
! !
!Image methodsFor: 'undefined' stamp: ''!
griddedpoint: pt | |
	^pt x | xgrid ⌾ (pt y | ygrid)
! !
!Image methodsFor: 'undefined' stamp: ''!
translateto: pt | |
	self translate: pt - origin "move the Image to pt."
! !
!Image methodsFor: 'undefined' stamp: ''!
translate: delta | |
	rectangle ← rectangle translate: delta "translate the origin and bounding rectangle of the Image.".
	origin translate: delta
! !
!Image methodsFor: 'undefined' stamp: ''!
normalize | delta i |
	 "recompute origin, rectangle and path so that: path origin = 0⌾0."
	nil ≡ path
	  ifFalse:
		[delta ← path origin copy.
		path normalize.
		origin translate: delta].
	(1 to: self length) do: [:i | (self ◦ i) normalize]
! !
!InspectWindow methodsFor: 'undefined' stamp: ''!
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
!InspectWindow methodsFor: 'undefined' stamp: ''!
classInit | |
	stdTemplates ← {(0 ⌾ 0 rect: 12 ⌾ 36) , (12 ⌾ 0 rect: 36 ⌾ 36)}
! !
!InspectWindow methodsFor: 'undefined' stamp: ''!
separator: c | |
	 "called by of: via Class fieldNamesInto"
! !
!InspectWindow methodsFor: 'undefined' stamp: ''!
identifier: s | |
	 "called by of: via Class fieldNamesInto"
	variables next← s
! !
!InspectWindow methodsFor: 'undefined' stamp: ''!
trailer: s | |
	 "called by of: via Class fieldNamesInto"
! !
!InspectWindow methodsFor: 'undefined' stamp: ''!
comment: s | |
	 "called by of: via Class fieldNamesInto"
! !
!InspectWindow methodsFor: 'undefined' stamp: ''!
contents | |
	 "called by of: via Class fieldNamesInto"
! !
!Integer methodsFor: 'undefined' stamp: ''!
gcd: n | m d |
	 "Greatest common divisor, R. Silver's algorithm"
	m ← self abs max: n abs.
	n ← self abs min: n abs.
	(m\\n) = 0 ifTrue: [^n].  "easy test, speeds up rest"
	d ← 1.
	[n even and: [m even]] whileTrueDo:
		[d ← d * 2.
		n ← n//2.
		m ← m//2].
	[n even] whileTrueDo: [n ← n // 2].
	[m even] whileTrueDo: [m ← m // 2].
	[m = n] whileFalseDo:
		[m > n ifTrue: [m ← m-n] ifFalse: [m ← n-m].
		[m even] whileTrueDo: [m ← m // 2]].
	^m * d
! !
!Integer methodsFor: 'undefined' stamp: ''!
| arg | |
	 "truncate"
	^self / arg * arg
! !
!Integer methodsFor: 'undefined' stamp: ''!
lcm: n | |
	 "least common multiple"
	^(self // (self gcd: n) * n) asInteger
! !
!Integer methodsFor: 'undefined' stamp: ''!
isIntegral | |
	^true
! !
!Interval methodsFor: 'undefined' stamp: ''!
isIntervalBy1 | |
	^step = 1
! !
!Interval methodsFor: 'undefined' stamp: ''!
hash | |
	^(((start lshift: 2) lxor: stop) lshift: 1) lxor: length
! !
!Interval methodsFor: 'undefined' stamp: ''!
= int | |
	^start = int start and: [(stop = int stop and: [length = int length])]
! !
!Interval methodsFor: 'undefined' stamp: ''!
cansubscript: a | |
	^length ≤ 0 or: [((start cansubscript: a) and: [(length - 1 * step + start cansubscript: a)])]
! !
!Interval methodsFor: 'undefined' stamp: ''!
from: t1 to: t2 by: t3 | |
	start ← t1.
	stop ← t2.
	step ← t3.
	length ← 1 + (stop - start / step).
	step < 0
	  ifTrue:
		[start < stop
		  ifTrue: [length ← 0]]
	  ifFalse:
		[stop < start
		  ifTrue: [length ← 0]]
! !
!Interval methodsFor: 'undefined' stamp: ''!
randomInit | |
	self randomInit: user ticks
! !
!Interval methodsFor: 'undefined' stamp: ''!
random | |
	 "See Lehmers linear congruential method, Knuth Vol. 1:
	modulus m=2^16
	a=27181 odd, and 5 = a mod 8
	c=13849 odd, and c/m around 0.21132"
	step ← (13849 + (27181 * step)) asInt16.
	^(start + (length asFloat * (32768.0 + step) / 65536.0)) asSmall
! !
!Interval methodsFor: 'undefined' stamp: ''!
randomInit: x | |
	 "Call with const to get repeatable sequence"
	step ← x "step holds the current state".
	(start is: Float)
	  ifTrue: [length ← stop - start] "for Float intervals"
! !
!Interval methodsFor: 'undefined' stamp: ''!
◦ x | |
	((x isNumber) and: [x≥1 and: [x≤length]])
	  ifTrue: [^start + (step * (x - 1))]
	  ifFalse: [^super ◦ x]
! !
!Interval methodsFor: 'undefined' stamp: ''!
do: aBlock | var |
	var← start-step.
	step>0
		ifTrue: [[(var← var+step)≤stop] whileTrueDo: [aBlock value: var]]
		ifFalse: [[(var← var+step)≥stop] whileTrueDo: [aBlock value: var]]
! !
!Interval methodsFor: 'undefined' stamp: ''!
start | |
	^start
! !
!Interval methodsFor: 'undefined' stamp: ''!
stop | |
	^stop
! !
!Interval methodsFor: 'undefined' stamp: ''!
length | |
	^length
! !
!Interval methodsFor: 'undefined' stamp: ''!
◦ x ← val | |
	user notify: 'Intervals are not for writing into'
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
implementationNotes | |
	 "
FIELDS
fLongFileHandle : a LongInteger representing the unique identifier for the file.
fShortFileHandle : an Integer representing the identifier for the file during a particular transaction.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
writePage: pPageBuffer | tRequest |
	 "...sends the data buffered in pPageBuffer (a JuniperPageBuffer) to the Juniper file system provided the buffer is not empty."
	pPageBuffer length = 0
	  ifFalse:
		[ "1"
		tRequest ← JuniperRequestParameterBlock new "2".
		tRequest packet← pPageBuffer page "3".
		tRequest leader: 1 ← 0.
		tRequest leader: 2 ← 0 "4".
		tRequest parameter: 1 ← pPageBuffer pageNumber - 1 "5".
		self doAction: sWritePage requestPrs: tRequest "6"] "
1. If there is no data in pPageBuffer do nothing.
2. Create a new request parameter block.
3. Set the packet to that of pPageBuffer.  This contains the data to be written on the Juniper file.
4. Set the authentication key and reserved word to 0.
5. Set the page number parameter to that of pPageBuffer less 1.  Juniper page numbers start at 0.
6. Issue a 'write page' request.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
open | tRequest tResult |
	 "...opens the file on the Juniper file system."
	tRequest ← self newRequestParameterBlock "1".
	tRequest nextDataBlockString← fLongFileHandle "2".
	tResult ← self interface doAction: sOpenFile requestPrs: tRequest "3".
	fShortFileHandle ← tResult parameter: 1 "4".
	self findLastPage "5" "
1. Create a new request parameter block.
2. Set the long file handle parameter.
3. Issue an 'open file' command to Juniper.
4. Set the superClass field serialNumber to the short file handle returned by Juniper in the result parameter block.
5. Set the number of the last page of the file.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
Write: pPageBuffer | tPageNumber |
	 "...adjusts the length of the file if pPageBuffer is the last page, sends the data buffered in pPageBuffer (a JuniperPageBuffer) to the Juniper file system, and returns pPageBuffer."
	tPageNumber ← pPageBuffer pageNumber.
	tPageNumber < lastpn
	  ifFalse:
		[ "1"
		tPageNumber > (lastpn + 1)
		  ifTrue: [^self error: 'invalid page number'].
		 "2"
		self length: tPageNumber - 1 * pPageBuffer dataLength + pPageBuffer length "3"].
	self writePage: pPageBuffer "4".
	^pPageBuffer "5" "
1. If the page number of pPageBuffer is less than the last page number of the file, do not readjust the file length.
2. If the page number of pPageBuffer is greater than the next available page number for the file, invoke error handling.
3. If pPageBuffer is the last page of the file or will immediately follow the last page of the file, adjust the file length.
4. Send the data to Juniper.
5. Return pPageBuffer unmodified.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
lastFullPage | |
	^self length / self entryClass new dataLength
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
length | tResult |
	 "...returns the number of bytes (an Integer) in the file."
	tResult ← self doAction: sReadLength requestPrs: nil "1".
	^tResult longInteger: 1 "2" "
1. Issue a 'read length' command to Juniper.
2. Return the length from the result parameter block.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
close | |
	 "...closes the file on the Juniper file system."
	self doAction: sCloseFile requestPrs: nil
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
Get: pPageBuffer | tNextPageNumber tNewPageNumber i |
	 "...reads the data from the page whose number is that specified by pPageBuffer (a JuniperPageBuffer) if such a page exists.  Otherwise pPageBuffer should contains data to be written on the file at the appropriate place."
	tNewPageNumber ← pPageBuffer pageNumber.
	(tNewPageNumber ≤ lastpn and: [(lastpn > 1 or: [self length > 0])])
	  ifTrue: [^self Read: pPageBuffer].
	tNextPageNumber ← lastpn + 1.
	tNewPageNumber = tNextPageNumber
	  ifFalse:
		[self length: tNewPageNumber - 1 * pPageBuffer dataLength.
		(tNextPageNumber to: tNewPageNumber - 1) do:
			[:i | pPageBuffer pageNumber: i.
			pPageBuffer ← self writePage: pPageBuffer]].
	pPageBuffer pageNumber: tNewPageNumber.
	pPageBuffer length: 0.
	^pPageBuffer
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
entryClass | |
	 "...returns the class of objects managed by JuniperFileController objects."
	^JuniperPageBuffer
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
Read: pPageBuffer | tRequest tResult pn |
	 "...returns false if the page number of pPageBuffer (a JuniperPageBuffer) is greater than the page number of the last page of the file; otherwise, the data for that page is read from the Juniper file system into the packet of pPageBuffer."
	(pn ← pPageBuffer pageNumber) > lastpn
	  ifTrue: [^false].
	 "1"
	(lastpn = 1 and: [self empty])
	  ifTrue:
		[pPageBuffer length: 0 "⇑ self Get: pPageBuffer"]
	  ifFalse:
		[tRequest ← self newRequestParameterBlock "2".
		tRequest parameter: 1 ← pn - 1 "3".
		tResult ← self doAction: sReadPage requestPrs: tRequest "4".
		pPageBuffer page: tResult packet "5".
		pPageBuffer length: (tResult parameter: 1) "6"].
	^pPageBuffer "7" "
1. Return false if the page number of pPageBuffer is greater than that of the last page of the file.
2. Create a new request parameter block.
3. Set the page number parameter to that of pPageBuffer less 1.  Juniper page numbers start at 0.
4. Issue a 'read page' request.
5. Set the pPageBuffer packet to that of the result parameter block.
6. Set the length of pPageBuffer to that returned in the result parameter block.
7. Return pPageBuffer modified.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
endFile: pPageBuffer | |
	 "...adjusts the file length, writes pPageBuffer (a JuniperPageBuffer) on the file, and returns pPageBuffer."
	self length: pPageBuffer pageNumber - 1 * pPageBuffer dataLength + pPageBuffer length "1".
	self writePage: pPageBuffer "2".
	^pPageBuffer "
1. Set the file length to the total number of bytes in the file.
2. Write the page on Juniper.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
length: pLength | tRequest |
	 "...sets the number of bytes in the file to the integer pLength (an Integer)."
	tRequest ← self newRequestParameterBlock "1".
	tRequest longInteger: 1 ← pLength "2".
	self doAction: sSetLength requestPrs: tRequest "3".
	lastpn ← self pageFrom: pLength "4" "
1. Create a new request parameter block.
2. Set the length parameter to pLength.
3. Issue a 'set length' command to Juniper.
4. Determine the number of the last page of the file and assign it to the superClass field lastpn.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
allocatePage | |
	 "...returns a new packet (Pacbuf) to be used as the data buffer in a JuniperPageBuffer."
	^self interface newPacket
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
newRequestParameterBlock | |
	 "...initializes and returns a new JuniperRequestParameterBlock."
	^self interface newRequestParameterBlock
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
doAction: pAction requestPrs: pRequest | |
	 "...the specified request, pAction, (selected from gJuniperConstants) with its corresponding request parameter block, pRequest ( a JuniperRequestParameter Block), is issued to the Juniper file server (through the file interface).  If no errors are found, a JuniperResultParameterBlock is returned; otherwise, error handling is invoked.  If no request parameters are required, pRequest can be specified as nil."
	pRequest ≡ nil
	  ifTrue:
		[pRequest ← self newRequestParameterBlock] "1".
	pRequest shortFileHandle← fShortFileHandle "2".
	^self interface doAction: pAction requestPrs: pRequest "3" "
1. Create a new request parameter block if one is not specified.
2. Set the short file handle parameter.
3. Issue the request to the Juniper interface and return the result parameter block.
"
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
longFileHandle: pLongFileHandle | |
	 "...sets the file's long file handle."
	fLongFileHandle ← pLongFileHandle
! !
!JuniperFileController methodsFor: 'undefined' stamp: ''!
interface | |
	 "...returns the JuniperInterface controlling the file."
	^directory
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
classInit | tIndex tConstant |
	 "...initializes the constants in gJuniperConstants
(see [ivy] <Juniper>4.4> CommonPineDefs.mesa).
Do the following to initialize:
Smalltalk declare: ↪gJuniperConstants as: (SymbolTable new init: 256).
JuniperInterface classInit."
	self juniperConstants do:
		[:tConstant | (tConstant Is: Integer)
		  ifTrue:
			[tIndex ← tConstant]
		  ifFalse:
			[gJuniperConstants declare: tConstant as: tIndex.
			tIndex ← tIndex + 1]]
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
juniperConstants | |
	 "...returns the set of constants used to specify requests to and to  interpret results from the Juniper file system."
	^#(168 sRequest sResult sUnsolicited sCustodian sSync sPineAck sNoop 5 sReadPage sWritePage sSetLength sReadLength sCloseFile sDestroyAnonymousFile sReadData sWriteData sReadAttribute sWriteAttribute sSetWriteLock sSetReadLock sReleaseReadLock 32 sChangePassword sRoom sOpenFile sCreateAnonymousFile sFindFile sLockQuery sTransCompletionQuery 42 sLogout sCloseTransaction sAbortTransaction sLoginRequest 60 sLookupFile sCreateFile sDestroyFile sRenameFile sNextFile sNextFewFiles 1 sCommandNak sCommandAck sHeresData sHeresEntry sHeresFileList sHeresLFH sHeresFile sHeresLength sLoginResponse sLogoutResponse sTransactionClosed sTransCompletionInfo sResourceData sHeresRoom 0 sTransactionAborted sReadLockBroken 8 sLogin sAddServer sResourceLocation 42 sUnimplementedFeature sIllegalLoginAttempt sSoftwareVersionMismatch sNoSuchUser sPasswordMismatch sBytesPerPageUnacceptable sServerTooBusy sOutOfSpace sNoFilesHere sNoDirectoryHere sIllegalFileName sFileNotFound sFileAlreadyExists sTransactionAborting sNoSuchOpenFile sBigFilesNotImplemented sIllegalAttribute sReadAttributeProtectionError sUserAskedForIt sWriteAttributeProtectionError sPackNotOnDrive sTooManyOpenFiles sProtectionViolation sFileNotOnPack sPackFull sFileRefOutOfBounds sFileSizeExcessive sByteRangeExcessive sPackNotOnMachine sNoSuchTransaction sBrokenLock sInconvenientUnwind sSequenceNumberGap sRecoveryUnderWay sTransactionClosing sCongestion 120 sBlockTooLarge sNoRouteToServer sServerUnknown sBadBytesPerPage sInvalidRequest sPupGlitch sUnableToDecrypt sNoResponseToRequest sNewServerUnwilling sTransactionInUnknownState sBadTimeForRequests ) "PineMsgType (Pup types)" "DataRequest" "TransactionRequest" "ActivityRequest" "DirectoryRequest" "ResultCode" "Unsolicited Code" "CustodianCode" "PineErrorCode" "TransTroubleCode" "Retryable" "UserDetected"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
implementationNotes | |
	 "
FIELDS
fName : a String representing the name of an account on the Juniper file system.
fPassword : a String representing the password of the same account.
fDefaultDirectory : a String representing the directory to be used as the default if one is not specified as part of a file name.
fJuniperSocket : a JuniperSocket used to interface to the etherWorld mechanism.
fTimer : a Timer used to periodically send noop commands when the interface is open to prevent timeout.
fExceptionHandler : an ExceptionHandler to invoke when a transaction is aborted.
fSpecialError : an error code to ignore on a command request.
fOpenIndicator : the interface status: true means open; nil means not open.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
Insert: pFile | tRequest tResult |
	 "...issues a 'create file' request to the Juniper file server and sets the returned long file handle and name in pFile (a JuniperFileController)."
	tRequest ← self newRequestParameterBlock "1".
	tRequest longInteger: 2 ← 0 "user rawtotalsecs" "1.5".
	tRequest nextDataBlockString← self checkDirectory: pFile name "2".
	tRequest nextDataBlockString← '' "3".
	tResult ← self doAction: sCreateFile requestPrs: tRequest "4".
	pFile longFileHandle: tResult nextDataBlockString.
	pFile name: tResult nextDataBlockString "5" "
1. Create a new request parameter block.
1.5 Set creation date (to default (current) -- later some specific date&time?)
2. Get the file name from pFile, add the default directory name to it if necessary, and write it in the request parameter block.
3. Blank the file server field of the request parameter block.
4. Issue a 'create file' request and get the result parameter block.
5. Set the long file handle and name (from the result parameter block) in pFile.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
Delete: pFile | tRequest |
	 "...deletes from the Juniper file system the file whose file name is specified by pFile (a JuniperFileController)."
	tRequest ← self newRequestParameterBlock "1".
	tRequest nextDataBlockString← self checkDirectory: pFile name "2".
	self doAction: sDestroyFile requestPrs: tRequest "3" "
1. Create a new request parameter block.
2. Get the file name from pFile, add the default directory name to it if necessary, and write it in the request parameter block.
3. Issue a 'destroy file' request.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
server | |
	^'Juniper1' "should be an instance variable?"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
Find: pFile | tRequest tResult |
	 "...sends a 'look up file' request to the Juniper file server.  If the file is found, its name and long file handle are set in pFile (a JuniperFileController). Otherwise false is returned."
	fSpecialError ← sFileNotFound "1".
	tRequest ← self newRequestParameterBlock "2".
	tRequest nextDataBlockString← self checkDirectory: pFile name "3".
	tResult ← self doAction: sLookupFile requestPrs: tRequest "4".
	fSpecialError ≡ true
	  ifTrue:
		[fSpecialError ← nil.
		^false].
	 "5"
	fSpecialError ← nil "6".
	pFile longFileHandle: tResult nextDataBlockString.
	pFile name: tResult nextDataBlockString "7" "
1. Set the error handling mechanism to ignore a 'file not found' error.
2. Create a new request parameter block.
3. Get the file name from pFile, add the default directory name to it if necessary, and write it in the request parameter block.
4. Issue a 'look up file' request and get the result parameter block.
5. If a 'file not found' error was encountered, reset the error handling mechanism and return false.
6. (no error encountered) Reset the error handling mechanism.
7. Set the long file handle and name (from the result parameter block) in pFile.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
Match: entries to: strm | entry pat ents name i p lastname |
	entries do:
		[:pat |  "search for Files matching patterns"
		name ← self checkDirectory: pat name.
		i ← name find: '*' ◦ 1.
		p ← name find: '#' ◦ 1.
		i ← (p = 0
				  ifTrue: [i]
				  ifFalse:
					[i = 0
					  ifTrue: [p]
					  ifFalse: [i min: p]]).
		i = 0
		  ifTrue:
			[(self Find: pat)
			  ifTrue: [strm next← pat "exact name found"]]
		  ifFalse:
			[ "pattern match over range of first to last possible matches"
			pat ← self makeEntry: name.
			entry ← self makeEntry: (name copy: 1 to: i - 1).
			lastname ← name copy: 1 to: i.
			lastname ◦ i ← 255.
			[(entry ← self nextFile: entry) and: [entry name < lastname]] whileTrueDo:
				[(pat match: entry)
				  ifTrue:
					[strm next← entry "copy entry since nextFile smashes into it".
					entry ← self makeEntry: entry name]]]]
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
nextFile: pFile | tRequest tResult |
	 "same as LookupFile (Find:), but it pertains to the file whose name is lexically next after the fileName specified in the request.  If the file is found, its name and long file handle are set in pFile (a JuniperFileController). Otherwise (no next file) false is returned."
	fSpecialError ← sFileNotFound "1".
	tRequest ← self newRequestParameterBlock "2".
	tRequest nextDataBlockString← self checkDirectory: pFile name "3".
	tResult ← self doAction: sNextFile requestPrs: tRequest "4".
	fSpecialError ≡ true
	  ifTrue:
		[fSpecialError ← nil.
		^false].
	 "5"
	fSpecialError ← nil "6".
	pFile longFileHandle: tResult nextDataBlockString.
	pFile name: tResult nextDataBlockString "7".
	^pFile
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
entryClass | |
	 "...returns the file class handled by the JuniperInterface."
	^JuniperFileController
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
Rename: oldFile from: newFile | tRequest newName |
	 "renames a file on Juniper"
	tRequest ← self newRequestParameterBlock.
	tRequest nextDataBlockString← oldFile name.
	tRequest nextDataBlockString← newName ← self checkDirectory: newFile name.
	self doAction: sRenameFile requestPrs: tRequest.
	oldFile name: newName
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
doAction: pAction requestPrs: pRequest | tResult |
	 "...corresponds to the Pine Protocol function.  The specified request, pAction, (selected from gJuniperConstants) with its corresponding request parameter block, pRequest (a JuniperRequestParameterBlock), is issued to the Juniper file server (through JuniperSocket sendRequest:). A packet is returned and inserted into a result parameter block and checked for error conditions.  If no errors are found, the result parameter block (a JuniperResultParameterBlock) is returned; otherwise, error handling is invoked.  If no request parameters are required, pRequest can be specified as nil."
	fOpenIndicator ≡ nil
	  ifTrue:
		[self open] "1".
	pRequest ≡ nil
	  ifTrue:
		[pRequest ← self newRequestParameterBlock] "2".
	fTimer disable "3".
	pRequest opcode← pAction "4".
	pRequest pupType← sRequest "5".
	tResult ← JuniperResultParameterBlock new "6".
	tResult packet← fJuniperSocket sendRequest: pRequest packet "7".
	fTimer reset "8".
	(self checkResult: tResult)
	  ifTrue: [^tResult].
	 "9"
	^self doAction: pAction requestPrs: pRequest "10" "
1. Make sure the JuniperInterface is open.
2. Create a request parameter block if none was specified.
3. Disable the timer to insure that a noop command is not sent while the current request is in progress.
4. Set the command code in the packet.
5. Set the packet pup type to 'request'.
6. Create a result parameter block.
7. Send the request to the JuniperSocket with the packet from the request parameter block; set the result packet in the result parameter block.
8. Reset the timer.
9. Check the result; if valid, return it.
10. If false is returned, then a retryable error was encountered so retry the command.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
checkResult: pResult | t2 |
	 "...is sent by doAction and doLogin to check the result of a Juniper request.  pResult is a result parameter block (JuniperResultParameterBlock) containing the packet (Pacbuf) returned as a result of the request."
	((pResult packet ≡ false or: [pResult pupType = 4]) or: [(pResult resultCode = sCommandNak and: [(pResult parameter: 1) = sTransactionAborting])])
	  ifTrue:
		[ "3"
		fExceptionHandler ≡ nil
		  ifFalse:
			[fExceptionHandler trap "4"].
		self error: 'No Juniper Transaction' "5"].
	(t2 ← pResult resultCode) = sCommandAck
	  ifTrue: [^true].
	 "6"
	t2 = sCommandNak
	  ifTrue:
		[ "7"
		(self checkRetry: (pResult parameter: 1))
		  ifTrue: [^false].
		 "8"
		(pResult parameter: 1) = fSpecialError
		  ifTrue:
			[fSpecialError ← true.
			^true].
		 "9"
		self error: pResult nextDataBlockString "10"]
	  ifFalse: [^true] "11" "
1. JuniperSocket sendRequest: and sendLogin: return false if no response is received from Juniper.
2. Invoke error handling if there is no response from Juniper.
3. Juniper returns an error pup (packet pup type  = 4) if the transaction has been aborted.
4. If the transaction has been aborted and there is an exception handler, then trap to the handler.
5. Invoke error handling if control is returned or if there is no exception handler.
6. Return true if a 'command acknowledged' was returned (this means success with no result parameters).
7. If a 'command not acknowledged' was returned,  an error condition exists.
8. Check for a retryable error and return false if so.
9. If the error is the same as that specified in the special error indicator, then indicate it and return true.
10. If not, invoke error handling.
11. Return true if a 'command not acknowledged' was not returned (this means success with result parameters).
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
error: pMessage | tMessage |
	 "...is sent by checkResult if an error is encountered on a Juniper request.  (pMessage is a String containing an appropriate error message.)  The error message is formed and a notify window is displayed."
	tMessage ← Stream default.
	tMessage append: pMessage.
	super error: tMessage contents
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
checkRetry: pErrorCode | |
	 "...is sent by checkResult to test for a retryable error.  Returns true if so; false otherwise."
	pErrorCode = sSequenceNumberGap
	  ifTrue: [^true].
	pErrorCode = sRecoveryUnderWay
	  ifTrue: [^true].
	pErrorCode = sTransactionClosing
	  ifTrue: [^true].
	pErrorCode = sCongestion
	  ifTrue: [^true].
	^false
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
newRequestParameterBlock | tRequestParameterBlock |
	 "...initializes and returns a new JuniperRequestParameterBlock."
	fJuniperSocket ≡ nil
	  ifTrue:
		[self open] "1".
	tRequestParameterBlock ← JuniperRequestParameterBlock new "2".
	tRequestParameterBlock packet← fJuniperSocket freePacket "3".
	tRequestParameterBlock dataBlockLength← 0 "4".
	tRequestParameterBlock leader: 1 ← 0.
	tRequestParameterBlock leader: 2 ← 0 "5".
	^tRequestParameterBlock "6" "
1. Open the interface if it is not already.
2. Create a new request parameter block.
3. Create a new packet and set it in the request parameter block.
4. Set the data block length to 0.
5. Set the authentication key and reserved word to 0.
6. Return the initialized request parameter block.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
newPacket | |
	 "...returns a new packet (Pacbuf) from the JuniperSocket."
	fJuniperSocket ≡ nil
	  ifTrue:
		[self open].
	^fJuniperSocket freePacket
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
doLogin: pRequest | tResult |
	 "...is sent by login:password: with pRequest (a JuniperRequestParameter Block) set.  doLogin: issues a login request, checks for errors, and retries if necessary.  If no error occurs, a JuniperResultParameterBlock is returned; otherwise, error handling is invoked."
	tResult ← JuniperResultParameterBlock new "1".
	tResult packet← fJuniperSocket sendLogin: pRequest packet "2".
	(self checkResult: tResult)
	  ifTrue: [^tResult].
	 "3"
	^self doLogin: pRequest "4" "
1. Create a new result parameter block.
2. Issue a login request and set the result packet in the result parameter block.
3. Check the result and return it if there is no error.
4. Retry the request if there is a retryable error.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
checkDirectory: pFileName | ps |
	 "...returns a string that has the default directory name added to the beginning of pFileName (a String) unless pFileName already begins with a directory name in which case it is returned unchanged. max length of ~58"
	ps ← (String new: 60) asStream.
	(pFileName length > 0 and: [pFileName ◦ 1 = ('<' ◦ 1)])
	  ifFalse:
		[ps append: '<'.
		ps append: self directory.
		ps append: '>'].
	ps append: pFileName.
	ps last = ('.' ◦ 1)
	  ifTrue: [ps skip: ¬1].
	^ps contents
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
hash: pString | tHash1 tHash2 i |
	 "...returns a hash value for pString (a String)."
	tHash1 ← 0.
	tHash2 ← 0.
	(1 to: pString length by: 2) do:
		[:i | tHash1 ← tHash1 lxor: UpperCase ◦ (pString ◦ i + 1).
		tHash2 ← tHash2 lxor: (i = pString length
				  ifTrue: [32]
				  ifFalse: [UpperCase ◦ (pString ◦ (i + 1) + 1)])].
	^tHash1 * 256 + tHash2
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
directory | |
	 "...Returns a string specifying the default file name directory.  The default directory is fDefaultDirectory if it is not nil, otherwise it is self name"
	fDefaultDirectory ≡ nil
	  ifTrue: [^self userName].
	^fDefaultDirectory
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
userPassword | |
	 "...returns the account password or a default (a String)."
	fPassword ≡ nil
	  ifTrue: [^super userPassword].
	^fPassword
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
userName | |
	 "...returns the account name or a default (a String)."
	fName ≡ nil
	  ifTrue: [^super userName].
	^fName
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
login: pName password: pPassword | tRequest |
	 "...sets the necessary request parameters and invokes doLogin to issue a login request.  pName (a String) specifies the name of an account on the Juniper file system.  pPassword (a String) specifies the password of the account."
	tRequest ← self newRequestParameterBlock "1".
	tRequest leader: 3 ← sLogin "2".
	tRequest leader: 4 ← 512 "3".
	tRequest leader: 5 ← 7 "4".
	tRequest leader: 6 ← self hash: pPassword "5".
	tRequest nextDataBlockString← pName "6".
	tRequest pupType← sCustodian "7".
	^self doLogin: tRequest "8" "
1. Create a new request parameter block.
2. Set the command to Login.
3. Set the number of bytes per page to 512.
4. Set the Juniper version number to 7 (Nov 80).
5. Set the hashed account password to pPassword hashed.
6. Set the account name to pName.
7. Set the packet pup type to custodian.
8. Issue the request and return the result.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
test | |
	 "
	dpj ← nil.
	dpj ← JuniperInterface new.
	dpj release.
	dpj open.
	dpj close.
	| f  [ dpj open.  f ← dpj file: 'test.test'.
until⦂ [ f end ] do⦂ [ user show: (f next) inString. ]. f close. dpj close. ]

JuniperSocket howMany 2 1 2 2 2  11 Timer howMany  33  NameUser howMany 1 1 1
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
noOp | tPacket |
	 "...sends a noop command to the Juniper file server.  It's only effect is to reset the timout mechanism of the server."
	tPacket ← fJuniperSocket freePacket "1".
	tPacket pupType← sNoop "2".
	tPacket dataString← '' "3".
	fJuniperSocket setAddressesAndComplete: tPacket.
	fJuniperSocket timerOff "4" "
1. Get a new packet (Pacbuf).
2. Set packet pup type to 'noop'.
3. Set the dataString to the empty string.  This has the necessary effect of setting the packet length to 0.
4. Send the packet.  It is necessary to bypass the normal JuniperSocket interface (sendRequest:) because no acknowledgement is returned for the noop.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
timerOff | |
	 "...disables fTimer and sets it to nil."
	fTimer ≡ nil
	  ifFalse:
		[fTimer disable.
		fTimer ← nil]
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
timerOn | |
	 "...assigns fTimer to a new timer object that wakes up every 100 seconds (6000 1/60 seconds) and issues a 'noop' command to Juniper.  This  is used to prevent Juniper from timing out during periods of inactivity."
	self timerOff "1".
	fTimer ← Timer new "2".
	fTimer for: 6000 action⦂ [
		(self noOp.
		user show: '.'.
		fTimer reset)] "3".
	user cr.
	fTimer reset "4" "
1. Make sure that the current timer is released.
2. Create a new timer.
3. Set the timer interval to 100 seconds.  The action of the timer is to send a noop, show a dot in the dispFrame, and reset.
4. Activate the timer.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
directory: pDirectory | |
	 "...specifies the directory name to be added to any file name that does not begin with a directory name."
	fDefaultDirectory ← pDirectory
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
close | tResult |
	 "...ends the current transaction and closes the interface."
	fOpenIndicator ≡ nil
	  ifFalse:
		[ "1"
		tResult ← self doAction: sLogout requestPrs: nil "2".
		user cr.
		user show: tResult nextDataBlockString "3".
		super close "4"] "
1. If the interface is not open, do nothing.
2. Issue a logout command.
3. Display the logout message returned by Juniper.
4. delete self from externalViews, Release timers, fields, etc.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
name: pName password: pPassword | |
	 "...specifies the name and password of an account on the Juniper file system.  This account will be logged into whenever the interface is opened."
	fName ← pName.
	fPassword ← pPassword
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
versionNumbers | |
	^true
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
closeTransaction | |
	 "...closes the current transaction leaving the user logged in with all open files still open, and all read and write locks still in effect."
	self doAction: sCloseTransaction requestPrs: nil
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
release | |
	 "...leaves the interface in a valid state after an error."
	fOpenIndicator ← nil "1".
	self timerOff "2".
	fJuniperSocket ≡ nil
	  ifFalse:
		[fJuniperSocket close "3".
		fJuniperSocket ← nil "4"] "
1. Set the interface status to 'not open'.
2. Turn off the timer.
3. Close the JuniperSocket if it exists.
4. Release the JuniperSocket field.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
open | tResult |
	 "...opens the interface allowing access to files on the Juniper file system.  A new transaction is started."
	fOpenIndicator ≡ true
	  ifFalse:
		[ "1"
		E wakeup "2".
		self release "3".
		fJuniperSocket ← JuniperSocket new hostName: self server "4".
		tResult ← self login: self userName password: self userPassword "5".
		user cr.
		user show: tResult nextDataBlockString "6".
		self timerOn "7".
		super open "8".
		fOpenIndicator ← true "9"] "
1. If the interface is already open, do nothing.
2. Make sure that the EtherWorld mechanism is in a valid state.
3. Release the interface to insure a valid initial state.  (An invalid state can occur from  an error on some statement in a previous invocation of 'open').
4. Create and initialize a JuniperSocket (parameterize server name?).
5. Issue a login command.
6. Display the login message that is returned by Juniper.
7. Turn on a timer to issue periodic noop commands to prevent timeout.
8. Do predefined open operations.
9. Set the interface status to 'open'.
"
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
obsolete | |
	^fOpenIndicator ≡ nil
! !
!JuniperInterface methodsFor: 'undefined' stamp: ''!
exceptionHandler: pExceptionHandler | |
	 "...specifies an exception handler to be invoked if any subsequent request discovers that the expected transaction has been closed.  If no exception handler is set, a notify window is displayed."
	fExceptionHandler ← pExceptionHandler
! !
!JuniperPageBuffer methodsFor: 'undefined' stamp: ''!
implementationNotes | |
	 "
FIELDS
fLength : an Integer specifiying the number of bytes in the page buffer.
fPageNumber : an Integer specifying the number of the page buffer.
fSerialNumber : an Integer specifying the serial number of the page buffer. 
"
! !
!JuniperPageBuffer methodsFor: 'undefined' stamp: ''!
pageNumber: pPageNumber | |
	 "...sets the page number of the page buffer."
	fPageNumber ← pPageNumber
! !
!JuniperPageBuffer methodsFor: 'undefined' stamp: ''!
length | |
	 "...returns the number of bytes in the page buffer."
	^fLength
! !
!JuniperPageBuffer methodsFor: 'undefined' stamp: ''!
length: pLength | |
	 "...sets the number of bytes in the page buffer."
	fLength ← pLength.
	super length: pLength
! !
!JuniperPageBuffer methodsFor: 'undefined' stamp: ''!
serialNumber: pSerialNumber | |
	 "...sets the serial number of the page buffer."
	fSerialNumber ← pSerialNumber
! !
!JuniperPageBuffer methodsFor: 'undefined' stamp: ''!
serialNumber | |
	 "...returns the serial number of the page buffer."
	^fSerialNumber
! !
!JuniperPageBuffer methodsFor: 'undefined' stamp: ''!
pageNumber | |
	 "...returns the page number of the page buffer."
	^fPageNumber
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
implementationNotes | |
	 "
FIELDS
fPacket : a Pacbuf for sending request commands and parameters to the Juniper file server.
fDataBlockPosition : an Integer specifying the current position in the data block of fPacket.
"
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
dataBlockGet | |
	 "...returns the current data block as a Stream."
	^Stream new of: fPacket pupString from: fDataBlockPosition to: 4 + fPacket pupLength - 2
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
packet | |
	 "...returns the packet (a Pacbuf)."
	^fPacket
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
pupType← pPupType | |
	 "...sets the pup type of the packet to pPupType (an Integer)."
	fPacket pupType← pPupType
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
packet← pPacket | |
	 "...sets the packet to pPacket (a Pacbuf) and resets the data block position."
	fPacket ← pPacket.
	fDataBlockPosition ← 45
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
pupType | |
	 "...returns the pup type (an Integer) of the packet."
	^fPacket pupType
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
leader: pIndex | |
	 "...returns the leader word (an Integer) specified by pIndex (an Integer) from the packet."
	^fPacket word: 12 + pIndex
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
dataBlockAdvance: pIncrement | |
	 "...advances the data block position by pIncrement (an Integer)."
	fDataBlockPosition ← fDataBlockPosition + pIncrement
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
dataBlockLength← pLength | |
	 "...sets the number of bytes in the data block to pLength (an Integer)."
	fPacket pupLength← pLength + 42
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
leader: pIndex ← pValue | |
	 "...sets pValue (an Integer) in the packet at the leader word specified by pIndex (an Integer)."
	fPacket word: (12 + pIndex) ← pValue
! !
!JuniperParameterBlock methodsFor: 'undefined' stamp: ''!
dataBlockLength | |
	 "...returns the number of bytes (an Integer) in the data block."
	^fPacket pupLength - 42
! !
!JuniperRequestParameterBlock methodsFor: 'undefined' stamp: ''!
opcode← pOpcode | |
	 "...sets the request opcode field in the packet to pOpcode (an Integer)."
	fPacket word: 16 ← pOpcode
! !
!JuniperRequestParameterBlock methodsFor: 'undefined' stamp: ''!
longInteger: pPosition ← pValue | tString tPosition |
	 "...sets pValue (4 bytes of long integer) in the packet at the parameter position specified by pPosition (an Integer)."
	tString ← fPacket pupString.
	tPosition ← pPosition + 16 * 2 - 1.
	(Stream new of: tString from: tPosition to: tPosition + 3) nextNumber: 4 ← pValue.
	tString swap: tPosition with: tPosition + 2.
	tString swap: tPosition + 1 with: tPosition + 3 "
1. the two words of a Juniper long integers are stored in reverse order from those in Smalltalk.
"
! !
!JuniperRequestParameterBlock methodsFor: 'undefined' stamp: ''!
shortFileHandle← pShortFileHandle | |
	 "...sets the request short file handle field in the packet to pShortFileHandle (an Integer)."
	fPacket word: 15 ← pShortFileHandle
! !
!JuniperRequestParameterBlock methodsFor: 'undefined' stamp: ''!
nextDataBlockString← pString | tDataBlock tString tLength tMaxLength |
	 "...sets pString (a String) in the data block at the current position and advances the position."
	fPacket pupLength← 554 "1".
	tDataBlock ← self dataBlockGet "2".
	tLength ← pString length "3".
	tMaxLength ← tLength + 1 land: ¬2 "4".
	tDataBlock nextword← tLength "5".
	tDataBlock nextword← tMaxLength "6".
	tDataBlock append: pString "7".
	tDataBlock next: (tMaxLength - tLength) ← 0 "8".
	self dataBlockAdvance: 4 + tMaxLength "9" "
1. Set the length of the packet to the maximum size.  The length of each string in the data block is specified in the data block itself.
2. Get the data block (as a Stream) from the current position to the end.
3. Get the length of pString.
4. Determine the length of pString including padding needed to make its length even.
5. Set the length of pString in the data block (2 bytes).
6. Set the length of pString with padding in the data block (2 bytes).
7. Set pString in the data block.
8. Add padding in the data block.
9. Advance the current data block position past length, maximum length, and pString.
"
! !
!JuniperRequestParameterBlock methodsFor: 'undefined' stamp: ''!
parameter: pIndex ← pValue | |
	 "...sets pValue (an Integer) in the packet at the request parameter field specified by pIndex (an Integer)."
	fPacket word: (16 + pIndex) ← pValue
! !
!JuniperResultParameterBlock methodsFor: 'undefined' stamp: ''!
parameter: pIndex | |
	 "...returns from the packet the result parameter (an Integer) specified by pIndex (an Integer)."
	^fPacket word: 15 + pIndex
! !
!JuniperResultParameterBlock methodsFor: 'undefined' stamp: ''!
nextDataBlockString | tDataBlock tString tLength tMaxLength |
	 "...returns the string in the data block at the current position and advances the position."
	tDataBlock ← self dataBlockGet "1".
	tLength ← tDataBlock nextword "2".
	tMaxLength ← tDataBlock nextword "3".
	tString ← tDataBlock next: tLength "4".
	self dataBlockAdvance: 4 + tMaxLength "5".
	^tString "6" "
1. Get the data block (as a Stream) from the current position to the end.
2. Get the length of the next string.
3. Get the length of the string including possible padding.
4. Get the next string from the data block.
5. Position the data block past the length, maximum length, string, and padding.
6. Return the string.
"
! !
!JuniperResultParameterBlock methodsFor: 'undefined' stamp: ''!
longInteger: pPosition | tArray tPosition |
	 "...returns the 4 bytes of long integer in the packet at the parameter position specified by pPosition (an Integer)."
	tPosition ← pPosition + 15 * 2 - 1.
	tArray ← (Stream new of: fPacket pupString from: tPosition to: tPosition + 3) next: 4.
	tArray swap: 1 with: 3.
	tArray swap: 2 with: 4 "1".
	^tArray asStream nextNumber: 4 "
1. Juniper long integers are stored in reverse order from those in Smalltalk.
"
! !
!JuniperResultParameterBlock methodsFor: 'undefined' stamp: ''!
resultCode | |
	 "...returns the result code (an Integer) from the result packet."
	^fPacket word: 15
! !
!JuniperSocket methodsFor: 'undefined' stamp: ''!
net: pNet host: pHost | |
	 "start with the well known Juniper listener, leave filterInput false"
	super net: pNet host: pHost soc: 64.
	self retransmit: 3 every: 500 "8 sec".
	outAck ← self freePacket "for the outgoing pineAck".
	outAck pupType← 173.
	outAck pupID1← 0.
	outAck dataString← '' "also sets length; need to set addresses later"
! !
!JuniperSocket methodsFor: 'undefined' stamp: ''!
sendLogin: t1 | |
	outPac ← t1.
	frnSocNum high: 0 low: 64 "Special routine to send login, wait for ack, retransmit.
	Need to reset seqNum, and get new Juniper socket number.
	Will return the Juniper response packet, or else a false.".
	self setOutAddBlock.
	filterInput ← false.
	outPac pupID0← 0.
	outPac pupID1← seqNum ← 0.
	notWaiting ← false.
	loginPending ← true.
	self setAddressesAndComplete: outPac.
	[notWaiting] whileFalseDo:  [ "all done"
		].
	^result
! !
!JuniperSocket methodsFor: 'undefined' stamp: ''!
sendRequest: t1 | |
	outPac ← t1.
	notWaiting ← false "General routine to send request packets, wait for ack, retransmit.
	Will return the Juniper response packet, or else a false.".
	outPac pupID0← seqNum ← seqNum + 1.
	self setAddressesAndComplete: outPac.
	[notWaiting] whileFalseDo:  [ "all done"
		].
	^result
! !
!JuniperSocket methodsFor: 'undefined' stamp: ''!
socProcess: Ipac | temp |
	loginPending
	  ifTrue:
		[ "Juniper has responded, we're running at a high level" "handle this special case"
		((frnNet ≠ Ipac sourceNet "watch out, src has not been checked" or: [frnHost ≠ Ipac sourceHost]) or: [(169 ≠ Ipac pupType or: [0 ≠ Ipac pupID0])])
		  ifFalse:
			[ "discard it" "Ipac ← self freePacket: Ipac" "this must be it!"
			frnSocNum high: Ipac sourceSoc0 low: Ipac sourceSoc1.
			self setOutAddBlock.
			self setAddresses: outAck "for later use" "generate an ack".
			outAck pupID0← 0.
			self completePup: outAck.
			loginPending ← false.
			notWaiting ← filterInput ← true.
			result ← Ipac]]
	  ifFalse:
		[ "src. and dest. should have been checked ??)"
		Ipac pupType = 169
		  ifTrue:
			[outAck pupID0← Ipac pupID0 "might be a retransmission of a previous Juniper response or what we want.
		acknowledge in either case then see whether we want to keep the result".
			self completePup: outAck.
			(notWaiting or: [seqNum ≠ Ipac pupID0])
			  ifFalse:
				[ "Ipac ← self freePacket: Ipac"
				notWaiting ← true.
				result ← Ipac]]
		  ifFalse:
			[ "Ipac pupType = 0255 ⇒ [Ipac←self freePacket: Ipac]" "discard pineack"
			(notWaiting ≡ false and: [(Ipac pupType = 4 and: [(Ipac word: 23) = 2])])
			  ifTrue:
				[self timerOff "no socket at Juniper".
				notWaiting ← true.
				result ← Ipac]
			  ifFalse:
				[ "Ipac ← self freePacket: Ipac."
				]]]
! !
!JuniperSocket methodsFor: 'undefined' stamp: ''!
timerFired | |
	notWaiting "This piece of code only runs when a timer fires!
	Thus, there is mutual exclusion between this and other timer code.
	Runs below the ethernet input.   Don't do an active return
	Timer sometimes fires even though its been disabled!"
	  ifFalse:
		[self timerOn
		  ifTrue: [self completePup: outPac "go ahead and retransmit....."]
		  ifFalse:
			[notWaiting ← true.
			result ← false]]
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
emitForEffect: stack on: strm | |
	value emitForValue: stack on: strm.
	variable emitStorePop: stack on: strm
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
sizeForValue: encoder | |
	^(value sizeForValue: encoder) + (variable sizeForStore: encoder)
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
emitForValue: stack on: strm | |
	value emitForValue: stack on: strm.
	variable emitStore: stack on: strm
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
sizeForEffect: encoder | |
	^(value sizeForValue: encoder) + (variable sizeForStorePop: encoder)
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
variable: var value: exp from: encoder | |
	 "compile"
	(var is: LADMessageNode)
	  ifTrue: [^var store: exp from: encoder].
	variable ← var.
	value ← exp
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
variable: var value: exp | |
	variable ← var.
	value ← exp
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | |
	variable printon: strm indent: level.
	strm append: ' ← '.
	value printon: strm indent: level + 2
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level precedence: p | |
	p < 4
	  ifTrue: [strm append: '('].
	self printon: strm indent: level.
	p < 4
	  ifTrue: [strm append: ')']
! !
!LADAssignmentNode methodsFor: 'undefined' stamp: ''!
simplify | |
	value ← value simplify
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
emitForValue: stack on: strm | arg oldRemote |
	strm next← LdThisContext.
	stack push: 1.
	nArgsNode emitForValue: stack on: strm.
	remoteCopyNode emit: stack args: 1 on: strm.
	self emitLongJump: size on: strm "jmp-around must be 2 bytes".
	stack push: arguments length.
	arguments reverse do: [:arg | arg emitStorePop: stack on: strm].
	self emitForEvaluatedValue: stack on: strm.
	self returns ifFalse: [strm next← EndRemote].
	stack pop: 1
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
sizeForValue: encoder | arg |
	nArgsNode ← encoder encodeLiteral: arguments length.
	remoteCopyNode ← encoder encodeSelector: #blockCopy:.
	size ← (self sizeForEvaluatedValue: encoder) + (self returns
			  ifTrue: [0]
			  ifFalse: [1]) "endBlock".
	arguments do: [:arg | size ← size + (arg sizeForStorePop: encoder)].
	^1 + (nArgsNode sizeForValue: encoder) "current" + (remoteCopyNode size: encoder) + 2 "long jmp" + size
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
sizeForEvaluatedValue: encoder | |
	^(self sizeExceptLast: encoder) + (returns
	  ifTrue: [statements last sizeForReturn: encoder]
	  ifFalse: [statements last sizeForValue: encoder])
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
sizeForEvaluatedEffect: encoder | |
	returns
	  ifTrue: [^self sizeForEvaluatedValue: encoder].
	^(self sizeExceptLast: encoder) + (statements last sizeForEffect: encoder)
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
emitForEvaluatedValue: stack on: strm | |
	self emitExceptLast: stack on: strm.
	returns
	  ifTrue: [statements last emitForReturn: stack on: strm]
	  ifFalse: [statements last emitForValue: stack on: strm]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
emitForEvaluatedEffect: stack on: strm | |
	returns
	  ifTrue:
		[self emitForEvaluatedValue: stack on: strm.
		stack pop: 1]
	  ifFalse:
		[self emitExceptLast: stack on: strm.
		statements last emitForEffect: stack on: strm]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
emitExceptLast: stack on: strm | i |
	(1 to: statements length - 1) do: [:i | statements ◦ i emitForEffect: stack on: strm]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
sizeExceptLast: encoder | i c |
	c ← 0.
	(1 to: statements length - 1) do: [:i | c ← c + (statements ◦ i sizeForEffect: encoder)].
	^c
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
mustReturn | |
	returns
	  ifFalse:
		[((statements last is: LADMessageNode) and: [statements last isReturningIf])
		  ifFalse:
			[returns ← true.
			statements ≡ NodeNullBlock
			  ifTrue: [statements ← NodeSelf inVector]
			  ifFalse: [statements ← {statements , NodeSelf}]]]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
returnLast | |
	returns ← true
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
quickCode | |
	(statements length = 1 and: [(statements ◦ 1 is: LADVariableNode)])
	  ifTrue: [^(statements ◦ 1) code].
	^false
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
returns | |
	^returns
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
transformRemoteVariable | |
	 "hack converts remote vars to [:x | x] so value← will work"
	(NoteTaker or: [returns])
	  ifTrue: [^false].
	statements length ≠ 1
	  ifTrue: [^false].
	(statements ◦ 1 isnt: LADVariableNode)
	  ifTrue: [^false].
	arguments ← statements.
	^true
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
arguments: argNodes statements: stmts returns: returnBool from: encoder | |
	 "compile"
	arguments ← argNodes.
	statements ← (stmts length > 0
			  ifTrue: [stmts]
			  ifFalse:
				[argNodes length > 0
				  ifTrue: [{stmts , arguments last}]
				  ifFalse: [NodeNullBlock]]).
	returns ← returnBool
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
default | |
	 "[] argument of missing kwyword in ifTrue: or ifFalse:"
	statements ← NodeNullBlock.
	arguments ← Vector new: 0.
	returns ← false
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
arguments: argNodes | |
	 "decompile"
	arguments ← argNodes
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
statements: stmts returns: returnBool | penult |
	 "decompile"
	statements ← ((stmts length > 1 and: [(penult ← stmts ◦ (stmts length - 1).
			(penult is: LADMessageNode) and: [penult isReturningIf])])
			  ifTrue:
				[returnBool ← false.
				(stmts ◦ (1 to: stmts length - 1)) copy]
			  ifFalse:
				[stmts length = 0
				  ifTrue: [NodeNullBlock]
				  ifFalse: [stmts]]).
	arguments ← Vector new: 0.
	returns ← returnBool
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
printStatementsOn: strm indent: level | i len shown |
	len ← shown ← statements length.
	(level = 1 and: [statements ◦ len ≡ NodeSelf])
	  ifTrue: [shown ← 1 max: shown - 1]
	  ifFalse:
		[(len = 1 and: [statements ◦ 1 ≡ NodeNil])
		  ifTrue: [shown ← shown - 1]].
	(1 to: shown) do:
		[:i | (i = len and: [returns])
		  ifTrue: [strm append: '^'].
		statements ◦ i printon: strm indent: level.
		i < shown
		  ifTrue:
			[strm append: '.'.
			strm crtab: level]]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | |
	statements length ≤ 1
	  ifFalse: [strm crtab: level].
	strm append: '['.
	self printArgumentsOn: strm indent: level.
	self printStatementsOn: strm indent: level.
	strm append: ']'
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
printArgumentsOn: strm indent: level | arg |
	arguments length = 0
	  ifFalse:
		[arguments reverse do:
			[:arg | strm append: ':'.
			strm append: arg key.
			strm space].
		strm append: '| ']
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
justTrue | |
	returns
	  ifTrue: [^false].
	^statements length = 1 and: [statements ◦ 1 ≡ NodeTrue]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
canBeSpecialArgument | |
	 "can I be an argument of (e.g.) ifTrue:?"
	^arguments length = 0
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
justFalse | |
	returns
	  ifTrue: [^false].
	^statements length = 1 and: [statements ◦ 1 ≡ NodeFalse]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
justNil | |
	returns
	  ifTrue: [^false].
	^statements length = 1 and: [statements ◦ 1 ≡ NodeNil]
! !
!LADBlockNode methodsFor: 'undefined' stamp: ''!
simplify | i |
	(1 to: statements length) do: [:i | statements ◦ i ← (statements ◦ i) simplify]
! !
!LADCascadeNode methodsFor: 'undefined' stamp: ''!
sizeForValue: encoder | msg size |
	size ← (receiver sizeForValue: encoder) + (messages length - 1 * 2).
	messages do: [:msg | size ← size + (msg sizeForValue: encoder)].
	^size
! !
!LADCascadeNode methodsFor: 'undefined' stamp: ''!
emitForValue: stack on: strm | i |
	receiver emitForValue: stack on: strm.
	(1 to: messages length - 1) do:
		[:i | strm next← Dup.
		stack push: 1.
		messages ◦ i emitForValue: stack on: strm.
		strm next← Pop.
		stack pop: 1].
	messages last emitForValue: stack on: strm
! !
!LADCascadeNode methodsFor: 'undefined' stamp: ''!
receiver: rcvr messages: msgs | |
	 "user show: 'abc'; tab; show: 'abc'; cr"
	receiver ← rcvr.
	messages ← msgs
! !
!LADCascadeNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | i |
	receiver printon: strm indent: level.
	(1 to: messages length) do:
		[:i | messages ◦ i printon: strm indent: level.
		i < messages length
		  ifTrue: [strm append: ';']]
! !
!LADCascadeNode methodsFor: 'undefined' stamp: ''!
simplify | msg |
	messages do: [:msg | msg ← msg simplify]
! !
!LADCollectionNode methodsFor: 'undefined' stamp: ''!
sizeForValue: encoder | element c |
	c ← (lengthAsNode sizeForValue: encoder) + (elementStreamNode size: encoder) + (asArrayNode size: encoder) + (elements length * (insertIntoNode size: encoder)).
	elements do: [:element | c ← c + (element sizeForValue: encoder)].
	^c
! !
!LADCollectionNode methodsFor: 'undefined' stamp: ''!
emitForValue: stack on: strm | element |
	lengthAsNode emitForValue: stack on: strm.
	elementStreamNode emit: stack args: 0 on: strm.
	elements do:
		[:element | element emitForValue: stack on: strm.
		insertIntoNode emit: stack args: 1 on: strm].
	asArrayNode emit: stack args: 0 on: strm
! !
!LADCollectionNode methodsFor: 'undefined' stamp: ''!
elements: vec | |
	 "Decompile via MessageNode simplify"
	elements ← vec
! !
!LADCollectionNode methodsFor: 'undefined' stamp: ''!
elements: vec from: encoder | |
	 "compile"
	elements ← vec.
	lengthAsNode ← encoder encodeLiteral: vec length.
	elementStreamNode ← encoder encodeSelector: #elementStream.
	insertIntoNode ← encoder encodeSelector: #nextElement:.
	asArrayNode ← encoder encodeSelector: #asArray
! !
!LADCollectionNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | i len |
	len ← elements length.
	strm append: '{'.
	(1 to: len) do:
		[:i | elements ◦ i printon: strm indent: level.
		i < len
		  ifTrue:
			[strm append: ','.
			strm space]].
	strm append: '}'
! !
!LADCollectionNode methodsFor: 'undefined' stamp: ''!
simplify | i |
	(1 to: elements length) do: [:i | elements ◦ i ← (elements ◦ i) simplify]
! !
!LADCompiler methodsFor: 'undefined' stamp: ''!
interactive || ^requestor interactive
! !
!LADCompiler methodsFor: 'undefined' stamp: ''!
notify: string | |
	failure ← true.
	user restoredisplay.
	failureValue ← requestor notify: string at: (sourceStream position min: sourceStream limit) + 1 in: sourceStream
! !
!LADCompiler methodsFor: 'undefined' stamp: ''!
translate: stream noPattern: noPattern | tree |
	tree ← LADParser new parse: stream class: class noPattern: noPattern context: context notifying: self.
	failure ifTrue: [^false].
	^tree
! !
!LADCompiler methodsFor: 'undefined' stamp: ''!
from: source class: cls instance: rcvr context: ctxt notifying: req | |
	sourceStream ← source asStream.
	class ← cls.
	instance ← rcvr.
	context ← ctxt.
	requestor ← req.
	failure ← false
! !
!LADCompiler methodsFor: 'undefined' stamp: ''!
translate: stream noPattern: noPattern | tree |
	LADVariableNode new NTinit "post-vmem init once only".
	tree ← LADParser new parse: stream class: class noPattern: noPattern context: context notifying: self.
	NoteTaker ≡ false
	  ifTrue:
		[self notify: (Statistics new symbolic: tree generate) "self notify: (LADDecompiler new classInit decompile: [context⇒[↪DoItIn:] ↪DoIt]
				 in: class method: tree generate asCompiledMethod) asString.".
		^false].
	failure
	  ifTrue: [^false].
	^tree
! !
!LADCompiler methodsFor: 'undefined' stamp: ''!
compile: parag in: cls under: category notifying: t4 | methodNode |
	requestor ← t4.
	self from: parag class: cls instance: false context: false notifying: requestor.
	(methodNode ← user displayoffwhile⦂ [(self translate: sourceStream noPattern: false)])
	  ifTrue: [^methodNode installIn: cls under: category source: parag].
	^false
! !
!LADCompiler methodsFor: 'undefined' stamp: ''!
evaluate: stream in: ctxt to: receiver notifying: t4 | methodNode method cls |
	requestor ← t4.
	cls ← (ctxt ifTrue: [ctxt receiver] ifFalse: [receiver]) class.
	self from: stream class: cls instance: receiver context: ctxt notifying: requestor.
	(methodNode ← user displayoffwhile⦂ [(self translate: stream noPattern: true)])
	  ifTrue:
		[method ← methodNode generate.
		context ifTrue: [^instance execute: method with: context].
		^instance execute: method]
	  ifFalse: [^failureValue]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decodeSpecial: which | |
	^LADSelectorNode new key: specialTable ◦ (which + 1) code: SendPlus + which
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decodeSelector: which | |
	^LADSelectorNode new key: (literals ◦ (which + 1)) key index: which type: SendType
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decodeLiteral: which | |
	^literals ◦ (which + 1)
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decodeConstant: lowbits | |
	lowbits ≥ 4
	  ifTrue: [^LADLiteralNode new key: ¬1 + lowbits - 4 code: LdMinus1 + lowbits - 4].
	lowbits = 0
	  ifTrue: [^NodeSelf].
	lowbits = 1
	  ifTrue: [^NodeTrue].
	lowbits = 2
	  ifTrue: [^NodeFalse].
	lowbits = 3
	  ifTrue: [^NodeNil].
	self confused: lowbits
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decodeTemporaryVariable: which | |
	^tempVars ◦ (which + 1)
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decodePoolVariable: which | global ref name |
	ref ← (literals ◦ (which + 1)) key.
	symbolTables do:
		[:global | (name ← global invertRef: ref)
		  ifTrue: [^LADVariableNode new name: name key: ref index: which type: LdLitIndType]].
	self error: 'Obsolete global/class/pool variable referenced'
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decodeInstanceVariable: which | |
	^instVars ◦ (which + 1)
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
remote | block args sel |
	 "[{:x} | ...]"
	hereType ≡ #:endMethod
	  ifFalse: [ "endRemote elided"
		self require: #:endRemote].
	block ← self block.
	self require: #:come.
	args ← (Vector new: 2) asStream.
	(self match: #:arguments)
	  ifTrue: [[self match: #:pop] whileTrueDo:
			[args next← self require: LADVariableNode.
			self require: #:store]].
	sel ← self advance.
	sel key = #blockCopy:
	  ifTrue: [self advance].
	self exactly: NodeThisContext.
	^block arguments: args contents
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
while: truth | if then |
	 "if whileTrue: [then]"
	then ← self block.
	self require: #:come.
	truth ≡ false
	  ifTrue: [self require: #:come].
	if ← self block.
	self require: #:begin.
	^LADMessageNode new receiver: if selector: (LADSelectorNode new key: (truth
	  ifTrue: [#whileTrueDo:]
	  ifFalse: [#whileFalseDo:]) code: #macro) arguments: then inVector precedence: 3
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
cascade: lastMessage | messages |
	messages ← lastMessage inVector asStream settoend.
	[self match: #:pop] whileTrueDo: [messages next← self message: (self require: LADSelectorNode)].
	^LADCascadeNode new receiver: self expression messages: messages reverseContents
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
message: selector | sel prec rcvr n args i supered |
	 "receiver selector arguments"
	ST80 ≡ false
	  ifTrue: [^self message76: selector].
	supered ← self match: #:super.
	sel ← selector key.
	n ← sel numArgs.
	args ← Vector new: n.
	(n to: 1 by: ¬1) do: [:i | args ◦ i ← self expression].
	prec ← (sel isinfix
			  ifTrue: [2]
			  ifFalse:
				[sel iskeyword
				  ifTrue: [3]
				  ifFalse: [1]]).
	hereType ≡ #:pop
	  ifTrue: [^self cascade: (LADMessageNode new "last msg of cascade" receiver: false selector: selector arguments: args precedence: prec)].
	rcvr ← (supered
			  ifTrue:
				[self exactly: NodeSelf.
				NodeSuper]
			  ifFalse:
				[(self match: #:dup)
				  ifTrue: [false]
				  ifFalse: [ "prior msgs of cascade"
					self expression]]).
	^LADMessageNode new receiver: rcvr selector: selector arguments: args precedence: prec
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
control | |
	 "conditional | loop"
	(self match: #:then)
	  ifTrue:
		[(self match: #:else)
		  ifTrue:
			[(self match: #:while)
			  ifTrue: [^self while: false].
			^self conditional: true]
		  ifFalse:
			[(self match: #:while)
			  ifTrue: [^self while: true].
			^self conditional: false]]
	  ifFalse:
		[(self match: #:else)
		  ifTrue: [^self remote].
		^false]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
block | |
	 "ignores for-effect pop"
	self match: #:pop.
	^self statements
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
conditional: withElse | if then else |
	 "if ifTrue: then ifFalse: else"
	withElse
	  ifTrue:
		[else ← self block.
		self require: #:come]
	  ifFalse: [else ← LADBlockNode default].
	then ← self block.
	self require: #:come.
	^LADMessageNode new receiver: self expression selector: (LADSelectorNode new key: #ifTrue:ifFalse: code: #macro) arguments: {then , else} precedence: 3
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
statements | stmts e returns |
	returns ← self match: #:endMethod.
	stmts ← (Vector new: 10) asStream.
	e ← self expression.
	(self match: #:endMethod)
	  ifTrue: [e ← self expression "last return was inaccessible"].
	[e] whileTrueDo:
		[stmts next← e.
		e ← ((self match: #:pop)
				  ifTrue: [self expression]
				  ifFalse: [self control])].
	^LADBlockNode new statements: stmts reverseContents returns: returns
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
assignment | var |
	var ← self advance.
	self require: #:store.
	^LADAssignmentNode new variable: var value: self expression
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
expression | e |
	 "assignment | message | variable | literal | control"
	aheadType ≡ #:store
	  ifTrue: [^self assignment].
	(e ← self match: LADVariableNode)
	  ifTrue: [^e].
	(e ← self match: LADSelectorNode)
	  ifTrue: [^self message: e].
	(e ← self match: LADLiteralNode)
	  ifTrue: [^e].
	^self control
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
method: selOrFalse primitive: primitive literals: lits class: class | method |
	 "pattern block"
	method ← LADMethodNode new selector: (selOrFalse
			  ifTrue: [LADSelectorNode new key: selOrFalse code: nil]
			  ifFalse: [selOrFalse]) arguments: self arguments precedence: (selOrFalse isinfix
			  ifTrue: [2]
			  ifFalse:
				[selOrFalse iskeyword
				  ifTrue: [3]
				  ifFalse: [1]]) temporaries: self temporaries block: self statements encoder: (LADEncoder new initScopeAndLiteralTables nTemps: tempVars length literals: lits class: class) primitive: primitive.
	^method
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
quickMethod: method selector: selOrFalse | block flags |
	flags ← method ◦ 3 / 32.
	block ← (flags = 5
			  ifTrue: [LADBlockNode new default mustReturn]
			  ifFalse:
				[flags = 6
				  ifTrue: [LADBlockNode new statements: (instVars ◦ (method ◦ 3 \ 32 + 1)) inVector returns: true]
				  ifFalse: [user notify: 'improper short method']]).
	^LADMethodNode new selector: (selOrFalse
	  ifTrue: [LADSelectorNode new key: selOrFalse code: nil]
	  ifFalse: [false]) arguments: #() precedence: (selOrFalse isinfix
	  ifTrue: [2]
	  ifFalse:
		[selOrFalse iskeyword
		  ifTrue: [3]
		  ifFalse: [1]]) temporaries: #() block: block encoder: nil primitive: 0
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
confused: lowbits | |
	self error: 'unexpected object code'
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
confused | |
	self error: 'unexpected object code'
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
classInit | x |
	 "LADDecompiler classInit."
	typeTable ← #(ldInst: ldTemp: ldLit: ldLit: ldLitInd: ldLitInd: shortStoP: constAndRet: sundry: goto: gotoLong: sendSpecial: sendSpecial: send: send: send: ) "16 groups of 16 byte codes each" "typeTable transform⦂ x to⦂ [x mustTake: 1]. check for performDangerously".
	baseTable ← #(0 16 32 32 64 64 96 112 128 144 160 176 176 208 208 208 ) "subtract base of group to get low bits".
	sundryTable ← #(longLoad: longSto: longStoPop: longSend: longSend: longSuper: longSuper: pop: dup: ldThisContext: ldHome: confused: confused: confused: confused: confused: ) "sundryTable transform⦂ x to⦂ [x mustTake: 1]. check for performDangerously".
	specialTable ← (SpecialOops ◦ (10 ~ 41)) copy.
	specialTable ◦ (19 ~ 23) ← {#length , #next , 'next←' unique , #end , #≡}.
	ST80 ← true
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decompile: sel in: class | old newBytes tree |
	NoteTaker
	  ifTrue:
		[ST80 ≡ false
		  ifTrue: [self classInit].
		^self decompile: sel in: class method: (class method: sel)].
	ST80
	  ifTrue: [self ST76Init].
	true
	  ifTrue:
		[tree ← self decompile76: sel in: class method: (class method: sel) "user notify: '*'.".
		^tree].
	 "special version to check out ST80 decompiling
Convert old bytes to new then decompile new bytes"
	tree ← self decompile76: sel in: class method: (class method: sel) "user notify: '*'. ".
	newBytes ← tree generate asCompiledMethod.
	self classInit.
	^self decompile: sel in: class method: newBytes
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
decompile: selOrFalse in: class method: method | i primitive nTemps lits |
	instVars ← class instvars.
	(1 to: instVars length) do: [:i | instVars ◦ i ← LADVariableNode new name: instVars ◦ i index: i - 1 type: LdInstType].
	method isQuick
	  ifTrue: [^self quickMethod: method selector: selOrFalse].
	 "parse the header"
	nLits ← method numLiterals.
	nTemps ← method numTemps.
	nArgs ← method numArgs.
	primitive ← method primitive "create symbol tables".
	tempVars ← Vector new: nTemps.
	(1 to: nTemps) do: [:i | tempVars ◦ i ← LADVariableNode new name: 't' + i asString index: i - 1 type: LdTempType].
	literals ← Vector new: nLits.
	lits ← method literals.
	(1 to: nLits) do: [:i | literals ◦ i ← LADLiteralNode new key: lits ◦ i index: i - 1 type: LdLitType].
	postfixStream ← method asStream.
	postfixStream skip: nLits * 2 + 4.
	NoteTaker
	  ifTrue:
		[postfixStream limit: postfixStream limit - 3 "three on end for file pos"].
	symbolTables ← {class wholeEnvironment , Smalltalk , Undeclared}.
	self reverse "reverse the bytes".
	^(self method: selOrFalse primitive: primitive literals: lits class: class) simplify
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
reverseNext | byte group sel thisLoc |
	byte ← postfixStream next "Mark loop beginnings: ".
	[nextBegin and: [postfixStream position = nextBegin]] whileTrueDo:
		[prefixStream last "ST80≡false and⦂" ≡ #:store
		  ifTrue:
			[ "handle for-loop optimization"
			nextBegin ← false "store var test -> store var pop begin var test".
			thisLoc ← postfixStream position.
			postfixStream skip: ¬1.
			self reverseNext.
			prefixStream next← #:pop.
			postfixStream position← thisLoc].
		prefixStream next← #:begin.
		nextBegin ← beginStream next].
	group ← byte / 16 + 1.
	sel ← typeTable ◦ group.
	self perform: sel with: byte - (baseTable ◦ group) "checked in classInit".
	^sel
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
backScan | byte dist start |
	 "quickly locates destinations (begins) of back jumps
		to handle old for-loops, and while loops with multi-stmt conditions"
	start ← postfixStream position.
	beginStream ← (Vector new: 2) asStream.
	postfixStream do:
		[:byte | byte < JmpLong
		  ifTrue:
			[ST80
			  ifTrue:
				[byte < 128
				  ifFalse:
					[byte ≤ 134
					  ifTrue:
						[postfixStream next "extension bytes".
						(byte = 132 or: [byte = 134])
						  ifTrue: [postfixStream next]]]]
			  ifFalse:
				[byte < 136
				  ifFalse:
					[byte ≤ 140
					  ifTrue: [postfixStream next "ST76"]]]]
		  ifFalse:
			[byte ≥ SendPlus
			  ifFalse:
				[dist ← byte \ 8 - 4 * 256 + postfixStream next.
				dist ≥ 0 "byte>(JmpLong+3)"
				  ifFalse: [beginStream next← postfixStream position + 1 + dist]]]].
	beginStream ← beginStream contents sort asStream.
	postfixStream position← start
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
reverse | |
	self backScan "Check for backward jumps".
	nextBegin ← beginStream next.
	prefixStream ← (Vector new: postfixStream limit * 2) asStream.
	self reverseTo: postfixStream limit "Reverse the method".
	prefixStream ← prefixStream reverseContents asStream.
	self advance.
	self advance "Start the scanner"
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
reverseTo: limit | |
	[postfixStream position ≥ limit] whileFalseDo:  [self reverseNext]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
longSto: lowbits | |
	self store: lowbits.
	self longLoad: lowbits
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
longSuper: lowbits | |
	prefixStream next← #:super.
	prefixStream next← ((lowbits allmask: 1)
	  ifTrue: [self decodeSelector: postfixStream next \ 32]
	  ifFalse:
		[postfixStream next "nargs".
		self decodeSelector: postfixStream next])
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
longSend: lowbits | |
	prefixStream next← ((lowbits allmask: 1)
	  ifTrue: [self decodeSelector: postfixStream next \ 32]
	  ifFalse:
		[postfixStream next "nargs".
		self decodeSelector: postfixStream next])
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
store: lowbits | |
	prefixStream next← #:store
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
longStoPop: lowbits | |
	self store: lowbits.
	self longLoad: lowbits.
	self pop: lowbits
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
longLoad: lowbits | extension type index |
	extension ← postfixStream next.
	type ← extension / 64.
	index ← extension \ 64.
	prefixStream next← (type = 0
	  ifTrue: [self decodeInstanceVariable: index]
	  ifFalse:
		[type = 1
		  ifTrue: [self decodeTemporaryVariable: index]
		  ifFalse:
			[type = 2
			  ifTrue: [self decodeLiteral: index]
			  ifFalse:
				[type = 3
				  ifTrue: [self decodePoolVariable: index]]]])
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
ldSelf: lowbits | |
	prefixStream next← #:ldSelf
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
dup: lowbits | |
	prefixStream next← #:dup
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
endRemote: lowbits | |
	prefixStream next← #:endRemote.
	(postfixStream peek between: JmpLong and: [JmpLong + 3])
	  ifTrue: [postfixStream skip: 2] "skip ST76 back-jmp so it wont make a :while"
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
endMethod: lowbits | |
	prefixStream next← #:endMethod
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
ldTemp: which | |
	prefixStream next← self decodeTemporaryVariable: which
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
ldInst: which | |
	prefixStream next← self decodeInstanceVariable: which
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
ldThisContext: lowbits | |
	prefixStream next← NodeThisContext
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
shortStoP: lowbits | |
	prefixStream next← #:store.
	prefixStream next← (lowbits < 8
	  ifTrue: [self decodeInstanceVariable: lowbits]
	  ifFalse: [self decodeTemporaryVariable: lowbits - 8]).
	prefixStream next← #:pop
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
constAndRet: lowbits | |
	lowbits < 8
	  ifTrue: [prefixStream next← self decodeConstant: lowbits]
	  ifFalse:
		[lowbits < 12
		  ifTrue:
			[prefixStream next← self decodeConstant: lowbits - 8.
			self endMethod: lowbits]
		  ifFalse:
			[lowbits = 12
			  ifTrue: [self endMethod: lowbits]
			  ifFalse:
				[lowbits = 13
				  ifTrue: [self endRemote: lowbits]
				  ifFalse: [self confused: lowbits]]]]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
pop: lowbits | |
	prefixStream next← #:pop
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
ldLit: which | |
	prefixStream next← self decodeLiteral: which
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
sendSpecial: which | |
	prefixStream next← self decodeSpecial: which
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
gotoLong: nibble | |
	nibble < 8
	  ifTrue: [self jmp: (self long: nibble - 4)]
	  ifFalse: [self bfp: (self long: nibble - 12)]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
ldLitInd: which | |
	prefixStream next← self decodePoolVariable: which
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
send: lowbits | |
	prefixStream next← self decodeSelector: lowbits \ 16
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
goto: nibble | |
	nibble < 8
	  ifTrue: [self jmp: nibble + 1]
	  ifFalse: [self bfp: nibble - 7]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
sundry: nibble | sel |
	sel ← sundryTable ◦ (nibble + 1).
	sel ◦ 1 = 58 ":"
	  ifTrue: [prefixStream next← sel]
	  ifFalse: [self perform: sel with: nibble]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
temporaries | |
	^(tempVars ◦ (nArgs + 1 to: tempVars length)) copy
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
bfp: dist | |
	self come: dist.
	prefixStream next← #:then
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
jmp: dist | nargs argLoc p |
	dist < 0
	  ifTrue: [prefixStream next← #:while]
	  ifFalse:
		[argLoc ← postfixStream position.
		ST80
		  ifTrue:
			[nargs ← 0 "collect arguments to block".
			[(p ← postfixStream peek) = StorePop or: [(p between: ShortStoP and: [ShortStoP + 15])]] whileTrueDo:
				[self reverseNext.
				nargs ← nargs + 1].
			nargs > 0
			  ifTrue: [prefixStream next← #:arguments]].
		self come: dist - (postfixStream position - argLoc).
		prefixStream next← #:else]
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
abguments | |
	^(tempVars ◦ (1 to: nArgs)) copy
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
long: octal | |
	^octal * 256 + postfixStream next
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
come: dist | |
	prefixStream next← #:come.
	self reverseTo: postfixStream position + dist
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
exactly: symbol | |
	here ≡ symbol
	  ifTrue: [^self advance].
	self confused
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
require: type | |
	hereType ≡ type
	  ifTrue: [^self advance].
	self confused
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
advance | old |
	old ← here.
	here ← ahead.
	hereType ← aheadType.
	ahead ← prefixStream next.
	aheadType ← ((ahead is: UniqueString)
			  ifTrue:
				[ahead ◦ 1 = 58
				  ifTrue: [ ":"
					ahead]
				  ifFalse: [#:selector]]
			  ifFalse: [ahead class "VariableNode or LiteralNode"]).
	^old
! !
!LADDecompiler methodsFor: 'undefined' stamp: ''!
match: type | |
	hereType ≡ type
	  ifTrue: [^self advance].
	^false
! !
!LADDictionary methodsFor: 'undefined' stamp: ''!
at: key ifAbsent⦂ expr | val |
	val ← super at: (self find: key).
	(val ≡ nil or: [val key ≠ key]) ifTrue: [^expr eval].
	^val
! !
!LADDictionary methodsFor: 'undefined' stamp: ''!
delete: key  | loc old len entry |
	loc ← self find: key.
	((entry ← self at: loc) ≡ nil or: [entry key ≠ key])
		ifTrue: [self error: 'Object not found'].
	self at: loc put: nil.
	len ← self length.
	[loc ← loc = len ifTrue: [1] ifFalse: [loc + 1].
		(self at: loc)≡ nil]
		whileFalseDo:
			[old ← self find: (self at: loc) key.
			loc=old ifFalse: [self swap: loc with: old]]
! !
!LADDictionary methodsFor: 'undefined' stamp: ''!
contents | s i |
	s ← Stream new of: (Vector new: self length).
	(1 to: self length) do:
		[:i | self ◦ i ≡ nil
		  ifFalse: [s next← (self ◦ i) key]].
	^s contents
! !
!LADDictionary methodsFor: 'undefined' stamp: ''!
find: key | index len probe pass |
	len ← self length.
	pass ← 1.
	index ← key hash \ len + 1.
	[(probe ← super at: index) ≡ nil or: [probe key = key]] whileFalseDo: 
		[(index ← index + 1) > len
		  ifTrue:
			[index ← 1.
			(pass ← pass + 1) > 2
			  ifTrue: [^index]]].
	^index
! !
!LADDictionary methodsFor: 'undefined' stamp: ''!
add: anAssociation | index |
	index ← self find: anAssociation key.
	(super at: index) ≡ nil
	  ifTrue: [tally ← tally + 1].
	self at: index put: anAssociation.
	self fullCheck.
	^anAssociation
! !
!LADDictionary methodsFor: 'undefined' stamp: ''!
noCheckAdd: anAssociation | |
	self at: (self find: anAssociation key) put: anAssociation.
	tally ← tally + 1.
	^anAssociation
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
encodeSelector: selector | |
	^self name: selector key: selector class: LADSelectorNode type: SendType set: selectorSet
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
encodeVariable: name | varNode global ref unq |
	(varNode ← scopeTable lookup: name)
	  ifTrue: [^varNode].
	(unq ← name hasBeenUniqued)
	  ifTrue: [symbolTables do:
			[:global | (ref ← global lookupRef: unq)
			  ifTrue: [^self global: ref name: unq]]].
	requestor interactive
	  ifTrue:
		[self notify: 'Undeclared'.
		^0].
	user show: ' (' + name + ' is Undeclared) '.
	unq ← name unique.
	Undeclared declare: unq.
	^self global: (Undeclared ref: unq) name: unq
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
litIndex: literal | p |
	p ← literalStream position.
	p = 64
	  ifTrue:
		[requestor notify: 'MORE THAN 64 LITERALS REFERENCED'.
		^0].
	literalStream next← literal.
	^p
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
encodeLiteral: object | |
	^self name: object key: object class: LADLiteralNode type: LdLitType set: litSet
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
release | |
	requestor ← false
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
notify: string | req |
	requestor
	  ifTrue:
		[req ← requestor.
		self release.
		req notify: string]
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
init: cls context: ctxt notifying: req | variable node n homeNode indexNode |
	requestor ← req.
	class ← cls.
	nTemps ← 0.
	supered ← false.
	symbolTables ← class wholeEnvironment concat: Smalltalk inVector.
	self initScopeAndLiteralTables.
	n ← ¬1.
	class instvars do:
		[:variable | node ← LADVariableNode new name: variable index: (n ← n + 1) type: LdInstType.
		scopeTable insert: variable with: node].
	ctxt
	  ifTrue:
		[homeNode ← self bind: 'homeContext' "first temp = ctxt passed as arg".
		n ← 0.
		ctxt tempNames do:
			[:variable | indexNode ← self encodeLiteral: (n ← n + 1).
			node ← LADMessageNode new receiver: homeNode selector: #tempAt: arguments: indexNode inVector precedence: 3 from: self.
			scopeTable insert: variable with: node]]
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
newTemp: name | |
	nTemps ← nTemps + 1.
	^LADVariableNode new name: name index: nTemps - 1 type: LdTempType
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
autoBind: name | node |
	 "declare a block argument as a temp if not already declared"
	(node ← scopeTable lookup: name)
	  ifTrue: [^node].
	^self bind: name
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
bind: name | node |
	 "declare a temporary; error not if a field or class variable"
	((scopeTable has: name) or: [(class classvars has: name)])
	  ifTrue: [self notify: 'Name already used in this class'].
	^self reallyBind: name
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
fillSet: set with: t2 mapping: keys to: codes | cs key |
	class ← t2.
	cs ← codes asStream.
	keys do: [:key | set insert: (class new key: key code: cs next)]
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
fillDict: dict with: t2 mapping: keys to: codes | cs key |
	class ← t2.
	cs ← codes asStream.
	keys do: [:key | dict insert: key with: (class new name: key key: key code: cs next)]
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
noteSuper | |
	supered ← true
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
nTemps: n literals: lits class: t3 | |
	 "Decompile"
	class ← t3.
	nTemps ← n.
	(literalStream ← lits asStream) position← lits length
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
initScopeAndLiteralTables | |
	scopeTable ← Dictionary new copyfrom: stdVariables.
	litSet ← HashSet new copyfrom: stdLiterals.
	selectorSet ← HashSet new copyfrom: stdSelectors.
	litIndSet ← HashSet new init: 16.
	literalStream ← (Vector new: 32) asStream
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
name: name key: key class: leafNodeClass type: type set: set | t |
	(t ← set lookup: key)
	  ifTrue: [^t].
	t ← leafNodeClass new name: name key: key index: false type: type.
	set insert: t.
	^t
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
global: ref name: name | |
	^self name: name key: ref class: LADVariableNode type: LdLitIndType set: litIndSet
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
reallyBind: name | node |
	node ← self newTemp: name.
	scopeTable insert: name with: node.
	^node
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
literals: primitive nArgs: nArgs | |
	(primitive > 0 or: [nArgs > 4])
	  ifTrue:
		[self litIndex: nArgs * 256 + primitive.
		self litIndex: (Smalltalk ref: class title unique)]
	  ifFalse:
		[supered
		  ifTrue:
			[literalStream last ≡ (Smalltalk ref: class title unique)
			  ifFalse: [self litIndex: (Smalltalk ref: class title unique)]]].
	^LiteralKeeperCrock ← literalStream contents
! !
!LADEncoder methodsFor: 'undefined' stamp: ''!
maxTemp | |
	^nTemps
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
key | |
	^key
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
hash | t |
	^key hash
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
code | |
	^code
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
= literal | t |
	^key class ≡ literal class and: [(key sameAs: literal)]
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
sizeForValue: encoder | |
	self reserve: encoder.
	code < 256
	  ifTrue: [^1].
	^2
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
size: encoder | |
	^self sizeForValue: encoder
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
sizeForEffect: encoder | |
	^0
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
reserve: encoder | |
	 "if this is a yet unused literal of type -code, reserve it"
	code < 0
	  ifTrue: [code ← self code: (encoder litIndex: key) type: 0 - code]
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
emitForEffect: stack on: strm | |
	
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
emitOn: strm | |
	code < 256
	  ifTrue: [strm next← code]
	  ifFalse:
		[strm next← code / 256.
		strm next← code \ 256]
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
emitLong: mode on: strm | type index |
	 "emit extended variable access"
	code < 256
	  ifTrue:
		[code < 16
		  ifTrue:
			[type ← 0.
			index ← code]
		  ifFalse:
			[code < 32
			  ifTrue:
				[type ← 1.
				index ← code - 16]
			  ifFalse:
				[code < 96
				  ifTrue:
					[type ← code / 32 + 1.
					index ← code \ 32]
				  ifFalse: [user notify: 'Sends should be handled in SelectorNode']]]]
	  ifFalse:
		[index ← code \ 256.
		type ← code / 256 - 1].
	strm next← mode.
	strm next← type * 64 + index
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
key: object index: i type: type | |
	self key: object code: (self code: i type: type)
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
key: object code: byte | |
	key ← object.
	code ← byte
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
name: literal key: object index: i type: type | |
	self key: object index: i type: type
! !
!LADLeafNode methodsFor: 'undefined' stamp: ''!
code: index type: type | |
	index ≡ false
	  ifTrue: [^0 - type].
	CodeLimits ◦ type > index
	  ifTrue: [^CodeBases ◦ type + index].
	^type * 256 + index
! !
!LADLiteralNode methodsFor: 'undefined' stamp: ''!
emitForValue: stack on: strm | |
	code < 256
	  ifTrue: [strm next← code]
	  ifFalse: [self emitLong: LdInstLong on: strm].
	stack push: 1
! !
!LADLiteralNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | |
	 "wouldn't handle UniqueString literals right"
	((key is: UniqueString) or: [(key is: Vector)])
	  ifTrue: [strm append: '#'].
	key printon: strm
! !
!LADLiteralNode methodsFor: 'undefined' stamp: ''!
printOn: strm indent: level | |
	 "wouldn't handle UniqueString literals right"
	((key is: UniqueString) or: [key is: Vector])
	  ifTrue: [strm append: '#'].
	key printOn: strm
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
sizeWhile: encoder value: forValue | cond stmt bfpSize |
	 "L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only)" "justStmt, wholeLoop, justJump"
	cond ← receiver.
	stmt ← arguments ◦ 1.
	sizes ← Vector new: 3.
	sizes ◦ 1 ← (stmt sizeForEvaluatedEffect: encoder) + 2.
	selector key ≡ #whileFalseDo:
	  ifTrue:
		[sizes ◦ 3 ← self sizeJump: sizes ◦ 1.
		bfpSize ← self sizeBranch: sizes ◦ 3]
	  ifFalse:
		[sizes ◦ 3 ← self sizeBranch: sizes ◦ 1.
		bfpSize ← 0].
	sizes ◦ 2 ← (cond sizeForEvaluatedValue: encoder) + bfpSize + (sizes ◦ 3) + (sizes ◦ 1).
	^sizes ◦ 2 + (forValue
	  ifTrue: [1]
	  ifFalse: [0]) "+1 (push nil) for value only"
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
emitWhile: stack on: strm value: forValue | cond stmt stmtSize loopSize |
	 "L1: ... Bfp(L2)|(Bfp(1)Jmp(L2)) ... Jmp(L1) L2:  "
	cond ← receiver.
	stmt ← arguments ◦ 1.
	stmtSize ← sizes ◦ 1.
	loopSize ← sizes ◦ 2.
	cond emitForEvaluatedValue: stack on: strm.
	selector key ≡ #whileFalseDo:
	  ifTrue:
		[self emitBranch: sizes ◦ 3 pop: stack on: strm.
		self emitJump: stmtSize on: strm]
	  ifFalse: [self emitBranch: stmtSize pop: stack on: strm].
	stmt emitForEvaluatedEffect: stack on: strm.
	self emitJump: 0 - loopSize on: strm.
	forValue
	  ifTrue:
		[strm next← LdNil.
		stack push: 1]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
sizeForValue: encoder | argument c s z |
	special > 0
	  ifTrue: [^self perform: macroSizers ◦ special with: encoder with: true].
	receiver ≡ NodeSuper
	  ifTrue: [selector ← selector copy "only necess for splOops"].
	c ← (selector size: encoder args: arguments length super: receiver ≡ NodeSuper) + (receiver
			  ifTrue: [receiver sizeForValue: encoder]
			  ifFalse: [0]).
	s ← sizes asStream.
	arguments do:
		[:argument | s next← z ← argument sizeForValue: encoder.
		c ← c + z].
	^c
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
emitForValue: stack on: strm | argument |
	special > 0
	  ifTrue: [self perform: macroEmitters ◦ special with: stack with: strm with: true]
	  ifFalse:
		[receiver
		  ifTrue: [receiver emitForValue: stack on: strm].
		arguments do: [:argument | argument emitForValue: stack on: strm].
		selector emit: stack args: arguments length on: strm super: receiver ≡ NodeSuper]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
emitForEffect: stack on: strm | |
	special > 0
	  ifTrue: [self perform: macroEmitters ◦ special with: stack with: strm with: false]
	  ifFalse: [super emitForEffect: stack on: strm]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
sizeForEffect: encoder | |
	special > 0
	  ifTrue: [^self perform: macroSizers ◦ special with: encoder with: false].
	^super sizeForEffect: encoder
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
sizeIf: encoder value: forValue | thenExpr thenSize elseExpr elseSize |
	thenExpr ← arguments ◦ 1.
	elseExpr ← arguments ◦ 2.
	forValue ≡ false
	  ifTrue:
		[(elseExpr justNil or: [elseExpr justFalse])
		  ifTrue:
			[elseSize ← 0.
			thenSize ← thenExpr sizeForEvaluatedEffect: encoder]
		  ifFalse: [^super sizeForEffect: encoder]]
	  ifFalse:
		[elseSize ← elseExpr sizeForEvaluatedValue: encoder.
		thenSize ← (thenExpr sizeForEvaluatedValue: encoder) + (thenExpr returns
				  ifTrue: [0]
				  ifFalse: [self sizeJump: elseSize])].
	sizes ◦ 1 ← thenSize.
	sizes ◦ 2 ← elseSize.
	^(receiver sizeForValue: encoder) + (self sizeBranch: thenSize) + thenSize + elseSize
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
emitIf: stack on: strm value: forValue | thenExpr thenSize elseExpr elseSize |
	thenSize ← sizes ◦ 1.
	elseSize ← sizes ◦ 2.
	(forValue ≡ false and: [elseSize > 0])
	  ifTrue: [super emitForEffect: stack on: strm]
	  ifFalse:
		[thenExpr ← arguments ◦ 1.
		elseExpr ← arguments ◦ 2.
		receiver emitForValue: stack on: strm.
		self emitBranch: thenSize pop: stack on: strm.
		(forValue ≡ false and: [elseSize = 0])
		  ifTrue: [thenExpr emitForEvaluatedEffect: stack on: strm]
		  ifFalse:
			[thenExpr emitForEvaluatedValue: stack on: strm.
			stack pop: 1.
			thenExpr returns
			  ifFalse: [self emitJump: elseSize on: strm].
			elseExpr emitForEvaluatedValue: stack on: strm]]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
classInit | |
	 "LADMessageNode classInit."
	macroSelectors ← #(ifTrue: ifFalse: ifTrue:ifFalse: and: or: whileFalseDo: whileTrueDo: ).
	macroTransformers ← #(transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformAnd: transformOr: transformWhile: transformWhile: ).
	macroEmitters ← #(emitIf:on:value: emitIf:on:value: emitIf:on:value: emitIf:on:value: emitIf:on:value: emitWhile:on:value: emitWhile:on:value: ).
	macroSizers ← #(sizeIf:value: sizeIf:value: sizeIf:value: sizeIf:value: sizeIf:value: sizeWhile:value: sizeWhile:value: )
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
cascadeReceiver | rcvr |
	(receiver ≡ NodeSuper or: [special > 0])
	  ifTrue: [^false].
	rcvr ← receiver.
	receiver ← false.
	^rcvr
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
store: expr from: encoder | |
	 "ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment)"
	selector key ≠ #tempAt:
	  ifTrue: [user notify: 'cant transform this message']
	  ifFalse: [^LADMessageNode new receiver: receiver selector: #tempAt:put: arguments: {arguments , expr} precedence: precedence from: encoder]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
receiver: rcvr selector: selNode arguments: args precedence: p | |
	 "decompile"
	self receiver: rcvr arguments: args precedence: p.
	special ← macroSelectors find: selNode key.
	selector ← selNode
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder | |
	 "compile"
	self receiver: rcvr arguments: args precedence: p.
	special ← macroSelectors find: selName.
	(self transform: encoder)
	  ifTrue: [selector ← LADSelectorNode new key: macroSelectors ◦ special code: #macro]
	  ifFalse:
		[selector ← encoder encodeSelector: selName.
		rcvr ≡ NodeSuper
		  ifTrue: [encoder noteSuper]]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
printIfOn: strm indent: level | |
	(arguments ◦ 2) justNil
	  ifTrue: [^self printKeywords: #ifTrue: arguments: (arguments ◦ 1) inVector on: strm indent: level].
	(arguments ◦ 2) justFalse
	  ifTrue: [^self printKeywords: #and: arguments: (arguments ◦ 1) inVector on: strm indent: level].
	(arguments ◦ 1) justNil
	  ifTrue: [^self printKeywords: #ifFalse: arguments: (arguments ◦ 2) inVector on: strm indent: level].
	(arguments ◦ 1) justTrue
	  ifTrue: [^self printKeywords: #or: arguments: (arguments ◦ 2) inVector on: strm indent: level].
	self printKeywords: #ifTrue:ifFalse: arguments: arguments on: strm indent: level
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | |
	receiver
	  ifTrue: [receiver printon: strm indent: level precedence: precedence].
	(special between: 1 and: [5])
	  ifTrue: [self printIfOn: strm indent: level]
	  ifFalse: [self printKeywords: selector key arguments: arguments on: strm indent: level]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level precedence: p | parenthesize |
	parenthesize ← precedence > p or: [(p = 3 and: [precedence = 3 "both keywords"])].
	parenthesize
	  ifTrue: [strm append: '('].
	self printon: strm indent: level.
	parenthesize
	  ifTrue: [strm append: ')']
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
printKeywords: key arguments: args on: strm indent: level | keywords part prev arg |
	args length = 0
	  ifTrue:
		[strm space.
		strm append: key.
		^self].
	keywords ← key keywords.
	prev ← receiver.
	(1 to: args length) do:
		[:part | arg ← args ◦ part.
		(((prev is: LADBlockNode) or: [((prev is: LADMessageNode) and: [prev precedence ≥ 3])]) or: [(part > 1 and: [(arg is: LADBlockNode)])])
		  ifTrue: [strm crtab: level + 1]
		  ifFalse: [ "newline after big args"
			strm space].
		key ← keywords ◦ part.
		strm append: key.
		strm space.
		arg printon: strm indent: level + 2 precedence: (precedence = 2
		  ifTrue: [1]
		  ifFalse:
			[key = '←'
			  ifTrue: [4]
			  ifFalse: [precedence]]).
		prev ← arg]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
precedence | |
	^precedence
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transformBoolean: encoder | |
	^self canBeSpecial: arguments ◦ 1 from: encoder
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
canBeSpecial: node from: encoder | |
	node canBeSpecialArgument
	  ifTrue: [^node is: LADBlockNode].
	^encoder notify: '←Must be a block or a variable'
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
addLeftArrow: lastArg encoder: encoder | |
	 "for ST76 compatibility"
	selector ← encoder encodeSelector: (selector key + '←') unique.
	arguments ← {arguments , lastArg}
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
receiver: rcvr arguments: args precedence: p | |
	receiver ← rcvr.
	arguments ← args.
	sizes ← Vector new: arguments length.
	precedence ← p
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transformIfTrueIfFalse: encoder | |
	^(self canBeSpecial: arguments ◦ 1 from: encoder) and: [(self canBeSpecial: arguments ◦ 2 from: encoder)]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transformIfFalse: encoder | |
	(self transformBoolean: encoder)
	  ifTrue: [arguments ← {(LADBlockNode new statements: NodeNil inVector returns: false) , (arguments ◦ 1)}]
	  ifFalse: [^false]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
isReturningIf | |
	special = 3
	  ifTrue: [^(arguments ◦ 1) returns and: [(arguments ◦ 2) returns]].
	^false
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transform: encoder | |
	special = 0
	  ifTrue: [^false].
	(self perform: macroTransformers ◦ special with: encoder)
	  ifTrue: [sizes ← Vector new: arguments length]
	  ifFalse:
		[special ← 0.
		^false]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transformWhile: encoder | |
	^(self transformBoolean: encoder) and: [(self canBeSpecial: receiver from: encoder)]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transformIfTrue: encoder | |
	(self transformBoolean: encoder)
	  ifTrue: [arguments ← {arguments ◦ 1 , (LADBlockNode new statements: NodeNil inVector returns: false)}]
	  ifFalse: [^false]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transformOr: encoder | |
	(self transformBoolean: encoder)
	  ifTrue: [arguments ← {(LADBlockNode new statements: NodeTrue inVector returns: false) , (arguments ◦ 1)}]
	  ifFalse: [^false]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
transformAnd: encoder | |
	(self transformBoolean: encoder)
	  ifTrue: [arguments ← {arguments ◦ 1 , (LADBlockNode new statements: NodeFalse inVector returns: false)}]
	  ifFalse: [^false]
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
simplifyCollection | strm |
	(strm ← receiver asCollectionElements: 0)
	  ifTrue: [^LADCollectionNode new elements: strm contents].
	^false
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
asCollectionElements: length | s |
	 "{1,2,3}"
	(selector key ≡ #elementStream and: [receiver key = length])
	  ifTrue: [^(Vector new: length) asStream].
	(selector key ≡ #nextElement: and: [(s ← receiver asCollectionElements: length + 1)])
	  ifTrue:
		[s next← (arguments ◦ 1) simplify.
		^s].
	^false
! !
!LADMessageNode methodsFor: 'undefined' stamp: ''!
simplify | n i s |
	((n ← #(asArray ) find: selector key) > 0 and: [(s ← self perform: #(simplifyCollection ) ◦ n)])
	  ifTrue: [^s].
	receiver ← receiver simplify.
	(1 to: arguments length) do: [:i | arguments ◦ i ← (arguments ◦ i) simplify]
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
installIn: class under: category source: sourceParagraph | selector method |
	selector← selectorOrFalse key.
	method ← self generate.
	class install: method selector: selector under: category source: sourceParagraph.
	method putSource: sourceParagraph text class: class category: category inFile: 2. "changes"
	^selector
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
quick: selector hi: hibyte lo: lobyte | method |
	method ← (NoteTaker
			  ifTrue: [CompiledMethod]
			  ifFalse: [String]) new: 7.
	method ◦ 3 ← hibyte.
	method ◦ 4 ← lobyte.
	NoteTaker
	  ifTrue: [method key: selectorOrFalse key]
	  ifFalse: [method word: 1 ← selector key asOop].
	^method
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
quickCode: sel | v |
	sel ≡ false
	  ifTrue: [^false].
	(v ← block quickCode)
	  ifTrue:
		[v < 0
		  ifTrue: [^false].
		v = LdSelf
		  ifTrue: [^self quick: sel hi: 160 lo: 1].
		v < (CodeBases ◦ LdInstType + (CodeLimits ◦ LdInstType))
		  ifTrue: [^self quick: sel hi: 192 + v lo: 1].
		(v / 256 = 1 and: [v \ 256 < 32])
		  ifTrue: [^self quick: sel hi: 192 + (v \ 256) lo: 1].
		^false]
	  ifFalse: [^false]
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
generate | blkSize method headerSize lit stack strm nArgs |
	method ← ((primitive = 0 and: [arguments length = 0])
			  ifTrue: [self quickCode: selectorOrFalse]
			  ifFalse: [false]).
	method
	  ifTrue: [^method].
	nArgs ← arguments length.
	blkSize ← block sizeForEvaluatedValue: encoder.
	literals ← encoder literals: primitive nArgs: nArgs.
	literals length > 63
	  ifTrue: [user notify: 'Too many literals referenced']
	  ifFalse:
		[headerSize ← 4 + (2 * literals length).
		method ← (NoteTaker
				  ifTrue: [CompiledMethod]
				  ifFalse: [String]) new: headerSize + blkSize + 3.
		strm ← method asStream.
		strm position← 2 * literals length + 4.
		stack ← ParseStack init.
		block emitForEvaluatedValue: stack on: strm.
		stack position ≠ 1
		  ifTrue: [user notify: 'Compiler stack discrepancy']
		  ifFalse:
			[strm position ≠ (method length - 3)
			  ifTrue: [user notify: 'Compiler code size discrepancy']
			  ifFalse:
				[ "Header"
				method ◦ 3 ← ((nArgs ≤ 4 and: [primitive = 0])
				  ifTrue: [nArgs]
				  ifFalse: [7]) * 32 + encoder maxTemp.
				method ◦ 4 ← headerSize - 4 + (encoder maxTemp + stack length > 12
				  ifTrue: [128]
				  ifFalse: [0]) + 1 "Literals".
				NoteTaker
				  ifTrue:
					[method key: selectorOrFalse key.
					(1 to: literals length) do: [:lit | method literalAt: lit put: literals ◦ lit]]
				  ifFalse:
					[method word: 1 ← selectorOrFalse key asOop.
					(1 to: literals length) do: [:lit | method word: (lit + 2) ← (literals ◦ lit) asOop]].
				^strm asArray]]]
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
selector | |
	^selectorOrFalse key
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
emitOn: strm | stack limit |
	limit ← strm limit.
	stack ← ParseStack init.
	block emitForValue: stack on: strm.
	strm next← EndMethod.
	strm position = limit
	  ifTrue: [^strm asArray].
	user notify: 'Compiler code size discrepancy'
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: t6 primitive: prim | |
	encoder ← t6.
	selectorOrFalse ← selOrFalse.
	precedence ← p.
	arguments ← args.
	temporaries ← temps.
	block ← blk.
	primitive ← prim
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
printOn: strm | s args |
	selectorOrFalse
	  ifTrue:
		[(precedence = 1 and: [selectorOrFalse key isarrow ≡ false])
		  ifTrue:
			[ "ST76"
			strm append: selectorOrFalse key.
			strm space]
		  ifFalse:
			[args ← arguments asStream.
			selectorOrFalse key keywords do:
				[:s | strm append: s.
				strm space.
				strm append: args next key.
				strm space]]].
	strm append: ' | '.
	temporaries do:
		[:s | strm append: s key.
		strm space].
	strm append: '|'.
	primitive > 0
	  ifTrue:
		[strm append: '  <primitive: '.
		strm print: primitive.
		strm append: '>'].
	strm crtab: 1.
	block printStatementsOn: strm indent: 1
! !
!LADMethodNode methodsFor: 'undefined' stamp: ''!
simplify | |
	block ← block simplify
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
emitForEffect: stack on: strm | |
	self emitForValue: stack on: strm.
	strm next← Pop.
	stack pop: 1
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
sizeJump: dist | |
	dist = 0
	  ifTrue: [^0].
	^self sizeShortOrLong: dist
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
sizeForReturn: encoder | |
	^(self sizeForValue: encoder) + 1
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
sizeBranch: dist | |
	dist = 0
	  ifTrue: [^1].
	^self sizeShortOrLong: dist
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
emitLongJump: dist on: strm | |
	 "force a two byte jump"
	self emitLong: dist code: JmpLong on: strm
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
emitJump: dist on: strm | |
	dist = 0
	  ifFalse: [self emitShortOrLong: dist code: Jmp on: strm]
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
sizeForEffect: encoder | |
	^(self sizeForValue: encoder) + 1
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
emitForReturn: stack on: strm | |
	self emitForValue: stack on: strm.
	strm next← EndMethod
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
emitBranch: dist pop: stack on: strm | |
	stack pop: 1.
	dist = 0
	  ifTrue: [strm next← Pop]
	  ifFalse: [self emitShortOrLong: dist code: Bfp on: strm]
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
encodeSelector: selector | |
	^nil
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
classInit | |
	 "LADParseNode classInit.  LADVariableNode classInit." " | x. LADParseNode classvars contents sort transform⦂ x to⦂ x+' '+(LADParseNode classvars ref: x) refct asString " "Smalltalk allCallsOn: (LADParseNode classvars ref: ↪NodeTrueFalse) from: (SystemOrganization superclassOrder: 'S80-Compiler') " "LADParseNode classvars clean."
	.
	LdInstType ← 1.
	LdTempType ← 2.
	LdLitType ← 3.
	LdLitIndType ← 4.
	SendType ← 5.
	CodeBases ← #(0 16 32 64 208 ).
	CodeLimits ← #(16 16 32 32 16 ).
	LdSelf ← 112.
	LdTrue ← 113.
	LdFalse ← 114.
	LdNil ← 115.
	LdMinus1 ← 116.
	LdInstLong ← 128.
	Store ← 129.
	StorePop ← 130.
	ShortStoP ← 96.
	SendLong ← 131.
	LdSuper ← 133.
	Pop ← 135.
	Dup ← 136.
	LdThisContext ← 137.
	EndMethod ← 124.
	EndRemote ← 125.
	Jmp ← 144.
	Bfp ← 152.
	JmpLimit ← 8.
	JmpLong ← 160.
	SendPlus ← 176.
	Send ← 208.
	SendLimit ← 16
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm append: ''.
	self printon: strm indent: 0.
	strm append: ''
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level precedence: p | |
	self printon: strm indent: level
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
sizeShortOrLong: dist | |
	(1 ≤ dist and: [dist ≤ JmpLimit])
	  ifTrue: [^1].
	^2
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
emitLong: dist code: longCode on: strm | code |
	 "force a two-byte jump"
	code ← longCode.
	dist < 0
	  ifTrue: [dist ← dist + 1024]
	  ifFalse:
		[dist > 1023
		  ifTrue: [dist ← ¬1]
		  ifFalse: [code ← code + 4]].
	dist < 0
	  ifTrue: [user notify: 'A block compiles more than 1K bytes of code']
	  ifFalse:
		[strm next← dist / 256 + code.
		strm next← dist \ 256]
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
emitShortOrLong: dist code: shortCode on: strm | |
	(1 ≤ dist and: [dist ≤ JmpLimit])
	  ifTrue: [strm next← shortCode + dist - 1]
	  ifFalse: [self emitLong: dist code: shortCode + 16 on: strm]
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
simplify | |
	
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
canBeSpecialArgument | |
	 "can I be an argument of (e.g.) ifTrue:?"
	^false
! !
!LADParseNode methodsFor: 'undefined' stamp: ''!
asCollectionElements: length | |
	^false
! !
!LADParser methodsFor: 'undefined' stamp: ''!
interactive || ^requestor interactive
! !
!LADParser methodsFor: 'undefined' stamp: ''!
notify: string | |
	mark ≡ false
	  ifTrue: [^false].
	hereMark ← hereMark +
		(tokenType ≡ #doIt ifTrue: [1] ifFalse: [0]) +
		(hereType ≡ #doIt ifTrue: [1] ifFalse: [0]).
	tokenType ← mark ← false.
	encoder≡nil ifFalse: [encoder release]. "break cycle"
	source skip: hereMark - source position - 1. "**1 too far right now**"
	requestor notify: string.
	here ← 30 inString.
	hereType ← #doIt.
	^false
! !
!LADParser methodsFor: 'undefined' stamp: ''!
expected: string | |
	^self notify: string + ' expected'
! !
!LADParser methodsFor: 'undefined' stamp: ''!
parseArgsAndTemps: sourceStream notifying: req | args temps |
	 "for debugger"
	self init: sourceStream notifying: req.
	encoder ← self.
	args ← self pattern ◦ 2.
	temps ← self temporaries.
	encoder ← nil. "break cycle"
	^args concat: temps
! !
!LADParser methodsFor: 'undefined' stamp: ''!
encodeSelector: sel | |
	^LADSelectorNode new key: sel code: 0
! !
!LADParser methodsFor: 'undefined' stamp: ''!
bind: name | |
	^name
! !
!LADParser methodsFor: 'undefined' stamp: ''!
unbind: name | |
	
! !
!LADParser methodsFor: 'undefined' stamp: ''!
primitive | n |
	here ≡ #<
	  ifTrue:
		[self advance.
		here = 'primitive:'
		  ifTrue:
			[self advance.
			((n ← self match: #number) and: [(n Is: Integer)])
			  ifTrue:
				[here ≡ #>
				  ifTrue:
					[self advance.
					^n].
				^self expected: '>']
			  ifFalse: [^self expected: 'Number']]
		  ifFalse: [^self expected: 'primitive:']]
	  ifFalse: [^0]
! !
!LADParser methodsFor: 'undefined' stamp: ''!
temporaries | vars |
	 "[ '|' {variable} ] ⇒ {variable, ..., variable}"
	(self match: #verticalBar) ifFalse: [^self expected: 'Vertical bar'].
	(self match: #verticalBar) ifTrue: [^#()].
	vars ← (Vector new: 8) asStream.
	[hereType ≡ #word] whileTrueDo: [vars next← encoder bind: self advance].
	(self match: #verticalBar) ifTrue: [^vars contents].
	^self expected: 'Vertical bar'
! !
!LADParser methodsFor: 'undefined' stamp: ''!
statements: argNodes doit: doit | stmts expr returns |
	 " 3. 4. 5. " "{expression '.'} [^]expression ⇒ BlockNode with supplied arguments"
	hereType ≡ #rightBracket
	  ifTrue: [^LADBlockNode new arguments: argNodes statements: #() returns: false from: encoder].
	stmts ← (Vector new: 10) asStream.
	returns ← false.
	[returns ← self match: #upArrow.
	expr ← self expression.
	stmts next← expr
		ifTrue: [expr]
		ifFalse: [encoder encodeVariable: (doit ifTrue: ['nil'] ifFalse: ['self'])].
	returns ≡ false
	  ifTrue: [self match: #period]
	  ifFalse:
		[(hereType ≡ #rightBracket or: [hereType ≡ #doIt])
		  ifTrue: [false]
		  ifFalse: [^self expected: 'end of block']]] whileTrueDo: [].
	^LADBlockNode new arguments: argNodes statements: stmts contents returns: returns from: encoder
! !
!LADParser methodsFor: 'undefined' stamp: ''!
keywordMessage | selector arg args |
	 "{keyword arg} ⇒ {selector, args}"
	selector ← Stream default.
	args ← (Vector new: 4) asStream.
	[hereType ≡ #keyword] whileTrueDo:
		[selector append: self advance.
		(arg ← self keywordFreeExpression: false)
		  ifTrue: [args next← arg]
		  ifFalse: [^self expected: 'argument']].
	^{selector contents unique , args contents}
! !
!LADParser methodsFor: 'undefined' stamp: ''!
collection | exprs e |
	 "'{' {simpleExpression ','} [simpleExpression] '}' ⇒ CollectionNode"
	(self match: #leftBrace)
	  ifTrue:
		[exprs ← (Vector new: 10) asStream.
		[((e ← self simpleExpression: false) and: [exprs next← e]) and: [(self match: #comma)]] whileTrueDo: [].
		(self match: #rightBrace)
		  ifTrue: [^LADCollectionNode new elements: exprs contents from: encoder].
		^self expected: 'Comma or right brace']
	  ifFalse: [^false]
! !
!LADParser methodsFor: 'undefined' stamp: ''!
binaryMessage | selector arg |
	 "binarySelector arg ⇒ {selector, {arg}}"
	selector ← self advance.
	(arg ← self argumentFreeExpression: false)
	  ifTrue: [^{selector unique , arg inVector}].
	^self expected: 'argument'
! !
!LADParser methodsFor: 'undefined' stamp: ''!
block | blk argNodes argument |
	 " '[' {:var.} (:var|statements) ']' ⇒ BlockNode"
	argNodes ← (Vector new: 1) asStream.
	(self match: #leftBracket)
	  ifTrue:
		[[(self match: #colon) and: [((argument ← self match: #word)
		  ifTrue: [argNodes next← encoder autoBind: argument]
		  ifFalse: [^self expected: 'Argument name'])]] whileTrueDo: [].
		(argNodes empty or: [(self match: #verticalBar)]) ≡ false
		  ifTrue: [^self expected: 'Vertical bar'].
		((blk ← self statements: argNodes contents doit: false) and: [(self match: #rightBracket)])
		  ifTrue: [^blk].
		^self expected: 'Period or right bracket']
	  ifFalse: [^self expected: 'Left bracket']
! !
!LADParser methodsFor: 'undefined' stamp: ''!
method: doit context: ctxt | sap blk prim temps |
	 "sap={selector, arguments, precedence}" "pattern [ | temporaries ] block ⇒ MethodNode"
	doit
	  ifTrue:
		[sap ← (ctxt ≡ false
				  ifTrue: [{(encoder encodeSelector: #DoIt) , #() , 1}]
				  ifFalse: [{(encoder encodeSelector: #DoItIn:) , (encoder encodeVariable: 'homeContext') inVector , 3}]).
		temps ← (hereType ≡ #verticalBar
				  ifTrue: [self temporaries]
				  ifFalse: [#()]).
		(blk ← self statements: #() doit: true)
		  ifTrue: [blk returnLast].
		prim ← 0]
	  ifFalse:
		[(sap ← self pattern)
		  ifTrue:
			[temps ← self temporaries.
			prim ← self primitive.
			blk ← self statements: #() doit: false]].
	(sap and: [blk])
	  ifTrue:
		[self more
		  ifTrue: [^self expected: 'Nothing more'].
		^LADMethodNode new selector: sap ◦ 1 arguments: sap ◦ 2 precedence: sap ◦ 3 temporaries: temps block: blk mustReturn encoder: encoder primitive: prim]
	  ifFalse: [^self expected: 'pattern']
! !
!LADParser methodsFor: 'undefined' stamp: ''!
subExpression | e |
	 "'(' expression ')' ⇒ expression"
	(self match: #leftParenthesis)
	  ifTrue:
		[(e ← self expression)
		  ifTrue:
			[(self match: #rightParenthesis)
			  ifTrue: [^e].
			^self expected: 'Right parenthesis']
		  ifFalse: [^self expected: 'Expression']]
	  ifFalse: [^false]
! !
!LADParser methodsFor: 'undefined' stamp: ''!
expression | var expr msgs temp rcvr |
	 "[variable '←'] expression ⇒ AssignmentNode
		 rcvr message {; message} ⇒ MessageNode | CascadeNode"
	(hereType ≡ #word and: [tokenType ≡ #leftArrow])
	  ifTrue:
		[var ← encoder encodeVariable: here.
		self advance.
		self advance.
		(expr ← self expression)
		  ifTrue: [^LADAssignmentNode new variable: var value: expr from: encoder].
		^self expected: 'Expression']
	  ifFalse:
		[(expr ← self simpleExpression: false)
		  ifTrue:
			[(hereType ≡ #semicolon and: [(expr is: LADMessageNode)])
			  ifTrue:
				[msgs ← expr inVector asStream settoend.
				rcvr ← expr cascadeReceiver.
				rcvr ≡ false
				  ifTrue: [^self notify: 'cannot cascade super'].
				[hereType ≡ #semicolon] whileTrueDo:
					[expr ← self simpleExpression: true.
					(expr is: LADMessageNode)
					  ifTrue:
						[expr cascadeReceiver
						  ifTrue: [msgs next← expr]
						  ifFalse: [^self notify: 'invalid cascade']]
					  ifFalse: [^self expected: 'Message']].
				^LADCascadeNode new receiver: rcvr messages: msgs contents]
			  ifFalse: [^expr]]
		  ifFalse: [^false]]
! !
!LADParser methodsFor: 'undefined' stamp: ''!
pattern | sap |
	 "unarySelector | binarySelector arg | keyword arg {keyword arg} ⇒
			{selector, arguments, precedence}"
	sap ← (hereType ≡ #keyword
			  ifTrue: [self arguments: 999 precedence: 3]
			  ifFalse:
				[(hereType ≡ #binary or: [(hereType ≡ #verticalBar or: [hereType ≡ #comma])])
				  ifTrue: [self arguments: 1 precedence: 2]
				  ifFalse:
					[hereType ≡ #word
					  ifTrue: [self arguments: 0 precedence: 1]
					  ifFalse: [^false]]]).
	hereType ≡ #leftArrow
	  ifTrue:
		[ "ST76"
		self advance.
		sap ◦ 1 ← encoder encodeSelector: ((sap ◦ 1) key + '←') unique.
		hereType ≡ #word
		  ifTrue:
			[sap ◦ 2 ← {sap ◦ 2 , (encoder bind: self advance)}.
			^sap].
		^self expected: 'argument name']
	  ifFalse: [^sap]
! !
!LADParser methodsFor: 'undefined' stamp: ''!
more | |
	^hereType ≠ #doIt
! !
!LADParser methodsFor: 'undefined' stamp: ''!
keywordFreeExpression: cascading | rcvr msg |
	 "argumentFreeExpression {binarySelector argumentFreeExpression} ⇒
			MessageNode"
	(rcvr ← self argumentFreeExpression: cascading)
	  ifTrue:
		[[(hereType ≡ #binary or: [hereType ≡ #verticalBar]) and: [(msg ← self binaryMessage "or⦂ hereType≡↪comma)" "ST76")]] whileTrueDo: [rcvr ← LADMessageNode new receiver: rcvr selector: msg ◦ 1 arguments: msg ◦ 2 precedence: 2 from: encoder].
		^rcvr].
	^false
! !
!LADParser methodsFor: 'undefined' stamp: ''!
simpleExpression: cascading | rcvr msg |
	 "keywordFreeExpression {keyword keywordFreeExpression} ⇒ MessageNode"
	(rcvr ← self keywordFreeExpression: cascading)
	  ifTrue:
		[(hereType ≡ #keyword and: [(msg ← self keywordMessage)])
		  ifTrue: [rcvr ← LADMessageNode new receiver: rcvr selector: msg ◦ 1 arguments: msg ◦ 2 precedence: 3 from: encoder].
		hereType ≡ #leftArrow
		  ifTrue:
			[ "ST76"
			self advance.
			^rcvr addLeftArrow: self expression encoder: encoder].
		^rcvr]
	  ifFalse: [^false]
! !
!LADParser methodsFor: 'undefined' stamp: ''!
arguments: maxArgs precedence: p | selector args type |
	 "unarySelector | binarySelector arg | keyword arg {keyword arg} ⇒
		 {selector, arguments, precedence}"
	selector ← Stream default.
	args ← (Vector new: 4) asStream.
	type ← hereType.
	[hereType ≡ type] whileTrueDo:
		[selector append: self advance.
		hereType ≡ #word
		  ifTrue:
			[args position = maxArgs
			  ifTrue: [^self notify: 'Too many arguments'].
			args next← encoder bind: self advance]
		  ifFalse:
			[maxArgs > 0
			  ifTrue: [^self expected: 'argument name']]].
	^{(encoder encodeSelector: selector contents unique) , args contents , p}
! !
!LADParser methodsFor: 'undefined' stamp: ''!
primaryExpression: cascading | |
	 "variable ⇒ VariableNode
		 constant ⇒ LiteralNode
		 '{' {expression ','} [expression] '}' ⇒ CollectionNode
		 ';' ⇒ ;  when allowed
		 - number ⇒ ¬number
		 	|  '(' expression ')'  |  block   "
	hereType ≡ #word
	  ifTrue: [^encoder encodeVariable: self advance].
	(hereType ≡ #string or: [(hereType ≡ #number or: [hereType ≡ #literal])])
	  ifTrue: [^encoder encodeLiteral: self advance].
	hereType ≡ #leftParenthesis
	  ifTrue: [^self subExpression].
	hereType ≡ #leftBracket
	  ifTrue: [^self block].
	(hereType ≡ #semicolon and: [cascading])
	  ifTrue: [^self advance].
	hereType ≡ #leftBrace
	  ifTrue: [^self collection].
	((here ≡ #- or: [here ≡ '¬' unique]) and: [tokenType ≡ #number])
	  ifTrue:
		[ "ST76"
		self advance.
		^encoder encodeLiteral: self advance negated].
	^false
! !
!LADParser methodsFor: 'undefined' stamp: ''!
argumentFreeExpression: cascading | rcvr |
	 "primaryExpression {unarySelector} ⇒ MessageNode"
	(rcvr ← self primaryExpression: cascading)
	  ifTrue:
		[[hereType ≡ #word] whileTrueDo: [rcvr ← LADMessageNode new receiver: rcvr selector: self advance unique arguments: #() precedence: 1 from: encoder].
		^rcvr].
	^false
! !
!LADParser methodsFor: 'undefined' stamp: ''!
init: sourceStream notifying: req | |
	requestor ← req.
	super scan: sourceStream.
	self advance
! !
!LADParser methodsFor: 'undefined' stamp: ''!
parse: sourceStream class: class noPattern: noPattern context: ctxt notifying: req | m |
	self init: sourceStream notifying: req "noPattern is false for a compile".
	encoder ← LADEncoder new init: class context: ctxt notifying: self.
	m ← self method: noPattern context: ctxt.
	encoder ← nil "break cycle".
	hereType ≡ #doIt
	  ifTrue: [^m].
	self notify: 'Unexpected construct'
! !
!LADParser methodsFor: 'undefined' stamp: ''!
match: type | |
	hereType ≡ type
	  ifTrue: [^self advance].
	^false
! !
!LADParser methodsFor: 'undefined' stamp: ''!
advance | t |
	t ← here.
	here ← token.
	hereType ← tokenType.
	hereMark ← mark.
	hereType
	  ifTrue: [self scanToken].
	^t
! !
!LADParser methodsFor: 'undefined' stamp: ''!
test: class | s |
	 "LADParser new test: Point."
	user waitnobug.
	#('noOp []' 'max: p2 [^(x>p2 x and: [y>p2 y]) ifTrue: [self] ifFalse: [p2]]' 'coord: p2 [[x>p2 x and: [y>p2 y]] whileTrue: [self past: p2]]' '& z | a b c [a←z. b←z*a max: z/a length/(c←a length+2 min: z)]' 'to: p2 by: d [^Interval new from: self to: p2 by: d]' 'incAll: coll [coll do: [:x. self inc: x with: ''string'']]' 'cascade: a [^a+3 to: 4; from: 5; length]' ) do:
		[:s | user cr.
		user show: (self parse: s class: class noPattern: false context: false notifying: user) simplify asString.
		user cr.
		user waitnobug.
		user waitbug].
	[(s ← user read) length = 0] whileFalseDo: 
		[user cr.
		user show: (self compile: s in: class) simplify asString.
		user cr]
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
error: string | |
	self notify: string
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
interactive | |
	^false
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
notify: string | |
	user notify: string
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
scan: inputStringOrStream | |
	source ← inputStringOrStream asStream "initialize reader".
	mark ← source position.
	self step.
	self step.
	self scanToken
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
classInit | |
	 "LADScanner classInit."
	typeTable ← Vector new: 256.
	(typeTable ◦ (1 to: 256)) all← #binary "default".
	(typeTable ◦ #(9 10 12 13 32)) all← #xDelimiter "tab lf ff cr space".
	(typeTable ◦ #(1 6 14 18)) all← #xAbbreviation " ≤ ≡ ≠ ≥ ".
	typeTable ◦ 15 ← #xLitQuote.
	typeTable ◦ 26 ← #xControlZ.
	typeTable ◦ 30 ← #doIt.
	typeTable ◦ 34 ← #xDoubleQuote.
	typeTable ◦ 35 ← #xLitQuote.
	typeTable ◦ 39 ← #xSingleQuote.
	typeTable ◦ 40 ← #leftParenthesis.
	typeTable ◦ 41 ← #rightParenthesis.
	typeTable ◦ 44 ← #comma.
	typeTable ◦ 46 ← #period.
	(typeTable ◦ (48 to: 57)) all← #xDigit.
	(typeTable ◦ #(3 58)) all← #colon.
	typeTable ◦ 59 ← #semicolon.
	(typeTable ◦ #(60 61 62 126)) all← #xRelational " < = > ~ ".
	(typeTable ◦ ((65 to: 90) concat: (97 to: 122))) all← #xLetter.
	typeTable ◦ 91 ← #leftBracket.
	typeTable ◦ 93 ← #rightBracket.
	typeTable ◦ 94 ← #upArrow.
	typeTable ◦ 95 ← #leftArrow.
	typeTable ◦ 123 ← #leftBrace.
	typeTable ◦ 124 ← #verticalBar.
	typeTable ◦ 125 ← #rightBrace
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
scanToken | |
	mark ← source position.
	tokenType ← typeTable ◦ hereChar.
	tokenType ◦ 1 = 120
	  ifTrue: 	"x as first letter means perform to compute token & type"
		[self perform: tokenType]
	  ifFalse:
		[token ← self step inUniqueString.  "first char"
		tokenType = #binary
		   ifTrue:	 		"check for double character binary operators like <= or //"
			[(typeTable◦hereChar) = #binary
		 	   ifTrue:
				[token ← (token concat: self step inUniqueString) unique]]]
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xLitQuote | type s |
	 "UniqueStrings and Vectors"
	s ← Stream default.
	self step "litQuote".
	(typeTable ◦ hereChar)	= #leftParenthesis
	  ifTrue:
		[self step "leftPar".
		[typeTable ◦ hereChar ≡ #rightParenthesis] whileFalseDo:  [s next← self step].
		self step "rightPar".
		tokenType ← #literal.
		token ← s contents asVector]	"invokes Reader"
	  ifFalse:
		[s next← self step "first char".
		typeTable◦hereChar = #binary
		  ifTrue: [s next← self step  "double binary"]
		  ifFalse: [[(type ← typeTable ◦ hereChar) = #xLetter
				or: [(type = #xDigit
				or: [type = #colon ])]] whileTrueDo: [s next← self step]].
		tokenType ← #literal.
		token ← s contents unique]
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xDigit | fracPart exp real radix |
	 "form a number"
	tokenType ← #number.
	real ← false.
	radix ← self convert: (self scanInteger: 10) radix: 10.
	hereChar = 114
	  ifTrue: "<radix>r<number>"
		[self step.
		token ← self convert: (self scanInteger: radix) radix: radix]
	  ifFalse:
		[token ← radix.
		radix ← 10].
	(hereChar = 46 and: [(self testDigit: aheadChar radix: radix)])
	  ifTrue: ".<fraction>"
		[self step.
		real ← true.
		fracPart ← self scanInteger: radix.
		fracPart ← (self convert: fracPart radix: radix) asFloat /
				(radix asFloat ipow: fracPart length).
		token ← token asFloat + fracPart].
	hereChar = 101
	  ifTrue: "e<exponent>"
		[self step.
		exp ← (hereChar = 45 "-"
				  ifTrue:
					[self step.
					0 - (self convert: (self scanInteger: 10) radix: 10)]
				  ifFalse: [self convert: (self scanInteger: 10) radix: 10]).
		token ← token * (real
				  ifTrue: [radix asFloat ipow: exp]
				  ifFalse: [radix ipow: exp])]
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xDelimiter | |
	 "ignore blanks etc."
	[typeTable ◦ hereChar ≡ #xDelimiter] whileTrueDo: [self step].
	self scanToken
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xRelational | |
	 "check for <= ~= >= == "
	tokenType ← #binary.
	token ← self step inUniqueString.
	hereChar = 61
	  ifTrue:
		[ "="
		self step.
		token ← #(≤ ≠ ≥ ≡ ) ◦ (#(< ~ > = ) find: token)]
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xSingleQuote | start s |
	 "string"
	start ← source position.
	self step.
	s ← Stream default.
	[hereChar = 39 and: [(aheadChar = 39
	  ifTrue:
		[self step.
		false]
	  ifFalse: [true])]] whileFalseDo: 
		[s next← self step.
		(hereChar = 30 "doit" and: [source end])
		  ifTrue:
			[ "Ran off end, back up."
			source skip: start - 1 - source position.
			^self error: 'Unmatched string quote']].
	self step.
	token ← s contents.
	tokenType ← #string
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xLetter | type s |
	 "form a word or keyword"
	s ← Stream default.
	[(type ← typeTable ◦ hereChar) ≡ #xLetter or: [type ≡ #xDigit]] whileTrueDo: [s next← self step].
	type ≡ #colon
	  ifTrue:
		[s next← self step.
		tokenType ← #keyword]
	  ifFalse: [tokenType ← #word].
	token ← s contents
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xDoubleQuote | |
	 "ignore comment"
	(self skipThrough: hereChar)
	  ifTrue: [self scanToken]
	  ifFalse: [self error: 'Unmatched comment quote']
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xControlZ | |
	 "ignore trailer"
	(self skipThrough: 13)
	  ifTrue: [self scanToken]
	  ifFalse: [self error: 'Missing cr in Bravo trailer']
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
xAbbreviation | |
	 " ≤ ≡ ≥ ≠ "
	tokenType ← #binary "token ← ↪(<= ~= >= ==) ◦ (↪(≤ ≠ ≥ ≡) find: self step inUniqueString)".
	token ← self step inUniqueString
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
convert: str radix: radix | |
	^Integer new readFrom: str asStream radix: radix
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
convert: str radix: radix | |
	^Integer new readFrom: str radix: radix
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
step | c |
	c ← hereChar.
	hereChar ← aheadChar.
	(aheadChar ← source next)
	  ifTrue:
		[aheadChar = 0
		  ifTrue: [aheadChar ← 256]]
	  ifFalse: [aheadChar ← 30 "doit"].
	^c
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
scanInteger: radix | s |
	 "Should be merged with Integer readFrom: "
	s ← Stream default.
	[self testDigit: hereChar radix: radix] whileTrueDo: [s next← self step].
	^s contents
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
skipThrough: char | start |
	 "⇑false if unmatched."
	start ← source position.
	self step.
	[self step = char] whileFalseDo: 
		[(hereChar = 30 and: [source end "doit"])
		  ifTrue:
			[source skip: start - 1 "Ran off end, back up." - source position.
			^false]].
	^true
! !
!LADScanner methodsFor: 'undefined' stamp: ''!
testDigit: char radix: radix | |
	^(char ≥ 48 and: [char < (48 + radix) "0-9"]) or: [(radix > 10 and: [(char ≥ 65 and: [char < (65 - 10 + radix) "A-Z"])])]
! !
!LADSelectorNode methodsFor: 'undefined' stamp: ''!
emit: stack args: nArgs on: strm | |
	self emit: stack args: nArgs on: strm super: false
! !
!LADSelectorNode methodsFor: 'undefined' stamp: ''!
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
!LADSelectorNode methodsFor: 'undefined' stamp: ''!
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
!LADSelectorNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | |
	strm append: key
! !
!LADSet methodsFor: 'undefined' stamp: ''!
swap: one with: other  | save |
	save ← self ◦ one.
	self ◦ one ← self ◦ other.
	self ◦ other ← save
! !
!LADSet methodsFor: 'undefined' stamp: ''!
asStream  | |
	^self contents asStream
! !
!LADSet methodsFor: 'undefined' stamp: ''!
grow | oldContents newSelf oldTally i |
	NoteTaker
	  ifTrue:
		[newSelf ← self species new: self length * 2.
		newSelf init "tally ← 0".
		(1 to: self length) do:
			[:i | self ◦ i ≡ nil
			  ifFalse: [newSelf noCheckAdd: self ◦ i]].
		self become: newSelf]
! !
!LADSet methodsFor: 'undefined' stamp: ''!
size | |
	^tally
! !
!LADSet methodsFor: 'undefined' stamp: ''!
init | |
	tally ← 0
! !
!LADSet methodsFor: 'undefined' stamp: ''!
fullCheck | |
	self length - tally ≤ (self length / 4)
	  ifTrue: [self grow]
! !
!LADSet methodsFor: 'undefined' stamp: ''!
at: index | |
	^self ◦ index
! !
!LADSet methodsFor: 'undefined' stamp: ''!
at: index put: anAssociation | |
	^self ◦ index ← anAssociation
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
sizeForReturn: encoder | |
	(code ≥ LdSelf and: [code ≤ LdNil])
	  ifTrue: [ "short returns"
		^1].
	^super sizeForReturn: encoder
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
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
!LADVariableNode methodsFor: 'undefined' stamp: ''!
sizeForStorePop: encoder | |
	self reserve: encoder.
	(code < 24 and: [(code nomask: 8)])
	  ifTrue: [^1].
	^2
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
sizeForStore: encoder | |
	self reserve: encoder.
	^2
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
emitStore: stack on: strm | |
	self emitLong: Store on: strm
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
emitForReturn: stack on: strm | |
	(code ≥ LdSelf and: [code ≤ LdNil])
	  ifTrue:
		[ "short returns"
		strm next← EndMethod - 4 + (code - LdSelf).
		stack push: 1 "doesnt seem right"]
	  ifFalse: [super emitForReturn: stack on: strm]
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
emitStorePop: stack on: strm | |
	(code between: 0 and: [7])
	  ifTrue: [strm next← ShortStoP + code "short stopop inst"]
	  ifFalse:
		[(code between: 16 and: [23])
		  ifTrue: [strm next← ShortStoP + 8 + code - 16 "short stopop temp"]
		  ifFalse: [self emitLong: StorePop on: strm]].
	stack pop: 1
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
name: varName index: i type: type | |
	self name: varName key: varName index: i type: type
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
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
!LADVariableNode methodsFor: 'undefined' stamp: ''!
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
!LADVariableNode methodsFor: 'undefined' stamp: ''!
name: string key: object code: byte | |
	name ← string.
	key ← object.
	code ← byte
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
name: varName key: objRef index: i type: type | |
	name ← varName.
	self key: objRef index: i type: type
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | |
	strm append: name
! !
!LADVariableNode methodsFor: 'undefined' stamp: ''!
canBeSpecialArgument | |
	 "can I be an argument of (e.g.) ifTrue:?"
	^code < LdNil
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
hibit | |
	 "Return the index of the high order bit of the binary representation of this number"
	^bytes last hibit + (8 * (bytes length - 1))
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
bit: index | byte |
	 "Return bit number i in the binary representation of this number. Bit number 1 is the low order bit"
	byte ← bytes ◦ (1 + (index - 1 / 8)).
	^(byte lshift: 0 - (index - 1 \ 8)) land: 1
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
bytes: t1 neg: t2 | |
	bytes ← t1.
	neg ← t2.
	[bytes isLarge] whileTrueDo: [bytes ← bytes bytes]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
neg | |
	^neg
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
neg← t1 | |
	 "Smashes sign - be careful!"
	neg ← t1
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
bytes | |
	^bytes
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
quoRem: aNumber | qr quo rem |
	qr ← bytes natdiv: aNumber bytes.
	quo ← (qr ◦ 1) asInteger.
	rem ← (qr ◦ 2) asInteger.
	qr ◦ 1 ← ((neg ~= aNumber neg)
	  ifTrue: [quo negated]
	  ifFalse: [quo]).
	qr ◦ 2 ← (neg
	  ifTrue: [rem negated]
	  ifFalse: [rem]).
	^qr
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
divMod: aNumber | qr |
	qr ← self quoRem: aNumber.
	(qr ◦ 2 ≠ 0 and: [(neg ~= aNumber neg)])
	  ifTrue:
		[qr ◦ 1 ← qr ◦ 1 - 1.
		qr ◦ 2 ← qr ◦ 2 + aNumber].
	^qr
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
= aNumber | |
	aNumber isNumber ≡ false
	  ifTrue: [^false].
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #= coercing: aNumber].
	^(self compare: aNumber) = 2
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
rem: aNumber | |
	^(self quoRem: aNumber) ◦ 2
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
quo: aNumber | |
	^(self quoRem: aNumber) ◦ 1
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
+ aNumber | as r |
	 "take care of sign. Arithmetic is done in Natural numbers."
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #+ coercing: aNumber].
	as ← aNumber neg.
	neg ≡ as
	  ifTrue:
		[r ← bytes natadd: aNumber bytes.
		^LargeInteger new bytes: r neg: neg].
	r ← bytes natsubtract: aNumber bytes.
	neg
	  ifTrue: [^r negated].
	^r
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
* aNumber | as r |
	 "take care of sign. Arithmetic is done in Natural numbers."
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #* coercing: aNumber].
	as ← aNumber neg.
	r ← bytes nattimes: aNumber bytes.
	^LargeInteger new bytes: r neg: neg ≡ as ≡ false
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
< aNumber | |
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #< coercing: aNumber].
	^(self compare: aNumber) = 1
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
// aNumber | |
	^(self divMod: aNumber) ◦ 1
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
\\ aNumber | |
	^(self divMod: aNumber) ◦ 2
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
coerce: aNumber | |
	^aNumber asLarge
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
generality | |
	^40
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
negated | |
	self = 0
	  ifTrue: [^self].
	^LargeInteger new bytes: bytes neg: neg ≡ false
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
even | |
	^(bytes ◦ 1 land: 1) = 0
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
compare: arg | i a |
	(((bytes length = 1 and: [bytes ◦ 1 = 0]) and: [arg bytes length = 1]) and: [arg bytes ◦ 1 = 0])
	  ifTrue: [^2].
	neg
	  ifTrue:
		[arg neg
		  ifTrue: [^arg bytes natcompare: bytes].
		^1]
	  ifFalse:
		[arg neg
		  ifTrue: [^3].
		^bytes natcompare: arg bytes]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
intdiv: arg | quo rem ng qr z |
	qr ← bytes natdiv: arg bytes "returns a vector of (quotient, remainder)".
	quo ← qr ◦ 1.
	rem ← (qr ◦ 2) asInteger.
	ng ← neg ≡ arg neg ≡ false.
	quo last = 0
	  ifTrue:
		[quo length < 2
		  ifFalse: [quo ← quo growby: ¬1]].
	qr ◦ 1 ← LargeInteger new bytes: quo neg: ng.
	qr ◦ 2 ← ((ng and: [0 ≠ rem])
	  ifTrue: [arg abs - rem]
	  ifFalse: [rem]).
	^qr
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
\ arg | |
	^(self intdiv: arg) ◦ 2
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
/ arg | |
	^((self intdiv: arg) ◦ 1) asInteger
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
inFourBytes | four i |
	bytes length > 4
	  ifTrue: [user notify: 'wont fit in 4 bytes']
	  ifFalse:
		[four ← String new: 4.
		four all← 0.
		(1 to: bytes length) do: [:i | four ◦ (5 - i) ← bytes ◦ i].
		^four]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
lxor: n | a b c |
	 "simulated 16 bit logic"
	NoteTaker
	  ifTrue:
		[a ← self logicArg bytes.
		b ← n asLarge logicArg bytes.
		c ← Natural new: 2.
		c ◦ 1 ← a ◦ 1 lxor: b ◦ 1.
		c ◦ 2 ← a ◦ 2 lxor: b ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg].
	^self asSmall lxor: n
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
logicUnArg | |
	 "convert 2's complement to signed"
	bytes length ≠ 2
	  ifTrue: [user notify: 'not logic arg']
	  ifFalse:
		[bytes ◦ 2 ≥ 128
		  ifTrue: [^(self - 65536) asInteger].
		^self asInteger]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
lor: n | a b c |
	 "simulated 16 bit logic"
	NoteTaker
	  ifTrue:
		[a ← self logicArg bytes.
		b ← n asLarge logicArg bytes.
		c ← Natural new: 2.
		c ◦ 1 ← a ◦ 1 lor: b ◦ 1.
		c ◦ 2 ← a ◦ 2 lor: b ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg].
	^self asSmall lor: n
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
bits: left to: right ← val | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width < 15
			  ifTrue: [biton ◦ (width + 1) - 1]
			  ifFalse:
				[width = 15
				  ifTrue: [32767]
				  ifFalse: [¬1]]).
	^(self land: ((mask lshift: 15 - right) lxor: ¬1)) lor: ((val land: mask) lshift: 15 - right)
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
logicArg | b a i |
	 "convert to two byte twos complement number"
	neg
	  ifTrue:
		[b ← Natural new: 2.
		b ◦ 1 ← (a ← (bytes ◦ 1 lxor: 255) + 1) land: 255.
		a ← a lshift: ¬8.
		b ◦ 2 ← (bytes ◦ 2 lxor: 255) + a land: 255.
		^LargeInteger new bytes: b neg: false] "if positive, just leave it"
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
bits: left to: right | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width < 15
			  ifTrue: [biton ◦ (width + 1) - 1]
			  ifFalse:
				[width = 15
				  ifTrue: [32767]
				  ifFalse: [¬1]]).
	^(self lshift: right - 15) land: mask
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
land: n | a b c |
	 "simulated 16 bit logic"
	NoteTaker
	  ifTrue:
		[a ← self logicArg bytes.
		b ← n asLarge logicArg bytes.
		c ← Natural new: 2.
		c ◦ 1 ← a ◦ 1 land: b ◦ 1.
		c ◦ 2 ← a ◦ 2 land: b ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg].
	^self asSmall land: n
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
anymask: b | |
	^0 ≠ (self land: b)
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
allmask: b | |
	^b = (self land: b)
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
nomask: b | |
	^0 = (self land: b)
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
field: spec ← val | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: (right - (spec / 16) + 1) to: right ← val
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
field: spec | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: right - (spec / 16) + 1 to: right
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
hash | |
	^bytes hash
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
lshift: n | a c s1 s2 h |
	NoteTaker
	  ifTrue:
		[a ← self logicArg.
		c ← Natural new: 2.
		c ◦ 1 ← a bytes ◦ 1.
		c ◦ 2 ← a bytes ◦ 2.
		a bytes: c neg: false.
		(h ← n abs) ≥ 16
		  ifTrue: [^0].
		s1 ← h / 2.
		s2 ← h - s1.
		s1 ← 1 lshift: s1.
		s2 ← 1 lshift: s2.
		h ← s1 asLarge * s2 asLarge.
		a ← (n < 0
				  ifTrue: [a / h]
				  ifFalse: [a * h]).
		c ← Natural new: 2.
		c ◦ 1 ← a bytes ◦ 1.
		c ◦ 2 ← a bytes ◦ 2.
		^(LargeInteger new bytes: c neg: false) logicUnArg]
	  ifFalse: [^self asSmall lshift: n]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
asInteger | |
	self canBeSmall
	  ifTrue: [^self asSmall].
	^self
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
isLarge | |
	
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
asSmall | t u |
	 "Truncate high bits to make small integer."
	t ← bytes ◦ 1.
	bytes length > 1
	  ifTrue:
		[u ← bytes ◦ 2.
		t ← ((NoteTaker
				  ifTrue: [(u land: 63) - (u land: 64)]
				  ifFalse: [u]) lshift: 8) + t].
	neg
	  ifTrue:
		[t = 0 minVal
		  ifTrue: [^t].
		^0 - t]
	  ifFalse: [^t]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
asObject | |
	NoteTaker
	  ifTrue:
		[self canBeSmall
		  ifTrue: [^self asSmall asObject].
		self < 49152
		  ifTrue:
			[self < 32768
			  ifTrue: [self error: 'invalid oop']
			  ifFalse: [^(self - 32768) asSmall]]
		  ifFalse:
			[self ≥ 65536
			  ifTrue: [self error: 'invalid oop']
			  ifFalse: [^(self - 65536) asSmall]]]
	  ifFalse: [^self asSmall asObject]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
canBeSmall | i |
	bytes length > 2
	  ifTrue: [(3 to: bytes length) do:
			[:i | bytes ◦ i ≠ 0
			  ifTrue: [^false]]].
	(self ≤ 1 maxVal and: [self ≥ 1 minVal])
	  ifTrue: [^true].
	^false
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
asFloat | |
	 "Built for comfort, not for speed"
	^self asString asFloat
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
asLarge | |
	
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
printOn: strm base: b | |
	neg
	  ifTrue: [strm append: '-'].
	bytes printOn: strm base: b
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
cansubscript: a | |
	^self ≥ 1 and: [self ≤ a length]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
subscripts: a ← val | t |
	(self cansubscript: a)
	  ifTrue:
		[((a class isBytes) and: [t ← val asInteger.  (t<0 or: [t>255])])
		  ifTrue: [user notify: 'Improper store into String']
		  ifFalse: [^a ◦ self asInteger ← t]]
	  ifFalse: [user notify: 'Subscript out of bounds: ' + self asString]
! !
!LargeInteger methodsFor: 'undefined' stamp: ''!
subscripts: a | |
	(self cansubscript: a)
	  ifTrue: [^a ◦ self asInteger].
	user notify: 'Subscript out of bounds: ' + self asString
! !
!LinearMeasure methodsFor: 'undefined' stamp: ''!
> aMeasure | |
	^aMeasure < self
! !
!LinearMeasure methodsFor: 'undefined' stamp: ''!
< aMeasure | |
	^self subclassResponsibility
! !
!LinearMeasure methodsFor: 'undefined' stamp: ''!
≤ aMeasure | |
	^aMeasure < self ≡ false
! !
!LinearMeasure methodsFor: 'undefined' stamp: ''!
= aMeasure | |
	^self subclassResponsibility
! !
!LinearMeasure methodsFor: 'undefined' stamp: ''!
≠ aMeasure | |
	^self = aMeasure ≡ false
! !
!LinearMeasure methodsFor: 'undefined' stamp: ''!
≥ aMeasure | |
	^self < aMeasure ≡ false
! !
!Link methodsFor: 'undefined' stamp: ''!
nextLink: aLink | | nextLink ← aLink
! !
!Link methodsFor: 'undefined' stamp: ''!
nextLink | | ^nextLink
! !
!LinkedList methodsFor: 'undefined' stamp: ''!
init | |
	 firstLink ← nil.
	 lastLink ← nil
! !
!LinkedList methodsFor: 'undefined' stamp: ''!
removeLink: oldLink | aLink |
	 oldLink==firstLink
		ifTrue:
			[firstLink ← oldLink nextLink.
			oldLink==lastLink ifTrue: [lastLink ← nil]]
		ifFalse:
			[aLink ← firstLink.
			[aLink nextLink==oldLink] whileFalse: [aLink ← aLink nextLink].
			aLink nextLink: oldLink nextlink.
			oldLink==lastLink ifTrue: [lastLink ← aLink]].
	 oldLink nextLink: nil
! !
!LinkedList methodsFor: 'undefined' stamp: ''!
removeFirstLink | oldLink |
	 self isEmpty ifTrue: [user notify: 'LinkedList is empty'].
	 oldLink ← firstLink.
	 firstLink==lastLink
		ifTrue: [firstLink ← nil.  lastLink ← nil]
		ifFalse: [firstLink ← oldLink nextLink].
	 oldLink nextLink: nil.
	 ^oldLink
! !
!LinkedList methodsFor: 'undefined' stamp: ''!
removeLastLink | oldLink aLink |
	 self isEmpty ifTrue: [user notify: 'LinkedList is empty'].
	 oldLink ← lastLink.
	 firstLink==lastLink
		ifTrue: [firstLink ← nil.  lastLink ← nil]
		ifFalse:
			[aLink ← firstLink.
			[aLink nextLink==oldLink] whileFalse: [aLink ← aLink nextLink].
			aLink nextLink: nil.
			lastLink ← aLink].
	 oldLink nextLink: nil.
	 ^oldLink
! !
!LinkedList methodsFor: 'undefined' stamp: ''!
addLastLink: aLink | |
	 self isEmpty
		ifTrue: [firstLink ← aLink]
		ifFalse: [lastLink nextLink: aLink].
	 aLink nextLink: nil.
	 lastLink ← aLink
! !
!LinkedList methodsFor: 'undefined' stamp: ''!
addFirstLink: aLink | |
	 self isEmpty ifTrue: [lastLink ← aLink].
	 aLink nextLink: firstLink.
	 firstLink ← aLink
! !
!LinkedList methodsFor: 'undefined' stamp: ''!
isEmpty | | ^firstLink==nil
! !
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
outline | |
	window outline: 1
! !
!ListPane methodsFor: 'undefined' stamp: ''!
scrollTo: f | place y |
	^self scrollUp: (f * lastline) asInteger - firstShown + 1 * style lineheight
! !
!ListPane methodsFor: 'undefined' stamp: ''!
windowleave | |
	self compselection.
	self grayselection
! !
!ListPane methodsFor: 'undefined' stamp: ''!
windowenter | |
	 "Refresh my image.  Reaffirm selection."
	self outline.
	self displayall.
	self compselection
! !
!ListPane methodsFor: 'undefined' stamp: ''!
scrollPos | |
	((firstShown ≡ nil or: [list ≡ nil]) or: [list length = 0])
	  ifTrue: [^0.0].
	^(firstShown - 1) asFloat / list length
! !
!ListPane methodsFor: 'undefined' stamp: ''!
firsttime | |
	(window has: user mp)
	  ifTrue: [self enter]
	  ifFalse: [^false]
! !
!ListPane methodsFor: 'undefined' stamp: ''!
close | |
	 "Zero my selection so it wont be grayed when I close.  Break cycles."
	selection ← 0.
	scrollBar close
! !
!ListPane methodsFor: 'undefined' stamp: ''!
outside | |
	^scrollBar startup
! !
!ListPane methodsFor: 'undefined' stamp: ''!
enter | |
	scrollBar show
! !
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
picked | |
	^window has: user mp
! !
!ListPane methodsFor: 'undefined' stamp: ''!
leave | |
	scrollBar hide
! !
!ListPane methodsFor: 'undefined' stamp: ''!
lasttime | |
	self leave
! !
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
kbd | |
	window flash.
	user kbd
! !
!ListPane methodsFor: 'undefined' stamp: ''!
frame← t1 | |
	 "(Re)initialize my window"
	window ← t1.
	(frame ← window inset: 2 ⌾ 0 and: [0 ⌾ 0]) width← 999.
	self fill.
	scrollBar ← (scrollBar ≡ nil
			  ifTrue: [ScrollBar new]
			  ifFalse: [scrollBar]) on: window from: self
! !
!ListPane methodsFor: 'undefined' stamp: ''!
yellowbug | |
	window flash
! !
!ListPane methodsFor: 'undefined' stamp: ''!
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
!ListPane methodsFor: 'undefined' stamp: ''!
lastShown: new | delta deltaPt |
	delta ← new - lastShown "negative".
	deltaPt ← 0 ⌾ (0 - delta * style lineheight) "positive".
	lastShown ← new.
	firstShown ← firstShown + delta.
	frame moveby: deltaPt.
	(Rectangle new origin: window origin extent: window width ⌾ (window height - deltaPt y | style lineheight)) blt: window origin + deltaPt mode: storing.
	self displaylines: firstShown to: (firstShown - delta - 1 min: lastShown)
! !
!ListPane methodsFor: 'undefined' stamp: ''!
grayselection | |
	selection ≠ 0
	  ifTrue: [self selectionRect color: ltgray mode: oring]
! !
!ListPane methodsFor: 'undefined' stamp: ''!
firstShown: new | delta deltaPt |
	delta ← new - firstShown.
	deltaPt ← 0 ⌾ (0 - delta * style lineheight) "negative".
	firstShown ← new.
	lastShown ← lastShown + delta.
	frame moveby: deltaPt.
	(window origin - deltaPt rect: window corner) blt: window origin mode: storing.
	self displaylines: (firstShown max: lastShown - delta + 1) to: lastShown
! !
!ListPane methodsFor: 'undefined' stamp: ''!
selectionRect | newy |
	^(Rectangle new "I have a selection.  Return its highlighting rectangle." origin: frame minX ⌾ (newy ← selection * style lineheight + frame minY) corner: frame maxX ⌾ (newy + style lineheight)) intersect: window
! !
!ListPane methodsFor: 'undefined' stamp: ''!
compselection | |
	 "If I have a selection, complement its image."
	selection ≠ 0
	  ifTrue: [self selectionRect comp]
! !
!ListPane methodsFor: 'undefined' stamp: ''!
dummy | |
	^'▱▱▱▱▱▱▱'
! !
!ListPane methodsFor: 'undefined' stamp: ''!
displayall | |
	self displaylines: firstShown to: lastShown
! !
!ListPane methodsFor: 'undefined' stamp: ''!
init | |
	self para: nil frame: nil
! !
!ListPane methodsFor: 'undefined' stamp: ''!
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
			  ifTrue: [frame origin y← window minY - (selection * style lineheight + style lineheight) + (window maxY - window minY | style lineheight) min: window minY]]].
	firstShown ← self lineofy: window minY + (style lineheight - 1).
	lastShown ← self lineofy: window maxY - (style lineheight - 1)
! !
!ListPane methodsFor: 'undefined' stamp: ''!
scrollUp: amount | linetomove |
	self compselection.
	amount ← amount | style lineheight.
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
!ListPane methodsFor: 'undefined' stamp: ''!
dirty | |
	 "My subclasses may want to prohibit a change of selection"
	^false
! !
!ListPane methodsFor: 'undefined' stamp: ''!
locked | |
	 "My subclasses may want to prohibit a change of selection"
	^selection = 0
	  ifTrue: [false]
	  ifFalse: [self dirty]
! !
!ListPane methodsFor: 'undefined' stamp: ''!
selected | |
	 "A new selection is highlighted.  I dont care, but my subclasses might"
! !
!ListPane methodsFor: 'undefined' stamp: ''!
deselected | |
	 "I just lost my selection.  I dont care, but my subclasses might."
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
low | |
	^low
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
high | |
	^high
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
low← t1 | |
	low ← t1.
	^self
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
high← t1 | |
	high ← t1.
	^self
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
extract | x |
	x ← low "returns low, moves high down and propagates sign.".
	low ← high.
	high ← ((low land: 128) = 0
			  ifTrue: [0]
			  ifFalse: [255]).
	^x
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
mdiv: y | x |
	high > y "Ignores y high (assumes it to be zero. Also assumes that y > high.
      This does a single precision unsigned divide into a double precision dividend
      that results in a single precision quotient (returned) and
      a single precision remainder(placed in self high).
		Fixed for NT 15-bit Integers - Dan I."
	  ifTrue: [user notify: 'illegal MachineDouble division']
	  ifFalse:
		[high < 64
		  ifTrue:
			[x ← high * 256 + low.
			high ← x \ y.
			^x / y].
		high < 128
		  ifTrue:
			[x ← (high lshift: 2) + (low lshift: ¬6) - y.
			high ← x lshift: ¬2.
			low ← low field: 38 ← x.
			^(self mdiv: y) + 64].
		x ← (high lshift: 1) + (low lshift: ¬7) - y.
		high ← x lshift: ¬1.
		low ← low field: 23 ← x.
		^(self mdiv: y) + 128]
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
increaseby: y | x |
	 "y is a positive <256 integer"
	x ← low + y.
	x > 255
	  ifTrue: [high ← high + 1 land: 255].
	low ← x land: 255.
	^self
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
gets: x mtimes: y | xh xl yh yl p1 p2 |
	xh ← x lshift: ¬4 "x and y are 8-bit positive #'s.
      Does single precision unsigned multiplication
      returning a double precision result.".
	xl ← x land: 15.
	yh ← y lshift: ¬4.
	yl ← y land: 15.
	low ← yl * xl.
	high ← yh * xh.
	p2 ← yh * xl.
	p1 ← p2 + (yl * xh).
	high ← high + (p1 lshift: ¬4).
	low ← ((p1 land: 15) lshift: 4) + low.
	low ≥ 256
	  ifTrue:
		[high ← high + 1.
		low ← low - 256].
	^self
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
decreaseby: y | x |
	 "y is a positive <256 integer"
	x ← low - y.
	x < 0
	  ifTrue: [high ← high - 1 land: 255].
	low ← x land: 255.
	^self
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
< arg | |
	high = arg high
	  ifTrue: [^low < arg low].
	^high < arg high
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm append: '[MachineDouble 0'.
	high printOn: strm base: 8.
	strm append: ' 0'.
	low printOn: strm base: 8.
	strm append: ']'
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
init | |
	low ← 0.
	high ← 0.
	^self
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
classInit | |
	low4 ← 64 "low4 is a field description for the low order 4 bits of an Integer
      high4 is a field description for the high order 4 bits of an 8-bit Integer".
	high4 ← 68
! !
!MachineDouble methodsFor: 'undefined' stamp: ''!
asInt | n i |
	 "may return a negative number"
	^(high lshift: 8) lor: low
! !
!Menu methodsFor: 'undefined' stamp: ''!
rescan | |
	 " | each. Menu allInstances notNil transform⦂ each to⦂ each rescan."
	self string: str "rescan (for new fonts, lineheight)"
! !
!Menu methodsFor: 'undefined' stamp: ''!
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
!Menu methodsFor: 'undefined' stamp: ''!
stringFromVector: v | s |
	s ← Stream default "DW classInit".
	v do:
		[:v | s append: v.
		s cr].
	self string: s contents
! !
!Menu methodsFor: 'undefined' stamp: ''!
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
!Menu methodsFor: 'undefined' stamp: ''!
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
!Menu methodsFor: 'undefined' stamp: ''!
bug | index bits |
	bits ← self movingsetup "set up and save background".
	index ← self bugit "get the index".
	frame bitsFromString: bits "restore background".
	^index "return index"
! !
!Menu methodsFor: 'undefined' stamp: ''!
wbug | index bits |
	bits ← self movingsetup "save background, display menu" "wait until a mouse button is down".
	[user anybug] whileFalseDo:  [ "get selection (possibly 0)"
		].
	index ← self bugit "restore background".
	frame bitsFromString: bits.
	^index
! !
!Menu methodsFor: 'undefined' stamp: ''!
has: pt | |
	^text frame has: pt
! !
!Menu methodsFor: 'undefined' stamp: ''!
clear | |
	frame clear
! !
!Menu methodsFor: 'undefined' stamp: ''!
frame | |
	^frame
! !
!Menu methodsFor: 'undefined' stamp: ''!
rebug | |
	user waitbug "wait for button down again".
	^self bug "bugcursor showwhile⦂"
! !
!Menu methodsFor: 'undefined' stamp: ''!
zbug | index bits |
	bits ← self movingsetup.
	[(index ← self bugit) = 0] whileTrueDo: [].
	frame bitsFromString: bits.
	^index
! !
!Menu methodsFor: 'undefined' stamp: ''!
moveto: pt | |
	self clear.
	frame moveto: pt.
	text frame moveto: pt + 2.
	thisline moveto: pt + 2
! !
!Menu methodsFor: 'undefined' stamp: ''!
show | |
	frame clear: black.
	text displayall
! !
!Menu methodsFor: 'undefined' stamp: ''!
fbug | index |
	index ← self bugit "for fixed menus" "get the index".
	^index "return index"
! !
!Message methodsFor: 'undefined' stamp: ''!
selector | |
	^selector
! !
!Message methodsFor: 'undefined' stamp: ''!
selector: sel arg: arg | |
	selector ← sel.
	args ← {arg}
! !
!Message methodsFor: 'undefined' stamp: ''!
selector: sel args: vec | |
	selector ← sel.
	args ← vec
! !
!Message methodsFor: 'undefined' stamp: ''!
args | |
	^args
! !
!Message methodsFor: 'undefined' stamp: ''!
selector: sel | |
	selector ← sel.
	args ← #()
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
for: number every: sixtieths onClass: newClass andMethod: newMethod | |
	self abort.
	timer ← Timer new for: sixtieths action⦂ [
				((number ← number - 1) ≤ 0
				  ifTrue: [timer disable]
				  ifFalse:
					[self tally: Top ◦ 1.
					timer reset])].
	self class: newClass method: newMethod.
	timer reset
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
class: t1 method: t2 | |
	class ← t1.
	method ← t2.
	tally ← 0.
	rcvrs ← Vector new: 0
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
method | |
	^method
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
bump: n | |
	tally ← tally + n
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
tally | |
	^tally
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
treeprinton: s tab: tab total: total over: threshold | i mt |
	tally ≤ threshold
	  ifFalse:
		[tab > 0
		  ifTrue:
			[(1 to: tab - 1) do: [:i | s append: '  |'].
			self printon: s total: total.
			s cr].
		self breakdown sort do: [:mt | mt treeprinton: s tab: tab + 1 total: total over: threshold]]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
cumprinton: s from: set total: total over: threshold | mt |
	set contents sort do:
		[:mt | mt tally > threshold
		  ifTrue:
			[mt printon: s total: total.
			s cr]
		  ifFalse: [^self]]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
into: set | mt i |
	(i ← set find: self)
	  ifTrue: [mt ← set objects ◦ i]
	  ifFalse: [set insert: (mt ← MessageTally new class: class method: method)].
	mt bump: tally
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
hash | |
	^method asOop
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
primitives: t1 | |
	tally ← t1.
	rcvrs ← nil
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
breakdown | n b mt t4 |
	b ← rcvrs.
	(b ≡ nil or: [b length = 0])
	  ifTrue: [^#()].
	n ← tally.
	b do: [:mt | n ← n - mt tally].
	n > 0
	  ifTrue: [b ← {b , ((t4 ← MessageTally new) class: class method: method.
				t4 primitives: n)}].
	^b
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
printon: s total: total | i v |
	v ← (0.0 + tally / total * 1000.0 + 0.5) asInteger asString.
	i ← v length.
	s append: '  ' ◦ (i to: 2).
	s append: v ◦ (1 to: i - 1).
	s append: '.'.
	s next← v ◦ i.
	s space.
	rcvrs ≡ nil
	  ifTrue: [s append: 'primitives']
	  ifFalse: [class describe: method on: s]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
= mt | |
	^mt method ≡ method
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
< mt | |
	^tally > mt tally
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
> mt | |
	^tally < mt tally
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
leaves: ldict | b mt |
	b ← self breakdown.
	b length = 0
	  ifTrue: [self into: ldict]
	  ifFalse: [b do: [:mt | mt leaves: ldict]]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
bump | |
	tally ← tally + 1
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
tallyPath: context | m path mt c |
	m ← context method.
	path ← false.
	rcvrs do:
		[:mt | mt method ≡ m
		  ifTrue: [path ← mt]].
	path ≡ false
	  ifTrue:
		[path ← MessageTally new class: context receiver class method: m.
		rcvrs ← {rcvrs , path}].
	^path bump
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
printOn: s | |
	class ≡ nil
	  ifTrue: [super printOn: s]
	  ifFalse: [self printOn: s total: 100]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
fullprinton: s cutoff: pct | set mt i t |
	user displayoffwhile⦂ [
		(s print: tally.
		s append: ' tallies'.
		s cr.
		tally = 0
		  ifFalse:
			[s cr.
			s cr.
			(pct is: Vector)
			  ifFalse: [pct ← {pct , pct}].
			pct ◦ 1 < 100
			  ifTrue:
				[s append: '**Leaves**'.
				s cr.
				t ← (pct ◦ 1 * (tally - 1) / 100) asInteger.
				set ← HashSet new init: 128.
				self leaves: set.
				self cumprinton: s from: set total: tally over: t.
				s next← 12.
				s cr.
				set ← nil].
			pct ◦ 2 < 100
			  ifTrue:
				[s append: '**Tree**'.
				s cr.
				t ← (pct ◦ 2 * (tally - 1) / 100) asInteger.
				self treeprinton: s tab: 0 total: tally over: t.
				s next← 12.
				s cr].
			s skip: ¬2])]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
report: filename cutoff: pct | f |
	 "pct=(leaves,roots,tree) or one number for all"
	f ← dp0 file: filename.
	f append: filename.
	f space.
	self fullprinton: f cutoff: pct.
	f close
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
report: filename | |
	self report: filename cutoff: 2
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
moreon⦂ remote | val |
	 "Spy on the specified evaluation without resetting"
	class ← remote receiver "use as follows:
eachtime  
	[spy every: 10.
	⇑spy moreon⦂ [super eachtime].
	]" class.
	method ← remote method.
	timer reset.
	val ← remote eval.
	timer disable.
	^val
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
close | |
	 "release storage"
	class ← method ← tally ← rcvrs ← nil
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
classInit | |
	Smalltalk define: #spy as: (MessageTally new every: 10)
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
reset | |
	tally ← 0 "reset stats".
	rcvrs ← Vector new: 0
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
from: context | |
	 "Create a tallier from the specified root"
	self class: context receiver class method: context method
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
every: sixtieths | |
	 "Create a spy that samples with the specified period"
	self abort.
	timer ← Timer new for: sixtieths action⦂ [
				(self tally: Top ◦ 1.
				timer reset)]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
abort | |
	(timer is: Timer)
	  ifTrue: [timer disable]
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
tally: context | root |
	 "Explicitly tally the specified context and its stack"
	context method ≡ method
	  ifTrue: [^self bump].
	(root ← context sender) ≡ nil
	  ifTrue: [^self bump tallyPath: context].
	^(self tally: root) tallyPath: context
! !
!MessageTally methodsFor: 'undefined' stamp: ''!
on⦂ remote | val |
	 "Spy on the specified evaluation"
	self from: remote.
	timer reset.
	val ← remote eval.
	timer disable.
	^val
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
tempAt: index put: value | |
	^self ◦ index ← value
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
method | |
	^method
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
receiver | |
	^receiver
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
tempAt: index | |
	^self ◦ index
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
blockCopy: nargs | |
	^(BlockContext new: self length) home: self startpc: pc + 2 nargs: nargs
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
remoteCopy | |
	^self blockCopy: 0
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
erase | |
	receiver ← nil.
	super erase
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
send: selector to: rcvr | nargs i mcl context |
	mcl ← rcvr class.
	[mcl md has: selector] whileFalseDo: 
		[mcl ← mcl superclass.
		mcl ≡ nil
		  ifTrue: [user notify: 'Message not understood: ' + selector]].
	method ← mcl method: selector.
	context ← MethodContext new: method frameSize.
	context sender: self receiver: rcvr mclass: mcl method: method.
	nargs ← method numArgs.
	(1 to: nargs) do: [:i | context temp: (nargs - i + 1) ← self pop].
	context pc: method initialPC stackp: nargs + method numTemps.
	^context "
 | NTP. NTP← MethodContext new.  (NTP send: ↪goBaby to: NTP) inspect.
"
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
goBaby | height |
	NoteTaker ← true.
	externalViews ← Set new vector: 10.
	height ← 480.
	user currentDisplay: (Form new extent: 640 ⌾ height bits: (Bitmap new: 640 / 16 * height) offset: nil).
	user restore. user waitbug.
	[true] whileTrueDo: [self run]
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
systemRestart | |
	thisContext releaseTo: RestartCode home.
	RestartCode eval
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
pc: t1 stackp: t2 | |
	pc ← t1.
	stackp ← t2
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
run | |
	self setRestart⦂ [(^nil)].
	user restore.
	user run
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
sender: t1 receiver: t2 mclass: ignored method: t4 | |
	sender ← t1.
	receiver ← t2.
	method ← t4
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
restart | |
	pc ← method initialPC.
	stackp ← method numArgs + method numTemps
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
setRestart⦂ code | |
	RestartCode ← code
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
goBaby | height |
	NoteTaker ← true.
	MethodKeeper ← (Vector new: 10) asStream.
	externalViews ← Set new vector: 10.
	height ← 760 " 480 376 ".
	user currentDisplay: (Form new extent: 640 ⌾ height bits: (Bitmap new: 640 / 16 * height) offset: nil).
	[true] whileTrueDo: [self run]
! !
!MethodContext methodsFor: 'undefined' stamp: ''!
systemRestart | |
	thisContext releaseTo: RestartCode home.
	NormalCursor topage1.
	RestartCode eval
! !
!MethodDictionary methodsFor: 'undefined' stamp: ''!
delete: key  | |
	self become: (self copy deleteDangerously: key)
! !
!MethodDictionary methodsFor: 'undefined' stamp: ''!
deleteDangerously: key  | |
	super delete: key
! !
!MethodDictionary methodsFor: 'undefined' stamp: ''!
method: sel | |
	^self at: sel ifAbsent⦂ [(self error: sel + ' not found')]
! !
!MethodDictionary methodsFor: 'undefined' stamp: ''!
methodorfalse: sel | |
	^self at: sel ifAbsent⦂ [(^false)]
! !
!MethodDictionary methodsFor: 'undefined' stamp: ''!
has: key | probe |
	(probe ← super at: (self find: key)) ≡ nil
	  ifTrue: [^false].
	^probe key = key
! !
!NameUser methodsFor: 'undefined' stamp: ''!
socProcess: Ipac | i j best bestHops |
	 "overwrite from Socket"
	 "called from Ether stuff, running at a very high level"
	resultSet "dummy block"
	  ifFalse:
		[ "we are not waiting!!" "must be the answer, or an error"
		self timerOff.
		resultSet ← true.
		146 = Ipac pupType "error"
		  ifFalse:
			[ "user show: (Ipac dataString). "
			.
			145 ≠ Ipac pupType "error"
			  ifFalse:
				[ "user show: 'unknown pup received by name user.'" "an answer arrived"
				.
				result ← Ipac dataString "1 or more 6 byte blocks".
				result length = 6
				  ifFalse:
					[ "all done" "more than one, find the nearest address"
					.
					best ← 1.
					bestHops ← 16.
					(1 to: result length by: 6) do:
						[:i | NETNUM = (result ◦ i)
						  ifTrue:
							[best ← i.
							bestHops ← 0]
						  ifFalse:
							[.
							j ← routingHopCount ◦ (result ◦ i).
							j < bestHops
							  ifTrue:
								[best ← i.
								bestHops ← j]
							  ifFalse:
								[]]].
					result ← result copy: best to: best + 5]]]] "dummy block" "all done".
	self freePacket: Ipac
! !
!NameUser methodsFor: 'undefined' stamp: ''!
timerFired | |
	self timerOn
	  ifTrue: [self completePup: outPac]
	  ifFalse: [resultSet ← true]
! !
!NameUser methodsFor: 'undefined' stamp: ''!
init | |
	E wakeup "create a NameUser, to socket 4, from a default local socket number".
	self net: 0 host: 0 soc: 4.
	self retransmit: 2 every: 300
! !
!NameUser methodsFor: 'undefined' stamp: ''!
getAddressString: str | temp |
	 "return string representation"
	(temp ← self getAddressBlock: str)
	  ifTrue: [^(temp ◦ 1) base8 + '#' + (temp ◦ 2) base8 + '#' + (temp word: 2) base8 + '|' + (temp word: 3) base8].
	^false
! !
!NameUser methodsFor: 'undefined' stamp: ''!
getName: str | |
	 "convert string address back to host name"
	 "not implemented yet"
! !
!NameUser methodsFor: 'undefined' stamp: ''!
getAddressBlock: str | |
	result ← resultSet ← false "returns a string, 6 bytes: net/host/socket".
	outPac ← self freePacket.
	outPac pupType← 144.
	outPac dataString← str.
	self setAddressesAndComplete: outPac.
	[resultSet] whileFalseDo:  [].
	^result
! !
!Natural methodsFor: 'undefined' stamp: ''!
natdiv: arg | quo rem ql d div dh dnh z z2 dl q i j k l carry digit flag |
	l ← self length "returns a vector of (quotient, remainder)" - arg length + 1.
	l ≤ 0
	  ifTrue: [^{Naturalzero , self}].
	d ← 8 - arg last hibit.
	rem ← self natnormalize: d "makes a copy and shifts".
	div ← arg natnormalize: d "shifts so high order word is >127".
	quo ← Natural new: l.
	dl ← div length - 1.
	ql ← l.
	dh ← div ◦ dl.
	dnh ← (dl = 1
			  ifTrue: [0]
			  ifFalse: [div ◦ (dl - 1)]).
	z ← MachineDouble init.
	z2 ← MachineDouble new.
	(1 to: ql) do:
		[:k |  "maintain quo*arg+rem=self"
		j ← rem length + 1 - k.
		z high← rem ◦ j.
		z high = dh
		  ifTrue: [q ← ¬1]
		  ifFalse:
			[z low← rem ◦ (j - 1).
			q ← z mdiv: dh.
			z low← (j < 3
			  ifTrue: [0]
			  ifFalse: [rem ◦ (j - 2)]).
			z2 gets: q mtimes: dnh.
			flag ← true.
			[z < z2 and: [flag]] whileTrueDo:
				[q ← q unsignedadd: ¬1.
				z2 decreaseby: dnh.
				z2 high < dh
				  ifTrue: [flag ← false]
				  ifFalse: [z2 high← z2 high - dh]]].
		l ← j - dl.
		z2 init.
		carry ← 0.
		(1 to: div length) do:
			[:i | z gets: q mtimes: div ◦ i.
			z2 increaseby: rem ◦ l.
			z2 decreaseby: carry "subtract q * div from rem".
			z2 decreaseby: z low.
			carry ← z high.
			rem ◦ l ← z2 extract.
			l ← l + 1].
		z2 low = 255
		  ifTrue:
			[q ← q unsignedadd: ¬1.
			l ← j - dl.
			z init.
			(1 to: div length) do:
				[:i | z increaseby: rem ◦ l.
				z increaseby: div ◦ i.
				rem ◦ l ← z extract.
				l ← l + 1]].
		quo ◦ (quo length + 1 - k) ← q].
	rem ← rem natunnormalize: d lookfirst: dl.
	quo last = 0
	  ifTrue:
		[ql < 2
		  ifFalse: [quo ← quo growby: ¬1]].
	^{quo , rem}
! !
!Natural methodsFor: 'undefined' stamp: ''!
natadd: arg | shorter longer i z sum |
	z ← MachineDouble init "returns a Natural number".
	self length < arg length
	  ifTrue:
		[longer ← arg.
		shorter ← self]
	  ifFalse:
		[longer ← self.
		shorter ← arg].
	sum ← Natural new: longer length.
	(1 to: longer length) do:
		[:i | z increaseby: longer ◦ i.
		z increaseby: shorter ◦ i.
		sum ◦ i ← z extract].
	z low ≠ 0
	  ifTrue:
		[sum ← sum growby: 1.
		sum last← z low].
	^sum
! !
!Natural methodsFor: 'undefined' stamp: ''!
nattimes: arg | prod z pl carry digit i j k |
	(self length = 1 and: [self ◦ 1 = 0])
	  ifTrue: [^Naturalzero].
	pl ← self length + arg length.
	prod ← Natural new: pl.
	z ← MachineDouble new.
	(1 to: pl) do: [:i | prod ◦ i ← 0].
	(1 to: self length) do:
		[:i | k ← i - 1.
		carry ← 0.
		digit ← self ◦ i.
		digit ≠ 0
		  ifTrue:
			[(1 to: arg length) do:
				[:j | z gets: digit mtimes: arg ◦ j.
				z increaseby: carry.
				k ← k + 1.
				z increaseby: prod ◦ k "k=i+j-1".
				prod ◦ k ← z low.
				carry ← z high].
			prod ◦ (k + 1) ← carry]].
	prod ◦ pl = 0
	  ifTrue: [^prod growby: ¬1].
	^prod
! !
!Natural methodsFor: 'undefined' stamp: ''!
natsubtract: arg | shorter longer i z sum sl al ng lastdigit |
	sl ← self length "returns an Integer that is created by this operation".
	al ← arg length.
	z ← MachineDouble init.
	sl = al
	  ifTrue:
		[i ← sl.
		[self ◦ i = (arg ◦ i) and: [i > 1]] whileTrueDo: [i ← i - 1].
		sl ← i.
		(self ◦ i unsignedlessthan: arg ◦ i)
		  ifTrue:
			[longer ← arg.
			ng ← true.
			shorter ← self]
		  ifFalse:
			[longer ← self.
			shorter ← arg.
			ng ← false]]
	  ifFalse:
		[sl < al
		  ifTrue:
			[longer ← arg.
			shorter ← self.
			ng ← true.
			sl ← al]
		  ifFalse:
			[longer ← self.
			shorter ← arg.
			ng ← false]].
	sum ← Natural new: longer length.
	lastdigit ← 1.
	(1 to: longer length) do:
		[:i | z increaseby: longer ◦ i.
		z decreaseby: shorter ◦ i.
		sum ◦ i ← z extract ≠ 0
		  ifTrue: [lastdigit ← i]].
	lastdigit = longer length
	  ifFalse:
		[z ← Natural new: lastdigit.
		(1 to: lastdigit) do: [:i | z ◦ i ← sum ◦ i].
		sum ← z].
	^LargeInteger new bytes: sum neg: ng
! !
!Natural methodsFor: 'undefined' stamp: ''!
natnormalize: n | x i r f digit |
	r ← Natural new: self length "n is the number of bits to shift by. The Natural number returned will be written over repeatedly, so we must make a new one." + 1.
	x ← 0.
	f ← n - 8.
	(1 to: r length) do:
		[:i | digit ← self ◦ i.
		r ◦ i ← ((digit lshift: n) lor: x) land: 255.
		x ← digit lshift: f].
	^r
! !
!Natural methodsFor: 'undefined' stamp: ''!
natcompare: arg | i len t4 t5 |
	 "speeded up for Integer args, same speed for LargeInteger (Natural) args"
	len ← self length.
	(t4 ← arg length) < len
	  ifTrue: [^3].
	t4 > len
	  ifTrue: [^1].
	(len to: 1 by: ¬1) do:
		[:i | (t5 ← arg ◦ i) < (self ◦ i)
		  ifTrue: [^3].
		t5 > (self ◦ i)
		  ifTrue: [^1]].
	^2
! !
!Natural methodsFor: 'undefined' stamp: ''!
natdivideandCarry: arg extra: pair | i len z |
	z ← pair ◦ 2 "arg is an integer < 256 - returns remainder, smashes self to quotient - pair is a 2-vector of len (index of high order non-zero word in self) and a MachineDouble - be careful!!!".
	z high← 0.
	len ← pair ◦ 1.
	(len to: 1 by: ¬1) do:
		[:i | z low← self ◦ i.
		self ◦ i ← z mdiv: arg].
	self ◦ len = 0
	  ifTrue:
		[len ← len - 1.
		len = 0
		  ifTrue: [len ← 1]].
	pair ◦ 1 ← len.
	^z high
! !
!Natural methodsFor: 'undefined' stamp: ''!
natunnormalize: n lookfirst: a | x i r f digit |
	n ← 0 - n.
	x ← 0.
	f ← n + 8.
	i ← a.
	digit ← self ◦ i.
	[((digit lshift: n) lor: x) = 0 and: [i ≠ 1]] whileTrueDo:
		[x ← digit lshift: f.
		i ← i - 1.
		digit ← self ◦ i].
	r ← Natural new: i.
	a ← i.
	x ← self ◦ 1 lshift: n.
	(1 to: a) do:
		[:i | digit ← self ◦ (i + 1).
		r ◦ i ← ((digit lshift: f) lor: x) land: 255.
		x ← digit lshift: n].
	^r
! !
!Natural methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	self printOn: strm base: 10
! !
!Natural methodsFor: 'undefined' stamp: ''!
asInteger | |
	self length = 1
	  ifTrue: [^self ◦ 1].
	^LargeInteger new bytes: self neg: false
! !
!Natural methodsFor: 'undefined' stamp: ''!
species | |
	^Natural
! !
!Natural methodsFor: 'undefined' stamp: ''!
isLarge | |
	^false
! !
!Natural methodsFor: 'undefined' stamp: ''!
classInit | |
	Naturalzero ← Natural new: 1.
	Naturalzero ◦ 1 ← 0.
	^self
! !
!Natural methodsFor: 'undefined' stamp: ''!
printOn: strm base: b | p z n b2 x |
	 "only works if b≤10"
	p ← Stream default.
	z ← {self length , MachineDouble new}.
	n ← Natural new: self length.
	b2 ← b * b.
	self copyto: n.
	[z ◦ 1 = 1 and: [n ◦ 1 < b2]] whileFalseDo: 
		[x ← n natdivideandCarry: b2 extra: z.
		p next← x \ b + 48.
		p next← x / b + 48].
	n ◦ 1 printOn: strm base: b.
	strm append: p contents reverse
! !
!Natural methodsFor: 'undefined' stamp: ''!
◦ n | |
	self length < n "changed from super for Glenn"
	  ifTrue: [^0].
	^super ◦ n
! !
!NotifyWindow methodsFor: 'undefined' stamp: ''!
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
!NotifyWindow methodsFor: 'undefined' stamp: ''!
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
!NotifyWindow methodsFor: 'undefined' stamp: ''!
classInit | |
	smallTemplates ← (0 ⌾ 0 rect: 36 ⌾ 36) inVector.
	bigTemplates ← {(0 ⌾ 0 rect: 12 ⌾ 18) , (12 ⌾ 0 rect: 36 ⌾ 18) , (0 ⌾ 18 rect: 12 ⌾ 27) , (12 ⌾ 18 rect: 36 ⌾ 27) , (0 ⌾ 27 rect: 12 ⌾ 36) , (12 ⌾ 27 rect: 36 ⌾ 36)}.
	smallFrame ← 204 ⌾ 366 rect: 404 ⌾ 402
! !
!NotifyWindow methodsFor: 'undefined' stamp: ''!
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
!NotifyWindow methodsFor: 'undefined' stamp: ''!
close | |
	super close.
	self erase.
	user unschedule: self.
	thisContext systemRestart
! !
!NotifyWindow methodsFor: 'undefined' stamp: ''!
closeNoRestart | |
	^super close
! !
!NotifyWindow methodsFor: 'undefined' stamp: ''!
aboutToFrame | |
	enoughpanes ← panes length = 6.
	super aboutToFrame
! !
!NotifyWindow methodsFor: 'undefined' stamp: ''!
enter | |
	enoughpanes
	  ifTrue: [super enter]
	  ifFalse: [self of: title stackPane: panes ◦ 1 codePane: CodePane new]
! !
!Number methodsFor: 'undefined' stamp: ''!
odd || ^self even≡false
! !
!Number methodsFor: 'undefined' stamp: ''!
abs | |
	 "return the absolute value (positive magnitude) of this number"
	self < 0
	  ifTrue: [^self negated].
	^self
! !
!Number methodsFor: 'undefined' stamp: ''!
divMod: aNumber | |
	 "returns a vector of (quotient, remainder) defined by division with truncation toward negative infinity."
	^{(self//aNumber) , (self\\aNumber)}
! !
!Number methodsFor: 'undefined' stamp: ''!
\\ aNumber | | "modulo"
	 "remainder defined in terms of //.  returns a number with the same sign as aNumber.  e.g.  9\\4 = 1,  -9\\4 = 3, 9\\-4 = -1,  0.9\\0.4 = 0.1 "
	^self - ((self//aNumber) * aNumber)
! !
!Number methodsFor: 'undefined' stamp: ''!
quo: aNumber | |
	 "integer quotient defined by divsion with truncation toward zero.  -9 quo: 4 = -2,  -0.9 quo: 0.4 = -2.  rem: returns the remainder from this division. "
	^(self / aNumber) truncated
! !
!Number methodsFor: 'undefined' stamp: ''!
+ aNumber | |
	self subclassResponsibility
! !
!Number methodsFor: 'undefined' stamp: ''!
intdiv: aNumber | | "temporary"
	^self divMod: aNumber
! !
!Number methodsFor: 'undefined' stamp: ''!
// aNumber | |
	 "integer quotient defined by division with truncation toward negative infinity.  9//4 = 2,  -9//4 = -3.  -0.9//0.4 = -3.  \\ returns the remainder from this division. "
	^(self / aNumber) floor
! !
!Number methodsFor: 'undefined' stamp: ''!
negated | |
	 "circular with -"
	^0 - self
! !
!Number methodsFor: 'undefined' stamp: ''!
- aNumber | |
	 "circular with negated"
	^self + aNumber negated
! !
!Number methodsFor: 'undefined' stamp: ''!
/ aNumber | |
	 "circular with reciprocal"
	^self * aNumber reciprocal
! !
!Number methodsFor: 'undefined' stamp: ''!
squared | |
	^self * self
! !
!Number methodsFor: 'undefined' stamp: ''!
reciprocal | |
	 "circular with /"
	^1 / self
! !
!Number methodsFor: 'undefined' stamp: ''!
sign | |
	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0
! !
!Number methodsFor: 'undefined' stamp: ''!
even | |
	^(self\\2) = 0
! !
!Number methodsFor: 'undefined' stamp: ''!
rem: aNumber | |
	 "remainder defined in terms of quo:.  returns a number with the same sign as self.  e.g. 9 rem: 4 = 1,  -9 rem: 4 = -1.  0.9 rem: 0.4 = 0.1 "
	^self - ((self quo: aNumber) * aNumber)
! !
!Number methodsFor: 'undefined' stamp: ''!
\ aNumber | |
	 ^self \\ aNumber
! !
!Number methodsFor: 'undefined' stamp: ''!
hex4 | strm i x bytes d t |
	strm ← (String new: 5) asStream.
	strm space.
	x ← self asInteger.
	x isLarge
	  ifTrue:
		[bytes ← x bytes.
		d ← bytes last.
		d > 15
		  ifTrue:
			[t ← d / 16.
			strm next← (t < 10
			  ifTrue: [48 + t]
			  ifFalse: [55 + t])].
		t ← d \ 16.
		strm next← (t < 10
		  ifTrue: [48 + t]
		  ifFalse: [55 + t]).
		(bytes length - 1 to: 1 by: ¬1) do:
			[:i | d ← bytes ◦ i.
			t ← d / 16.
			strm next← (t < 10
			  ifTrue: [48 + t]
			  ifFalse: [55 + t]).
			t ← d \ 16.
			strm next← (t < 10
			  ifTrue: [48 + t]
			  ifFalse: [55 + t])].
		^strm contents].
	bytes ← String new: 4.
	(1 to: 4) do:
		[:i | bytes ◦ i ← x \ 16.
		x ← x lshift: ¬4].
	(4 to: 1 by: ¬1) do:
		[:i | t ← bytes ◦ i.
		strm next← (t < 10
		  ifTrue: [48 + t]
		  ifFalse: [55 + t])].
	^strm contents
! !
!Number methodsFor: 'undefined' stamp: ''!
bits: interval | |
	^self bits: interval start to: interval stop
! !
!Number methodsFor: 'undefined' stamp: ''!
hex | |
	^self base: 16
! !
!Number methodsFor: 'undefined' stamp: ''!
ipow: x | | ^self raisedTo: x
! !
!Number methodsFor: 'undefined' stamp: ''!
retry: aUniqueString coercing: aNumber | t3 |
	(t3 ← self generality compare: aNumber generality) = 1
	  ifTrue: [^(aNumber coerce: self) perform: aUniqueString with: aNumber].
	t3 = 3
	  ifTrue: [^self perform: aUniqueString with: (self coerce: aNumber)].
	user notify: 'coercion attempt failed'
! !
!Number methodsFor: 'undefined' stamp: ''!
differentGenerality: aNumber | |
	^self generality ≠ aNumber generality
! !
!Number methodsFor: 'undefined' stamp: ''!
moreGeneralThan: aNumber | |
	^self generality > aNumber generality
! !
!Number methodsFor: 'undefined' stamp: ''!
min: aNumber | |
	self < aNumber
	  ifTrue: [^self].
	^aNumber
! !
!Number methodsFor: 'undefined' stamp: ''!
near: aNumber error: relativeError | |
	 "for testing near equality, e.g. error convergence"
	self = aNumber
	  ifTrue: [^true].
	^self near: aNumber within: (self abs min: aNumber abs) * relativeError
! !
!Number methodsFor: 'undefined' stamp: ''!
compare: aNumber | | "change to ^(self-aNumber) sign ?"
	self < aNumber
	  ifTrue: [^1].
	self = aNumber
	  ifTrue: [^2].
	^3
! !
!Number methodsFor: 'undefined' stamp: ''!
max: aNumber | |
	self > aNumber
	  ifTrue: [^self].
	^aNumber
! !
!Number methodsFor: 'undefined' stamp: ''!
= aNumber | |
	self subclassResponsibility
! !
!Number methodsFor: 'undefined' stamp: ''!
< aNumber | |
	self subclassResponsibility
! !
!Number methodsFor: 'undefined' stamp: ''!
≥ aNumber | |
	^self < aNumber ≡ false
! !
!Number methodsFor: 'undefined' stamp: ''!
≠ aNumber | |
	^self = aNumber ≡ false
! !
!Number methodsFor: 'undefined' stamp: ''!
within: anInterval | |
	 "return true if I fall within this Interval"
	^anInterval start ≤ self and: [self ≤ anInterval stop]
! !
!Number methodsFor: 'undefined' stamp: ''!
> n | |
	^n < self
! !
!Number methodsFor: 'undefined' stamp: ''!
≤ aNumber | |
	^aNumber < self ≡ false
! !
!Number methodsFor: 'undefined' stamp: ''!
between: min and: max | |
	^self ≥ min and: [self ≤ max]
! !
!Number methodsFor: 'undefined' stamp: ''!
near: aNumber | |
	^self near: aNumber error: 1.0e-8
! !
!Number methodsFor: 'undefined' stamp: ''!
near: aNumber within: epsilon | |
	 "test absolute difference"
	^(self - aNumber) abs ≤ epsilon
! !
!Number methodsFor: 'undefined' stamp: ''!
isNumber | |
	^true
! !
!Number methodsFor: 'undefined' stamp: ''!
sameAs: anObject | | "temporary?"
	^self = anObject
! !
!Number methodsFor: 'undefined' stamp: ''!
isLarge | |
	^false
! !
!Number methodsFor: 'undefined' stamp: ''!
readFrom: aStream | start radix value |
	aStream ← aStream asStream.
	start ← aStream position.
	radix ← Integer new readFrom: aStream radix: 10.
	(radix is: Error) ifTrue: [^radix].
	aStream ∢ 114
	  ifTrue: "<radix>r<number>"
		[radix <= 1 ifTrue: [^Error new message: 'INVALID RADIX'].
		start ← aStream position.
		value ← Integer new readFrom: aStream radix: radix.
		(value is: Error) ifTrue: [^value]]
	  ifFalse:
		[value ← radix.
		radix ← 10].
	aStream ∢ 46
	  ifTrue:
		[(aStream end ≡ false and: [aStream peek isDigitRadix: radix])
	  	   ifTrue: ".<fraction>"
			[aStream position← start.
			^Float new readFrom: aStream radix: radix]
		   ifFalse:
			[aStream skip: -1.  "back up"
			^value]].
	aStream ∢ 101
	  ifTrue: "e<exponent>"
		[aStream position← start.
		^Float new readFrom: aStream radix: radix].
	^value
! !
!Number methodsFor: 'undefined' stamp: ''!
radiansToDegrees | |
	^self / radiansPerDegree
! !
!Number methodsFor: 'undefined' stamp: ''!
to: stop | |
	 "create an Interval"
	^Interval new from: self to: stop by: 1
! !
!Number methodsFor: 'undefined' stamp: ''!
fractionalPart | |
	^self - self truncated
! !
!Number methodsFor: 'undefined' stamp: ''!
integerPart | |
	^self - self fractionalPart
! !
!Number methodsFor: 'undefined' stamp: ''!
to: stop by: step | |
	 "create an Interval"
	^Interval new from: self to: stop by: step
! !
!Number methodsFor: 'undefined' stamp: ''!
degreesToRadians | |
	^self * radiansPerDegree
! !
!Number methodsFor: 'undefined' stamp: ''!
coerce: aNumber | |
	 "must be defined by all Number classes."
	self subclassResponsibility.
	"e.g. for Integer, 
	^ aNumber asInteger"
! !
!Number methodsFor: 'undefined' stamp: ''!
asDirection | |
	^self cos ⌾ self sin
! !
!Number methodsFor: 'undefined' stamp: ''!
for: n | |
	 "create an Interval, starting at me"
	^Interval new from: self to: self + (n - 1) by: 1
! !
!Number methodsFor: 'undefined' stamp: ''!
asPoint | |
	^self ⌾ self "Return a Point with me as both coordinates."
! !
!Number methodsFor: 'undefined' stamp: ''!
asRectOrigin | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Number methodsFor: 'undefined' stamp: ''!
asRectCorner | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Number methodsFor: 'undefined' stamp: ''!
asRectangle | |
	^self ⌾ self "Return a Rectangle with me as all coordinates." rect: self ⌾ self
! !
!Number methodsFor: 'undefined' stamp: ''!
asPtX | |
	 "pretend to be a Point for Point +-*/"
! !
!Number methodsFor: 'undefined' stamp: ''!
asPtY | |
	 "pretend to be a Point for Point +-*/"
! !
!Number methodsFor: 'undefined' stamp: ''!
classInit | |
 "constants from Computer Approximations, pp. 182-183
	pi = 3.14159265358979323846264338327950288
	pi/2 = 1.57079632679489661923132169163975144
	pi/4 = 0.78539816339744830961566084581987572
	pi*2 = 6.28318530717958647692528676655900576
	pi/180 = 0.01745329251994329576923690768488612
	2.0 ln = 0.69314718055994530941723212145817657
	2.0 sqrt = 1.41421356237309504880168872420969808"

	pi ← 3.1415927.
	halfpi ← pi / 2.0.
	fourthpi ← pi / 4.0.
	twopi ← pi * 2.0.
	radiansPerDegree ← pi / 180.0.
	degreesPerRadian ← 180.0 / pi.
	ln2 ← 0.69314718.
	sqrt2 ← 1.4142136
! !
!Number methodsFor: 'undefined' stamp: ''!
to: limit do: aBlock | | ^(self to: limit by: 1) do: aBlock
! !
!Number methodsFor: 'undefined' stamp: ''!
~ x | |
	^Interval new from: self to: x by: 1
! !
!Number methodsFor: 'undefined' stamp: ''!
⌾ y | |
	^Point new x: self y: y
! !
!Number methodsFor: 'undefined' stamp: ''!
tan | x x2 sum const |
	 "for angles in radians"
	self < 0.0
	  ifTrue: [^self negated tan negated].
	 " normalize to 0≤self≤(pi/4) "
	self > pi
	  ifTrue: [^(self asFloat \\ pi) tan].
	self > halfpi
	  ifTrue: [^(self - halfpi) tan negated].
	self > fourthpi
	  ifTrue: [^1.0 / (halfpi - self) tan].
	sum ← x ← self.
	x2 ← x * x.
	#(0.3333314 0.1333924 0.05337406 0.024565089 0.002900525 0.0095168091 ) do: [:const |  "Now compute the series"
		sum ← const * (x ← x * x2) + sum].
	^sum
! !
!Number methodsFor: 'undefined' stamp: ''!
sqrt | guess i |
	self ≤ 0.0
	  ifTrue:
		[self = 0.0
		  ifTrue: [^0.0].
		user notify: 'sqrt invalid for x<0.']
	  ifFalse:
		[guess ← (self + 0.0) asFloat "copy me into a Float".
		guess instfield: 1 ← (guess instfield: 1)//4 * 2 "and halve expt for first guess".
		(1 to: 5) do: [:i | guess ← self - (guess * guess) / (guess * 2.0) + guess].
		^guess]
! !
!Number methodsFor: 'undefined' stamp: ''!
sin | x x2 sum const |
	 "for angles in radians"
	self < 0.0
	  ifTrue: [^self negated sin negated].
	 " normalize to 0≤self≤(pi/4) "
	self > twopi
	  ifTrue: [^(self asFloat \\ twopi) sin].
	self > pi
	  ifTrue: [^(self - pi) sin negated].
	self > halfpi
	  ifTrue: [^(pi - self) sin].
	sum ← x ← self.
	x2 ← x * x.
	#(0.16666667 0.0083333315 ¬1.98409e¬4 2.7526e¬6 ¬2.39e¬8 ) do: [:const |  "Now compute the series"
		sum ← const * (x ← x * x2) + sum].
	^sum
! !
!Number methodsFor: 'undefined' stamp: ''!
ln | a x x2 n P |
	 "see Computer Approximations, pp. 105-111, p. 227 (LOGE 2663)"
	self ≤ 0.0
	  ifTrue: [user notify: 'ln not valid for ' + self asString]
	  ifFalse:
		[x ← (self + 0.0) asFloat "copy into a Float" "exponent".
		n ← ln2 * ((x instfield: 1)//2 - 0.5) "mantissa between 0.5 and 1.0".
		.
		x instfield: 1 ← 0.
		x ← x * sqrt2.
		x ← x - 1.0 / (x + 1.0).
		x2 ← x * x.
		P ← 0.0 "↪(0.2000000000046727e1 0.666666635059382 0.4000059794795
		0.28525381498 0.2376245609) reverse copy".
		#(0.23762456 0.28525381 0.40000598 0.66666664 2.0 ) do: [:a | P ← P * x2 + a].
		^n + (x * P)]
! !
!Number methodsFor: 'undefined' stamp: ''!
log: base | |
	^self ln / base ln
! !
!Number methodsFor: 'undefined' stamp: ''!
floorLog: base | x |
	 "gives floor log.base self"
	self < base
	  ifTrue: [^0].
	 "self assumed positive"
	self < base squared
	  ifTrue: [^1].
	x ← 2 * (self floorLog: base squared). "binary recursion like ipow"
	^x + (self / (base ipow: x) floorLog: base)
! !
!Number methodsFor: 'undefined' stamp: ''!
floorLog2 | i cnt |
	 "floor of log base 2"
	self < 0
	  ifTrue: [^self negated floorLog2].
	self < 1
	  ifTrue: [^self reciprocal floorLog2 negated].
	i ← 1.
	cnt ← 0.
	[self ≥ i] whileTrueDo:
		[i ← i + i.
		cnt ← cnt + 1].
	^cnt - 1
! !
!Number methodsFor: 'undefined' stamp: ''!
exp | a n1 x x2 P Q |
	 "see Computer Approximations, pp. 96-104, p. 205 (EXPB 1065)"
	self abs > 9212.0 "0.0 maxVal ln"
	  ifTrue: [user notify: 'exp overflow']
	  ifFalse:
		[x ← self asFloat / ln2.
		(n1 ← Float new "2.0 ipow: x truncated") instfield: 1 ← x truncated * 2.
		(x ← x fractionalPart) ≥ 0.5
		  ifTrue:
			[n1 ← n1 * sqrt2.
			x ← x - 0.5].
		x2 ← x * x "compute 2.0 power: x".
		P ← Q ← 0.0 "↪(0.25250428525576241933744e4 0.28875563776168927289e2) reverse copy".
		#(28.875564 2525.0429 ) do: [:a | P ← P * x2 + a "↪(0.72857336028361108885189e4 0.375021654220866600213e3 0.1e1) reverse copy"].
		#(1.0 375.02165 7285.7336 ) do: [:a | Q ← Q * x2 + a].
		^n1 * (Q + (x * P) / (Q - (x * P)))]
! !
!Number methodsFor: 'undefined' stamp: ''!
arcTan | theta term y eps i |
	 "return angle in degrees good to .02 degrees."
	self = 1.0
	  ifTrue: [^45.0].
	self = 1.0
	  ifTrue: [^45.0].
	self * self > 1.0
	  ifTrue:
		[theta ← halfpi.
		y ← 1.0 / (self * self).
		term ← 1.0 / self abs]
	  ifFalse:
		[theta ← 0.0.
		y ← 0.0 - (self * self).
		term ← self abs].
	i ← 1.
	eps ← 1.0e¬4.
	[term abs > eps] whileTrueDo:
		[theta ← theta + term.
		term ← term * y * i asFloat / (i + 2) asFloat.
		i ← i + 2].
	theta ← self sign asFloat * theta * 360.0 / twopi.
	^theta
! !
!Number methodsFor: 'undefined' stamp: ''!
raisedTo: x | |
	 "fixed powers in log n steps"
	x = 0
	  ifTrue: [^1.0].
	x = 1
	  ifTrue: [^self].
	x > 1
	  ifTrue: [^(self * self raisedTo: x//2) * (self raisedTo: x\\2)].
	^(self raisedTo: x negated) reciprocal
! !
!Number methodsFor: 'undefined' stamp: ''!
log2 | |
	 "for VirtualMemory"
	^self floorLog2
! !
!Number methodsFor: 'undefined' stamp: ''!
cos | |
	 "for angles in radians"
	self < 0.0
	  ifTrue: [^(self + halfpi) sin].
	^(halfpi - self) sin
! !
!Number methodsFor: 'undefined' stamp: ''!
epart: aNumber | |
	"Floor of log"
	^(self log: aNumber) floor
! !
!Number methodsFor: 'undefined' stamp: ''!
rounded | |
	 "rounds to the nearest integer"
	^(self + (self < 0
	  ifTrue: [0.5]
	  ifFalse: [0.5])) integerPart
! !
!Number methodsFor: 'undefined' stamp: ''!
ceiling | |
	 "returns the nearest integer toward positive infinity"
	self ≤ 0.0
	  ifTrue: [^self integerPart].
	^self negated floor negated
! !
!Number methodsFor: 'undefined' stamp: ''!
truncated | |
	 "truncates to the nearest integer toward zero"
	self subclassResponsibility
! !
!Number methodsFor: 'undefined' stamp: ''!
floor | trunc |
	 "returns the nearest integer toward negative infinity"
	trunc ← self integerPart.
	self ≥ 0
	  ifTrue: [^trunc].
	self = trunc
	  ifTrue: [^trunc].
	^self truncated - 1
! !
!Number methodsFor: 'undefined' stamp: ''!
isIntegral | |
	^false
! !
!Number methodsFor: 'undefined' stamp: ''!
roundTo: d | |
	^(self / d) rounded * d
! !
!Number methodsFor: 'undefined' stamp: ''!
truncateTo: aNumber | |
	 "truncates to next multiple of aNumber toward zero"
	^(self//aNumber) * aNumber
! !
!Number methodsFor: 'undefined' stamp: ''!
subscripts: a ← val | |
	^a ◦ self asInteger ← val
! !
!Number methodsFor: 'undefined' stamp: ''!
subscripts: a | |
	^a ◦ self asInteger
! !
!Number methodsFor: 'undefined' stamp: ''!
cansubscript: a | |
	^self asInteger cansubscript: a
! !
!Object methodsFor: 'undefined' stamp: ''!
growTo: newLength | |<primitive: 33>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
become: other | |<primitive: 35>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
comment | |
	^''
! !
!Object methodsFor: 'undefined' stamp: ''!
comment: cccc | |
	
! !
!Object methodsFor: 'undefined' stamp: ''!
do: aBlock || ^self asStream do: aBlock
! !
!Object methodsFor: 'undefined' stamp: ''!
sizeInWords | class |
	class← self class.
	class isBytes ifTrue: [^self length+1/2].
	class isVariable ifTrue: [^class instsize+self length].
	^class instsize
! !
!Object methodsFor: 'undefined' stamp: ''!
size | | <primitive: 65>
	user notify: 'Message not understood'
! !
!Object methodsFor: 'undefined' stamp: ''!
length | | <primitive: 65>
	"Old synonym for size"
	user notify: 'Message not understood'
! !
!Object methodsFor: 'undefined' stamp: ''!
at: i | | <primitive: 63>
	^i subscripts: self
! !
!Object methodsFor: 'undefined' stamp: ''!
◦ i | | <primitive: 63>
	"Old synonym for at:"
	^i subscripts: self
! !
!Object methodsFor: 'undefined' stamp: ''!
instVarAt: anInteger put: anObject | |<primitive: 38>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
instfield: n | |<primitive: 37>
	"Old synonym for instVarAt:"
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
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
!Object methodsFor: 'undefined' stamp: ''!
◦ i ← value | | <primitive: 64>
	"Old synonym for at:put:"
	^i subscripts: self ← value
! !
!Object methodsFor: 'undefined' stamp: ''!
instVarAt: anInteger | |<primitive: 37>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
at: i put: value | | <primitive: 64>
	^i subscripts: self ← value
! !
!Object methodsFor: 'undefined' stamp: ''!
error: s | |
	^user notify: s
! !
!Object methodsFor: 'undefined' stamp: ''!
subError | |
	self error: 'message not defined by subclass'
! !
!Object methodsFor: 'undefined' stamp: ''!
itself | |
	
! !
!Object methodsFor: 'undefined' stamp: ''!
fields | |
	self class "Return an Array of all my field names or many of my subscripts." isVariable
	  ifTrue: [^self class instvars concat: (self length ≤ 50
		  ifTrue: [1 to: self length]
		  ifFalse: [(1 to: 20) concat: (self length - 20 to: self length)])].
	^self class instvars
! !
!Object methodsFor: 'undefined' stamp: ''!
instfields | field |
	self class "Return an Array of all my field values or many of my elements." isVariable
	  ifTrue: [^(1 ~ self class instsize transform⦂ [:field | field] to⦂ [(self instfield: field)]) concat: self ◦ (self length ≤ 50
		  ifTrue: [1 ~ self length]
		  ifFalse: [1 ~ 20 concat: (self length - 20 to: self length)])].
	^1 ~ self class instsize transform⦂ [:field | field] to⦂ [(self instfield: field)]
! !
!Object methodsFor: 'undefined' stamp: ''!
inspect | |
	NoteTaker
	  ifTrue: [user schedule: (InspectWindow new of: self)]
	  ifFalse:
		[user leaveTop.
		user restartup: (InspectWindow new of: self)]
! !
!Object methodsFor: 'undefined' stamp: ''!
asOop | |<primitive: 39>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
canunderstand: selector | |
	^self class canunderstand: selector
! !
!Object methodsFor: 'undefined' stamp: ''!
instfield: n ← val | |<primitive: 38>
	"Old synonym for instVarAt:put:"
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
title | |
	^self class title + '.' + self asOop base8
! !
!Object methodsFor: 'undefined' stamp: ''!
hash | |<primitive: 39>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
is: x | |
	^self class ≡ x
! !
!Object methodsFor: 'undefined' stamp: ''!
Is: x | |
	 "Is the class x a superclass or class of self"
	self class ≡ x
	  ifTrue: [^true].
	^self class Isa: x
! !
!Object methodsFor: 'undefined' stamp: ''!
Isnt: x | |
	^(self Is: x) ≡ false
! !
!Object methodsFor: 'undefined' stamp: ''!
isNumber | |
	^false
! !
!Object methodsFor: 'undefined' stamp: ''!
isArray | |
	^false
! !
!Object methodsFor: 'undefined' stamp: ''!
isnt: x | |
	^self class ≡ x ≡ false
! !
!Object methodsFor: 'undefined' stamp: ''!
class | |<primitive: 24>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
species | |
	^self class
! !
!Object methodsFor: 'undefined' stamp: ''!
≡ x | |<primitive: 78>
	^self ≡ x "In case this is reached by perform:"
! !
!Object methodsFor: 'undefined' stamp: ''!
≠ x | |
	^self = x ≡ false
! !
!Object methodsFor: 'undefined' stamp: ''!
≥ x | |
	^self < x ≡ false
! !
!Object methodsFor: 'undefined' stamp: ''!
≤ x | |
	^self > x ≡ false
! !
!Object methodsFor: 'undefined' stamp: ''!
= x | |
	^self ≡ x
! !
!Object methodsFor: 'undefined' stamp: ''!
empty | |
	^self length = 0
! !
!Object methodsFor: 'undefined' stamp: ''!
sameAs: object | |
	^self ≡ object
! !
!Object methodsFor: 'undefined' stamp: ''!
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
!Object methodsFor: 'undefined' stamp: ''!
ⓢ code | |
	^LADCompiler new evaluate: code asStream in: false to: self notifying: self
! !
!Object methodsFor: 'undefined' stamp: ''!
ⓢ code | |
	^(NoteTaker
	  ifTrue: [LADCompiler]
	  ifFalse: [Generator]) new evaluate: code asStream in: false to: self notifying: self
! !
!Object methodsFor: 'undefined' stamp: ''!
interactive | |
	^false
! !
!Object methodsFor: 'undefined' stamp: ''!
notify: errorString at: position in: stream | |
	^self notify: errorString at: position in: stream for: self class
! !
!Object methodsFor: 'undefined' stamp: ''!
recopy | |
	 "recursively copy whole structure"
	(self is: Object)
	  ifTrue: [^self].
	^self class recopy: self
! !
!Object methodsFor: 'undefined' stamp: ''!
, x | v |
	v ← Vector new: 2.
	v ◦ 1 ← self.
	v ◦ 2 ← x.
	^v
! !
!Object methodsFor: 'undefined' stamp: ''!
asParagraph | |
	^self asString asParagraph
! !
!Object methodsFor: 'undefined' stamp: ''!
inVector | vec |
	vec ← Vector new: 1 "Return me as the sole element of a new Vector.".
	vec ◦ 1 ← self.
	^vec
! !
!Object methodsFor: 'undefined' stamp: ''!
asStream | |
	^self asVector asStream
! !
!Object methodsFor: 'undefined' stamp: ''!
copy | |
	 "create new copy of self"
	(self is: Object)
	  ifTrue: [^self].
	^self class copy: self
! !
!Object methodsFor: 'undefined' stamp: ''!
asVector | v |
	self ≡ nil
	  ifTrue: [^Vector new: 0].
	v ← Vector new: 1.
	v ◦ 1 ← self.
	^v
! !
!Object methodsFor: 'undefined' stamp: ''!
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
!Object methodsFor: 'undefined' stamp: ''!
asString | strm |
	strm ← (String new: 16) asStream.
	self printOn: strm.
	^strm contents
! !
!Object methodsFor: 'undefined' stamp: ''!
printon: strm indent: level | |
	self printOn: strm
! !
!Object methodsFor: 'undefined' stamp: ''!
filout | file |
	^user displayoffwhile⦂ [
		(file ← dp0 file: self title asFileName.
		self fullprinton: file.
		file close)]
! !
!Object methodsFor: 'undefined' stamp: ''!
asFullString | strm |
	strm ← (String new: 20) asStream.
	self fullprinton: strm.
	^strm contents
! !
!Object methodsFor: 'undefined' stamp: ''!
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
!Object methodsFor: 'undefined' stamp: ''!
fullprint | strm |
	strm ← Stream default.
	self fullprinton: strm.
	user show: strm contents
! !
!Object methodsFor: 'undefined' stamp: ''!
print | |
	user show: self asString
! !
!Object methodsFor: 'undefined' stamp: ''!
processFor: selector withArgs: args | newProcess |
	newProcess ← self
		processForContext⦂ [self perform: selector withArgs: args.  newProcess terminate]
		priority: Processor activePriority.
	^newProcess
! !
!Object methodsFor: 'undefined' stamp: ''!
processForContext⦂ context priority: priority | |
	^Process new forContext: context priority: priority
! !
!Object methodsFor: 'undefined' stamp: ''!
fork: selector withArgs: args | |
	Processor resume: (self processFor: selector withArgs: args)
! !
!Object methodsFor: 'undefined' stamp: ''!
fork: selector | |
	Processor resume: (self processFor: selector)
! !
!Object methodsFor: 'undefined' stamp: ''!
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
!Object methodsFor: 'undefined' stamp: ''!
perform: selector with: arg1 with: arg2 with: arg3 | |<primitive: 36>
	 "selector had better take 3 args"
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
perform: selector with: arg1 with: arg2 | |<primitive: 36>
	 "selector had better take 2 args"
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
perform: selector with: arg1 | |<primitive: 36>
	 "selector had better take 1 arg"
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
perform: selector | |<primitive: 36>
	 "Send self the message, selector; it had better be unary"
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
doesNotUnderstand: message | selector args |
	selector ← message selector.
	args ← message args.
	user notify: 'Message not understood: ' + selector.
	^self perform: selector withArgs: args
! !
!Object methodsFor: 'undefined' stamp: ''!
processFor: selector | newProcess |
	newProcess ← self
		processForContext⦂ [self perform: selector.  newProcess terminate]
		priority: Processor activePriority.
	^newProcess
! !
!Object methodsFor: 'undefined' stamp: ''!
execute: method with: arg | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
execute: method with: arg1 with: arg2 with: arg3 | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
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
!Object methodsFor: 'undefined' stamp: ''!
execute: method with: arg1 with: arg2 | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
execute: method | |<primitive: 78>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
refct | |<primitive: 34>
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
swap⦂ variable | x |
	 "assign me to variable and return its old value"
	x ← variable value.
	variable value← self.
	^x
! !
!Object methodsFor: 'undefined' stamp: ''!
startup | |
	 "loopless scheduling"
	self firsttime
	  ifTrue:
		[[self eachtime] whileTrueDo: [].
		^self lasttime].
	^false
! !
!Object methodsFor: 'undefined' stamp: ''!
nextInstance | |<primitive: 82>
	 "return next in enumeration of my class, false if done"
	user croak
! !
!Object methodsFor: 'undefined' stamp: ''!
messageNotUnderstood: op withArgs: args from: sender | |
	thisContext sender← sender.
	user notify: 'Message not understood: ' + op
! !
!Object methodsFor: 'undefined' stamp: ''!
error | sender op n args i |
	 "after compiling execute: nil installError.  "
	sender ← thisContext sender.
	op ← sender thisop.
	n ← op numArgs.
	args ← Vector new: n.
	(n to: 1 by: ¬1) do: [:i | args ◦ i ← sender pop].
	^self messageNotUnderstood: op withArgs: args from: sender 
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
syntaxChoice | |
	 "kludge for new choosing syntax"
	class ≡ nil
	  ifFalse:
		[class usesNewSyntax
		  ifTrue: [^class new]]
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
execute: parag | |
	^class ⓢ parag
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
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
!OrganizationPane methodsFor: 'undefined' stamp: ''!
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
!OrganizationPane methodsFor: 'undefined' stamp: ''!
code: selector | |
	^class code: selector
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
forget: selector | cat |
	class derstands: selector.
	cat ← list ◦ selection.
	self revise: (self listFor: class) with: cat.
	selection > 0
	  ifTrue: [selectorPane revise: (class organization category: cat) with: selector]
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
dirty | |
	^selectorPane dirty
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
spawn: selector with: parag formerly: oldparag | |
	selectorPane compselection.
	selectorPane select: 0.
	class edit: selector para: parag formerly: oldparag
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
class: t1 | |
	class ← t1.
	self of: (self listFor: class)
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
classInit | |
	editmenu ← Menu new string: 'filout
print'
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
from: t1 to: t2 | |
	classPane ← t1.
	selectorPane ← t2
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
listFor: t1 | |
	class ← t1.
	^class ≡ nil
	  ifTrue: [Vector new: 0]
	  ifFalse: [#(ClassDefinition ClassOrganization ) concat: class organization categories]
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
selected | |
	selectorPane of: (selection ≤ 2
	  ifTrue: [Vector new: 0]
	  ifFalse: [class organization category: list ◦ selection])
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
deselected | |
	selectorPane of: (Vector new: 0) "I just lost my selection.  Tell selectorPane to display nothing."
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
close | |
	classPane ← nil.
	super close
! !
!OrganizationPane methodsFor: 'undefined' stamp: ''!
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
!PQueue methodsFor: 'undefined' stamp: ''!
length | l |
	Top critical⦂ [(l ← position - readposition)].
	^l
! !
!PQueue methodsFor: 'undefined' stamp: ''!
dequeue: num | n |
	Top critical⦂ [
		(position - readposition < num
		  ifTrue: [n ← false]
		  ifFalse:
			[n ← (array ◦ (readposition + 1 to: readposition + num)) copy.
			readposition ← readposition + num])].
	^n
! !
!PQueue methodsFor: 'undefined' stamp: ''!
pastend← x | n i |
	 "simple arg"
	Top critical⦂ [
		(position ≥ limit
		  ifTrue:
			[readposition = 0
			  ifTrue: [super pastend← x]
			  ifFalse:
				[n ← position - readposition.
				(1 to: n) do: [:i | array ◦ i ← array ◦ (readposition + i)].
				readposition ← 0.
				position ← n.
				self next← x]]
		  ifFalse: [array ◦ (position ← position + 1) ← x])].
	^x
! !
!PQueue methodsFor: 'undefined' stamp: ''!
myend | |
	^true
! !
!PQueue methodsFor: 'undefined' stamp: ''!
skip: x | |
	Top critical⦂ [(readposition ← readposition + x)]
! !
!PQueue methodsFor: 'undefined' stamp: ''!
peek | n |
	Top critical⦂ [
		(readposition ≥ position
		  ifTrue:
			[readposition ← position ← 0.
			n ← false]
		  ifFalse: [n ← array ◦ (readposition + 1)])].
	^n
! !
!PQueue methodsFor: 'undefined' stamp: ''!
next | n |<primitive: 75>
	Top critical⦂ [
		(readposition ≥ position
		  ifTrue:
			[readposition ← position ← 0.
			n ← false]
		  ifFalse: [n ← array ◦ (readposition ← readposition + 1)])].
	^n
! !
!PQueue methodsFor: 'undefined' stamp: ''!
push: x | |
	 "treat as LIFO queue"
	Top critical⦂ [
		(readposition > 0
		  ifTrue:
			[array ◦ readposition ← x.
			readposition ← readposition - 1]
		  ifFalse: [ "readpositon > 0, just jam it in"
			self insert: x])] "otherwise insert on front"
! !
!PQueue methodsFor: 'undefined' stamp: ''!
of: t1 | |
	array ← t1.
	Top critical⦂ [
		(position ← 0.
		readposition ← 0.
		limit ← array length)]
! !
!PQueue methodsFor: 'undefined' stamp: ''!
end | n |
	Top critical⦂ [(n ← readposition ≥ position)].
	^n
! !
!PQueue methodsFor: 'undefined' stamp: ''!
empty | l |<primitive: 75>
	Top critical⦂ [(l ← readposition ≥ position)].
	^l
! !
!PQueue methodsFor: 'undefined' stamp: ''!
reset | |
	Top critical⦂ [(readposition ← position ← 0)]
! !
!PQueue methodsFor: 'undefined' stamp: ''!
of: t1 from: t2 to: t3 | |
	array ← t1.
	position ← t2.
	limit ← t3.
	user notify: 'of:from:to: is not appropriate for PQueues'
! !
!PQueue methodsFor: 'undefined' stamp: ''!
contents | n |
	Top critical⦂ [(n ← (array ◦ (readposition + 1 to: position)) copy)].
	^n
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
dataString← str | i |
	i ← str length.
	i > 532
	  ifTrue: [user notify: 'Data string too big for single PUP']
	  ifFalse:
		[pupString copy: 25 to: 24 + i with: str from: 1 to: i.
		self dataLength← i.
		^str]
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
dataString | |
	^pupString copy: 25 to: 24 + self dataLength
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
dataLength | |
	^(pupString word: 3) - 22 "self pupLength"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
dataWord: i ← v | |
	^pupString word: (i + 12) ← v
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
dataWord: i | |
	^pupString word: i + 12
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
dataLength← len | |
	^pupString word: 3 ← len + 22 "self pupLength"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
unlock | |
	locked
	  ifTrue:
		[locked ← false.
		"pupString unlock"]
	  ifFalse: [user notify: 'trying to unlock a buffer not locked']
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
lock | |
	locked
	  ifTrue: [E notify: 'trying to lock a buffer already locked']
	  ifFalse:
		[locked ← true.
		^pupString "lock"]
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
lockwith: string | |
	locked
	  ifTrue: [E notify: 'trying to lock a buffer already locked']
	  ifFalse:
		[locked ← string.
		^pupString lock]
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
locked | |
	^locked
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupString | |
	^pupString
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupString← t1 | |
	pupString ← t1.
	^pupString
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
header | |
	^pupString ◦ (1 to: 24)
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
word: i ← v | |
	^pupString word: i ← v
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
word: i | |
	^pupString word: i
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
◦ i | |
	^pupString ◦ i
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
◦ i ← v | |
	^pupString ◦ i ← v
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
ethType | |
	^pupString unsignedWord: 2
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
imEthSrcHost← iESH | |
	pupString ◦ 2 ← iESH
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
imEthDestHost← iEDH | |
	pupString ◦ 1 ← iEDH
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
imEthSrcHost | |
	^pupString ◦ 2
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
imEthDestHost | |
	^pupString ◦ 1
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
ethType← eT | |
	pupString word: 2 ← eT
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
init | |
	pupString ← String new: 558.
	locked ← false
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
doChecksum | i cs |<primitive: 88>
	cs ← 0.
	(3 to: self length + 1 / 2 + 2) do:
		[:i |  "does not work"
		cs ← cs + (pupString word: i) "for packets with carries".
		cs < 0
		  ifTrue: [cs ← (cs lshift: 1) + 1]
		  ifFalse: [cs ← cs lshift: 1]].
	cs = ¬1
	  ifTrue: [cs ← 0].
	^cs
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
checksum← cs | |
	pupString word: (self pupLength + 1 / 2 + 2) ← cs
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
checksum | |
	^pupString word: self pupLength + 1 / 2 + 2
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
checksumOK | |
	 "Boolean, returns true or false"
	^self checksum "just look at the current packet" = self doChecksum
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceSocNum | |
	^pupString doubleWordAt: 21  "words 11, 12"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceSoc0 | |
	^pupString unsignedWord: 11
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceSoc1 | |
	^pupString unsignedWord: 12
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupLength | |
	^pupString unsignedWord: 3
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupID | |
	^pupString doubleWordAt: 9  "words 5, 6"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destSocNum | |
	^pupString doubleWordAt: 15  "words 8, 9"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destSoc0 | |
	^pupString unsignedWord: 8
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destSoc1 | |
	^pupString unsignedWord: 9
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupID1 | |
	^pupString unsignedWord: 6
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceHost← sH | |
	pupString ◦ 20 ← sH
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceHost | |
	^pupString ◦ 20
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceNet← sN | |
	pupString ◦ 19 ← sN
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceNet | |
	^pupString ◦ 19
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destHost← dH | |
	pupString ◦ 14 ← dH
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
addressBlock← addBlock | |
	 "for quickly setting the 6 fields"
	pupString copy: 13 to: 24 with: addBlock from: 1 to: 12 "pupString◦(13 to: 24) ← addBlock"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupLength← pL | |
	^pupString word: 3 ← pL
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destSoc0← i | |
	^pupString word: 8 ← i
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupID1← pID | |
	^pupString word: 6 ← pID
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupID0← pID | |
	^pupString word: 5 ← pID
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupID0 | |
	^pupString unsignedWord: 5
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
transportControl | |
	^pupString ◦ 7
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
swapPorts | i |
	(13 to: 18) do: [:i | pupString swap: i with: i + 6]
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceSoc1← i | |
	^pupString word: 12 ← i
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceSoc0← i | |
	^pupString word: 11 ← i
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
sourceSocNum← sSN | |
	pupString doubleWordAt: 21 ← sSN  "words 11, 12"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destSoc1← i | |
	^pupString word: 9 ← i
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
addressBlock | |
	^pupString ◦ (13 to: 24)
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
totLengthWords | |
	^self pupLength + 5 / 2
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destNet | |
	^pupString ◦ 13
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupType | |
	^pupString ◦ 8
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
transportControl← tC | |
	pupString ◦ 7 ← tC
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destSocNum← dSN | |
	pupString doubleWordAt: 15 ← dSN  "words 8, 9"
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destHost | |
	^pupString ◦ 14
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
destNet← dN | |
	pupString ◦ 13 ← dN
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupType← pT | |
	pupString ◦ 8 ← pT
! !
!Pacbuf methodsFor: 'undefined' stamp: ''!
pupID← pID | |
	pupString doubleWordAt: 9 ← pID  "words 5, 6"
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
title: t1 with: t2 at: t3 | pane |
	 "The instance variable templates is a set of Rectangles for the frames of the panes normalized such that the whole PanedWindow is a frame of 0⌾0 rect: 36⌾36."
	title ← t1.
	panes ← t2.
	templates ← t3.
	self reset.
	panes do: [:pane | pane init]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
vanish | |
	self close.
	self erase.
	user unschedule: self
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
titlerect | |
	^frame origin - (2 ⌾ (DefaultTextStyle lineheight + 4)) rect: frame corner x ⌾ frame origin y + (2 ⌾ 0)
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
hardcopy | p |
	user displayoffwhile⦂ [
		(p ← dp0 pressfile: (self title + '.press') asFileName.
		self hardcopy: p.
		p close.
		p toPrinter)]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
pickedpane | pane |
	panes do:
		[:pane | pane picked
		  ifTrue: [^pane]].
	frame flash.
	^false
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
takeCursor | |
	(panes ◦ 1) takeCursor
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
title | |
	^title
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
close | pane |
	panes do: [:pane | pane close]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
hardcopyTitle: pf | |
	self showtitle "refresh title (since it's a class var)" "draw title rectangle".
	titleframe window hardcopy: pf "print title text (make frame larger)".
	titleframe para presson: pf in: (pf transrect: (titleframe frame origin rect: titleframe frame corner + (999 ⌾ 2))) style: titleframe style
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
yellowbug | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane yellowbug]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
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
!PanedWindow methodsFor: 'undefined' stamp: ''!
enter | pane |
	super show.
	panes do: [:pane | pane windowenter]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
erase | |
	self titlerect clear.
	super erase
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
fixframe: f | |
	^Rectangle new origin: f origin extent: (f extent max: 160 ⌾ 80)
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
hardcopy: pf | pane |
	self hardcopyTitle: pf "print frame rectangle".
	frame hardcopy: pf "print all panes".
	panes do: [:pane | pane hardcopy: pf "print cursor if it's inside"].
	(frame has: user mp)
	  ifTrue: [user currentCursor hardcopy: pf]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
leave | pane |
	panes do: [:pane | pane windowleave]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
show | pane |
	super show.
	panes do: [:pane | pane outline]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
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
!PanedWindow methodsFor: 'undefined' stamp: ''!
keyset | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane keyset]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
redbug | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane redbug]
! !
!PanedWindow methodsFor: 'undefined' stamp: ''!
kbd | pane |
	(pane ← self pickedpane)
	  ifTrue: [^pane kbd]
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
toBravo | s |
	s ← (String new: text length * 2) asStream.
	s append: text.
	s next← 26.
	self bravoRuns: s.
	^s contents asParagraph
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
readFrom: file | |
	text ← file nextString.
	runs ← file nextString.
	alignment ← file next.
	runs empty
	  ifTrue: [runs ← nil]
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
storeOn: file | |
	file nextString← text.
	runs ≡ nil
	  ifTrue: [file next← 0]
	  ifFalse: [file nextString← runs].
	file next← alignment
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
text: t1 | |
	text ← t1.
	alignment ← 0
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
text: t1 runs: t2 alignment: t3 | |
	text ← t1.
	runs ← t2.
	alignment ← t3
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
copy | |
	^self class new text: text runs: runs alignment: alignment
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
text: t1 alignment: t2 | |
	text ← t1.
	alignment ← t2
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
allFont: n | |
	(n is: String)
	  ifTrue: [n ← (self textStyle fontnames find: n) - 1].
	self maskrunsunder: 240 to: n * 16
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
maskrunsunder: m to: val | |
	self maskrun: 1 to: text length under: m to: val
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
runfind: index | run t |
	 "index into run"
	run ← 1.
	[(t ← index - (runs ◦ run)) > 0] whileTrueDo:
		[index ← t.
		run ← run + 2].
	^{run , index}
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
maskrun: i to: j under: m to: val | r k |
	 "Alter my runs so that the bits selected by m become val." "Maybe merge this with mergestyle"
	r ← self run: i to: j.
	(2 to: r length by: 2) do: [:k | r ◦ k ← (r ◦ k land: 255 - m) + val].
	runs ← self runcat: (self run: 1 to: i - 1) and: [r] and: [(self run: j + 1 to: text length)]
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
runs | |
	 "return runs or default if none"
	runs ≡ nil
	  ifTrue: [^self makerun: text length val: 0].
	^runs
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
allItalic | |
	self maskrunsunder: 2 to: 2
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
allBold | |
	self maskrunsunder: 1 to: 1
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
runcat: x to: y | |
	^self runcat: x and: [y] and: ['']
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
subst: x for: y | |
	 "runs are not supported yet here"
	^text subst: x for: y
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
textStyle | |
	^DefaultTextStyle
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
asStream | |
	^text asStream
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
findString: str startingAt: start | |
	^text findString: str startingAt: start
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
length | |
	^text length
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
asVector | |
	^text asVector
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
text | |
	^text
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
copy: a to: b | |
	 "Return a copied subrange of this paragraph"
	^self class new text: (text copy: a to: b) runs: (self run: a to: b) alignment: alignment
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
asParagraph | |
	^self
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
◦ x | |
	^text ◦ x
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
presson: press in: r | |
	^self presson: press in: r style: self textStyle
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
hidePress: press complete: c | |
	press skipcode: self pressCode "not called by Form-Path-Image, but probably by Class printout" data: (self hideData: c)
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
pressCode | |
	^99
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
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
!Paragraph methodsFor: 'undefined' stamp: ''!
center | |
	alignment ← 2
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
alignment← t1 | |
	alignment ← t1
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
alignment | |
	^alignment
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
flushleft | |
	alignment ← 0
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
justify | |
	alignment ← 1
! !
!Paragraph methodsFor: 'undefined' stamp: ''!
flushright | |
	alignment ← 4
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
frame← t1 | |
	frame ← t1
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
leading← t1 | |
	leading ← t1
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
defaultleading | |
	^defaultleading
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
defaultframe | |
	^defaultframe
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
style← t1 | |
	style ← t1
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
frame | |
	^frame
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
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
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
printForget: selector class: class | t |
	 "Print a line that causes a message to be forgotten"
	user cr.
	user show: '~' + class title + ' ' + selector.
	t ← selector last=('←'◦1) ifTrue: ['''' + selector + ''' unique'] ifFalse: ['#' + selector].
	self print: (class title + ' derstands: ' + t + '.
') asParagraph
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
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
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
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
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
close | |
	strm close
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
init | |
	self frame← self defaultframe.
	self leading← defaultleading.
	self style← DefaultTextStyle
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
of: t1 | |
	strm ← t1
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
classInit | inch |
	inch ← 2540 "1 inch in micas".
	defaultframe ← (0.75 * inch) asInteger ⌾ (1 * inch) rect: (7.75 * inch) asInteger ⌾ (10 * inch).
	defaultleading ← 0
! !
!ParagraphPrinter methodsFor: 'undefined' stamp: ''!
print: para | |
	 "A dummy, subclasses will override"
	strm append: para text
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
position | |
	^textstrm position
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
width | |
	^width
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
init | |
	ascent ← descent ← width ← spaces ← 0.
	tabpos reset
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
of: t1 to: t2 style: t3 | |
	para ← t1.
	press ← t2.
	style ← t3.
	textstrm ← '' asStream.
	runstrm ← para runs asStream.
	tabpos ← (Vector new: 10) asStream
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
in: t1 | |
	rect ← t1
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
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
						  ifTrue: [w ◦ 2 ← w ◦ 2 + ifont tab | ifont tab]]].
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
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
newrun | len pos |
	(len ← runstrm next)
	  ifTrue:
		[pos ← textstrm position.
		textstrm of: para text from: pos + 1 to: pos + len.
		font ← press codefont: runstrm next style: style]
	  ifFalse: [^false]
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
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
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
backup | |
	textstrm skip: ¬1
! !
!ParagraphScanner methodsFor: 'undefined' stamp: ''!
tab | |
	spaces ← 0.
	tabpos next← textstrm position.
	tabpos next← width ← width + font tab | font tab
! !
!ParseStack methodsFor: 'undefined' stamp: ''!
pop: n | |
	(position ← position - n) < 0
	  ifTrue: [user notify: 'Parse stack underflow']
! !
!ParseStack methodsFor: 'undefined' stamp: ''!
push: n | |
	(position ← position + n) > length
	  ifTrue: [length ← position]
! !
!ParseStack methodsFor: 'undefined' stamp: ''!
init | |
	length ← position ← 0
! !
!ParseStack methodsFor: 'undefined' stamp: ''!
length | |
	^length
! !
!ParseStack methodsFor: 'undefined' stamp: ''!
position | |
	^position
! !
!Path methodsFor: 'undefined' stamp: ''!
pointnearestto: p | distance i nearest d |
	distance ← p dist: self ◦ 1 "return the index of the point  in the path nearest (manhatten norm) to p.".
	nearest ← 1.
	(1 to: position) do:
		[:i | d ← p dist: self ◦ i.
		d < distance
		  ifTrue:
			[nearest ← i.
			distance ← d]].
	^nearest
! !
!Path methodsFor: 'undefined' stamp: ''!
comment | |
	 "see Set for these ... add:, append:, and ◦← are the main ones"
! !
!Path methodsFor: 'undefined' stamp: ''!
init | |
	self default "must be executed for each new instance."
! !
!Path methodsFor: 'undefined' stamp: ''!
size | |
	^self rectangle "return the extent of the bounding rectangle that includes all the points in the path." extent
! !
!Path methodsFor: 'undefined' stamp: ''!
origin | |
	^self rectangle "return the origin of the bounding rectangle that includes all the points in the path." origin
! !
!Path methodsFor: 'undefined' stamp: ''!
height | |
	^self size "return the height of the bounding rectangle that includes all the points in the path." y
! !
!Path methodsFor: 'undefined' stamp: ''!
corner | |
	^self rectangle "return the corner of the bounding rectangle that includes all the points in the path." corner
! !
!Path methodsFor: 'undefined' stamp: ''!
extent | |
	^self rectangle "return the extent of the bounding rectangle that includes all the points in the path." extent
! !
!Path methodsFor: 'undefined' stamp: ''!
width | |
	^self size "return the width of the bounding rectangle that includes all the points in the path." x
! !
!Path methodsFor: 'undefined' stamp: ''!
rectangle | r i |
	r ← Rectangle new "return the bounding rectangle that includes all the points in the path." origin: self ◦ 1 extent: 1 ⌾ 1.
	(1 to: self length) do: [:i | r ← r include: self ◦ i].
	^r
! !
!Path methodsFor: 'undefined' stamp: ''!
insert: pt atindex: index | r |
	index > position "insert pt at index in the path"
	  ifTrue: [self next← pt]
	  ifFalse:
		[r ← (position = limit
				  ifTrue: [self grow]
				  ifFalse: [self growby: 0]).
		position ← 0.
		self append: (r ◦ 1 to: index - 1).
		self next← pt.
		self append: r ◦ (index to: r length)]
! !
!Path methodsFor: 'undefined' stamp: ''!
deleteindex: i | r |
	r ← array ◦ (i + 1 to: position).
	position ← i - 1.
	self append: r.
	array ◦ (position + 1) ← nil
! !
!Path methodsFor: 'undefined' stamp: ''!
addarcfrom: p1 via: p2 to: p3 | pa pb i k s |
	s ← Path new "Kaehler method for Flegal curve" init.
	s add: p1.
	pa ← p2 - p1.
	pb ← p3 - p2.
	k ← 5 max: pa x abs + pa y abs + pb x abs + pb y abs / 20.
	(1 to: k) do: [:i |  "k is a guess how many segments are appropriate"
		s add: pa * i / k + p1 * (k - i) + (pb * (i - 1) / k + p2 * (i - 1)) / (k - 1)].
	s add: p3.
	(1 to: s length - 1) do: [:i | self addlinefrom: s ◦ i to: s ◦ (i + 1)]
! !
!Path methodsFor: 'undefined' stamp: ''!
addlinefrom: p1 to: p2 | x1 y1 dx dy yinc x0 y0 cdl i |
	dx ← p2 x "for now just add points to the space at alto resolution between p1 and p2
	inclusive" - p1 x.
	dy ← p2 y - p1 y.
	dx < 0
	  ifTrue:
		[dx ← 0 - dx.
		dy ← 0 - dy.
		x0 ← p2 x.
		y0 ← p2 y]
	  ifFalse:
		[x0 ← p1 x.
		y0 ← p1 y].
	dy ≥ 0
	  ifTrue: [yinc ← 1]
	  ifFalse:
		[yinc ← 0 - 1.
		dy ← 0 - dy].
	dx ≥ dy
	  ifTrue:
		[cdl ← dx / 2.
		(0 to: dx) do:
			[:i | self add: x0 ⌾ y0.
			cdl ← cdl + dy.
			x0 ← x0 + 1.
			cdl > dx
			  ifTrue:
				[cdl ← cdl - dx.
				y0 ← y0 + yinc]]]
	  ifFalse:
		[ "y is fastest mover"
		cdl ← dy / 2.
		(0 to: dy) do:
			[:i | self add: x0 ⌾ y0.
			cdl ← cdl + dx.
			y0 ← y0 + yinc.
			cdl > dy
			  ifTrue:
				[cdl ← cdl - dy.
				x0 ← x0 + 1]]]
! !
!Path methodsFor: 'undefined' stamp: ''!
printOn: strm | t |
	strm append: 'a Path: '.
	(array is: String)
	  ifTrue: [strm space append: self]
	  ifFalse: [self do: [:t | strm space print: t]]
! !
!Path methodsFor: 'undefined' stamp: ''!
copy | t |
	t ← Path new "returns a new instance of Path that is a copy " init.
	t append: (array ◦ (1 to: position)) copy.
	^t
! !
!Path methodsFor: 'undefined' stamp: ''!
pressCode | |
	^6
! !
!Path methodsFor: 'undefined' stamp: ''!
scale: factor | i |
	(1 to: self length "scale every point in the path by factor") do: [:i | self ◦ i ← self ◦ i * factor]
! !
!Path methodsFor: 'undefined' stamp: ''!
translate: delta | i |
	(1 to: self length "add delta to every point in the path") do: [:i | self ◦ i ← self ◦ i + delta]
! !
!Path methodsFor: 'undefined' stamp: ''!
normalize | delta i |
	delta ← self origin "subtract the origin of the path from every point in the path".
	(1 to: self length) do: [:i | self ◦ i ← self ◦ i - delta]
! !
!Path methodsFor: 'undefined' stamp: ''!
+ delta | i |
	^self copy "add delta to every point in the path" translate: delta
! !
!Point methodsFor: 'undefined' stamp: ''!
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
!Point methodsFor: 'undefined' stamp: ''!
hash | |
	^(x lshift: 2) lxor: y
! !
!Point methodsFor: 'undefined' stamp: ''!
y | |
	^y
! !
!Point methodsFor: 'undefined' stamp: ''!
x | |
	^x
! !
!Point methodsFor: 'undefined' stamp: ''!
x← t1 | |
	x ← t1
! !
!Point methodsFor: 'undefined' stamp: ''!
y← t1 | |
	y ← t1
! !
!Point methodsFor: 'undefined' stamp: ''!
normal | n |
	 "unit vector rotated 90 deg clockwise"
	n ← y asFloat neg ⌾ x asFloat.
	^n / n length
! !
!Point methodsFor: 'undefined' stamp: ''!
min: t | |
	^Point new x: (x min: t x) y: (y min: t y)
! !
!Point methodsFor: 'undefined' stamp: ''!
translate: delta | |
	x ← x + delta x "increment self by delta".
	y ← y + delta y
! !
!Point methodsFor: 'undefined' stamp: ''!
max: t | |
	^Point new x: (x max: t x) y: (y max: t y)
! !
!Point methodsFor: 'undefined' stamp: ''!
abs | |
	 "absolute value of a point"
	^Point new x: x abs y: y abs
! !
!Point methodsFor: 'undefined' stamp: ''!
normalize | |
	self x← 0 "set selt to zero".
	self y← 0
! !
!Point methodsFor: 'undefined' stamp: ''!
length | |
	^(x asFloat * x asFloat + (y asFloat * y asFloat)) sqrt
! !
!Point methodsFor: 'undefined' stamp: ''!
dist: pt | t |
	 "distance (Manhattan norm) between pt and self"
	t ← (pt - self) abs.
	^t x + t y
! !
!Point methodsFor: 'undefined' stamp: ''!
| grid | |
	^Point new x: x | grid y: y | grid
! !
!Point methodsFor: 'undefined' stamp: ''!
/ scale | |
	^Point new "Return a Point that is the quotient of me and scale (which is a Point or Number)" x: x / scale asPtX y: y / scale asPtY
! !
!Point methodsFor: 'undefined' stamp: ''!
* scale | |
	^Point new "Return a Point that is the product of me and scale (which is a Point or Number)" x: x * scale asPtX y: y * scale asPtY
! !
!Point methodsFor: 'undefined' stamp: ''!
≥ pt | |
	^x ≥ pt x and: [y ≥ pt y]
! !
!Point methodsFor: 'undefined' stamp: ''!
> pt | |
	^x > pt x and: [y > pt y]
! !
!Point methodsFor: 'undefined' stamp: ''!
≤ pt | |
	^x ≤ pt x and: [y ≤ pt y]
! !
!Point methodsFor: 'undefined' stamp: ''!
= pt | |
	^x = pt x and: [y = pt y]
! !
!Point methodsFor: 'undefined' stamp: ''!
- delta | |
	^Point new "Return a Point that is the difference of me and delta (which is a Point or Number)" x: x - delta asPtX y: y - delta asPtY
! !
!Point methodsFor: 'undefined' stamp: ''!
< pt | |
	^x < pt x and: [y < pt y]
! !
!Point methodsFor: 'undefined' stamp: ''!
+ delta | |
	^Point new "Return a Point that is the sum of me and delta (which is a Point or Number)" x: x + delta asPtX y: y + delta asPtY
! !
!Point methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm print: x.
	strm append: '⌾'.
	strm print: y
! !
!Point methodsFor: 'undefined' stamp: ''!
asPoint | |
	 "Return self."
! !
!Point methodsFor: 'undefined' stamp: ''!
extent: p | |
	 "infix creation of rectangles"
	^Rectangle new origin: self extent: p
! !
!Point methodsFor: 'undefined' stamp: ''!
height | |
	^1
! !
!Point methodsFor: 'undefined' stamp: ''!
corner | |
	^self + (1 ⌾ 1)
! !
!Point methodsFor: 'undefined' stamp: ''!
origin | |
	^self
! !
!Point methodsFor: 'undefined' stamp: ''!
extent | |
	^1 ⌾ 1
! !
!Point methodsFor: 'undefined' stamp: ''!
asRectOrigin | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Point methodsFor: 'undefined' stamp: ''!
asRectCorner | |
	 "pretend to be a Rectangle for Rectangle +-*/"
! !
!Point methodsFor: 'undefined' stamp: ''!
asRectangle | |
	^self rect: self "Return a Rectangle with me as both origin and corner."
! !
!Point methodsFor: 'undefined' stamp: ''!
width | |
	^1
! !
!Point methodsFor: 'undefined' stamp: ''!
asPtY | |
	^y
! !
!Point methodsFor: 'undefined' stamp: ''!
rect: p | |
	 "infix creation of rectangles"
	^Rectangle new origin: self corner: p
! !
!Point methodsFor: 'undefined' stamp: ''!
asPtX | |
	^x
! !
!Point methodsFor: 'undefined' stamp: ''!
x: t1 y: t2 | |
	x ← t1.
	y ← t2
! !
!Point methodsFor: 'undefined' stamp: ''!
copy | |
	^x ⌾ y
! !
!Point methodsFor: 'undefined' stamp: ''!
fromPress: press value: s | |
	x ← s nextword.
	y ← s nextword
! !
!Point methodsFor: 'undefined' stamp: ''!
hidePress: press complete: c | |
	press skipcode: self pressCode data: (self hideData: c)
! !
!Point methodsFor: 'undefined' stamp: ''!
pressCode | |
	^7
! !
!Point methodsFor: 'undefined' stamp: ''!
hideData: complete | s t3 |
	(t3 ← Stream new) of: (s ← String new: 4).
	t3 nextPoint← self.
	^s
! !
!PressFile methodsFor: 'undefined' stamp: ''!
name | |
	^DL name
! !
!PressFile methodsFor: 'undefined' stamp: ''!
defaultPrinterName | |
	^currentProfile ≡ nil
	  ifTrue: [PrinterName]
	  ifFalse: [currentProfile printerName]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
scale | |
	^scale
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setwindowwidth: w height: h skipdots: sd skiplines: sl | |
	DL nextword← 1.
	DL nextword← sd.
	DL nextword← w.
	DL nextword← sl.
	DL nextword← h
! !
!PressFile methodsFor: 'undefined' stamp: ''!
dotsfromAIS: file | f |
	f ← file length inString + file + (file length even
			  ifTrue: [' ']
			  ifFalse: ['']) "BCPLize".
	DL nextword← 4.
	DL nextword← 4.
	DL append: f.
	FL append: f
! !
!PressFile methodsFor: 'undefined' stamp: ''!
AIS: file width: w height: h croprect: r at: pt scale: s | |
	self setp: (self transpt: pt).
	self dots⦂ [
		(self setcoding: 8 dots: w lines: h "byte samples".
		self setmode: 3 "to right and to bottom of page".
		self setsizewidth: (s * r width * scale) asInteger height: (s * r height * scale) asInteger.
		self setwindowwidth: r width height: r height skipdots: r minX skiplines: r minY.
		self dotsfromAIS: file)] "
(dp0 pressfile: 'pix.press') somefont; AIS: 'girl.ais' width: 512 height: 512 croprect: (50⌾50 rect: 500⌾500) at: 36⌾80 scale: 0.65; close.
"
! !
!PressFile methodsFor: 'undefined' stamp: ''!
dotsfollow | |
	DL nextword← 3
! !
!PressFile methodsFor: 'undefined' stamp: ''!
bitmap: rect bits: bits | w w16 h |
	w ← rect width "some pecularities of spruce:
	scale must be 32, and multiples of 16 for width (maybe extra stuff prints)".
	w16 ← w + 15 | 16 "width to next word boundary".
	h ← rect height "origin should be set earlier".
	self dots⦂ [
		(self setcoding: 0 dots: w16 lines: h "bitmap".
		self setmode: 3 "to right and to bottom of page".
		self setsizewidth: scale * w16 height: scale * h.
		self setwindowwidth: (ColorPrint
		  ifTrue: [w]
		  ifFalse: [w16]) height: h.
		self dotsfollow.
		bits
		  ifTrue: [DL append: bits "bits supplied"]
		  ifFalse: [ "else from screen"
			rect bitsOntoStream: DL])]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setwindowwidth: w height: h | |
	self setwindowwidth: w height: h skipdots: 0 skiplines: 0
! !
!PressFile methodsFor: 'undefined' stamp: ''!
dots⦂ exp | dlpos |
	dlpos ← self padword.
	exp eval.
	self showdots: DL wordposition - dlpos
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setsizewidth: w height: h | |
	DL nextword← 2.
	DL nextword← w.
	DL nextword← h
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setcoding: c dots: d lines: l | |
	DL next← 1.
	DL next← c.
	DL nextword← d.
	DL nextword← l
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setmode: m | |
	DL next← 2.
	DL next← m
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setx: x | |
	EL next← 238.
	EL nextword← x
! !
!PressFile methodsFor: 'undefined' stamp: ''!
sety: y | |
	EL next← 239.
	EL nextword← y
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showrect: rect color: c | |
	ColorPrint
	  ifTrue: [self brightness: c].
	self showrect: (self transrect: rect)
! !
!PressFile methodsFor: 'undefined' stamp: ''!
hue: b | |
	EL next← 249.
	EL next← b
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showchars: n skip: t | |
	(t = 1 and: [(n ≥ 1 and: [n ≤ 32])])
	  ifTrue: [EL next← 64 + n - 1]
	  ifFalse:
		[self showchars: n.
		self skipchars: t]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showchars: n | |
	n = 0
	  ifFalse:
		[(n ≥ 1 and: [n ≤ 32])
		  ifTrue: [EL next← n - 1 "short form"]
		  ifFalse:
			[EL next← 240.
			EL next← n]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
skipchars: n | |
	n = 0
	  ifFalse:
		[(n ≥ 1 and: [n ≤ 32])
		  ifTrue: [EL next← 32 + n "short form" - 1]
		  ifFalse:
			[EL next← 241.
			EL next← n]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setspacex: x | |
	estate ◦ 2 = x
	  ifFalse:
		[estate ◦ 2 ← x.
		(x ≥ 0 and: [x ≤ 2047])
		  ifTrue: [EL nextword← 24576 + x "short form"]
		  ifFalse:
			[EL next← 244.
			EL nextword← x]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showrect: rect | |
	self setp: rect origin.
	EL next← 254.
	EL nextword← rect width.
	EL nextword← rect height
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setp: p | |
	EL next← 238 "self setx: p x; sety: p y".
	EL nextword← p x.
	EL next← 239.
	EL nextword← p y
! !
!PressFile methodsFor: 'undefined' stamp: ''!
saturation: s | |
	EL next← 250.
	EL next← s
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showdotsopaque: nwords | |
	EL next← 253.
	EL nextNumber: 4 ← nwords
! !
!PressFile methodsFor: 'undefined' stamp: ''!
brightness: b | |
	EL next← 248.
	EL next← b
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showdots: nwords | |
	EL next← 252.
	EL nextNumber: 4 ← nwords
! !
!PressFile methodsFor: 'undefined' stamp: ''!
skipcontrol: n | |
	EL next← 235 "immediate".
	EL next← n "now put n bytes in EL"
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showchar: char | |
	EL next← 243 "immediate".
	EL next← char
! !
!PressFile methodsFor: 'undefined' stamp: ''!
setspacey: y | |
	estate ◦ 3 = y
	  ifFalse:
		[estate ◦ 3 ← y.
		(y ≥ 0 and: [y ≤ 2047])
		  ifTrue: [EL nextword← 26624 + y "short form"]
		  ifFalse:
			[EL next← 245.
			EL nextword← y]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
resetspace | |
	EL next← 246
! !
!PressFile methodsFor: 'undefined' stamp: ''!
onlyoncopy: n | |
	EL next← 237.
	EL next← n
! !
!PressFile methodsFor: 'undefined' stamp: ''!
space | |
	EL next← 247
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showrectwidth: w height: h | |
	EL next← 254.
	EL nextword← w.
	EL nextword← h
! !
!PressFile methodsFor: 'undefined' stamp: ''!
skipcontrol: n type: t | |
	EL next← 242 "n bytes have been put in DL".
	EL nextword← n.
	EL next← t
! !
!PressFile methodsFor: 'undefined' stamp: ''!
entityorigin: t1 | |
	eorigin ← t1
! !
!PressFile methodsFor: 'undefined' stamp: ''!
selectPrinter | |
	^self selectPrinter: self defaultPrinterName
! !
!PressFile methodsFor: 'undefined' stamp: ''!
selectPrinter: oldName | t |
	user cr.
	user show: 'select a printer (currently '.
	user show: ((oldName ≡ false or: [oldName empty])
	  ifTrue: ['none']
	  ifFalse: [oldName]).
	user show: ')'.
	user cursorloc← user screenrect center.
	user restoredisplay.
	t ← 0.
	[t = 0] whileTrueDo: [t ← printerMenu wbug].
	^t ≤ printers length
	  ifTrue: [printers ◦ t]
	  ifFalse:
		[t = (printers length + 1)
		  ifTrue: [oldName]
		  ifFalse: [ "same" "none"
			false]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
page | |
	self closePage
! !
!PressFile methodsFor: 'undefined' stamp: ''!
screenout: rect scale: t2 | |
	scale ← t2.
	user displayoffwhile⦂ [
		 "puts a bit map image onto the PressFile.  The standard
	scaling is 32 micas per Alto dot.  22 looks better, Dover only
	works with 32"
		(self somefont.
		self setp: (self transrect: rect) origin.
		self bitmap: rect bits: false.
		self close)]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
close | p i font t4 |
	DL writing ≡ false
	  ifTrue: [DL close]
	  ifFalse:
		[self closePage.
		(parts ≡ false or: [parts empty])
		  ifFalse:
			[ "if present, include the external file part  --- added Sept 80"
			FL empty
			  ifFalse:
				[self part⦂ [(DL append: FL)] code: 2.
				FL reset.
				self padpage] "put font names and descriptions into font directory (part)".
			self part⦂ [((1 to: fontdefs length) do:
					[:i | font ← fontdefs ◦ i.
					DL nextword← 16.
					DL nextword← i - 1.
					DL next← font min.
					DL next← font max.
					self Bcpl: font name pad: 20.
					DL next← font face.
					DL next← font min.
					DL nextword← font pointsize.
					DL nextword← 0])] code: 1 "write part directory. Pstart is current page position".
			DL append: parts asReadStream.
			self padpage.
			p ← self recordnum "document directory".
			DL nextword← 27183 "press password".
			DL nextword← p + 1 "number of records".
			DL nextword← parts position / 8 "number of parts".
			DL nextword← Pstart "part dir and length".
			DL nextword← p - Pstart.
			DL nextword← ¬1 "backpointer to obsolete doc dir".
			DL append: user timewords "2 time words".
			DL nextword← 1 "first and last copies".
			DL nextword← 1.
			DL nextword← ¬1 "first and last pages".
			DL nextword← ¬1.
			DL nextword← 'S' ◦ 1 "solid color (looked at by color printers)".
			DL next: (2 * (127 - 12)) ← 255.
			p ← user now.
			self Bcpl: self name pad: 52.
			self Bcpl: (currentProfile ≡ nil
			  ifTrue: [dp0 diskID ◦ 1]
			  ifFalse: [currentProfile printedBy]) pad: 32.
			self Bcpl: ((t4 ← (String new: 40) asStream) print: p ◦ 1.
			t4 space.
			t4 print: p ◦ 2.
			t4 contents) pad: 40.
			self padpage.
			DL close.
			parts reset]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
toPrinter: ndest | psocket dest np t perr |
	 "a printer name"
	ndest ≡ false
	  ifTrue: [^false].
	 "don't try to print"
	E ≡ nil
	  ifTrue:
		[t ← (String new: 100) "use O.S. if Smalltalk ethercode not alive" asStream.
		t append: 'Empress. '.
		t append: self name.
		ndest length > 0
		  ifTrue:
			[t space.
			t append: ndest.
			t append: '/H'].
		t append: '; Resume.~ Small.Boot'.
		user quitThen: t asReadStream]
	  ifFalse:
		[dest ← ''.
		perr ← psocket ← false.
		np ← printers length + 1.
		DL readonly.
		[ndest] whileTrueDo:
			[(perr or: [ndest empty])
			  ifTrue:
				[perr ← false.
				ndest ← self selectPrinter: dest]
			  ifFalse:
				[dest = ndest
				  ifFalse:
					[ "to same printer"
					dest ← ndest "close previous socket".
					psocket
					  ifTrue:
						[psocket close.
						psocket ← false]].
				psocket
				  ifFalse:
					[ "create new socket"
					(psocket ← EFTPSender new hostName: dest)
					  ifTrue: [psocket wakeup]
					  ifFalse:
						[user cr.
						user show: 'name lookup failure']] "send file".
				(psocket and: [(user displayoffwhile⦂ [(psocket send: DL reset)])])
				  ifTrue: [ndest ← false "success--stop"]
				  ifFalse: [ "failure--switch servers?"
					perr ← true]] "cleanup after success or abort"].
		DL close.
		psocket
		  ifTrue: [psocket close]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
pictureinit: rect scale: t2 | |
	scale ← t2.
	boundbox ← boundbox include: (self transrect: rect).
	self somefont
! !
!PressFile methodsFor: 'undefined' stamp: ''!
pictureinit | |
	self pictureinit: user screenrect scale: PressScale
! !
!PressFile methodsFor: 'undefined' stamp: ''!
toPrinter | |
	self toPrinter: self defaultPrinterName
! !
!PressFile methodsFor: 'undefined' stamp: ''!
clip: boundingbox | |
	
! !
!PressFile methodsFor: 'undefined' stamp: ''!
entity: box containing⦂ expr | v |
	self startEntity.
	boundbox ← box.
	v ← expr eval.
	self closeEntity.
	^v
! !
!PressFile methodsFor: 'undefined' stamp: ''!
box: rect hue: hue sat: sat bright: bright containing⦂ expr | w r |
	self entity: (self transrect: (w ← rect inset: ¬2)) containing⦂ [
		((w minus: rect) do: [:r | self showrect: r color: 0].
		ColorPrint
		  ifTrue:
			[self hue: hue.
			self saturation: sat.
			self showrect: rect color: bright.
			self brightness: 0].
		expr eval)]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
somefont | |
	 "fool self into writing non-empty fontdir"
	self fontindex: 5 * 16 style: DefaultTextStyle
! !
!PressFile methodsFor: 'undefined' stamp: ''!
selectfont: f | |
	estate ◦ 1 = f
	  ifFalse: [EL next← 112 + (estate ◦ 1 ← f)]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
fontindex: code style: style | ix font n |
	code ← code land: 243 "return index if in font dictionary" "Remove underline and strikeout".
	style = prevstyle
	  ifTrue:
		[(ix ← fontcodes find: code) > 0
		  ifTrue: [^ix]]
	  ifFalse:
		[fontcodes all← nil "invalid across style change".
		prevstyle ← style].
	n ← code / 16 + 1.
	font ← (WidthTable new named: (style fontfamily: n) pointsize: (style fontsize: n) face: (code / 2 land: 1) + (code * 2 land: 2)) lookup.
	(ix ← fontdefs find: font) > 0
	  ifTrue:
		[fontcodes ◦ ix ← code.
		^ix].
	 "add entry to font dictionary"
	fontdefs length = 16
	  ifTrue:
		[user notify: 'too many fonts'.
		^1].
	fontcodes ← {fontcodes , code}.
	fontdefs ← {fontdefs , font}.
	^fontcodes length
! !
!PressFile methodsFor: 'undefined' stamp: ''!
codefont: code style: style | |
	^fontdefs ◦ (self fontindex: code style: style)
! !
!PressFile methodsFor: 'undefined' stamp: ''!
scale: t1 | |
	scale ← t1
! !
!PressFile methodsFor: 'undefined' stamp: ''!
reset | |
	DL readwriteshorten.
	DL reset.
	self of: DL
! !
!PressFile methodsFor: 'undefined' stamp: ''!
of: t1 | |
	DL ← t1.
	EL ← Set new string: 200.
	FL ← Set new string: 40.
	parts ← Set new string: 40.
	fontcodes ← Vector new: 0.
	fontdefs ← Vector new: 0.
	estate ← Vector new: 3 "font, spacex, spacey, ...".
	prevstyle ← nil.
	self scale: PressScale.
	self startPage
! !
!PressFile methodsFor: 'undefined' stamp: ''!
moveto: p | |
	DL nextword← 0.
	DL nextPoint← p
! !
!PressFile methodsFor: 'undefined' stamp: ''!
objectGotoScreen: p pen: pen | |
	DL nextword← pen.
	DL nextPoint← self transpt: p
! !
!PressFile methodsFor: 'undefined' stamp: ''!
drawlinefromscreen: p1 to: p2 width: width | |
	^self drawlinefrom: (self transpt: p1) to: (self transpt: p2) width: width * scale
! !
!PressFile methodsFor: 'undefined' stamp: ''!
drawto: p | |
	DL nextword← 1.
	DL nextPoint← p
! !
!PressFile methodsFor: 'undefined' stamp: ''!
object⦂ expr atScreen: p | |
	self showobject⦂ [
		(self objectGotoScreen: p pen: 0.
		expr eval)]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
showobject⦂ exp | p |
	p ← self padword "expression containing moveto, drawto, drawcurve".
	exp eval.
	EL next← 251.
	EL nextword← DL wordposition - p
! !
!PressFile methodsFor: 'undefined' stamp: ''!
drawcurve: v | |
	v length ≠ 12
	  ifTrue: [user notify: 'illegal drawcurve']
	  ifFalse:
		[DL nextword← 2.
		v do: [:v | DL nextword← v]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
drawlinefrom: p1 to: p2 width: width | d length t1 t2 |
	(d ← p2 - p1) = (0 ⌾ 0)
	  ifFalse:
		[d x← d x asFloat.
		d y← d y asFloat.
		width ← width asFloat.
		length ← (d x * d x + (d y * d y)) sqrt.
		d x← (d x * width / length) asInteger.
		d y← (d y * width / length) asInteger.
		t1 ← d y ⌾ (0 - d x).
		t2 ← 0 - d y ⌾ d x.
		self showobject⦂ [
			(self moveto: p1 + t1.
			self drawto: p2 + t1.
			self drawto: p2 + t2.
			self drawto: p1 + t2.
			self drawto: p1 + t1)]].
	self drawdiscat: p2 radius: width
! !
!PressFile methodsFor: 'undefined' stamp: ''!
drawdiscat: pt radius: radius | dx dy i |
	radius ≤ 16
	  ifFalse:
		[dx ← #(5 4 3 1 ¬1 ¬3 ¬4 ¬5 ¬5 ¬4 ¬3 ¬1 1 3 4 5 ).
		dy ← #(1 3 4 5 5 4 3 1 ¬1 ¬3 ¬4 ¬5 ¬5 ¬4 ¬3 ¬1 ).
		self showobject⦂ [
			(self moveto: pt + (dx ◦ 16 * radius / 5 ⌾ (dy ◦ 16 * radius / 5)).
			(1 to: 16) do: [:i | self drawto: pt + (dx ◦ i * radius / 5 ⌾ (dy ◦ i * radius / 5))])]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
recordnum | |
	^DL positionSize: recordsize
! !
!PressFile methodsFor: 'undefined' stamp: ''!
closeEntity | |
	self closeEntity: SMentity
! !
!PressFile methodsFor: 'undefined' stamp: ''!
Bcpl: s pad: n | slen |
	slen ← s length "write a Bcpl string and padding to fill n bytes (used by close)" min: n - 1.
	DL next← slen.
	DL append: s ◦ (1 to: slen).
	DL next: (n - (slen + 1)) ← 0
! !
!PressFile methodsFor: 'undefined' stamp: ''!
skipcode: code data: s | t |
	(t ← s length "called by hidePress:complete:. s is a String" + 1) < 256
	  ifTrue:
		[self skipcontrol: t "immediate, in EL".
		EL next← code.
		EL append: s]
	  ifFalse:
		[ "in DL"
		DL next← code.
		DL append: s.
		self skipcontrol: t type: SMentity]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
data | |
	^DL "slightly dangerous"
! !
!PressFile methodsFor: 'undefined' stamp: ''!
classInit | a p |
	Smalltalk declare: #PressScale as: 32 "PressFile classInit.".
	recordsize ← 512.
	SMentity ← 5.
	a ← (String new: 250) asStream "from [Maxc1]<Altodocs>NetTopology.Press, October 1980. in order of net number".
	printers ← #('Navajo' 'Menlo' 'Clover' 'Lilac' 'Kanji' 'Wonder' 'Quake' 'Puff' 'White' 'Colorado' 'Niagara' 'Tioga' 'Yoda' 'Lily' 'Ranger' 'Windfall' 'Genesee' 'Amarok' 'Yankee' 'Cyclops' 'Rover' 'SPGEng' 'Emperor' 'Thud' 'Adelie' 'Daisy' 'RockHopper' 'Bud' ) "net #" "printer names" " 1" "HENRIETTA" " 3" "PARC: BLDG 35, FLOOR 2" " 5" "PARC:  BLDG 34" " 6" "PARC: BLDG 35, FLOOR 1&3" "10" "A&E" "12" "PASADENA" "14" "WEBSTER" "20" "PARC: BLDG 32" "21" "SPG" "23" "DALLAS" "26" "DC" "27" "WEBSTER" "33" "TORONTO" "34" "STAMFORD" "36" "LEESBURG" "54" "A&E" "55" "A&E" "56" "A&E" "60" "BAYHILL" "62" "?".
	printers do:
		[:p | a append: p.
		a cr].
	a append: 'same printer'.
	a cr.
	a append: 'no printer'.
	printerMenu ← Menu new string: a contents
! !
!PressFile methodsFor: 'undefined' stamp: ''!
padword | |
	 "make object (lines or dots) start on word boundary"
	DL padNext← 0
	  ifTrue: [self skipchars: 1].
	^DL wordposition
! !
!PressFile methodsFor: 'undefined' stamp: ''!
startPage | |
	EL reset.
	Pstart ← self recordnum.
	self startEntity
! !
!PressFile methodsFor: 'undefined' stamp: ''!
startEntity | |
	DLstart ← DL position.
	ELstart ← EL wordposition.
	boundbox ← 0 asRectangle.
	eorigin ← 0 ⌾ 0.
	estate all← ¬1.
	estate ◦ 1 ← 0
! !
!PressFile methodsFor: 'undefined' stamp: ''!
part⦂ exp code: c | fp |
	self closePage.
	fp ← self recordnum.
	exp eval.
	self part: c start: fp
! !
!PressFile methodsFor: 'undefined' stamp: ''!
closeEntity: etype | |
	EL wordposition = ELstart
	  ifFalse:
		[ "Put a trailer into the EL"
		EL padNext← 255 "word-pad EL with <Nop>".
		EL next← etype.
		EL next← 0 "fontset" "dlstart relative to DL location in file".
		EL nextNumber: 4 ← DLstart - (Pstart * recordsize).
		EL nextNumber: 4 ← DL position - DLstart.
		EL nextPoint← eorigin "entity origin".
		EL nextPoint← boundbox origin.
		EL nextPoint← boundbox extent.
		EL nextword← EL wordposition - ELstart + 1.
		self startEntity]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
part: type start: start | padding |
	padding ← self padpage.
	parts nextword← type.
	parts nextword← start.
	parts nextword← self recordnum - start.
	parts nextword← padding.
	self startPage
! !
!PressFile methodsFor: 'undefined' stamp: ''!
closePage | |
	self closeEntity.
	EL empty
	  ifFalse:
		[DL padNext← 0.
		DL nextword← 0.
		DL append: EL asReadStream.
		self part: 0 start: Pstart]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
append: x | |
	^DL append: x
! !
!PressFile methodsFor: 'undefined' stamp: ''!
padpage | |
	^(DL pad: recordsize with: 0) "words of padding to end of page" / 2
! !
!PressFile methodsFor: 'undefined' stamp: ''!
open | t |
	DL readonly "read the parts (and font directory?)" "reopen?".
	DL settoend.
	DL skip: 0 - recordsize.
	(DL nextword = 27183 and: [DL nextword = (self recordnum + 1)])
	  ifTrue:
		[t ← DL nextword.
		DL position: DL nextword size: recordsize.
		parts ← (DL next: t * 8) viewer.
		self readPart]
	  ifFalse: [self error: 'not a press file']
! !
!PressFile methodsFor: 'undefined' stamp: ''!
nextParagraph | s p |
	(s ← self nextControl)
	  ifTrue:
		[s ← s asStream.
		p ← Paragraph new.
		s next = p pressCode
		  ifTrue: [^p fromPress: self value: s].
		^false]
	  ifFalse: [^false]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
filin | p |
	self open.
	[p ← self nextParagraph] whileTrueDo:
		[FilinSource ← self.
		user print: nil ⓢ p text.
		user space].
	FilinSource ← nil.
	self close
! !
!PressFile methodsFor: 'undefined' stamp: ''!
nextControl | command t entity |
	command ← nil "return the next skip-control information".
	[true] whileTrueDo:
		[(estate and: [command]) ≡ false
		  ifTrue:
			[(t ← EL next "either or both false. get next entity")
			  ifTrue:
				[estate ← EL next viewer.
				command ← nil.
				t ◦ 1 ≠ SMentity
				  ifTrue: [estate ← false "ignore this entity"]
				  ifFalse:
					[DLstart ← (t ◦ (3 to: 6)) asStream nextNumber: 4.
					DL position← Pstart * recordsize + DLstart]]
			  ifFalse:
				[ "no more entities on current part (page)"
				self readPart
				  ifFalse: [ "no more pages"
					^false]]]
		  ifFalse:
			[entity ← estate.
			[command ← entity next] whileTrueDo:
				[command < 64 "some stuff arranged by probable frequency"
				  ifTrue: [DL skip: (command land: 31) "show-characters-short (0-037)
		skip-characters-short (040-077)" + 1]
				  ifFalse:
					[command = 238
					  ifTrue: [entity nextword "set-x"]
					  ifFalse:
						[command = 239
						  ifTrue: [entity nextword "set-y"]
						  ifFalse:
							[command < 96
							  ifTrue: [DL skip: (command land: 31) "show-characters-and-skip (0100-0137)" + 2]
							  ifFalse:
								[command < 112
								  ifTrue: [entity next "set-space-x-short (0140-0147)
		set-space-y-short  (0150-0157)" "(command land: 7)*256 +"]
								  ifFalse:
									[command < 128
									  ifFalse:
										[ "font" "command land: 017"
										command = 242
										  ifTrue:
											[t ← entity nextword "skip-control-bytes".
											entity next ≠ SMentity
											  ifTrue: [DL skip: t "ignore"]
											  ifFalse: [^DL next: t]]
										  ifFalse:
											[command = 240
											  ifTrue: [DL skip: entity next "show-characters"]
											  ifFalse:
												[command = 255
												  ifFalse:
													[ "nop"
													command < 235
													  ifFalse:
														[ "available (0200-0237)
		spare (0240-0352)"
														command = 235
														  ifTrue: [^entity next: entity next "skip-control-bytes-immediate"].
														command = 236
														  ifTrue: [entity skipwords: 5 "alternative"]
														  ifFalse:
															[command = 237
															  ifTrue: [entity next "only-on-copy"]
															  ifFalse:
																[command = 241
																  ifTrue: [DL skip: entity next "skip characters"]
																  ifFalse:
																	[command = 243
																	  ifTrue: [entity next "show-character-immediate"]
																	  ifFalse:
																		[command < 246
																		  ifTrue: [entity nextword "set-space-x (0364)
		set-space-y (0365)"]
																		  ifFalse:
																			[command < 248
																			  ifFalse:
																				[ "reset-space (0366)
		space (0367)"
																				command < 251
																				  ifTrue: [entity next "set-brightness (0370)
		set-hue (0371)
		set-saturation (0372)"]
																				  ifFalse:
																					[command = 251
																					  ifTrue: [DL skipwords: entity nextword "show-object"]
																					  ifFalse:
																						[command < 254
																						  ifTrue: [DL skipwords: (entity nextNumber: 4) "show-dots (0374)
		show-dots-opaque (0375)"]
																						  ifFalse:
																							[command = 254
																							  ifTrue: [entity skipwords: 2 "show-rectangle"]]]]]]]]]]]]]]]]]]]]]]]
! !
!PressFile methodsFor: 'undefined' stamp: ''!
readPart | t |
	estate ← false "read parts until we find a printed page or end".
	[t ← parts nextword] whileTrueDo:
		[Pstart ← parts nextword.
		t ≠ 0
		  ifTrue:
			[parts skip: 4 "not a printed page".
			t > 0 "font or other part" "a non-standard part. let document (estate?) interpret" "DL position ← Pstart*recordsize.
			estate fromPress: self name: t value: DL"]
		  ifFalse:
			[ "go to end of last record of entity list, ignoring padding"
			t ← parts nextword "length".
			DL position← Pstart + t * recordsize - (1 + parts nextword * 2).
			EL ← Set new vector: 50 "scan backwards for beginning of entity list, reading entities".
			[(t ← DL nextword) > 0] whileTrueDo:
				[t < 12
				  ifTrue: [user notify: 'illegal entity']
				  ifFalse:
					[DL skipwords: 0 - t "read entity and trailer (last 12 words of entity)".
					EL next← DL next: t - 12 * 2.
					EL next← DL next: 24.
					DL skipwords: ¬1 - t] "now reverse:  trailer, entity (1st), ... (last)"].
			^EL ← (EL asArray ◦ (EL length to: 1 by: ¬1)) asStream]].
	^false
! !
!PressFile methodsFor: 'undefined' stamp: ''!
transpt: p | |
	^Point new x: (p x * scale) asInteger y: (25400 - (p y * scale)) asInteger
! !
!PressFile methodsFor: 'undefined' stamp: ''!
transrect: rect | |
	^Rectangle new origin: (self transpt: rect minX ⌾ rect maxY) corner: (self transpt: rect maxX ⌾ rect minY)
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
close | |
	press close
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
toPrinter | |
	press toPrinter
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
press: t1 | |
	press ← t1
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
of: t1 | |
	strm ← t1.
	press ← PressFile new of: strm
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
init | |
	super init.
	page ← 1.
	ypos ← frame maxY
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
classInit | inch |
	inch ← 2540 "1 inch in micas".
	defaultframe ← (1.1 * inch) asInteger ⌾ (1 * inch) rect: (7.75 * inch) asInteger ⌾ (10 * inch)
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
defaultframe | |
	^defaultframe
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
print: para in: rect | result oldpara |
	(rect width = 0 or: [rect height = 0])
	  ifTrue: [user notify: 'zero dimension'].
	oldpara ← para.
	[(result ← para presson: press in: rect) Is: Integer] whileFalseDo: 
		[self nextpage "rest of para goes on next page".
		para ← result.
		rect ← rect minX ⌾ frame minY rect: rect maxX ⌾ ypos "original para can hide information. if it split across page boundaries,
	the format may vary. other completion flags can be added later"].
	oldpara hidePress: press complete: (oldpara ≡ para
	  ifTrue: [0]
	  ifFalse: [1]).
	^ypos ← result
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
nextpage | |
	self nextpage: true
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
print: para | |
	self print: para in: (Rectangle new origin: frame origin corner: frame maxX ⌾ ypos)
! !
!PressPrinter methodsFor: 'undefined' stamp: ''!
nextpage: h | n |
	press page.
	page ← page + 1.
	ypos ← frame maxY.
	h
	  ifTrue:
		[n ← page asString.
		press setp: frame maxX + 800 ⌾ (ypos + 960).
		press selectfont: (press fontindex: 0 style: DefaultTextStyle) - 1.
		press append: n.
		press showchars: n length]
! !
!Process methodsFor: 'undefined' stamp: ''!
suspendedContext: aContext | | suspendedContext ← aContext
! !
!Process methodsFor: 'undefined' stamp: ''!
suspendedContext | | ^suspendedContext
! !
!Process methodsFor: 'undefined' stamp: ''!
priority: anInteger | | priority ← anInteger
! !
!Process methodsFor: 'undefined' stamp: ''!
priority | | ^priority
! !
!Process methodsFor: 'undefined' stamp: ''!
onList: aLinkedList | | myList ← aLinkedList
! !
!Process methodsFor: 'undefined' stamp: ''!
offList | | myList ← nil
! !
!Process methodsFor: 'undefined' stamp: ''!
forContext: aContext priority: anInteger | |
	 self suspendedContext: aContext.
	 self priority: anInteger
! !
!Process methodsFor: 'undefined' stamp: ''!
terminate | |
	 Processor activeProcess==self
		ifTrue:
			[thisContext sender==nil
				ifFalse: [thisContext sender release].
			Processor suspend]
		ifFalse:
			[self suspendedContext sender==nil
				ifFalse: [self suspendedContext sender release].
			myList removeLink: self]
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
activeProcess | | ^activeProcess
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
activePriority | | ^activeProcess priority
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
critical⦂ aBlock | savedPriority value |
	savedPriority ← activeProcess priority.
	activeProcess priority: highestPriority + 1.  "Can't be pre-empted"
	value ← aBlock value.
	activeProcess priority: savedPriority.
	^value
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
systemInit |
 |
	"Really create and install the scheduler, and snapshot the result."
	self init.
	activeProcess suspendedContext: thisContext.
	Processor ← self.
	"Do a user snapshot without setting InitialContext, which no longer exists."
	user releaseExternalViews.
	user snapshotPrimitive.
	"Control returns here in the new system with everything as it should be."
	user mapDisplay.
	user restore
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
init | |
	 quiescentProcessLists ← Vector new: highestPriority.
	 (1 to: highestPriority)
		do: [:priority |
			quiescentProcessLists◦priority ← LinkedList init].
	 activeProcess ← Process new priority: 2
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
classInit | | highestPriority ← 4
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
wakeHighestPriority | priority processList process | 
	 priority ← highestPriority.
	 [(quiescentProcessLists at: priority) isEmpty]
				whileTrue: [priority ← priority-1].
	 processList ← quiescentProcessLists at: priority.
	 process ← processList removeFirstLink.
	 process offList.
	 ^process
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
sleep: aProcess | processList | 
	 processList ← quiescentProcessLists at: aProcess priority.
	 processList addLastLink: aProcess.
	 aProcess onList: processList
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
transferTo: newProcess | |
	 activeProcess suspendedContext: thisContext sender.
	 activeProcess ← newProcess.
	 thisContext sender← newProcess suspendedContext
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
simulateResume: newProcess | | 
	 (newProcess priority) > (activeProcess priority)
		ifTrue: [self sleep: activeProcess.
			self transferTo: newProcess]
		ifFalse: [self sleep: newProcess]
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
simulateSuspend | oldProcess | 
	 oldProcess ← activeProcess.
	 self transferTo: (self wakeHighestPriority).
	 ^oldProcess
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
suspend | | <primitive: 71>
	user croak
! !
!ProcessorScheduler methodsFor: 'undefined' stamp: ''!
resume: newProcess | | <primitive: 70>
	user croak
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
install | |
	 "Establish this project and its userview as the current screen view"
	Changes ← changes.
	(user ← userview) install.
	self putTitle.
	user restart
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
putTitle | |
	titlepara ≡ nil
	  ifTrue: [titlepara ← 'Top View' asParagraph allBold].
	titleframe put: titlepara centered: user screenrect extent x / 3 ⌾ 8.
	titleframe outline
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
runParent | |
	 "leave this view by installing the one above"
	parent install
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
userview: t1 changes: t2 parent: t3 | |
	 "load state"
	userview ← t1.
	changes ← t2.
	parent ← t3
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
init | |
	 "a new window"
	self userview: (user copyIn: self) changes: HashSet init parent: user projectWindow.
	self newframe.
	self show
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
classInit | |
	actionMenu ← Menu new string: 'enter'
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
close | |
	 "break circular links"
	userview ← parent ← nil
! !
!ProjectWindow methodsFor: 'undefined' stamp: ''!
yellowbug | |
	actionMenu bug = 1
	  ifTrue: [self install]
! !
!Queue methodsFor: 'undefined' stamp: ''!
dequeue: num | n |
	position - readposition < num
	  ifTrue: [^false].
	n ← (array ◦ (readposition + 1 to: readposition + num)) copy.
	readposition ← readposition + num.
	^n
! !
!Queue methodsFor: 'undefined' stamp: ''!
length | |
	^position - readposition
! !
!Queue methodsFor: 'undefined' stamp: ''!
dequeue | |
	readposition ≥ position
	  ifTrue:
		[readposition ← position ← 0.
		^false].
	^array ◦ (readposition ← readposition + 1)
! !
!Queue methodsFor: 'undefined' stamp: ''!
enQ1: n | |
	 "A noninterruptable enqueue"
	Top critical⦂ [(super next← n)].
	^n
! !
!Queue methodsFor: 'undefined' stamp: ''!
skip: x | |
	readposition ← readposition + x
! !
!Queue methodsFor: 'undefined' stamp: ''!
deQ1 | n |
	 "A noninterruptable dequeue"
	Top critical⦂ [(n ← self dequeue)].
	^n
! !
!Queue methodsFor: 'undefined' stamp: ''!
peek | |
	readposition ≥ position
	  ifTrue:
		[readposition ← position ← 0.
		^false].
	^array ◦ (readposition + 1)
! !
!Queue methodsFor: 'undefined' stamp: ''!
next | |
	readposition ≥ position
	  ifTrue:
		[readposition ← position ← 0.
		^false].
	^array ◦ (readposition ← readposition + 1)
! !
!Queue methodsFor: 'undefined' stamp: ''!
push: x | |
	 "treat as LIFO queue"
	readposition > 0
	  ifTrue:
		[array ◦ readposition ← x.
		readposition ← readposition - 1]
	  ifFalse: [ "readpositon > 0, just jam it in"
		self insert: x] "otherwise insert on front"
! !
!Queue methodsFor: 'undefined' stamp: ''!
pastend← x | n |
	readposition = 0
	  ifTrue: [^super pastend← x].
	n ← position - readposition.
	array ◦ (1 to: n) ← array ◦ (readposition + 1 to: position).
	readposition ← 0.
	position ← n.
	^self next← x
! !
!Queue methodsFor: 'undefined' stamp: ''!
of: t1 | |
	array ← t1.
	position ← 0.
	readposition ← 0.
	limit ← array length
! !
!Queue methodsFor: 'undefined' stamp: ''!
end | |
	^readposition ≥ position
! !
!Queue methodsFor: 'undefined' stamp: ''!
empty | |
	^readposition ≥ position
! !
!Queue methodsFor: 'undefined' stamp: ''!
reset | |
	readposition ← position ← 0
! !
!Queue methodsFor: 'undefined' stamp: ''!
of: t1 from: t2 to: t3 | |
	array ← t1.
	position ← t2.
	limit ← t3.
	user notify: 'of:from:to: is not appropriate for Queues'
! !
!Queue methodsFor: 'undefined' stamp: ''!
contents | |
	^(array ◦ (readposition + 1 to: position)) copy
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
process: packet | |
	self freePacket: packet "my subclasses use this"
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
socProcess: Ipac | |
	 "I have received a packet"
	Ipac pupType = ackType
	  ifTrue:
		[(Ipac pupID1 = seqNum and: [Ipac pupID0 = transaction])
		  ifTrue:
			[self timerOff "a legal acknowledgement".
			ackOK ← true] "an old acknowledgement".
		self freePacket: Ipac]
	  ifFalse:
		[ "must be a trasmission started elsewhere"
		self process: Ipac]
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
init | |
	self retransmit: 10 every: 180.
	seqNum ← transaction ← 0.
	outPac ← ackOK ← false.
	abortTransfer ← true "stop an old timer from perhaps firing"
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
sendBlock: str | |
	outPac dataString← str "Take the data from a string (1-532 bytes), send it out; uses outPac".
	self sendPacket "tries to do it reliably".
	abortTransfer
	  ifTrue: [^false]
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
sendPacketOnce | |
	 "special routine to send the outPac packet, no retransmission"
	outPac pupID1← seqNum.
	outPac pupID0← transaction "pupID0 can be used by one of my subclasses".
	self setAddressesAndComplete: outPac.
	self timerOff
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
sendEndSequence | |
	outPac pupType← 26 "This will do the 3-way handshake, and close the connection.
		send end, wait for ack" "end" "set the packet length".
	outPac pupLength← 22.
	self sendPacket "gets sent reliably, we hope".
	abortTransfer
	  ifTrue:
		[self reset.
		^false].
	 "send the last gratuitous end, do not try to retransmit"
	.
	self sendPacketOnce.
	self reset.
	^true
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
sendData | i t buf len |
	buf ← outPac pupString "send one packet of data from myStream".
	i ← 24 "data bytes are 1-512, 25-536".
	(myStream is: FileStream)
	  ifTrue: [len ← 512 - (myStream readString: buf from: i + 1 "read characters faster (should work especially well for the usual case:
		FileStreams starting on a page boundary, with page sizes of 512)" to: i + 512)]
	  ifFalse:
		[ "fill the buffer the slow, careful way"
		[i < 536 and: [(t ← myStream next)]] whileTrueDo: [buf ◦ (i ← i + 1) ← t].
		len ← i - 24].
	eof ← len < 512.
	len = 0
	  ifFalse:
		[ "empty packet. don't send"
		outPac pupType← 24 "Data" "set the packet length".
		outPac dataLength← len "send packet reliably or abort, then return".
		self sendPacket]
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
sendPacket | |
	 "general routine to send the outPac packet, maybe retransmit, get ack"
	ackOK ← abortTransfer ← false.
	outPac pupID1← seqNum.
	outPac pupID0← transaction "pupID0 can be used by one of my subclasses".
	self setAddressesAndComplete: outPac.
	[abortTransfer or: [ackOK]] whileFalseDo:  [].
	seqNum ← seqNum + 1
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
send: t1 | |
	myStream ← t1.
	 "Sends a whole stream, and an end sequence.
	let the caller hand in a stream, or a file already opened"
	outPac
	  ifFalse: [outPac ← self freePacket].
	seqNum ← 0.
	abortTransfer ← eof ← false.
	[eof or: [abortTransfer]] whileFalseDo:  [self sendData].
	abortTransfer
	  ifTrue:
		[self reset.
		^false].
	 "We hit the end of file, do the end sequence and close the connection"
	self sendEndSequence
	  ifTrue: [^myStream].
	^false "all done!"
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
release | |
	self reset.
	inQ ← false.
	super release
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
reset | |
	outPac ← self freePacket: outPac.
	self timerOff
! !
!RPPSocket methodsFor: 'undefined' stamp: ''!
timerFired | |
	(ackOK or: [abortTransfer]) "This piece of code only runs when a Timer fires;  
	Don't do an active return"
	  ifFalse:
		[ "This transaction has been terminated"
		self timerOn
		  ifTrue: [self completePup: outPac "retransmit"]
		  ifFalse:
			[user show: 'Excessive retransmits in RPP retransmit'.
			abortTransfer ← true]]
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
vec | |
	^vec
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
has: pt | |
	^rect has: pt
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
reset | |
	cur ← 0
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
value | |
	^vec ◦ cur
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
moveto: pt | |
	rect moveto: pt.
	cur ← 0.
	^rect corner x ⌾ rect origin y
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
vec: t1 at: r width: t3 | |
	vec ← t1.
	size ← t3.
	rect ← r rect: r + (1 ⌾ vec length * size).
	cur ← 0
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
vec: t1 at: r height: t3 | |
	vec ← t1.
	size ← t3.
	rect ← r rect: r + (vec length ⌾ 1 * size).
	cur ← 0
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
release: a thenPush: b | boxer offset |
	a = b
	  ifFalse:
		[offset ← (size = rect extent y
				  ifTrue: [size ⌾ 0]
				  ifFalse: [0 ⌾ size]).
		a ≠ 0
		  ifTrue:
			[boxer ← Rectangle new origin: offset * (a - 1) + rect origin + 1 extent: size ⌾ size - 1.
			boxer comp.
			(boxer inset: 1 ⌾ 1) comp].
		b ≠ 0
		  ifTrue:
			[boxer ← Rectangle new origin: offset * (b - 1) + rect origin + 1 extent: size ⌾ size - 1.
			boxer comp.
			(boxer inset: 1 ⌾ 1) comp]]
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
push: a | |
	self release: cur thenPush: a.
	^vec ◦ (cur ← a)
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
bug: pt | r a |
	r ← pt - rect origin - (1 ⌾ 1) / size.
	a ← r x + r y + 1.
	^self push: a
! !
!RadioButtons methodsFor: 'undefined' stamp: ''!
setvalue: v | i |
	i ← (vec find: v) "if value has been lost, set self to 1" max: 1.
	self push: i.
	^i
! !
!Reader methodsFor: 'undefined' stamp: ''!
of: s | |
	typetbl ← typetable.
	token ← Stream default.
	source ← s asStream.
	self step
! !
!Reader methodsFor: 'undefined' stamp: ''!
classInit | strm type first last i |
	 "Initialize the type and mask tables"
	typetable ← String new: 256.
	strm ← Stream new of: #(5 0 255 1 65 90 1 97 122 2 48 57 3 58 58 3 3 3 4 9 10 4 12 13 4 32 32 6 34 34 6 25 25 7 39 39 8 21 21 9 26 26 10 30 30 11 40 41 ) "(initialize)" "upper and lower case letters" "digits" "colon, open colon" "TAB, LF, FF, CR, blank" "5 is one-char tokens" "comment quote and ➲" "string quote" "high-minus" "^Z (format trailer)" "DOIT" "open and close paren".
	[type ← strm next] whileTrueDo:
		[first ← strm next.
		last ← strm next.
		(first + 1 to: last + 1) do: [:i | typetable ◦ i ← type]]
! !
!Reader methodsFor: 'undefined' stamp: ''!
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
!Reader methodsFor: 'undefined' stamp: ''!
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
!Reader methodsFor: 'undefined' stamp: ''!
step | |
	nextchar ← source next
! !
!Reader methodsFor: 'undefined' stamp: ''!
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
!Reader methodsFor: 'undefined' stamp: ''!
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
!Reader methodsFor: 'undefined' stamp: ''!
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
!Reader methodsFor: 'undefined' stamp: ''!
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
!Reader methodsFor: 'undefined' stamp: ''!
read | |
	^self readInto: TokenCollector default
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
translateto: pt | |
	self translate: pt - origin
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
translate: pt | |
	origin ← origin + pt.
	corner ← corner + pt
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
usersize | |
	self usersize: user screenrect
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
usermove | |
	self usermove: user screenrect
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
moveby: pt | |
	origin ← origin + pt.
	corner ← corner + pt
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
moveto: pt | |
	corner ← corner + pt - origin.
	origin ← pt
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
dragto: dest | v i |
	self blt: dest mode: storing.
	v ← dest rect: dest + self extent.
	(self minus: v) do: [:i | i clear].
	origin ← dest.
	corner ← v corner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
maxstretch: bound | bx by boundr selfr |
	bx ← (bound corner - origin) x.
	by ← (bound corner - origin) y.
	boundr ← bx asFloat / by.
	selfr ← self width asFloat / self height.
	selfr > boundr
	  ifTrue: [self extent← bx ⌾ (bx asFloat / selfr) asInteger]
	  ifFalse: [self extent← (by asFloat * selfr) asInteger ⌾ by]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
growto: t1 | |
	corner ← t1
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
growby: pt | |
	corner ← corner + pt
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
isWithin: rect | |
	 "am I equal to or contained within rect"
	^origin ≥ rect origin and: [corner ≤ rect corner]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
intersects: r | |
	^(origin max: r origin) < (corner min: r corner)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
center | |
	^origin + corner / 2
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
inset: p1 | |
	^origin + p1 rect: corner - p1
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
inset: p1 and: p2 | |
	^origin + p1 rect: corner - p2
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
sideNearest: pt | d dmin i imin |
	dmin ← 32767.
	(0 to: 3) do:
		[:i | dmin > (d ← self side: i distanceTo: pt) abs
		  ifTrue:
			[dmin ← d.
			imin ← i]].
	^imin
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
intersect: r | |
	^Rectangle new origin: (origin max: r origin) corner: (corner min: r corner)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
/ scale | |
	^Rectangle new "Return a Rectangle which is the quotient of me and scale (which is a Rectangle, Point, or Number)" origin: origin / scale asRectOrigin corner: corner / scale asRectCorner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
* scale | |
	^Rectangle new "Return a Rectangle which is the product of me and scale (which is a Rectangle, Point, or Number)" origin: origin * scale asRectOrigin corner: corner * scale asRectCorner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
= r | |
	^origin = r origin and: [corner = r corner]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
- delta | |
	^Rectangle new "Return a Rectangle which is the difference of me and delta (which is a Rectangle, Point, or Number)" origin: origin - delta asRectOrigin corner: corner - delta asRectCorner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
+ delta | |
	^Rectangle new "Return a Rectangle which is the sum of me and delta (which is a Rectangle, Point, or Number)" origin: origin + delta asRectOrigin corner: corner + delta asRectCorner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
nearest: pt | |
	^((origin x max: pt x) min: corner x) ⌾ ((origin y max: pt y) min: corner y)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
include: r | |
	 "Returns the merge with an adjacent rectangle."
	^(origin min: r origin) rect: (corner max: r corner)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
empty | |
	^origin < corner ≡ false
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
max: rect | |
	^Rectangle new origin: (origin min: rect origin) corner: (corner max: rect corner)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
has: pt | |
	^origin ≤ pt and: [pt < corner]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
top | |
	^origin y
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
leftside | |
	^origin x
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
height | |
	^corner y - origin y
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
corner | |
	^corner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
extent | |
	^corner - origin
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
origin | |
	^origin
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
width← w | |
	 "change my right x to make my width w"
	corner x← origin x + w
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
area | |
	^self width * self height
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
width | |
	^corner x - origin x
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
extent← extent | |
	corner ← origin + extent.
	^extent
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
corner← t1 | |
	corner ← t1
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
origin← t1 | |
	origin ← t1
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
height← h | |
	 "change my bottom y to make my height h"
	corner y← origin y + h
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
corners | v |
	v ← Vector new: 4.
	v ◦ 1 ← origin.
	v ◦ 2 ← corner x ⌾ origin y.
	v ◦ 3 ← corner.
	v ◦ 4 ← origin x ⌾ corner y.
	^v
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
bottom | |
	^corner y
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
maxX | |
	^corner x
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
maxY | |
	^corner y
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
minY | |
	^origin y
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
minX | |
	^origin x
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
size | |
	^corner - origin
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
hash | |
	^super hash
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
rightside | |
	^corner x
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
outline | |
	 "default border is two thick"
	self outline: 2
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
outline: thick | t |
	t ← ¬1 ⌾ ¬1 * thick.
	(self inset: t) clear: black.
	self clear: white
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
bitsFromString: bitmap mode: mode clippedBy: clipRect | t5 |
	"Load the screen bits within my area from those stored in bitmap.  If clipRect is not nil, then load only those bits within both  myself and clipRect"
	(t5 ← BitBlt new toDisplay window: clipRect) effect: mode.
	t5 sourceForm: (Form new extent: corner - origin bits: bitmap offset: nil).
	t5 destOrigin: origin.
	t5 sourceRect: (0 ⌾ 0 rect: corner - origin).
	t5 callBLT
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm print: origin.
	strm append: ' rect: '.
	strm print: corner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
bitsFromString: str | |
	self bitsFromString: str mode: storing "default stores bits onto display"
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
bitsIntoString: str | |
	self bitsIntoString: str mode: storing "default stores bits into the string"
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
bitsIntoString | str |
	str ← self bitmap.
	self bitsIntoString: str mode: storing.
	^str
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
bitStringLength | extent |
	extent ← corner - origin.
	^2 * extent y * (extent x + 15 / 16)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
asRectOrigin | |
	^origin
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
asRectCorner | |
	^corner
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
asRectangle | |
	 "Return self."
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
bitmap | extent |
	extent ← corner - origin.
	^Bitmap new: extent y * (extent x + 15 / 16)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
hardcopy: pf | |
	self hardcopy: pf thickness: 2
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
hardcopy: pf thickness: th | r |
	((self inset: 0 - th) minus: self) do: [:r | pf showrect: r color: 0]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
bitmapLength | extent |
	extent ← corner - origin.
	^extent y * (extent x + 15 / 16)
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
brush: dest mode: mode color: color clippedBy: clipRect | |
	 "Brush the screen bits within my area to the rectangle whose
		origin is dest and whose extent is the same as mine.
		If clipRect is not nil, then brush only those bits within both
		the destination rectangle and clipRect"
	DisplayBLTer copy window: clipRect;
		copyRect: self toPoint: dest effect: 8 + (mode land: 3) screen: color
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
blt: dest mode: mode clippedBy: clipRect | |
	 "Copy the screen bits within my area to the rectangle whose
		origin is dest and whose extent is the same as mine.
		If clipRect is not nil, then copy only those bits within both
		the destination rectangle and clipRect"
	DisplayBLTer copy window: clipRect;
		copyRect: self toPoint: dest effect: (mode land: 3) screen: 0
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
clear: color | |
	self color: color mode: storing
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
color: color mode: mode | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: origin effect: 12 + (mode land: 3) screen: color]
	  ifFalse: [self ALTOcolor: color mode: mode]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
bltcomp: dest mode: mode | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: dest effect: 4 + (mode land: 3) screen: 0]
	  ifFalse: [self ALTObltcomp: dest mode: mode]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
blt: dest mode: mode | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: dest effect: (mode land: 3) screen: 0]
	  ifFalse: [self ALTOblt: dest mode: mode]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
brush: dest mode: mode color: color | |
	NoteTaker
	  ifTrue: [DisplayBLTer copyRect: self toPoint: dest effect: 8 + (mode land: 3) screen: color]
	  ifFalse: [self ALTObrush: dest mode: mode color: color]
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
reverse | |
	self color: black mode: xoring
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
flash | |
	self comp.
	self comp
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
comp: color | |
	self color: color mode: xoring
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
clear | |
	 "default is backround"
	self color: background mode: storing
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
comp | |
	self color: black mode: xoring
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
origin: t1 corner: t2 | |
	origin ← t1.
	corner ← t2
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
origin: t1 extent: extent | |
	origin ← t1.
	corner ← origin + extent
! !
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
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
!Rectangle methodsFor: 'undefined' stamp: ''!
copy | |
	^origin copy "new rectangle" rect: corner copy
! !
!RemoteParagraph methodsFor: 'undefined' stamp: ''!
asParagraph | |
	file≡nil ifTrue: [^'an inaccessible RemoteParagraph' asParagraph].
	file position← self position.
	^Paragraph new readFrom: file
! !
!RemoteParagraph methodsFor: 'undefined' stamp: ''!
on: t1 | |
	 "Refer me to a specific file"
	file ← t1
! !
!RemoteParagraph methodsFor: 'undefined' stamp: ''!
asString | |
	^self asParagraph text
! !
!RemoteParagraph methodsFor: 'undefined' stamp: ''!
fromString: s | |
	self fromParagraph: s asParagraph
! !
!RemoteParagraph methodsFor: 'undefined' stamp: ''!
position← p | |
	p ← p intdiv: 2000.
	hipos ← (p ◦ 1) asInteger - 1000.
	lowpos ← (p ◦ 2) asInteger - 1000
! !
!RemoteParagraph methodsFor: 'undefined' stamp: ''!
position | |
	^hipos + 1000 * 2000 + (lowpos + 1000)
! !
!RemoteParagraph methodsFor: 'undefined' stamp: ''!
fromParagraph: p | |
	self position← file position "write me (only once!) on file".
	p storeOn: file
! !
!RetransmitSocket methodsFor: 'undefined' stamp: ''!
release | |
	retransTimer ≡ nil
	  ifFalse:
		[ "release circular structure"
		retransTimer disable.
		retransTimer ← nil]
! !
!RetransmitSocket methodsFor: 'undefined' stamp: ''!
setAddressesAndComplete: pac | |
	pac addressBlock← outAddBlock "this may need to be bracketed as critical?" "start timer".
	retransCount ← 0.
	retransTimer reset.
	self completePup: pac "self startTimer.
	super setAddressesAndComplete: pac"
! !
!RetransmitSocket methodsFor: 'undefined' stamp: ''!
timerFired | |
	self timerOn "subclass should redefine this" "again, e.g. self completePup: pac" "done"
! !
!RetransmitSocket methodsFor: 'undefined' stamp: ''!
timerOn | |
	(retransCount ← retransCount + 1 "turn on timer if retry count has not been reached") ≤ retransMax
	  ifTrue: [retransTimer reset]
	  ifFalse: [^false]
! !
!RetransmitSocket methodsFor: 'undefined' stamp: ''!
timerOff | |
	retransTimer ≡ nil
	  ifFalse: [retransTimer disable]
! !
!RetransmitSocket methodsFor: 'undefined' stamp: ''!
retransmit: t1 every: delay | |
	retransMax ← t1.
	retransTimer ← Timer new.
	retransTimer for: delay action⦂ [(self timerFired)]
! !
!RetransmitSocket methodsFor: 'undefined' stamp: ''!
startTimer | |
	retransCount ← 0.
	retransTimer reset
! !
!RoutingUpdater methodsFor: 'undefined' stamp: ''!
init | |
	super net: 0 host: 0 soc: 2 "create a new local soc number, broadcast to socket 2".
	outPac ← self freePacket.
	outPac pupType← 128.
	outPac dataString← ''.
	self retransmit: 3 every: 300
! !
!RoutingUpdater methodsFor: 'undefined' stamp: ''!
socProcess: pac | block gateway net count i |
	pac pupType "an input has arrived, we are running at a higher level.
	Check the packet type" = 129
	  ifTrue:
		[self timerOff.
		resultSet ← NETNUM ← pac sourceNet.
		block ← pac pupString.
		gateway ← pac sourceHost.
		(25 to: 24 + pac dataLength by: 4) do:
			[:i | net ← block ◦ i.
			count ← block ◦ (i + 3) + 1.
			count < (routingHopCount ◦ net)
			  ifTrue:
				[routingTable ◦ net ← gateway.
				routingHopCount ◦ net ← count]]].
	self freePacket: pac
! !
!RoutingUpdater methodsFor: 'undefined' stamp: ''!
timerFired | |
	self timerOn
	  ifTrue: [self completePup: outPac]
	  ifFalse: [resultSet ← true]
! !
!RoutingUpdater methodsFor: 'undefined' stamp: ''!
update | i |
	(1 to: 255) do:
		[:i | routingTable ◦ i ← 0.
		routingHopCount ◦ i ← 8].
	resultSet ← false.
	self setAddressesAndComplete: outPac.
	[resultSet] whileFalseDo:  []
! !
!RunVector methodsFor: 'undefined' stamp: ''!
length | |
	max ≡ nil
	  ifTrue: [^0].
	^max - min + 1
! !
!RunVector methodsFor: 'undefined' stamp: ''!
min | |
	^min
! !
!RunVector methodsFor: 'undefined' stamp: ''!
◦ i | index |
	index ← starts findSorted: i.
	offset ← i - (starts ◦ index) "distance into run".
	^values ◦ index
! !
!RunVector methodsFor: 'undefined' stamp: ''!
max | |
	^max
! !
!RunVector methodsFor: 'undefined' stamp: ''!
◦ i ← val | |
	offset ← 0.
	min ≡ nil
	  ifTrue:
		[min ← max ← i.
		starts ← i inVector.
		values ← val inVector]
	  ifFalse:
		[i - 1 ≠ max
		  ifTrue:
			[user notify: 'RunVectors must be loaded sequentially'.
			^val].
		max ← i.
		val = values last
		  ifTrue:
			[offset ← i - starts last.
			^val].
		starts ← {starts , i}.
		values ← {values , val}.
		^val]
! !
!SafeQ methodsFor: 'undefined' stamp: ''!
disable | |
	enabled ← false
! !
!SafeQ methodsFor: 'undefined' stamp: ''!
length | |
	^position - readposition
! !
!SafeQ methodsFor: 'undefined' stamp: ''!
status | |
	^enabled
! !
!SafeQ methodsFor: 'undefined' stamp: ''!
next← arg | i |
	 "short comment"
	enabled
	  ifTrue: [(readposition + 1 to: position) do:
			[:i | array ◦ i ≡ arg
			  ifTrue: [E notify: 'putting same guy on Q twice']]].
	arg locked
	  ifTrue: [E notify: 'putting locked Pacbuf on Q']
	  ifFalse: [super next← arg]
! !
!SafeQ methodsFor: 'undefined' stamp: ''!
enable | |
	enabled ← true
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
boxPosition← f | |
	position moveto: rect origin + (9 ⌾ (4 + ((f < 0.0
	  ifTrue: [0.0]
	  ifFalse:
		[f > 1.0
		  ifTrue: [1.0]
		  ifFalse: [f]]) * (rect height - 16)) asInteger ))
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
reposition⦂ expr | |
	self reshow⦂ [
		(expr eval.
		self boxPosition← owner scrollPos)]
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
reshow⦂ expr | r |
	r ← position inset: ¬2.
	expr eval.
	r clear: white.
	position outline
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
hidewhile⦂ expr | v |
	self hide.
	v ← expr eval.
	self show.
	^v
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
hide | |
	 "restore background"
	bitstr ≡ nil
	  ifTrue: [user notify: 'Attempt to hide unshown scrollbar']
	  ifFalse:
		[rect bitsFromString: bitstr.
		bitstr ← nil]
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
show | |
	 "Save background and turn gray"
	bitstr ← rect bitsIntoString.
	rect clear: black.
	(rect inset: 2 ⌾ 2 and: [1 ⌾ 2]) clear: white.
	position outline
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
on: frame from: t2 at: f | |
	owner ← t2.
	rect ← Rectangle new origin: frame origin - (32 ⌾ 2) extent: 32 ⌾ (frame height + 4).
	position ← Rectangle new origin: rect origin + (9 ⌾ 4) extent: 16 ⌾ 8.
	self boxPosition← f
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
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
!ScrollBar methodsFor: 'undefined' stamp: ''!
useBitmap | |
	bitstr ≡ nil
	  ifFalse: [bitstr ← bitstr asBitmap] " | x. ScrollBar allInstances transform⦂ x to⦂ x useBitmap. "
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
on: f from: o | |
	self on: f from: o at: o scrollPos
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
firsttime | |
	^rect has: user mp
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
close | |
	owner ← nil
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
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
!ScrollBar methodsFor: 'undefined' stamp: ''!
lasttime | |
	
! !
!ScrollBar methodsFor: 'undefined' stamp: ''!
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
!SelectorPane methodsFor: 'undefined' stamp: ''!
dirty | |
	^codePane dirty
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
compile: parag | |
	^organizationPane compile: parag
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
execute: parseStream for: t2 | |
	codePane ← t2.
	^codePane execute: parseStream in: false to: organizationPane syntaxChoice
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
compile: parag in: class under: heading | |
	^codePane compile: parag in: class under: heading
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
from: t1 to: t2 | |
	organizationPane ← t1.
	codePane ← t2
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
classInit | |
	editmenu ← Menu new string: 'spawn
forget'
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
selected | |
	codePane showing: (organizationPane code: list ◦ selection)
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
deselected | |
	codePane showing: organizationPane noCode
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
close | |
	organizationPane ← nil.
	super close
! !
!SelectorPane methodsFor: 'undefined' stamp: ''!
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
!Semaphore methodsFor: 'undefined' stamp: ''!
simulateSignal | |
	 self isEmpty
			ifTrue: [excessSignals ← excessSignals+1]
			ifFalse: [Processor resume: self removeFirstLink]
! !
!Semaphore methodsFor: 'undefined' stamp: ''!
wait | | <primitive: 69>
	user croak
! !
!Semaphore methodsFor: 'undefined' stamp: ''!
signal | | <primitive: 68>
	user croak
! !
!Semaphore methodsFor: 'undefined' stamp: ''!
simulateWait | |
	 excessSignals>0
			ifTrue: [excessSignals ← excessSignals-1]
			ifFalse: [self addLastLink: Processor suspend]
! !
!Semaphore methodsFor: 'undefined' stamp: ''!
init | |
	 super init.
	 excessSignals ← 0
! !
!Set methodsFor: 'undefined' stamp: ''!
summation | i summation |
	summation ← 0.0 "sum of the values in the set".
	(1 to: position) do: [:i | summation ← summation + (self ◦ i)].
	^summation
! !
!Set methodsFor: 'undefined' stamp: ''!
dotproduct: s | i dotproduct |
	dotproduct ← 0.0 "dot product of two sets ... sets must be of equal length".
	self length = s length
	  ifTrue:
		[(1 to: position) do: [:i | dotproduct ← dotproduct + (s ◦ i * (self ◦ i))].
		^dotproduct].
	user notify: 'dot product undefined...sets are not of equal length'
! !
!Set methodsFor: 'undefined' stamp: ''!
product: s | product i |
	product ← Set new "product of two sets ... sets must be of equal length" default.
	self length = s length
	  ifTrue:
		[(1 to: position) do: [:i | product add: s ◦ i * (self ◦ i)].
		^product].
	user notify: 'product undefined...sets are not of equal length'
! !
!Set methodsFor: 'undefined' stamp: ''!
deleteI: i to: j | n k |
	n ← j - i + 1.
	(i to: position - n) do: [:k | array ◦ k ← array ◦ (k + n)].
	(position - n + 1 to: position) do: [:k | array ◦ k ← nil].
	position ← position - n
! !
!Set methodsFor: 'undefined' stamp: ''!
insertI: i value: v | old j |
	i > position
	  ifTrue: [self next← v]
	  ifFalse:
		[old ← array.
		position = limit
		  ifTrue:
			[limit ← limit + (10 max: limit / 4).
			array ← array species new: limit.
			(1 to: i - 1) do: [:j | array ◦ j ← old ◦ j]].
		(position to: i by: ¬1) do: [:j | array ◦ (j + 1) ← old ◦ j].
		array ◦ i ← v.
		position ← position + 1]
! !
!Set methodsFor: 'undefined' stamp: ''!
deleteI: i | v j |
	v ← self ◦ i.
	(i to: position - 1) do: [:j | array ◦ j ← array ◦ (j + 1)].
	array ◦ position ← nil.
	position ← position - 1.
	^v
! !
!Set methodsFor: 'undefined' stamp: ''!
◦ i | |
	^array ◦ i "self checkIndex:"
! !
!Set methodsFor: 'undefined' stamp: ''!
◦ i ← val | |
	position + 1 = i
	  ifTrue: [self next← val]
	  ifFalse: [^array ◦ i ← val "self checkIndex:"]
! !
!Set methodsFor: 'undefined' stamp: ''!
string: t1 | |
	limit ← t1.
	self of: (String new: limit)
! !
!Set methodsFor: 'undefined' stamp: ''!
vector: t1 | |
	limit ← t1.
	self of: (Vector new: limit)
! !
!Set methodsFor: 'undefined' stamp: ''!
default | |
	self vector: 8
! !
!Set methodsFor: 'undefined' stamp: ''!
of: t1 to: t2 | |
	array ← t1.
	position ← t2.
	limit ← array length
! !
!Set methodsFor: 'undefined' stamp: ''!
grow | |
	^self growby: (10 max: limit / 4 "self grown and reset. returns another Set with old contents")
! !
!Set methodsFor: 'undefined' stamp: ''!
pastend← x | |
	^self append: self grow.
	self next← x
! !
!Set methodsFor: 'undefined' stamp: ''!
checkIndex: i | |
	(i ≥ 1 and: [i ≤ position])
	  ifTrue: [^i].
	^user notify: 'illegal index'
! !
!Set methodsFor: 'undefined' stamp: ''!
next | |
	user notify: 'no direct reading of a Set'
! !
!Set methodsFor: 'undefined' stamp: ''!
growby: n | old |
	old ← Set new "grow and reset self. return old Set for copying" of: array to: position.
	self of: (array species new: limit + n) to: 0.
	^old
! !
!Set methodsFor: 'undefined' stamp: ''!
delete: x | i |
	(1 to: position) do:
		[:i | array ◦ i ≡ x
		  ifTrue: [^self deleteI: i]].
	^false
! !
!Set methodsFor: 'undefined' stamp: ''!
find: v | i |
	(1 to: position) do:
		[:i | array ◦ i = v
		  ifTrue: [^i]].
	^0
! !
!Set methodsFor: 'undefined' stamp: ''!
insert: x | |
	(self find: x) = 0
	  ifTrue: [self next← x]
! !
!Set methodsFor: 'undefined' stamp: ''!
has: x | |
	^(self find: x) > 0
! !
!Set methodsFor: 'undefined' stamp: ''!
append: x | |
	x do: [:x | self next← x]
! !
!Set methodsFor: 'undefined' stamp: ''!
add: x | |
	self next← x
! !
!Set methodsFor: 'undefined' stamp: ''!
printOn: strm | t |
	strm append: 'a Set: '.
	(array is: String)
	  ifTrue: [strm append: self]
	  ifFalse: [self do:
			[:t | strm space.
			strm print: t]]
! !
!Set methodsFor: 'undefined' stamp: ''!
viewer | |
	^SetReader new of: array from: 1 to: position "self viewRange: 1 to: position"
! !
!Set methodsFor: 'undefined' stamp: ''!
notViewed: v | |
	views delete: v.
	views empty
	  ifTrue: [views ← nil]
! !
!Set methodsFor: 'undefined' stamp: ''!
initView: v | |
	^v of: array to: position
! !
!Set methodsFor: 'undefined' stamp: ''!
viewRange: i to: j | |
	^SetReader new "self viewer:" of: array from: i to: j "max: 1" "min: position"
! !
!Set methodsFor: 'undefined' stamp: ''!
asStream | |
	^self viewer
! !
!Set methodsFor: 'undefined' stamp: ''!
species | |
	^array species
! !
!Set methodsFor: 'undefined' stamp: ''!
copy | |
	^self viewer copy
! !
!Set methodsFor: 'undefined' stamp: ''!
length | |
	^position
! !
!Set methodsFor: 'undefined' stamp: ''!
asSet | |
	
! !
!Set methodsFor: 'undefined' stamp: ''!
viewer: v | |
	views ≡ nil
	  ifTrue: [views ← Set default].
	views next← v
! !
!SetReader methodsFor: 'undefined' stamp: ''!
of: t1 from: t2 for: n | |
	array ← t1.
	position ← t2.
	position ← position - 1.
	limit ← position + n
! !
!SetReader methodsFor: 'undefined' stamp: ''!
copy | t1 |
	 "yield contents all at once as a Set"
	^(t1 ← Set new) of: (array species new: limit - position).
	t1 append: self
! !
!SetReader methodsFor: 'undefined' stamp: ''!
length | |
	^limit - position "how much left"
! !
!SetReader methodsFor: 'undefined' stamp: ''!
asSet | |
	^self copy
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
printOn: aStream | |
	self < 0
	  ifTrue:
		[self = self minVal
		  ifTrue: [aStream append: (NoteTaker
			  ifTrue: ['-16384']
			  ifFalse: ['-32768'])]
		  ifFalse:
			[aStream append: '-'.
			self negated printOn: aStream base: 10]]
	  ifFalse: [self printOn: aStream base: 10]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
cansubscript: a | |
	^self ≥ 1 and: [self ≤ a length]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asInteger | |
	^self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
intdiv: arg | |
	(arg is: SmallInteger)
	  ifTrue: [^{self / arg , (self \ arg)}].
	(arg is: LargeInteger)
	  ifTrue: [^self asLarge intdiv: arg].
	user notify: 'I give up'
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
eightbitAdd: aNumber | |
	 "treat numbers as unsigned 8-bit quantities. (for Natural)"
	^self + aNumber land: 255
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
bytes | |
	^self "behave like a LargeInteger - negative integers behave like positive naturals"
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
subscripts: a ← val | t |
	(self cansubscript: a)
	  ifTrue:
		[((a class isBytes) and: [t ← val asInteger.  (t<0 or: [t>255])])
		  ifTrue: [user notify: 'Improper store into String']
		  ifFalse: [^a ◦ self ← t]]
	  ifFalse: [user notify: 'Subscript out of bounds: ' + self asString]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
bits: left to: right | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width < 15
			  ifTrue: [biton ◦ (width + 1) - 1]
			  ifFalse:
				[width = 15
				  ifTrue: [32767]
				  ifFalse: [¬1]]).
	^(self lshift: right - 15) land: mask
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
subscripts: a | |
	(self cansubscript: a)
	  ifTrue: [^a ◦ self].
	user notify: 'Subscript out of bounds: ' + self asString
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asDigit | |
	"Convert to digit 0-9 or A-Z"
	^self + (self>9 ifTrue: [55] ifFalse: [48])
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
unsignedlessthan: y | |
	^self eightbitLessThan: y
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
nomask: b | |
	^0 = (self land: b)
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
isalphanumeric | |
	self isletter
	  ifTrue: [^true].
	 "lower-case"
	^self isdigit
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
even | |
	^(self land: 1) = 0
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
> aNumber | | "primitive"
	^self retry: #> coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
instfield: i | |
	 "small integer gives trouble"
	i = 1
	  ifTrue: [^self].
	user notify: 'arg too big'
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
allmask: b | |
	^b = (self land: b)
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
hash | |
	 "used to find large integers in dictionaries"
	^self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
≤ aNumber | | "primitive"
	^self retry: #≤ coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
recopy | |
	^self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
quo: aNumber | |
	(aNumber moreGeneralThan: self)
	  ifTrue: [^self retry: #quo: coercing: aNumber].
	^self / aNumber  "invoke integer divide primitive"
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
inUniqueString | |
	self < 128
	  ifTrue: [^UST1 ◦ (self + 1)].
	^self inString unique
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
coerce: n | | ^n asInteger
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
compare: arg | |
	(arg is: SmallInteger)
	  ifTrue:
		[self < arg
		  ifTrue: [^1].
		self = arg
		  ifTrue: [^2].
		^3]
	  ifFalse: [^self natcompare: arg bytes "4 - (arg bytes natcompare: self)"]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
minVal | |
	NoteTaker
	  ifTrue: [^¬16384].
	^¬32768
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asInt16 | |
	
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
natnormalize: n | x i r f digit |
	r ← Natural new: self length "n is the number of bits to shift by. The Natural number returned will be written over repeatedly, so we must make a new one." + 1.
	x ← 0.
	f ← n - 8.
	(1 to: r length) do:
		[:i | digit ← self ◦ i.
		r ◦ i ← ((digit lshift: n) lor: x) land: 255.
		x ← digit lshift: f].
	^r
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
field: spec | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: right - (spec / 16) + 1 to: right
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
copy | |
	^self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asSmall | |
	
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
length | |
	(self ≥ 256 "behave like a Natural" or: [self ≤ ¬256])
	  ifTrue: [^2].
	^1
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
unsignedadd: y | |
	^self eightbitAdd: y
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asOop | |
	NoteTaker
	  ifTrue:
		[self ≥ 0
		  ifTrue: [^32768 + self].
		 "0...16K-1 --> 32K...48K-1"
		^65536 + self]
	  ifFalse: [ "-16K...-1 --> 48K...64K-1"
		^super asOop]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
fromString: str radix: radix | maxdigit c val i |
	maxdigit ← radix + (radix > 10
			  ifTrue: [55 - 1]
			  ifFalse: [48 - 1]).
	val ← 0.
	(1 to: str length) do:
		[:i | c ← str ◦ i.
		(c < 48 or: [c > maxdigit])
		  ifTrue: [^false].
		val ← val * radix + (c ≤ 57
				  ifTrue: [c - 48]
				  ifFalse:
					[c < 65
					  ifTrue: [^false].
					c - 55])].
	^val
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asLarge | me digits |
	 "convert to LargeInteger"
	me ← self bytes.
	digits ← Natural new: me length.
	digits ◦ 1 ← me ◦ 1.
	digits length = 2
	  ifTrue: [digits ◦ 2 ← me ◦ 2].
	^LargeInteger new bytes: digits neg: self neg
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
≠ aNumber | | "primitive"
	aNumber isNumber ifFalse: [^true].
	^self retry: #≠ coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
\ arg | | "primitive" <primitive: 11>
	 "mod"
	0 = arg
	  ifTrue: [user notify: 'Attempt to divide by 0']
	  ifFalse:
		[arg isLarge
		  ifTrue: [^self asLarge \ arg].
		^self \ arg asInteger]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
maxVal | |
	NoteTaker
	  ifTrue: [^16383].
	^32767
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
tokenish | |
	 "test for token-chars"
	self isletter
	  ifTrue: [^true].
	 "lower-case"
	self isdigit
	  ifTrue: [^true].
	 "digits"
	^'¬.:⦂' has: self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
inString | t |
	t ← String new: 1.
	t ◦ 1 ← self.
	^t
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
classInit | |
	 "Initialize the digit buffer"
	digitbuffer ← String new: 16
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asNatural | t |
	t ← Natural new: self length.
	t ◦ 1 ← self ◦ 1.
	t length > 1
	  ifTrue: [t ◦ 2 ← self ◦ 2].
	^t
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
= aNumber | | "primitive"
	aNumber isNumber ifFalse: [^false].
	^self retry: #= coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
hibit | i |
	(1 to: 16) do:
		[:i | (self land: biton ◦ (17 - i)) ≠ 0
		  ifTrue: [^17 - i]].
	^0
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
odd | |
	^(self land: 1) = 1
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
// aNumber | d | "div"
	d ← self quo: aNumber.  "temporary crock"
	(self\\aNumber) = 0
	  ifTrue: [^d].
	(self < 0) ~= (aNumber < 0)
	  ifTrue: [^d - 1].
	^d
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
bits: left to: right ← val | width mask |
	 "  0 is leftmost bit, 15 is rightmost"
	width ← right - left + 1.
	mask ← (width < 15
			  ifTrue: [biton ◦ (width + 1) - 1]
			  ifFalse:
				[width = 15
				  ifTrue: [32767]
				  ifFalse: [¬1]]).
	^(self land: ((mask lshift: 15 - right) lxor: ¬1)) lor: ((val land: mask) lshift: 15 - right)
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
◦ n | t |
	n = 1 "behave like a Natural"
	  ifTrue:
		[self < 0
		  ifTrue: [^((self land: 255) lxor: 255) + 1 land: 255].
		^self land: 255]
	  ifFalse:
		[n = 2
		  ifTrue:
			[self < 0
			  ifTrue:
				[t ← (self lshift: ¬8) lxor: 255.
				(self land: 255) = 0
				  ifTrue: [^t + 1 land: 255].
				^t]
			  ifFalse: [^self lshift: ¬8]]
		  ifFalse: [^0]]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
/ arg | | "primitive" <primitive: 10>
	0 = arg
	  ifTrue: [user notify: 'Attempt to divide by 0']
	  ifFalse:
		[arg isLarge
		  ifTrue: [^self asLarge / arg].
		^self / arg asInteger]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
isDigitRadix: radix | |
	^(self - (self ≥ 65 ifTrue: [55] ifFalse: [48]) between: 0 and: radix-1)
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
generality | | ^ 20
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
isdigit | |
	self ≥ 48
	  ifTrue: [ " 0 "
		^self ≤ 57].
	 " 9 "
	^false
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
anymask: b | |
	^0 ≠ (self land: b)
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
isletter | |
	self ≥ 97
	  ifTrue: [ " a "
		^self ≤ 122].
	 " z "
	self ≥ 65
	  ifTrue: [ " A "
		^self ≤ 90].
	 " Z "
	^false
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
last | |
	^self ◦ self length
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
≥ aNumber | | "primitive"
	^self retry: #≥ coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
elementStream | |
	^Stream new of: (Vector new: self)
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
- aNumber | | "primitive"
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge - aNumber].  "overflow"
	^self retry: #- coercing: aNumber 
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
natcompare: arg | i len t4 t5 |
	len ← self length.
	(t4 ← arg length) < len
	  ifTrue: [^3].
	t4 > len
	  ifTrue: [^1].
	(len to: 1 by: ¬1) do:
		[:i | (t5 ← self ◦ i) > (arg ◦ i)
		  ifTrue: [^3].
		t5 < (arg ◦ i)
		  ifTrue: [^1]].
	^2
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
field: spec ← val | right |
	 "spec=width*16 + right (15=left, 0=right)"
	right ← 15 - (spec land: 15).
	^self bits: (right - (spec / 16) + 1) to: right ← val
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
+ aNumber | | "primitive"
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge + aNumber].  "overflow"
	^self retry: #+ coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
lxor: arg | |<primitive: 14>
	^arg lxor: self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asObjectNoFail | |<primitive: 40>
	^#VanishedObject
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
* aNumber | | "primitive" <primitive: 9>
	(aNumber is: SmallInteger)
	  ifTrue: [^self asLarge * aNumber].  "overflow"
	^self retry: #* coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
printOn: aStream base: b | i j x |
	(x ← self) < 0
	  ifTrue:
		[i ← 1.
		digitbuffer ◦ 1 ← 16384 \ b * 2 + self - ¬32768 \ b "Im trying to print 16-bit nos.".
		x ← 16384 / b * 2 + (self - ¬32768 / b)]
	  ifFalse: [i ← 0].
	[x ≥ b] whileTrueDo:
		[digitbuffer ◦ (i ← i + 1) ← x \ b.
		x ← x / b].
	digitbuffer ◦ (i ← i + 1) ← x.
	[i > 0] whileTrueDo:
		[x ← digitbuffer ◦ i.
		aStream next← (x < 10
		  ifTrue: [48 + x]
		  ifFalse: [55 + x]).
		i ← i - 1]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
\\ aNumber | m |
	m ← self\aNumber.  "invoke integer modulo primitive"
	m = 0
	  ifTrue: [^m].
	(self < 0) ~= (aNumber < 0)
	  ifTrue: [^m - aNumber].
	^m
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
compareChar: c | a |
	a ← self "⇑self asLowercase compare: c asLowercase" "written in-line for speed".
	65 ≤ a
	  ifTrue:
		[a ≤ 90
		  ifTrue: [a ← a + 32]].
	65 ≤ c
	  ifTrue:
		[c ≤ 90
		  ifTrue: [c ← c + 32]].
	a < c
	  ifTrue: [^1].
	a = c
	  ifTrue: [^2].
	^3
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
< aNumber | | "primitive"
	^self retry: #< coercing: aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asLowercase | |
	65 ≤ self
	  ifTrue:
		[self ≤ 90
		  ifTrue: [^self + 32]]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asObject | |<primitive: 40>
	user notify: 'This object does not exist!'
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
hex4 | |
	^self base: 16
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
lor: arg | |<primitive: 16>
	^arg lor: self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
land: arg | |<primitive: 15>
	^arg land: self
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asUppercase | |
	97 ≤ self
	  ifTrue:
		[self ≤ 122
		  ifTrue: [^self - 32]]
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
asFloat | |<primitive: 62>
	user croak
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
eightbitLessThan: aNumber | |
	 "treat numbers as unsigned 8-bit quantities."
	^self < aNumber
! !
!SmallInteger methodsFor: 'undefined' stamp: ''!
lshift: arg | |<primitive: 13>
	arg class ≡ SmallInteger
	  ifTrue: [^self asLarge lshift: arg].
	^self lshift: arg asSmall
! !
!Socket methodsFor: 'undefined' stamp: ''!
freePacket: p | |
	 "put a used packet into free queue"
	(freeQ and: [p])
	  ifTrue: [freeQ next← p].
	^false
! !
!Socket methodsFor: 'undefined' stamp: ''!
freePacket | p |
	freeQ
	  ifTrue:
		[ "get a packet"
		(p ← freeQ next)
		  ifTrue: [^p].
		user show: 'Warning, empty freeQ, in Socket'.
		^false]
	  ifFalse: [^Pacbuf new init]
! !
!Socket methodsFor: 'undefined' stamp: ''!
close | |
	self release.
	(sockeTable lookup: lclSocNum)
	  ifTrue: [sockeTable delete: lclSocNum]
! !
!Socket methodsFor: 'undefined' stamp: ''!
disable | |
	user show: 'unnecessary disable' "left for compatibility".
	self close
! !
!Socket methodsFor: 'undefined' stamp: ''!
lclSocNum← t1 | |
	lclSocNum ← t1.
	^lclSocNum
! !
!Socket methodsFor: 'undefined' stamp: ''!
lclSocNum | |
	^lclSocNum
! !
!Socket methodsFor: 'undefined' stamp: ''!
frnSocNum← t1 | |
	frnSocNum ← t1.
	^frnSocNum
! !
!Socket methodsFor: 'undefined' stamp: ''!
frnSocNum | |
	^frnSocNum
! !
!Socket methodsFor: 'undefined' stamp: ''!
filterInput← t1 | |
	filterInput ← t1.
	^filterInput
! !
!Socket methodsFor: 'undefined' stamp: ''!
filterInput | |
	^filterInput
! !
!Socket methodsFor: 'undefined' stamp: ''!
frnNet← t1 | |
	frnNet ← t1.
	^frnNet
! !
!Socket methodsFor: 'undefined' stamp: ''!
frnHost | |
	^frnHost
! !
!Socket methodsFor: 'undefined' stamp: ''!
enable | |
	user show: 'someone did unnecessary enable' "now a no-op".
	self print
! !
!Socket methodsFor: 'undefined' stamp: ''!
computeOutgoingCS← t1 | |
	computeOutgoingCS ← t1.
	^computeOutgoingCS
! !
!Socket methodsFor: 'undefined' stamp: ''!
computeOutgoingCS | |
	^computeOutgoingCS
! !
!Socket methodsFor: 'undefined' stamp: ''!
frnHost← t1 | |
	frnHost ← t1.
	^frnHost
! !
!Socket methodsFor: 'undefined' stamp: ''!
frnNet | |
	^frnNet
! !
!Socket methodsFor: 'undefined' stamp: ''!
setOutAddBlock | |
	outAddBlock ◦ 1 ← frnNet.
	outAddBlock ◦ 2 ← frnHost.
	outAddBlock doubleWordAt: 3 ← frnSocNum.  "words 2, 3"
	outAddBlock ◦ 7 ← NETNUM.
	outAddBlock ◦ 8 ← ALTONUM.
	outAddBlock doubleWordAt: 9 ← lclSocNum high  "words 5, 6"
! !
!Socket methodsFor: 'undefined' stamp: ''!
default | |
	self net: 0 host: 0 soc: 0 "default local socket number and leave frn port open"
! !
!Socket methodsFor: 'undefined' stamp: ''!
from: t1 | |
	self from: t1 net: 0 host: 0 soc: 0 "set lcl soc number, leave frnPort open -- useful for creating
	a well-known socket as a listener"
! !
!Socket methodsFor: 'undefined' stamp: ''!
net: t1 host: t2 soc: t3 | |
	"default the local socket number:
		use some memory dependent info (space) for the high word so that no two
			sockets (instances) can be the same, also non-zero.
		use time for low word, so that same instance will not usually have the
			same socket number (odds = 1/65536)"
	self from: ((self asOop land: 8r77777) * 65536 + user ticks) net: t1 host: t2 soc: t3
! !
!Socket methodsFor: 'undefined' stamp: ''!
to: h | |
	self net: NETNUM host: h "convenient default if on my net"
! !
!Socket methodsFor: 'undefined' stamp: ''!
hostName: name | a nh |
	a ← NameUser init "lookup name, then set net and host numbers (maybe socket?)".
	nh ← a getAddressBlock: name "since this socket may get many responses,
	make sure socket is not half deleted from sockeTable after first response".
	Top critical⦂ [(a close)].
	nh
	  ifTrue: [self net: nh ◦ 1 host: nh ◦ 2]
	  ifFalse: [ "invalid name?"
		^false]
! !
!Socket methodsFor: 'undefined' stamp: ''!
wakeup | |
	 "when E goes from ethAsleep to ethAwak"
	
! !
!Socket methodsFor: 'undefined' stamp: ''!
from: t1 net: t2 host: t3 soc: t4 | |
	lclSocNum ← t1.
	frnNet ← t2.
	frnHost ← t3.
	frnSocNum ← t4.
	outAddBlock ← String new: 12 "this is the most general initialization, both lcl soc# and frnPort given".
	self setOutAddBlock.
	computeOutgoingCS ← filterInput ← false.
	sockeTable insert: lclSocNum with: self "put me in socket table".
	self doMoreInit
! !
!Socket methodsFor: 'undefined' stamp: ''!
kill | |
	 "whole world about to go.  I don't care, but my subclasses might"
! !
!Socket methodsFor: 'undefined' stamp: ''!
socDispose: Ipac | |
	self freePacket: Ipac
! !
!Socket methodsFor: 'undefined' stamp: ''!
sleep | |
	 "the user is quitting.  I don't care, but my subclasses might"
! !
!Socket methodsFor: 'undefined' stamp: ''!
release | |
	 "disable Timers, undo circular structures etc."
! !
!Socket methodsFor: 'undefined' stamp: ''!
doMoreInit | |
	
! !
!Socket methodsFor: 'undefined' stamp: ''!
socProcess: Ipac | |
	self freePacket: Ipac
! !
!Socket methodsFor: 'undefined' stamp: ''!
acceptPacbuf: Ipac | temp |
	(filterInput and: [(frnNet ≠ Ipac sourceNet "if we get here, we know that the input distributer has verified the
	PUP dest as being us (or a broadcast, if broadcast filter is off).
	We do not have responsibility for verifying incoming checksum.
	First, check if we've been asked to filter by source:" or: [(frnHost ≠ Ipac sourceHost or: [frnSocNum ≠ Ipac sourceSocNum])])])
	  ifTrue: [^self socDispose: Ipac].
	 "It's good, take it..."
	^self socProcess: Ipac
! !
!Socket methodsFor: 'undefined' stamp: ''!
broadcast: packet to: t2 | |
	 "I want to broadcast this packet"
	socNumber ← t2.
	self setAddresses: packet.
	packet destHost← 0.
	packet destNet← 0.
	packet destSocNum← socNumber "I assume that the length and type have been done".
	self completePup: packet
! !
!Socket methodsFor: 'undefined' stamp: ''!
setAddresses: pac | |
	pac addressBlock← outAddBlock
! !
!Socket methodsFor: 'undefined' stamp: ''!
completePup: pac | t |
	 "the user must have set all 6 address fields,ID, length, and type" "Now route the packet appropriately, assuming we have Ethernet..."
	NETNUM = pac destNet
	  ifTrue: [pac imEthDestHost← pac destHost]
	  ifFalse:
		[ "most common case"
		0 = pac destNet
		  ifTrue: [pac imEthDestHost← 0]
		  ifFalse:
			[ "broadcast"
			0 = (t ← routingTable ◦ pac destNet)
			  ifTrue:
				[user show: '
Inaccessible destination net: ' + pac destNet asString + ', packet not sent.'.
				^pac].
			.
			pac imEthDestHost← t]].
	pac imEthSrcHost← ALTONUM.
	pac ethType← 512.
	pac transportControl← 0 "as a socket we have an option about computing outgoing checksums".
	pac checksum← (computeOutgoingCS
	  ifTrue: [pac doChecksum]
	  ifFalse: [¬1]) "Fix this up later......".
	E sendOutput: pac.
	^pac
! !
!Socket methodsFor: 'undefined' stamp: ''!
defaultAndComplete: pac | |
	self defaultAddresses: pac.
	self completePup: pac
! !
!Socket methodsFor: 'undefined' stamp: ''!
defaultAddresses: pac | |
	 "overwrites any fields which are 0"
	pac destNet = 0
	  ifTrue: [pac destNet← frnNet].
	pac destHost = 0
	  ifTrue: [pac destHost← frnHost].
	(pac destSoc0 = 0 and: [pac destSoc1 = 0])
	  ifTrue: [pac destSocNum← frnSocNum].
	pac sourceNet = 0
	  ifTrue: [pac sourceNet← NETNUM].
	pac sourceHost = 0
	  ifTrue: [pac sourceHost← ALTONUM].
	(pac sourceSoc0 = 0 and: [pac sourceSoc1 = 0])
	  ifTrue: [pac sourceSocNum← lclSocNum]
! !
!Socket methodsFor: 'undefined' stamp: ''!
setAddressesAndComplete: pac | |
	pac addressBlock← outAddBlock.
	self completePup: pac
! !
!StackPane methodsFor: 'undefined' stamp: ''!
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
!StackPane methodsFor: 'undefined' stamp: ''!
context: t1 at: level instance: t3 code: t4 | |
	contextVarPane ← t1.
	instanceVarPane ← t3.
	codePane ← t4.
	variables ← (Vector new: 16) asStream.
	proceed ≡ nil
	  ifTrue: [proceed ← {false , nil , level}]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
classInit | |
	stackmenu ← Menu new string: 'stack
spawn
proceed
restart'
! !
!StackPane methodsFor: 'undefined' stamp: ''!
interrupt: flag | |
	proceed ◦ 1 ← flag
! !
!StackPane methodsFor: 'undefined' stamp: ''!
selected | context instance code safeVec |
	contextVarPane ifFalse: [^self].
	context ← list ◦ selection.
	instance ← context receiver.
	code ← self code.
	codePane showing: (code ifTrue: [code] ifFalse: ['']).
	code
	  ifTrue:
		[contextVarPane names: (#(thisContext ) concat: context tempNames)
			values: {context , context tempframe} wrt: context.
		context tempframe ≡ nil ifTrue: [user notify: 'NIL TEMPFRAME']]
	  ifFalse: [contextVarPane names: #(thisContext ) values: context inVector wrt: context].
	safeVec ← Vector new: 2.
	safeVec all← instance.
	instanceVarPane names: (#(self ) concat: instance class instvars) values: safeVec wrt: context.
	contextVarPane select: 1
! !
!StackPane methodsFor: 'undefined' stamp: ''!
deselected | |
	contextVarPane ≡ false
	  ifFalse:
		[codePane showing: ''.
		contextVarPane names: (Vector new: 0) values: #(nil ) wrt: false.
		instanceVarPane names: (Vector new: 0) values: #(nil ) wrt: false]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
locked | |
	^contextVarPane and: [(selection > 0 and: [self dirty])]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
compile: parseStream | ctxt selector method mcl |
	ctxt ← list ◦ (selection max: 1).
	mcl ← ctxt mclass.
	proceed ◦ 2 ← selector ← codePane compile: parseStream in: mcl under: 'As yet unclassified'
	  ifTrue:
		[(codePane reflects: selection)
		  ifTrue:
			[(method ← mcl md methodorfalse: selector)
			  ifTrue:
				[self releaseAboveSelection.
				ctxt restartWith: method.
				proceed ◦ 1 ← true.
				self of: list ◦ (selection to: list length) copy.
				self select: 1]]]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
execute: parseStream for: t2 | |
	codePane ← t2.
	^proceed ◦ 2 ← codePane execute: parseStream in: (selection = 0
	  ifTrue: [false]
	  ifFalse: [list ◦ selection]) to: nil
! !
!StackPane methodsFor: 'undefined' stamp: ''!
dirty | |
	^codePane and: [codePane dirty]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
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
!StackPane methodsFor: 'undefined' stamp: ''!
code | mclass selector |
	 "code of my selected context"
	mclass ← (list ◦ selection) mclass.
	selector ← self selector.
	^(mclass canunderstand: selector)
		ifTrue: [mclass code: selector]
		ifFalse: [(LADDecompiler new decompile: selector in: mclass method: (list◦selection) method) asParagraph makeBoldPattern]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
separator: c | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'undefined' stamp: ''!
identifier: s | |
	 "called by selected via Class fieldNamesInto"
	variables next← s
! !
!StackPane methodsFor: 'undefined' stamp: ''!
releaseAboveSelection | |
	selection > 1
	  ifTrue:
		[list ◦ (selection - 1) sender← nil.
		(list ◦ 1) release "Fully"].
	NoteTaker
	  ifFalse: [ "??"
		(list ◦ (selection max: 1)) verifyFrames]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
selector | context |
	context ← list ◦ (selection max: 1).
	^context sender ≡ nil
	  ifTrue: [false]
	  ifFalse: [context selector]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
trailer: s | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'undefined' stamp: ''!
comment: s | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'undefined' stamp: ''!
notify: msg | |
	 "selected context doesnt know its variables"
! !
!StackPane methodsFor: 'undefined' stamp: ''!
declaration: dummy1 name: string asArg: dummy2 | |
	variables next← string
! !
!StackPane methodsFor: 'undefined' stamp: ''!
contents | |
	 "called by selected via Class fieldNamesInto"
! !
!StackPane methodsFor: 'undefined' stamp: ''!
terminate | |
	 "called by parser close during initialization"
! !
!StackPane methodsFor: 'undefined' stamp: ''!
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
!StackPane methodsFor: 'undefined' stamp: ''!
close | |
	NoteTaker
	  ifTrue: [super close]
	  ifFalse:
		[Top enable: proceed ◦ 3.
		super close.
		list
		  ifTrue: [(list ◦ 1) releaseFully]]
! !
!StackPane methodsFor: 'undefined' stamp: ''!
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
!Stream methodsFor: 'undefined' stamp: ''!
do: aBlock | |
	[self end] whileFalseDo:  [aBlock value: self next].
	^nil
! !
!Stream methodsFor: 'undefined' stamp: ''!
reverseContents | a s i |
	a ← array species new: position.
	s ← a asStream.
	(position to: 1 by: ¬1) do: [:i | s next← array ◦ i].
	^a
! !
!Stream methodsFor: 'undefined' stamp: ''!
print: obj | |
	obj printOn: self
! !
!Stream methodsFor: 'undefined' stamp: ''!
crtab: n | i |
	self next← 13.
	(1 to: n) do: [:i | self next← 9]
! !
!Stream methodsFor: 'undefined' stamp: ''!
space | |
	self next← 32
! !
!Stream methodsFor: 'undefined' stamp: ''!
semicrtab | |
	self append: ';
	'
! !
!Stream methodsFor: 'undefined' stamp: ''!
tab | |
	self next← 9
! !
!Stream methodsFor: 'undefined' stamp: ''!
cr | |
	self next← 13
! !
!Stream methodsFor: 'undefined' stamp: ''!
asReadStream | |
	^Stream new "an alternative to Set/SetReader.
	create another Stream which reads the contents of this one" of: array from: 1 to: position
! !
!Stream methodsFor: 'undefined' stamp: ''!
viewer | |
	^SetReader new of: array from: 1 to: position
! !
!Stream methodsFor: 'undefined' stamp: ''!
asArray | |
	^array
! !
!Stream methodsFor: 'undefined' stamp: ''!
asStream | |
	
! !
!Stream methodsFor: 'undefined' stamp: ''!
asVector | |
	 "Convert a string to a vector of tokens"
	^(Reader new of: self) read
! !
!Stream methodsFor: 'undefined' stamp: ''!
emitLong: jmpOrBfp by: dist | |
	dist < 0
	  ifTrue: [dist ← dist + 1024]
	  ifFalse:
		[dist > 1023
		  ifTrue: [dist ← ¬1]
		  ifFalse: [jmpOrBfp ← jmpOrBfp + 4]].
	dist < 0
	  ifTrue: [user notify: 'A block compiles more than 1K bytes of code']
	  ifFalse:
		[self next← dist / 256 + jmpOrBfp.
		self next← dist \ 256]
! !
!Stream methodsFor: 'undefined' stamp: ''!
release | |
	array ← nil
! !
!Stream methodsFor: 'undefined' stamp: ''!
close | |
	limit ← position.
	position ← 0
! !
!Stream methodsFor: 'undefined' stamp: ''!
of: t1 | |
	array ← t1.
	position ← 0.
	limit ← array length
! !
!Stream methodsFor: 'undefined' stamp: ''!
default | |
	self of: (String new: 16)
! !
!Stream methodsFor: 'undefined' stamp: ''!
of: t1 from: pos to: lim | len |
	array ← t1.
	limit ← (lim > (len ← array length)
			  ifTrue: [len]
			  ifFalse: [lim]).
	position ← (pos ≤ 1
			  ifTrue: [0]
			  ifFalse: [pos - 1])
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextword | hi lo |
	(hi ← self next)
	  ifTrue:
		[(lo ← self next)
		  ifTrue: [^(hi*256) + lo].
		^false]
	  ifFalse: [^false]
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextPoint← p | |
	self nextword← p x.
	self nextword← p y
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextPoint | x |
	x ← self nextword.
	^Point new x: x y: self nextword
! !
!Stream methodsFor: 'undefined' stamp: ''!
padNext | |
	position even "make position even (on word boundary), returning padding character if any"
	  ifTrue: [^false].
	^self next
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextElement: element | |
	self next← element
! !
!Stream methodsFor: 'undefined' stamp: ''!
into: x endError: err | i t len |
	i ← 0 "Array result".
	len ← x length "read until count or stream is exhausted".
	[i < len and: [(t ← self next)]] whileTrueDo: [x ◦ (i ← i + 1) ← t].
	err
	  ifTrue:
		[t
		  ifTrue: [^x].
		user notify: 'only read first ' + i asString]
	  ifFalse: [ "return number that were read"
		^i]
! !
!Stream methodsFor: 'undefined' stamp: ''!
next: n ← v | |
	(1 to: n) do: [:n | self next← v]
! !
!Stream methodsFor: 'undefined' stamp: ''!
next: n | |
	^self into: (array species new: n) endError: true
! !
!Stream methodsFor: 'undefined' stamp: ''!
next← x | |<primitive: 21>
	 "simple arg"
	self myend
	  ifTrue: [^self pastend← x].
	^array ◦ (position ← position + 1) ← x
! !
!Stream methodsFor: 'undefined' stamp: ''!
dequeue | |
	 "use it as a FIFO"
	^self dequeue: 1
! !
!Stream methodsFor: 'undefined' stamp: ''!
dequeue: n | t |
	position < n
	  ifTrue: [^false].
	t ← (array ◦ (1 to: n)) copy.
	array ◦ (1 to: position - n) ← array ◦ (n + 1 to: position).
	position ← position - n.
	^t
! !
!Stream methodsFor: 'undefined' stamp: ''!
upto: x | y s |
	s ← (String new: 250) asStream.
	self do:
		[:y | y = x
		  ifTrue: [^s contents].
		s next← y].
	^s contents
! !
!Stream methodsFor: 'undefined' stamp: ''!
into: x | |
	^self into: x endError: true "generate an error if the Stream is exhausted before x is filled"
! !
!Stream methodsFor: 'undefined' stamp: ''!
peek | x |
	(x ← self next)
	  ifTrue:
		[position ← position - 1.
		^x].
	 "peek at next element"
	^false
! !
!Stream methodsFor: 'undefined' stamp: ''!
next | |<primitive: 20>
	 "simple result"
	self myend
	  ifTrue: [^self pastend].
	^array ◦ (position ← position + 1)
! !
!Stream methodsFor: 'undefined' stamp: ''!
integerScan | sign base maxdigit c val |
	sign ←  "get the next Integer or LargeInteger (Float?) from a Stream.
	copied from String asInteger"
			(self ∢ 21
			  ifTrue: [¬1]
			  ifFalse: [1]).
	base ← (self ∢ 48
			  ifTrue: [8]
			  ifFalse: [10]).
	maxdigit ← 48 + base.
	val ← 0.
	[(c ← self next) and: [(c ≥ 48 and: [c < maxdigit])]] whileTrueDo: [val ← val * base + (c - 48)].
	c
	  ifTrue: [self skip: ¬1].
	(base = 8 and: [(val > 32767 and: [(sign = 1 and: [val < 65536])])])
	  ifTrue: [^val asSmall].
	^(val * sign) asInteger
! !
!Stream methodsFor: 'undefined' stamp: ''!
padNext← c | |
	position even
	  ifTrue: [^false].
	^self next← c
! !
!Stream methodsFor: 'undefined' stamp: ''!
next: n from: strm | |
	(1 to: n) do: [:n | self next← strm next]
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextword← val | |
	self next← val lshift: ¬8.
	self next← val land: 255.
	^val
! !
!Stream methodsFor: 'undefined' stamp: ''!
append: x | i |
	 "Array arg"
	x do: [:i | self next← i].
	^x
! !
!Stream methodsFor: 'undefined' stamp: ''!
pop | |
	 "use it as a LIFO"
	position < 1
	  ifTrue: [^false].
	position ← position - 1.
	^array ◦ (position + 1)
! !
!Stream methodsFor: 'undefined' stamp: ''!
pop: n | t |
	position < n
	  ifTrue: [^false].
	t ← self last: n.
	position ← position - n.
	^t
! !
!Stream methodsFor: 'undefined' stamp: ''!
∢ x | y |
	(y ← self next)
	  ifTrue:
		[ "peek for matching element"
		x = y
		  ifTrue: [^y].
		 "gobble it if found"
		position ← position - 1.
		^false]
	  ifFalse: [^false]
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextString← s | len |
	(len ← s length) < 192
	  ifTrue: [self next← len]
	  ifFalse:
		[self next← len / 256 + 192.
		self next← len \ 256].
	self append: s.
	^s
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextString | len |
	^self into: (String new: ((len ← self next) < 192
	  ifTrue: [len]
	  ifFalse: [ "up to 191 chars (BCPL compat)"
		len - 192 * 256 + self next])) endError: true "up to 16383 chars"
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextNumber: n | i s t |
	s ← false "return next n characters s as a positive Integer or LargeInteger" "scan for first non-zero byte, then collect rest appropriately".
	(1 to: n) do:
		[:i | t ← self next.
		s
		  ifTrue: [s ◦ (n + 1 "more LargeInteger: reverse order of significance" - i) ← t]
		  ifFalse:
			[i = n
			  ifTrue: [^t].
			t = 0
			  ifFalse:
				[(i ≤ (n - 2) or: [(t land: 128) "i=n-1" ≠ 0])
				  ifTrue:
					[s ← Natural new: n + 1 "LargeInteger of 2 or more bytes" - i.
					s last← t]
				  ifFalse: [ "positive Integer"
					^(t lshift: 8) + self next]]]].
	^LargeInteger new bytes: s neg: false
! !
!Stream methodsFor: 'undefined' stamp: ''!
nextNumber: n ← v | vlen |
	v ← v bytes "write a positive SmallInteger or LargeInteger as n characters".
	vlen ← v length.
	n < vlen
	  ifTrue: [user notify: 'number too big']
	  ifFalse:
		[n > vlen
		  ifTrue: [self next: (n - vlen) "pad beginning with 0's" ← 0]].
	vlen = 1
	  ifTrue: [self next← v]
	  ifFalse:
		[(vlen = 2 and: [v is: SmallInteger])
		  ifTrue: [self nextword← v]
		  ifFalse: [ "LargeInteger (assume pos, no negative convention)"
			self append: v reverse]]
! !
!Stream methodsFor: 'undefined' stamp: ''!
first | |
	position ≠ 0
	  ifTrue: [^array ◦ 1]
! !
!Stream methodsFor: 'undefined' stamp: ''!
last: n | |
	^(array ◦ (position - n + 1 to: position)) copy
! !
!Stream methodsFor: 'undefined' stamp: ''!
contents | |
	^array copy: 1 to: position
! !
!Stream methodsFor: 'undefined' stamp: ''!
last | |
	position ≠ 0
	  ifTrue: [^array ◦ position]
! !
!Stream methodsFor: 'undefined' stamp: ''!
rest | |
	^array copy: position + 1 to: limit
! !
!Stream methodsFor: 'undefined' stamp: ''!
insert: x | i |
	 "treat as LIFO queue, insert in front"
	 "grow array if necessary"
	position = limit
	  ifTrue:
		[array ← array grow.
		limit ← array length].
	(1 to: position) do: [:i | array ◦ (position - i + 2) ← array ◦ (position - i + 1)].
	array ◦ 1 ← x.
	position ← position + 1
! !
!Stream methodsFor: 'undefined' stamp: ''!
◦ x | |
	^array ◦ x
! !
!Stream methodsFor: 'undefined' stamp: ''!
◦ x ← val | |
	^array ◦ x ← val
! !
!Stream methodsFor: 'undefined' stamp: ''!
wordposition | |
	^self position / 2
! !
!Stream methodsFor: 'undefined' stamp: ''!
skipwords: w | |
	self skip: 2 * w
! !
!Stream methodsFor: 'undefined' stamp: ''!
limit | |
	^limit
! !
!Stream methodsFor: 'undefined' stamp: ''!
position | |
	^position
! !
!Stream methodsFor: 'undefined' stamp: ''!
empty | |
	 "for"
	^position = 0
! !
!Stream methodsFor: 'undefined' stamp: ''!
pastend | |
	^false
! !
!Stream methodsFor: 'undefined' stamp: ''!
reset | |
	position ← 0
! !
!Stream methodsFor: 'undefined' stamp: ''!
pastend← x | |
	array ← array grow.
	limit ← array length.
	^self next← x
! !
!Stream methodsFor: 'undefined' stamp: ''!
settoend | |
	position ← limit
! !
!Stream methodsFor: 'undefined' stamp: ''!
myend | |
	^position ≥ limit
! !
!Stream methodsFor: 'undefined' stamp: ''!
skip: x | |
	position ← position + x
! !
!Stream methodsFor: 'undefined' stamp: ''!
limit: t1 | |
	limit ← t1
! !
!Stream methodsFor: 'undefined' stamp: ''!
skipTo: x | y |
	self do:
		[:y | y = x
		  ifTrue: [^true]].
	^false
! !
!Stream methodsFor: 'undefined' stamp: ''!
loc | |
	 "synonym for compiler"
	^position
! !
!Stream methodsFor: 'undefined' stamp: ''!
end | |
	^position ≥ limit
! !
!Stream methodsFor: 'undefined' stamp: ''!
position← t1 | |
	position ← t1
! !
!Stream methodsFor: 'undefined' stamp: ''!
wordposition← w | |
	self position← w * 2
! !
!String methodsFor: 'undefined' stamp: ''!
stringhash | |
	^self hash
! !
!String methodsFor: 'undefined' stamp: ''!
setSourcePosition: pos inFile: f | loc p1 |
	 "set last three bytes to be pos in file (0-3)"
	f > 3
	  ifTrue: [user notify: 'invalid file number']
	  ifFalse:
		[loc ← self length - 2.
		self ◦ (loc + 2) ← f * 64 + (p1 ← pos / 65536).
		pos ← pos - (p1 * 65536) "into two bytes".
		self ◦ (loc + 1) ← p1 ← pos / 256.
		pos ← pos land: 255 "into one byte".
		self ◦ loc ← pos]
! !
!String methodsFor: 'undefined' stamp: ''!
alignForDisplay | |<primitive: 84>
	
! !
!String methodsFor: 'undefined' stamp: ''!
inBase: b | neg i j large large10 |
	 "convert to a LargeInteger"
	self ◦ 1 = 21
	  ifTrue: [neg ← true]
	  ifFalse: [neg ← false].
	large ← 0 asLarge.
	large10 ← b asLarge.
	((neg
	  ifTrue: [2]
	  ifFalse: [1]) to: self length) do: [:i | large ← large * large10 + ((j ← self ◦ i) ≥ 65 "A"
				  ifTrue: [j - 65 + 10]
				  ifFalse: [j - 48 "0"])].
	neg
	  ifTrue: [^large negated].
	^large
! !
!String methodsFor: 'undefined' stamp: ''!
asCompiledMethod | |
	^self copyto: (CompiledMethod new: self length)
! !
!String methodsFor: 'undefined' stamp: ''!
compare: s | i len lcomp u1 u2 t7 |
	lcomp ← ((t7 ← self length) < (len ← s length)
			  ifTrue:
				[len ← self length.
				1]
			  ifFalse:
				[t7 = len
				  ifTrue: [2]
				  ifFalse: [3]]).
	(1 to: len) do:
		[:i | (u1 ← UpperCase ◦ (self ◦ i + 1)) = (u2 ← UpperCase ◦ (s ◦ i + 1))
		  ifFalse:
			[u1 < u2
			  ifTrue: [^1].
			^3]].
	^lcomp
! !
!String methodsFor: 'undefined' stamp: ''!
match: text | star pound pattern scanning p t back |
	star ← 42 "*".
	pound ← 35 "#".
	pattern ← self asStream.
	text ← text asStream.
	scanning ← false.
	[true] whileTrueDo:
		[(p ← pattern next) = star
		  ifTrue:
			[pattern end
			  ifTrue: [^true].
			scanning ← pattern position]
		  ifFalse:
			[(t ← text next) ≡ false
			  ifTrue: [^t ≡ p].
			p ≡ false
			  ifTrue:
				[scanning
				  ifTrue:
					[back ← scanning - pattern position.
					pattern skip: back.
					text skip: back]
				  ifFalse: [^false]]
			  ifFalse:
				[(UpperCase ◦ (t + 1) = (UpperCase ◦ (p + 1)) or: [p = pound])
				  ifFalse:
					[scanning
					  ifTrue:
						[back ← scanning - pattern position.
						pattern skip: back.
						text skip: back + 1]
					  ifFalse: [^false]]]]]
! !
!String methodsFor: 'undefined' stamp: ''!
hash | l m |
	(l ← m ← self length) ≤ 2
	  ifTrue:
		[l = 2
		  ifTrue: [m ← 3]
		  ifFalse:
			[l = 1
			  ifTrue: [^(self ◦ 1 land: 127) * 106].
			^21845]].
	^self ◦ 1 * 48 + (self ◦ (m - 1) + l)
! !
!String methodsFor: 'undefined' stamp: ''!
- s | i c ldiff |
	(1 to:  "Return a negative, zero, or positive integer as I compare < = or > s" "The collation sequence is ascii with case differences ignored."
	((ldiff ← self length - s length) < 0
	  ifTrue: [self length]
	  ifFalse: [s length])) do:
		[:i | (c ← UpperCase ◦ (self ◦ i + 1) - (UpperCase ◦ (s ◦ i + 1))) ≠ 0
		  ifTrue: [^c]].
	^ldiff
! !
!String methodsFor: 'undefined' stamp: ''!
< s | |
	^(self compare: s) "Return true iff I collate before s.  The collation sequence is ascii with case differences ignored." = 1
! !
!String methodsFor: 'undefined' stamp: ''!
> s | |
	^(self compare: s) "Return true iff I collate after s.  The collation sequence is ascii with case differences ignored." = 3
! !
!String methodsFor: 'undefined' stamp: ''!
systemRehash | dicts d left loop |
	String understands: 'hash | l m
	[[(l← m← self length)≤2⇒
		[l=2⇒[m←3]; =1⇒[⇑((self◦1) land: 0177)*0152] ⇑052525]].
	 ⇑(self◦1)*060+(self◦(m-1)+l)]' "change the meaning of hash for Strings" "rehash the atom table".
	#a rehash "rehash all dictionaries which have strings in them".
	dicts ← HashSet allInstances + Dictionary allInstances + SymbolTable allInstances.
	dicts do:
		[:d | left ← d objects asStream.
		loop ← left next.
		[loop] whileTrueDo:
			[(loop is: String)
			  ifTrue:
				[d rehash.
				loop ← false]
			  ifFalse: [loop ← left next]]]
! !
!String methodsFor: 'undefined' stamp: ''!
species | |
	^String
! !
!String methodsFor: 'undefined' stamp: ''!
printOn: strm | x |
	 "print inside string quotes"
	strm next← 39.
	self do:
		[:x | strm next← x.
		x = 39
		  ifTrue: [strm next← x]].
	strm next← 39 "imbedded quotes get doubled"
! !
!String methodsFor: 'undefined' stamp: ''!
asFloat | strm int frac exp |
	strm ← Stream new of: self.
	int ← strm upto: 46.
	frac ← strm upto: 101.
	exp ← strm rest asInteger - frac length.
	int ← (int concat: frac) asDecimalDigits asFloat.
	exp = 0
	  ifTrue: [^int].
	exp > 0
	  ifTrue: [^int * (10.0 ipow: exp)].
	.
	^int / (10.0 ipow: 0 - exp)
! !
!String methodsFor: 'undefined' stamp: ''!
asFileName | |
	^dp0 checkName: self fixing: true
! !
!String methodsFor: 'undefined' stamp: ''!
asInteger | sign base maxdigit c val i i1 i2 |
	self length = 0
	  ifTrue: [^0].
	i1 ← 1.
	i2 ← self length.
	sign ← (self ◦ 1 = 21
			  ifTrue:
				[i1 ← 2.
				¬1]
			  ifFalse: [1]).
	base ← (self ◦ self length = 72
			  ifTrue:
				[i2 ← i2 - 1.
				16]
			  ifFalse:
				[self ◦ i1 = 48
				  ifTrue: [8]
				  ifFalse: [10]]).
	maxdigit ← (base = 16
			  ifTrue: [70]
			  ifFalse: [47 + base]).
	val ← (self length > 4
			  ifTrue: [0.0]
			  ifFalse: [0]).
	(i1 to: i2) "octal and hex packed into 16-bit Integers" do:
		[:i | c ← self ◦ i.
		(c < 48 or: [c > maxdigit])
		  ifTrue: [user notify: self + ' isn''t a valid Integer']
		  ifFalse: [val ← val * base + (c ≤ 57
					  ifTrue: [c - 48]
					  ifFalse: [c - 55])]].
	val > 32767
	  ifTrue:
		[(base = 8 or: [base = 16])
		  ifTrue:
			[sign = 1
			  ifTrue:
				[val < 65536
				  ifTrue: [^(val - 65536) asInteger].
				user notify: 'Warning: Octal or Hex number exceeds 16 bits']]].
	^(val * sign) asInteger
! !
!String methodsFor: 'undefined' stamp: ''!
asBitmap | t |
	t ← Bitmap new: self length / 2.
	^t fromString: self
! !
!String methodsFor: 'undefined' stamp: ''!
unique | u |
	 "copy and intern"
	^#a intern: self
! !
!String methodsFor: 'undefined' stamp: ''!
base8: i | |
	 "word: i  in base 8 as a String"
	^(self word: i) base8
! !
!String methodsFor: 'undefined' stamp: ''!
asBytes | s c |
	s ← Stream default.
	self do:
		[:c | s append: c base8.
		s space].
	^s contents
! !
!String methodsFor: 'undefined' stamp: ''!
asLarge | neg i large large10 |
	 "convert to a LargeInteger"
	self ◦ 1 = 21
	  ifTrue: [neg ← true]
	  ifFalse: [neg ← false].
	large ← 0 asLarge.
	large10 ← 10 asLarge.
	((neg
	  ifTrue: [2]
	  ifFalse: [1]) to: self length) do: [:i | large ← large * large10 + (self ◦ i - 48)].
	neg
	  ifTrue: [^large negated].
	^large
! !
!String methodsFor: 'undefined' stamp: ''!
asVector | |
	^self asStream asVector
! !
!String methodsFor: 'undefined' stamp: ''!
hasBeenUniqued | |
	^#a hasInterned: self
! !
!String methodsFor: 'undefined' stamp: ''!
asDecimalDigits | strm sign c val |
	 "Not asInteger, because the result may be a Float if it's too big"
	strm ← Stream new of: self.
	sign ← strm ∢ 21.
	val ← (self length > 4
			  ifTrue: [0.0]
			  ifFalse: [0]).
	strm do:
		[:c | (c < 48 or: [c > 57])
		  ifTrue: [user notify: self + ' isn''t a valid integer']
		  ifFalse: [val ← val * 10 + (c - 48)]].
	sign
	  ifTrue: [^val * ¬1].
	^val
! !
!String methodsFor: 'undefined' stamp: ''!
asUppercase | s c |
	s ← Stream default.
	self do: [:c | s next← UpperCase ◦ (c + 1)].
	^s contents
! !
!String methodsFor: 'undefined' stamp: ''!
asParagraph | |
	^Paragraph new text: self alignment: 0
! !
!String methodsFor: 'undefined' stamp: ''!
subst: repl for: key | key1 i nskip result |
	nskip ← 0.
	key1 ← key ◦ 1.
	result ← Stream default.
	(1 to: self length) do:
		[:i |  " the Boyer Slow string replacement "
		nskip > 0
		  ifTrue: [nskip ← nskip - 1]
		  ifFalse:
			[self ◦ i = key1
			  ifTrue:
				[self ◦ (i to: (self length min: i + key length - 1)) = key
				  ifTrue:
					[result append: repl.
					nskip ← key length - 1]
				  ifFalse: [result next← self ◦ i]]
			  ifFalse: [result next← self ◦ i]]].
	^result contents
! !
!String methodsFor: 'undefined' stamp: ''!
copy: a to: b with: s from: c to: d | i |
	((b - a "like replace, except in place. self◦(a to: b) ← s◦(c to: d).
	use BitBlt unless size too small, StringBlter≡false, or index/sizes too large" > 12 and: [StringBlter]) and: [(BitBlt new stringCopy: self from: a to: b with: s from: c to: d)])
	  ifFalse:
		[(self ≡ s and: [(c < a and: [d ≥ a])])
		  ifTrue: [(b - a "overlap of second range with below first in same string.
		copy in reverse order: self◦(b to: a by: ¬1) ← self◦(d to: c by: ¬1)" to: 0 by: ¬1) do: [:i | self ◦ (a + i) ← self ◦ (c + i)]]
		  ifFalse: [s copy: c to: d to: (Stream new of: self from: a to: b)]]
! !
!String methodsFor: 'undefined' stamp: ''!
replace: a to: b by: r from: c to: d | s t |
	s ← self species new: self length + (d - c) - (b - a) "use BitBlt unless StringBlter≡false or index/sizes too large".
	(StringBlter and: [(BitBlt new stringReplace: s with: self from: a to: b and: [r] from: c to: d)])
	  ifTrue: [^s].
	 "see Array concat:"
	t ← Stream new of: s.
	self copy: 1 to: a - 1 to: t.
	r copy: c to: d to: t.
	self copy: b + 1 to: self length to: t.
	^s
! !
!String methodsFor: 'undefined' stamp: ''!
growto: n | len |
	(len ← self length) ≤ n
	  ifFalse: [len ← n].
	^(self species new: n) copy: 1 to: len with: self from: 1 to: len
! !
!String methodsFor: 'undefined' stamp: ''!
recopy | |
	^self copy
! !
!String methodsFor: 'undefined' stamp: ''!
findString: str startingAt: start | i t |
	str length = 0
	  ifTrue: [^0].
	t ← str ◦ 1.
	(start to: self length - str length + 1) do:
		[:i | self ◦ i = t
		  ifTrue:
			[self ◦ (i to: i + str length - 1) = str
			  ifTrue: [^i]]].
	^0
! !
!String methodsFor: 'undefined' stamp: ''!
concat: s | len |
	((len ← self length) + s length > 20 and: [(s Is: String)])
	  ifTrue: [^self replace: len + 1 "this concatenates more quickly if BitBlt is used" to: len by: s from: 1 to: s length].
	^super concat: s
! !
!String methodsFor: 'undefined' stamp: ''!
copy: a to: b | |
	^(self species new: 1 + b - a) copy: 1 to: 1 + b - a with: self from: a to: b
! !
!String methodsFor: 'undefined' stamp: ''!
replace: a to: b by: s | |
	(s Is: String)
	  ifTrue: [^self replace: a to: b by: s from: 1 to: s length].
	^self replace: a to: b by: s asArray from: 1 to: s position
! !
!String methodsFor: 'undefined' stamp: ''!
doubleWordAt: i ← value | w |
	"Primarily for setting socket #s in Pup headers"
	w ← value bytes.
	self◦i ← w◦4.
	self◦(i+1) ← w◦3.
	self◦(i+2) ← w◦2.
	self◦(i+3) ← w◦1.
	^value
! !
!String methodsFor: 'undefined' stamp: ''!
doubleWordAt: i | b0 b1 b2 w |
	"Primarily for reading socket #s in Pup headers"
	b0 ← self◦i.  b1 ← self◦(i+1).  b2 ← self◦(i+2).  w ← self◦(i+3).
	"Following sequence minimizes LargeInteger arithmetic for small results."
	b2=0 ifFalse: [w ← b2*256 + w].
	b1=0 ifFalse: [w ← b1*65536 + w].
	b0=0 ifFalse: [w ← b0*16777216 + w].
	^w
! !
!String methodsFor: 'undefined' stamp: ''!
unsignedWord: x | |
	 "Will replace word: someday"
	^self◦(x+x+1)*256 + (self◦(x+x))
! !
!String methodsFor: 'undefined' stamp: ''!
all← val | |
	self fill: 1 to: self length with: val
! !
!String methodsFor: 'undefined' stamp: ''!
fill: a to: b with: val | i |
	(a to: b) do: [:i |  "eventually use BitBlt?"
		self ◦ i ← val]
! !
!String methodsFor: 'undefined' stamp: ''!
word: x ← y | |
	 "write word in String"
	self ◦ (x + x - 1) ← y lshift: ¬8.
	self ◦ (x + x) ← y land: 255.
	^y
! !
!String methodsFor: 'undefined' stamp: ''!
word: x | |
	 "read word in String"
	^self ◦ (x + x) + (self ◦ (x + x - 1) lshift: 8)
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
spacesize | |
	^spacesize
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
baseline | |
	^baseline
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
lineheight | |
	^lineheight
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
fontsize: fontindex | |
	^fontset size: fontindex
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
fontfamily: fontindex | |
	^fontset family: fontindex
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
effect | |
	^effect
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
linelead | |
	^linelead
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
tabsize | |
	^tabsize
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
font: fontindex | |
	^fontset ◦ fontindex
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
NoteTakerize | x |
	(fontset ◦ 0) NTformat
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
fontset: t1 | |
	fontset ← t1.
	self fontset: fontset lineheight: fontset height linelead: 0 baseline: fontset baseline tabsize: 32 spacesize: 4 effect: storing
! !
!StyleSheet methodsFor: 'undefined' stamp: ''!
fontset: t1 lineheight: t2 linelead: t3 baseline: t4 tabsize: t5 spacesize: t6 effect: t7 | |
	fontset ← t1.
	lineheight ← t2.
	linelead ← t3.
	baseline ← t4.
	tabsize ← t5.
	spacesize ← t6.
	effect ← t7
! !
!Substring methodsFor: 'undefined' stamp: ''!
species | |
	^data species
! !
!Substring methodsFor: 'undefined' stamp: ''!
asStream | |
	map isIntervalBy1
	  ifTrue: [ "direct stream for simple substrings"
		^Stream new of: data from: map start to: map stop].
	^Stream new of: self from: 1 to: map length
! !
!Substring methodsFor: 'undefined' stamp: ''!
swap: i with: j | t |
	t ← map ◦ i "By permuting my map (a writable Array), swap elements i and j.".
	map ◦ i ← map ◦ j.
	map ◦ j ← t
! !
!Substring methodsFor: 'undefined' stamp: ''!
data: t1 map: t2 | |
	data ← t1.
	map ← t2
! !
!Substring methodsFor: 'undefined' stamp: ''!
map | |
	^map "Return my map."
! !
!Substring methodsFor: 'undefined' stamp: ''!
length | |
	^map length
! !
!Substring methodsFor: 'undefined' stamp: ''!
◦ x | |
	^data ◦ (map ◦ x)
! !
!Substring methodsFor: 'undefined' stamp: ''!
◦ x ← val | |
	^data ◦ (map ◦ x) ← val
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
ref: name | |
	^super ◦ name
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
ref: name ← val | |
	^super ◦ name ← val
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
forEachClass⦂ class selector⦂ selector do⦂ action | name cl s |
	user cr.
	user show: 'anybug to interrupt.'.
	user cr.
	AllClassNames do:
		[:name | user anybug
		  ifFalse:
			[user show: name.
			user cr.
			class value← cl ← Smalltalk ◦ name.
			user displayoffwhile⦂ [(cl md do:
					[:s | selector value← s.
					action eval])]]]
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
clean | name |
	 "release unreferenced entries"
	self do:
		[:name |  "slick, huh"
		(super ◦ name) refct = 1
		  ifTrue: [self delete: name]]
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
rehash | i copy |
	copy ← SymbolTable new init: self size "create a copy".
	(1 to: objects length) "hash each entry into it" do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [copy insert: objects ◦ i withref: values ◦ i]].
	self copyfrom: copy
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
growto: size | name copy |
	copy ← self class new init: size "create a copy of the new size".
	self do: [:name | copy insert: name withref: (self ref: name)].
	self copyfrom: copy "hash each entry into it"
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
insert: name with: x | |
	(self has: name)
	  ifFalse: [super insert: name with: Association new].
	self ◦ name ← x
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
declare: name | |
	 "Take ref(s) and value(s) from Undeclared, if name(s) there"
	self declare: name from: Undeclared
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
declare: name from: symTab | a |
	 "take name(s), ref(s) and value(s) from symTab"
	(name is: Vector)
	  ifTrue: [name do: [:a | self declare: a from: symTab]]
	  ifFalse:
		[(self has: name)
		  ifFalse:
			[(symTab has: name)
			  ifTrue:
				[super insert: name with: (symTab ref: name).
				symTab delete: name]
			  ifFalse: [self insert: name with: nil]]]
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
insert: name withref: ref | |
	super insert: name with: ref
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
define: name as: x | |
	 "synonym"
	^self declare: name as: x
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
declare: name as: x | a s |
	(name is: Vector)
	  ifTrue:
		[s ← x asStream.
		name do: [:a | self declare: a as: s next]]
	  ifFalse:
		[self declare: name.
		self ◦ name ← x]
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
◦ name ← x | |
	^super ◦ name value← x
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
invertRef: obj | i |
	(1 to: values length) do:
		[:i | obj ≡ (values ◦ i)
		  ifTrue: [^objects ◦ i]].
	^false
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
invert: obj | i |
	(1 to: values length) do:
		[:i | nil ≡ (values ◦ i)
		  ifFalse:
			[obj ≡ (values ◦ i) value
			  ifTrue: [^objects ◦ i]]].
	^false
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
lookup: name | r |
	(r ← super lookup: name)
	  ifTrue: [^r value].
	^false
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
lookupRef: name | |
	^super lookup: name
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
allRefsTo: symbol from: classNames | s |
	(symbol is: Vector)
	  ifFalse: [symbol ← symbol inVector].
	^Smalltalk allCallsOn: (symbol transform⦂ [:s | s] to⦂ [(self ref: s)]) from: classNames "
Smalltalk allRefsTo: ↪ST80 from: (SystemOrganization superclassOrder: 'S80-Compiler')
"
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
allRefs | |
	 "what methods reference my variables (I am probably 'Undeclared')"
	^self allRefsTo: self contents from: user classNames
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
allCallsOn: selector ||
	^self allCallsOn: selector from: user classNames
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
allCallsOn: selector from: classNames | className s w cl sel |
	(selector is: Vector)
	  ifFalse: [selector ← selector inVector].
	s ← Stream default.
	user displayoffwhile⦂ [(classNames do:
			[:className | cl ← self ◦ className.
			selector do:
				[:sel | w ← cl whosends: sel.
				w length = 0
				  ifFalse:
					[s append: className.
					s append: '⇒'.
					s append: w asString.
					s cr]]])].
	^s contents
! !
!SymbolTable methodsFor: 'undefined' stamp: ''!
◦ name | |
	^(super ◦ name) value
! !
!SyntaxWindow methodsFor: 'undefined' stamp: ''!
aboutToFrame | nw t s c |
	t ← title.
	s ← panes ◦ 1.
	c ← panes ◦ 2.
	user unschedule: self.
	nw ← NotifyWindow new frame: frame.
	nw of: t stackPane: s codePane: c.
	user restartup: nw
! !
!SyntaxWindow methodsFor: 'undefined' stamp: ''!
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
!SyntaxWindow methodsFor: 'undefined' stamp: ''!
classInit | |
	stdTemplates ← {(0 ⌾ 0 rect: 12 ⌾ 36) , (12 ⌾ 0 rect: 36 ⌾ 36)}.
	stdFrame ← 60 ⌾ 320 rect: 570 ⌾ 500
! !
!SystemOrganizer methodsFor: 'undefined' stamp: ''!
superclassOrder: cat | all lis title i c sup |
	 "Arrange classes in superclass order so they can be filed in"
	lis ← (self category: cat) copy.
	all ← (Vector new: lis length) asStream.
	[lis length > 0] whileTrueDo:
		[i ← 1.
		title ← lis ◦ i.
		sup ← c ← Smalltalk ◦ title.
		sup ← sup superclass.
		[ "Make sure it doesn't have an as yet uncollected superclass"
		sup ≡ nil or: [(lis has: sup title unique)]] whileFalseDo:  [sup ← sup superclass].
		[sup ≡ nil] whileFalseDo: 
			[i ← i + 1.
			title ← lis ◦ i.
			sup ← c ← Smalltalk ◦ title.
			sup ← sup superclass "check as yet uncollected superclass".
			[sup ≡ nil or: [(lis has: sup title unique)]] whileFalseDo:  [sup ← sup superclass]].
		all next← title.
		lis ← lis delete: title].
	^all contents
! !
!SystemOrganizer methodsFor: 'undefined' stamp: ''!
printCategory: cat | |
	user displayoffwhile⦂ [((dp0 file: (cat + '.press') asFileName) printoutclass: (self superclassOrder: cat))]
! !
!SystemOrganizer methodsFor: 'undefined' stamp: ''!
printAll | cat |
	commentVector do: [:cat | self printCategory: cat]
! !
!SystemOrganizer methodsFor: 'undefined' stamp: ''!
filoutCategory: cat | all a |
	user displayoffwhile⦂ [
		(all ← self superclassOrder: cat.
		(dp0 file: (cat + '.st.') asFileName) filoutclass: all.
		all do: [:a | (Smalltalk ◦ a) noChanges])]
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
forget: className | t2 |
	"user notify: 'Class ' + className + ' will disappear if you proceed...'."
	(t2 ← Smalltalk ◦ className) noChanges.
	t2 obsolete.
	Smalltalk delete: className.
	SystemOrganization delete: className.
	AllClassNames ← AllClassNames delete: className.
	classPane revise: self classes with: className
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
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
!SystemPane methodsFor: 'undefined' stamp: ''!
classes | |
	 "return a Vector of the classes in my selected category"
	selection = 1
	  ifTrue: [^user classNames].
	selection ≤ 2
	  ifTrue: [^Vector new: 0].
	^SystemOrganization category: list ◦ selection
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
dirty | |
	^classPane dirty
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
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
!SystemPane methodsFor: 'undefined' stamp: ''!
update | |
	self of: (#(AllClasses SystemOrganization ) concat: SystemOrganization categories).
	mySysOrgVersion ← user classNames
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
classInit | |
	sysmenu ← Menu new string: 'filout
print'
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
to: t1 | |
	classPane ← t1
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
deselected | |
	classPane of: (Vector new: 0)
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
selected | |
	classPane of: self classes
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
leave | |
	 "I am up to date"
	mySysOrgVersion ← user classNames.
	super leave
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
enter | |
	 "be sure I am up to date"
	mySysOrgVersion ≡ user classNames
	  ifTrue: [super enter]
	  ifFalse:
		[window outline.
		self update.
		super enter]
! !
!SystemPane methodsFor: 'undefined' stamp: ''!
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
!SystemTracer methodsFor: 'undefined' stamp: ''!
initDict | class |
	writeDict ← Dictionary init: 256.
	Class allInstances do:
		[:class | writeDict insert: class with:
		(class isBits ifTrue:
			[class isBytes ifTrue: [#writeBytes:] ifFalse: [#writeWords:]]
		  ifFalse:
			[(class Isa: LADSet) ifTrue: [#writePermuted:] ifFalse:
				[(class Isa: HashSet) ifTrue: [#writeHashSet:] ifFalse: [#writePointers:]]])].
	writeDict insert: SmallInteger with: #writeClamped:.
	writeDict insert: CompiledMethod with: #writeMethod:
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
test | t1 t2 n start  | "SystemTracer new test."
	self init.
	start← MethodContext new.
	Processor activeProcess suspendedContext: (start send: ↪goBaby to: start).
	user clearshow: 'Tracing . . . '.
	t1← user time asSeconds.
	self clampAlto.
	n←self writeImage: {Smalltalk}.
	t2← user time asSeconds.
	user print: n; show: ' objects, words, '; print: t2-t1; show: ' sec.'; cr
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
classInit | |  "SystemTracer classInit. "
	ODD← 128. PTRS← 64. MT← 32.
	NoRefs← 0. OVref← 128. Clamped← 255.
	NewNil← 1
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
clampAlto | str class |
	{'IFS File System', 'Juniper','Events','Ethernet Control',
		'Picture Editor', 'Documents'} do: 
		[:str | (SystemOrganization superclassOrder: str) do:
			[:class | self clamp: Smalltalk◦class]].
	#(FormSet HalfToner FontWindow SystemTracer PressFile SystemTracer VmemChecker
		PressPrinter WidthTable gJuniperConstants ILFilePool SourceFiles) do:
		[:class | (Smalltalk has: class) ifTrue: [self clamp: Smalltalk◦class]]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
init | |
	refcts← String new: 30000.  map← Vector new: 30000.
	file← dp0 file: 'clone.im'.  ot← dp0 file: 'ot.scratch'.
	maxOop← 0.
	self initDict
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
clamp: obj | |
	refcts◦(self oopOf: obj) ← Clamped.
	(obj is: Class) ifTrue: [writeDict insert: obj with: #writeClamped:]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeSpecial1 | obj i |
	specialObjects ← {nil , false , true , (Smalltalk ref: #Processor) , Class , SmallInteger , String , Vector , Stream , Float , MethodContext , BlockContext , Point , Natural , LargeInteger , Message , CompiledMethod , Process , Semaphore , #unusedOop20 , #doesNotUnderstand: , #cannotReturn: , #monitor:}
	concat: (#(+ - < > ≤ ≥ = ≠ * / \ ⌾ lshift: lxor: land: lor:)
	concat: ({ #◦ , '◦←' unique , #length , #next , 'next←' unique}
	concat: #( end ≡ class blockCopy: value value: unusedOop51 new new: x y ))).

	specialObjects do:  "create OT entries"
		[:obj | self new: obj class: obj class length: obj sizeInWords
			trace: false write: []].
	specialObjects do:  "now its OK to trace them"
		[:obj | self trace: obj class.
		obj class isBits ifFalse: [1 to: obj sizeInWords do:
			[:i | self trace: (obj instfield: i)]]]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeSpecial2 | obj pos otpos max |
	pos← file position. file position← 512.
	otpos← ot position. ot position← 0.
	max← maxOop.
	specialObjects do:  "go back and write data after tracing"
		[:obj | (refcts◦(self oopOf: obj)← refcts◦(self oopOf: obj)-1)=NoRefs
			ifTrue: [user notify: 'a specialObject did not get marked!'].
		self perform: writeDict◦obj class with: obj].
	max≠maxOop ifTrue: [user notify: 'bug in tracer'].
	file position← pos. ot position← otpos
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeImage: roots | datalen otlen root oop i |
	ot readwrite.  file readwrite.  file position← 512.  "Skip header page"
	ot position← 0.  addr ← bank ← 0.
	self writeSpecial1.
	roots do: [:root | self trace: root].
	self writeSpecial2.
	datalen ← file position - 512 / 2.
	ot shorten.  otlen ← ot position / 5 + 1 * 2.
	file pad: 512 with: 0.  ot position← 0.
	file nextword← MT. file nextword← 0.  "oop zero"
	1 to: maxOop do:  "append the OT, rectifying reference counts"
		[:i | oop← ot nextword.
		file next← refcts◦(oop).  file next← ot next.
		file next← ot next.  file next← ot next].
	file shorten.  file position← 0.
	file nextword← datalen / 65536. file nextword← datalen \ 65536.  "info in header page"
	file nextword← otlen / 65536. file nextword← otlen \ 65536.
	file pad: 512 with: 0.  "Pad header page"
	ot close.  file close.
	^{otlen/2, datalen }
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
permute: vec by: perm | copy i j len inverse |
	len ← vec length.
	copy← Vector new: len.  inverse← Vector new: len.
	(1 to: len) do: [:i | (j←perm◦i)≡nil ifFalse:
			[copy◦i ← vec◦j.  inverse◦j← i]].
	(1 to: len) do: [:i | vec◦i ← copy◦i].
	^inverse
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writePointerField: obj | oop | "avoid overhead of lgints and nextword←"
	(obj is: SmallInteger)
		ifTrue: [file next← ((obj lshift: ¬7) land: 255).
			file next← (obj land: 127)*2+1]
		ifFalse: [(oop← map◦(self oopOf: obj))≡nil ifTrue: [oop← NewNil].
			file next← oop lshift: ¬7.
			file next← (oop land: 127)*2]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeBitField: bits | bytes | "avoid overhead of lgints and nextword←"
	(bits class ≡ SmallInteger)
		ifTrue: [file next← bits lshift: ¬8. file next← bits land: 255]
		ifFalse: [bytes← bits bytes. file next← bytes◦2. file next← bytes◦1]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
permutationFor: dict | len vec off i entry old key h |
	"Return an inverse permutation vector for a dictionary to permute it according to
	the mapped oop values.  Returns false if all the keys arent UniqueStrings."
	old← dict is: Vector.
	len ← dict length.  vec ← Vector new: len.
	off ← dict class instsize "Use instfield to bypass ◦ behavior".
	(1 to: len) do:
		[:i | (entry ← dict instfield: off + i) ≡ nil
		  ifFalse:
			[((key← old ifTrue: [entry] ifFalse: [entry key]) is: UniqueString)
			  ifTrue: [h ← map◦(self oopOf: key).
				h≡nil ifTrue: [^false].  h← h \ len + 1.
				[vec ◦ h ≡ nil] whileFalseDo:
					[h ← (h = len ifTrue: [1] ifFalse: [h + 1])].
				vec ◦ h ← i]
			  ifFalse: [^false]]].
	^vec
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
oopOf: obj | oop |  "return oops in range 1 to 32767. obj must not be an Integer"
	oop ← obj asOop.
	oop ≥ 0 ifTrue: [^oop] ifFalse: [^oop+32768]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
new: obj class: class length: length trace: traceBlock write: writeBlock
	| oop objpos endpos |
	oop← self oopOf: obj.
	refcts◦oop=NoRefs ifTrue: "create a new ot entry"
		[map◦oop← maxOop← maxOop+1.
		maxOop\1000=0 ifTrue: [user print: maxOop; space].
		refcts◦oop← 1.
		ot nextword← oop.  "save oop; fill in refct in pass2"
		ot next← bank +
			(class  isBits
			ifTrue: [(class isBytes and: [obj length even ≡ false])
				ifTrue: [ODD]  ifFalse: [0]]
			ifFalse: [PTRS]).
		ot nextword← addr.
		addr ← addr + length+2.
		addr ≥ 65536 ifTrue:
			[addr ← (addr - 65536) asInteger.
			bank ← bank + 1]]
		ifFalse: [oop>specialObjects length ifTrue: [user notify: 'double write!']].
	(class isBits and: [refcts◦(self oopOf: class)>NoRefs])  "ie. no tracing to do"
		ifTrue: [self writeBitField: length+2.  self writePointerField: class.  writeBlock value]
		ifFalse: [objpos← file position.  file skip: length+2*2.
			traceBlock ifTrue: [self trace: class.  traceBlock value].
			endpos← file position.  file position← objpos.
			self writeBitField: length+2.  self writePointerField: class.  writeBlock value.
			file position← endpos]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeHashSet: obj | perm i inverse |
	perm← self permutationFor: (obj instfield: 1).
	perm ifFalse: [^self writePointers: obj].
	1 to: obj sizeInWords do: [:i | inverse← self permute: (obj instfield: i) by: perm].
	self writePointers: obj.
	1 to: obj sizeInWords do: [:i | self permute: (obj instfield: i) by: inverse]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeMethod: obj | i nptrs length |
	nptrs← obj numLiterals+2.
	self new: obj class: obj class length: (length← obj sizeInWords)
	trace: [1 to: nptrs do: [:i | self trace: (obj objectAt: i)]]
	write: [1 to: nptrs do: [:i | self writePointerField: (obj objectAt: i)].
		file append: obj◦(nptrs*2+1 to: obj length). file padNext← 0]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeWords: obj | i length |
	self new: obj class: obj class length: (length← obj sizeInWords)
		trace: []
		write: [1 to: length do: [:i | self writeBitField: (obj instfield: i)]]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeBytes: obj | |
	self new: obj class: obj class length: obj sizeInWords
		trace: []
		write: [file append: obj. file padNext← 0]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writePermuted: obj | i perm fixPart length |
	perm← self permutationFor: obj.
	perm ifFalse: [^self writePointers: obj].
	self new: obj class: obj class length: (length← obj sizeInWords)
	trace: [1 to: length do: [:i | self trace: (obj instfield: i)]]
	write: [1 to: (fixPart← obj class instsize) do: [:i | self writePointerField: (obj instfield: i)].
		1 to: obj length do: [:i | self writePointerField:
			(perm◦i≡nil ifTrue: [nil] ifFalse: [obj instfield: perm◦i+fixPart])]]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
trace: obj | oop refct length |
	(obj is: SmallInteger) ifTrue: [^self].
	oop← self oopOf: obj.
	refct← refcts◦oop.
	refct=Clamped ifTrue: [^self].
	refct=OVref ifTrue: [^self].
	refct>NoRefs ifTrue: [refcts◦oop← refct+1. ^self].
	self perform: writeDict◦obj class with: obj
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writePointers: obj | i length |
	self new: obj class: obj class length: (length← obj sizeInWords)
	trace: [1 to: length do: [:i | self trace: (obj instfield: i)]]
	write: [1 to: length do: [:i | self writePointerField: (obj instfield: i)]]
! !
!SystemTracer methodsFor: 'undefined' stamp: ''!
writeClamped: obj | |
	^false
! !
!TextImage methodsFor: 'undefined' stamp: ''!
Scrap← s | |
	Scrap ← s
! !
!TextImage methodsFor: 'undefined' stamp: ''!
formerly | |
	^oldEntity
! !
!TextImage methodsFor: 'undefined' stamp: ''!
frame← f | |
	self fixframe: f
! !
!TextImage methodsFor: 'undefined' stamp: ''!
c2← t1 | |
	c2 ← t1
! !
!TextImage methodsFor: 'undefined' stamp: ''!
c1← t1 | |
	c1 ← t1
! !
!TextImage methodsFor: 'undefined' stamp: ''!
Deletion← s | |
	Deletion ← s
! !
!TextImage methodsFor: 'undefined' stamp: ''!
height | rect |
	self pointofchar: para length + 1 andrect⦂ [:rect | rect].
	^rect corner y - frame minY
! !
!TextImage methodsFor: 'undefined' stamp: ''!
contents | |
	^para
! !
!TextImage methodsFor: 'undefined' stamp: ''!
begintypein← t1 | |
	begintypein ← t1
! !
!TextImage methodsFor: 'undefined' stamp: ''!
frameoffset | |
	^frame minY "a useful number" - window minY
! !
!TextImage methodsFor: 'undefined' stamp: ''!
formerly: t1 | |
	oldEntity ← t1
! !
!TextImage methodsFor: 'undefined' stamp: ''!
enter | |
	begintypein ← false.
	self show.
	self select
! !
!TextImage methodsFor: 'undefined' stamp: ''!
leave | |
	self complement: off
! !
!TextImage methodsFor: 'undefined' stamp: ''!
show | |
	lastline = 0
	  ifTrue: [super show]
	  ifFalse: [self displayall].
	sel ← off
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
realign | |
	self align.
	sel ← on
! !
!TextImage methodsFor: 'undefined' stamp: ''!
paste | |
	self fintype "paste the Scrap over the current selection and redisplay the paragraph.".
	self unselect.
	self replace: Scrap.
	self selectAndScroll
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
align | |
	para alignment← #(1 2 4 0 0 ) ◦ (1 + para alignment).
	self displayall.
	self reversefrom: c1 to: c2
! !
!TextImage methodsFor: 'undefined' stamp: ''!
undo | |
	self fintype.
	self replace: Deletion.
	self complement
! !
!TextImage methodsFor: 'undefined' stamp: ''!
fit | rect |
	(window intersect: frame) "make the bounding rectangle of the TextImage contain all the textwhile not changing the width of the TextImage." white.
	frame extent← frame width ⌾ 1000.
	self pointofchar: para length + 1 andrect⦂ [:rect | rect].
	frame extent← frame width ⌾ (rect corner y - frame origin y).
	self show.
	frame border: 1 color: ¬1.
	self reversefrom: c1 to: c2
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
cut | |
	self fintype "cut out the current selection and redisplay the paragraph.".
	self replace: nullString.
	self selectAndScroll.
	Scrap ← Deletion
! !
!TextImage methodsFor: 'undefined' stamp: ''!
copyselection | |
	Scrap ← self selection "copy the current selection and store it in the Scrap."
! !
!TextImage methodsFor: 'undefined' stamp: ''!
fixframe: f | dy |
	dy ← (frame ≡ nil
			  ifTrue: [0]
			  ifFalse: [self frameoffset]).
	window ← f copy.
	frame ← Rectangle new origin: window origin + (2 ⌾ dy) extent: window width - 4 ⌾ 9999.
	self measureall.
	^window
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
paragraph: t1 frame: t2 style: t3 | |
	para ← t1.
	frame ← t2.
	style ← t3.
	nil ≡ para
	  ifTrue: [para ← nullString].
	c1 ← c2 ← begintypein ← 1.
	self para: para frame: frame style: style
! !
!TextImage methodsFor: 'undefined' stamp: ''!
close | |
	superimage ← nil
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
scrollTo: f | |
	self scrollUp: self frameoffset + (f * self height) asInteger - 4
! !
!TextImage methodsFor: 'undefined' stamp: ''!
scrollUp: n | |
	self scrollby: n / self lineheight
! !
!TextImage methodsFor: 'undefined' stamp: ''!
scrollPos | t |
	t ← self height - self lineheight.
	t = 0
	  ifTrue: [^0.0].
	^0.0 - self frameoffset / t
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
select: t | |
	self complement: off.
	c1 ← c2 ← t.
	self selectAndScroll
! !
!TextImage methodsFor: 'undefined' stamp: ''!
select | |
	sel ← off.
	c1 ≡ nil
	  ifTrue: [c1 ← c2 ← 1].
	self complement: on
! !
!TextImage methodsFor: 'undefined' stamp: ''!
unselect | |
	self complement: off
! !
!TextImage methodsFor: 'undefined' stamp: ''!
selectionAsStream | |
	^Stream new of: para text from: c1 to: c2 - 1
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
selectRange: r | |
	self complement: off.
	c1 ← r start.
	c2 ← r stop.
	self complement: on
! !
!TextImage methodsFor: 'undefined' stamp: ''!
selectRange | |
	^c1 to: c2 - 1
! !
!TextImage methodsFor: 'undefined' stamp: ''!
complement | |
	self complement: on
! !
!TextImage methodsFor: 'undefined' stamp: ''!
selection | |
	para text empty
	  ifTrue: [^para copy].
	^para copy: c1 to: c2 - 1
! !
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
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
!TextImage methodsFor: 'undefined' stamp: ''!
presson: press in: r | |
	^para presson: press in: r
! !
!TextImage methodsFor: 'undefined' stamp: ''!
copy | t |
	t ← TextImage new paragraph: para copy frame: frame copy style: style copy.
	t c1← c1.
	t c2← c2.
	t begintypein← begintypein.
	^t
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
chari | |
	^chari "index of last char scanned"
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
spacex | |
	^spacex
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
spacei | |
	^spacei
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
rightx | |
	^destX
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
spacecount | |
	^0 - spacecount "because counted down from zero"
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
leftx | |
	^destX - width
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
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
!TextScanner methodsFor: 'undefined' stamp: ''!
exceptionTable: t1 | |
	exceptions ← t1
! !
!TextScanner methodsFor: 'undefined' stamp: ''!
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
!TextScanner methodsFor: 'undefined' stamp: ''!
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
!TextScanner methodsFor: 'undefined' stamp: ''!
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
!TextScanner methodsFor: 'undefined' stamp: ''!
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
!TextScanner methodsFor: 'undefined' stamp: ''!
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
!TextScanner methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
lineheight | |
	^style lineheight
! !
!Textframe methodsFor: 'undefined' stamp: ''!
frame | |
	^frame
! !
!Textframe methodsFor: 'undefined' stamp: ''!
window | |
	^window
! !
!Textframe methodsFor: 'undefined' stamp: ''!
frame← t1 | |
	frame ← t1.
	window ← frame "Change my frame and window."
! !
!Textframe methodsFor: 'undefined' stamp: ''!
para | |
	^para
! !
!Textframe methodsFor: 'undefined' stamp: ''!
makeParagraph | |
	para ≡ nil "simulate ListPane for hardcopy"
	  ifTrue: [para ← 'NIL !' asParagraph]
! !
!Textframe methodsFor: 'undefined' stamp: ''!
put: t1 at: pt | |
	para ← t1.
	self put: para at: pt centered: false
! !
!Textframe methodsFor: 'undefined' stamp: ''!
erase | |
	(window inset: ¬2 ⌾ ¬2) clear
! !
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
displayall | liney |
	 "display all the lines in window"
	liney ← self displaylines: 1 to: lastline.
	self clearfrom: liney "self outline"
! !
!Textframe methodsFor: 'undefined' stamp: ''!
clearfrom: y | |
	((frame origin x ⌾ y rect: frame corner) intersect: window) clear: white
! !
!Textframe methodsFor: 'undefined' stamp: ''!
put: t1 centered: pt | |
	para ← t1.
	self put: para at: pt centered: true
! !
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
clear | |
	(frame intersect: window) clear
! !
!Textframe methodsFor: 'undefined' stamp: ''!
show | |
	self measureall.
	self displayall
! !
!Textframe methodsFor: 'undefined' stamp: ''!
outline | |
	 "from TextFrame"
	window border: 2 color: black
! !
!Textframe methodsFor: 'undefined' stamp: ''!
printer: printing | |
	^TextScanner new frame: frame window: window para: para style: style printing: printing
! !
!Textframe methodsFor: 'undefined' stamp: ''!
comp | |
	window comp
! !
!Textframe methodsFor: 'undefined' stamp: ''!
show: t1 | |
	para ← t1.
	self measureall.
	self displayall
! !
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
style | |
	^style
! !
!Textframe methodsFor: 'undefined' stamp: ''!
style: t1 | |
	style ← t1
! !
!Textframe methodsFor: 'undefined' stamp: ''!
para: t1 frame: t2 | |
	para ← t1.
	frame ← t2.
	self frame: frame window: frame para: para style: DefaultTextStyle
! !
!Textframe methodsFor: 'undefined' stamp: ''!
para: t1 frame: t2 style: t3 | |
	para ← t1.
	frame ← t2.
	style ← t3.
	self frame: frame window: frame para: para style: style
! !
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
classInit | |
	space ← 32.
	cr ← 13.
	tab ← 9
! !
!Textframe methodsFor: 'undefined' stamp: ''!
line: i ← line | |
	 "store a line, track last, and grow lines if necessary"
	i > lastline
	  ifTrue: [lastline ← i].
	lastline > lines length
	  ifTrue: [lines ← lines grow].
	^lines ◦ i ← line
! !
!Textframe methodsFor: 'undefined' stamp: ''!
lineofchar: index | i |
	 "Return line number (may be 0)"
	(1 to: lastline) do:
		[:i | index ≤ (lines ◦ i) stopi
		  ifTrue: [^i]].
	^lastline
! !
!Textframe methodsFor: 'undefined' stamp: ''!
adjustLines: i by: delta | |
	 "add to indices of charlines beginning with stopi in i"
	lines ◦ i stopi← (lines ◦ i) stopi + delta.
	(i + 1 to: lastline) do: [:i | lines ◦ i slide: delta]
! !
!Textframe methodsFor: 'undefined' stamp: ''!
trimLinesTo: last | i |
	(last + 1 to: lastline) do: [:i | lines ◦ i ← nil].
	(lastline ← last) < (lines length / 2)
	  ifTrue: [lines ← lines growby: 0 - (lines length / 2)]
! !
!Textframe methodsFor: 'undefined' stamp: ''!
yofline: line | |
	 "top y of given line"
	^frame minY + (line - 1 * style lineheight)
! !
!Textframe methodsFor: 'undefined' stamp: ''!
lineofy: y | |
	 "Return line number for a given y (may be out of range!)"
	^y - frame minY / style lineheight + 1 min: lastline
! !
!Textframe methodsFor: 'undefined' stamp: ''!
maxx: char | i rect maxx |
	self measureall.
	maxx ← 0.
	(1 to: lastline) do:
		[:i | self pointofchar: (lines ◦ i) stopi andrect⦂ [:rect | rect].
		rect corner x > maxx
		  ifTrue: [maxx ← rect corner x]].
	^maxx ⌾ rect corner y
! !
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
scrolln: n | ignored |
	^self charofpoint: frame corner x ⌾ (frame origin y + (n * style lineheight)) andrect⦂ [:ignored | ignored]
! !
!Textframe methodsFor: 'undefined' stamp: ''!
lastshown | ignored |
	^self charofpoint: window corner andrect⦂ [:ignored | ignored]
! !
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
hardcopy | pf |
	user displayoffwhile⦂ [
		(pf ← dp0 pressfile: 'frame.press'.
		window hardcopy: pf.
		self hardcopy: pf.
		pf close.
		pf toPrinter)]
! !
!Textframe methodsFor: 'undefined' stamp: ''!
takeCursor | |
	user cursorloc← window center "Move the cursor to the center of my window."
! !
!Textframe methodsFor: 'undefined' stamp: ''!
aboutToFrame | |
	 "My frame is about to change.  I dont care."
! !
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Textframe methodsFor: 'undefined' stamp: ''!
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
!Time methodsFor: 'undefined' stamp: ''!
= aTime | |
	^h = aTime hours and: [m = aTime minutes and: [s = aTime seconds]]
! !
!Time methodsFor: 'undefined' stamp: ''!
< aTime | |
	h ≠ aTime hours ifTrue: [^h < aTime hours].
	m ≠ aTime minutes ifTrue: [^m < aTime minutes].
	^s < aTime seconds
! !
!Time methodsFor: 'undefined' stamp: ''!
- aTime | |
	^Time new fromSeconds: (self asSeconds - aTime asSeconds)
! !
!Time methodsFor: 'undefined' stamp: ''!
+ aTime | |
	^Time new fromSeconds: (self asSeconds + aTime asSeconds)
! !
!Time methodsFor: 'undefined' stamp: ''!
seconds | |
	^s
! !
!Time methodsFor: 'undefined' stamp: ''!
minutes | |
	^m
! !
!Time methodsFor: 'undefined' stamp: ''!
asSeconds | |
	^3600 * h + (60 * m + s)
! !
!Time methodsFor: 'undefined' stamp: ''!
hours | |
	^h
! !
!Time methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	 "Format is h:mm:ss am/pm"
	strm print: (h > 12
	  ifTrue: [h - 12]
	  ifFalse:
		[h < 1
		  ifTrue: [12]
		  ifFalse: [h]]).
	strm append: (m < 10
	  ifTrue: [':0']
	  ifFalse: [':']).
	strm print: m.
	strm append: (s < 10
	  ifTrue: [':0']
	  ifFalse: [':']).
	strm print: s.
	strm space append: (h < 12
	  ifTrue: ['am']
	  ifFalse: ['pm'])
! !
!Time methodsFor: 'undefined' stamp: ''!
fromSeconds: sec | |
	"seconds since midnight (adjusted for time zone and DST)" 
	sec ← sec asInteger divMod: 3600.
	h ← (sec ◦ 1) asSmall.
	sec ← (sec ◦ 2) asSmall.
	m ← sec // 60.
	s ← sec \\ 60.
! !
!Time methodsFor: 'undefined' stamp: ''!
default | |
	^user now "right now" ◦ 2
! !
!Time methodsFor: 'undefined' stamp: ''!
hours: t1 | |
	h ← t1
! !
!Time methodsFor: 'undefined' stamp: ''!
minutes: t1 | |
	m ← t1
! !
!Time methodsFor: 'undefined' stamp: ''!
seconds: t1 | |
	s ← t1
! !
!Timer methodsFor: 'undefined' stamp: ''!
reset | nextTimeout foundit |
	Top critical⦂ [
		 "Set up this Timer to add <action> to the Queue of remote Contexts to be evaled after an interval of <delay> sixtieths of a second. Find the proper place in the doubly linked list and calculate the amount of time to run after the preceeding timer fires"
		(activeTime ≡ nil
		  ifFalse: [self disable].
		activeTime ← delay.
		nextTimer ← currentTimer.
		lastTimer ← nil.
		foundit ← false.
		[foundit] whileFalseDo: 
			[nextTimer ≡ nil
			  ifTrue: [foundit ← true]
			  ifFalse:
				[.
				(nextTimeout ← nextTimer activetime) > activeTime
				  ifTrue: [foundit ← true]
				  ifFalse:
					[.
					activeTime ← activeTime - nextTimeout.
					lastTimer ← nextTimer.
					nextTimer ← lastTimer nexttimer]]].
		nextTimer ≡ nil
		  ifFalse: [nextTimer insertlast: self].
		lastTimer ≡ nil
		  ifTrue: [self startup]
		  ifFalse: [lastTimer insertnext: self])]
! !
!Timer methodsFor: 'undefined' stamp: ''!
for: t1 action⦂ t2 | |
	delay ← t1.
	action ← t2 "Initialize a new Timer"
! !
!Timer methodsFor: 'undefined' stamp: ''!
classInit | |
	 "Initialize the processes used by the Timers"
	timerActions ← Queue new of: (Vector new: 4).
	self init16.
	self init12
! !
!Timer methodsFor: 'undefined' stamp: ''!
init12 | nextAction |
	 "Initialize the process which evals Timer actions"
	Top install⦂ [([true] whileTrueDo:
			[[nextAction ← timerActions next] whileTrueDo: [nextAction eval].
			Top sleep: 12])] at: 12.
	Top enable: 12
! !
!Timer methodsFor: 'undefined' stamp: ''!
init16 | |
	 "Initialize the process wakened by a Timer timing out"
	Top install⦂ [([true] whileTrueDo:
			[currentTimer fire.
			Top sleep: 16])] at: 16.
	Top enable: 16
! !
!Timer methodsFor: 'undefined' stamp: ''!
release | |
	lastTimer ← nil.
	nextTimer ← nil.
	action ← nil
! !
!Timer methodsFor: 'undefined' stamp: ''!
lasttimer | |
	^lastTimer
! !
!Timer methodsFor: 'undefined' stamp: ''!
insertnext: t1 | |
	nextTimer ← t1
! !
!Timer methodsFor: 'undefined' stamp: ''!
deletenext | |
	nextTimer ← nextTimer nexttimer "Delete the Timer after this one"
! !
!Timer methodsFor: 'undefined' stamp: ''!
deletelast | |
	activeTime ← activeTime + lastTimer activetime "Delete the Timer before this one. When deleting a Timer, the activeTime of the Timer after it must be increased by its activeTime".
	(lastTimer ← lastTimer lasttimer) ≡ nil
	  ifTrue: [self startup]
! !
!Timer methodsFor: 'undefined' stamp: ''!
insertlast: t1 | |
	lastTimer ← t1.
	activeTime ← self activetime "Insert a new Timer before this one. When inserting a Timer in front of another, the activeTime of the later one must be reduced so it is the amount of time after the new Timers firing" - lastTimer activetime
! !
!Timer methodsFor: 'undefined' stamp: ''!
nexttimer | |
	^nextTimer
! !
!Timer methodsFor: 'undefined' stamp: ''!
shutoff | |<primitive: 75>
	currentTimer ← nil "this message informs the virtual machine and class Timer that there are no more Timers to fire"
! !
!Timer methodsFor: 'undefined' stamp: ''!
primstartup | |<primitive: 75>
	 "this message informs the virtual machine that this is the next Timer to fire"
	
! !
!Timer methodsFor: 'undefined' stamp: ''!
activetime | |<primitive: 75>
	^activeTime "If this is the current Timer return the time until it fires, otherwise return activeTime"
! !
!Timer methodsFor: 'undefined' stamp: ''!
startup | |
	lastTimer ← nil "make this the next Timer to fire".
	currentTimer ← self.
	self primstartup
! !
!Timer methodsFor: 'undefined' stamp: ''!
disable | |
	 "Remove this timer from the list"
	Top critical⦂ [
		((self ≡ currentTimer and: [nextTimer ≡ nil])
		  ifTrue:
			[self shutoff.
			Top deepsleep: 16].
		lastTimer ≡ nil
		  ifFalse: [lastTimer deletenext].
		nextTimer ≡ nil
		  ifFalse: [nextTimer deletelast].
		activeTime ← nil.
		lastTimer ← nil.
		nextTimer ← nil)]
! !
!Timer methodsFor: 'undefined' stamp: ''!
fire | |
	 "Time is up, add the action to the Queue to be evaled"
	timerActions next← action.
	Top wakeup: 12.
	activeTime ← nil.
	lastTimer ← nil.
	nextTimer ≡ nil
	  ifTrue: [self shutoff]
	  ifFalse:
		[nextTimer startup.
		nextTimer ← nil]
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
separator: c | |
	
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
identifier: s | |
	self next← s unique
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
float: i fraction: f exp: e | |
	self next← (i + '.' + f + 'e' + e) asFloat
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
string: s | |
	self next← s
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
otheratom: s | |
	self next← s unique
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
rightparen | |
	parenstack empty
	  ifFalse:
		[ "Error will be caught elsewhere"
		parenstack last next← sink contents.
		sink ← parenstack pop]
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
leftparen | |
	parenstack next← sink.
	sink ← (Vector new: 10) asStream
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
integer: s | |
	self next← s asInteger
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
keyword: s | |
	self next← s unique
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
onechar: c | x |
	x ← String new: 1.
	x ◦ 1 ← c.
	self next← x unique
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
trailer: s | |
	
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
comment: s | |
	
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
next← obj | |
	sink next← obj "subclasses can override easily"
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
contents | |
	 "Close all parentheses first"
	[parenstack empty] whileFalseDo:  [self rightparen].
	^sink contents
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
notify: errorString | |
	user notify: errorString
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
to: v | |
	 "Initialize"
	sink ← v asStream.
	parenstack ← (Vector new: 5) asStream
! !
!TokenCollector methodsFor: 'undefined' stamp: ''!
default | |
	self to: (Vector new: 20)
! !
!Turtle methodsFor: 'undefined' stamp: ''!
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
!Turtle methodsFor: 'undefined' stamp: ''!
white | |
	self screen: white
! !
!Turtle methodsFor: 'undefined' stamp: ''!
frame | |
	^frame
! !
!Turtle methodsFor: 'undefined' stamp: ''!
init | |
	source ← sourceRaster ← (NoteTaker
					  ifFalse: [0]).
	super init.
	super toDisplay.
	frame ← user screenrect.
	self pendn.
	self color: black.
	self width: 1.
	self inking: storing.
	self home.
	self up " | n i [n←Turtle init. n color: gray; width: 4; inking: oring.
	 for⦂ i to: 50 do⦂ [n go: i*4; turn: 89]]"
! !
!Turtle methodsFor: 'undefined' stamp: ''!
frame: t1 | |
	frame ← t1
! !
!Turtle methodsFor: 'undefined' stamp: ''!
inking: t1 | |
	inking ← t1
! !
!Turtle methodsFor: 'undefined' stamp: ''!
turn: degrees | |
	direction ← direction + degrees
! !
!Turtle methodsFor: 'undefined' stamp: ''!
goto: p | old |
	old ← location.
	location ← p.
	penDown
	  ifTrue: [self drawfrom: old to: location]
! !
!Turtle methodsFor: 'undefined' stamp: ''!
penup | |
	penDown ← false
! !
!Turtle methodsFor: 'undefined' stamp: ''!
pendn | |
	penDown ← true
! !
!Turtle methodsFor: 'undefined' stamp: ''!
up | |
	direction ← 270.0
! !
!Turtle methodsFor: 'undefined' stamp: ''!
go: dist | |
	self goto: direction asRadians asDirection * dist + location
! !
!Turtle methodsFor: 'undefined' stamp: ''!
erase | |
	super window clear: white
! !
!Turtle methodsFor: 'undefined' stamp: ''!
width | |
	^width
! !
!Turtle methodsFor: 'undefined' stamp: ''!
black | |
	self screen: black
! !
!Turtle methodsFor: 'undefined' stamp: ''!
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
!Turtle methodsFor: 'undefined' stamp: ''!
place: t1 | |
	location ← t1
! !
!Turtle methodsFor: 'undefined' stamp: ''!
color: color | |
	self screen: color
! !
!Turtle methodsFor: 'undefined' stamp: ''!
home | |
	location ← frame center
! !
!Turtle methodsFor: 'undefined' stamp: ''!
width: w | |
	self pen: w
! !
!Turtle methodsFor: 'undefined' stamp: ''!
pen: t1 | |
	pen ← t1
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
hash | |<primitive: 39>
	
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
stringhash | |
	^super hash
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
= x | |
	^self ≡ x
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
species | |
	^String
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
copy | |
	^self
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
recopy | |
	^self
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
printOn: strm | |
	strm append: self
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
asString | |
	^super copy
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
classInit | i a v |
	 "make up table of 1-char atoms"
	v ← Vector new: 128.
	a ← String new: 1.
	(1 to: 128) do:
		[:i | a ◦ 1 ← i - 1.
		v ◦ i ← a unique].
	UST1 ← v
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
intern: s | ustr h |
	(ustr ← self hasInterned: s)
	  ifTrue: [^ustr].
	 "check if already exists"
	ustr ← ((s is: UniqueString)
			  ifTrue: [s]
			  ifFalse: [ "create a new one"
				(UniqueString new: s length) str: s]).
	h ← s stringhash \ USTable length + 1.
	USTable ◦ h ← {USTable ◦ h , ustr}.
	^ustr
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
hasInterned: s | i v |
	 "⇑false if String s hasnt been interned, else ⇑s unique"
	s length = 1
	  ifTrue:
		[s ◦ 1 < 128
		  ifTrue: [^UST1 ◦ (s ◦ 1 + 1)]].
	v ← USTable ◦ (s stringhash \ USTable length + 1).
	(1 to: v length) do:
		[:i | v ◦ i ≡ nil
		  ifFalse:
			[s length = (v ◦ i) length
			  ifTrue:
				[s = (v ◦ i)
				  ifTrue: [^v ◦ i]]]].
	^false
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
str: s | j |
	(1 to: s length) do: [:j | super ◦ j ← s ◦ j].
	^self
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
rehash | old s i |
	 " ↪a rehash. "
	old ← USTable.
	USTable ← Vector new: old length.
	(1 to: old length) do:
		[:i | USTable ◦ i ← Vector new: 0.
		old ◦ i do:
			[:s | s ≡ nil
			  ifFalse: [self intern: s]].
		old ◦ i ← nil]
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
unique | |
	
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
◦ x ← val | |
	user notify: 'UniqueStrings are not for writing into'
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
isuneval | x |
	 "ends with open colon"
	^self ◦ self length = 3
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
isarrow | |
	 "ends with ←"
	self length ≤ 1
	  ifTrue: [^false].
	^self ◦ self length = 95
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
iskeyword | x |
	 "ends with colon"
	self length ≤ 1
	  ifTrue: [^false].
	x ← self ◦ self length.
	x = 58
	  ifTrue: [^true].
	^x = 3
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
keywords | result strm i l char colon ocolon |
	 "return a vector of the keywords that compose me"
	'◦←' = self
	  ifTrue: [^#('◦' '←' )].
	result ← (Vector new: 10) asStream.
	strm ← Stream default.
	colon ← ':' ◦ 1.
	ocolon ← '⦂' ◦ 1.
	i ← 1.
	l ← self length.
	[i ≤ l] whileTrueDo:
		[char ← self ◦ i.
		strm append: char.
		((char = colon or: [char = ocolon]) or: [i = l])
		  ifTrue:
			[result next← strm contents.
			strm reset].
		i ← i + 1].
	^result contents
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
numArgs | len n i t4 |
	 "the number of arguments I take when I am a selector"
	len ← self length.
	len = 1
	  ifTrue: [^(self ◦ 1) isletter
		  ifTrue: [0]
		  ifFalse: [1]].
	n ← 0 "count colons, dots, and arrows".
	(1 to: len) do:
		[:i | (t4 ← self ◦ i) = 58
		  ifTrue: [n ← n + 1]
		  ifFalse:
			[t4 = 3
			  ifTrue: [n ← n + 1]
			  ifFalse:
				[t4 = 95
				  ifTrue: [n ← n + 1]
				  ifFalse:
					[t4 = 7
					  ifTrue: [n ← n + 1]]]]].
	^n
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
mustTake: nargs | |
	 "fatal error if I am not a selector that takes nargs arguments"
	self numArgs ≠ nargs
	  ifTrue: [user notify: self + ' does not take ' + nargs asString + ' arguments']
! !
!UniqueString methodsFor: 'undefined' stamp: ''!
isinfix | x |
	self length ≠ 1
	  ifTrue: [^false].
	^(self ◦ 1) isletter ≡ false
! !
!UserView methodsFor: 'undefined' stamp: ''!
displayHeight: h | | 
	h ← h | 16.
	DisplayForm refct>3
	  ifTrue: [user croak]
	  ifFalse:
		[user currentDisplay: (Form new extent: 640 ⌾ 16 bits: (String new: 16 * 80) offset: nil).
		user currentDisplay: (Form new extent: 640 ⌾ h bits: (String new: h * 80) offset: nil)].
	user restore
! !
!UserView methodsFor: 'undefined' stamp: ''!
primMousePt | |<primitive: 42>
	^self croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
primMouseKeys | |<primitive: 46>
	^self croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
primCursorLoc← pt | |<primitive: 43>
	^self croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
cursorLink: bool | |<primitive: 75>
	^self croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
mapDisplay | t1 |
	(DisplayBLTer ← BitBlt init) sourceForm: DisplayForm.
	DisplayBLTer destForm: DisplayForm.
	(t1 ← BitBlt init) sourceForm: currentCursor asForm.
	t1 destForm: DisplayForm.
	t1 sourceRect: (0 ⌾ 0 rect: currentCursor extent).
	t1 effect: oring.
	t1 installDisplay
! !
!UserView methodsFor: 'undefined' stamp: ''!
primKbdNext | |<primitive: 45>
	^self croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
primKbdPeek | |<primitive: 44>
	^self croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
currentDisplay: form | |
	DisplayForm ← form.
	screenrect ← 0 ⌾ 0 rect: form extent.
	vtab ← htab ← mxoffset ← myoffset ← 0.
	self mapDisplay
! !
!UserView methodsFor: 'undefined' stamp: ''!
primKbdBlock | |<primitive: 47>
	^self croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
changedClasses | titles space str |
	space ← ' ' ◦ 1 "return a vector of the names of classes which have been changed".
	titles ← HashSet new init.
	Changes contents do:
		[:str | titles insert: ((Stream new of: str) upto: space) "class title"].
	^titles contents
! !
!UserView methodsFor: 'undefined' stamp: ''!
changedCategories | titles space str |
	space ← ' ' ◦ 1 "return a vector of the names of class categories which have been changed".
	titles ← HashSet new init.
	Changes contents do: [:str | titles insert: (SystemOrganization invert: ((Stream new of: str) upto: space) unique)].
	^titles contents
! !
!UserView methodsFor: 'undefined' stamp: ''!
noChanges | |
	Changes init
! !
!UserView methodsFor: 'undefined' stamp: ''!
changedMessages | |
	^Changes contents sort
! !
!UserView methodsFor: 'undefined' stamp: ''!
newdisploc: origin and: corner | |
	 "for moving disp"
	(disp text "user newdisploc: 8⌾0 and: 150⌾96" frame inset: ¬2 ⌾ ¬2) clear.
	disp text frame← origin rect: corner.
	disp show
! !
!UserView methodsFor: 'undefined' stamp: ''!
newdisp | |
	 "for when some class associated with running Dispframe  changed"
	self unschedule: disp.
	disp ← Dispframe new rect: (8 ⌾ 0 rect: 150 ⌾ 96).
	self schedule: disp.
	self clearshow: 'New Dialogue window created.
'
! !
!UserView methodsFor: 'undefined' stamp: ''!
print: x | |
	disp print: x.
	disp show
! !
!UserView methodsFor: 'undefined' stamp: ''!
ev | |
	disp ev
! !
!UserView methodsFor: 'undefined' stamp: ''!
tab | |
	disp tab
! !
!UserView methodsFor: 'undefined' stamp: ''!
cr | |
	disp cr
! !
!UserView methodsFor: 'undefined' stamp: ''!
frame | |
	 "return rectangle of dialogue window"
	^disp text frame
! !
!UserView methodsFor: 'undefined' stamp: ''!
clear | |
	 "clear disp of debris and characters"
	disp clear
! !
!UserView methodsFor: 'undefined' stamp: ''!
show | |
	disp outline.
	disp show
! !
!UserView methodsFor: 'undefined' stamp: ''!
read | |
	^disp read
! !
!UserView methodsFor: 'undefined' stamp: ''!
clearshow: str | |
	disp clear.
	disp append: str.
	disp show
! !
!UserView methodsFor: 'undefined' stamp: ''!
next← x | |
	disp cr "simulate a Vector Stream".
	disp print: x.
	disp show
! !
!UserView methodsFor: 'undefined' stamp: ''!
show: str | |
	disp append: str.
	disp show
! !
!UserView methodsFor: 'undefined' stamp: ''!
request: s | |
	^disp request: s
! !
!UserView methodsFor: 'undefined' stamp: ''!
croak | |
	self notify: 'A primitive has failed.'
! !
!UserView methodsFor: 'undefined' stamp: ''!
space | |
	disp space
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
allImplementors: lit | result cl |
	result ← (Vector new: 10) asStream.
	Class allInstancesDo:
		[:cl| (cl canunderstand: lit) ifTrue: [result next← cl title]].
	^result contents
! !
!UserView methodsFor: 'undefined' stamp: ''!
allSenders: lit | result cl lis |
	result ← (Vector new: 10) asStream.
	Class allInstancesDo:
		[:cl| lis ← cl whosends: lit.
		lis length>0 ifTrue: [result next← {cl title, lis}]].
	^result contents
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
purgealittle | |<primitive: 0>
	
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
coreLeft | | <primitive: 48>
	user croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
releaseMessage | |
	user clearshow: 'Welcome to ' + user version
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
writeSources: newSources | |
	user file: newSources classes: SystemOrganization changesOnly: newSources end "write a new Sources file (usually on [phylum]Smalltalk.Sources.xxx
		(i.e. xxx = user versionName))
	if it's a new file or empty, write all Sources. otherwise it better be a copy of
	the previous Sources file (only changes will be appended. do the copy with ftp)" ≡ false.
	Sources close.
	Sources ← newSources
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
versionName | s |
	s ← self version asStream "skip Smalltalk".
	s skipTo: 32 "return version identification, e.g. 5.5f".
	^s upto: 32
! !
!UserView methodsFor: 'undefined' stamp: ''!
cursorloc← pt | |
	^self primCursorLoc← pt - (mxoffset ⌾ myoffset)
! !
!UserView methodsFor: 'undefined' stamp: ''!
currentCursor: c | coff p |
	currentCursor ← c.
	coff ← c offset.
	p ← self mp.
	mxoffset ← coff x - htab.
	myoffset ← coff y - vtab.
	self cursorloc← p "Cursor ref pt contin across boundary".
	self mapDisplay
! !
!UserView methodsFor: 'undefined' stamp: ''!
mp | |
	^self primMousePt + (mxoffset ⌾ myoffset)
! !
!UserView methodsFor: 'undefined' stamp: ''!
buttons | |
	^self primMouseKeys land: 7
! !
!UserView methodsFor: 'undefined' stamp: ''!
tabletrelative | |
	mem ◦ 86 ← ¬1
! !
!UserView methodsFor: 'undefined' stamp: ''!
waitclickbug | |
	self waitnobug.
	^self waitbug
! !
!UserView methodsFor: 'undefined' stamp: ''!
waitnokey | |
	[self keyset = 0] whileFalseDo:  [self rawkbck]
! !
!UserView methodsFor: 'undefined' stamp: ''!
tabletbug | |
	^mem ◦ ¬448 < 0
! !
!UserView methodsFor: 'undefined' stamp: ''!
tabletabsolute | |
	mem ◦ 86 ← 1
! !
!UserView methodsFor: 'undefined' stamp: ''!
redbug | |
	^self buttons = 4
! !
!UserView methodsFor: 'undefined' stamp: ''!
anybug | |
	^self buttons > 0
! !
!UserView methodsFor: 'undefined' stamp: ''!
mpnext | |
	self redbug "return next mouse point if red button or tablet is down; otherwise false"
	  ifTrue: [^self mp].
	^false
! !
!UserView methodsFor: 'undefined' stamp: ''!
kbd | |
	[self rawkbck] whileFalseDo: 
		[NoteTaker
		  ifFalse: [self purgealittle]].
	^kbMap ◦ self rawkbd
! !
!UserView methodsFor: 'undefined' stamp: ''!
y | |
	^self mp y
! !
!UserView methodsFor: 'undefined' stamp: ''!
x | |
	^self mp x
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
anykeys | |
	^self keyset > 0
! !
!UserView methodsFor: 'undefined' stamp: ''!
bluebug | |
	^self buttons = 1
! !
!UserView methodsFor: 'undefined' stamp: ''!
nobug | |
	^self anybug ≡ false
! !
!UserView methodsFor: 'undefined' stamp: ''!
yellowbug | |
	^self buttons "From Smalltalk 5.4i on 30 October 1979 at 2:28:57 pm." = 2
! !
!UserView methodsFor: 'undefined' stamp: ''!
waitbug | |
	[self anybug] whileFalseDo:  [].
	^self mp
! !
!UserView methodsFor: 'undefined' stamp: ''!
kbck | t |
	(t ← self rawkbck)
	  ifTrue: [^kbMap ◦ t].
	NoteTaker
	  ifTrue: [^false].
	self purgealittle.
	^false
! !
!UserView methodsFor: 'undefined' stamp: ''!
waitnobug | |
	[self anybug] whileTrueDo: [].
	^self mp
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
tablet | |
	^mem ◦ ¬448 ≠ 0
! !
!UserView methodsFor: 'undefined' stamp: ''!
leftShiftKey | |
	 "left shift key down?"
	NoteTaker
	  ifTrue: [^false].
	^(mem ◦ ¬482 land: 64) = 0
! !
!UserView methodsFor: 'undefined' stamp: ''!
currentCursor | |
	^currentCursor
! !
!UserView methodsFor: 'undefined' stamp: ''!
keyset | |
	^(self primMouseKeys lshift: ¬3) land: 31
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
restoredisplay | |
! !
!UserView methodsFor: 'undefined' stamp: ''!
displayoffwhile⦂ expr | |
	^expr eval  "Don't bother turning off the display"
! !
!UserView methodsFor: 'undefined' stamp: ''!
reconfigure | |<primitive: 75>
	
! !
!UserView methodsFor: 'undefined' stamp: ''!
copyIn: p | |
	^UserView new screenrect: screenrect copy vtab: vtab htab: htab scale: scale color: color projectWindow: p disp: disp sched: #()
! !
!UserView methodsFor: 'undefined' stamp: ''!
screenrect | |
	^screenrect
! !
!UserView methodsFor: 'undefined' stamp: ''!
install | |
	self screenextent: screenrect extent tab: htab ⌾ vtab
! !
!UserView methodsFor: 'undefined' stamp: ''!
color: t1 scale: t2 | |
	color ← t1.
	scale ← t2.
	self install
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
projectWindow | |
	projectWindow ≡ nil
	  ifTrue:
		[projectWindow ← ProjectWindow new.
		projectWindow userview: self changes: Changes parent: projectWindow].
	^projectWindow
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	InitialContext ← thisContext.
	self snapshotPrimitive.
	self mapDisplay.
	self restore
! !
!UserView methodsFor: 'undefined' stamp: ''!
releaseExternalViews | t |
	SourceFiles notNil do: [:t | t close].
	dp0 close.
	dp1 close.
	(externalViews length to: 1 by: ¬1) do: "release (obsolete) known external views"
		[:t | (externalViews ◦ t) release.
		externalViews ◦ t ← nil].
	externalViews reset
! !
!UserView methodsFor: 'undefined' stamp: ''!
overlay: fileid | |
	self releaseExternalViews.
	E ≡ nil
	  ifFalse: [E sleep]. "put the ethernet to sleep"
	self InLd: fileid.
	[user keyset > 0] "we start here after a resume"
		whileTrueDo: [user show: 'The keyset is stuck'.
		user cr]
! !
!UserView methodsFor: 'undefined' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	InitialContext ← thisContext.
	self snapshotPrimitive.
	self mapDisplay.
	self restore
! !
!UserView methodsFor: 'undefined' stamp: ''!
quitFrom: controller | |
	self overlay: #(0 0 0 0 0 ).
	screenrect clear.
	controller restore
! !
!UserView methodsFor: 'undefined' stamp: ''!
newSourceFile: f || "user newSourceFile: (dp0 file: 'new.sources').
				then snapshot and copy into st80dec25"
	f append: 'ST80 sources of January 13'; cr. "should be more appropriate header"
	CompiledMethod allInstances do: [:meth | meth moveSourceTo: f].
	f close.
! !
!UserView methodsFor: 'undefined' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	InitialContext ← thisContext.
	self snapshotPrimitive.
	self mapDisplay.
	SourceFiles◦2 ← (dp0 file: 'st80changes.v00') readonly. "Avoid remap problem for now"
	self restore
! !
!UserView methodsFor: 'undefined' stamp: ''!
backup | |
	 "back up smalltalk on ivy and resume"
	ivy open.
	ivy delete: 'small.boot'.
	ivy store: 'small.boot'.
	ivy close
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
Swat | |<primitive: 75>
	
! !
!UserView methodsFor: 'undefined' stamp: ''!
quit | |
	self quitFrom: self "yup"
! !
!UserView methodsFor: 'undefined' stamp: ''!
st80FileInit: version | |
	 " user st80FileInit: 'ST80dec25'. "
	(SourceFiles ◦ 1) file rename: version+'.sources'.
	(SourceFiles ◦ 1) readonly.
	(SourceFiles ◦ 2) file rename: version+'.changes'.
	(SourceFiles ◦ 2) readonly
! !
!UserView methodsFor: 'undefined' stamp: ''!
st80FileInit | |
	 " user st80FileInit. "
	dp0 release.
	SourceFiles ← Vector new: 4.
	SourceFiles ◦ 1 ← dp0 file: 'ST80Sources.v00'.
	(SourceFiles ◦ 1) readonly.
	SourceFiles ◦ 2 ← dp0 file: 'ST80Changes.v00'.
	(SourceFiles ◦ 2) readonly
! !
!UserView methodsFor: 'undefined' stamp: ''!
snapshotPrimitive | |<primitive: 83>
	 "write the OT and Data of this Smalltalk out"
	user croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
InLd: fileid | |<primitive: 75>
	 "write out the core image, then load in OS"
	user notify: 'file problem'
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
snapshot | i |
	 "write the OT and Data of this Smalltalk out"
	self releaseExternalViews.
	self snapshotPrimitive.
	self mapDisplay.
	self restore
! !
!UserView methodsFor: 'undefined' stamp: ''!
timeWordsInto: s | |<primitive: 81>
	 "s length=4!"
	user croak
! !
!UserView methodsFor: 'undefined' stamp: ''!
rawtotalsecs | |
	^self rawtotalsecs: self timewords
! !
!UserView methodsFor: 'undefined' stamp: ''!
totalsecs | |
	^self totalsecs: self timewords
! !
!UserView methodsFor: 'undefined' stamp: ''!
timewords | s |
	s ← String new: 4 "seconds (in GMT) since Jan 1 1901: as a String".
	self timeWordsInto: s.
	^s
! !
!UserView methodsFor: 'undefined' stamp: ''!
now | |
	^self dateAndTime: self timewords
! !
!UserView methodsFor: 'undefined' stamp: ''!
time | |
	^self now ◦ 2
! !
!UserView methodsFor: 'undefined' stamp: ''!
totalsecs: secs | |
	^self convertTime: (self rawtotalsecs: secs) "convert from GMT to local and correct for Daylight Savings" returnSecs: true
! !
!UserView methodsFor: 'undefined' stamp: ''!
rawtotalsecs: secs | s |
	s ← Natural new: 4 "secs is a String of 4 characters representing seconds (in GMT) since Jan 1 1901.
	copy (in reverse order) to a Natural string, then return a LargeInteger".
	s ◦ 1 ← secs ◦ 4.
	s ◦ 2 ← secs ◦ 3.
	s ◦ 3 ← secs ◦ 2.
	s ◦ 4 ← secs ◦ 1.
	^LargeInteger new bytes: s neg: false
! !
!UserView methodsFor: 'undefined' stamp: ''!
dateAndTime: secs | |
	^self convertTime: (self rawtotalsecs: secs) "secs is a String of 4 characters representing seconds (in GMT) since Jan 1 1901.
	convert it to a LargeInteger (rawtotalsecs:), then return a Vector (Date, Time),
	which is corrected for local time zone and daylight savings" returnSecs: false
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
ticks | |
	 "Return the 38.08-millisecond interval timer"
	^mem ◦ 280
! !
!UserView methodsFor: 'undefined' stamp: ''!
time⦂ expr | t |
	t ← self ticks.
	expr eval.
	^self ticks - t
! !
!UserView methodsFor: 'undefined' stamp: ''!
today | |
	^self now ◦ 1
! !
!UserView methodsFor: 'undefined' stamp: ''!
restore | w |
	screenrect clear.
	projectWindow ≡ nil
	  ifFalse: [projectWindow putTitle].
	disp outline.
	(sched length to: 1 by: ¬1) do: [:w | (sched ◦ w) show]
! !
!UserView methodsFor: 'undefined' stamp: ''!
restart | i |
	Events ≡ nil
	  ifTrue:
		["Events ← EventQueue init. Top init3.  initialize Event queue and Time interrupt"].
	NormalCursor topage1.
	self restart⦂ [(user run)]
! !
!UserView methodsFor: 'undefined' stamp: ''!
schedule: window | |
	sched ≡ nil
	  ifTrue: [sched ← window inVector]
	  ifFalse: [sched ← window inVector concat: sched]
! !
!UserView methodsFor: 'undefined' stamp: ''!
leaveTop | |
	 "leave the top window if there is one"
	sched length = 0
	  ifFalse: [(sched ◦ 1) leave]
! !
!UserView methodsFor: 'undefined' stamp: ''!
run | |
	self run: false
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
topWindow | |
	^sched ◦ 1
! !
!UserView methodsFor: 'undefined' stamp: ''!
promote: window | |
	sched promote: window
! !
!UserView methodsFor: 'undefined' stamp: ''!
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
!UserView methodsFor: 'undefined' stamp: ''!
unschedule: window | t |
	0 < (t ← sched find: window)
	  ifTrue: [sched ← sched ◦ (1 to: t - 1) concat: sched ◦ (t + 1 to: sched length)]
! !
!UserView methodsFor: 'undefined' stamp: ''!
scheduleOnBottom: window | |
	sched ≡ nil
	  ifTrue: [sched ← window asVector]
	  ifFalse: [sched ← sched concat: window asVector]
! !
!UserView methodsFor: 'undefined' stamp: ''!
sched | |
	^sched
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
to: t1 | |
	valuePane ← t1
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
names: vars values: t2 wrt: t3 | |
	values ← t2.
	context ← t3.
	self of: vars
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
classInit | |
	varmenu ← Menu new string: 'inspect'
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
deselected | |
	valuePane showing: ''
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
selected | |
	valuePane showing: self value asString
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
compile: parag | |
	window flash.
	^false
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
execute: parseStream for: t2 | |
	valuePane ← t2.
	^valuePane execute: parseStream in: context to: values ◦ 1
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
value | |
	selection = 1
	  ifTrue: [^values ◦ 1].
	^values ◦ 2 inspectfield: selection - 1
! !
!VariablePane methodsFor: 'undefined' stamp: ''!
yellowbug | |
	selection = 0
	  ifTrue: [window flash]
	  ifFalse: [scrollBar hidewhile⦂ [
			(varmenu bug = 1
			  ifTrue: [self value inspect])]]
! !
!Vector methodsFor: 'undefined' stamp: ''!
base: b | strm i |
	 "convert vector of numbers to a base"
	strm ← Stream default.
	(1 to: self length) do:
		[:i | strm append: (self ◦ i base: b).
		strm space].
	^strm contents
! !
!Vector methodsFor: 'undefined' stamp: ''!
hex | s y |
	s ← (String new: 4 * self length) asStream.
	self do: [:y | s append: y hex].
	^s contents
! !
!Vector methodsFor: 'undefined' stamp: ''!
firstPush | |
	^(self ◦ 1) firstPush
! !
!Vector methodsFor: 'undefined' stamp: ''!
emitForValue: code on: stack | x |
	self do: [:x | x emitForValue: code on: stack]
! !
!Vector methodsFor: 'undefined' stamp: ''!
sizeForValue | size x |
	size ← 0.
	self do: [:x | size ← size + x sizeForValue].
	^size
! !
!Vector methodsFor: 'undefined' stamp: ''!
remote: generator | x |
	self do: [:x | x remote: generator]
! !
!Vector methodsFor: 'undefined' stamp: ''!
argsOff: stack | |
	stack pop: self length
! !
!Vector methodsFor: 'undefined' stamp: ''!
printOn: strm | i |
	strm append: '('.
	(1 to: self length) do:
		[:i | strm print: self ◦ i.
		strm space].
	strm append: ')'
! !
!Vector methodsFor: 'undefined' stamp: ''!
asVector | |
	
! !
!Vector methodsFor: 'undefined' stamp: ''!
, x | v |
	v ← self growby: 1 "use a stream if youre in a hurry".
	v last← x.
	^v
! !
!Vector methodsFor: 'undefined' stamp: ''!
max | biggest i |
	biggest ← self ◦ 1 "return largest value in a vector".
	(1 to: self length) do:
		[:i | self ◦ i > biggest
		  ifTrue: [biggest ← self ◦ i]].
	^biggest
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
on: file | | "Smalltalk declare: #Vmem as: (VmemChecker new on: 'clone.im'). "
	(file is: String) ifTrue: [file← dp0 file: file].
	image← file readonly.
	image position← 0.
	OTbase← image nextword*65536. OTbase← OTbase+(image nextword-1/256+1*256).
	OTbase← OTbase*2+512
"
 | strm x | strm← Stream default.
1~60 do: [:x| strm print: x; space; append: (Vmem print: x*2); cr].
strm contents
"
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
stringOf: oop | refct flags addr length |
	oop odd ifTrue: [user notify: 'cant load out of an Integer'].
	image position← OTbase+(oop*2).
	refct← image next.
	flags← image next.
	addr← image nextword.
	image position← (flags land: 15)*65536+addr*2+512.
	length← image nextword.
	(flags allmask: PTRS) ifTrue: [^'not bytes'].
	length← length-2*2-((flags allmask: ODD) ifTrue: [1] ifFalse: [0]).
	image skip: 2.
	^image next: (length min: 30)
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
scan " Vmem scan. "
	| enddata refcts maxoop data oop i refct flags addr length class field nlits |
	image position← 0.
	enddata← image nextword. enddata← enddata*65536+image nextword.
	enddata← enddata*2+512.
	maxoop← image nextword. maxoop← maxoop*65536+image nextword.
	maxoop← maxoop/2-1.  refcts← String new: maxoop.
	data← (dp0 file: image name) readonly.  "avoid thrashing"
	user clear.
	image position← OTbase+4.
	1~maxoop do:
		[:oop | oop\1000=0 ifTrue: [user print: oop; space].
		refct← image next. flags← image next.
		addr← image nextword+(65536*(flags land: 15)).
		flags=8r40 ifTrue: [user print: oop; show: ' is FREE.'; cr]
			ifFalse:
			[data position← addr*2+512.
			length← data nextword.
			class← data nextword.
			class odd ifTrue: [user notify: 'Integer in class field!']
				ifFalse: [class← class lshift: ¬1].
			refcts◦class← refcts◦class+1 min: 128.
			(flags allmask: PTRS)
				ifTrue:
				[(1 to: length-2) do:
					[:i | field← data nextword.
					field even ifTrue: [field← field lshift: ¬1.
						refcts◦field← refcts◦field+1 min: 128]]]
				ifFalse: 
				[class=17 "methods"
				ifTrue:
				[data skip: 2. nlits← (data nextword land: 16r7E)/2. data skip: ¬4.
				(1 to: nlits+2) do:
					[:i | field← data nextword.
					field even ifTrue: [field← field lshift: ¬1.
						refcts◦field← refcts◦field+1 min: 128]]]]]].
	image position← OTbase+4.
	1~maxoop do:
		[:oop | refct← image next. image skip: 3.
		refct≠(refcts◦oop) ifTrue: [user show: 'oop='; print: oop; space.
				user show: 'refct='; print: refct; space.
				user show: 'actually='; print: refcts◦oop; cr; waitbug]].
	data close
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
print: oop | refct flags addr length class title | " Vmem printObj: 2 "
	oop odd ifTrue: [^'Integer='+(oop/2) asString].
	image position← OTbase+(oop*2).
	refct← image next.
	flags← image next.
	flags=8r40 ifTrue: [^'FREE'].
	addr← image nextword.
	image position← (flags land: 15)*65536+addr*2+512.
	length← image nextword.
	class← image nextword.
	title← self stringOf: (self field: 1 of: class).
	title='Class' ifTrue: [^'Class '+(self stringOf: (self field: 1 of: oop))].
	(flags nomask: PTRS) ifTrue: [^title+': '+(self stringOf: oop)].
	^'a '+title
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
field: n of: oop | refct flags addr |
	oop odd ifTrue: [user notify: 'cant load out of an Integer'].
	image position← OTbase+(oop*2).
	refct← image next.
	flags← image next.
	addr← image nextword.
	image position← (flags land: 15)*65536+addr*2+512.
	image skip: n*2+2.
	^image nextword
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
debug: oop | refct flags addr length class title strm | " Vmem printObj: 2 "
	oop odd ifTrue: [^'Integer='+(oop/2) asString].
	image position← OTbase+(oop*2).
	refct← image next.
	flags← image next.
	flags=8r40 ifTrue: [^'FREE'].
	addr← image nextword.
	image position← (flags land: 15)*65536+addr*2+512.
	length← image nextword.
	class← image nextword.
	title← self stringOf: (self field: 1 of: class).
	strm← Stream default.
	strm append: 'refct='; print: refct; space.
	strm append: 'flags='+flags base8; space.
	strm append: 'class='+title; space.
	strm append: 'length='; print: length; cr.
	(1 to: (length-2 min: 10)) do:
		[:x | strm append: (self field: x of: oop) base8; space].
	strm cr.  ^strm contents
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
classInit ||  "VmemChecker classInit. "
	ODD← 128.  PTRS← 64.  MT← 32.  "OT flags"
! !
!VmemChecker methodsFor: 'undefined' stamp: ''!
checkend | enddata | " Vmem checkend. "
	image position← 0.
	enddata← image nextword. enddata← enddata*65536+image nextword.
	enddata← enddata*2+512.
	image position← enddata. image skip: ¬2.
	image nextword=0 ifTrue: [user notify: 'zero within data area'].
	(1 to: OTbase-enddata/2) do:
		[:x | image nextword≠0 ifTrue: [user notify: 'non-zero padding']]
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
name | |
	^name
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
scan: strm until: width exceeds: maxw | char w |
	[char ← strm next] whileTrueDo:
		[char < min
		  ifTrue:
			[(char = 32 or: [(char = 13 or: [char = 9])])
			  ifTrue: [^{char , width}].
			user notify: 'char too low']
		  ifFalse:
			[char > max
			  ifTrue: [user notify: 'char too high']
			  ifFalse:
				[(w ← widths ◦ (char + 1 - min)) = 0
				  ifTrue: [^{char , width}].
				(width ← width + w) > maxw
				  ifTrue: [^{true , width}]]]].
	^{false , width}
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
ascent | |
	^ascent
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
space | |
	^150
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
min | |
	^min
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
tab← t | |
	tab ← t
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
max | |
	^max
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
tab | |
	^tab
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
descent | |
	^descent
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
face | |
	^face
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
pointsize | |
	^pointsize
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
named: t1 pointsize: t2 face: t3 | |
	name ← t1.
	pointsize ← t2.
	face ← t3
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
classInit | |
	WidthDict ← Dictionary init.
	tab ← 500
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
lookup | key font i |
	key ← name + pointsize asString + (#('' 'I' 'B' 'BI' ) ◦ (face + 1)).
	(font ← WidthDict lookup: key)
	  ifTrue: [^font].
	self fontfrom: (dp0 oldFile: 'Fonts.Widths') readonly.
	#(9 13 32 ) do:
		[:i | (i ≥ min and: [i ≤ max])
		  ifTrue: [widths ◦ (i - min + 1) ← 0]].
	WidthDict insert: key with: self.
	^self
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
fontfrom: file | i code fam fmin fmax start len found w scale |
	file reset "find code for font family".
	fam ← ''.
	[fam = name] whileFalseDo: 
		[self findfield: 1 on: file.
		code ← file nextword.
		fam ← file next: (len ← file next).
		file skip: 19 - len "now search for proper face"].
	found ← false "Convert from points to micas".
	scale ← (pointsize asFloat * 2540 / 72) asInteger.
	[found] whileFalseDo: 
		[self findfield: 4 on: file.
		found ← (file next = code).
		file next ≠ face
		  ifTrue: [found ← false].
		fmin ← file next.
		fmax ← file next.
		i ← file nextword.
		(i ≠ scale and: [i ≠ 0])
		  ifTrue: [found ← false].
		file skip: 4.
		start ← file nextword.
		file skip: 4].
	scale ← (i ≠ 0
			  ifTrue: [1]
			  ifFalse: [ "don't need to scale"
				pointsize asFloat * 254 / 7200]).
	min ← fmin.
	max ← fmax "get bb and x-tables".
	file wordposition← start + 1.
	descent ← 0 - (scale * file nextword) asInteger.
	file nextword.
	ascent ← (scale * file nextword) asInteger.
	file nextword.
	widths ← Vector new: max - min + 1.
	(1 to: widths length) do:
		[:i | w ← file nextword.
		widths ◦ i ← (w > 0
		  ifTrue: [(scale * w) asInteger]
		  ifFalse: [0])].
	file close
! !
!WidthTable methodsFor: 'undefined' stamp: ''!
findfield: n on: file | IXH t4 |
	[IXH ← file nextword.
	(t4 ← IXH bits: (0 to: 3)) = 0 "type"
	  ifTrue: [user notify: 'field not found']
	  ifFalse: [t4 ≠ n]] whileTrueDo: [file skipwords: (IXH land: 4095) "length" - 1]
! !
!Window methodsFor: 'undefined' stamp: ''!
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
!Window methodsFor: 'undefined' stamp: ''!
close | |
	
! !
!Window methodsFor: 'undefined' stamp: ''!
outside | |
	(titleframe window has: user mp)
	  ifTrue:
		[user anybug
		  ifTrue: [self editTitle]
		  ifFalse: [^false]]
	  ifFalse: [^false]
! !
!Window methodsFor: 'undefined' stamp: ''!
enter | |
	self show
! !
!Window methodsFor: 'undefined' stamp: ''!
leave | |
	
! !
!Window methodsFor: 'undefined' stamp: ''!
keyset | |
	frame flash
! !
!Window methodsFor: 'undefined' stamp: ''!
redbug | |
	frame flash
! !
!Window methodsFor: 'undefined' stamp: ''!
kbd | |
	user kbd.
	frame flash
! !
!Window methodsFor: 'undefined' stamp: ''!
hardcopy | |
	frame flash
! !
!Window methodsFor: 'undefined' stamp: ''!
print | t1 |
	(t1 ← dp0 pressfile: (self title + '.press.') asFileName) screenout: frame scale: PressScale.
	t1 toPrinter
! !
!Window methodsFor: 'undefined' stamp: ''!
aboutToFrame | |
	 "My frame is about to change.  I dont care."
! !
!Window methodsFor: 'undefined' stamp: ''!
yellowbug | |
	frame flash
! !
!Window methodsFor: 'undefined' stamp: ''!
outline | |
	frame outline "Clear and outline me."
! !
!Window methodsFor: 'undefined' stamp: ''!
showtitle | |
	titlepara ≡ nil
	  ifTrue: [titlepara ← Paragraph new text: self title runs: titlerun alignment: 0].
	titleframe put: titlepara at: frame origin + titleloc.
	titleframe outline
! !
!Window methodsFor: 'undefined' stamp: ''!
moveFrom: oldframe | |
	(oldframe inset: ¬2) clear.
	self show
! !
!Window methodsFor: 'undefined' stamp: ''!
clearTitle: color | |
	(titleframe window inset: ¬2 ⌾ ¬2) clear: color
! !
!Window methodsFor: 'undefined' stamp: ''!
frame: f | |
	frame ← self fixframe: f
! !
!Window methodsFor: 'undefined' stamp: ''!
frame | |
	^frame
! !
!Window methodsFor: 'undefined' stamp: ''!
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
!Window methodsFor: 'undefined' stamp: ''!
erase | |
	(frame inset: ¬2 ⌾ ¬2) clear.
	self clearTitle: background
! !
!Window methodsFor: 'undefined' stamp: ''!
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
!Window methodsFor: 'undefined' stamp: ''!
takeCursor | |
	user cursorloc← frame center "Move the cursor to my center."
! !
!Window methodsFor: 'undefined' stamp: ''!
title | |
	^'Untitled'
! !
!Window methodsFor: 'undefined' stamp: ''!
fixframe: f | |
	^Rectangle new origin: f origin extent: (f extent max: 32 ⌾ 32)
! !
!Window methodsFor: 'undefined' stamp: ''!
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
!Window methodsFor: 'undefined' stamp: ''!
show | |
	self outline.
	growing
	  ifFalse: [self showtitle]
! !
!Window methodsFor: 'undefined' stamp: ''!
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
!Window methodsFor: 'undefined' stamp: ''!
reset | |
	exitflag ← true.
	growing ← false
! !
!Window methodsFor: 'undefined' stamp: ''!
firsttime | |
	(frame has: user mp)
	  ifTrue:
		[self reset.
		^self enter].
	^false
! !
!Window methodsFor: 'undefined' stamp: ''!
schedule | |
	user restartup: self
! !
!Window methodsFor: 'undefined' stamp: ''!
lasttime | |
	self leave.
	^exitflag
! !
!Window methodsFor: 'undefined' stamp: ''!
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