File subclass: #AltoFile
	instanceVariableNames: 'leader pageAddresses'
	classVariableNames: ''
	category: 'Alto File System'!
!AltoFile methodsFor: 'Alto' stamp: ''!
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
!AltoFile methodsFor: 'Alto' stamp: ''!
dskprim: diskNumber address: a command: com page: string | |<primitive: 79>
	 "0/1" "starting Alto disk address" "disk command (usually CCR, CCW, CWW)" "string containing label and data"
	error ← ¬1 "if disk routine encounters an error,
	error ← (DCB status, to be interpreted by errorString:).
	⇑false" "if other error occurs, e.g. nil instead of Integer...".
	^false
! !
!AltoFile methodsFor: 'Alto' stamp: ''!
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
!AltoFile methodsFor: 'Alto' stamp: ''!
leader: t1 | |
	leader ← t1
! !
!AltoFile methodsFor: 'Alto' stamp: ''!
leader | |
	^leader
! !
!AltoFile methodsFor: 'Alto' stamp: ''!
pageAddresses: t1 | |
	pageAddresses ← t1
! !
!AltoFile methodsFor: 'Dictionary' stamp: ''!
entryClass | |
	^AltoFilePage
! !
!AltoFile methodsFor: 'Dictionary' stamp: ''!
open | |
	type ← read "don't find last page immediately.  for later close"
! !
!AltoFile methodsFor: 'Dictionary' stamp: ''!
close | |
	type ← self updateLeader: (self read: 0) "to look at at reopen"
! !
!AltoFile methodsFor: 'DictionaryEntry' stamp: ''!
readFrom: s | |
	serialNumber ← s next: 4 "read file description from SysDir".
	s skip: 4 "self version: s nextword. s skip: 2".
	leader ← directory virtualToReal: s nextword.
	name ← s nextString.
	s padNext
! !
!AltoFile methodsFor: 'DictionaryEntry' stamp: ''!
storeOn: s | |
	s append: serialNumber.
	s nextword← 1.
	s nextword← 0.
	s nextword← directory realToVirtual: leader.
	s nextString← name.
	s padNext← 0
! !
!AltoFile methodsFor: 'DictionaryEntry' stamp: ''!
fileSize | |
	^11 + (name length "sn, version, fn, leader, name" lor: 1)
! !
!AltoFile methodsFor: 'DictionaryEntry' stamp: ''!
init | |
	super init.
	pageAddresses ← AltoFileAddressTable new
! !
!AltoFile methodsFor: 'File' stamp: ''!
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
!AltoFile methodsFor: 'File' stamp: ''!
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
!AltoFile methodsFor: 'File' stamp: ''!
doCommand: com page: page error: e | addr |
	error ← nullString.
	addr ← page address.
	"Crock to handle negative 16-bit numbers"
	addr < 0 ifTrue: [addr ← 65536 + addr].
	(self dskprim: directory diskNumber address: addr command: com page: page page)
	  ifTrue: [^page].
	error ← self errorString: error "set by dskprim:...".
	^self error: e
! !
!AltoFile methodsFor: 'File' stamp: ''!
findLastPage | |
	self read: 20000.
	^lastpn
! !
!AltoFile methodsFor: 'File' stamp: ''!
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
!AltoFile methodsFor: 'File' stamp: ''!
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
!AltoFile methodsFor: 'File' stamp: ''!
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
!AltoFile methodsFor: 'File' stamp: ''!
classInit | |
	AltoFilePool declare: #(CRR CCR CCW CWW ) as: #(18496 18512 18520 18536 ) "before filing in:
		Smalltalk declare: ↪AltoFilePool as: (SymbolTable new init: 32)" "disk commands".
	AltoFilePool declare: #(dfmask boffset dirname ) as: #(1024 32 'SysDir.' ) "bit means active directory entry" "byte offset of bit table in DiskDescriptor".
	AltoFilePool declare: #(nextp backp numch pagen vn ) as: #(1 2 4 5 6 )
! !
RunVector subclass: #AltoFileAddressTable
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Alto File System'!
!AltoFileAddressTable methodsFor: 'Reading and writing' stamp: ''!
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
!AltoFileAddressTable methodsFor: 'Reading and writing' stamp: ''!
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
!AltoFileAddressTable methodsFor: 'Reading and writing' stamp: ''!
◦ i | base |
	base ← super ◦ i.
	^dp0 virtualToReal: base + offset
! !
FileDirectory subclass: #AltoFileDirectory
	instanceVariableNames: 'dirFile bitsFile closed diskPages totalPages nSectors'
	classVariableNames: ''
	category: 'Alto File System'!
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
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
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
diskID | f u |
	(f ← self oldFile: 'sys.boot' "return user name and disk name installed in O.S.") readonly.
	f position← 512.
	u ← f nextString.
	f padNext.
	u ← {u , f nextString}.
	f close.
	^u
! !
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
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
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
configure: s | nDisks nHeads nTracks |
	nDisks ← s nextword "read disk configuration from a Stream:
		either leader page of SysDir or beginning of DiskDescriptor".
	nTracks ← s nextword.
	nHeads ← s nextword.
	nSectors ← s nextword.
	diskPages ← nTracks * nHeads * nSectors.
	totalPages ← nDisks * diskPages
! !
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
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
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
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
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
diskNumber | |
	^directory "directory is: Integer⇒ [" "] ⇑directory diskNumber"
! !
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
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
		[user cr; show: 'page already free (deallocate:)']
! !
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
filesMatching: pattern | files v i |
	files ← self match: (pattern last = ('.' ◦ 1)
			  ifTrue: [pattern]
			  ifFalse: [pattern + '.']).
	v ← Vector new: files length.
	(1 to: v length) do: [:i | v ◦ i ← (files ◦ i) name].
	^v
! !
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
flush | |
	bitsFile ≡ nil
	  ifFalse: [bitsFile flush]
! !
!AltoFileDirectory methodsFor: 'Alto' stamp: ''!
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
!AltoFileDirectory methodsFor: 'As yet unclassified' stamp: ''!
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
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
reset | |
	self obsolete
	  ifTrue: [self open]
	  ifFalse: [self flush].
	dirFile readonly.
	dirFile reset
! !
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
release | |
	dirFile ← bitsFile ← nil
! !
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
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
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
close | |
	self obsolete
	  ifFalse:
		[dirFile close.
		bitsFile ≡ nil
		  ifFalse: [ "an interrupted open?"
			bitsFile close].
		super close]
! !
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
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
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
entryClass | |
	^AltoFile
! !
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
Delete: file | t2 |
	(t2 ← self deleteEntry: file) open.
	t2 endFile: false.
	bitsFile flush
! !
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
obsolete | |
	^dirFile ≡ nil
! !
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
entrySize: file | |
	^1 + (file fileSize "entry size in words" / 2)
! !
!AltoFileDirectory methodsFor: 'Dictionary' stamp: ''!
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
!AltoFileDirectory methodsFor: 'FileDirectory' stamp: ''!
allocateSN: file | sn |
	bitsFile position← 8.
	sn ← bitsFile next: 4.
	sn word: 2 ← (sn word: 2) + 1 = 0
	  ifTrue: [sn word: 1 ← (sn word: 1) "overflow" + 1].
	bitsFile skip: ¬4.
	bitsFile append: sn.
	^sn
! !
!AltoFileDirectory methodsFor: 'FileDirectory' stamp: ''!
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
!AltoFileDirectory methodsFor: 'FileDirectory' stamp: ''!
checkName: s | |
	^self checkName: s fixing: false
! !
!AltoFileDirectory methodsFor: 'FileDirectory' stamp: ''!
virtualToReal: vadr | sec d | "inverse of realToVirtual:"
	"vadr < 0 or⦂ vadr ≥ totalPages⇒ [
		self error: 'illegal virtual address']"
	"faster to do /\ for normal Integers"
	"t ← vadr intdiv: diskPages.
	sec ← t◦2 intdiv: nSectors"

	vadr < diskPages
	  ifTrue: [d ← 0. sec ← vadr] "d ← (vadr / diskPages) lshift: 1"
	  ifFalse: [d ← 2. sec ← vadr \ diskPages].
	^"disk" d +
	"cylinder & head" (sec / nSectors lshift: 2) +
	"sector" (sec \ nSectors lshift: 12)

" | t | t← user time asSeconds. (dp0 file: 'st80dec25.changes') settoend; close.
	user time asSeconds-t "
! !
!AltoFileDirectory methodsFor: 'FileDirectory' stamp: ''!
rename: file newName: newName | holesize pos |
	(newName ← self checkName: newName)
	  ifTrue:
		[(self position← newName)
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
FilePage subclass: #AltoFilePage
	instanceVariableNames: 'address'
	classVariableNames: ''
	category: 'Alto File System'!
!AltoFilePage methodsFor: 'Alto' stamp: ''!
freePage | |
	page fill: 11 to: 16 with: 255 "label for a free page: version, sn1, sn2 = ¬1"
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
lastPage | |
	^(page word: nextp) "self header:" = 0
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
length | |
	^page word: numch "self header:"
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
pageNumber: pn | |
	page word: pagen ← pn "self header:"
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
headerLength | |
	^16
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
address | |
	^address
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
length: len | |
	page word: numch ← len "self header:"
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
init | |
	page ≡ nil
	  ifTrue: [super init]
	  ifFalse: [ "nextp, backp, lnused, numch, pn"
		page fill: 1 to: 10 with: 0]
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
serialNumber: sn | |
	page copy: 13 to: 16 with: sn from: 1 to: 4 "page◦(13 to: 16) ← sn" "self header:".
	page word: vn ← 1 "fixed version"
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
address: t1 | |
	address ← t1
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
serialNumber | |
	^page ◦ (13 to: 16)
! !
!AltoFilePage methodsFor: 'FilePage' stamp: ''!
pageNumber | |
	^page word: pagen "self header:"
! !
