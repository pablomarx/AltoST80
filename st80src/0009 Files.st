FilePage subclass: #EtherFilePage
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Files'!
!EtherFilePage methodsFor: 'Ether' stamp: ''!
packet | |
	^page
! !
!EtherFilePage methodsFor: 'Ether' stamp: ''!
pupType← p | |
	^page pupType← p
! !
!EtherFilePage methodsFor: 'Ether' stamp: ''!
pupType | |
	^page pupType
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
header: n ← v | |
	^page word: (12 + n) ← v
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
header: n | |
	^page word: 12 + n "for accessing information after pup header, e.g. file commands and parameters.
	n = 1 to (self headerLength-24)/2"
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
trailerLength | |
	^2 "checksum"
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
length | |
	^page pupLength - (self headerLength - 2)
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
headerLength | |
	^44 "ethernet encap.(4), pup header(20), file label (20=default)"
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
dataString | |
	^page pupString
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
length: len | |
	page pupLength← len + self headerLength - 2
! !
!EtherFilePage methodsFor: 'FilePage' stamp: ''!
init | |
	page ≡ nil
	  ifTrue: [page ← file allocatePage "self page:"].
	self length: 0
! !
Dict subclass: #File
	instanceVariableNames: 'directory type name serialNumber pageReaders pageBuffer lastpn error'
	classVariableNames: ''
	category: 'Files'!
!File methodsFor: 'Dictionary' stamp: ''!
reopen | |
	self sameFile
	  ifFalse: [ "init and directory access"
		directory get: self init].
	self open
! !
!File methodsFor: 'Dictionary' stamp: ''!
Find: page | |
	^page pageNumber ≤ lastpn
! !
!File methodsFor: 'Dictionary' stamp: ''!
release | |
	
! !
!File methodsFor: 'Dictionary' stamp: ''!
open | |
	self findLastPage "compute lastpn"
! !
!File methodsFor: 'Dictionary' stamp: ''!
found: page | |
	self subError "read an existing page"
! !
!File methodsFor: 'Dictionary' stamp: ''!
entryClass | |
	self subError "a subclass of FilePage"
! !
!File methodsFor: 'Dictionary' stamp: ''!
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
!File methodsFor: 'Dictionary' stamp: ''!
close | |
	
! !
!File methodsFor: 'DictionaryEntry' stamp: ''!
printOn: strm | |
	strm append: name
! !
!File methodsFor: 'DictionaryEntry' stamp: ''!
name | |
	^name
! !
!File methodsFor: 'DictionaryEntry' stamp: ''!
name: t1 | |
	name ← t1
! !
!File methodsFor: 'DictionaryEntry' stamp: ''!
dictionary: t1 | |
	directory ← t1
! !
!File methodsFor: 'DictionaryEntry' stamp: ''!
dictionary | |
	^directory
! !
!File methodsFor: 'DictionaryEntry' stamp: ''!
match: entry | |
	^self name match: entry name
! !
!File methodsFor: 'DictionaryEntry' stamp: ''!
init | |
	lastpn ← false.
	error ← nullString.
	serialNumber ← String new: 4
! !
!File methodsFor: 'Documentation' stamp: ''!
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
!File methodsFor: 'File Length' stamp: ''!
lastPage: t1 | |
	 "for those who know what they're doing"
	lastpn ← t1
! !
!File methodsFor: 'File Length' stamp: ''!
findLastPage | |
	^lastpn ← self pageFrom: self length "the default definitions for findLastPage and length are circular.
	at least one of them must be defined by a subclass"
! !
!File methodsFor: 'File Length' stamp: ''!
pageFrom: len | |
	^(len - 1 "compute page number for a character index" / self entryClass new dataLength) asSmall + 1
! !
!File methodsFor: 'File Length' stamp: ''!
lastPage | |
	lastpn
	  ifTrue: [ "length in pages"
		^lastpn].
	^self findLastPage
! !
!File methodsFor: 'File Length' stamp: ''!
endFile: page | |
	self subError "make File end with this FilePage. false means delete all of File"
! !
!File methodsFor: 'File Length' stamp: ''!
lastFullPage | |
	(self read: self lastPage) full
	  ifTrue: [^lastpn].
	^lastpn - 1
! !
!File methodsFor: 'File Length' stamp: ''!
length | page |
	page ← self read: self lastPage "length in characters" "defeat ST76 optimization".
	.
	^lastpn - 1 * page dataLength + page length
! !
!File methodsFor: 'FileDirectory' stamp: ''!
type | |
	^type
! !
!File methodsFor: 'FileDirectory' stamp: ''!
delete | |
	^directory delete: self
! !
!File methodsFor: 'FileDirectory' stamp: ''!
directory: t1 | |
	directory ← t1
! !
!File methodsFor: 'FileDirectory' stamp: ''!
directory | |
	^directory
! !
!File methodsFor: 'FileDirectory' stamp: ''!
type: t1 | |
	 "used by different Files in different ways, e.g. read/write mode"
	type ← t1
! !
!File methodsFor: 'FileDirectory' stamp: ''!
rename: newName | |
	^directory rename: self newName: newName
! !
!File methodsFor: 'FilePage' stamp: ''!
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
!File methodsFor: 'FilePage' stamp: ''!
Read: page | |
	self subError "return page or false"
! !
!File methodsFor: 'FilePage' stamp: ''!
newPage: pn | |
	^self makeEntry: pn
! !
!File methodsFor: 'FilePage' stamp: ''!
newPage | |
	^self makeEntry: 0
! !
!File methodsFor: 'FilePage' stamp: ''!
Write: page | |
	self subError "update lastpn, write page and return result (maybe next page)"
! !
!File methodsFor: 'FilePage' stamp: ''!
doCommand: com page: page error: s | |
	self subError "execute a File command on page. if an error occurs, include
	error ← 'some error message'.
	⇑self error: s
	if s is false, returns false.
	otherwise s is passed to an error routine"
! !
!File methodsFor: 'FilePage' stamp: ''!
read: pn | |
	^self Read: (self makeEntry: pn)
! !
!File methodsFor: 'FilePage' stamp: ''!
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
!File methodsFor: 'FilePage' stamp: ''!
error | |
	^error
! !
!File methodsFor: 'FilePage' stamp: ''!
get: pn | |
	^self Get: (self makeEntry: pn)
! !
!File methodsFor: 'FileStream' stamp: ''!
asStream | |
	^FileStream new on: (self open.
	self get: 1)
! !
!File methodsFor: 'Initialize' stamp: ''!
sameFile | |
	 "is File's current internal representation the same as what is stored externally? if so, usually can avoid some initialization, directory lookup"
	^false
! !
!File methodsFor: 'Initialize' stamp: ''!
classInit | |
	FilePool declare: #(read write shorten ) as: #(1 2 4 ) "subclasses of File may want to share variables in pools.
	execute before filin:
		Smalltalk declare: ↪XFilePool as: (SymbolTable new init: 16).
	in classInit: XFilePool declare: ↪() as: ↪()"
! !
!File methodsFor: 'Name' stamp: ''!
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
!File methodsFor: 'Name' stamp: ''!
serialNumber | |
	^serialNumber
! !
Dict subclass: #FileDirectory
	instanceVariableNames: 'directory fileReaders'
	classVariableNames: ''
	category: 'Files'!
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
printOn: strm | |
	strm append: (self obsolete
	  ifTrue: ['a closed ']
	  ifFalse: ['an open ']).
	strm append: self class title.
	strm append: ' on '.
	self server printOn: strm
! !
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
close | |
	self obsolete
	  ifFalse:
		[externalViews delete: self.
		self release]
! !
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
checkName: s | |
	(s empty "default behavior is to get rid of ending period.
	subclasses can do any kind of checking they want and
	return false if name is no good" or: [s last ≠ ('.' ◦ 1)])
	  ifTrue: [^s].
	^s copy: 1 to: s length - 1
! !
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
error: e entry: file | |
	^file error: e
! !
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
Find: file | name |
	(name ← self checkName: file name)
	  ifTrue:
		[file name: name.
		^self Position← file].
	file error: 'illegal name'
! !
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
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
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
open | |
	externalViews insert: self
! !
!FileDirectory methodsFor: 'Dictionary' stamp: ''!
entryClass | |
	self subError "a subclass of File"
! !
!FileDirectory methodsFor: 'DictionaryEntry' stamp: ''!
dictionary: t1 | |
	directory ← t1
! !
!FileDirectory methodsFor: 'DictionaryEntry' stamp: ''!
dictionary | |
	^directory
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
asFtpDirectory | ftp |
	(ftp ← FtpDirectory new "to allow convenient (kludgey) access to file servers (e.g. phylum, dpj) via Ftp") server: self server.
	ftp open.
	ftp userName empty
	  ifTrue: [ftp login: self userName password: self userPassword].
	^ftp
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
server | |
	^directory
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
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
!FileDirectory methodsFor: 'FTP' stamp: ''!
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
!FileDirectory methodsFor: 'FTP' stamp: ''!
userPassword | |
	^currentProfile ≡ nil
	  ifTrue: ['']
	  ifFalse: [currentProfile userPassword: self server]
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
userName | |
	^currentProfile ≡ nil
	  ifTrue: ['']
	  ifFalse: [currentProfile userName: self server]
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
login: name password: pw | |
	self subError
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
login: name | |
	^self login: name password: '' "or prompt?"
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
store: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self store: t as: t]]
	  ifFalse: [^self store: s as: s]
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
server: t1 | |
	directory ← t1
! !
!FileDirectory methodsFor: 'FTP' stamp: ''!
retrieve: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self retrieve: t as: t]]
	  ifFalse: [^self retrieve: s as: s]
! !
!FileDirectory methodsFor: 'File' stamp: ''!
versionNumbers | |
	^false "generally, version numbers are not supported"
! !
!FileDirectory methodsFor: 'File' stamp: ''!
directory | |
	^directory
! !
!FileDirectory methodsFor: 'File' stamp: ''!
newPage | |
	^(self makeEntry: nullString) "return a dummy FilePage from a dummy File" newPage
! !
!FileDirectory methodsFor: 'File' stamp: ''!
allocateSN: file | |
	self subError "allocate a new serial number for a File"
! !
!FileDirectory methodsFor: 'FileStream' stamp: ''!
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
!FileDirectory methodsFor: 'FileStream' stamp: ''!
filin: s | |
	self filin: s format: 1
! !
!FileDirectory methodsFor: 'FileStream' stamp: ''!
pressfile: name | |
	^PressFile new of: (self file: name)
! !
!FileDirectory methodsFor: 'FileStream' stamp: ''!
oldFile: name | |
	^(self find: name) asStream
! !
!FileDirectory methodsFor: 'FileStream' stamp: ''!
newFile: name | |
	^(self insert: name) asStream
! !
!FileDirectory methodsFor: 'FileStream' stamp: ''!
file: name | |
	^(self get: name) asStream
! !
!FileDirectory methodsFor: 'FileStream' stamp: ''!
pressfilin: s | |
	self filin: s format: 2
! !
!FileDirectory methodsFor: 'Initialize' stamp: ''!
directory: t1 | |
	directory ← t1
! !
!FileDirectory methodsFor: 'Juniper' stamp: ''!
exceptionHandler: eh | |
	 "default is to do nothing"
! !
!FileDirectory methodsFor: 'Juniper' stamp: ''!
closeTransaction | |
	 "default is to do nothing"
! !
Dict subclass: #FilePage
	instanceVariableNames: 'file page'
	classVariableNames: ''
	category: 'Files'!
!FilePage methodsFor: 'Dictionary' stamp: ''!
reopen | |
	file reopen.
	file makeEntry: self "self may have been released"
! !
!FilePage methodsFor: 'Dictionary' stamp: ''!
asStream | |
	^self asStream: Stream new
! !
!FilePage methodsFor: 'Dictionary' stamp: ''!
◦ i | |
	^page ◦ (self checkIndex: i)
! !
!FilePage methodsFor: 'Dictionary' stamp: ''!
◦ i ← v | |
	^page ◦ (self checkIndex: i) ← v
! !
!FilePage methodsFor: 'DictionaryEntry' stamp: ''!
init | |
	page ≡ nil
	  ifTrue: [page ← String new: self pageLength "self page:"].
	self length: 0 "not sure who depends on this besides FileStream read:"
! !
!FilePage methodsFor: 'DictionaryEntry' stamp: ''!
name: sp | |
	self init.
	self serialNumber: sp ◦ 1.
	self pageNumber: sp ◦ 2
! !
!FilePage methodsFor: 'DictionaryEntry' stamp: ''!
dictionary: t1 | |
	file ← t1
! !
!FilePage methodsFor: 'DictionaryEntry' stamp: ''!
dictionary | |
	^file
! !
!FilePage methodsFor: 'File' stamp: ''!
read: pn | |
	self pageNumber: pn "recycle self".
	self length: 0.
	^file Read: self
! !
!FilePage methodsFor: 'File' stamp: ''!
endFile | |
	^file endFile: self
! !
!FilePage methodsFor: 'File' stamp: ''!
get: pn | |
	self pageNumber: pn "recycle self".
	self length: 0.
	^file Get: self
! !
!FilePage methodsFor: 'File' stamp: ''!
write | |
	^file Write: self "some files, e.g. AltoFile, will return a last empty page instead of a full one"
! !
!FilePage methodsFor: 'File' stamp: ''!
file | |
	^file
! !
!FilePage methodsFor: 'File' stamp: ''!
doCommand: com error: s | |
	^file doCommand: com page: self error: s
! !
!FilePage methodsFor: 'Initialize' stamp: ''!
file: t1 | |
	file ← t1
! !
!FilePage methodsFor: 'Initialize' stamp: ''!
page: t1 | |
	page ← t1
! !
!FilePage methodsFor: 'Page' stamp: ''!
pageNumber: pn | |
	self subError
! !
!FilePage methodsFor: 'Page' stamp: ''!
header: n | |
	^page word: n "return n-th header word"
! !
!FilePage methodsFor: 'Page' stamp: ''!
pageNumber | |
	self subError
! !
!FilePage methodsFor: 'Page' stamp: ''!
serialNumber: sn | |
	self subError
! !
!FilePage methodsFor: 'Page' stamp: ''!
serialNumber | |
	^file serialNumber
! !
!FilePage methodsFor: 'Page' stamp: ''!
dataEnd: pos | |
	self length: pos - self headerLength
! !
!FilePage methodsFor: 'Page' stamp: ''!
address: a | |
	self subError
! !
!FilePage methodsFor: 'Page' stamp: ''!
word: i ← v | |
	^page word: (self headerLength "no bounds checking" / 2 + i) ← v
! !
!FilePage methodsFor: 'Page' stamp: ''!
lastPage | |
	^self pageNumber "is this last page in file?" ≥ file lastPage
! !
!FilePage methodsFor: 'Page' stamp: ''!
length: len | |
	self subError
! !
!FilePage methodsFor: 'Page' stamp: ''!
address | |
	self subError "page address, e.g. on a disk"
! !
!FilePage methodsFor: 'Page' stamp: ''!
dataEnd | |
	^self headerLength "logical end of data in page" + self length
! !
!FilePage methodsFor: 'Page' stamp: ''!
checkIndex: i | |
	(i > 0 and: [i ≤ self length])
	  ifTrue: [^i + self headerLength].
	self error: 'illegal index'
! !
!FilePage methodsFor: 'Page' stamp: ''!
full | |
	^self length = self dataLength
! !
!FilePage methodsFor: 'Page' stamp: ''!
word: i | |
	^page word: self headerLength "no bounds checking" / 2 + i
! !
!FilePage methodsFor: 'Page' stamp: ''!
length | |
	self subError "logical length of data in page"
! !
!FilePage methodsFor: 'Page' stamp: ''!
header: n ← v | |
	^page word: n ← v "set and return n-th header word"
! !
!FilePage methodsFor: 'Page' stamp: ''!
dataString | |
	^page
! !
!FilePage methodsFor: 'Page' stamp: ''!
trailerLength | |
	^0 "length of stuff after data ends in page"
! !
!FilePage methodsFor: 'Page' stamp: ''!
pageLength | |
	^self headerLength "physical size of page" + self dataLength + self trailerLength
! !
!FilePage methodsFor: 'Page' stamp: ''!
page | |
	^page
! !
!FilePage methodsFor: 'Page' stamp: ''!
dataMaxEnd | |
	^self headerLength "physical end of data in page" + self dataLength
! !
!FilePage methodsFor: 'Page' stamp: ''!
dataBeginning | |
	^self headerLength
! !
!FilePage methodsFor: 'Page' stamp: ''!
headerLength | |
	^0 "length of stuff before data begins in page"
! !
!FilePage methodsFor: 'Page' stamp: ''!
asStream: s | offset |
	offset ← self headerLength.
	^s of: self dataString from: offset + 1 to: offset + self length "self dataEnd"
! !
!FilePage methodsFor: 'Page' stamp: ''!
dataLength | |
	^512 "physical length of data in page. default"
! !
Stream subclass: #FileStream
	instanceVariableNames: 'page dirty rwmode'
	classVariableNames: ''
	category: 'Files'!
!FileStream methodsFor: 'Access Modes' stamp: ''!
readonly | |
	self setMode: read
! !
!FileStream methodsFor: 'Access Modes' stamp: ''!
setMode: m | |
	rwmode = m
	  ifFalse:
		[ "don't flush if first time or not write mode or continuing write mode"
		(rwmode ≡ nil or: [((rwmode nomask: write) or: [(m anymask: write)])])
		  ifFalse: [self flush].
		rwmode ← m]
! !
!FileStream methodsFor: 'Access Modes' stamp: ''!
writing | |
	rwmode ≡ nil
	  ifTrue: [^self readwriteshorten "default mode. true"].
	^(rwmode land: write) = write
! !
!FileStream methodsFor: 'Access Modes' stamp: ''!
readwrite | |
	self setMode: read + write "allow read and write but don't automatically shorten"
! !
!FileStream methodsFor: 'Access Modes' stamp: ''!
readwriteshorten | |
	self setMode: read + write "allow read and write and shorten File upon closing" + shorten
! !
!FileStream methodsFor: 'Access Modes' stamp: ''!
writeshorten | |
	self setMode: write + shorten "allow write and shorten File upon closing. in general, this would be faster for overwriting Files since pages might not have to be read first. at present, treated same as readwriteshorten"
! !
!FileStream methodsFor: 'CodePane Editor' stamp: ''!
edit | |
	NoteTaker
	  ifTrue: [user schedule: (CodeWindow new file: self)]
	  ifFalse: [user restartup: (CodeWindow new file: self)]
! !
!FileStream methodsFor: 'Dictionary' stamp: ''!
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
!FileStream methodsFor: 'Dictionary' stamp: ''!
release | |
	self obsolete
	  ifFalse:
		[dirty ← limit ← 0.
		self file release]
! !
!FileStream methodsFor: 'Dictionary' stamp: ''!
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
!FileStream methodsFor: 'Dictionary' stamp: ''!
obsolete | |
	^dirty
! !
!FileStream methodsFor: 'Dictionary' stamp: ''!
file | |
	^page file
! !
!FileStream methodsFor: 'Fast Access' stamp: ''!
readPages: n | charsLeft len s |
	len ← n * page dataLength "read n pages of characters".
	s ← String new: len "charsRead ← self into: s endError: false.".
	charsLeft ← self readString: s from: 1 to: len.
	charsLeft = 0
	  ifTrue: [^s].
	 "read len chars" "return characters read only before end of file"
	^s copy: 1 to: len - charsLeft
! !
!FileStream methodsFor: 'Fast Access' stamp: ''!
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
!FileStream methodsFor: 'Fast Access' stamp: ''!
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
!FileStream methodsFor: 'Fast Access' stamp: ''!
streamPosition← t1 | |
	position ← t1.
	^position
! !
!FileStream methodsFor: 'Fast Access' stamp: ''!
streamPosition | |
	^position
! !
!FileStream methodsFor: 'Fast Access' stamp: ''!
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
!FileStream methodsFor: 'File' stamp: ''!
nextPage | |
	^self read: page pageNumber + 1
! !
!FileStream methodsFor: 'File' stamp: ''!
directory | |
	^self file directory
! !
!FileStream methodsFor: 'File' stamp: ''!
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
!FileStream methodsFor: 'File' stamp: ''!
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
!FileStream methodsFor: 'File' stamp: ''!
name | |
	^self file name
! !
!FileStream methodsFor: 'File' stamp: ''!
fixEnd | |
	(self writing and: [position > page dataEnd])
	  ifTrue: [page dataEnd: (limit ← position) "fix the end of page"]
! !
!FileStream methodsFor: 'File' stamp: ''!
page | |
	^page
! !
!FileStream methodsFor: 'File' stamp: ''!
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
!FileStream methodsFor: 'File' stamp: ''!
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
!FileStream methodsFor: 'File' stamp: ''!
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
!FileStream methodsFor: 'File' stamp: ''!
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
!FileStream methodsFor: 'File' stamp: ''!
shorten | |
	self on:  "normally called by close and not directly by user"
	(page dataEnd: (limit ← position).
	page endFile).
	position ← limit
! !
!FileStream methodsFor: 'File' stamp: ''!
flush | |
	self obsolete
	  ifTrue: [^page].
	self fixEnd.
	^page write
! !
!FileStream methodsFor: 'File' stamp: ''!
settopage: p char: c | |
	(self read: p asSmall "mainly for compatibility, since page sizes may vary.
	in general, use position←, wordposition←")
	  ifTrue: [self skip: c asSmall]
	  ifFalse: [self error: 'no page']
! !
!FileStream methodsFor: 'Filin/Filout' stamp: ''!
asParagraphPrinter | |
	^BravoPrinter init "default format for filout etc." of: self
! !
!FileStream methodsFor: 'Filin/Filout' stamp: ''!
backup | |
	self directory "assume ivy open" ≡ dp0
	  ifTrue: [ivy replace: self name]
! !
!FileStream methodsFor: 'Filin/Filout' stamp: ''!
filoutclass: class | t2 |
	(t2 ← self asParagraphPrinter) stamp.
	t2 printclass: class.
	t2 close
! !
!FileStream methodsFor: 'Filin/Filout' stamp: ''!
nextParagraph | text |
	self end "Bravo format paragraph (or self contents if no trailer)"
	  ifTrue: [^false].
	text ← self upto: 26 "ctrl-z".
	^text asParagraph applyBravo: self at: 1 to: text length
! !
!FileStream methodsFor: 'Filin/Filout' stamp: ''!
filout | |
	self filout: Changes contents sort
! !
!FileStream methodsFor: 'Filin/Filout' stamp: ''!
filout: source | t2 |
	(t2 ← self asParagraphPrinter) stamp.
	t2 printchanges: source.
	t2 close
! !
!FileStream methodsFor: 'Filin/Filout' stamp: ''!
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
!FileStream methodsFor: 'Initialize' stamp: ''!
on: t1 | |
	 "some page from a File, usually page 1, or another FileStream"
	page ← t1.
	(page is: FileStream)
	  ifTrue: [page ← page page].
	page asStream: self.
	externalViews insert: self "obsolete flag".
	dirty ← false
! !
!FileStream methodsFor: 'Print' stamp: ''!
asPressPrinter | |
	^PressPrinter init "default format for printt etc." of: self
! !
!FileStream methodsFor: 'Print' stamp: ''!
printoutclass: class | t2 |
	(t2 ← self asPressPrinter) stamp.
	t2 printclass: class.
	t2 close.
	t2 toPrinter
! !
!FileStream methodsFor: 'Print' stamp: ''!
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
!FileStream methodsFor: 'Print' stamp: ''!
printout: source | t2 |
	(t2 ← self asPressPrinter) stamp.
	t2 printchanges: source.
	t2 close.
	t2 toPrinter
! !
!FileStream methodsFor: 'Stream' stamp: ''!
length | |
	self reopen; fixEnd.
	page lastPage
	  ifTrue: [^page pageNumber - 1 * page dataLength + page length].
	^self file length
! !
!FileStream methodsFor: 'Stream' stamp: ''!
printOn: strm | |
	super printOn: strm.
	strm append: ' on '.
	self file printOn: strm
! !
!FileStream methodsFor: 'Stream' stamp: ''!
wordposition | |
	^self positionSize: 2
! !
!FileStream methodsFor: 'Stream' stamp: ''!
reset | |
	(self read: 1) "self position ← 0"
	  ifFalse: [self error: 'reset']
! !
!FileStream methodsFor: 'Stream' stamp: ''!
position | |
	^self positionSize: 1
! !
!FileStream methodsFor: 'Stream' stamp: ''!
pastend | |
	(self reopen or: [(page lastPage ≡ false and: [self nextPage])])
	  ifTrue: [^self next].
	^false
! !
!FileStream methodsFor: 'Stream' stamp: ''!
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
!FileStream methodsFor: 'Stream' stamp: ''!
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
!FileStream methodsFor: 'Stream' stamp: ''!
settoend | |
	self reopen "self position ← self length" "make sure file is open so lastPage is correct" "when writing on the last page, lastPage may be too small".
	(self read: (self file lastPage max: page pageNumber))
	  ifTrue: [position ← limit]
	  ifFalse: [self error: 'settoend???']
! !
!FileStream methodsFor: 'Stream' stamp: ''!
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
!FileStream methodsFor: 'Stream' stamp: ''!
word: i ← v | |
	self wordposition← i - 1.
	^self nextword← v
! !
!FileStream methodsFor: 'Stream' stamp: ''!
contents | s |
	self readonly "read all of a File".
	self reset.
	s ← self next: self length.
	self close.
	^s
! !
!FileStream methodsFor: 'Stream' stamp: ''!
word: i | |
	self wordposition← i - 1.
	^self nextword
! !
!FileStream methodsFor: 'Stream' stamp: ''!
next: n from: strm | |
	(n > 80 and: [(strm is: FileStream)])
	  ifTrue: [self writeFile: strm for: n]
	  ifFalse: [^super next: n from: strm]
! !
!FileStream methodsFor: 'Stream' stamp: ''!
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
!FileStream methodsFor: 'Stream' stamp: ''!
end | |
	self reopen.
	position < limit
	  ifTrue: [^false].
	(self read: page pageNumber + 1)
	  ifTrue: [^position = limit "page empty"].
	^true
! !
!FileStream methodsFor: 'Stream' stamp: ''!
position← p | |
	^self position: p size: 1
! !
!FileStream methodsFor: 'Stream' stamp: ''!
wordposition← w | |
	^self position: w size: 2
! !
!FileStream methodsFor: 'Stream' stamp: ''!
◦ i | |
	self position← i - 1.
	^self next
! !
!FileStream methodsFor: 'Stream' stamp: ''!
◦ i ← v | |
	self position← i - 1.
	^self next← v
! !
FileDirectory subclass: #FtpDirectory
	instanceVariableNames: 'command'
	classVariableNames: ''
	category: 'Files'!
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
open | |
	command ← Stream new of: (String new: 100).
	command append: 'Ftp '.
	command append: directory.
	self login: self userName password: self userPassword
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
login: name password: pw | |
	name empty
	  ifFalse:
		[command append: ' Login/C '.
		command append: name.
		command space.
		command append: pw]
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
store: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self store: t]]
	  ifFalse:
		[command append: ' Store/C '.
		command append: (self checkName: s)]
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
close | |
	self closeThen: 'Resume. Small.Boot'
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
rename: oldName newName: newName | |
	command append: ' Rename/C '.
	command append: (self checkName: oldName).
	command space.
	command append: (self checkName: newName)
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
closeThen: s | |
	command append: '; '.
	command append: s.
	user quitThen: command contents.
	self open
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
directoryName: name | |
	command append: ' Directory/C ' "this message should be directory:, but until rewriting...".
	command append: name
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
retrieve: s | t |
	(s is: Vector)
	  ifTrue: [s do: [:t | self retrieve: t]]
	  ifFalse:
		[command append: ' Retrieve/C '.
		command append: (self checkName: s)]
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
connect: name password: pw | |
	command append: ' Connect/C '.
	command append: name.
	command space.
	command append: pw
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
store: local as: remote | |
	command append: ' Store/S '.
	command append: (self checkName: local).
	command space.
	command append: (self checkName: remote)
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
retrieve: remote as: local | |
	command append: ' Retrieve/S '.
	command append: (self checkName: remote).
	command space.
	command append: (self checkName: local)
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
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
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
delete: name | |
	command append: ' Delete/C '.
	command append: (self checkName: name)
! !
!FtpDirectory methodsFor: 'FileDirectory' stamp: ''!
commands | |
	^command contents
! !
