Object subclass: #Association
	instanceVariableNames: 'key value'
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!Association methodsFor: 'As yet unclassified' stamp: ''!
key | |
	^key
! !
!Association methodsFor: 'As yet unclassified' stamp: ''!
printOn: strm | |
	strm print: key.
	strm append: '->'.
	strm print: value
! !
!Association methodsFor: 'As yet unclassified' stamp: ''!
value← val | |
	^value ← val
! !
!Association methodsFor: 'As yet unclassified' stamp: ''!
value | |
	^value
! !
!Association methodsFor: 'As yet unclassified' stamp: ''!
key: t1 value: t2 | |
	key ← t1.
	value ← t2
! !
Object subclass: #ClassOrganizer
	instanceVariableNames: 'globalComment commentVector groupVector'
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
globalComment | |
	^globalComment asParagraph text
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
invert: selector | i |
	(1 to: groupVector length) do:
		[:i | (groupVector ◦ i has: selector)
		  ifTrue: [^commentVector ◦ i]].
	^false
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
deleteCategory: cat | index |
	index← commentVector find: cat.
	groupVector ← groupVector without: index.
	commentVector ← commentVector without: index
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
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
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
category: str | i |
	i ← commentVector find: str.
	i = 0
	  ifTrue: [user notify: 'No such category: ' + str]
	  ifFalse: [^groupVector ◦ i]
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
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
		  ifTrue: [self deleteCategory: default]]
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
globalComment← t1 | |
	 "String or RemoteParagraph"
	globalComment ← t1
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
categories | |
	^commentVector
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
delete: selector | i |
	 "delete this from all categories"
	(1 to: groupVector length) do:
		[:i | (groupVector ◦ i has: selector)
		  ifTrue:
			[groupVector ◦ i ← groupVector ◦ i delete: selector.
			((groupVector ◦ i) length = 0 and: [commentVector ◦ i = default])
			  ifTrue: [self deleteCategory: i]]]
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
asStream | v t |
	v ← Stream new of: (Vector new: 200).
	groupVector do: [:t | v append: t].
	^v contents asStream
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
has: sel | t |
	groupVector do:
		[:t | (t has: sel)
		  ifTrue: [^true]].
	^false
! !
!ClassOrganizer methodsFor: 'Access to parts' stamp: ''!
globalCommentItself | |
	^globalComment "used only by Class archiveOn:changesOnly:"
! !
!ClassOrganizer methodsFor: 'Conversion to text' stamp: ''!
asParagraph | s i |
	s ← Stream default.
	s print: self globalComment.
	(1 to: commentVector length) do:
		[:i | s cr.
		s print: ((commentVector ◦ i) inVector concat: groupVector ◦ i)].
	^s contents asParagraph
! !
!ClassOrganizer methodsFor: 'Conversion to text' stamp: ''!
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
!ClassOrganizer methodsFor: 'Initialization' stamp: ''!
init: sortedVec | |
	self globalComment← 'This class has not yet been commented'.
	commentVector ← 'As yet unclassified' inVector.
	groupVector ← sortedVec inVector
! !
!ClassOrganizer methodsFor: 'Initialization' stamp: ''!
classInit | |
	default ← 'As yet unclassified'
! !
Object subclass: #Dict
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!Dict methodsFor: 'Entry Access' stamp: ''!
store: entry | |
	^self write: entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
match: entries | set |
	set ← Set new vector: 50.
	self match: entries to: set.
	^set contents
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
replace: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue:
		[self Replace: entry.
		^entry].
	^self error: 'not replaced (not found)' entry: entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
exists: entry | |
	^self Find: (self makeEntry: entry) "doesn't initialize too much"
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
match: entries to: strm | entry nentries |
	 "return a Set of entries which match those in entries
	(can include exact values and patterns and ranges)"
	((entries is: Vector) or: [(entries Is: Set)])
	  ifFalse: [entries ← entries inVector].
	nentries ← Set new vector: entries length.
	entries do: [:entry | nentries next← self makeEntry: entry].
	^self Match: nentries to: strm
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
list: entries | |
	self match: entries to: user
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
write: entry | |
	 "replace or insert"
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue: [self Replace: entry]
	  ifFalse: [self Insert: entry].
	^entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
read: entry | |
	^self find: entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
find: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue: [^self found: entry].
	^self error: 'not found' entry: entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
get: entry | |
	(self Find: (entry ← self makeEntry: entry "find or insert"))
	  ifTrue: [^self found: entry].
	self Insert: entry.
	^entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
contents | |
	^self match: '*'
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
list | |
	self list: '*'
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
rename: entry newName: name | nentry |
	(self Find: (nentry ← self makeEntry: name "not tested"))
	  ifTrue: [^self error: 'already exists' error: nentry].
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue:
		[self Rename: entry from: nentry.
		^entry].
	^self error: 'not found' entry: entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
insert: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue: [^self error: 'not inserted (already found)' entry: entry].
	self Insert: entry.
	^entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
delete: entry | |
	(self Find: (entry ← self makeEntry: entry))
	  ifTrue:
		[self Delete: entry.
		^entry].
	^self error: 'not deleted (not found)' entry: entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
create: entry | |
	^self insert: entry
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
found: entry | |
	^self nextEntry: entry "found, fill it in from dictionary"
! !
!Dict methodsFor: 'Entry Access' stamp: ''!
retrieve: entry | |
	^self find: entry "match:?"
! !
!Dict methodsFor: 'Entry Creation' stamp: ''!
nextEntry: entry | |
	^entry "return next name and value in entry, or false.
	if insert or delete occurs after previous next, may be problem"
! !
!Dict methodsFor: 'Entry Creation' stamp: ''!
entryClass | |
	self subError "a subclass of DictionaryEntry"
! !
!Dict methodsFor: 'Entry Creation' stamp: ''!
makeEntry: entry | cl |
	 "entry or name"
	cl ← self entryClass.
	(cl ≡ false or: [(entry Is: cl)])
	  ifTrue: [^entry].
	 "entry should not be converted or is the correct type" "convert entry from a name to an entry with that name"
	^self newEntry name: entry
! !
!Dict methodsFor: 'Entry Creation' stamp: ''!
newEntry | t1 |
	^(t1 ← self entryClass new) dictionary: self.
	t1 init
! !
!Dict methodsFor: 'Entry Operations' stamp: ''!
Replace: entry | |
	self subError "entry found (next), replace it's value"
! !
!Dict methodsFor: 'Entry Operations' stamp: ''!
Find: entry | |
	^self Position← entry "is entry in dictionary?"
! !
!Dict methodsFor: 'Entry Operations' stamp: ''!
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
!Dict methodsFor: 'Entry Operations' stamp: ''!
Rename: entry from: nentry | |
	self Delete: entry.
	self Insert: (entry name: nentry name)
! !
!Dict methodsFor: 'Entry Operations' stamp: ''!
Insert: entry | |
	self subError "entry not found, insert it (next)"
! !
!Dict methodsFor: 'Entry Operations' stamp: ''!
Delete: entry | |
	self subError "entry found (next), delete it"
! !
!Dict methodsFor: 'Entry Operations' stamp: ''!
entrySize: entry | |
	self subError "storage size of entry, constant or variable"
! !
!Dict methodsFor: 'Entry Operations' stamp: ''!
error: e entry: entry | |
	^false "entry error: e"
! !
!Dict methodsFor: 'File-Based dictionary' stamp: ''!
file | |
	^false "return my file"
! !
!Dict methodsFor: 'File-Based dictionary' stamp: ''!
reopen | |
	self open "reinitialize, especially if a File is involved"
! !
!Dict methodsFor: 'File-Based dictionary' stamp: ''!
release | |
	 "obsolete and deallocate storage, especially if connected to an external view,
	e.g. a File"
	self file
	  ifTrue: [self file release]
! !
!Dict methodsFor: 'File-Based dictionary' stamp: ''!
obsolete | |
	self file "is my information obsolete (should I regenerate it)?"
	  ifTrue: [^self file obsolete].
	^false
! !
!Dict methodsFor: 'File-Based dictionary' stamp: ''!
open | |
	
! !
!Dict methodsFor: 'File-Based dictionary' stamp: ''!
close | |
	self obsolete
	  ifFalse:
		[ "possible cleanup before a release"
		self file
		  ifTrue: [self file close].
		self release]
! !
!Dict methodsFor: 'Initialize' stamp: ''!
init: initialSize | |
	 "default is to ignore"
! !
!Dict methodsFor: 'Name-Value Access' stamp: ''!
replace: name with: value | |
	^self replace: (self newEntry name: name value: value)
! !
!Dict methodsFor: 'Name-Value Access' stamp: ''!
◦ name ← value | |
	^self write: (self newEntry "replace or insert" name: name value: value)
! !
!Dict methodsFor: 'Name-Value Access' stamp: ''!
◦ name | entry |
	(entry ← self find: name "find")
	  ifTrue: [^entry value].
	^false
! !
!Dict methodsFor: 'Name-Value Access' stamp: ''!
lookup: name | |
	^self ◦ name
! !
!Dict methodsFor: 'Name-Value Access' stamp: ''!
insert: name with: value | |
	^self insert: (self newEntry name: name value: value)
! !
!Dict methodsFor: 'Stream Access' stamp: ''!
append: dict | entry |
	dict do: [:entry | self write: entry]
! !
!Dict methodsFor: 'Stream Access' stamp: ''!
position← name | |
	^self Position← self makeEntry: name
! !
!Dict methodsFor: 'Stream Access' stamp: ''!
next | |
	^self nextEntry: self newEntry "return next entry or false"
! !
!Dict methodsFor: 'Stream Access' stamp: ''!
Position← entry | |
	^self position← entry name "position to name, or position to insert place and return false if not found.
	subclass had better define position← or Position← (preferably)
	otherwise circularity results!!!"
! !
!Dict methodsFor: 'Stream Access' stamp: ''!
position | |
	self subError "current position (name)"
! !
!Dict methodsFor: 'Stream Access' stamp: ''!
asStream | |
	 "leave position where it is"
! !
!Dict methodsFor: 'Stream Access' stamp: ''!
reset | |
	self subError "position to beginning"
! !
HashSet subclass: #Dictionary
	instanceVariableNames: 'values'
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!Dictionary methodsFor: 'Initialization' stamp: ''!
init: size | |
	values ← Vector new: size.
	super init: size
! !
!Dictionary methodsFor: 'Initialization' stamp: ''!
copyfrom: dict | |
	self objects← dict objects copy.
	values ← dict values copy
! !
!Dictionary methodsFor: 'Inserting and Deleting' stamp: ''!
insert: name with: value | |
	self insert: name.
	values ◦ (self findorerror: name) ← value
! !
!Dictionary methodsFor: 'Inserting and Deleting' stamp: ''!
tally: name | x |
	(x ← self find: name)
	  ifTrue: [^values ◦ x ← values ◦ x + 1].
	self insert: name with: 1.
	^1
! !
!Dictionary methodsFor: 'Inserting and Deleting' stamp: ''!
with: names values: vals | i |
	(1 to: names length) do: [:i | self insert: names ◦ i with: vals ◦ i]
! !
!Dictionary methodsFor: 'Inserting and Deleting' stamp: ''!
delete: name | |
	(name is: Vector)
	  ifTrue: [super delete: name]
	  ifFalse:
		[values ◦ (self findorerror: name) ← nil.
		super delete: name]
! !
!Dictionary methodsFor: 'Inserting and Deleting' stamp: ''!
clean | name |
	 "release unreferenced entries"
	self do:
		[:name |  "slick, huh"
		(self ◦ name) refct = 1
		  ifTrue: [self delete: name]]
! !
!Dictionary methodsFor: 'Inserting and Deleting' stamp: ''!
insertall: names with: vals | i |
	 "insert many entries"
	(1 to: names length) do: [:i | self insert: names ◦ i with: vals ◦ i]
! !
!Dictionary methodsFor: 'Inserting and Deleting' stamp: ''!
insertall: names | |
	 "default value is nil"
	self insertall: names with: (Vector new: names length)
! !
!Dictionary methodsFor: 'Inversion' stamp: ''!
invert: obj | i |
	(1 to: values length) do:
		[:i | values ◦ i = obj
		  ifTrue: [^objects ◦ i]].
	^false
! !
!Dictionary methodsFor: 'Inversion' stamp: ''!
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
!Dictionary methodsFor: 'Inversion' stamp: ''!
invertto: dict | i |
	(1 to: objects length) do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [dict insert: values ◦ i with: objects ◦ i]].
	^dict
! !
!Dictionary methodsFor: 'Inversion' stamp: ''!
invert | |
	^self invertto: (Dictionary new init: objects length)
! !
!Dictionary methodsFor: 'Private' stamp: ''!
swap: i with: j | |
	values swap: i with: j.
	super swap: i with: j
! !
!Dictionary methodsFor: 'Private' stamp: ''!
growto: size | name copy |
	copy ← self class new init: size "create a copy of the new size".
	self do: [:name | copy insert: name with: self ◦ name].
	self copyfrom: copy "hash each entry into it"
! !
!Dictionary methodsFor: 'Private' stamp: ''!
values | |
	^values
! !
!Dictionary methodsFor: 'Private' stamp: ''!
rehash | i copy |
	copy ← Dictionary new init: self size "create a copy".
	(1 to: objects length) "hash each entry into it" do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [copy insert: objects ◦ i with: values ◦ i]].
	self copyfrom: copy
! !
!Dictionary methodsFor: 'Searching' stamp: ''!
lookup: name | x |
	(x ← self find: name)
	  ifTrue: [^values ◦ x].
	^false
! !
!Dictionary methodsFor: 'Searching' stamp: ''!
◦ name ← val | |
	^values ◦ (self findorerror: name) ← val
! !
!Dictionary methodsFor: 'Searching' stamp: ''!
◦ name | |
	^values ◦ (self findorerror: name)
! !
Object subclass: #DictionaryEntry
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!DictionaryEntry methodsFor: 'Filing' stamp: ''!
readFrom: file | |
	self subError "inverse of storeOn:"
! !
!DictionaryEntry methodsFor: 'Filing' stamp: ''!
storeOn: file | |
	self subError "store self as fileSize characters on file"
! !
!DictionaryEntry methodsFor: 'Filing' stamp: ''!
fileSize | |
	self subError "size in characters for filing"
! !
!DictionaryEntry methodsFor: 'Initialize' stamp: ''!
dictionary: dict | |
	
! !
!DictionaryEntry methodsFor: 'Initialize' stamp: ''!
name: name | |
	
! !
!DictionaryEntry methodsFor: 'Initialize' stamp: ''!
init | |
	
! !
!DictionaryEntry methodsFor: 'Other' stamp: ''!
match: entry | |
	self subError "does self (some kind of pattern) match entry?"
! !
!DictionaryEntry methodsFor: 'Other' stamp: ''!
dictionary | |
	^false "what dictionary did I come from?"
! !
Object subclass: #HashSet
	instanceVariableNames: 'objects'
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!HashSet methodsFor: 'Access to parts' stamp: ''!
contents | obj strm |
	strm ← (Vector new: objects length) asStream.
	objects do:
		[:obj | obj ≡ nil
		  ifFalse: [strm next← obj]].
	^strm contents
! !
!HashSet methodsFor: 'Access to parts' stamp: ''!
asStream | |
	^self contents asStream
! !
!HashSet methodsFor: 'Access to parts' stamp: ''!
size | |
	^objects length
! !
!HashSet methodsFor: 'As yet unclassified' stamp: ''!
lookup: obj | i |
	 "⇑object if found, else false (object=obj but may not ≡obj)"
	i ← self findornil: obj.
	objects ◦ i = obj
	  ifTrue: [^objects ◦ i].
	^false
! !
!HashSet methodsFor: 'Growing and shrinking' stamp: ''!
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
!HashSet methodsFor: 'Growing and shrinking' stamp: ''!
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
!HashSet methodsFor: 'Initialization' stamp: ''!
init: size | |
	objects ← Vector new: (size max: 2)
! !
!HashSet methodsFor: 'Initialization' stamp: ''!
init | |
	self init: 4
! !
!HashSet methodsFor: 'Initialization' stamp: ''!
copy | |
	 "⇑ a copy of me"
	^self class new copyfrom: self
! !
!HashSet methodsFor: 'Initialization' stamp: ''!
copyfrom: hset | |
	 "take on state of hset"
	objects ← hset objects copy
! !
!HashSet methodsFor: 'Insertion and deletion' stamp: ''!
insert: obj | i |
	self findorinsert: obj.
	^obj
! !
!HashSet methodsFor: 'Insertion and deletion' stamp: ''!
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
!HashSet methodsFor: 'Insertion and deletion' stamp: ''!
insertall: objs | x |
	objs do: [:x | self insert: x]
! !
!HashSet methodsFor: 'Private' stamp: ''!
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
!HashSet methodsFor: 'Private' stamp: ''!
sparse | i n |
	n ← objects length "⇑true if (1 max: 1/4 of table) is nil".
	(1 to: objects length) do:
		[:i | objects ◦ i ≡ nil
		  ifTrue:
			[(n ← n - 4) ≤ 0
			  ifTrue: [^true]]].
	^false
! !
!HashSet methodsFor: 'Private' stamp: ''!
objects← t1 | |
	objects ← t1
! !
!HashSet methodsFor: 'Private' stamp: ''!
objects | |
	^objects
! !
!HashSet methodsFor: 'Private' stamp: ''!
swap: i with: j | |
	objects swap: i with: j
! !
!HashSet methodsFor: 'Private' stamp: ''!
rawinsert: t1 | t2 |
	 "assumes there is room for the new one"
	t2 ← self findornil: t1.
	objects ◦ t2 ← t1.
	^t2
! !
!HashSet methodsFor: 'Private' stamp: ''!
growto: t1 | t2 t3 |
	 "faster insert for growing"
	t2 ← self class new init: t1.
	t1 < objects length
	  ifTrue: [self do: [:t3 | t2 insert: t3]]
	  ifFalse: [self do: [:t3 | t2 rawinsert: t3]].
	objects ← t2 objects
! !
!HashSet methodsFor: 'Private' stamp: ''!
rehash | i copy |
	copy ← HashSet new init: self size "create a copy".
	(1 to: objects length) "hash each entry into it" do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [copy insert: objects ◦ i]].
	objects ← copy objects
! !
!HashSet methodsFor: 'Private' stamp: ''!
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
!HashSet methodsFor: 'Searching' stamp: ''!
findorerror: name | i |
	i ← self findornil: name.
	objects ◦ i = name
	  ifTrue: [^i].
	 "allow the user to put a correct value into i"
	user notify: name asString + ' cannot be found'.
	^i
! !
!HashSet methodsFor: 'Searching' stamp: ''!
has: obj | |
	^objects ◦ (self findornil: obj) = obj
! !
!HashSet methodsFor: 'Searching' stamp: ''!
find: obj | i |
	 "⇑index if found, else false"
	i ← self findornil: obj.
	objects ◦ i = obj
	  ifTrue: [^i].
	^false
! !
LADDictionary subclass: #IdentityDictionary
	instanceVariableNames: 'values'
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
findOrNil: key | index len probe pass |
	len ← self length. pass ← 1.
	index ← key hash \ len + 1.
	[(probe ← self elementAt: index) ≡ nil or: [probe ≡ key]] whileFalseDo: 
		[(index ← index + 1) > len ifTrue:
			[index ← 1.
			pass ← pass + 1.
			pass > 2 ifTrue: [^self grow findOrNil: key]]].
	^index
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
at: key put: value | index |
	index← self findOrNil: key.
	(self elementAt: index) ≡ nil ifTrue:
		[tally← tally+1.
		self elementAt: index put: key].
	^values◦index ← value
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
invert: value | index |
	index← values find: value.
	index=0 ifTrue: [^false].
	^self elementAt: index
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
init | |
	tally← 0.
	values← Vector new: self length
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
noCheckAdd: key with: value | index |
	index← self findOrNil: key.
	self elementAt: index put: key.
	tally ← tally + 1.
	^values◦index← value
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
swap: one with: other  | save |
	super swap: one with: other.
	values swap: one with: other
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
test | i | "(IdentityDictionary init: 1) test"
	(17 to: 200 by: 13) do: [:i | self at: i put: 5*i].
	(17 to: 200 by: 13) do: [:i | (self at: i)≠(5*i) ifTrue: [user notify: 'bad value']].
	self delete: 30.
	tally≠self contents length ifTrue: [user notify: 'bad tally']
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
fromOld: old | len i |  "build from an old MethodDictionary"
	tally← old tally.
	values← Vector new: (len← old length).
	1 to: len do: [:i | old◦i≡nil ifFalse: 
		[self◦i← (old◦i) key.
		values◦i← old◦i]]
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
copy | v copy |
	v← values. values← values copy.
	copy← super copy. values← v.
	^copy
	
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
deleteDangerously: key  | loc old len entry |
	(loc ← self find: key)=0 ifTrue: [self error: 'key not found'].
	self elementAt: loc put: nil.  values◦loc← nil.
	tally← tally - 1.
	len ← self length.
	[loc ← loc = len ifTrue: [1] ifFalse: [loc + 1].
		(self elementAt: loc)≡ nil]
		whileFalseDo:
			[old ← self findOrNil: (self elementAt: loc).
			loc=old ifFalse: [self swap: loc with: old]]
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
delete: key | |
	self become: (self copy deleteDangerously: key)
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
accept: assoc at: index | |  "preserves original assoc if its there"
	(self elementAt: index)≡nil
		ifTrue: [self elementAt: index put: assoc key.
			tally← tally+1.
			values◦index← assoc value.
			self fullCheck]
		ifFalse: [values◦index← assoc value]
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
at: key | |
	^values ◦ (self find: key)
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
method: key | |  "**change this to at: in all clients**"
	^self at: key
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
add: assoc | |
	self at: assoc key put: assoc value
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
keyAt: index | |
	^self elementAt: index
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
matchKey: key at: index | t |
	(t← self elementAt: index)≡nil ifTrue: [^false].
	^t ≡ key
! !
!IdentityDictionary methodsFor: 'As yet unclassified' stamp: ''!
grow | copy i | "ought to be able to share super code"
	copy ← self species init: self length * 2.
	1 to: self length do:
		[:i | self ◦ i ≡ nil ifFalse: [copy noCheckAdd: self ◦ i with: values◦i]].
	self become: copy
! !
LADSet subclass: #LADDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
contents | s i |
	s ← Stream new of: (Vector new: tally).
	1 to: self length do:
		[:i | self ◦ i ≡ nil ifFalse: [s next← self keyAt: i]].
	^s contents
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
noCheckAdd: anAssociation | |
	self elementAt: (self findOrNil: anAssociation key) put: anAssociation.
	tally ← tally + 1.
	^anAssociation
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
at: key ifAbsent⦂ expr | loc |
	loc← self find: key.
	loc=0 ifTrue: [^expr eval].
	^self elementAt: loc
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
delete: key  | loc old len entry |
	(loc ← self find: key)=0 ifTrue: [self error: 'key not found'].
	self elementAt: loc put: nil.
	tally← tally - 1.
	len ← self length.
	[loc ← loc = len ifTrue: [1] ifFalse: [loc + 1].
		(self elementAt: loc)≡ nil]
		whileFalseDo:
			[old ← self findOrNil: (self elementAt: loc) key.
			loc=old ifFalse: [self swap: loc with: old]]
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
accept: assoc at: index | |  "preserves original assoc if its there"
	(self elementAt: index)≡nil
		ifTrue: [tally← tally+1.
			self elementAt: index put: assoc.
			self fullCheck]
		ifFalse: [(self elementAt: index) value← assoc value]
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
has: key | |
	^(self find: key) ≠ 0
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
add: anAssociation | |
	self accept: anAssociation at: (self findOrNil: anAssociation key).
	^anAssociation
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
keyAt: index | |
	^(self elementAt: index) key
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
findOrNil: key | index len probe pass |
	len ← self length. pass ← 1.
	index ← key hash \ len + 1.
	[(probe ← self elementAt: index) ≡ nil or: [probe key = key]]
	whileFalseDo: 
		[(index ← index + 1) > len ifTrue:
			[index ← 1.
			pass ← pass + 1.
			pass > 2 ifTrue: [^self grow findOrNil: key]]].
	^index
! !
!LADDictionary methodsFor: 'As yet unclassified' stamp: ''!
find: key | index |
	index← self findOrNil: key.
	(self elementAt: index) ≡ nil ifTrue: [^0].
	(self keyAt: index) = key
		ifTrue: [^index]
		ifFalse: [^0]
! !
Object subclass: #LADSet
	instanceVariableNames: 'tally'
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
elementAt: index | |
	^self ◦ index
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
tally | |
	^tally
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
at: index put: anAssociation | |
	^self ◦ index ← anAssociation
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
init | |
	tally ← 0
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
swap: one with: other  | save |
	save ← self ◦ one.
	self ◦ one ← self ◦ other.
	self ◦ other ← save
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
asStream  | |
	^self contents asStream
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
at: index | |
	^self ◦ index
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
fullCheck | |
	self length - tally ≤ (self length / 4)
	  ifTrue: [self grow]
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
elementAt: index put: value | |
	^self ◦ index ← value
! !
!LADSet methodsFor: 'As yet unclassified' stamp: ''!
grow | copy i |
	copy ← self species init: self length * 2.
	1 to: self length do:
		[:i | self ◦ i ≡ nil ifFalse: [copy noCheckAdd: self ◦ i]].
	self become: copy
! !
Dictionary subclass: #SymbolTable
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!SymbolTable methodsFor: 'Access to parts' stamp: ''!
ref: name | |
	^super ◦ name
! !
!SymbolTable methodsFor: 'Access to parts' stamp: ''!
ref: name ← val | |
	^super ◦ name ← val
! !
!SymbolTable methodsFor: 'As yet unclassified' stamp: ''!
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
!SymbolTable methodsFor: 'Growing and shrinking' stamp: ''!
clean | name |
	 "release unreferenced entries"
	self do:
		[:name |  "slick, huh"
		(super ◦ name) refct = 1
		  ifTrue: [self delete: name]]
! !
!SymbolTable methodsFor: 'Growing and shrinking' stamp: ''!
growto: size | name copy |
	copy ← self class new init: size "create a copy of the new size".
	self do: [:name | copy insert: name withref: (self ref: name)].
	self copyfrom: copy "hash each entry into it"
! !
!SymbolTable methodsFor: 'Growing and shrinking' stamp: ''!
rehash | i copy |
	copy ← SymbolTable new init: self size "create a copy".
	(1 to: objects length) "hash each entry into it" do:
		[:i | objects ◦ i ≡ nil
		  ifFalse: [copy insert: objects ◦ i withref: values ◦ i]].
	self copyfrom: copy
! !
!SymbolTable methodsFor: 'Insertion' stamp: ''!
insert: name with: x | |
	(self has: name)
	  ifFalse: [super insert: name with: Association new].
	self ◦ name ← x
! !
!SymbolTable methodsFor: 'Insertion' stamp: ''!
define: name as: x | |
	 "synonym"
	^self declare: name as: x
! !
!SymbolTable methodsFor: 'Insertion' stamp: ''!
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
!SymbolTable methodsFor: 'Insertion' stamp: ''!
insert: name withref: ref | |
	super insert: name with: ref
! !
!SymbolTable methodsFor: 'Insertion' stamp: ''!
declare: name | |
	 "Take ref(s) and value(s) from Undeclared, if name(s) there"
	self declare: name from: Undeclared
! !
!SymbolTable methodsFor: 'Insertion' stamp: ''!
◦ name ← x | |
	^super ◦ name value← x
! !
!SymbolTable methodsFor: 'Insertion' stamp: ''!
declare: name as: x | a s |
	(name is: Vector)
	  ifTrue:
		[s ← x asStream.
		name do: [:a | self declare: a as: s next]]
	  ifFalse:
		[self declare: name.
		self ◦ name ← x]
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
allCallsOn: selector ||
	^self allCallsOn: selector from: user classNames
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
allRefs | |
	 "what methods reference my variables (I am probably 'Undeclared')"
	^self allRefsTo: self contents from: user classNames
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
lookup: name | r |
	(r ← super lookup: name)
	  ifTrue: [^r value].
	^false
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
invert: obj | i |
	(1 to: values length) do:
		[:i | nil ≡ (values ◦ i)
		  ifFalse:
			[obj ≡ (values ◦ i) value
			  ifTrue: [^objects ◦ i]]].
	^false
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
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
					[s append: className; append: '⇒'.
					s append: w asString; cr]]])].
	^s contents
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
invertBlock: blk | i str|
	str ← Stream new of: (String new: 10).
	(1 to: values length) do:
		[:i | nil ≡ (values ◦ i)
		  ifFalse:
			[(blk value: (values ◦ i) value)
			  ifTrue: [objects ◦ i printOn: str]]].
	^str
"(Smalltalk invertBlock: [:i | i class≡Cursor]) inspect."
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
allRefsTo: symbol from: classNames | s |
	(symbol is: Vector)
	  ifFalse: [symbol ← symbol inVector].
	^Smalltalk allCallsOn: (symbol transform⦂ [:s | s] to⦂ [(self ref: s)]) from: classNames "
Smalltalk allRefsTo: ↪ST80 from: (SystemOrganization superclassOrder: 'S80-Compiler')
"
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
◦ name | |
	^(super ◦ name) value
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
lookupRef: name | |
	^super lookup: name
! !
!SymbolTable methodsFor: 'Searching' stamp: ''!
invertRef: obj | i |
	(1 to: values length) do:
		[:i | obj ≡ (values ◦ i)
		  ifTrue: [^objects ◦ i]].
	^false
! !
ClassOrganizer subclass: #SystemOrganizer
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'Sets and Dictionaries'!
!SystemOrganizer methodsFor: 'Filout and printing' stamp: ''!
printCategory: cat | |
	user displayoffwhile⦂ [((dp0 file: (cat + '.press') asFileName) printoutclass: (self superclassOrder: cat))]
! !
!SystemOrganizer methodsFor: 'Filout and printing' stamp: ''!
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
!SystemOrganizer methodsFor: 'Filout and printing' stamp: ''!
printAll | cat |
	commentVector do: [:cat | self printCategory: cat]
! !
!SystemOrganizer methodsFor: 'Filout and printing' stamp: ''!
filoutCategory: cat | all a |
	user displayoffwhile⦂ [
		(all ← self superclassOrder: cat.
		(dp0 file: (cat + '.st.') asFileName) filoutclass: all.
		all do: [:a | (Smalltalk ◦ a) noChanges])]
! !
