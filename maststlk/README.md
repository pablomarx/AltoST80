sources.st is a lightly parsed over copy of st80feb8.sources from [maststlk.dsk](http://bitsavers.org/bits/Xerox/Alto/disk_images/maststlk.dsk.Z). While this file is complete, it does not include any class definitions as they aren't in the original sources file.

The other files are based on sources.st, in combination with the js-parser code. They do include class definitions. 

altoSt80feb8.im (from [maststlk.dsk](http://bitsavers.org/bits/Xerox/Alto/disk_images/maststlk.dsk.Z)) was loaded into js-parser, the SystemOrganization object was found (via sole instance of SystemOrganizer), the category names and associated class names were retrieved from that.

```
var sysorg = image.allInstancesOf(image.classWithName("SystemOrganizer"))[0];
var titles = sysorg.fields[1].fields;

for (var i=0; i<titles.length-2; i++) {
	console.log("Category: " +titles[i].stringValue());
	var classes = sysorg.fields[2].fields[i].fields;
	for (var j=0; j<classes.length-2; j++) {
		var clsname = classes[j].stringValue();
		console.log("  => "+clsname);
	}
}
```
