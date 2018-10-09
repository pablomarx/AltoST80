class VImage {
	constructor(data) {
		this.data = data;
		this.reset();
	}
	copy() {
		return new VImage(this.data);
	}
	next() {
		return this.data[this.position++];
	}
	nextWord() {
		var hi = this.next();
		var lo = this.next();
		return (hi << 8) | lo;
	}
	skip(n) {
		this.position += n;
	}
	reset() {
		this.position = 0;
	}
}

class VMem {
	constructor() {
		this.MT = 32;
		this.PTRS = 64;
		this.ODD = 128;
		this.HeaderSize = 512;
	}
	help() {
		console.log("Things to try: ");
		console.log("Gather all UniqueStrings(Symbols):");
		console.log("image.allInstancesOf(image.classWithName('UniqueString')).map(obj => { return obj.stringValue(); })")
		console.log("Gather all Strings:");
		console.log("image.allInstancesOf(image.classWithName('String')).map(obj => { return obj.stringValue(); })")
		console.log("Gather all Class names:");
		console.log("image.allClasses().map(obj => { return obj.fields[0].stringValue(); })")
	}
	readImage(data) {
		this.image = new VImage(data);
		
		var OTbase = this.image.nextWord() * 65536;
		OTbase = OTbase + ((Math.floor((this.image.nextWord() - 1) / 256) + 1) * 256);
		OTbase = (OTbase * 2) + this.HeaderSize;
		this.OTbase = OTbase;
		this.scan();
		this.help();
	}
	isInteger(oop) {
		return ((oop % 2) == 1);
	}
	integerValue(oop) {
		return (oop >> 1);
	}
	objectFor(oop) {
		if (this.isInteger(oop) == true) {
			return this.integerValue(oop);
		}
		
		if (this.objects[oop] != null) {
			return this.objects[oop];
		}

		var data = this.image.copy();
		
		data.position = this.OTbase + (oop * 2);
		var refct = data.next();
		var flags = data.next();
		var addr = data.nextWord();
		data.position = ((((flags & 15) * 65536) + addr) * 2) + this.HeaderSize;
		var length = data.nextWord();
		var cls = data.nextWord();
		
		var numFields = length;
		var isPointers = ((flags & this.PTRS) == this.PTRS);
		var isMethod = (isPointers == false && cls == 17);
		if (isMethod == true) {
			data.skip(2);
			var nlits = (data.nextWord() & 0x7E) / 2;
			data.skip(-4);
			numFields = nlits+2;
		}
		else if (isPointers == false) {
			numFields = ((numFields - 2) * 2);
			if ((flags & this.ODD) == this.ODD) {
				numFields += 1;
			}
		}
		
		var fields = [];
		var object = {
			oop: oop,
			cls: cls,
			refct: refct,
			flags: flags,
			addr: addr,
			length: length,
			fields: fields,
			stringValue: function() { return fields.map(i=>{ return i == 0 ? "" : String.fromCharCode(i)}).join("") }
		};
		
		this.objects[oop] = object;
		
		if (object.cls == oop) {
			console.log("class oop: "+oop)
			object.cls = object;
		}
		else {
			object.cls = this.objectFor(object.cls);
		}

		for (var i=1; i<=numFields; i++) {
			if (isPointers == true) {
				var field = data.nextWord();
				if (this.isInteger(field)) {
					field = this.integerValue(field);
				}
				else {
					field = this.objectFor(field);
				}
				fields.push(field);
			}
			else if (isMethod == true) {
				var field = data.nextWord();
				if (field % 2 == 0) {
					field = field >> 1;
				}
				fields.push(field);
			}
			else {
				fields.push(data.next());
			}
		}
		return object;
	}
	allInstancesOf(cls) {
		return this.objects.filter(anObject => {
			return (anObject.cls == cls);
		})
	}
	allClasses() {
		if (true) {
			var clsobj = this.objectFor(10);
			return this.allInstancesOf(clsobj);
		}
		else {
			var classes = [];
			for (var i=0; i<this.objects.length; i++) {
				var obj = this.objects[i];
				if (obj == null) {
					continue;
				}
				if (classes.indexOf(obj.cls) == -1) {
					classes.push(obj.cls);
				}
			}
			return classes;
		}
	}
	classWithName(name) {
		return this.allClasses().find(anObject => {
			var title = anObject.fields[0].stringValue();
			return title == name;
		});
	}
	scan() {
		this.image.position = 0;

		var enddata = this.image.nextWord();
		enddata = (enddata * 65536) + this.image.nextWord();
		enddata = (enddata * 2) + this.HeaderSize;
		var maxoop = this.image.nextWord();
		maxoop = (maxoop * 65536) + this.image.nextWord();
		maxoop = (maxoop / 2) - 1;
		console.log("enddata: "+enddata+", maxoop: "+maxoop);
		
		this.image.position = this.OTbase + 4;
		this.objects = new Array(maxoop);

		for (var oop=0; oop<maxoop; oop+=2) {
			var obj = this.objectFor(oop);
			if (typeof obj == "number") {
				throw "weird, got an integer for oop "+oop;
			}
		}
	}
}
