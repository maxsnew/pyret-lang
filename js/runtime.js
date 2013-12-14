var PYRET = (function () {

    function makeRuntime() {

	function PNothing() {

	}
	function isNothing(v) { return v instanceof PNothing; }
	var nothing = new PNothing();

	function PMethod(f) {
	    this.method = f;
	}
	function makeMethod(f) { return new PMethod(f); } 
	function isMethod(v) { return v instanceof PMethod; }
	PMethod.prototype = {
	    app: function() { throw "Cannot apply method directly."; },
	    dict: {}
	};

	function PFunction(f) {
	    this.app = f;
	}
	function makeFunction(f) { return new PFunction(f); }
	function isFunction(v) { return v instanceof PFunction; }
	PFunction.prototype = {
	    dict: {} 
	};

	var numberDict = {
	    _plus: makeMethod(function(left, right) {
		return makeNumber(left.n + right.n);
	    }),
	    _equals: makeMethod(function(left, right) {
		return makeBool(left.n === right.n);
	    })
	};

	function PNumber(n) {
	    this.n = n;
	}
	function makeNumber(n) { return new PNumber(n); }
	function isNumber(v) { return v instanceof PNumber; }
	PNumber.prototype = {
	    dict : numberDict
	};

	var stringDict = {
	    _plus: makeMethod(function(left, right) {
		return makeString(left.s + right.s);
	    })
	};

	function PBool(b) {
	    this.b = b;
	}
	function makeBool(b) { return new PBool(b); }
	function isBool(v) { return v instanceof PBool; }
	function isTrue(v) { return isBool(v) && v.b === true; }

	function PString(s) {
	    this.s = s;
	}
	function makeString(s) { return new PString(s); }
	function isString(v) { return v instanceof PString; }
	PString.prototype = {
	    dict : stringDict
	};

	function PObject(dict) {
	    this.dict = dict;
	}
	function isObject(v) { return v instanceof PObject; }
	function makeObject(o) { return new PObject(o); }

	function equal(val1, val2) {
	    if(isNumber(val1) && isNumber(val2)) {
		return val1.n === val2.n;
	    }
	    else if (isString(val1) && isString(val2)) {
		return val1.s === val2.s;
	    }
	    return false;
	}

	function toRepr(val) {
	    if(isNumber(val)) {
		return makeString(String(val.n));
	    }
	    else if (isString(val)) {
		return makeString('"' + val.s + '"');
	    }
	    else if (isFunction(val)) {
		return makeString("fun: end");
	    }
	    else if (isMethod(val)) {
		return makeString("method: end");
	    }
	    else if (isNothing(val)) {
		return makeString("nothing");
	    }
	    else if (isObject(val)) {
		var arr = [];
		for (var i in val.dict) {
		    arr.push(i + ": " + toRepr(val.dict[i]).s);
		}
		return makeString("{" + arr.join(", ") + "}");
	    }
	    throw ("toStringJS on an unknown type: " + val);
	}

	function getField(val, str) {
	    var fieldVal = val.dict[str];
	    if (isMethod(fieldVal)) {
		return makeFunction(function() {
		    var argList = Array.prototype.slice.call(arguments);
		    return fieldVal.method.apply(null, [val].concat(argList));
		});
	    } else {
		return fieldVal;
	    }
	}

	var testPrintOutput = "";
	function testPrint(val) {
	    var str = toRepr(val).s;
	    console.log("testPrint: ", val, str);
	    testPrintOutput += str + "\n";
	    return val;
	}

	function NormalResult(val, namespace) {
	    this.val = val;
	    this.namespace = namespace;
	}
	function makeNormalResult(val, ns) { return new NormalResult(val, ns); }

	function FailResult(exn) {
	    this.exn = exn;
	}
	function makeFailResult(exn) { return new FailResult(exn); }

	function PyretException(exnVal) {
	    this.exnVal = exnVal;
	}
	function makePyretException(exnVal) {
	    return new PyretException(exnVal);
	}

	function errToJSON(exn) {
	    return JSON.stringify({exn: String(exn)})
	}

	return {
	    namespace: Namespace({
		nothing: nothing,
		"test-print": makeFunction(testPrint),
		brander: makeFunction(function() {
		    throw "brander NYI";
		}),
		"check-brand": makeFunction(function() {
		    throw "check-brand NYI";
		}),
		Function: makeFunction(function() {
		    throw "function NYI";
		}),
		builtins: makeObject({
		    equiv: makeMethod(function (left, right) {
			if (isNumber(left) && isNumber(right)) {
			    return makeBool(equal(left, right));
			}

			return makeBool(false);
		    })
		})
	    }),
	    runtime: {
		makeObject: makeObject,
		makeNumber: makeNumber,
		makeFunction: makeFunction,
		isNumber: isNumber,
		isObject: isObject,
		makeBool: makeBool,
		isBool: isBool,
		isTrue: isTrue,
		equal: equal,
		getField: getField,
		getTestPrintOutput: function(val) {
		    return testPrintOutput + toRepr(val).s;
		},
		NormalResult: NormalResult,
		FailResult: FailResult,
		PyretException: PyretException,
		makeNormalResult: makeNormalResult,
		makeFailResult: makeFailResult,
		makePyretException: makePyretException,
		toReprJS: toRepr,
		errToJSON: errToJSON
	    }
	}
    }

    return {
	makeRuntime: makeRuntime
    };
})();
