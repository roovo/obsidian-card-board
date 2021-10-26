(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $author$project$Worker$Worker$Loading = {$: 'Loading'};
var $author$project$Worker$Worker$ReceiveTime = function (a) {
	return {$: 'ReceiveTime', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $dillonkearns$elm_ts_json$TsJson$Decode$decoder = function (_v0) {
	var decoder_ = _v0.a;
	return decoder_;
};
var $author$project$InteropDefinitions$Flags = F4(
	function (folder, format, now, zone) {
		return {folder: folder, format: format, now: now, zone: zone};
	});
var $dillonkearns$elm_ts_json$TsJson$Decode$Decoder = F2(
	function (a, b) {
		return {$: 'Decoder', a: a, b: b};
	});
var $dillonkearns$elm_ts_json$Internal$TsJsonType$ArrayIndex = F2(
	function (a, b) {
		return {$: 'ArrayIndex', a: a, b: b};
	});
var $dillonkearns$elm_ts_json$Internal$TsJsonType$Intersection = function (a) {
	return {$: 'Intersection', a: a};
};
var $dillonkearns$elm_ts_json$Internal$TsJsonType$Optional = {$: 'Optional'};
var $dillonkearns$elm_ts_json$Internal$TsJsonType$Required = {$: 'Required'};
var $dillonkearns$elm_ts_json$Internal$TsJsonType$TsNever = {$: 'TsNever'};
var $dillonkearns$elm_ts_json$Internal$TsJsonType$TypeObject = function (a) {
	return {$: 'TypeObject', a: a};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $dillonkearns$elm_ts_json$Internal$TypeReducer$deduplicateBy = F2(
	function (toComparable, list) {
		return $elm$core$Dict$values(
			A3(
				$elm$core$List$foldl,
				F2(
					function (value, accum) {
						return A3(
							$elm$core$Dict$insert,
							toComparable(value),
							value,
							accum);
					}),
				$elm$core$Dict$empty,
				list));
	});
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm_community$dict_extra$Dict$Extra$insertDedupe = F4(
	function (combine, key, value, dict) {
		var _with = function (mbValue) {
			if (mbValue.$ === 'Just') {
				var oldValue = mbValue.a;
				return $elm$core$Maybe$Just(
					A2(combine, oldValue, value));
			} else {
				return $elm$core$Maybe$Just(value);
			}
		};
		return A3($elm$core$Dict$update, key, _with, dict);
	});
var $elm_community$dict_extra$Dict$Extra$fromListDedupeBy = F3(
	function (combine, keyfn, xs) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (x, acc) {
					return A4(
						$elm_community$dict_extra$Dict$Extra$insertDedupe,
						combine,
						keyfn(x),
						x,
						acc);
				}),
			$elm$core$Dict$empty,
			xs);
	});
var $dillonkearns$elm_ts_json$Internal$TypeReducer$either = F2(
	function (predicateFn, _v0) {
		var type1 = _v0.a;
		var type2 = _v0.b;
		return predicateFn(type1) || predicateFn(type2);
	});
var $dillonkearns$elm_ts_json$Internal$TypeReducer$isNonEmptyObject = function (tsType) {
	if ((tsType.$ === 'TypeObject') && tsType.a.b) {
		var _v1 = tsType.a;
		var atLeastOne = _v1.a;
		var possiblyMore = _v1.b;
		return true;
	} else {
		return false;
	}
};
var $dillonkearns$elm_ts_json$Internal$TypeReducer$isPrimitive = function (tsType) {
	switch (tsType.$) {
		case 'Number':
			return true;
		case 'Integer':
			return true;
		case 'String':
			return true;
		case 'Boolean':
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_ts_json$Internal$TypeReducer$isContradictory = function (types) {
	return A2($dillonkearns$elm_ts_json$Internal$TypeReducer$either, $dillonkearns$elm_ts_json$Internal$TypeReducer$isNonEmptyObject, types) && A2($dillonkearns$elm_ts_json$Internal$TypeReducer$either, $dillonkearns$elm_ts_json$Internal$TypeReducer$isPrimitive, types);
};
var $dillonkearns$elm_ts_json$Internal$TsJsonType$Unknown = {$: 'Unknown'};
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $dillonkearns$elm_ts_json$Internal$TypeToString$parenthesize = function (string) {
	return '(' + (string + ')');
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $dillonkearns$elm_ts_json$Internal$TypeToString$parenthesizeToString = function (type_) {
	var needsParens = function () {
		if (type_.$ === 'Union') {
			var types = type_.a;
			return true;
		} else {
			return false;
		}
	}();
	return needsParens ? ('(' + ($dillonkearns$elm_ts_json$Internal$TypeToString$toString(type_) + ')')) : $dillonkearns$elm_ts_json$Internal$TypeToString$toString(type_);
};
var $dillonkearns$elm_ts_json$Internal$TypeToString$toString = function (tsType_) {
	switch (tsType_.$) {
		case 'TsNever':
			return 'never';
		case 'String':
			return 'string';
		case 'Integer':
			return 'number';
		case 'Number':
			return 'number';
		case 'Boolean':
			return 'boolean';
		case 'Unknown':
			return 'JsonValue';
		case 'List':
			var listType = tsType_.a;
			return $dillonkearns$elm_ts_json$Internal$TypeToString$parenthesizeToString(listType) + '[]';
		case 'Literal':
			var literalValue = tsType_.a;
			return A2($elm$json$Json$Encode$encode, 0, literalValue);
		case 'Union':
			var _v1 = tsType_.a;
			var firstType = _v1.a;
			var tsTypes = _v1.b;
			return A2(
				$elm$core$String$join,
				' | ',
				A2(
					$elm$core$List$map,
					$dillonkearns$elm_ts_json$Internal$TypeToString$toString,
					A2($elm$core$List$cons, firstType, tsTypes)));
		case 'TypeObject':
			var keyTypes = tsType_.a;
			return '{ ' + (A2(
				$elm$core$String$join,
				'; ',
				A2(
					$elm$core$List$map,
					function (_v2) {
						var optionality = _v2.a;
						var key = _v2.b;
						var tsType__ = _v2.c;
						return function () {
							if (optionality.$ === 'Required') {
								return key;
							} else {
								return key + '?';
							}
						}() + (' : ' + $dillonkearns$elm_ts_json$Internal$TypeToString$toString(tsType__));
					},
					keyTypes)) + ' }');
		case 'ObjectWithUniformValues':
			var tsType = tsType_.a;
			return '{ [key: string]: ' + ($dillonkearns$elm_ts_json$Internal$TypeToString$toString(tsType) + ' }');
		case 'Tuple':
			var tsTypes = tsType_.a;
			var maybeRestType = tsType_.b;
			var restTypePart = A2(
				$elm$core$Maybe$map,
				function (restType) {
					return '...(' + ($dillonkearns$elm_ts_json$Internal$TypeToString$toString(restType) + ')[]');
				},
				maybeRestType);
			return '[ ' + (A2(
				$elm$core$String$join,
				', ',
				A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					_Utils_ap(
						A2(
							$elm$core$List$map,
							function (type_) {
								return $elm$core$Maybe$Just(
									$dillonkearns$elm_ts_json$Internal$TypeToString$toString(type_));
							},
							tsTypes),
						_List_fromArray(
							[restTypePart])))) + ' ]');
		case 'Intersection':
			var types = tsType_.a;
			return $dillonkearns$elm_ts_json$Internal$TypeToString$parenthesize(
				A2(
					$elm$core$String$join,
					' & ',
					A2($elm$core$List$map, $dillonkearns$elm_ts_json$Internal$TypeToString$toString, types)));
		default:
			var _v4 = tsType_.a;
			var index = _v4.a;
			var tsType = _v4.b;
			var otherIndices = tsType_.b;
			var dict = $elm$core$Dict$fromList(
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(index, tsType),
					otherIndices));
			var highestIndex = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$List$maximum(
					$elm$core$Dict$keys(dict)));
			return '[' + (A2(
				$elm$core$String$join,
				',',
				_Utils_ap(
					A2(
						$elm$core$List$map,
						function (cur) {
							return $dillonkearns$elm_ts_json$Internal$TypeToString$toString(
								A2(
									$elm$core$Maybe$withDefault,
									$dillonkearns$elm_ts_json$Internal$TsJsonType$Unknown,
									A2($elm$core$Dict$get, cur, dict)));
						},
						A2($elm$core$List$range, 0, highestIndex)),
					_List_fromArray(
						['...JsonValue[]']))) + ']');
	}
};
var $dillonkearns$elm_ts_json$Internal$TypeReducer$intersect = F2(
	function (type1, type2) {
		if ($dillonkearns$elm_ts_json$Internal$TypeReducer$isContradictory(
			_Utils_Tuple2(type1, type2))) {
			return $dillonkearns$elm_ts_json$Internal$TsJsonType$TsNever;
		} else {
			if (_Utils_eq(type1, type2)) {
				return type1;
			} else {
				var _v8 = _Utils_Tuple2(type1, type2);
				_v8$1:
				while (true) {
					_v8$8:
					while (true) {
						switch (_v8.a.$) {
							case 'Unknown':
								var _v9 = _v8.a;
								var known = _v8.b;
								return known;
							case 'Intersection':
								switch (_v8.b.$) {
									case 'Unknown':
										break _v8$1;
									case 'Intersection':
										var types1 = _v8.a.a;
										var types2 = _v8.b.a;
										return $dillonkearns$elm_ts_json$Internal$TypeReducer$simplifyIntersection(
											_Utils_ap(types1, types2));
									default:
										break _v8$8;
								}
							case 'ArrayIndex':
								switch (_v8.b.$) {
									case 'Unknown':
										break _v8$1;
									case 'ArrayIndex':
										if ((!_v8.a.b.b) && (!_v8.b.b.b)) {
											var _v11 = _v8.a;
											var _v12 = _v11.a;
											var index1 = _v12.a;
											var indexType1 = _v12.b;
											var _v13 = _v8.b;
											var _v14 = _v13.a;
											var index2 = _v14.a;
											var indexType2 = _v14.b;
											return A2(
												$dillonkearns$elm_ts_json$Internal$TsJsonType$ArrayIndex,
												_Utils_Tuple2(index1, indexType1),
												_List_fromArray(
													[
														_Utils_Tuple2(index2, indexType2)
													]));
										} else {
											break _v8$8;
										}
									default:
										break _v8$8;
								}
							case 'TypeObject':
								switch (_v8.b.$) {
									case 'Unknown':
										break _v8$1;
									case 'TypeObject':
										var fields1 = _v8.a.a;
										var fields2 = _v8.b.a;
										return $dillonkearns$elm_ts_json$Internal$TsJsonType$TypeObject(
											A2($dillonkearns$elm_ts_json$Internal$TypeReducer$mergeFields, fields1, fields2));
									case 'Union':
										var fields1 = _v8.a.a;
										var unionedTypes = _v8.b.a;
										return $dillonkearns$elm_ts_json$Internal$TsJsonType$Intersection(
											_List_fromArray(
												[type1, type2]));
									default:
										break _v8$8;
								}
							case 'String':
								switch (_v8.b.$) {
									case 'Unknown':
										break _v8$1;
									case 'Number':
										var _v15 = _v8.a;
										var _v16 = _v8.b;
										return $dillonkearns$elm_ts_json$Internal$TsJsonType$TsNever;
									default:
										break _v8$8;
								}
							case 'Number':
								switch (_v8.b.$) {
									case 'Unknown':
										break _v8$1;
									case 'String':
										var _v17 = _v8.a;
										var _v18 = _v8.b;
										return $dillonkearns$elm_ts_json$Internal$TsJsonType$TsNever;
									default:
										break _v8$8;
								}
							default:
								if (_v8.b.$ === 'Unknown') {
									break _v8$1;
								} else {
									break _v8$8;
								}
						}
					}
					return _Utils_eq(type1, type2) ? $dillonkearns$elm_ts_json$Internal$TsJsonType$Intersection(
						_List_fromArray(
							[type1, type2])) : $dillonkearns$elm_ts_json$Internal$TsJsonType$Intersection(
						_List_fromArray(
							[type1, type2]));
				}
				var known = _v8.a;
				var _v10 = _v8.b;
				return known;
			}
		}
	});
var $dillonkearns$elm_ts_json$Internal$TypeReducer$mergeFields = F2(
	function (fields1, fields2) {
		return $elm$core$Dict$values(
			A3(
				$elm_community$dict_extra$Dict$Extra$fromListDedupeBy,
				F2(
					function (_v5, _v6) {
						var optionality1 = _v5.a;
						var fieldName1 = _v5.b;
						var fieldType1 = _v5.c;
						var optionality2 = _v6.a;
						var fieldName2 = _v6.b;
						var fieldType2 = _v6.c;
						return (_Utils_eq(optionality1, $dillonkearns$elm_ts_json$Internal$TsJsonType$Required) || _Utils_eq(optionality2, $dillonkearns$elm_ts_json$Internal$TsJsonType$Required)) ? _Utils_Tuple3(
							$dillonkearns$elm_ts_json$Internal$TsJsonType$Required,
							fieldName1,
							A2($dillonkearns$elm_ts_json$Internal$TypeReducer$intersect, fieldType1, fieldType2)) : _Utils_Tuple3($dillonkearns$elm_ts_json$Internal$TsJsonType$Optional, fieldName1, fieldType1);
					}),
				function (_v7) {
					var fieldName = _v7.b;
					return fieldName;
				},
				_Utils_ap(fields1, fields2)));
	});
var $dillonkearns$elm_ts_json$Internal$TypeReducer$simplifyIntersection = function (types) {
	var thing = function () {
		var _v0 = A2($dillonkearns$elm_ts_json$Internal$TypeReducer$deduplicateBy, $dillonkearns$elm_ts_json$Internal$TypeToString$toString, types);
		if (_v0.b) {
			if (!_v0.b.b) {
				var single = _v0.a;
				return single;
			} else {
				var first = _v0.a;
				var rest = _v0.b;
				if (first.$ === 'TypeObject') {
					var fields = first.a;
					var _v2 = A3(
						$elm$core$List$foldr,
						F2(
							function (thisType, _v3) {
								var objectsSoFar = _v3.a;
								var otherSoFar = _v3.b;
								if (thisType.$ === 'TypeObject') {
									var theseFields = thisType.a;
									return _Utils_Tuple2(
										A2($dillonkearns$elm_ts_json$Internal$TypeReducer$mergeFields, theseFields, objectsSoFar),
										otherSoFar);
								} else {
									return _Utils_Tuple2(
										objectsSoFar,
										A2($elm$core$List$cons, thisType, otherSoFar));
								}
							}),
						_Utils_Tuple2(fields, _List_Nil),
						rest);
					var otherObjects = _v2.a;
					var nonObjectTypes = _v2.b;
					return $dillonkearns$elm_ts_json$Internal$TsJsonType$Intersection(
						A2(
							$elm$core$List$cons,
							$dillonkearns$elm_ts_json$Internal$TsJsonType$TypeObject(otherObjects),
							nonObjectTypes));
				} else {
					return $dillonkearns$elm_ts_json$Internal$TsJsonType$Intersection(types);
				}
			}
		} else {
			return $dillonkearns$elm_ts_json$Internal$TsJsonType$TsNever;
		}
	}();
	return thing;
};
var $elm$json$Json$Decode$map2 = _Json_map2;
var $dillonkearns$elm_ts_json$TsJson$Decode$map2 = F3(
	function (mapFn, _v0, _v1) {
		var innerDecoder1 = _v0.a;
		var innerType1 = _v0.b;
		var innerDecoder2 = _v1.a;
		var innerType2 = _v1.b;
		return A2(
			$dillonkearns$elm_ts_json$TsJson$Decode$Decoder,
			A3($elm$json$Json$Decode$map2, mapFn, innerDecoder1, innerDecoder2),
			A2($dillonkearns$elm_ts_json$Internal$TypeReducer$intersect, innerType1, innerType2));
	});
var $dillonkearns$elm_ts_json$TsJson$Decode$andMap = $dillonkearns$elm_ts_json$TsJson$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$field = _Json_decodeField;
var $dillonkearns$elm_ts_json$TsJson$Decode$field = F2(
	function (fieldName, _v0) {
		var innerDecoder = _v0.a;
		var innerType = _v0.b;
		return A2(
			$dillonkearns$elm_ts_json$TsJson$Decode$Decoder,
			A2($elm$json$Json$Decode$field, fieldName, innerDecoder),
			$dillonkearns$elm_ts_json$Internal$TsJsonType$TypeObject(
				_List_fromArray(
					[
						_Utils_Tuple3($dillonkearns$elm_ts_json$Internal$TsJsonType$Required, fieldName, innerType)
					])));
	});
var $dillonkearns$elm_ts_json$Internal$TsJsonType$Integer = {$: 'Integer'};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $dillonkearns$elm_ts_json$TsJson$Decode$int = A2($dillonkearns$elm_ts_json$TsJson$Decode$Decoder, $elm$json$Json$Decode$int, $dillonkearns$elm_ts_json$Internal$TsJsonType$Integer);
var $dillonkearns$elm_ts_json$Internal$TsJsonType$String = {$: 'String'};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $dillonkearns$elm_ts_json$TsJson$Decode$string = A2($dillonkearns$elm_ts_json$TsJson$Decode$Decoder, $elm$json$Json$Decode$string, $dillonkearns$elm_ts_json$Internal$TsJsonType$String);
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $dillonkearns$elm_ts_json$TsJson$Decode$succeed = function (value_) {
	return A2(
		$dillonkearns$elm_ts_json$TsJson$Decode$Decoder,
		$elm$json$Json$Decode$succeed(value_),
		$dillonkearns$elm_ts_json$Internal$TsJsonType$Unknown);
};
var $author$project$InteropDefinitions$flags = A2(
	$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
	A2($dillonkearns$elm_ts_json$TsJson$Decode$field, 'zone', $dillonkearns$elm_ts_json$TsJson$Decode$int),
	A2(
		$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
		A2($dillonkearns$elm_ts_json$TsJson$Decode$field, 'now', $dillonkearns$elm_ts_json$TsJson$Decode$int),
		A2(
			$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
			A2($dillonkearns$elm_ts_json$TsJson$Decode$field, 'format', $dillonkearns$elm_ts_json$TsJson$Decode$string),
			A2(
				$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
				A2($dillonkearns$elm_ts_json$TsJson$Decode$field, 'folder', $dillonkearns$elm_ts_json$TsJson$Decode$string),
				$dillonkearns$elm_ts_json$TsJson$Decode$succeed($author$project$InteropDefinitions$Flags)))));
var $dillonkearns$elm_ts_json$TsJson$Encode$Encoder = F2(
	function (a, b) {
		return {$: 'Encoder', a: a, b: b};
	});
var $dillonkearns$elm_ts_json$Internal$TsJsonType$List = function (a) {
	return {$: 'List', a: a};
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $dillonkearns$elm_ts_json$TsJson$Encode$list = function (_v0) {
	var encodeFn = _v0.a;
	var tsType_ = _v0.b;
	return A2(
		$dillonkearns$elm_ts_json$TsJson$Encode$Encoder,
		function (input) {
			return A2($elm$json$Json$Encode$list, encodeFn, input);
		},
		$dillonkearns$elm_ts_json$Internal$TsJsonType$List(tsType_));
};
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $dillonkearns$elm_ts_json$TsJson$Encode$object = function (propertyEncoders) {
	var propertyTypes = $dillonkearns$elm_ts_json$Internal$TsJsonType$TypeObject(
		A2(
			$elm$core$List$map,
			function (_v1) {
				var optionality = _v1.a;
				var propertyName = _v1.b;
				var tsType_ = _v1.d;
				return _Utils_Tuple3(optionality, propertyName, tsType_);
			},
			propertyEncoders));
	var encodeObject = function (input) {
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$filterMap,
				function (_v0) {
					var propertyName = _v0.b;
					var encodeFn = _v0.c;
					return A2(
						$elm$core$Maybe$map,
						function (encoded) {
							return _Utils_Tuple2(propertyName, encoded);
						},
						encodeFn(input));
				},
				propertyEncoders));
	};
	return A2($dillonkearns$elm_ts_json$TsJson$Encode$Encoder, encodeObject, propertyTypes);
};
var $dillonkearns$elm_ts_json$TsJson$Encode$Property = F4(
	function (a, b, c, d) {
		return {$: 'Property', a: a, b: b, c: c, d: d};
	});
var $dillonkearns$elm_ts_json$TsJson$Encode$required = F3(
	function (name, getter, _v0) {
		var encodeFn = _v0.a;
		var tsType_ = _v0.b;
		return A4(
			$dillonkearns$elm_ts_json$TsJson$Encode$Property,
			$dillonkearns$elm_ts_json$Internal$TsJsonType$Required,
			name,
			function (input) {
				return $elm$core$Maybe$Just(
					encodeFn(
						getter(input)));
			},
			tsType_);
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $dillonkearns$elm_ts_json$TsJson$Encode$string = A2($dillonkearns$elm_ts_json$TsJson$Encode$Encoder, $elm$json$Json$Encode$string, $dillonkearns$elm_ts_json$Internal$TsJsonType$String);
var $author$project$InteropDefinitions$addFilePreviewHoversEncoder = $dillonkearns$elm_ts_json$TsJson$Encode$object(
	_List_fromArray(
		[
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'filePath',
			function ($) {
				return $.filePath;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'ids',
			function ($) {
				return $.ids;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$list($dillonkearns$elm_ts_json$TsJson$Encode$string))
		]));
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $dillonkearns$elm_ts_json$Internal$TsJsonType$Union = function (a) {
	return {$: 'Union', a: a};
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $dillonkearns$elm_ts_json$Internal$TypeReducer$union = function (tsTypes) {
	var withoutNevers = A2(
		$elm$core$List$filter,
		$elm$core$Basics$neq($dillonkearns$elm_ts_json$Internal$TsJsonType$TsNever),
		tsTypes);
	var hadNevers = !_Utils_eq(
		$elm$core$List$length(tsTypes),
		$elm$core$List$length(withoutNevers));
	if (!withoutNevers.b) {
		return hadNevers ? $dillonkearns$elm_ts_json$Internal$TsJsonType$TsNever : $dillonkearns$elm_ts_json$Internal$TsJsonType$Unknown;
	} else {
		if (!withoutNevers.b.b) {
			var singleType = withoutNevers.a;
			return singleType;
		} else {
			var first = withoutNevers.a;
			var rest = withoutNevers.b;
			return $dillonkearns$elm_ts_json$Internal$TsJsonType$Union(
				_Utils_Tuple2(first, rest));
		}
	}
};
var $dillonkearns$elm_ts_json$TsJson$Encode$unwrapUnion = function (_v0) {
	var rawValue = _v0.a;
	return rawValue;
};
var $dillonkearns$elm_ts_json$TsJson$Encode$buildUnion = function (_v0) {
	var toValue = _v0.a;
	var tsTypes_ = _v0.b;
	return A2(
		$dillonkearns$elm_ts_json$TsJson$Encode$Encoder,
		A2($elm$core$Basics$composeR, toValue, $dillonkearns$elm_ts_json$TsJson$Encode$unwrapUnion),
		$dillonkearns$elm_ts_json$Internal$TypeReducer$union(tsTypes_));
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $dillonkearns$elm_ts_json$TsJson$Encode$int = A2($dillonkearns$elm_ts_json$TsJson$Encode$Encoder, $elm$json$Json$Encode$int, $dillonkearns$elm_ts_json$Internal$TsJsonType$Integer);
var $author$project$InteropDefinitions$deleteTodoEncoder = $dillonkearns$elm_ts_json$TsJson$Encode$object(
	_List_fromArray(
		[
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'filePath',
			function ($) {
				return $.filePath;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'lineNumber',
			function ($) {
				return $.lineNumber;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$int),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'originalText',
			function ($) {
				return $.originalText;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string)
		]));
var $author$project$InteropDefinitions$markdownListEncoder = $dillonkearns$elm_ts_json$TsJson$Encode$object(
	_List_fromArray(
		[
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'id',
			function ($) {
				return $.id;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'markdown',
			function ($) {
				return $.markdown;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string)
		]));
var $author$project$InteropDefinitions$displayTodoMarkdownEncoder = $dillonkearns$elm_ts_json$TsJson$Encode$object(
	_List_fromArray(
		[
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'filePath',
			function ($) {
				return $.filePath;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'todoMarkdown',
			function ($) {
				return $.todoMarkdown;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$list($author$project$InteropDefinitions$markdownListEncoder))
		]));
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $dillonkearns$elm_ts_json$TsJson$Encode$UnionBuilder = F2(
	function (a, b) {
		return {$: 'UnionBuilder', a: a, b: b};
	});
var $dillonkearns$elm_ts_json$TsJson$Encode$union = function (constructor) {
	return A2($dillonkearns$elm_ts_json$TsJson$Encode$UnionBuilder, constructor, _List_Nil);
};
var $dillonkearns$elm_ts_json$TsJson$Encode$UnionEncodeValue = function (a) {
	return {$: 'UnionEncodeValue', a: a};
};
var $dillonkearns$elm_ts_json$TsJson$Encode$variant = F2(
	function (_v0, _v1) {
		var encoder_ = _v0.a;
		var tsType_ = _v0.b;
		var builder = _v1.a;
		var tsTypes_ = _v1.b;
		return A2(
			$dillonkearns$elm_ts_json$TsJson$Encode$UnionBuilder,
			builder(
				A2($elm$core$Basics$composeR, encoder_, $dillonkearns$elm_ts_json$TsJson$Encode$UnionEncodeValue)),
			A2($elm$core$List$cons, tsType_, tsTypes_));
	});
var $dillonkearns$elm_ts_json$Internal$TsJsonType$Literal = function (a) {
	return {$: 'Literal', a: a};
};
var $dillonkearns$elm_ts_json$TsJson$Encode$variantLiteral = F2(
	function (literalValue, _v0) {
		var builder = _v0.a;
		var tsTypes = _v0.b;
		return A2(
			$dillonkearns$elm_ts_json$TsJson$Encode$UnionBuilder,
			builder(
				$dillonkearns$elm_ts_json$TsJson$Encode$UnionEncodeValue(literalValue)),
			A2(
				$elm$core$List$cons,
				$dillonkearns$elm_ts_json$Internal$TsJsonType$Literal(literalValue),
				tsTypes));
	});
var $dillonkearns$elm_ts_json$TsJson$Encode$maybe = function (encoder_) {
	return $dillonkearns$elm_ts_json$TsJson$Encode$buildUnion(
		A2(
			$dillonkearns$elm_ts_json$TsJson$Encode$variant,
			encoder_,
			A2(
				$dillonkearns$elm_ts_json$TsJson$Encode$variantLiteral,
				$elm$json$Json$Encode$null,
				$dillonkearns$elm_ts_json$TsJson$Encode$union(
					F3(
						function (vNull, vJust, maybeValue) {
							if (maybeValue.$ === 'Just') {
								var justValue = maybeValue.a;
								return vJust(justValue);
							} else {
								return vNull;
							}
						})))));
};
var $author$project$InteropDefinitions$openTodoSourceFileEncoder = $dillonkearns$elm_ts_json$TsJson$Encode$object(
	_List_fromArray(
		[
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'filePath',
			function ($) {
				return $.filePath;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'blockLink',
			function ($) {
				return $.blockLink;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$maybe($dillonkearns$elm_ts_json$TsJson$Encode$string)),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'lineNumber',
			function ($) {
				return $.lineNumber;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$int),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'originalText',
			function ($) {
				return $.originalText;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string)
		]));
var $author$project$InteropDefinitions$todoUpdatesEncoder = $dillonkearns$elm_ts_json$TsJson$Encode$object(
	_List_fromArray(
		[
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'lineNumber',
			function ($) {
				return $.lineNumber;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$int),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'originalText',
			function ($) {
				return $.originalText;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'newText',
			function ($) {
				return $.newText;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string)
		]));
var $author$project$InteropDefinitions$updateTodosEncoder = $dillonkearns$elm_ts_json$TsJson$Encode$object(
	_List_fromArray(
		[
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'filePath',
			function ($) {
				return $.filePath;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$string),
			A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$required,
			'todos',
			function ($) {
				return $.todos;
			},
			$dillonkearns$elm_ts_json$TsJson$Encode$list($author$project$InteropDefinitions$todoUpdatesEncoder))
		]));
var $dillonkearns$elm_ts_json$TsJson$Encode$literal = function (literalValue) {
	return A2(
		$dillonkearns$elm_ts_json$TsJson$Encode$Encoder,
		function (_v0) {
			return literalValue;
		},
		$dillonkearns$elm_ts_json$Internal$TsJsonType$Literal(literalValue));
};
var $dillonkearns$elm_ts_json$TsJson$Encode$variantObject = F3(
	function (variantName, objectFields, unionBuilder) {
		return A2(
			$dillonkearns$elm_ts_json$TsJson$Encode$variant,
			$dillonkearns$elm_ts_json$TsJson$Encode$object(
				A2(
					$elm$core$List$cons,
					A3(
						$dillonkearns$elm_ts_json$TsJson$Encode$required,
						'tag',
						$elm$core$Basics$identity,
						$dillonkearns$elm_ts_json$TsJson$Encode$literal(
							$elm$json$Json$Encode$string(variantName))),
					objectFields)),
			unionBuilder);
	});
var $dillonkearns$elm_ts_json$TsJson$Encode$variantTagged = F3(
	function (tagName, dataEncoder, builder) {
		return A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$variantObject,
			tagName,
			_List_fromArray(
				[
					A3($dillonkearns$elm_ts_json$TsJson$Encode$required, 'data', $elm$core$Basics$identity, dataEncoder)
				]),
			builder);
	});
var $author$project$InteropDefinitions$fromElm = $dillonkearns$elm_ts_json$TsJson$Encode$buildUnion(
	A3(
		$dillonkearns$elm_ts_json$TsJson$Encode$variantTagged,
		'updateTodos',
		$author$project$InteropDefinitions$updateTodosEncoder,
		A3(
			$dillonkearns$elm_ts_json$TsJson$Encode$variantTagged,
			'openTodoSourceFile',
			$author$project$InteropDefinitions$openTodoSourceFileEncoder,
			A3(
				$dillonkearns$elm_ts_json$TsJson$Encode$variantTagged,
				'displayTodoMarkdown',
				$author$project$InteropDefinitions$displayTodoMarkdownEncoder,
				A3(
					$dillonkearns$elm_ts_json$TsJson$Encode$variantTagged,
					'deleteTodo',
					$author$project$InteropDefinitions$deleteTodoEncoder,
					A3(
						$dillonkearns$elm_ts_json$TsJson$Encode$variantTagged,
						'addFilePreviewHovers',
						$author$project$InteropDefinitions$addFilePreviewHoversEncoder,
						$dillonkearns$elm_ts_json$TsJson$Encode$union(
							F6(
								function (vAddFilePreviewHovers, vDeleteTodo, vDisplayTodoMarkdown, vOpenTodoSourceFile, vUpdateTodos, value) {
									switch (value.$) {
										case 'AddFilePreviewHovers':
											var info = value.a;
											return vAddFilePreviewHovers(info);
										case 'DeleteTodo':
											var info = value.a;
											return vDeleteTodo(info);
										case 'DisplayTodoMarkdown':
											var info = value.a;
											return vDisplayTodoMarkdown(info);
										case 'OpenTodoSourceFile':
											var info = value.a;
											return vOpenTodoSourceFile(info);
										default:
											var info = value.a;
											return vUpdateTodos(info);
									}
								}))))))));
var $author$project$InteropDefinitions$FileAdded = function (a) {
	return {$: 'FileAdded', a: a};
};
var $author$project$InteropDefinitions$FileDeleted = function (a) {
	return {$: 'FileDeleted', a: a};
};
var $author$project$InteropDefinitions$FileUpdated = function (a) {
	return {$: 'FileUpdated', a: a};
};
var $author$project$MarkdownFile$MarkdownFile = F3(
	function (filePath, fileDate, fileContents) {
		return {fileContents: fileContents, fileDate: fileDate, filePath: filePath};
	});
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $dillonkearns$elm_ts_json$TsJson$Decode$nullable = function (_v0) {
	var innerDecoder = _v0.a;
	var innerType = _v0.b;
	return A2(
		$dillonkearns$elm_ts_json$TsJson$Decode$Decoder,
		$elm$json$Json$Decode$nullable(innerDecoder),
		$dillonkearns$elm_ts_json$Internal$TypeReducer$union(
			_List_fromArray(
				[
					innerType,
					$dillonkearns$elm_ts_json$Internal$TsJsonType$Literal($elm$json$Json$Encode$null)
				])));
};
var $author$project$MarkdownFile$decoder = A2(
	$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
	A2($dillonkearns$elm_ts_json$TsJson$Decode$field, 'fileContents', $dillonkearns$elm_ts_json$TsJson$Decode$string),
	A2(
		$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
		A2(
			$dillonkearns$elm_ts_json$TsJson$Decode$field,
			'fileDate',
			$dillonkearns$elm_ts_json$TsJson$Decode$nullable($dillonkearns$elm_ts_json$TsJson$Decode$string)),
		A2(
			$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
			A2($dillonkearns$elm_ts_json$TsJson$Decode$field, 'filePath', $dillonkearns$elm_ts_json$TsJson$Decode$string),
			$dillonkearns$elm_ts_json$TsJson$Decode$succeed($author$project$MarkdownFile$MarkdownFile))));
var $dillonkearns$elm_ts_json$TsJson$Decode$oneOf = function (decoders) {
	return A2(
		$dillonkearns$elm_ts_json$TsJson$Decode$Decoder,
		$elm$json$Json$Decode$oneOf(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var innerDecoder = _v0.a;
					return innerDecoder;
				},
				decoders)),
		$dillonkearns$elm_ts_json$Internal$TypeReducer$union(
			A2(
				$elm$core$List$map,
				function (_v1) {
					var innerType = _v1.b;
					return innerType;
				},
				decoders)));
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $dillonkearns$elm_ts_json$TsJson$Decode$literal = F2(
	function (value_, literalValue) {
		return A2(
			$dillonkearns$elm_ts_json$TsJson$Decode$Decoder,
			A2(
				$elm$json$Json$Decode$andThen,
				function (decodeValue) {
					return _Utils_eq(literalValue, decodeValue) ? $elm$json$Json$Decode$succeed(value_) : $elm$json$Json$Decode$fail(
						'Expected the following literal value: ' + A2($elm$json$Json$Encode$encode, 0, literalValue));
				},
				$elm$json$Json$Decode$value),
			$dillonkearns$elm_ts_json$Internal$TsJsonType$Literal(literalValue));
	});
var $author$project$InteropDefinitions$toElmVariant = F3(
	function (tagName, constructor, decoder_) {
		return A2(
			$dillonkearns$elm_ts_json$TsJson$Decode$andMap,
			A2($dillonkearns$elm_ts_json$TsJson$Decode$field, 'data', decoder_),
			A2(
				$dillonkearns$elm_ts_json$TsJson$Decode$field,
				'tag',
				A2(
					$dillonkearns$elm_ts_json$TsJson$Decode$literal,
					constructor,
					$elm$json$Json$Encode$string(tagName))));
	});
var $author$project$InteropDefinitions$toElm = $dillonkearns$elm_ts_json$TsJson$Decode$oneOf(
	_List_fromArray(
		[
			A3($author$project$InteropDefinitions$toElmVariant, 'fileAdded', $author$project$InteropDefinitions$FileAdded, $author$project$MarkdownFile$decoder),
			A3($author$project$InteropDefinitions$toElmVariant, 'fileDeleted', $author$project$InteropDefinitions$FileDeleted, $dillonkearns$elm_ts_json$TsJson$Decode$string),
			A3($author$project$InteropDefinitions$toElmVariant, 'fileUpdated', $author$project$InteropDefinitions$FileUpdated, $author$project$MarkdownFile$decoder)
		]));
var $author$project$InteropDefinitions$interop = {flags: $author$project$InteropDefinitions$flags, fromElm: $author$project$InteropDefinitions$fromElm, toElm: $author$project$InteropDefinitions$toElm};
var $author$project$InteropPorts$decodeFlags = function (flags) {
	return A2(
		$elm$json$Json$Decode$decodeValue,
		$dillonkearns$elm_ts_json$TsJson$Decode$decoder($author$project$InteropDefinitions$interop.flags),
		flags);
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$here = _Time_here(_Utils_Tuple0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$core$Debug$toString = _Debug_toString;
var $elm$core$Debug$todo = _Debug_todo;
var $author$project$Worker$Worker$init = function (flags) {
	var _v0 = $author$project$InteropPorts$decodeFlags(flags);
	if (_v0.$ === 'Err') {
		var error = _v0.a;
		return _Debug_todo(
			'Worker.Worker',
			{
				start: {line: 49, column: 13},
				end: {line: 49, column: 23}
			})(
			$elm$core$Debug$toString(error));
	} else {
		var okFlags = _v0.a;
		return _Utils_Tuple2(
			{
				dailyNotesFolder: okFlags.folder,
				dailyNotesFormat: okFlags.format,
				taskList: $author$project$Worker$Worker$Loading,
				timeWithZone: {
					now: $elm$time$Time$millisToPosix(okFlags.now),
					zone: A2($elm$time$Time$customZone, okFlags.zone, _List_Nil)
				}
			},
			A2(
				$elm$core$Task$perform,
				$author$project$Worker$Worker$ReceiveTime,
				A3($elm$core$Task$map2, $elm$core$Tuple$pair, $elm$time$Time$here, $elm$time$Time$now)));
	}
};
var $author$project$Worker$Worker$BadInputFromTypeScript = {$: 'BadInputFromTypeScript'};
var $author$project$Worker$Worker$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $author$project$Worker$Worker$VaultFileAdded = function (a) {
	return {$: 'VaultFileAdded', a: a};
};
var $author$project$Worker$Worker$VaultFileDeleted = function (a) {
	return {$: 'VaultFileDeleted', a: a};
};
var $author$project$Worker$Worker$VaultFileUpdated = function (a) {
	return {$: 'VaultFileUpdated', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$core$Platform$Sub$map = _Platform_map;
var $author$project$InteropPorts$interopToElm = _Platform_incomingPort('interopToElm', $elm$json$Json$Decode$value);
var $author$project$InteropPorts$toElm = $author$project$InteropPorts$interopToElm(
	$elm$json$Json$Decode$decodeValue(
		$dillonkearns$elm_ts_json$TsJson$Decode$decoder($author$project$InteropDefinitions$interop.toElm)));
var $author$project$Worker$Worker$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2($elm$time$Time$every, 1000, $author$project$Worker$Worker$Tick),
				A2(
				$elm$core$Platform$Sub$map,
				function (result) {
					if (result.$ === 'Ok') {
						var toElm = result.a;
						switch (toElm.$) {
							case 'FileAdded':
								var markdownFile = toElm.a;
								return $author$project$Worker$Worker$VaultFileAdded(markdownFile);
							case 'FileDeleted':
								var filePath = toElm.a;
								return $author$project$Worker$Worker$VaultFileDeleted(filePath);
							default:
								var markdownFile = toElm.a;
								return $author$project$Worker$Worker$VaultFileUpdated(markdownFile);
						}
					} else {
						var error = result.a;
						return $author$project$Worker$Worker$BadInputFromTypeScript;
					}
				},
				$author$project$InteropPorts$toElm)
			]));
};
var $author$project$Worker$Worker$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $author$project$TaskList$TaskList = function (a) {
	return {$: 'TaskList', a: a};
};
var $author$project$TaskList$append = F2(
	function (_v0, _v1) {
		var root = _v0.a;
		var toAppend = _v1.a;
		return $author$project$TaskList$TaskList(
			_Utils_ap(root, toAppend));
	});
var $author$project$Worker$Worker$addTaskItems = F2(
	function (model, taskList) {
		var _v0 = model.taskList;
		if (_v0.$ === 'Loading') {
			return _Utils_update(
				model,
				{
					taskList: $author$project$Worker$Worker$Loaded(taskList)
				});
		} else {
			var currentList = _v0.a;
			return _Utils_update(
				model,
				{
					taskList: $author$project$Worker$Worker$Loaded(
						A2($author$project$TaskList$append, currentList, taskList))
				});
		}
	});
var $author$project$TaskItem$isFromFile = F2(
	function (pathToFile, _v0) {
		var fields = _v0.a;
		return _Utils_eq(fields.filePath, pathToFile);
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$TaskList$itemsNotFromFile = F2(
	function (pathToFile, taskItems) {
		return A2(
			$elm$core$List$filter,
			function (t) {
				return !A2($author$project$TaskItem$isFromFile, pathToFile, t);
			},
			taskItems);
	});
var $author$project$TaskList$removeForFile = F2(
	function (filePath, _v0) {
		var taskItems = _v0.a;
		return $author$project$TaskList$TaskList(
			A2($author$project$TaskList$itemsNotFromFile, filePath, taskItems));
	});
var $author$project$Worker$Worker$deleteItemsFromFile = F2(
	function (model, filePath) {
		var _v0 = model.taskList;
		if (_v0.$ === 'Loading') {
			return model;
		} else {
			var currentList = _v0.a;
			return _Utils_update(
				model,
				{
					taskList: $author$project$Worker$Worker$Loaded(
						A2($author$project$TaskList$removeForFile, filePath, currentList))
				});
		}
	});
var $author$project$TaskList$empty = $author$project$TaskList$TaskList(_List_Nil);
var $elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0.a;
			var _v1 = parse(s0);
			if (_v1.$ === 'Good') {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (step.$ === 'Loop') {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
			});
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (step.$ === 'Loop') {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $elm$parser$Parser$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					var _v2 = callback(a);
					var parseB = _v2.a;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var p = _v2.a;
					var x = _v2.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v2.a;
					var a = _v2.b;
					var s1 = _v2.c;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3(
							$elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$core$Char$fromCode = _Char_fromCode;
var $author$project$ParserHelper$carriageReturn = $elm$core$Char$fromCode(13);
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $author$project$ParserHelper$lineEnd = $elm$parser$Parser$chompWhile(
	function (c) {
		return _Utils_eq(
			c,
			_Utils_chr('\n')) || _Utils_eq(c, $author$project$ParserHelper$carriageReturn);
	});
var $elm$core$String$length = _String_length;
var $elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var $elm$parser$Parser$Advanced$problem = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $author$project$ParserHelper$checkIfEmpty = F2(
	function (calledFrom, parsedString) {
		return (!$elm$core$String$length(parsedString)) ? $elm$parser$Parser$problem('Empty string found in ' + calledFrom) : $elm$parser$Parser$succeed(parsedString);
	});
var $elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $author$project$ParserHelper$isLineEnd = function (_char) {
	var _v0 = $elm$core$Char$toCode(_char);
	switch (_v0) {
		case 10:
			return true;
		case 13:
			return true;
		default:
			return false;
	}
};
var $author$project$ParserHelper$chompWithEndOfLine = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(_Utils_Tuple0),
		$elm$parser$Parser$chompWhile(
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$ParserHelper$isLineEnd))),
	$elm$parser$Parser$chompIf($author$project$ParserHelper$isLineEnd));
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $author$project$ParserHelper$nonEmptyLineParser = A2(
	$elm$parser$Parser$andThen,
	$author$project$ParserHelper$checkIfEmpty('nonEmptyLineParser'),
	$elm$parser$Parser$getChompedString($author$project$ParserHelper$chompWithEndOfLine));
var $author$project$ParserHelper$anyLineParser = function () {
	var removeTrailingEol = function (parsedLine) {
		return $elm$parser$Parser$succeed(
			A2($elm$core$String$dropRight, 1, parsedLine));
	};
	return A2(
		$elm$parser$Parser$andThen,
		removeTrailingEol,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Basics$identity),
			A2($elm$parser$Parser$ignorer, $author$project$ParserHelper$nonEmptyLineParser, $author$project$ParserHelper$lineEnd)));
}();
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0.a;
	return $elm$parser$Parser$Advanced$Parser(
		function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 'Bad') {
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, false, x);
			} else {
				var a = _v1.b;
				var s1 = _v1.c;
				return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
			}
		});
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $author$project$TaskItem$TaskItem = F2(
	function (a, b) {
		return {$: 'TaskItem', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$getCol = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.col, s);
	});
var $elm$parser$Parser$getCol = $elm$parser$Parser$Advanced$getCol;
var $elm$parser$Parser$Advanced$getIndent = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.indent, s);
	});
var $elm$parser$Parser$getIndent = $elm$parser$Parser$Advanced$getIndent;
var $author$project$ParserHelper$isSpaceOrTab = function (_char) {
	switch (_char.valueOf()) {
		case ' ':
			return true;
		case '\t':
			return true;
		default:
			return false;
	}
};
var $author$project$ParserHelper$spaces = $elm$parser$Parser$chompWhile($author$project$ParserHelper$isSpaceOrTab);
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $elm$parser$Parser$Advanced$end = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				$elm$core$String$length(s.src),
				s.offset) ? A3($elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $author$project$ParserHelper$isSpaceTabOrLineEnd = function (_char) {
	return $author$project$ParserHelper$isSpaceOrTab(_char) || $author$project$ParserHelper$isLineEnd(_char);
};
var $author$project$ParserHelper$spacesOrLineEnd = $elm$parser$Parser$chompWhile($author$project$ParserHelper$isSpaceTabOrLineEnd);
var $author$project$ParserHelper$indented = function (next) {
	var proceed = function (_v1) {
		var minimal = _v1.a;
		var actual = _v1.b;
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$andThen,
					function (_v0) {
						return next.ending;
					},
					$elm$parser$Parser$end),
					_Utils_eq(actual, minimal) ? next.exactly : ((_Utils_cmp(actual, minimal) > 0) ? next.larger : next.smaller)
				]));
	};
	return A2(
		$elm$parser$Parser$andThen,
		proceed,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F2(
						function (a, b) {
							return _Utils_Tuple2(a, b);
						})),
				A2($elm$parser$Parser$ignorer, $elm$parser$Parser$getIndent, $author$project$ParserHelper$spacesOrLineEnd)),
			$elm$parser$Parser$getCol));
};
var $author$project$ParserHelper$step = F2(
	function (parser_, values) {
		var next = function (value_) {
			return $elm$parser$Parser$Loop(
				A2($elm$core$List$cons, value_, values));
		};
		var finish = $elm$parser$Parser$Done(
			$elm$core$List$reverse(values));
		return $author$project$ParserHelper$indented(
			{
				ending: $elm$parser$Parser$succeed(finish),
				exactly: $elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(next),
							parser_),
							$elm$parser$Parser$succeed(finish)
						])),
				larger: $elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(next),
							parser_),
							$elm$parser$Parser$succeed(finish)
						])),
				smaller: $elm$parser$Parser$succeed(finish)
			});
	});
var $elm$parser$Parser$Advanced$changeIndent = F2(
	function (newIndent, s) {
		return {col: s.col, context: s.context, indent: newIndent, offset: s.offset, row: s.row, src: s.src};
	});
var $elm$parser$Parser$Advanced$withIndent = F2(
	function (newIndent, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(
					A2($elm$parser$Parser$Advanced$changeIndent, newIndent, s0));
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						a,
						A2($elm$parser$Parser$Advanced$changeIndent, s0.indent, s1));
				} else {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var $elm$parser$Parser$withIndent = $elm$parser$Parser$Advanced$withIndent;
var $author$project$ParserHelper$indentParser = function (parser) {
	var parser_ = A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($elm$core$Basics$identity),
		A2(
			$elm$parser$Parser$loop,
			_List_Nil,
			$author$project$ParserHelper$step(parser)));
	var list_ = function (_v0) {
		var indent = _v0.a;
		var column = _v0.b;
		return (_Utils_cmp(column, indent) > 0) ? A2($elm$parser$Parser$withIndent, indent + 1, parser_) : $elm$parser$Parser$succeed(_List_Nil);
	};
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$andThen,
				list_,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed(
								F2(
									function (i, c) {
										return _Utils_Tuple2(i, c);
									})),
							$author$project$ParserHelper$spaces),
						$elm$parser$Parser$getIndent),
					$elm$parser$Parser$getCol)),
				$elm$parser$Parser$succeed(_List_Nil)
			]));
};
var $author$project$TaskItem$Note = function (a) {
	return {$: 'Note', a: a};
};
var $author$project$TaskItem$notesParser = A2(
	$elm$parser$Parser$keeper,
	$elm$parser$Parser$succeed($author$project$TaskItem$Note),
	$author$project$ParserHelper$anyLineParser);
var $author$project$TaskItem$Subtask = function (a) {
	return {$: 'Subtask', a: a};
};
var $author$project$TaskItem$AutoCompleteTag = function (a) {
	return {$: 'AutoCompleteTag', a: a};
};
var $author$project$TaskItem$CompletedTag = function (a) {
	return {$: 'CompletedTag', a: a};
};
var $author$project$TaskItem$DueTag = function (a) {
	return {$: 'DueTag', a: a};
};
var $author$project$TaskItem$Word = function (a) {
	return {$: 'Word', a: a};
};
var $elm$parser$Parser$ExpectingKeyword = function (a) {
	return {$: 'ExpectingKeyword', a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$keyword = function (_v0) {
	var kwd = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(kwd);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, kwd, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return (_Utils_eq(newOffset, -1) || (0 <= A3(
				$elm$parser$Parser$Advanced$isSubChar,
				function (c) {
					return $elm$core$Char$isAlphaNum(c) || _Utils_eq(
						c,
						_Utils_chr('_'));
				},
				newOffset,
				s.src))) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$keyword = function (kwd) {
	return $elm$parser$Parser$Advanced$keyword(
		A2(
			$elm$parser$Parser$Advanced$Token,
			kwd,
			$elm$parser$Parser$ExpectingKeyword(kwd)));
};
var $author$project$ParserHelper$booleanParser = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(true),
			$elm$parser$Parser$keyword('true')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(false),
			$elm$parser$Parser$keyword('false'))
		]));
var $author$project$ParserHelper$ParseResult = F2(
	function (a, b) {
		return {$: 'ParseResult', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$commit = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, true, a, s);
		});
};
var $elm$parser$Parser$commit = $elm$parser$Parser$Advanced$commit;
var $author$project$ParserHelper$checkEnding = function (_v0) {
	var p = _v0.a;
	var isBadEnding = _v0.b;
	return isBadEnding ? $elm$parser$Parser$problem('expecting whitespace after the parsed token') : $elm$parser$Parser$commit(p);
};
var $author$project$ParserHelper$isNotWhitespace = function (_char) {
	return !$author$project$ParserHelper$isSpaceTabOrLineEnd(_char);
};
var $author$project$ParserHelper$checkWhitespaceFollows = function (xp) {
	return A2(
		$elm$parser$Parser$andThen,
		$author$project$ParserHelper$checkEnding,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($author$project$ParserHelper$ParseResult),
				$elm$parser$Parser$backtrackable(xp)),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$map,
						function (_v0) {
							return true;
						},
						$elm$parser$Parser$backtrackable(
							$elm$parser$Parser$chompIf($author$project$ParserHelper$isNotWhitespace))),
						$elm$parser$Parser$succeed(false)
					]))));
};
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 'Expecting', a: a};
};
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$token = function (str) {
	return $elm$parser$Parser$Advanced$token(
		$elm$parser$Parser$toToken(str));
};
var $author$project$TaskPaperTag$tagParser = F3(
	function (tagKeyword, valueParser, tagger) {
		return A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(tagger),
				$elm$parser$Parser$token('@' + (tagKeyword + '('))),
			A2(
				$elm$parser$Parser$ignorer,
				valueParser,
				$elm$parser$Parser$token(')')));
	});
var $author$project$TaskPaperTag$parser = F3(
	function (tagKeyword, valueParser, tagger) {
		return $author$project$ParserHelper$checkWhitespaceFollows(
			A3($author$project$TaskPaperTag$tagParser, tagKeyword, valueParser, tagger));
	});
var $author$project$TaskPaperTag$autocompleteTagParser = A2($author$project$TaskPaperTag$parser, 'autocomplete', $author$project$ParserHelper$booleanParser);
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$Basics$round = _Basics_round;
var $elm$core$String$toFloat = _String_toFloat;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs = A2(
	$elm$parser$Parser$andThen,
	function (str) {
		if ($elm$core$String$length(str) <= 9) {
			var _v0 = $elm$core$String$toFloat('0.' + str);
			if (_v0.$ === 'Just') {
				var floatVal = _v0.a;
				return $elm$parser$Parser$succeed(
					$elm$core$Basics$round(floatVal * 1000));
			} else {
				return $elm$parser$Parser$problem('Invalid float: \"' + (str + '\"'));
			}
		} else {
			return $elm$parser$Parser$problem(
				'Expected at most 9 digits, but got ' + $elm$core$String$fromInt(
					$elm$core$String$length(str)));
		}
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompWhile($elm$core$Char$isDigit)));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts = F6(
	function (monthYearDayMs, hour, minute, second, ms, utcOffsetMinutes) {
		return $elm$time$Time$millisToPosix((((monthYearDayMs + (((hour * 60) * 60) * 1000)) + (((minute - utcOffsetMinutes) * 60) * 1000)) + (second * 1000)) + ms);
	});
var $elm$core$String$append = _String_append;
var $elm$core$String$toInt = _String_toInt;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt = function (quantity) {
	var helper = function (str) {
		if (_Utils_eq(
			$elm$core$String$length(str),
			quantity)) {
			var _v0 = $elm$core$String$toInt(str);
			if (_v0.$ === 'Just') {
				var intVal = _v0.a;
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$Done,
					$elm$parser$Parser$succeed(intVal));
			} else {
				return $elm$parser$Parser$problem('Invalid integer: \"' + (str + '\"'));
			}
		} else {
			return A2(
				$elm$parser$Parser$map,
				function (nextChar) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$String$append, str, nextChar));
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
		}
	};
	return A2($elm$parser$Parser$loop, '', helper);
};
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear = 1970;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay = function (day) {
	return $elm$parser$Parser$problem(
		'Invalid day: ' + $elm$core$String$fromInt(day));
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear = function (year) {
	return (!A2($elm$core$Basics$modBy, 4, year)) && ((!(!A2($elm$core$Basics$modBy, 100, year))) || (!A2($elm$core$Basics$modBy, 400, year)));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore = function (y1) {
	var y = y1 - 1;
	return (((y / 4) | 0) - ((y / 100) | 0)) + ((y / 400) | 0);
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay = 86400000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear = 31536000000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay = function (_v0) {
	var year = _v0.a;
	var month = _v0.b;
	var dayInMonth = _v0.c;
	if (dayInMonth < 0) {
		return $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth);
	} else {
		var succeedWith = function (extraMs) {
			var yearMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear * (year - $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear);
			var days = ((month < 3) || (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year))) ? (dayInMonth - 1) : dayInMonth;
			var dayMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay * (days + ($rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore(year) - $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore($rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear)));
			return $elm$parser$Parser$succeed((extraMs + yearMs) + dayMs);
		};
		switch (month) {
			case 1:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(0);
			case 2:
				return ((dayInMonth > 29) || ((dayInMonth === 29) && (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year)))) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(2678400000);
			case 3:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(5097600000);
			case 4:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(7776000000);
			case 5:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(10368000000);
			case 6:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(13046400000);
			case 7:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(15638400000);
			case 8:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(18316800000);
			case 9:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(20995200000);
			case 10:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(23587200000);
			case 11:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(26265600000);
			case 12:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(28857600000);
			default:
				return $elm$parser$Parser$problem(
					'Invalid month: \"' + ($elm$core$String$fromInt(month) + '\"'));
		}
	}
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs = A2(
	$elm$parser$Parser$andThen,
	$rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F3(
						function (year, month, day) {
							return _Utils_Tuple3(year, month, day);
						})),
				$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(4)),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed($elm$core$Basics$identity),
							$elm$parser$Parser$symbol('-')),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
					]))),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($elm$core$Basics$identity),
						$elm$parser$Parser$symbol('-')),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
				]))));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes = function () {
	var utcOffsetMinutesFromParts = F3(
		function (multiplier, hours, minutes) {
			return (multiplier * (hours * 60)) + minutes;
		});
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return 0;
					},
					$elm$parser$Parser$symbol('Z')),
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(utcOffsetMinutesFromParts),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$map,
										function (_v1) {
											return 1;
										},
										$elm$parser$Parser$symbol('+')),
										A2(
										$elm$parser$Parser$map,
										function (_v2) {
											return -1;
										},
										$elm$parser$Parser$symbol('-'))
									]))),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									$elm$parser$Parser$succeed($elm$core$Basics$identity),
									$elm$parser$Parser$symbol(':')),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
								$elm$parser$Parser$succeed(0)
							]))),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(0),
					$elm$parser$Parser$end)
				])));
}();
var $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601 = A2(
	$elm$parser$Parser$andThen,
	function (datePart) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed(
											$rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts(datePart)),
										$elm$parser$Parser$symbol('T')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$elm$parser$Parser$oneOf(
									_List_fromArray(
										[
											A2(
											$elm$parser$Parser$keeper,
											A2(
												$elm$parser$Parser$ignorer,
												$elm$parser$Parser$succeed($elm$core$Basics$identity),
												$elm$parser$Parser$symbol(':')),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
										]))),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$keeper,
										A2(
											$elm$parser$Parser$ignorer,
											$elm$parser$Parser$succeed($elm$core$Basics$identity),
											$elm$parser$Parser$symbol(':')),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
										$elm$parser$Parser$succeed(0)
									]))),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$symbol('.')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs),
									$elm$parser$Parser$succeed(0)
								]))),
					A2($elm$parser$Parser$ignorer, $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes, $elm$parser$Parser$end)),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						A6($rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts, datePart, 0, 0, 0, 0, 0)),
					$elm$parser$Parser$end)
				]));
	},
	$rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs);
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (_v0.$ === 'Ok') {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$toTime = function (str) {
	return A2($elm$parser$Parser$run, $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601, str);
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$ParserHelper$timeParser = function () {
	var convertToTime = function (timeString) {
		return A2(
			$elm$core$Result$withDefault,
			$elm$parser$Parser$problem('not a valid date'),
			A2(
				$elm$core$Result$map,
				$elm$parser$Parser$succeed,
				$rtfeldman$elm_iso8601_date_strings$Iso8601$toTime(timeString)));
	};
	return A2(
		$elm$parser$Parser$andThen,
		convertToTime,
		$elm$parser$Parser$getChompedString(
			$elm$parser$Parser$chompWhile(
				function (c) {
					return $elm$core$Char$isDigit(c) || (_Utils_eq(
						c,
						_Utils_chr('-')) || (_Utils_eq(
						c,
						_Utils_chr('T')) || _Utils_eq(
						c,
						_Utils_chr(':'))));
				})));
}();
var $author$project$TaskPaperTag$completedTagParser = A2($author$project$TaskPaperTag$parser, 'completed', $author$project$ParserHelper$timeParser);
var $justinmimbs$date$Date$deadEndToString = function (_v0) {
	var problem = _v0.problem;
	if (problem.$ === 'Problem') {
		var message = problem.a;
		return message;
	} else {
		return 'Expected a date in ISO 8601 format';
	}
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $justinmimbs$date$Date$MonthAndDay = F2(
	function (a, b) {
		return {$: 'MonthAndDay', a: a, b: b};
	});
var $justinmimbs$date$Date$OrdinalDay = function (a) {
	return {$: 'OrdinalDay', a: a};
};
var $justinmimbs$date$Date$WeekAndWeekday = F2(
	function (a, b) {
		return {$: 'WeekAndWeekday', a: a, b: b};
	});
var $elm$parser$Parser$mapChompedString = $elm$parser$Parser$Advanced$mapChompedString;
var $justinmimbs$date$Date$int1 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	$elm$parser$Parser$chompIf($elm$core$Char$isDigit));
var $justinmimbs$date$Date$int2 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(_Utils_Tuple0),
			$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
		$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
var $justinmimbs$date$Date$int3 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(_Utils_Tuple0),
				$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
			$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
		$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
var $justinmimbs$date$Date$dayOfYear = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$token('-')),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						$elm$parser$Parser$backtrackable(
						A2(
							$elm$parser$Parser$andThen,
							$elm$parser$Parser$commit,
							A2($elm$parser$Parser$map, $justinmimbs$date$Date$OrdinalDay, $justinmimbs$date$Date$int3))),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed($justinmimbs$date$Date$MonthAndDay),
							$justinmimbs$date$Date$int2),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$token('-')),
									$justinmimbs$date$Date$int2),
									$elm$parser$Parser$succeed(1)
								]))),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed($justinmimbs$date$Date$WeekAndWeekday),
								$elm$parser$Parser$token('W')),
							$justinmimbs$date$Date$int2),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$token('-')),
									$justinmimbs$date$Date$int1),
									$elm$parser$Parser$succeed(1)
								])))
					]))),
			$elm$parser$Parser$backtrackable(
			A2(
				$elm$parser$Parser$andThen,
				$elm$parser$Parser$commit,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						$elm$parser$Parser$succeed($justinmimbs$date$Date$MonthAndDay),
						$justinmimbs$date$Date$int2),
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								$justinmimbs$date$Date$int2,
								$elm$parser$Parser$succeed(1)
							]))))),
			A2($elm$parser$Parser$map, $justinmimbs$date$Date$OrdinalDay, $justinmimbs$date$Date$int3),
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed($justinmimbs$date$Date$WeekAndWeekday),
					$elm$parser$Parser$token('W')),
				$justinmimbs$date$Date$int2),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						$justinmimbs$date$Date$int1,
						$elm$parser$Parser$succeed(1)
					]))),
			$elm$parser$Parser$succeed(
			$justinmimbs$date$Date$OrdinalDay(1))
		]));
var $justinmimbs$date$Date$RD = function (a) {
	return {$: 'RD', a: a};
};
var $justinmimbs$date$Date$isLeapYear = function (y) {
	return ((!A2($elm$core$Basics$modBy, 4, y)) && (!(!A2($elm$core$Basics$modBy, 100, y)))) || (!A2($elm$core$Basics$modBy, 400, y));
};
var $justinmimbs$date$Date$daysBeforeMonth = F2(
	function (y, m) {
		var leapDays = $justinmimbs$date$Date$isLeapYear(y) ? 1 : 0;
		switch (m.$) {
			case 'Jan':
				return 0;
			case 'Feb':
				return 31;
			case 'Mar':
				return 59 + leapDays;
			case 'Apr':
				return 90 + leapDays;
			case 'May':
				return 120 + leapDays;
			case 'Jun':
				return 151 + leapDays;
			case 'Jul':
				return 181 + leapDays;
			case 'Aug':
				return 212 + leapDays;
			case 'Sep':
				return 243 + leapDays;
			case 'Oct':
				return 273 + leapDays;
			case 'Nov':
				return 304 + leapDays;
			default:
				return 334 + leapDays;
		}
	});
var $justinmimbs$date$Date$floorDiv = F2(
	function (a, b) {
		return $elm$core$Basics$floor(a / b);
	});
var $justinmimbs$date$Date$daysBeforeYear = function (y1) {
	var y = y1 - 1;
	var leapYears = (A2($justinmimbs$date$Date$floorDiv, y, 4) - A2($justinmimbs$date$Date$floorDiv, y, 100)) + A2($justinmimbs$date$Date$floorDiv, y, 400);
	return (365 * y) + leapYears;
};
var $justinmimbs$date$Date$daysInMonth = F2(
	function (y, m) {
		switch (m.$) {
			case 'Jan':
				return 31;
			case 'Feb':
				return $justinmimbs$date$Date$isLeapYear(y) ? 29 : 28;
			case 'Mar':
				return 31;
			case 'Apr':
				return 30;
			case 'May':
				return 31;
			case 'Jun':
				return 30;
			case 'Jul':
				return 31;
			case 'Aug':
				return 31;
			case 'Sep':
				return 30;
			case 'Oct':
				return 31;
			case 'Nov':
				return 30;
			default:
				return 31;
		}
	});
var $justinmimbs$date$Date$isBetweenInt = F3(
	function (a, b, x) {
		return (_Utils_cmp(a, x) < 1) && (_Utils_cmp(x, b) < 1);
	});
var $elm$time$Time$Apr = {$: 'Apr'};
var $elm$time$Time$Aug = {$: 'Aug'};
var $elm$time$Time$Dec = {$: 'Dec'};
var $elm$time$Time$Feb = {$: 'Feb'};
var $elm$time$Time$Jan = {$: 'Jan'};
var $elm$time$Time$Jul = {$: 'Jul'};
var $elm$time$Time$Jun = {$: 'Jun'};
var $elm$time$Time$Mar = {$: 'Mar'};
var $elm$time$Time$May = {$: 'May'};
var $elm$time$Time$Nov = {$: 'Nov'};
var $elm$time$Time$Oct = {$: 'Oct'};
var $elm$time$Time$Sep = {$: 'Sep'};
var $justinmimbs$date$Date$numberToMonth = function (mn) {
	var _v0 = A2($elm$core$Basics$max, 1, mn);
	switch (_v0) {
		case 1:
			return $elm$time$Time$Jan;
		case 2:
			return $elm$time$Time$Feb;
		case 3:
			return $elm$time$Time$Mar;
		case 4:
			return $elm$time$Time$Apr;
		case 5:
			return $elm$time$Time$May;
		case 6:
			return $elm$time$Time$Jun;
		case 7:
			return $elm$time$Time$Jul;
		case 8:
			return $elm$time$Time$Aug;
		case 9:
			return $elm$time$Time$Sep;
		case 10:
			return $elm$time$Time$Oct;
		case 11:
			return $elm$time$Time$Nov;
		default:
			return $elm$time$Time$Dec;
	}
};
var $justinmimbs$date$Date$fromCalendarParts = F3(
	function (y, mn, d) {
		return (A3($justinmimbs$date$Date$isBetweenInt, 1, 12, mn) && A3(
			$justinmimbs$date$Date$isBetweenInt,
			1,
			A2(
				$justinmimbs$date$Date$daysInMonth,
				y,
				$justinmimbs$date$Date$numberToMonth(mn)),
			d)) ? $elm$core$Result$Ok(
			$justinmimbs$date$Date$RD(
				($justinmimbs$date$Date$daysBeforeYear(y) + A2(
					$justinmimbs$date$Date$daysBeforeMonth,
					y,
					$justinmimbs$date$Date$numberToMonth(mn))) + d)) : $elm$core$Result$Err(
			'Invalid calendar date (' + ($elm$core$String$fromInt(y) + (', ' + ($elm$core$String$fromInt(mn) + (', ' + ($elm$core$String$fromInt(d) + ')'))))));
	});
var $justinmimbs$date$Date$fromOrdinalParts = F2(
	function (y, od) {
		return (A3($justinmimbs$date$Date$isBetweenInt, 1, 365, od) || ((od === 366) && $justinmimbs$date$Date$isLeapYear(y))) ? $elm$core$Result$Ok(
			$justinmimbs$date$Date$RD(
				$justinmimbs$date$Date$daysBeforeYear(y) + od)) : $elm$core$Result$Err(
			'Invalid ordinal date (' + ($elm$core$String$fromInt(y) + (', ' + ($elm$core$String$fromInt(od) + ')'))));
	});
var $justinmimbs$date$Date$weekdayNumber = function (_v0) {
	var rd = _v0.a;
	var _v1 = A2($elm$core$Basics$modBy, 7, rd);
	if (!_v1) {
		return 7;
	} else {
		var n = _v1;
		return n;
	}
};
var $justinmimbs$date$Date$daysBeforeWeekYear = function (y) {
	var jan4 = $justinmimbs$date$Date$daysBeforeYear(y) + 4;
	return jan4 - $justinmimbs$date$Date$weekdayNumber(
		$justinmimbs$date$Date$RD(jan4));
};
var $justinmimbs$date$Date$firstOfYear = function (y) {
	return $justinmimbs$date$Date$RD(
		$justinmimbs$date$Date$daysBeforeYear(y) + 1);
};
var $justinmimbs$date$Date$is53WeekYear = function (y) {
	var wdnJan1 = $justinmimbs$date$Date$weekdayNumber(
		$justinmimbs$date$Date$firstOfYear(y));
	return (wdnJan1 === 4) || ((wdnJan1 === 3) && $justinmimbs$date$Date$isLeapYear(y));
};
var $justinmimbs$date$Date$fromWeekParts = F3(
	function (wy, wn, wdn) {
		return (A3($justinmimbs$date$Date$isBetweenInt, 1, 7, wdn) && (A3($justinmimbs$date$Date$isBetweenInt, 1, 52, wn) || ((wn === 53) && $justinmimbs$date$Date$is53WeekYear(wy)))) ? $elm$core$Result$Ok(
			$justinmimbs$date$Date$RD(
				($justinmimbs$date$Date$daysBeforeWeekYear(wy) + ((wn - 1) * 7)) + wdn)) : $elm$core$Result$Err(
			'Invalid week date (' + ($elm$core$String$fromInt(wy) + (', ' + ($elm$core$String$fromInt(wn) + (', ' + ($elm$core$String$fromInt(wdn) + ')'))))));
	});
var $justinmimbs$date$Date$fromYearAndDayOfYear = function (_v0) {
	var y = _v0.a;
	var doy = _v0.b;
	switch (doy.$) {
		case 'MonthAndDay':
			var mn = doy.a;
			var d = doy.b;
			return A3($justinmimbs$date$Date$fromCalendarParts, y, mn, d);
		case 'WeekAndWeekday':
			var wn = doy.a;
			var wdn = doy.b;
			return A3($justinmimbs$date$Date$fromWeekParts, y, wn, wdn);
		default:
			var od = doy.a;
			return A2($justinmimbs$date$Date$fromOrdinalParts, y, od);
	}
};
var $justinmimbs$date$Date$int4 = A2(
	$elm$parser$Parser$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(_Utils_Tuple0),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									$elm$parser$Parser$chompIf(
									function (c) {
										return _Utils_eq(
											c,
											_Utils_chr('-'));
									}),
									$elm$parser$Parser$succeed(_Utils_Tuple0)
								]))),
					$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
				$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
			$elm$parser$Parser$chompIf($elm$core$Char$isDigit)),
		$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
var $justinmimbs$date$Date$resultToParser = function (result) {
	if (result.$ === 'Ok') {
		var x = result.a;
		return $elm$parser$Parser$succeed(x);
	} else {
		var message = result.a;
		return $elm$parser$Parser$problem(message);
	}
};
var $justinmimbs$date$Date$parser = A2(
	$elm$parser$Parser$andThen,
	A2($elm$core$Basics$composeR, $justinmimbs$date$Date$fromYearAndDayOfYear, $justinmimbs$date$Date$resultToParser),
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Tuple$pair),
			$justinmimbs$date$Date$int4),
		$justinmimbs$date$Date$dayOfYear));
var $justinmimbs$date$Date$fromIsoString = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run(
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Basics$identity),
			A2(
				$elm$parser$Parser$ignorer,
				$justinmimbs$date$Date$parser,
				A2(
					$elm$parser$Parser$andThen,
					$justinmimbs$date$Date$resultToParser,
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2($elm$parser$Parser$map, $elm$core$Result$Ok, $elm$parser$Parser$end),
								A2(
								$elm$parser$Parser$map,
								$elm$core$Basics$always(
									$elm$core$Result$Err('Expected a date only, not a date and time')),
								$elm$parser$Parser$chompIf(
									$elm$core$Basics$eq(
										_Utils_chr('T')))),
								$elm$parser$Parser$succeed(
								$elm$core$Result$Err('Expected a date only'))
							])))))),
	$elm$core$Result$mapError(
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$head,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$map($justinmimbs$date$Date$deadEndToString),
				$elm$core$Maybe$withDefault('')))));
var $author$project$ParserHelper$dateParser = function () {
	var convertToDate = function (dateString) {
		return A2(
			$elm$core$Result$withDefault,
			$elm$parser$Parser$problem('not a valid date'),
			A2(
				$elm$core$Result$map,
				$elm$parser$Parser$succeed,
				$justinmimbs$date$Date$fromIsoString(dateString)));
	};
	return A2(
		$elm$parser$Parser$andThen,
		convertToDate,
		$elm$parser$Parser$getChompedString(
			$elm$parser$Parser$chompWhile(
				function (c) {
					return $elm$core$Char$isDigit(c) || _Utils_eq(
						c,
						_Utils_chr('-'));
				})));
}();
var $author$project$TaskPaperTag$dueTagParser = A2($author$project$TaskPaperTag$parser, 'due', $author$project$ParserHelper$dateParser);
var $author$project$TaskItem$ObsidianTag = function (a) {
	return {$: 'ObsidianTag', a: a};
};
var $author$project$ParserHelper$chompToEndOfWord = A2(
	$elm$parser$Parser$ignorer,
	$elm$parser$Parser$succeed(_Utils_Tuple0),
	$elm$parser$Parser$chompWhile(
		A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$ParserHelper$isSpaceTabOrLineEnd)));
var $author$project$ParserHelper$wordParser = $author$project$ParserHelper$checkWhitespaceFollows(
	A2(
		$elm$parser$Parser$andThen,
		$author$project$ParserHelper$checkIfEmpty('wordParser'),
		$elm$parser$Parser$getChompedString($author$project$ParserHelper$chompToEndOfWord)));
var $author$project$TaskItem$obsidianTagParser = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed($author$project$TaskItem$ObsidianTag),
		$elm$parser$Parser$token('#')),
	$author$project$ParserHelper$wordParser);
var $author$project$TaskItem$tokenParser = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			$elm$parser$Parser$backtrackable(
			$author$project$TaskPaperTag$completedTagParser($author$project$TaskItem$CompletedTag)),
			$elm$parser$Parser$backtrackable(
			$author$project$TaskPaperTag$dueTagParser($author$project$TaskItem$DueTag)),
			$elm$parser$Parser$backtrackable(
			$author$project$TaskPaperTag$autocompleteTagParser($author$project$TaskItem$AutoCompleteTag)),
			$elm$parser$Parser$backtrackable($author$project$TaskItem$obsidianTagParser),
			A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$TaskItem$Word),
			$author$project$ParserHelper$wordParser)
		]));
var $author$project$TaskItem$contentHelp = function (revContents) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					function (content) {
						return $elm$parser$Parser$Loop(
							A2($elm$core$List$cons, content, revContents));
					}),
				A2(
					$elm$parser$Parser$ignorer,
					$author$project$TaskItem$tokenParser,
					$elm$parser$Parser$chompWhile($author$project$ParserHelper$isSpaceOrTab))),
				A2(
				$elm$parser$Parser$map,
				function (_v0) {
					return $elm$parser$Parser$Done(
						$elm$core$List$reverse(revContents));
				},
				$elm$parser$Parser$succeed(_Utils_Tuple0))
			]));
};
var $author$project$TaskItem$contentParser = A2($elm$parser$Parser$loop, _List_Nil, $author$project$TaskItem$contentHelp);
var $elm_community$maybe_extra$Maybe$Extra$join = function (mx) {
	if (mx.$ === 'Just') {
		var x = mx.a;
		return x;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Result$toMaybe = function (result) {
	if (result.$ === 'Ok') {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$TaskItem$fileDateParser = function (fileDate) {
	return $elm$parser$Parser$succeed(
		$elm_community$maybe_extra$Maybe$Extra$join(
			A2(
				$elm$core$Maybe$map,
				$elm$core$Result$toMaybe,
				A2($elm$core$Maybe$map, $justinmimbs$date$Date$fromIsoString, fileDate))));
};
var $elm$parser$Parser$Advanced$getOffset = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.offset, s);
	});
var $elm$parser$Parser$getOffset = $elm$parser$Parser$Advanced$getOffset;
var $elm$parser$Parser$Advanced$getRow = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.row, s);
	});
var $elm$parser$Parser$getRow = $elm$parser$Parser$Advanced$getRow;
var $elm$parser$Parser$Advanced$getSource = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.src, s);
	});
var $elm$parser$Parser$getSource = $elm$parser$Parser$Advanced$getSource;
var $author$project$ParserHelper$lineEndOrEnd = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$author$project$ParserHelper$lineEnd, $elm$parser$Parser$end]));
var $author$project$TaskItem$Completed = {$: 'Completed'};
var $author$project$TaskItem$Incomplete = {$: 'Incomplete'};
var $author$project$TaskItem$prefixParser = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$TaskItem$Incomplete),
			$elm$parser$Parser$token('- [ ] ')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$TaskItem$Completed),
			$elm$parser$Parser$token('- [x] ')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$TaskItem$Completed),
			$elm$parser$Parser$token('- [X] '))
		]));
var $author$project$TaskItem$CompletedAt = function (a) {
	return {$: 'CompletedAt', a: a};
};
var $author$project$TaskItem$FalseSpecified = {$: 'FalseSpecified'};
var $author$project$TaskItem$NotSpecifed = {$: 'NotSpecifed'};
var $author$project$TaskItem$TrueSpecified = {$: 'TrueSpecified'};
var $author$project$TaskItem$isCompleted = function (_v0) {
	var fields = _v0.a;
	var _v1 = fields.completion;
	if (_v1.$ === 'Incomplete') {
		return false;
	} else {
		return true;
	}
};
var $elm$core$String$startsWith = _String_startsWith;
var $author$project$TaskItem$taskItemFieldsBuilder = F9(
	function (startOffset, startColumn, path, row, completion_, dueFromFile, contents, endOffset, source) {
		var sourceText = A3($elm$core$String$slice, startOffset - (startColumn - 1), endOffset, source);
		var parsedBlockLink = function () {
			var _v6 = $elm$core$List$reverse(contents);
			if (_v6.b && (_v6.a.$ === 'Word')) {
				var endWord = _v6.a.a;
				return A2($elm$core$String$startsWith, '^', endWord) ? $elm$core$Maybe$Just(endWord) : $elm$core$Maybe$Nothing;
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}();
		var extractWords = F2(
			function (content, words) {
				if (content.$ === 'Word') {
					var word = content.a;
					return A2($elm$core$List$cons, word, words);
				} else {
					return words;
				}
			});
		var parsedTitle = function () {
			var wordsWithoutBlockLink = function () {
				var _v4 = $elm$core$List$reverse(contents);
				if (_v4.b && (_v4.a.$ === 'Word')) {
					var endWord = _v4.a.a;
					var cs = _v4.b;
					return A2($elm$core$String$startsWith, '^', endWord) ? $elm$core$List$reverse(cs) : contents;
				} else {
					return contents;
				}
			}();
			return A2(
				$elm$core$String$join,
				' ',
				A3($elm$core$List$foldr, extractWords, _List_Nil, wordsWithoutBlockLink));
		}();
		var extractTag = F2(
			function (content, ts) {
				if (content.$ === 'ObsidianTag') {
					var t = content.a;
					return A2($elm$core$List$cons, t, ts);
				} else {
					return ts;
				}
			});
		var obsidianTags = A3($elm$core$List$foldr, extractTag, _List_Nil, contents);
		var extractDueDate = F2(
			function (content, date) {
				if (content.$ === 'DueTag') {
					var tagDate = content.a;
					return $elm$core$Maybe$Just(tagDate);
				} else {
					return date;
				}
			});
		var tagDueDate = A3($elm$core$List$foldr, extractDueDate, $elm$core$Maybe$Nothing, contents);
		var extractCompletionTime = F2(
			function (content, time) {
				if (content.$ === 'CompletedTag') {
					var completionTime = content.a;
					return $elm$core$Maybe$Just(completionTime);
				} else {
					return time;
				}
			});
		var extractAutoComplete = F2(
			function (content, autoComplete_) {
				if (content.$ === 'AutoCompleteTag') {
					if (!content.a) {
						return $author$project$TaskItem$FalseSpecified;
					} else {
						return $author$project$TaskItem$TrueSpecified;
					}
				} else {
					return autoComplete_;
				}
			});
		var autoCompletefromTag = A3($elm$core$List$foldr, extractAutoComplete, $author$project$TaskItem$NotSpecifed, contents);
		var addCompletionTime = function (fields) {
			return $author$project$TaskItem$isCompleted(
				A2($author$project$TaskItem$TaskItem, fields, _List_Nil)) ? A2(
				$elm$core$Maybe$withDefault,
				fields,
				A2(
					$elm$core$Maybe$map,
					function (completionDate_) {
						return _Utils_update(
							fields,
							{
								completion: $author$project$TaskItem$CompletedAt(completionDate_)
							});
					},
					A3($elm$core$List$foldr, extractCompletionTime, $elm$core$Maybe$Nothing, contents))) : fields;
		};
		return addCompletionTime(
			{autoComplete: autoCompletefromTag, blockLink: parsedBlockLink, completion: completion_, dueFile: dueFromFile, dueTag: tagDueDate, filePath: path, lineNumber: row, notes: '', originalText: sourceText, tags: obsidianTags, title: parsedTitle});
	});
var $author$project$TaskItem$subTaskParser = F2(
	function (pathToFile, fileDate) {
		return A2(
			$elm$parser$Parser$andThen,
			function (f) {
				return $elm$parser$Parser$succeed(
					$author$project$TaskItem$Subtask(f));
			},
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$keeper,
										A2(
											$elm$parser$Parser$keeper,
											A2(
												$elm$parser$Parser$keeper,
												$elm$parser$Parser$succeed($author$project$TaskItem$taskItemFieldsBuilder),
												$elm$parser$Parser$getOffset),
											$elm$parser$Parser$getCol),
										$elm$parser$Parser$succeed(pathToFile)),
									$elm$parser$Parser$getRow),
								A2(
									$elm$parser$Parser$ignorer,
									$author$project$TaskItem$prefixParser,
									$elm$parser$Parser$chompWhile($author$project$ParserHelper$isSpaceOrTab))),
							$author$project$TaskItem$fileDateParser(fileDate)),
						$author$project$TaskItem$contentParser),
					A2($elm$parser$Parser$ignorer, $elm$parser$Parser$getOffset, $author$project$ParserHelper$lineEndOrEnd)),
				$elm$parser$Parser$getSource));
	});
var $author$project$TaskItem$indentedItemParser = F2(
	function (pathToFile, fileDate) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2($author$project$TaskItem$subTaskParser, pathToFile, fileDate),
					$author$project$TaskItem$notesParser
				]));
	});
var $author$project$TaskItem$addAnySubtasksAndNotes = F3(
	function (pathToFile, fileDate, fields) {
		var parsedSubtasks = function (indentedItems) {
			return A2(
				$elm$core$List$filterMap,
				function (i) {
					if (i.$ === 'Subtask') {
						var taskItemFields = i.a;
						return $elm$core$Maybe$Just(taskItemFields);
					} else {
						return $elm$core$Maybe$Nothing;
					}
				},
				indentedItems);
		};
		var parsedNotes = function (indentedItems) {
			return A2(
				$elm$core$String$join,
				'\n',
				A2(
					$elm$core$List$filterMap,
					function (i) {
						if (i.$ === 'Note') {
							var notes_ = i.a;
							return $elm$core$Maybe$Just(notes_);
						} else {
							return $elm$core$Maybe$Nothing;
						}
					},
					indentedItems));
		};
		var buildTaskItem = function (indentedItems) {
			return $elm$parser$Parser$succeed(
				A2(
					$author$project$TaskItem$TaskItem,
					_Utils_update(
						fields,
						{
							notes: parsedNotes(indentedItems)
						}),
					parsedSubtasks(indentedItems)));
		};
		return A2(
			$elm$parser$Parser$andThen,
			buildTaskItem,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$author$project$ParserHelper$indentParser(
					A2($author$project$TaskItem$indentedItemParser, pathToFile, fileDate))));
	});
var $author$project$TaskItem$rejectIfNoTitle = function (fields) {
	return (!$elm$core$String$length(fields.title)) ? $elm$parser$Parser$problem('Task has no title') : $elm$parser$Parser$succeed(fields);
};
var $author$project$TaskItem$parser = F2(
	function (pathToFile, fileDate) {
		return A2(
			$elm$parser$Parser$andThen,
			A2($author$project$TaskItem$addAnySubtasksAndNotes, pathToFile, fileDate),
			A2(
				$elm$parser$Parser$andThen,
				$author$project$TaskItem$rejectIfNoTitle,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$keeper,
										A2(
											$elm$parser$Parser$keeper,
											A2(
												$elm$parser$Parser$keeper,
												A2(
													$elm$parser$Parser$keeper,
													$elm$parser$Parser$succeed($author$project$TaskItem$taskItemFieldsBuilder),
													$elm$parser$Parser$getOffset),
												$elm$parser$Parser$getCol),
											$elm$parser$Parser$succeed(pathToFile)),
										$elm$parser$Parser$getRow),
									A2(
										$elm$parser$Parser$ignorer,
										$author$project$TaskItem$prefixParser,
										$elm$parser$Parser$chompWhile($author$project$ParserHelper$isSpaceOrTab))),
								$author$project$TaskItem$fileDateParser(fileDate)),
							$author$project$TaskItem$contentParser),
						A2($elm$parser$Parser$ignorer, $elm$parser$Parser$getOffset, $author$project$ParserHelper$lineEndOrEnd)),
					$elm$parser$Parser$getSource)));
	});
var $author$project$TaskList$taskItemsHelp = F3(
	function (filePath, fileDate, revTaskItems) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$backtrackable(
					A2(
						$elm$parser$Parser$map,
						function (taskItem) {
							return $elm$parser$Parser$Loop(
								A2($elm$core$List$cons, taskItem, revTaskItems));
						},
						A2($author$project$TaskItem$parser, filePath, fileDate))),
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return $elm$parser$Parser$Loop(revTaskItems);
					},
					$author$project$ParserHelper$anyLineParser),
					A2(
					$elm$parser$Parser$map,
					function (_v1) {
						return $elm$parser$Parser$Done(
							$elm$core$List$reverse(revTaskItems));
					},
					$elm$parser$Parser$succeed(_Utils_Tuple0))
				]));
	});
var $author$project$TaskList$parser = F2(
	function (filePath, fileDate) {
		return A2(
			$elm$parser$Parser$map,
			function (ts) {
				return $author$project$TaskList$TaskList(ts);
			},
			A2(
				$elm$parser$Parser$loop,
				_List_Nil,
				A2($author$project$TaskList$taskItemsHelp, filePath, fileDate)));
	});
var $author$project$TaskList$fromMarkdown = F3(
	function (filePath, fileDate, fileContents) {
		return A2(
			$elm$core$Result$withDefault,
			$author$project$TaskList$empty,
			A2(
				$elm$parser$Parser$run,
				A2($author$project$TaskList$parser, filePath, fileDate),
				fileContents + '\n'));
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$TimeWithZone$now = F2(
	function (time, timeWithZone) {
		return _Utils_update(
			timeWithZone,
			{now: time});
	});
var $author$project$TaskList$replaceForFile = F3(
	function (filePath, updatedList, currentList) {
		return A2(
			$author$project$TaskList$append,
			updatedList,
			A2($author$project$TaskList$removeForFile, filePath, currentList));
	});
var $author$project$Worker$Worker$updateTaskItems = F3(
	function (model, filePath, updatedList) {
		var _v0 = model.taskList;
		if (_v0.$ === 'Loading') {
			return _Utils_update(
				model,
				{
					taskList: $author$project$Worker$Worker$Loaded(updatedList)
				});
		} else {
			var currentList = _v0.a;
			return _Utils_update(
				model,
				{
					taskList: $author$project$Worker$Worker$Loaded(
						A3($author$project$TaskList$replaceForFile, filePath, updatedList, currentList))
				});
		}
	});
var $author$project$Worker$Worker$update = F2(
	function (msg, model) {
		var _v0 = _Utils_Tuple2(msg, model);
		switch (_v0.a.$) {
			case 'BadInputFromTypeScript':
				var _v1 = _v0.a;
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'ReceiveTime':
				var _v2 = _v0.a.a;
				var zone = _v2.a;
				var posix = _v2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timeWithZone: {now: posix, zone: zone}
						}),
					$elm$core$Platform$Cmd$none);
			case 'Tick':
				var time = _v0.a.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timeWithZone: A2($author$project$TimeWithZone$now, time, model.timeWithZone)
						}),
					$elm$core$Platform$Cmd$none);
			case 'VaultFileAdded':
				var markdownFile = _v0.a.a;
				var newTaskItems = A3($author$project$TaskList$fromMarkdown, markdownFile.filePath, markdownFile.fileDate, markdownFile.fileContents);
				return _Utils_Tuple2(
					A2($author$project$Worker$Worker$addTaskItems, model, newTaskItems),
					$elm$core$Platform$Cmd$none);
			case 'VaultFileDeleted':
				var filePath = _v0.a.a;
				return _Utils_Tuple2(
					A2($author$project$Worker$Worker$deleteItemsFromFile, model, filePath),
					$elm$core$Platform$Cmd$none);
			default:
				var markdownFile = _v0.a.a;
				var updatedTaskItems = A3($author$project$TaskList$fromMarkdown, markdownFile.filePath, markdownFile.fileDate, markdownFile.fileContents);
				return _Utils_Tuple2(
					A3($author$project$Worker$Worker$updateTaskItems, model, markdownFile.filePath, updatedTaskItems),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Worker$Worker$main = $elm$core$Platform$worker(
	{init: $author$project$Worker$Worker$init, subscriptions: $author$project$Worker$Worker$subscriptions, update: $author$project$Worker$Worker$update});
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$browser$Browser$element = _Browser_element;
var $author$project$CardBoard$DateBoardConfig = function (a) {
	return {$: 'DateBoardConfig', a: a};
};
var $author$project$Main$Loading = {$: 'Loading'};
var $author$project$Main$ReceiveTime = function (a) {
	return {$: 'ReceiveTime', a: a};
};
var $author$project$CardBoard$TagBoardConfig = function (a) {
	return {$: 'TagBoardConfig', a: a};
};
var $author$project$SafeZipper$EmptyZipper = {$: 'EmptyZipper'};
var $author$project$SafeZipper$SafeZipper = F3(
	function (a, b, c) {
		return {$: 'SafeZipper', a: a, b: b, c: c};
	});
var $author$project$SafeZipper$fromList = function (xs) {
	if (!xs.b) {
		return $author$project$SafeZipper$EmptyZipper;
	} else {
		var y = xs.a;
		var ys = xs.b;
		return A3($author$project$SafeZipper$SafeZipper, _List_Nil, y, ys);
	}
};
var $author$project$Main$init = function (flags) {
	var boardConfigs = _List_fromArray(
		[
			$author$project$CardBoard$DateBoardConfig(
			{includeCompleted: true, includeUndated: true, title: 'By Date'}),
			$author$project$CardBoard$TagBoardConfig(
			{
				columns: _List_fromArray(
					[
						{displayTitle: 'People', tag: 'people/'},
						{displayTitle: 'Hydra', tag: 'hydra/'},
						{displayTitle: 'Finance', tag: 'finance/'},
						{displayTitle: 'ITT POC', tag: 'POC/'}
					]),
				includeCompleted: false,
				includeOthers: false,
				title: 'By Tag'
			})
		]);
	var _v0 = $author$project$InteropPorts$decodeFlags(flags);
	if (_v0.$ === 'Err') {
		var error = _v0.a;
		return _Debug_todo(
			'Main',
			{
				start: {line: 78, column: 13},
				end: {line: 78, column: 23}
			})(
			$elm$core$Debug$toString(error));
	} else {
		var okFlags = _v0.a;
		return _Utils_Tuple2(
			{
				boardConfigs: $author$project$SafeZipper$fromList(boardConfigs),
				dailyNotesFolder: okFlags.folder,
				dailyNotesFormat: okFlags.format,
				taskList: $author$project$Main$Loading,
				timeWithZone: {
					now: $elm$time$Time$millisToPosix(okFlags.now),
					zone: A2($elm$time$Time$customZone, okFlags.zone, _List_Nil)
				}
			},
			A2(
				$elm$core$Task$perform,
				$author$project$Main$ReceiveTime,
				A3($elm$core$Task$map2, $elm$core$Tuple$pair, $elm$time$Time$here, $elm$time$Time$now)));
	}
};
var $author$project$Main$BadInputFromTypeScript = {$: 'BadInputFromTypeScript'};
var $author$project$Main$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $author$project$Main$VaultFileAdded = function (a) {
	return {$: 'VaultFileAdded', a: a};
};
var $author$project$Main$VaultFileDeleted = function (a) {
	return {$: 'VaultFileDeleted', a: a};
};
var $author$project$Main$VaultFileUpdated = function (a) {
	return {$: 'VaultFileUpdated', a: a};
};
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2($elm$time$Time$every, 1000, $author$project$Main$Tick),
				A2(
				$elm$core$Platform$Sub$map,
				function (result) {
					if (result.$ === 'Ok') {
						var toElm = result.a;
						switch (toElm.$) {
							case 'FileAdded':
								var markdownFile = toElm.a;
								return $author$project$Main$VaultFileAdded(markdownFile);
							case 'FileDeleted':
								var filePath = toElm.a;
								return $author$project$Main$VaultFileDeleted(filePath);
							default:
								var markdownFile = toElm.a;
								return $author$project$Main$VaultFileUpdated(markdownFile);
						}
					} else {
						var error = result.a;
						return $author$project$Main$BadInputFromTypeScript;
					}
				},
				$author$project$InteropPorts$toElm)
			]));
};
var $author$project$TaskItem$id = function (_v0) {
	var fields = _v0.a;
	return fields.filePath + (':' + $elm$core$String$fromInt(fields.lineNumber));
};
var $author$project$Card$taskItemId = function (_v0) {
	var item = _v0.b;
	return $author$project$TaskItem$id(item);
};
var $author$project$Card$id = function (card) {
	var idPrefix = card.a;
	return _Utils_ap(
		idPrefix,
		$author$project$Card$taskItemId(card));
};
var $author$project$Card$editButtonId = function (card) {
	return $author$project$Card$id(card) + ':editButton';
};
var $dillonkearns$elm_ts_json$TsJson$Encode$encoder = F2(
	function (_v0, input) {
		var encodeFn = _v0.a;
		return encodeFn(input);
	});
var $author$project$InteropPorts$encodeVariant = F3(
	function (variantName, encoder_, arg1) {
		return A3(
			$elm$core$Basics$apR,
			$dillonkearns$elm_ts_json$TsJson$Encode$object(
				_List_fromArray(
					[
						A3(
						$dillonkearns$elm_ts_json$TsJson$Encode$required,
						'tag',
						$elm$core$Basics$identity,
						$dillonkearns$elm_ts_json$TsJson$Encode$literal(
							$elm$json$Json$Encode$string(variantName))),
						A3($dillonkearns$elm_ts_json$TsJson$Encode$required, 'data', $elm$core$Basics$identity, encoder_)
					])),
			$dillonkearns$elm_ts_json$TsJson$Encode$encoder,
			arg1);
	});
var $author$project$InteropPorts$interopFromElm = _Platform_outgoingPort('interopFromElm', $elm$core$Basics$identity);
var $author$project$InteropPorts$addHoverToCardEditButtons = F2(
	function (filePath, cards) {
		return $author$project$InteropPorts$interopFromElm(
			A3(
				$author$project$InteropPorts$encodeVariant,
				'addFilePreviewHovers',
				$author$project$InteropDefinitions$addFilePreviewHoversEncoder,
				{
					filePath: filePath,
					ids: A2($elm$core$List$map, $author$project$Card$editButtonId, cards)
				}));
	});
var $author$project$Main$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $author$project$Main$addTaskItems = F2(
	function (model, taskList) {
		var _v0 = model.taskList;
		if (_v0.$ === 'Loading') {
			return _Utils_update(
				model,
				{
					taskList: $author$project$Main$Loaded(taskList)
				});
		} else {
			var currentList = _v0.a;
			return _Utils_update(
				model,
				{
					taskList: $author$project$Main$Loaded(
						A2($author$project$TaskList$append, currentList, taskList))
				});
		}
	});
var $author$project$SafeZipper$first = function (zipper) {
	if (zipper.$ === 'EmptyZipper') {
		return zipper;
	} else {
		var ls = zipper.a;
		var x = zipper.b;
		var rs = zipper.c;
		var _v1 = $elm$core$List$reverse(ls);
		if (!_v1.b) {
			return zipper;
		} else {
			var y = _v1.a;
			var ys = _v1.b;
			return A3(
				$author$project$SafeZipper$SafeZipper,
				_List_Nil,
				y,
				_Utils_ap(
					ys,
					_Utils_ap(
						_List_fromArray(
							[x]),
						rs)));
		}
	}
};
var $author$project$SafeZipper$next = function (zipper) {
	if (zipper.$ === 'EmptyZipper') {
		return zipper;
	} else {
		var ls = zipper.a;
		var x = zipper.b;
		var rs = zipper.c;
		if (!rs.b) {
			return zipper;
		} else {
			var y = rs.a;
			var ys = rs.b;
			return A3(
				$author$project$SafeZipper$SafeZipper,
				A2($elm$core$List$cons, x, ls),
				y,
				ys);
		}
	}
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $author$project$SafeZipper$atIndex = F2(
	function (index, zipper) {
		if (zipper.$ === 'EmptyZipper') {
			return zipper;
		} else {
			var ls = zipper.a;
			var x = zipper.b;
			var rs = zipper.c;
			var moveAlong = F2(
				function (_v1, z) {
					return $author$project$SafeZipper$next(z);
				});
			return A3(
				$elm$core$List$foldl,
				moveAlong,
				$author$project$SafeZipper$first(zipper),
				A2($elm$core$List$repeat, index, 0));
		}
	});
var $author$project$TaskItem$blockLink = function (_v0) {
	var fields = _v0.a;
	return fields.blockLink;
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $author$project$TaskItem$completion = function (_v0) {
	var fields = _v0.a;
	return fields.completion;
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $author$project$TaskItem$completedPosix = function (taskItem) {
	var _v0 = $author$project$TaskItem$completion(taskItem);
	if (_v0.$ === 'CompletedAt') {
		var time_ = _v0.a;
		return $elm$time$Time$posixToMillis(time_);
	} else {
		return 0;
	}
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$TaskItem$title = function (_v0) {
	var fields = _v0.a;
	return fields.title;
};
var $elm$core$String$toLower = _String_toLower;
var $author$project$TaskList$topLevelTasks = function (_v0) {
	var taskList = _v0.a;
	return taskList;
};
var $author$project$DateBoard$appendCompleted = F3(
	function (taskList, config, columnList) {
		var completedTasks = $elm$core$List$reverse(
			A2(
				$elm$core$List$sortBy,
				$author$project$TaskItem$completedPosix,
				$elm$core$List$reverse(
					A2(
						$elm$core$List$sortBy,
						A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
						A2(
							$elm$core$List$filter,
							$author$project$TaskItem$isCompleted,
							$author$project$TaskList$topLevelTasks(taskList))))));
		return config.includeCompleted ? A2(
			$elm$core$List$append,
			columnList,
			_List_fromArray(
				[
					_Utils_Tuple2('Completed', completedTasks)
				])) : columnList;
	});
var $justinmimbs$date$Date$Days = {$: 'Days'};
var $justinmimbs$date$Date$Months = {$: 'Months'};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $justinmimbs$date$Date$monthToNumber = function (m) {
	switch (m.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var $justinmimbs$date$Date$toCalendarDateHelp = F3(
	function (y, m, d) {
		toCalendarDateHelp:
		while (true) {
			var monthDays = A2($justinmimbs$date$Date$daysInMonth, y, m);
			var mn = $justinmimbs$date$Date$monthToNumber(m);
			if ((mn < 12) && (_Utils_cmp(d, monthDays) > 0)) {
				var $temp$y = y,
					$temp$m = $justinmimbs$date$Date$numberToMonth(mn + 1),
					$temp$d = d - monthDays;
				y = $temp$y;
				m = $temp$m;
				d = $temp$d;
				continue toCalendarDateHelp;
			} else {
				return {day: d, month: m, year: y};
			}
		}
	});
var $justinmimbs$date$Date$divWithRemainder = F2(
	function (a, b) {
		return _Utils_Tuple2(
			A2($justinmimbs$date$Date$floorDiv, a, b),
			A2($elm$core$Basics$modBy, b, a));
	});
var $justinmimbs$date$Date$year = function (_v0) {
	var rd = _v0.a;
	var _v1 = A2($justinmimbs$date$Date$divWithRemainder, rd, 146097);
	var n400 = _v1.a;
	var r400 = _v1.b;
	var _v2 = A2($justinmimbs$date$Date$divWithRemainder, r400, 36524);
	var n100 = _v2.a;
	var r100 = _v2.b;
	var _v3 = A2($justinmimbs$date$Date$divWithRemainder, r100, 1461);
	var n4 = _v3.a;
	var r4 = _v3.b;
	var _v4 = A2($justinmimbs$date$Date$divWithRemainder, r4, 365);
	var n1 = _v4.a;
	var r1 = _v4.b;
	var n = (!r1) ? 0 : 1;
	return ((((n400 * 400) + (n100 * 100)) + (n4 * 4)) + n1) + n;
};
var $justinmimbs$date$Date$toOrdinalDate = function (_v0) {
	var rd = _v0.a;
	var y = $justinmimbs$date$Date$year(
		$justinmimbs$date$Date$RD(rd));
	return {
		ordinalDay: rd - $justinmimbs$date$Date$daysBeforeYear(y),
		year: y
	};
};
var $justinmimbs$date$Date$toCalendarDate = function (_v0) {
	var rd = _v0.a;
	var date = $justinmimbs$date$Date$toOrdinalDate(
		$justinmimbs$date$Date$RD(rd));
	return A3($justinmimbs$date$Date$toCalendarDateHelp, date.year, $elm$time$Time$Jan, date.ordinalDay);
};
var $justinmimbs$date$Date$add = F3(
	function (unit, n, _v0) {
		var rd = _v0.a;
		switch (unit.$) {
			case 'Years':
				return A3(
					$justinmimbs$date$Date$add,
					$justinmimbs$date$Date$Months,
					12 * n,
					$justinmimbs$date$Date$RD(rd));
			case 'Months':
				var date = $justinmimbs$date$Date$toCalendarDate(
					$justinmimbs$date$Date$RD(rd));
				var wholeMonths = ((12 * (date.year - 1)) + ($justinmimbs$date$Date$monthToNumber(date.month) - 1)) + n;
				var m = $justinmimbs$date$Date$numberToMonth(
					A2($elm$core$Basics$modBy, 12, wholeMonths) + 1);
				var y = A2($justinmimbs$date$Date$floorDiv, wholeMonths, 12) + 1;
				return $justinmimbs$date$Date$RD(
					($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + A2(
						$elm$core$Basics$min,
						date.day,
						A2($justinmimbs$date$Date$daysInMonth, y, m)));
			case 'Weeks':
				return $justinmimbs$date$Date$RD(rd + (7 * n));
			default:
				return $justinmimbs$date$Date$RD(rd + n);
		}
	});
var $justinmimbs$date$Date$toMonths = function (rd) {
	var date = $justinmimbs$date$Date$toCalendarDate(
		$justinmimbs$date$Date$RD(rd));
	var wholeMonths = (12 * (date.year - 1)) + ($justinmimbs$date$Date$monthToNumber(date.month) - 1);
	return wholeMonths + (date.day / 100);
};
var $elm$core$Basics$truncate = _Basics_truncate;
var $justinmimbs$date$Date$diff = F3(
	function (unit, _v0, _v1) {
		var rd1 = _v0.a;
		var rd2 = _v1.a;
		switch (unit.$) {
			case 'Years':
				return ((($justinmimbs$date$Date$toMonths(rd2) - $justinmimbs$date$Date$toMonths(rd1)) | 0) / 12) | 0;
			case 'Months':
				return ($justinmimbs$date$Date$toMonths(rd2) - $justinmimbs$date$Date$toMonths(rd1)) | 0;
			case 'Weeks':
				return ((rd2 - rd1) / 7) | 0;
			default:
				return rd2 - rd1;
		}
	});
var $author$project$TaskItem$due = function (_v0) {
	var fields = _v0.a;
	var _v1 = fields.dueTag;
	if (_v1.$ === 'Just') {
		return fields.dueTag;
	} else {
		return fields.dueFile;
	}
};
var $justinmimbs$date$Date$toRataDie = function (_v0) {
	var rd = _v0.a;
	return rd;
};
var $author$project$TaskItem$dueRataDie = function (taskItem) {
	var _v0 = $author$project$TaskItem$due(taskItem);
	if (_v0.$ === 'Just') {
		var dueDate = _v0.a;
		return $justinmimbs$date$Date$toRataDie(dueDate);
	} else {
		return 0;
	}
};
var $author$project$DateBoard$futureItems = F3(
	function (today, taskList, config) {
		var tomorrow = A3($justinmimbs$date$Date$add, $justinmimbs$date$Date$Days, 1, today);
		var isToday = function (t) {
			var _v0 = $author$project$TaskItem$due(t);
			if (_v0.$ === 'Nothing') {
				return false;
			} else {
				var date = _v0.a;
				return (A3($justinmimbs$date$Date$diff, $justinmimbs$date$Date$Days, tomorrow, date) > 0) ? true : false;
			}
		};
		return A2(
			$elm$core$List$sortBy,
			$author$project$TaskItem$dueRataDie,
			A2(
				$elm$core$List$sortBy,
				A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
				A2(
					$elm$core$List$filter,
					function (t) {
						return (!$author$project$TaskItem$isCompleted(t)) && isToday(t);
					},
					$author$project$TaskList$topLevelTasks(taskList))));
	});
var $elm_community$maybe_extra$Maybe$Extra$isJust = function (m) {
	if (m.$ === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var $author$project$TaskItem$isDated = function (taskItem) {
	return $elm_community$maybe_extra$Maybe$Extra$isJust(
		$author$project$TaskItem$due(taskItem));
};
var $author$project$DateBoard$prependUndated = F3(
	function (taskList, config, columnList) {
		var undatedtasks = A2(
			$elm$core$List$sortBy,
			A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
			A2(
				$elm$core$List$filter,
				function (t) {
					return (!$author$project$TaskItem$isCompleted(t)) && (!$author$project$TaskItem$isDated(t));
				},
				$author$project$TaskList$topLevelTasks(taskList)));
		return config.includeUndated ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2('Undated', undatedtasks),
			columnList) : columnList;
	});
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $justinmimbs$date$Date$fromCalendarDate = F3(
	function (y, m, d) {
		return $justinmimbs$date$Date$RD(
			($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + A3(
				$elm$core$Basics$clamp,
				1,
				A2($justinmimbs$date$Date$daysInMonth, y, m),
				d));
	});
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_v0) {
			case 1:
				return $elm$time$Time$Jan;
			case 2:
				return $elm$time$Time$Feb;
			case 3:
				return $elm$time$Time$Mar;
			case 4:
				return $elm$time$Time$Apr;
			case 5:
				return $elm$time$Time$May;
			case 6:
				return $elm$time$Time$Jun;
			case 7:
				return $elm$time$Time$Jul;
			case 8:
				return $elm$time$Time$Aug;
			case 9:
				return $elm$time$Time$Sep;
			case 10:
				return $elm$time$Time$Oct;
			case 11:
				return $elm$time$Time$Nov;
			default:
				return $elm$time$Time$Dec;
		}
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var $justinmimbs$date$Date$fromPosix = F2(
	function (zone, posix) {
		return A3(
			$justinmimbs$date$Date$fromCalendarDate,
			A2($elm$time$Time$toYear, zone, posix),
			A2($elm$time$Time$toMonth, zone, posix),
			A2($elm$time$Time$toDay, zone, posix));
	});
var $author$project$TimeWithZone$toDate = function (timeWithZone) {
	return A2($justinmimbs$date$Date$fromPosix, timeWithZone.zone, timeWithZone.now);
};
var $author$project$DateBoard$todaysItems = F3(
	function (today, taskList, config) {
		var isToday = function (t) {
			var _v0 = $author$project$TaskItem$due(t);
			if (_v0.$ === 'Nothing') {
				return false;
			} else {
				var date = _v0.a;
				return (A3($justinmimbs$date$Date$diff, $justinmimbs$date$Date$Days, today, date) <= 0) ? true : false;
			}
		};
		return A2(
			$elm$core$List$sortBy,
			$author$project$TaskItem$dueRataDie,
			A2(
				$elm$core$List$sortBy,
				A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
				A2(
					$elm$core$List$filter,
					function (t) {
						return (!$author$project$TaskItem$isCompleted(t)) && isToday(t);
					},
					$author$project$TaskList$topLevelTasks(taskList))));
	});
var $author$project$DateBoard$tomorrowsItems = F3(
	function (today, taskList, config) {
		var tomorrow = A3($justinmimbs$date$Date$add, $justinmimbs$date$Date$Days, 1, today);
		var isTomorrow = function (t) {
			var _v0 = $author$project$TaskItem$due(t);
			if (_v0.$ === 'Nothing') {
				return false;
			} else {
				var date = _v0.a;
				return (!A3($justinmimbs$date$Date$diff, $justinmimbs$date$Date$Days, tomorrow, date)) ? true : false;
			}
		};
		return A2(
			$elm$core$List$sortBy,
			A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
			A2(
				$elm$core$List$filter,
				function (t) {
					return isTomorrow(t) && (!$author$project$TaskItem$isCompleted(t));
				},
				$author$project$TaskList$topLevelTasks(taskList)));
	});
var $author$project$DateBoard$columns = F3(
	function (timeWithZone, config, taskList) {
		var datestamp = $author$project$TimeWithZone$toDate(timeWithZone);
		return A3(
			$author$project$DateBoard$appendCompleted,
			taskList,
			config,
			A3(
				$author$project$DateBoard$prependUndated,
				taskList,
				config,
				_List_fromArray(
					[
						_Utils_Tuple2(
						'Today',
						A3($author$project$DateBoard$todaysItems, datestamp, taskList, config)),
						_Utils_Tuple2(
						'Tomorrow',
						A3($author$project$DateBoard$tomorrowsItems, datestamp, taskList, config)),
						_Utils_Tuple2(
						'Future',
						A3($author$project$DateBoard$futureItems, datestamp, taskList, config))
					])));
	});
var $author$project$TaskList$filter = F2(
	function (fn, _v0) {
		var taskItems = _v0.a;
		return $author$project$TaskList$TaskList(
			A2($elm$core$List$filter, fn, taskItems));
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var $elm$regex$Regex$contains = _Regex_contains;
var $elm$core$String$endsWith = _String_endsWith;
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$never = _Regex_never;
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$TaskItem$subtasks = function (_v0) {
	var subtasks_ = _v0.b;
	return A2(
		$elm$core$List$map,
		function (s) {
			return A2($author$project$TaskItem$TaskItem, s, _List_Nil);
		},
		subtasks_);
};
var $author$project$TaskItem$tags = function (taskItem) {
	var fields = taskItem.a;
	return A2(
		$elm$core$List$append,
		fields.tags,
		A2(
			$elm$core$List$concatMap,
			function (_v0) {
				var fs = _v0.a;
				return fs.tags;
			},
			$author$project$TaskItem$subtasks(taskItem)));
};
var $author$project$TaskItem$hasTag = F2(
	function (tagToMatch, taskItem) {
		var regex = function (tagToMatch_) {
			return A2(
				$elm$core$Maybe$withDefault,
				$elm$regex$Regex$never,
				A2(
					$elm$regex$Regex$fromStringWith,
					{caseInsensitive: true, multiline: false},
					tagToMatch_ + '(?:/.*)*'));
		};
		var matches = function (itemTag) {
			return A2($elm$core$String$endsWith, '/', tagToMatch) ? A2(
				$elm$regex$Regex$contains,
				regex(
					A2($elm$core$String$dropRight, 1, tagToMatch)),
				itemTag) : _Utils_eq(
				$elm$core$String$toLower(itemTag),
				$elm$core$String$toLower(tagToMatch));
		};
		return A2(
			$elm$core$List$any,
			matches,
			$author$project$TaskItem$tags(taskItem));
	});
var $author$project$TaskItem$hasOneOfTheTags = F2(
	function (tagsToMatch, taskItem) {
		return A2(
			$elm$core$List$any,
			function (t) {
				return A2($author$project$TaskItem$hasTag, t, taskItem);
			},
			tagsToMatch);
	});
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm_community$list_extra$List$Extra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			if (!remaining.b) {
				return $elm$core$List$reverse(accumulator);
			} else {
				var first = remaining.a;
				var rest = remaining.b;
				var computedFirst = f(first);
				if (A2($elm$core$Set$member, computedFirst, existing)) {
					var $temp$f = f,
						$temp$existing = existing,
						$temp$remaining = rest,
						$temp$accumulator = accumulator;
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				} else {
					var $temp$f = f,
						$temp$existing = A2($elm$core$Set$insert, computedFirst, existing),
						$temp$remaining = rest,
						$temp$accumulator = A2($elm$core$List$cons, first, accumulator);
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$uniqueBy = F2(
	function (f, list) {
		return A4($elm_community$list_extra$List$Extra$uniqueHelp, f, $elm$core$Set$empty, list, _List_Nil);
	});
var $author$project$TagBoard$appendCompleted = F3(
	function (config, taskList, columnList) {
		var uniqueColumnTags = A2(
			$elm$core$List$map,
			function ($) {
				return $.tag;
			},
			A2(
				$elm_community$list_extra$List$Extra$uniqueBy,
				function ($) {
					return $.tag;
				},
				config.columns));
		var isCompleteWithTags = function (item) {
			return $author$project$TaskItem$isCompleted(item) && A2($author$project$TaskItem$hasOneOfTheTags, uniqueColumnTags, item);
		};
		var completedTasks = $elm$core$List$reverse(
			A2(
				$elm$core$List$sortBy,
				$author$project$TaskItem$completedPosix,
				$elm$core$List$reverse(
					A2(
						$elm$core$List$sortBy,
						A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
						$author$project$TaskList$topLevelTasks(
							A2($author$project$TaskList$filter, isCompleteWithTags, taskList))))));
		return config.includeCompleted ? A2(
			$elm$core$List$append,
			columnList,
			_List_fromArray(
				[
					_Utils_Tuple2('Completed', completedTasks)
				])) : columnList;
	});
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$TagBoard$fillColumn = F3(
	function (taskList, columnConfig, acc) {
		var isIncompleteWithTag = F2(
			function (tag, item) {
				return (!$author$project$TaskItem$isCompleted(item)) && A2($author$project$TaskItem$hasTag, tag, item);
			});
		return A2(
			$elm$core$List$append,
			acc,
			$elm$core$List$singleton(
				A2(
					$elm$core$Tuple$pair,
					columnConfig.displayTitle,
					A2(
						$elm$core$List$sortBy,
						$author$project$TaskItem$dueRataDie,
						A2(
							$elm$core$List$sortBy,
							A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
							$author$project$TaskList$topLevelTasks(
								A2(
									$author$project$TaskList$filter,
									isIncompleteWithTag(columnConfig.tag),
									taskList)))))));
	});
var $author$project$TagBoard$prependOthers = F3(
	function (config, taskList, columnList) {
		var uniqueColumnTags = A2(
			$elm$core$List$map,
			function ($) {
				return $.tag;
			},
			A2(
				$elm_community$list_extra$List$Extra$uniqueBy,
				function ($) {
					return $.tag;
				},
				config.columns));
		var isIncompleteWithoutTags = function (item) {
			return (!$author$project$TaskItem$isCompleted(item)) && (!A2($author$project$TaskItem$hasOneOfTheTags, uniqueColumnTags, item));
		};
		var cards = A2(
			$elm$core$List$sortBy,
			$author$project$TaskItem$dueRataDie,
			A2(
				$elm$core$List$sortBy,
				A2($elm$core$Basics$composeL, $elm$core$String$toLower, $author$project$TaskItem$title),
				$author$project$TaskList$topLevelTasks(
					A2($author$project$TaskList$filter, isIncompleteWithoutTags, taskList))));
		return config.includeOthers ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2('Others', cards),
			columnList) : columnList;
	});
var $author$project$TagBoard$columns = F2(
	function (config, taskList) {
		return A3(
			$author$project$TagBoard$appendCompleted,
			config,
			taskList,
			A3(
				$author$project$TagBoard$prependOthers,
				config,
				taskList,
				A3(
					$elm$core$List$foldl,
					$author$project$TagBoard$fillColumn(taskList),
					_List_Nil,
					A2(
						$elm_community$list_extra$List$Extra$uniqueBy,
						function ($) {
							return $.tag;
						},
						config.columns))));
	});
var $author$project$Card$Card = F2(
	function (a, b) {
		return {$: 'Card', a: a, b: b};
	});
var $author$project$Card$fromTaskItem = $author$project$Card$Card;
var $author$project$Panel$placeCardsInColumns = F2(
	function (panelIndex, columnList) {
		var cardIdPrefix = function (columnTitle) {
			return $elm$core$String$fromInt(panelIndex) + (':' + (columnTitle + ':'));
		};
		var placeCardsInColumn = function (_v0) {
			var columnTitle = _v0.a;
			var taskItems = _v0.b;
			return A2(
				$elm$core$Tuple$pair,
				columnTitle,
				A2(
					$elm$core$List$map,
					$author$project$Card$fromTaskItem(
						cardIdPrefix(columnTitle)),
					taskItems));
		};
		return A2($elm$core$List$map, placeCardsInColumn, columnList);
	});
var $author$project$Panel$columns = F3(
	function (timeWithZone, panelIndex, _v0) {
		var config = _v0.a;
		var taskList = _v0.b;
		if (config.$ === 'DateBoardConfig') {
			var dateBoardConfig = config.a;
			return A2(
				$author$project$Panel$placeCardsInColumns,
				panelIndex,
				A3($author$project$DateBoard$columns, timeWithZone, dateBoardConfig, taskList));
		} else {
			var tagBoardConfig = config.a;
			return A2(
				$author$project$Panel$placeCardsInColumns,
				panelIndex,
				A2($author$project$TagBoard$columns, tagBoardConfig, taskList));
		}
	});
var $author$project$SafeZipper$indexedMapSelectedAndRest = F3(
	function (selectedFn, restFn, zipper) {
		if (zipper.$ === 'EmptyZipper') {
			return $author$project$SafeZipper$EmptyZipper;
		} else {
			var b = zipper.a;
			var c = zipper.b;
			var a = zipper.c;
			var mappedBefore = A2($elm$core$List$indexedMap, restFn, b);
			var beforeLength = $elm$core$List$length(b);
			var mappedAfter = A2(
				$elm$core$List$indexedMap,
				F2(
					function (i, item) {
						return A2(restFn, (beforeLength + 1) + i, item);
					}),
				a);
			var mappedCurrent = A2(selectedFn, beforeLength, c);
			return A3($author$project$SafeZipper$SafeZipper, mappedBefore, mappedCurrent, mappedAfter);
		}
	});
var $author$project$Panel$Panel = F2(
	function (a, b) {
		return {$: 'Panel', a: a, b: b};
	});
var $author$project$Panel$init = F2(
	function (config, taskList) {
		return A2($author$project$Panel$Panel, config, taskList);
	});
var $author$project$Panels$panel = F3(
	function (taskList, _v0, config) {
		return A2($author$project$Panel$init, config, taskList);
	});
var $author$project$Panels$panels = function (_v0) {
	var configs = _v0.a;
	var taskList = _v0.b;
	return A3(
		$author$project$SafeZipper$indexedMapSelectedAndRest,
		$author$project$Panels$panel(taskList),
		$author$project$Panels$panel(taskList),
		configs);
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$SafeZipper$toList = function (zipper) {
	if (zipper.$ === 'EmptyZipper') {
		return _List_Nil;
	} else {
		var ls = zipper.a;
		var x = zipper.b;
		var rs = zipper.c;
		return A2(
			$elm$core$List$append,
			ls,
			A2($elm$core$List$cons, x, rs));
	}
};
var $author$project$Panels$cards = F2(
	function (timeWithZone, ps) {
		return $elm$core$List$concat(
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$second,
				$elm$core$List$concat(
					A2(
						$elm$core$List$indexedMap,
						$author$project$Panel$columns(timeWithZone),
						$author$project$SafeZipper$toList(
							$author$project$Panels$panels(ps))))));
	});
var $author$project$Main$deleteItemsFromFile = F2(
	function (model, filePath) {
		var _v0 = model.taskList;
		if (_v0.$ === 'Loading') {
			return model;
		} else {
			var currentList = _v0.a;
			return _Utils_update(
				model,
				{
					taskList: $author$project$Main$Loaded(
						A2($author$project$TaskList$removeForFile, filePath, currentList))
				});
		}
	});
var $author$project$InteropPorts$deleteTodo = function (info) {
	return $author$project$InteropPorts$interopFromElm(
		A3($author$project$InteropPorts$encodeVariant, 'deleteTodo', $author$project$InteropDefinitions$deleteTodoEncoder, info));
};
var $author$project$TaskItem$hasNotes = function (_v0) {
	var fields = _v0.a;
	return !$elm$core$String$isEmpty(fields.notes);
};
var $author$project$TaskItem$notes = function (_v0) {
	var fields = _v0.a;
	return fields.notes;
};
var $author$project$Card$notesId = function (card) {
	return $author$project$Card$id(card) + ':notes';
};
var $author$project$Card$subtasks = function (_v0) {
	var idPrefix = _v0.a;
	var item = _v0.b;
	return A2(
		$elm$core$List$map,
		function (sub) {
			return _Utils_Tuple2(
				_Utils_ap(
					idPrefix,
					$author$project$TaskItem$id(sub)),
				sub);
		},
		$author$project$TaskItem$subtasks(item));
};
var $author$project$Card$taskItem = function (_v0) {
	var item = _v0.b;
	return item;
};
var $author$project$Card$markdownWithIds = function (card) {
	var subtaskMarkdownWithId = function (_v0) {
		var subtaskId = _v0.a;
		var subtask = _v0.b;
		return {
			id: subtaskId,
			markdown: $author$project$TaskItem$title(subtask)
		};
	};
	var subtasksWithIds = A2(
		$elm$core$List$map,
		subtaskMarkdownWithId,
		$author$project$Card$subtasks(card));
	var item = $author$project$Card$taskItem(card);
	var markdownWithId = function (c) {
		return _List_fromArray(
			[
				{
				id: $author$project$Card$id(c),
				markdown: $author$project$TaskItem$title(item)
			}
			]);
	};
	var notesWithId = $author$project$TaskItem$hasNotes(item) ? _List_fromArray(
		[
			{
			id: $author$project$Card$notesId(card),
			markdown: $author$project$TaskItem$notes(item)
		}
		]) : _List_Nil;
	return A2(
		$elm$core$List$append,
		notesWithId,
		A2(
			$elm$core$List$append,
			subtasksWithIds,
			markdownWithId(card)));
};
var $author$project$InteropPorts$displayTaskMarkdown = F2(
	function (filePath, cards) {
		return $author$project$InteropPorts$interopFromElm(
			A3(
				$author$project$InteropPorts$encodeVariant,
				'displayTodoMarkdown',
				$author$project$InteropDefinitions$displayTodoMarkdownEncoder,
				{
					filePath: filePath,
					todoMarkdown: A2($elm$core$List$concatMap, $author$project$Card$markdownWithIds, cards)
				}));
	});
var $author$project$TaskItem$filePath = function (_v0) {
	var fields = _v0.a;
	return fields.filePath;
};
var $author$project$Panels$Panels = F2(
	function (a, b) {
		return {$: 'Panels', a: a, b: b};
	});
var $author$project$Panels$init = F2(
	function (configs, taskList) {
		return A2($author$project$Panels$Panels, configs, taskList);
	});
var $author$project$TaskItem$lineNumber = function (_v0) {
	var fields = _v0.a;
	return fields.lineNumber;
};
var $author$project$InteropPorts$openTodoSourceFile = function (info) {
	return $author$project$InteropPorts$interopFromElm(
		A3($author$project$InteropPorts$encodeVariant, 'openTodoSourceFile', $author$project$InteropDefinitions$openTodoSourceFileEncoder, info));
};
var $author$project$TaskItem$originalText = function (_v0) {
	var fields = _v0.a;
	return fields.originalText;
};
var $elm$core$String$fromList = _String_fromList;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth = function (month) {
	switch (month.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $elm$time$Time$toMillis = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			1000,
			$elm$time$Time$posixToMillis(time));
	});
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString = F2(
	function (digits, time) {
		return A3(
			$elm$core$String$padLeft,
			digits,
			_Utils_chr('0'),
			$elm$core$String$fromInt(time));
	});
var $elm$time$Time$toSecond = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				1000));
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime = function (time) {
	return A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		4,
		A2($elm$time$Time$toYear, $elm$time$Time$utc, time)) + ('-' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		$rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth(
			A2($elm$time$Time$toMonth, $elm$time$Time$utc, time))) + ('-' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toDay, $elm$time$Time$utc, time)) + ('T' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toHour, $elm$time$Time$utc, time)) + (':' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toMinute, $elm$time$Time$utc, time)) + (':' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toSecond, $elm$time$Time$utc, time)) + ('.' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		3,
		A2($elm$time$Time$toMillis, $elm$time$Time$utc, time)) + 'Z'))))))))))));
};
var $elm_community$list_extra$List$Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				if (!list.b) {
					return $elm$core$List$reverse(memo);
				} else {
					var x = list.a;
					var xs = list.b;
					if (predicate(x)) {
						var $temp$memo = A2($elm$core$List$cons, x, memo),
							$temp$list = xs;
						memo = $temp$memo;
						list = $temp$list;
						continue takeWhileMemo;
					} else {
						return $elm$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(_List_Nil);
};
var $justinmimbs$date$Date$day = A2(
	$elm$core$Basics$composeR,
	$justinmimbs$date$Date$toCalendarDate,
	function ($) {
		return $.day;
	});
var $justinmimbs$date$Date$month = A2(
	$elm$core$Basics$composeR,
	$justinmimbs$date$Date$toCalendarDate,
	function ($) {
		return $.month;
	});
var $justinmimbs$date$Date$monthNumber = A2($elm$core$Basics$composeR, $justinmimbs$date$Date$month, $justinmimbs$date$Date$monthToNumber);
var $justinmimbs$date$Date$ordinalDay = A2(
	$elm$core$Basics$composeR,
	$justinmimbs$date$Date$toOrdinalDate,
	function ($) {
		return $.ordinalDay;
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $justinmimbs$date$Date$padSignedInt = F2(
	function (length, _int) {
		return _Utils_ap(
			(_int < 0) ? '-' : '',
			A3(
				$elm$core$String$padLeft,
				length,
				_Utils_chr('0'),
				$elm$core$String$fromInt(
					$elm$core$Basics$abs(_int))));
	});
var $justinmimbs$date$Date$monthToQuarter = function (m) {
	return (($justinmimbs$date$Date$monthToNumber(m) + 2) / 3) | 0;
};
var $justinmimbs$date$Date$quarter = A2($elm$core$Basics$composeR, $justinmimbs$date$Date$month, $justinmimbs$date$Date$monthToQuarter);
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $elm$time$Time$Fri = {$: 'Fri'};
var $elm$time$Time$Mon = {$: 'Mon'};
var $elm$time$Time$Sat = {$: 'Sat'};
var $elm$time$Time$Sun = {$: 'Sun'};
var $elm$time$Time$Thu = {$: 'Thu'};
var $elm$time$Time$Tue = {$: 'Tue'};
var $elm$time$Time$Wed = {$: 'Wed'};
var $justinmimbs$date$Date$numberToWeekday = function (wdn) {
	var _v0 = A2($elm$core$Basics$max, 1, wdn);
	switch (_v0) {
		case 1:
			return $elm$time$Time$Mon;
		case 2:
			return $elm$time$Time$Tue;
		case 3:
			return $elm$time$Time$Wed;
		case 4:
			return $elm$time$Time$Thu;
		case 5:
			return $elm$time$Time$Fri;
		case 6:
			return $elm$time$Time$Sat;
		default:
			return $elm$time$Time$Sun;
	}
};
var $justinmimbs$date$Date$toWeekDate = function (_v0) {
	var rd = _v0.a;
	var wdn = $justinmimbs$date$Date$weekdayNumber(
		$justinmimbs$date$Date$RD(rd));
	var wy = $justinmimbs$date$Date$year(
		$justinmimbs$date$Date$RD(rd + (4 - wdn)));
	var week1Day1 = $justinmimbs$date$Date$daysBeforeWeekYear(wy) + 1;
	return {
		weekNumber: 1 + (((rd - week1Day1) / 7) | 0),
		weekYear: wy,
		weekday: $justinmimbs$date$Date$numberToWeekday(wdn)
	};
};
var $justinmimbs$date$Date$weekNumber = A2(
	$elm$core$Basics$composeR,
	$justinmimbs$date$Date$toWeekDate,
	function ($) {
		return $.weekNumber;
	});
var $justinmimbs$date$Date$weekYear = A2(
	$elm$core$Basics$composeR,
	$justinmimbs$date$Date$toWeekDate,
	function ($) {
		return $.weekYear;
	});
var $justinmimbs$date$Date$weekday = A2($elm$core$Basics$composeR, $justinmimbs$date$Date$weekdayNumber, $justinmimbs$date$Date$numberToWeekday);
var $justinmimbs$date$Date$ordinalSuffix = function (n) {
	var nn = A2($elm$core$Basics$modBy, 100, n);
	var _v0 = A2(
		$elm$core$Basics$min,
		(nn < 20) ? nn : A2($elm$core$Basics$modBy, 10, nn),
		4);
	switch (_v0) {
		case 1:
			return 'st';
		case 2:
			return 'nd';
		case 3:
			return 'rd';
		default:
			return 'th';
	}
};
var $justinmimbs$date$Date$withOrdinalSuffix = function (n) {
	return _Utils_ap(
		$elm$core$String$fromInt(n),
		$justinmimbs$date$Date$ordinalSuffix(n));
};
var $justinmimbs$date$Date$formatField = F4(
	function (language, _char, length, date) {
		switch (_char.valueOf()) {
			case 'y':
				if (length === 2) {
					return A2(
						$elm$core$String$right,
						2,
						A3(
							$elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							$elm$core$String$fromInt(
								$justinmimbs$date$Date$year(date))));
				} else {
					return A2(
						$justinmimbs$date$Date$padSignedInt,
						length,
						$justinmimbs$date$Date$year(date));
				}
			case 'Y':
				if (length === 2) {
					return A2(
						$elm$core$String$right,
						2,
						A3(
							$elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							$elm$core$String$fromInt(
								$justinmimbs$date$Date$weekYear(date))));
				} else {
					return A2(
						$justinmimbs$date$Date$padSignedInt,
						length,
						$justinmimbs$date$Date$weekYear(date));
				}
			case 'Q':
				switch (length) {
					case 1:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$quarter(date));
					case 2:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$quarter(date));
					case 3:
						return 'Q' + $elm$core$String$fromInt(
							$justinmimbs$date$Date$quarter(date));
					case 4:
						return $justinmimbs$date$Date$withOrdinalSuffix(
							$justinmimbs$date$Date$quarter(date));
					case 5:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$quarter(date));
					default:
						return '';
				}
			case 'M':
				switch (length) {
					case 1:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$monthNumber(date));
					case 2:
						return A3(
							$elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							$elm$core$String$fromInt(
								$justinmimbs$date$Date$monthNumber(date)));
					case 3:
						return language.monthNameShort(
							$justinmimbs$date$Date$month(date));
					case 4:
						return language.monthName(
							$justinmimbs$date$Date$month(date));
					case 5:
						return A2(
							$elm$core$String$left,
							1,
							language.monthNameShort(
								$justinmimbs$date$Date$month(date)));
					default:
						return '';
				}
			case 'w':
				switch (length) {
					case 1:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$weekNumber(date));
					case 2:
						return A3(
							$elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							$elm$core$String$fromInt(
								$justinmimbs$date$Date$weekNumber(date)));
					default:
						return '';
				}
			case 'd':
				switch (length) {
					case 1:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$day(date));
					case 2:
						return A3(
							$elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							$elm$core$String$fromInt(
								$justinmimbs$date$Date$day(date)));
					case 3:
						return language.dayWithSuffix(
							$justinmimbs$date$Date$day(date));
					default:
						return '';
				}
			case 'D':
				switch (length) {
					case 1:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$ordinalDay(date));
					case 2:
						return A3(
							$elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							$elm$core$String$fromInt(
								$justinmimbs$date$Date$ordinalDay(date)));
					case 3:
						return A3(
							$elm$core$String$padLeft,
							3,
							_Utils_chr('0'),
							$elm$core$String$fromInt(
								$justinmimbs$date$Date$ordinalDay(date)));
					default:
						return '';
				}
			case 'E':
				switch (length) {
					case 1:
						return language.weekdayNameShort(
							$justinmimbs$date$Date$weekday(date));
					case 2:
						return language.weekdayNameShort(
							$justinmimbs$date$Date$weekday(date));
					case 3:
						return language.weekdayNameShort(
							$justinmimbs$date$Date$weekday(date));
					case 4:
						return language.weekdayName(
							$justinmimbs$date$Date$weekday(date));
					case 5:
						return A2(
							$elm$core$String$left,
							1,
							language.weekdayNameShort(
								$justinmimbs$date$Date$weekday(date)));
					case 6:
						return A2(
							$elm$core$String$left,
							2,
							language.weekdayNameShort(
								$justinmimbs$date$Date$weekday(date)));
					default:
						return '';
				}
			case 'e':
				switch (length) {
					case 1:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$weekdayNumber(date));
					case 2:
						return $elm$core$String$fromInt(
							$justinmimbs$date$Date$weekdayNumber(date));
					default:
						return A4(
							$justinmimbs$date$Date$formatField,
							language,
							_Utils_chr('E'),
							length,
							date);
				}
			default:
				return '';
		}
	});
var $justinmimbs$date$Date$formatWithTokens = F3(
	function (language, tokens, date) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (token, formatted) {
					if (token.$ === 'Field') {
						var _char = token.a;
						var length = token.b;
						return _Utils_ap(
							A4($justinmimbs$date$Date$formatField, language, _char, length, date),
							formatted);
					} else {
						var str = token.a;
						return _Utils_ap(str, formatted);
					}
				}),
			'',
			tokens);
	});
var $justinmimbs$date$Pattern$Literal = function (a) {
	return {$: 'Literal', a: a};
};
var $justinmimbs$date$Pattern$escapedQuote = A2(
	$elm$parser$Parser$ignorer,
	$elm$parser$Parser$succeed(
		$justinmimbs$date$Pattern$Literal('\'')),
	$elm$parser$Parser$token('\'\''));
var $justinmimbs$date$Pattern$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $justinmimbs$date$Pattern$fieldRepeats = function (str) {
	var _v0 = $elm$core$String$toList(str);
	if (_v0.b && (!_v0.b.b)) {
		var _char = _v0.a;
		return A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F2(
						function (x, y) {
							return A2($justinmimbs$date$Pattern$Field, _char, 1 + (y - x));
						})),
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$getOffset,
					$elm$parser$Parser$chompWhile(
						$elm$core$Basics$eq(_char)))),
			$elm$parser$Parser$getOffset);
	} else {
		return $elm$parser$Parser$problem('expected exactly one char');
	}
};
var $justinmimbs$date$Pattern$field = A2(
	$elm$parser$Parser$andThen,
	$justinmimbs$date$Pattern$fieldRepeats,
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompIf($elm$core$Char$isAlpha)));
var $justinmimbs$date$Pattern$finalize = A2(
	$elm$core$List$foldl,
	F2(
		function (token, tokens) {
			var _v0 = _Utils_Tuple2(token, tokens);
			if (((_v0.a.$ === 'Literal') && _v0.b.b) && (_v0.b.a.$ === 'Literal')) {
				var x = _v0.a.a;
				var _v1 = _v0.b;
				var y = _v1.a.a;
				var rest = _v1.b;
				return A2(
					$elm$core$List$cons,
					$justinmimbs$date$Pattern$Literal(
						_Utils_ap(x, y)),
					rest);
			} else {
				return A2($elm$core$List$cons, token, tokens);
			}
		}),
	_List_Nil);
var $elm$parser$Parser$Advanced$lazy = function (thunk) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v0 = thunk(_Utils_Tuple0);
			var parse = _v0.a;
			return parse(s);
		});
};
var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
var $justinmimbs$date$Pattern$isLiteralChar = function (_char) {
	return (!_Utils_eq(
		_char,
		_Utils_chr('\''))) && (!$elm$core$Char$isAlpha(_char));
};
var $justinmimbs$date$Pattern$literal = A2(
	$elm$parser$Parser$map,
	$justinmimbs$date$Pattern$Literal,
	$elm$parser$Parser$getChompedString(
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(_Utils_Tuple0),
				$elm$parser$Parser$chompIf($justinmimbs$date$Pattern$isLiteralChar)),
			$elm$parser$Parser$chompWhile($justinmimbs$date$Pattern$isLiteralChar))));
var $justinmimbs$date$Pattern$quotedHelp = function (result) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$andThen,
				function (str) {
					return $justinmimbs$date$Pattern$quotedHelp(
						_Utils_ap(result, str));
				},
				$elm$parser$Parser$getChompedString(
					A2(
						$elm$parser$Parser$ignorer,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed(_Utils_Tuple0),
							$elm$parser$Parser$chompIf(
								$elm$core$Basics$neq(
									_Utils_chr('\'')))),
						$elm$parser$Parser$chompWhile(
							$elm$core$Basics$neq(
								_Utils_chr('\'')))))),
				A2(
				$elm$parser$Parser$andThen,
				function (_v0) {
					return $justinmimbs$date$Pattern$quotedHelp(result + '\'');
				},
				$elm$parser$Parser$token('\'\'')),
				$elm$parser$Parser$succeed(result)
			]));
};
var $justinmimbs$date$Pattern$quoted = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed($justinmimbs$date$Pattern$Literal),
		$elm$parser$Parser$chompIf(
			$elm$core$Basics$eq(
				_Utils_chr('\'')))),
	A2(
		$elm$parser$Parser$ignorer,
		$justinmimbs$date$Pattern$quotedHelp(''),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$chompIf(
					$elm$core$Basics$eq(
						_Utils_chr('\''))),
					$elm$parser$Parser$end
				]))));
var $justinmimbs$date$Pattern$patternHelp = function (tokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$andThen,
				function (token) {
					return $justinmimbs$date$Pattern$patternHelp(
						A2($elm$core$List$cons, token, tokens));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[$justinmimbs$date$Pattern$field, $justinmimbs$date$Pattern$literal, $justinmimbs$date$Pattern$escapedQuote, $justinmimbs$date$Pattern$quoted]))),
				$elm$parser$Parser$lazy(
				function (_v0) {
					return $elm$parser$Parser$succeed(
						$justinmimbs$date$Pattern$finalize(tokens));
				})
			]));
};
var $justinmimbs$date$Pattern$fromString = function (str) {
	return A2(
		$elm$core$Result$withDefault,
		_List_fromArray(
			[
				$justinmimbs$date$Pattern$Literal(str)
			]),
		A2(
			$elm$parser$Parser$run,
			$justinmimbs$date$Pattern$patternHelp(_List_Nil),
			str));
};
var $justinmimbs$date$Date$formatWithLanguage = F2(
	function (language, pattern) {
		var tokens = $elm$core$List$reverse(
			$justinmimbs$date$Pattern$fromString(pattern));
		return A2($justinmimbs$date$Date$formatWithTokens, language, tokens);
	});
var $justinmimbs$date$Date$monthToName = function (m) {
	switch (m.$) {
		case 'Jan':
			return 'January';
		case 'Feb':
			return 'February';
		case 'Mar':
			return 'March';
		case 'Apr':
			return 'April';
		case 'May':
			return 'May';
		case 'Jun':
			return 'June';
		case 'Jul':
			return 'July';
		case 'Aug':
			return 'August';
		case 'Sep':
			return 'September';
		case 'Oct':
			return 'October';
		case 'Nov':
			return 'November';
		default:
			return 'December';
	}
};
var $justinmimbs$date$Date$weekdayToName = function (wd) {
	switch (wd.$) {
		case 'Mon':
			return 'Monday';
		case 'Tue':
			return 'Tuesday';
		case 'Wed':
			return 'Wednesday';
		case 'Thu':
			return 'Thursday';
		case 'Fri':
			return 'Friday';
		case 'Sat':
			return 'Saturday';
		default:
			return 'Sunday';
	}
};
var $justinmimbs$date$Date$language_en = {
	dayWithSuffix: $justinmimbs$date$Date$withOrdinalSuffix,
	monthName: $justinmimbs$date$Date$monthToName,
	monthNameShort: A2(
		$elm$core$Basics$composeR,
		$justinmimbs$date$Date$monthToName,
		$elm$core$String$left(3)),
	weekdayName: $justinmimbs$date$Date$weekdayToName,
	weekdayNameShort: A2(
		$elm$core$Basics$composeR,
		$justinmimbs$date$Date$weekdayToName,
		$elm$core$String$left(3))
};
var $justinmimbs$date$Date$format = function (pattern) {
	return A2($justinmimbs$date$Date$formatWithLanguage, $justinmimbs$date$Date$language_en, pattern);
};
var $justinmimbs$date$Date$toIsoString = $justinmimbs$date$Date$format('yyyy-MM-dd');
var $elm$core$String$trim = _String_trim;
var $author$project$TaskItem$toString = function (_v0) {
	var fields = _v0.a;
	var leadingWhiteSpace = $elm$core$String$fromList(
		A2(
			$elm_community$list_extra$List$Extra$takeWhile,
			$author$project$ParserHelper$isSpaceOrTab,
			$elm$core$String$toList(fields.originalText)));
	var fieldTags = ($elm$core$List$length(fields.tags) > 0) ? A2(
		$elm$core$String$append,
		' ',
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$String$append('#'),
				fields.tags))) : '';
	var dueTag = function () {
		var _v5 = fields.dueTag;
		if (_v5.$ === 'Just') {
			var date = _v5.a;
			return ' @due(' + ($justinmimbs$date$Date$toIsoString(date) + ')');
		} else {
			return '';
		}
	}();
	var completionTag = function () {
		var _v4 = fields.completion;
		if (_v4.$ === 'CompletedAt') {
			var completionTime = _v4.a;
			var completionString = A2(
				$elm$core$String$left,
				19,
				$rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime(completionTime));
			return ' @completed(' + (completionString + ')');
		} else {
			return '';
		}
	}();
	var checkbox = function () {
		var _v3 = fields.completion;
		if (_v3.$ === 'Incomplete') {
			return '- [ ] ';
		} else {
			return '- [x] ';
		}
	}();
	var blockLinkText = function () {
		var _v2 = fields.blockLink;
		if (_v2.$ === 'Just') {
			var blockLink_ = _v2.a;
			return ' ' + blockLink_;
		} else {
			return '';
		}
	}();
	var autoCompleteTag = function () {
		var _v1 = fields.autoComplete;
		switch (_v1.$) {
			case 'NotSpecifed':
				return '';
			case 'FalseSpecified':
				return ' @autocomplete(false)';
			default:
				return ' @autocomplete(true)';
		}
	}();
	return _Utils_ap(
		leadingWhiteSpace,
		_Utils_ap(
			checkbox,
			_Utils_ap(
				$elm$core$String$trim(fields.title),
				_Utils_ap(
					fieldTags,
					_Utils_ap(
						dueTag,
						_Utils_ap(
							autoCompleteTag,
							_Utils_ap(completionTag, blockLinkText)))))));
};
var $author$project$TaskItem$toggleCompletion = F2(
	function (timeWithZone, _v0) {
		var fields = _v0.a;
		var subtasks_ = _v0.b;
		var _v1 = fields.completion;
		switch (_v1.$) {
			case 'Completed':
				return A2(
					$author$project$TaskItem$TaskItem,
					_Utils_update(
						fields,
						{completion: $author$project$TaskItem$Incomplete}),
					subtasks_);
			case 'CompletedAt':
				return A2(
					$author$project$TaskItem$TaskItem,
					_Utils_update(
						fields,
						{completion: $author$project$TaskItem$Incomplete}),
					subtasks_);
			default:
				return A2(
					$author$project$TaskItem$TaskItem,
					_Utils_update(
						fields,
						{
							completion: $author$project$TaskItem$CompletedAt(timeWithZone.now)
						}),
					subtasks_);
		}
	});
var $author$project$InteropPorts$rewriteTodos = F3(
	function (timeWithZone, filePath, taskItems) {
		var rewriteDetails = function (taskItem) {
			return {
				lineNumber: $author$project$TaskItem$lineNumber(taskItem),
				newText: $author$project$TaskItem$toString(
					A2($author$project$TaskItem$toggleCompletion, timeWithZone, taskItem)),
				originalText: $author$project$TaskItem$originalText(taskItem)
			};
		};
		return $author$project$InteropPorts$interopFromElm(
			A3(
				$author$project$InteropPorts$encodeVariant,
				'updateTodos',
				$author$project$InteropDefinitions$updateTodosEncoder,
				{
					filePath: filePath,
					todos: A2($elm$core$List$map, rewriteDetails, taskItems)
				}));
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$TaskItem$containsId = F2(
	function (targetId, taskItem) {
		return A2(
			$elm$core$List$member,
			targetId,
			A2(
				$elm$core$List$cons,
				$author$project$TaskItem$id(taskItem),
				A2(
					$elm$core$List$map,
					$author$project$TaskItem$id,
					$author$project$TaskItem$subtasks(taskItem))));
	});
var $elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $author$project$TaskList$tasks = function (_v0) {
	var taskList = _v0.a;
	return A2(
		$elm$core$List$concatMap,
		function (t) {
			return A2(
				$elm$core$List$cons,
				t,
				$author$project$TaskItem$subtasks(t));
		},
		taskList);
};
var $author$project$TaskList$taskContainingId = F2(
	function (id, taskList) {
		return A2(
			$elm_community$list_extra$List$Extra$find,
			$author$project$TaskItem$containsId(id),
			$author$project$TaskList$tasks(taskList));
	});
var $author$project$TaskList$taskFromId = F2(
	function (id, taskList) {
		return A2(
			$elm_community$list_extra$List$Extra$find,
			function (i) {
				return _Utils_eq(
					$author$project$TaskItem$id(i),
					id);
			},
			$author$project$TaskList$tasks(taskList));
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$TaskItem$autoComplete = function (_v0) {
	var fields = _v0.a;
	return fields.autoComplete;
};
var $author$project$TaskItem$tasksToToggle = F3(
	function (id_, timeWithZone, taskItem) {
		var topLevelTaskIsNotAlreadyComplete = !$author$project$TaskItem$isCompleted(taskItem);
		var shouldAutoComplete = function () {
			var _v0 = $author$project$TaskItem$autoComplete(taskItem);
			if (_v0.$ === 'TrueSpecified') {
				return true;
			} else {
				return false;
			}
		}();
		var resultIsAllSubtasksCompleted = A2(
			$elm$core$List$all,
			$author$project$TaskItem$isCompleted,
			A2(
				$elm$core$List$map,
				function (t) {
					return _Utils_eq(
						$author$project$TaskItem$id(t),
						id_) ? A2($author$project$TaskItem$toggleCompletion, timeWithZone, t) : t;
				},
				$author$project$TaskItem$subtasks(taskItem)));
		var matchingTaskItem = A2(
			$elm$core$List$filter,
			function (t) {
				return _Utils_eq(
					$author$project$TaskItem$id(t),
					id_);
			},
			A2(
				$elm$core$List$cons,
				taskItem,
				$author$project$TaskItem$subtasks(taskItem)));
		var idBelongsToSubtask = A2(
			$elm$core$List$member,
			id_,
			A2(
				$elm$core$List$map,
				$author$project$TaskItem$id,
				$author$project$TaskItem$subtasks(taskItem)));
		return (shouldAutoComplete && (idBelongsToSubtask && (resultIsAllSubtasksCompleted && topLevelTaskIsNotAlreadyComplete))) ? A2($elm$core$List$cons, taskItem, matchingTaskItem) : matchingTaskItem;
	});
var $author$project$Main$updateTaskItems = F3(
	function (model, filePath, updatedList) {
		var _v0 = model.taskList;
		if (_v0.$ === 'Loading') {
			return _Utils_update(
				model,
				{
					taskList: $author$project$Main$Loaded(updatedList)
				});
		} else {
			var currentList = _v0.a;
			return _Utils_update(
				model,
				{
					taskList: $author$project$Main$Loaded(
						A3($author$project$TaskList$replaceForFile, filePath, updatedList, currentList))
				});
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var _v0 = _Utils_Tuple2(msg, model);
		switch (_v0.a.$) {
			case 'BadInputFromTypeScript':
				var _v1 = _v0.a;
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'ReceiveTime':
				var _v2 = _v0.a.a;
				var zone = _v2.a;
				var posix = _v2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timeWithZone: {now: posix, zone: zone}
						}),
					$elm$core$Platform$Cmd$none);
			case 'TabSelected':
				var tabIndex = _v0.a.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							boardConfigs: A2($author$project$SafeZipper$atIndex, tabIndex, model.boardConfigs)
						}),
					$elm$core$Platform$Cmd$none);
			case 'TaskItemDeleteClicked':
				var id = _v0.a.a;
				var _v3 = model.taskList;
				if (_v3.$ === 'Loaded') {
					var taskList = _v3.a;
					var _v4 = A2($author$project$TaskList$taskFromId, id, taskList);
					if (_v4.$ === 'Just') {
						var matchingItem = _v4.a;
						return _Utils_Tuple2(
							model,
							$author$project$InteropPorts$deleteTodo(
								{
									filePath: $author$project$TaskItem$filePath(matchingItem),
									lineNumber: $author$project$TaskItem$lineNumber(matchingItem),
									originalText: $author$project$TaskItem$originalText(matchingItem)
								}));
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'TaskItemEditClicked':
				var id = _v0.a.a;
				var _v5 = model.taskList;
				if (_v5.$ === 'Loaded') {
					var taskList = _v5.a;
					var _v6 = A2($author$project$TaskList$taskFromId, id, taskList);
					if (_v6.$ === 'Just') {
						var matchingItem = _v6.a;
						return _Utils_Tuple2(
							model,
							$author$project$InteropPorts$openTodoSourceFile(
								{
									blockLink: $author$project$TaskItem$blockLink(matchingItem),
									filePath: $author$project$TaskItem$filePath(matchingItem),
									lineNumber: $author$project$TaskItem$lineNumber(matchingItem),
									originalText: $author$project$TaskItem$originalText(matchingItem)
								}));
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'TaskItemToggled':
				var id = _v0.a.a;
				var _v7 = model.taskList;
				if (_v7.$ === 'Loaded') {
					var taskList = _v7.a;
					var _v8 = A2($author$project$TaskList$taskContainingId, id, taskList);
					if (_v8.$ === 'Just') {
						var matchingItem = _v8.a;
						return _Utils_Tuple2(
							model,
							A3(
								$author$project$InteropPorts$rewriteTodos,
								model.timeWithZone,
								$author$project$TaskItem$filePath(matchingItem),
								A3($author$project$TaskItem$tasksToToggle, id, model.timeWithZone, matchingItem)));
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'Tick':
				var time = _v0.a.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timeWithZone: A2($author$project$TimeWithZone$now, time, model.timeWithZone)
						}),
					$elm$core$Platform$Cmd$none);
			case 'VaultFileAdded':
				var markdownFile = _v0.a.a;
				var newTaskItems = A3($author$project$TaskList$fromMarkdown, markdownFile.filePath, markdownFile.fileDate, markdownFile.fileContents);
				var cards = A2(
					$author$project$Panels$cards,
					model.timeWithZone,
					A2($author$project$Panels$init, model.boardConfigs, newTaskItems));
				return _Utils_Tuple2(
					A2($author$project$Main$addTaskItems, model, newTaskItems),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2($author$project$InteropPorts$displayTaskMarkdown, markdownFile.filePath, cards),
								A2($author$project$InteropPorts$addHoverToCardEditButtons, markdownFile.filePath, cards)
							])));
			case 'VaultFileDeleted':
				var filePath = _v0.a.a;
				return _Utils_Tuple2(
					A2($author$project$Main$deleteItemsFromFile, model, filePath),
					$elm$core$Platform$Cmd$none);
			default:
				var markdownFile = _v0.a.a;
				var updatedTaskItems = A3($author$project$TaskList$fromMarkdown, markdownFile.filePath, markdownFile.fileDate, markdownFile.fileContents);
				var cards = A2(
					$author$project$Panels$cards,
					model.timeWithZone,
					A2($author$project$Panels$init, model.boardConfigs, updatedTaskItems));
				return _Utils_Tuple2(
					A3($author$project$Main$updateTaskItems, model, markdownFile.filePath, updatedTaskItems),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2($author$project$InteropPorts$displayTaskMarkdown, markdownFile.filePath, cards),
								A2($author$project$InteropPorts$addHoverToCardEditButtons, markdownFile.filePath, cards)
							])));
		}
	});
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$SafeZipper$selectedIndex = function (zipper) {
	if (zipper.$ === 'EmptyZipper') {
		return $elm$core$Maybe$Nothing;
	} else {
		var b = zipper.a;
		return $elm$core$Maybe$Just(
			$elm$core$List$length(b));
	}
};
var $author$project$Panels$currentIndex = function (_v0) {
	var config = _v0.a;
	return $author$project$SafeZipper$selectedIndex(config);
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Main$TaskItemToggled = function (a) {
	return {$: 'TaskItemToggled', a: a};
};
var $author$project$Main$TaskItemDeleteClicked = function (a) {
	return {$: 'TaskItemDeleteClicked', a: a};
};
var $author$project$Main$TaskItemEditClicked = function (a) {
	return {$: 'TaskItemEditClicked', a: a};
};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $1602$elm_feather$FeatherIcons$Icon = function (a) {
	return {$: 'Icon', a: a};
};
var $1602$elm_feather$FeatherIcons$defaultAttributes = function (name) {
	return {
		_class: $elm$core$Maybe$Just('feather feather-' + name),
		size: 24,
		sizeUnit: '',
		strokeWidth: 2,
		viewBox: '0 0 24 24'
	};
};
var $1602$elm_feather$FeatherIcons$makeBuilder = F2(
	function (name, src) {
		return $1602$elm_feather$FeatherIcons$Icon(
			{
				attrs: $1602$elm_feather$FeatherIcons$defaultAttributes(name),
				src: src
			});
	});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var $elm$svg$Svg$Attributes$strokeLinejoin = _VirtualDom_attribute('stroke-linejoin');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $1602$elm_feather$FeatherIcons$xmlns = function (s) {
	return A2(
		$elm$virtual_dom$VirtualDom$property,
		'xmlns',
		$elm$json$Json$Encode$string(s));
};
var $1602$elm_feather$FeatherIcons$edit = A2(
	$1602$elm_feather$FeatherIcons$makeBuilder,
	'edit',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$svg,
			_List_fromArray(
				[
					$1602$elm_feather$FeatherIcons$xmlns('http://www.w3.org/2000/svg'),
					$elm$svg$Svg$Attributes$width('24'),
					$elm$svg$Svg$Attributes$height('24'),
					$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
					$elm$svg$Svg$Attributes$fill('none'),
					$elm$svg$Svg$Attributes$stroke('currentColor'),
					$elm$svg$Svg$Attributes$strokeWidth('2'),
					$elm$svg$Svg$Attributes$strokeLinecap('round'),
					$elm$svg$Svg$Attributes$strokeLinejoin('round'),
					$elm$svg$Svg$Attributes$class('feather feather-edit')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('M20 14.66V20a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h5.34')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$polygon,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$points('18 2 22 6 12 16 8 16 8 12 18 2')
						]),
					_List_Nil)
				]))
		]));
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$svg$Svg$map = $elm$virtual_dom$VirtualDom$map;
var $1602$elm_feather$FeatherIcons$toHtml = F2(
	function (attributes, _v0) {
		var src = _v0.a.src;
		var attrs = _v0.a.attrs;
		var strSize = $elm$core$String$fromFloat(attrs.size);
		var baseAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$fill('none'),
				$elm$svg$Svg$Attributes$height(
				_Utils_ap(strSize, attrs.sizeUnit)),
				$elm$svg$Svg$Attributes$width(
				_Utils_ap(strSize, attrs.sizeUnit)),
				$elm$svg$Svg$Attributes$stroke('currentColor'),
				$elm$svg$Svg$Attributes$strokeLinecap('round'),
				$elm$svg$Svg$Attributes$strokeLinejoin('round'),
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromFloat(attrs.strokeWidth)),
				$elm$svg$Svg$Attributes$viewBox(attrs.viewBox)
			]);
		var combinedAttributes = _Utils_ap(
			function () {
				var _v1 = attrs._class;
				if (_v1.$ === 'Just') {
					var c = _v1.a;
					return A2(
						$elm$core$List$cons,
						$elm$svg$Svg$Attributes$class(c),
						baseAttributes);
				} else {
					return baseAttributes;
				}
			}(),
			attributes);
		return A2(
			$elm$svg$Svg$svg,
			combinedAttributes,
			A2(
				$elm$core$List$map,
				$elm$svg$Svg$map($elm$core$Basics$never),
				src));
	});
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $1602$elm_feather$FeatherIcons$trash = A2(
	$1602$elm_feather$FeatherIcons$makeBuilder,
	'trash',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$svg,
			_List_fromArray(
				[
					$1602$elm_feather$FeatherIcons$xmlns('http://www.w3.org/2000/svg'),
					$elm$svg$Svg$Attributes$width('24'),
					$elm$svg$Svg$Attributes$height('24'),
					$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
					$elm$svg$Svg$Attributes$fill('none'),
					$elm$svg$Svg$Attributes$stroke('currentColor'),
					$elm$svg$Svg$Attributes$strokeWidth('2'),
					$elm$svg$Svg$Attributes$strokeLinecap('round'),
					$elm$svg$Svg$Attributes$strokeLinejoin('round'),
					$elm$svg$Svg$Attributes$class('feather feather-trash')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$polyline,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$points('3 6 5 6 21 6')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2')
						]),
					_List_Nil)
				]))
		]));
var $1602$elm_feather$FeatherIcons$withSize = F2(
	function (size, _v0) {
		var attrs = _v0.a.attrs;
		var src = _v0.a.src;
		return $1602$elm_feather$FeatherIcons$Icon(
			{
				attrs: _Utils_update(
					attrs,
					{size: size}),
				src: src
			});
	});
var $1602$elm_feather$FeatherIcons$withSizeUnit = F2(
	function (sizeUnit, _v0) {
		var attrs = _v0.a.attrs;
		var src = _v0.a.src;
		return $1602$elm_feather$FeatherIcons$Icon(
			{
				attrs: _Utils_update(
					attrs,
					{sizeUnit: sizeUnit}),
				src: src
			});
	});
var $author$project$Main$cardActionButtons = F2(
	function (taskItemId, editButtonId) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-card-action-area-buttons')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-card-action-area-button'),
							$elm$html$Html$Events$onClick(
							$author$project$Main$TaskItemEditClicked(taskItemId)),
							$elm$html$Html$Attributes$id(editButtonId)
						]),
					_List_fromArray(
						[
							A2(
							$1602$elm_feather$FeatherIcons$toHtml,
							_List_Nil,
							A2(
								$1602$elm_feather$FeatherIcons$withSizeUnit,
								'em',
								A2($1602$elm_feather$FeatherIcons$withSize, 1, $1602$elm_feather$FeatherIcons$edit)))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-card-action-area-button'),
							$elm$html$Html$Events$onClick(
							$author$project$Main$TaskItemDeleteClicked(taskItemId))
						]),
					_List_fromArray(
						[
							A2(
							$1602$elm_feather$FeatherIcons$toHtml,
							_List_Nil,
							A2(
								$1602$elm_feather$FeatherIcons$withSizeUnit,
								'em',
								A2($1602$elm_feather$FeatherIcons$withSize, 1, $1602$elm_feather$FeatherIcons$trash)))
						]))
				]));
	});
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Main$cardTagView = function (tagText) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card-board-card-tag')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('cm-hashtag-begin cm-hashtag')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('#')
					])),
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('cm-list-1 cm-hashtag cm-hashtag-end')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(tagText)
					])),
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('cm-list-1')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(' ')
					]))
			]));
};
var $author$project$Main$cardTagsView = function (tags) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card-board-card-tag-area')
			]),
		A2($elm$core$List$map, $author$project$Main$cardTagView, tags));
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$TaskItem$hasSubtasks = function (_v0) {
	var subtasks_ = _v0.b;
	return !$elm$core$List$isEmpty(subtasks_);
};
var $author$project$TaskItem$hasTags = function (taskItem) {
	return !$elm$core$List$isEmpty(
		$author$project$TaskItem$tags(taskItem));
};
var $author$project$Card$HighlightCritical = {$: 'HighlightCritical'};
var $author$project$Card$HighlightGood = {$: 'HighlightGood'};
var $author$project$Card$HighlightImportant = {$: 'HighlightImportant'};
var $author$project$Card$HighlightNone = {$: 'HighlightNone'};
var $author$project$Card$highlight = F2(
	function (timeWithZone, _v0) {
		var item = _v0.b;
		var datestamp = $author$project$TimeWithZone$toDate(timeWithZone);
		var _v1 = _Utils_Tuple2(
			$author$project$TaskItem$isCompleted(item),
			$author$project$TaskItem$due(item));
		if ((!_v1.a) && (_v1.b.$ === 'Just')) {
			var dueDate = _v1.b.a;
			return _Utils_eq(datestamp, dueDate) ? $author$project$Card$HighlightImportant : ((_Utils_cmp(
				$justinmimbs$date$Date$toRataDie(datestamp),
				$justinmimbs$date$Date$toRataDie(dueDate)) > 0) ? $author$project$Card$HighlightCritical : ((_Utils_cmp(
				$justinmimbs$date$Date$toRataDie(datestamp),
				$justinmimbs$date$Date$toRataDie(dueDate)) < 0) ? $author$project$Card$HighlightGood : $author$project$Card$HighlightNone));
		} else {
			return $author$project$Card$HighlightNone;
		}
	});
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$li = _VirtualDom_node('li');
var $author$project$Main$notesView = function (notesId) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card-board-card-notes-area'),
				$elm$html$Html$Attributes$id(notesId)
			]),
		_List_Nil);
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$subtaskView = function (_v0) {
	var uniqueId = _v0.a;
	var subtask = _v0.b;
	return A2(
		$elm$html$Html$li,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card-board-card-subtask task-list-item')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('checkbox'),
						$elm$html$Html$Attributes$class('task-list-item-checkbox'),
						$elm$html$Html$Events$onClick(
						$author$project$Main$TaskItemToggled(
							$author$project$TaskItem$id(subtask))),
						$elm$html$Html$Attributes$checked(
						$author$project$TaskItem$isCompleted(subtask))
					]),
				_List_Nil),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('card-board-card-title'),
						$elm$html$Html$Attributes$id(uniqueId)
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Main$subtasksView = function (subtasks) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card-board-card-subtasks-area')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$ul,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('contains-task-list')
					]),
				A2($elm$core$List$map, $author$project$Main$subtaskView, subtasks))
			]));
};
var $author$project$Main$dueDateString = function (dueDate) {
	if (dueDate.$ === 'Just') {
		var date = dueDate.a;
		return A2($justinmimbs$date$Date$format, 'E, MMM ddd', date);
	} else {
		return 'n/a';
	}
};
var $author$project$Main$taskDueDate = function (dueDate) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card-board-card-action-area-due')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				'Due: ' + $author$project$Main$dueDateString(dueDate))
			]));
};
var $author$project$Main$empty = $elm$html$Html$text('');
var $author$project$Main$when = F2(
	function (shouldRender, html) {
		return shouldRender ? html : $author$project$Main$empty;
	});
var $author$project$Main$cardView = F2(
	function (timeWithZone, card) {
		var taskItemId = $author$project$Card$taskItemId(card);
		var taskItem = $author$project$Card$taskItem(card);
		var highlightAreaClass = function () {
			var _v0 = A2($author$project$Card$highlight, timeWithZone, card);
			switch (_v0.$) {
				case 'HighlightCritical':
					return 'critical';
				case 'HighlightGood':
					return 'good';
				case 'HighlightImportant':
					return 'important';
				default:
					return '';
			}
		}();
		var cardId = $author$project$Card$id(card);
		return A2(
			$elm$core$Tuple$pair,
			cardId,
			A2(
				$elm$html$Html$li,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('card-board-card cm-s-obsidian markdown-preview-view')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('card-board-card-highlight-area ' + highlightAreaClass)
							]),
						_List_Nil),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('card-board-card-content-area')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('checkbox'),
										$elm$html$Html$Attributes$class('task-list-item-checkbox'),
										$elm$html$Html$Events$onClick(
										$author$project$Main$TaskItemToggled(taskItemId)),
										$elm$html$Html$Attributes$checked(
										$author$project$TaskItem$isCompleted(taskItem))
									]),
								_List_Nil),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('card-board-card-title'),
										$elm$html$Html$Attributes$id(cardId)
									]),
								_List_Nil),
								A2(
								$author$project$Main$when,
								$author$project$TaskItem$hasTags(taskItem),
								$author$project$Main$cardTagsView(
									$author$project$TaskItem$tags(taskItem))),
								A2(
								$author$project$Main$when,
								$author$project$TaskItem$hasSubtasks(taskItem),
								$author$project$Main$subtasksView(
									$author$project$Card$subtasks(card))),
								A2(
								$author$project$Main$when,
								$author$project$TaskItem$hasNotes(taskItem),
								$author$project$Main$notesView(
									$author$project$Card$notesId(card))),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('card-board-card-footer-area')
									]),
								_List_fromArray(
									[
										A2(
										$author$project$Main$when,
										$author$project$TaskItem$isDated(taskItem),
										$author$project$Main$taskDueDate(
											$author$project$TaskItem$due(taskItem))),
										A2(
										$author$project$Main$cardActionButtons,
										taskItemId,
										$author$project$Card$editButtonId(card))
									]))
							]))
					])));
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm$html$Html$Keyed$ul = $elm$html$Html$Keyed$node('ul');
var $author$project$Main$column = F3(
	function (timeWithZone, title, cards) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-column')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-column-header')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						])),
					A2(
					$elm$html$Html$Keyed$ul,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-column-list')
						]),
					A2(
						$elm$core$List$map,
						$author$project$Main$cardView(timeWithZone),
						cards))
				]));
	});
var $elm$html$Html$Attributes$hidden = $elm$html$Html$Attributes$boolProperty('hidden');
var $author$project$Main$panelView = F3(
	function (timeWithZone, index, panel) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-panel'),
					$elm$html$Html$Attributes$hidden(true)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-columns')
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var n = _v0.a;
							var cs = _v0.b;
							return A3($author$project$Main$column, timeWithZone, n, cs);
						},
						A3($author$project$Panel$columns, timeWithZone, index, panel)))
				]));
	});
var $author$project$Main$selectedPanelView = F3(
	function (timeWithZone, index, panel) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-panel')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-columns')
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var n = _v0.a;
							var cs = _v0.b;
							return A3($author$project$Main$column, timeWithZone, n, cs);
						},
						A3($author$project$Panel$columns, timeWithZone, index, panel)))
				]));
	});
var $author$project$SafeZipper$length = function (zipper) {
	if (zipper.$ === 'EmptyZipper') {
		return 0;
	} else {
		var b = zipper.a;
		var c = zipper.b;
		var a = zipper.c;
		return ($elm$core$List$length(b) + 1) + $elm$core$List$length(a);
	}
};
var $author$project$Panels$length = function (_v0) {
	var config = _v0.a;
	return $author$project$SafeZipper$length(config);
};
var $author$project$Main$selectedTabHeader = F2(
	function (index, title) {
		return A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-tab-title is-active')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-tabs-inner')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						]))
				]));
	});
var $author$project$Main$TabSelected = function (a) {
	return {$: 'TabSelected', a: a};
};
var $author$project$Main$tabHeaderClass = F2(
	function (currentIndex, index) {
		if (currentIndex.$ === 'Just') {
			var i = currentIndex.a;
			return _Utils_eq(index, i - 1) ? ' is-before-active' : (_Utils_eq(index, i + 1) ? ' is-after-active' : '');
		} else {
			return '';
		}
	});
var $author$project$Main$tabHeader = F3(
	function (currentIndex, index, title) {
		var headerClass = A2($author$project$Main$tabHeaderClass, currentIndex, index);
		return A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-tab-title' + headerClass),
					$elm$html$Html$Events$onClick(
					$author$project$Main$TabSelected(index))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-tabs-inner')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						]))
				]));
	});
var $author$project$Panels$tabTitle = F2(
	function (_v0, config) {
		if (config.$ === 'DateBoardConfig') {
			var dateBoardConfig = config.a;
			return dateBoardConfig.title;
		} else {
			var tagBoardConfig = config.a;
			return tagBoardConfig.title;
		}
	});
var $author$project$Panels$tabTitles = function (_v0) {
	var configs = _v0.a;
	return A3($author$project$SafeZipper$indexedMapSelectedAndRest, $author$project$Panels$tabTitle, $author$project$Panels$tabTitle, configs);
};
var $author$project$Main$tabHeaders = F2(
	function (currentIndex, panels) {
		var tabs = $author$project$SafeZipper$toList(
			A3(
				$author$project$SafeZipper$indexedMapSelectedAndRest,
				$author$project$Main$selectedTabHeader,
				$author$project$Main$tabHeader(currentIndex),
				$author$project$Panels$tabTitles(panels)));
		var beforeHeaderClass = A2($author$project$Main$tabHeaderClass, currentIndex, -1);
		var beforeFirst = A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-pre-tabs' + beforeHeaderClass)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-tabs-inner')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('')
						]))
				]));
		var afterHeaderClass = A2(
			$author$project$Main$tabHeaderClass,
			currentIndex,
			$author$project$Panels$length(panels));
		var afterLast = A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board-post-tabs' + afterHeaderClass)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-tabs-inner')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('')
						]))
				]));
		return A2(
			$elm$core$List$cons,
			beforeFirst,
			A2(
				$elm$core$List$append,
				tabs,
				_List_fromArray(
					[afterLast])));
	});
var $author$project$Main$view = function (model) {
	var _v0 = model.taskList;
	if (_v0.$ === 'Loaded') {
		var taskList = _v0.a;
		var panels = A2($author$project$Panels$init, model.boardConfigs, taskList);
		var currentIndex = $author$project$Panels$currentIndex(panels);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-board')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-board-container')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$ul,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-board-tab-list')
								]),
							A2($author$project$Main$tabHeaders, currentIndex, panels)),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-board-panels')
								]),
							$author$project$SafeZipper$toList(
								A3(
									$author$project$SafeZipper$indexedMapSelectedAndRest,
									$author$project$Main$selectedPanelView(model.timeWithZone),
									$author$project$Main$panelView(model.timeWithZone),
									$author$project$Panels$panels(panels))))
						]))
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)},'Worker':{'Worker':{'init':$author$project$Worker$Worker$main($elm$json$Json$Decode$value)(0)}}});}(this));