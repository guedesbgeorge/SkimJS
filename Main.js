function len(list) {
	if (list == []) {
		return 0;
	} else {
		return (1 + len(tail(list)));
	}
}

var a = [1, 2, 3, 4];
var b = [5, 6];
var c = concat(a, b);

var d = len(c);
d;