function len(list) {
	c = 1;
	if (list == []) {
		return 0;
	} else {
		return (1 + len(tail(list)));
	}
}

var b = [1, 2, 3, 4];
var c = [5, 6];
var g = concat(c, tail(b));
// g = [5, 6, 2, 3, 4]

var x = len(g) + c;
x;