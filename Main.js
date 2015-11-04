var b = 0;
function fib(a) {
	b = 3;
	var e = 1;
	if(a == 1) {
		return 1;
	} 
	if (a == 0) {
		return 0;
	} else {
		return (fib(a - 1) + fib (a - 2));
	}
}

var z = fib(8);
z;