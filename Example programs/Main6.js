function fatorial(n) {
	if (n == 0) {
		return 1;
	} else {
		return (n*fatorial(n - 1));
	}
}

var a = fatorial(6);
a;