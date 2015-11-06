function power(value, power) {
	g = "I'm a automatic global variable!";
	var ans = 1;
	for(var i = power; i >= 1; --i) {
		ans = ans*value;
	}
	return ans;
}

var a = power(2, 4);
a;