function fat(a) {
	if(a == 1) {
		return 1;
	} else {
		return (a*(fat(a - 1)));
	}
}

var z = fat(5);
z;