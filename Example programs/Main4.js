var a = 3;

for(var i = 1; i <= 5;  i = i + 1) {
	if(i > 1) {
		if (i > 2) {
			break;
		}
	}
	++a;
}

a;