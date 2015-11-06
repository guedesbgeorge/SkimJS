// Testando laço for sem inicialização e comando break dentro de um escopo de if.
var i = 1;
var a = 5;

for(; i < 5; ++i) {
	if (i == 3) {
		break;
	}
	++a;
}

a;