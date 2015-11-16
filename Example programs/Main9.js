// Função que eleva um número a outro. Testando variáveis automaticamente globais e locais.
function pow(value, power) {
	g = "I'm a automatic global variable!";
	var ans = 1;
	for(var i = power; i >= 1; --i) {
		ans = ans*value;
	}
	return ans;
}

var a = pow(2, 4);
a;