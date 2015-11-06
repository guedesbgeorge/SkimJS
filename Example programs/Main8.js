// Função sem retorno incrementando uma variável global.
function incrementGlobalVariableA() {
	++a;
}
var a = 0;

for(var i = 1;;++i) {
	if (i > 3) {
		break;
	} else {
		incrementGlobalVariableA();
	}
}

a;