function getMenores(list, pivo) {
	if(list == []) {
		return [];
	} else {
		if(head(list) < pivo) {
			return concat([head(list)], getMenores(tail(list), pivo));
		} else {
			return getMenores(tail(list), pivo);
		}
	}
}

function getMaiores(list, pivo) {
	if(list == []) {
		return [];
	} else {
		if(head(list) >= pivo) {
			return concat([head(list)], getMaiores(tail(list), pivo));
		} else {
			return getMaiores(tail(list), pivo);
		}
	}
}

function quicksort(list) {
	if (list == []) {
		return [];
	} else {
		return concat(quicksort(getMenores(tail(list), head(list))), [head(list)], quicksort(getMaiores(tail(list), head(list))));
	}
}

var desord = [10, 1, 5, 4, 4, -3, 7, 8];
var ord = quicksort(desord);
ord;