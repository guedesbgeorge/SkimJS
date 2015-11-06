var list1 = [1, 2, 3];
var list2 = [2*2, 4 + 1, 3 + 3]; // list2 = [3, 5, 6]

var a = head(list1); // a = 1
var b = tail(list1); // b = [2, 3]
var c = tail(list2); // c = [5, 6]

var d = concat(b, c); // d = [2, 3, 5, 6]
d;