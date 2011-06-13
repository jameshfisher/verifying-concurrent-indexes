def gcd(a,b):
	while a: a, b = b%a, a
	return b

assert gcd(28,14) == 14
assert gcd(29,1) == 1
assert gcd(1,29) == 1
assert gcd(7,7) == 7
assert gcd(5, 1000) == 5
assert gcd(15, 18) == 3
assert gcd(34, 58) == 2
assert gcd(0,7) == 7
assert gcd(7,0) == 7
