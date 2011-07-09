SHELL=/bin/bash

htm:
	for v in `find . -name "*.d"`; do
		cat $v | ./mkhtml/mkhtml > html/$v.html
	done
