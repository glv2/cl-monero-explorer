LISP ?= sbcl

all: bin/monero-explorer-gtk bin/monero-explorer-ltk bin/monero-explorer-mcclim bin/monero-explorer-nodgui bin/monero-explorer-qt

bin/monero-explorer-gtk: monero-explorer-gtk.asd src/lookup.lisp src/gui-gtk.lisp
	mkdir -p "bin"
	${LISP} --eval "(asdf:make \"monero-explorer-gtk\")" --eval "(uiop:quit)"
	mv "monero-explorer-gtk" "bin/"

bin/monero-explorer-ltk: monero-explorer-ltk.asd src/lookup.lisp src/gui-ltk.lisp
	mkdir -p "bin"
	${LISP} --eval "(asdf:make \"monero-explorer-ltk\")" --eval "(uiop:quit)"
	mv "monero-explorer-ltk" "bin/"

bin/monero-explorer-mcclim: monero-explorer-mcclim.asd src/lookup.lisp src/gui-mcclim.lisp
	mkdir -p "bin"
	${LISP} --eval "(asdf:make \"monero-explorer-mcclim\")" --eval "(uiop:quit)"
	mv "monero-explorer-mcclim" "bin/"

bin/monero-explorer-nodgui: monero-explorer-nodgui.asd src/lookup.lisp src/gui-nodgui.lisp
	mkdir -p "bin"
	${LISP} --eval "(asdf:make \"monero-explorer-nodgui\")" --eval "(uiop:quit)"
	mv "monero-explorer-nodgui" "bin/"

bin/monero-explorer-qt: monero-explorer-qt.asd src/lookup.lisp src/gui-qt.lisp
	mkdir -p "bin"
	${LISP} --eval "(asdf:make \"monero-explorer-qt\")" --eval "(uiop:quit)"
