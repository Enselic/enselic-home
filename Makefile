all: emacs-files.zip

ELISPFILES = Elisp/psvn.el              \
             Elisp/grep+.el             \
             Elisp/browse-kill-ring.el  \
             Elisp/enspro.el            \
             Elisp/gtk-doc.el           \
             Elisp/session.el           \
             Elisp/whitespace.el        \
             Elisp/ucm.el               \
             Elisp/vc-clearcase.el      \
             Elisp/vc-clearcase-auto.el \
             Elisp/etags-select.el

emacs-files.zip: .emacs $(ELISPFILES)
	mkdir -p emacs-files/Elisp
	cp .emacs emacs-files/.emacs
	cp $(ELISPFILES) emacs-files/Elisp
	zip -r $@ emacs-files

clean:
	rm emacs-files.zip
	rm -rf emacs-files
