#MAIN := _build/default/examples/example_strict.exe
MAIN := _build/default/examples/strict_lwt/example_lwt_strict.exe

default:
	jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	@jbuilder clean

examples: clean
	@jbuilder build examples/strict_lwt/example_lwt_strict.exe
	@jbuilder build examples/example_strict.exe


run: examples
	@$(MAIN)


.PHONY: default install uninstall reinstall clean examples
