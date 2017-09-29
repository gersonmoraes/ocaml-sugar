#MAIN := _build/default/examples/example_strict.exe
MAIN := _build/default/examples/strict_lwt/example_lwt_strict.exe

default:
	jbuilder build @install

lint:
	jbuilder build @install --dev

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	@jbuilder clean

docs:
	@jbuilder build @doc

examples: clean
	@jbuilder build examples/strict_lwt/example_lwt_strict.exe
	@jbuilder build examples/example_strict.exe
	@jbuilder build examples/example_basic.exe


run: examples
	@$(MAIN)


.PHONY: default install uninstall reinstall clean examples
