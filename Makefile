.PHONY: pnut-sh pnut.sh pnut-bootstrapped.sh install clean test

BUILD_DIR = build

BUILD_OPT_SH = -DRELEASE_PNUT_SH $(BUILD_OPT)

pnut-sh: pnut.c sh.c sh-runtime.c
	mkdir -p $(BUILD_DIR)
	gcc $(BUILD_OPT_SH) pnut.c -o $(BUILD_DIR)/pnut-sh

pnut.sh: pnut-sh
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut.sh
	chmod +x $(BUILD_DIR)/pnut.sh

pnut-bootstrapped.sh: pnut-sh
	$$SHELL $(BUILD_DIR)/pnut.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-bootstrapped.sh
	diff $(BUILD_DIR)/pnut.sh $(BUILD_DIR)/pnut-bootstrapped.sh

install: pnut-sh pnut.sh
	sudo cp $(BUILD_DIR)/pnut-sh /usr/local/bin/pnut
	sudo cp $(BUILD_DIR)/pnut.sh /usr/local/bin/pnut.sh

uninstall:
	sudo $(RM) /usr/local/bin/pnut
	sudo $(RM) /usr/local/bin/pnut.sh

clean:
	$(RM) $(BUILD_DIR)/pnut-sh $(BUILD_DIR)/pnut.sh $(BUILD_DIR)/pnut-bootstrapped.shield
	# Recursively remove .exe files from the tests directory
	find tests -name "*.exe" -exec $(RM) {} \;
	find tests -name "*.sh" -exec $(RM) {} \;
	find tests -name "*.err" -exec $(RM) {} \;
	find tests -name "*.output" -exec $(RM) {} \;

test-sh:
	./run-tests.sh "sh"

test-i386-linux:
	./run-tests.sh "i386_linux"

test-x86_64-linux:
	./run-tests.sh "x86_64_linux"

test-x86_64-mac:
	./run-tests.sh "x86_64_mac"
