.PHONY: pnut-sh pnut-sh.sh pnut-sh-bootstrapped.sh pnut-exe pnut-exe.sh pnut-exe-bootstrapped install uninstall clean test-sh test-i386-linux test-x86_64-linux test-x86_64-mac

BUILD_DIR = build

BUILD_OPT_SH = -DRELEASE_PNUT_SH $(BUILD_OPT)

BUILD_OPT_EXE.Linux.i386 = -DRELEASE_PNUT_i386_linux $(BUILD_OPT)
BUILD_OPT_EXE.Linux.x86_64 = -DRELEASE_PNUT_x86_64_linux $(BUILD_OPT)
BUILD_OPT_EXE.Darwin.x86_64 = -DRELEASE_PNUT_x86_64_mac $(BUILD_OPT)
BUILD_OPT_EXE.Darwin.arm64 = -DRELEASE_PNUT_x86_64_mac $(BUILD_OPT) # no arm64 backend yet, x86 is emulated on ARM Macs

# Detect the operating system and architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
# Set the build options for the current operating system and architecture
BUILD_OPT_EXE = $(BUILD_OPT_EXE.$(UNAME_S).$(UNAME_M))
# If BUILD_OPT is empty, default to the Linux x86_64 build options
ifeq ($(BUILD_OPT_EXE),)
$(info !!!!! Defaulting to Linux x86_64 build options !!!!!)
BUILD_OPT_EXE = $(BUILD_OPT_EXE.Linux.x86_64)
endif

pnut-sh: pnut.c sh.c sh-runtime.c
	mkdir -p $(BUILD_DIR)
	gcc $(BUILD_OPT_SH) pnut.c -o $(BUILD_DIR)/pnut-sh

pnut-sh.sh: pnut-sh
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh.sh
	chmod +x $(BUILD_DIR)/pnut-sh.sh

pnut-sh-bootstrapped.sh: pnut-sh
	$$SHELL $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-bootstrapped.sh
	diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-bootstrapped.sh

pnut-exe: pnut.c x86.c exe.c elf.c mach-o.c
	mkdir -p $(BUILD_DIR)
	gcc $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe

pnut-exe.sh: pnut-sh pnut.c x86.c exe.c elf.c mach-o.c
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe.sh
	chmod +x $(BUILD_DIR)/pnut-exe.sh

pnut-exe-bootstrapped: pnut-exe
	$(BUILD_DIR)/pnut-exe $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe-bootstrapped

install: pnut-sh pnut-sh.sh
	sudo cp $(BUILD_DIR)/pnut-sh /usr/local/bin/pnut
	sudo cp $(BUILD_DIR)/pnut-sh.sh /usr/local/bin/pnut.sh

uninstall:
	sudo $(RM) /usr/local/bin/pnut
	sudo $(RM) /usr/local/bin/pnut.sh

clean:
	$(RM) -r $(BUILD_DIR)
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

pnut-artifact-x86:
	docker build -t pnut-artifact-x86 . --build-arg PNUT_SOURCE=clone --platform linux/amd64

pnut-artifact-arm:
	docker build -t pnut-artifact-arm . --build-arg PNUT_SOURCE=clone --platform linux/arm64
