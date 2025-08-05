BUILD_DIR = build

BUILD_OPT_SH = -DRELEASE_PNUT_SH $(BUILD_OPT)

BUILD_OPT_EXE.Linux.i386 = -DRELEASE_PNUT_i386_linux $(BUILD_OPT)
BUILD_OPT_EXE.Linux.x86_64 = -DRELEASE_PNUT_x86_64_linux $(BUILD_OPT)
BUILD_OPT_EXE.Darwin.x86_64 = -DRELEASE_PNUT_x86_64_mac $(BUILD_OPT)
BUILD_OPT_EXE.Darwin.arm64 = -DRELEASE_PNUT_x86_64_mac $(BUILD_OPT) # no arm64 backend yet, x86 is emulated on ARM Macs

# Shell for building
BUILD_SHELL=sh

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

.PHONY: default install uninstall clean test-sh test-i386-linux test-x86_64-linux test-x86_64-mac

default: $(BUILD_DIR)/pnut-sh $(BUILD_DIR)/pnut-exe

all: $(BUILD_DIR)/pnut-sh \
	$(BUILD_DIR)/pnut-sh.sh \
	$(BUILD_DIR)/pnut-sh-bootstrapped.sh \
	$(BUILD_DIR)/pnut-exe \
	$(BUILD_DIR)/pnut-exe.sh \
	$(BUILD_DIR)/pnut-exe-bootstrapped

$(BUILD_DIR)/config.mk:
	./configure

include $(BUILD_DIR)/config.mk

PNUT_SH_DEPS=pnut.c debug.c sh.c sh-runtime.c env.c
PNUT_EXE_DEPS= pnut.c debug.c x86.c exe.c env.c elf.c mach-o.c

$(BUILD_DIR)/pnut-sh: $(PNUT_SH_DEPS)
	$(CC) $(CFLAGS) $(BUILD_OPT_SH) pnut.c -o $(BUILD_DIR)/pnut-sh

$(BUILD_DIR)/pnut-sh.sh: $(BUILD_DIR)/pnut-sh $(PNUT_SH_DEPS)
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh.sh
	chmod +x $(BUILD_DIR)/pnut-sh.sh

$(BUILD_DIR)/pnut-sh-bootstrapped.sh: $(BUILD_DIR)/pnut-sh.sh $(PNUT_SH_DEPS)
	$(BUILD_SHELL) $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-bootstrapped.sh
	diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-bootstrapped.sh

$(BUILD_DIR)/pnut-exe: $(PNUT_EXE_DEPS)
	$(CC) $(CFLAGS) $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe

$(BUILD_DIR)/pnut-exe.sh: $(BUILD_DIR)/pnut-sh $(PNUT_EXE_DEPS)
	$(BUILD_DIR)/pnut-sh $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe.sh
	chmod +x $(BUILD_DIR)/pnut-exe.sh

$(BUILD_DIR)/pnut-exe-bootstrapped: $(BUILD_DIR)/pnut-exe $(PNUT_EXE_DEPS)
	$(BUILD_DIR)/pnut-exe $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe-bootstrapped

install: $(BUILD_DIR)/pnut-sh $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/config.mk
	install -D $(BUILD_DIR)/pnut-sh $(DESTDIR)$(prefix)/bin/pnut
	install -D $(BUILD_DIR)/pnut-sh.sh $(DESTDIR)$(prefix)/bin/pnut.sh

uninstall:
	$(RM) $(DESTDIR)$(prefix)/bin/pnut
	$(RM) $(DESTDIR)$(prefix)/bin/pnut.sh

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
