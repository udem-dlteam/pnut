.PHONY: \
	pnut-sh pnut-sh.sh pnut-sh-bootstrapped.sh pnut-exe pnut-exe.sh pnut-exe-bootstrapped \
	install uninstall clean \
	test-sh test-i386-linux test-x86_64-linux test-x86_64-mac \
	pnut-artifact-x86 pnut-artifact-arm \
	bootstrap-pnut-sh bootstrap-pnut-exe-script bootstrap-pnut-exe \
	bootstrap-pnut-exe-no-shell bootstrap-pnut-sh-with-pnut-exe

BUILD_DIR = build

# PNUT_BUILD_OPT can be used to pass additional compilation flags to pnut
ifeq ($(CFLAGS),)
	CFLAGS = -std=c99
endif

# Default: use 'env time -p' to measure time
ifeq ($(TIMEC),)
	TIMEC = env time -p
endif

# Bootstrap targets with integrated options
BOOTSTRAP_SHELL ?= /bin/sh

# Targets for pnut-exe: auto, Linux.i386, Linux.x86_64, Darwin.x86_64, Darwin.arm64
TARGET ?= auto

BUILD_OPT_EXE.Linux.i386 = 	 	-Dtarget_i386_linux
BUILD_OPT_EXE.Linux.x86_64 = 	-Dtarget_x86_64_linux
BUILD_OPT_EXE.Darwin.x86_64 = -Dtarget_x86_64_mac
BUILD_OPT_EXE.Darwin.arm64 = 	-Dtarget_x86_64_mac # no arm64 backend yet, x86 is emulated on ARM Macs

# Handle custom target selection
ifeq ($(TARGET),auto)
  # Detect the operating system and architecture
  UNAME_S := $(shell uname -s)
  UNAME_M := $(shell uname -m)
	TARGET := $(UNAME_S).$(UNAME_M)
endif

# Set the build options for the current operating system and architecture
BUILD_OPT_EXE = $(BUILD_OPT_EXE.$(TARGET))
# If targetting MacOS, disable EXE_ONE_PASS because not supported.
ifeq ($(TARGET),Darwin.x86_64)
	EXE_ONE_PASS = 0
endif
ifeq ($(TARGET),Darwin.arm64)
	EXE_ONE_PASS = 0
endif
# Error if unknown target
ifeq ($(BUILD_OPT_EXE),)
$(error Unknown target specified: $(TARGET). Supported targets are: Linux.i386, Linux.x86_64, Darwin.x86_64, Darwin.arm64)
endif

############################### General options ################################
# Bootstrap script options that can be passed via make variables
# Examples:
#		make pnut-sh.sh  MINIMAL=1 							 # Build minimal pnut-sh.sh
#		make pnut-sh.sh  MINIMAL=1 SH_ANNOTATE=1 # Build minimal annotated pnut-sh.sh
#		make pnut-exe TARGET=Linux.x86_64 EXE_ONE_PASS=1 # Build exe for Linux x86_64 with one-pass generator

# Include only features used to bootstrap pnut
MINIMAL 				?= 0
# Better error messages and more complete command line options
NICE_UX 				?= 0
# Allow reading input from stdin when no input file is specified
STDIN_INPUT 		?= 0

############################# Development options ##############################
# Enables safe mode with additional runtime checks
SAFE 					  ?= 0
# Print memory usage statistics after execution
STATS 					?= 0

########################### pnut-sh specific options ###########################
# Generate faster shell code that's less readable
SH_FAST 			 	?= 0
# Annotate generated shell code with comments
SH_ANNOTATE    	?= 0
# Use compact runtime library in shell scripts
SH_COMPACT_RT  	?= 0
# Shell scripts count memory usage (for development/debugging)
SH_PROFILE_MEM 	?= 0

########################## pnut-exe specific options ###########################
# Use one-pass code generator (default enabled)
EXE_ONE_PASS 		?= 1

# Bootstrap flags
BOOTSTRAP_FLAGS =
ifeq ($(MINIMAL),1)
  BOOTSTRAP_FLAGS += -DPNUT_BOOTSTRAP
else
# Default is NICE_UX enabled
	NICE_UX = 1
endif
ifeq ($(NICE_UX),1)
  BOOTSTRAP_FLAGS += -DNICE_UX
endif
ifeq ($(STDIN_INPUT),1)
	BOOTSTRAP_FLAGS += -DSUPPORT_STDIN_INPUT
endif

ifeq ($(SAFE),1)
  BOOTSTRAP_FLAGS += -DSAFE_MODE
endif
ifeq ($(STATS),1)
  BOOTSTRAP_FLAGS += -DPRINT_MEMORY_STATS
endif

ifeq ($(SH_FAST),1)
  BOOTSTRAP_FLAGS += -DSH_SAVE_VARS_WITH_SET
endif
ifeq ($(SH_ANNOTATE),1)
	BOOTSTRAP_FLAGS += -DSH_INCLUDE_C_CODE
endif
ifeq ($(SH_COMPACT_RT),1)
	BOOTSTRAP_FLAGS += -DRT_COMPACT
endif
ifeq ($(SH_PROFILE_MEM),1)
	BOOTSTRAP_FLAGS += -DSH_PROFILE_MEMORY
endif

ifeq ($(EXE_ONE_PASS),1)
  BOOTSTRAP_FLAGS += -DONE_PASS_GENERATOR
endif

BUILD_OPT_SH = -Dtarget_sh $(PNUT_BUILD_OPT) $(BOOTSTRAP_FLAGS)
BUILD_OPT_EXE += $(BOOTSTRAP_FLAGS)

build:
	@mkdir -p $(BUILD_DIR)

pnut-sh: build pnut.c sh.c sh-runtime.c
	$(CC) $(CFLAGS) $(BUILD_OPT_SH) pnut.c -o $(BUILD_DIR)/pnut-sh

pnut-sh.sh: pnut-sh
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh.sh
	@chmod +x $(BUILD_DIR)/pnut-sh.sh

pnut-sh-bootstrapped.sh: pnut-sh.sh
	$(BOOTSTRAP_SHELL) $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-bootstrapped.sh
	@chmod +x $(BUILD_DIR)/pnut-sh-bootstrapped.sh
	diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-bootstrapped.sh

pnut-exe: build pnut.c x86.c exe.c elf.c mach-o.c
	$(CC) $(CFLAGS) $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe

pnut-exe.sh: pnut-sh pnut.c x86.c exe.c elf.c mach-o.c
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe.sh
	@chmod +x $(BUILD_DIR)/pnut-exe.sh

pnut-exe-bootstrapped: pnut-exe
	$(BUILD_DIR)/pnut-exe $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe-bootstrapped
	@chmod +x $(BUILD_DIR)/pnut-exe-bootstrapped

kit/bintools.c:
	./utils/process-includes.sh kit/bintools/bintools-base.c > kit/bintools.c

kit/jammed.sh: kit/bintools.c
	./kit/make-jammed.sh

install: pnut-sh pnut-sh.sh
	cp $(BUILD_DIR)/pnut-sh $(DESTDIR)$(PREFIX)/bin/pnut
	cp $(BUILD_DIR)/pnut-sh.sh $(DESTDIR)$(PREFIX)/bin/pnut-sh.sh

install-pnut-exe: pnut-exe pnut-exe.sh
	cp $(BUILD_DIR)/pnut-exe $(DESTDIR)$(PREFIX)/bin/pnut-exe
	cp $(BUILD_DIR)/pnut-exe.sh $(DESTDIR)$(PREFIX)/bin/pnut-exe.sh

uninstall:
	$(RM) $(DESTDIR)$(PREFIX)/bin/pnut
	$(RM) $(DESTDIR)$(PREFIX)/bin/pnut-sh.sh
	$(RM) $(DESTDIR)$(PREFIX)/bin/pnut-exe
	$(RM) $(DESTDIR)$(PREFIX)/bin/pnut-exe.sh

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

define BOOTSTRAP_HELP
The following recipes perform some steps of the complete pnut bootstrap
process to allow each part to be tested individually. The **bootstrap test**
is used to verify that the step output is in a good enough state to recompile
and reproduce itself bit-for-bit.

The bootstrap steps are:
1) Bootstrap pnut-sh.sh from pnut-sh.sh:  bootstrap-pnut-sh
2) Bootstrap pnut-exe.sh from pnut-sh.sh: bootstrap-pnut-exe-script
3) Bootstrap pnut-exe from pnut-exe.sh:   bootstrap-pnut-exe
4) Bootstrap pnut-exe from pnut-exe:      bootstrap-pnut-exe-no-shell

In principle, these steps depend on the output of the previous step. However,
to speed up testing, the bootstrap compiler of each step is produced using the
system C compiler (gcc/clang) rather than the output of the previous step. To
ensure this does not invalidate the bootstrap process, each step compares its
output to the output obtained using the system compiler. This ensures that the
output and input of adjacent steps don't diverge.

For completeness, an additional recipe bootstraps pnut-sh from pnut-exe.
endef

export BOOTSTRAP_HELP
bootstrap-help:
	@echo "$$BOOTSTRAP_HELP"

# Bootstrap pnut-sh with pnut-sh.sh (obtained using $(CC)).
bootstrap-pnut-sh: pnut-sh.sh
	@echo "Bootstrapping pnut-sh.sh from pnut-sh.sh..."
	$(TIMEC) $(BOOTSTRAP_SHELL) $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-bootstrapped.sh
	@if ! diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-bootstrapped.sh >/dev/null 2>&1; then \
		echo "FAILURE: Bootstrap scripts differ"; \
		exit 1; \
	fi
	@echo "Success!"

# Bootstrap pnut-exe.sh with pnut-sh.sh (obtained using $(CC)).
bootstrap-pnut-exe-script: pnut-sh.sh pnut-exe.sh
	@echo "Bootstrapping pnut-exe.sh from pnut-sh.sh..."
	$(TIMEC) $(BOOTSTRAP_SHELL) $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe-bootstrapped.sh
	@if ! diff $(BUILD_DIR)/pnut-exe.sh $(BUILD_DIR)/pnut-exe-bootstrapped.sh >/dev/null 2>&1; then \
		echo "FAILURE: Bootstrap scripts differ"; \
		exit 1; \
	fi
	@echo "Success!"

# Bootstrap pnut-exe from pnut-exe (by $(CC)).
bootstrap-pnut-exe: pnut-exe.sh pnut-exe-bootstrapped
	@echo "Bootstrapping pnut-exe from pnut-exe.sh..."
	$(TIMEC) $(BOOTSTRAP_SHELL) $(BUILD_DIR)/pnut-exe.sh $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe-bootstrapped-again
	@if ! diff $(BUILD_DIR)/pnut-exe-bootstrapped $(BUILD_DIR)/pnut-exe-bootstrapped-again >/dev/null 2>&1; then \
		echo "FAILURE: Bootstrap executables differ"; \
		exit 1; \
	fi
	@echo "Success!"

bootstrap-pnut-exe-no-shell: pnut-exe-bootstrapped
	@echo "Bootstrapping pnut-exe from pnut-exe..."
	@$(RM) $(BUILD_DIR)/pnut-exe-bootstrapped-again # MacOS behaves differently if the file exists
	$(TIMEC) $(BUILD_DIR)/pnut-exe-bootstrapped $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe-bootstrapped-again
	@if ! diff $(BUILD_DIR)/pnut-exe-bootstrapped $(BUILD_DIR)/pnut-exe-bootstrapped-again >/dev/null 2>&1; then \
		echo "FAILURE: Bootstrap executables differ"; \
		exit 1; \
	fi
	@echo "Success!"

# For completeness, bootstrap pnut-sh from pnut-exe, then recompile pnut-sh from
# the bootstrapped pnut-sh.
bootstrap-pnut-sh-with-pnut-exe: pnut-exe-bootstrapped pnut-sh.sh
	@echo "Bootstrapping pnut-sh from pnut-exe..."
	@$(RM) $(BUILD_DIR)/pnut-sh-from-pnut-exe # MacOS behaves differently if the file exists
	$(TIMEC) $(BUILD_DIR)/pnut-exe $(BUILD_OPT_SH) pnut.c -o $(BUILD_DIR)/pnut-sh-from-pnut-exe
	@chmod +x $(BUILD_DIR)/pnut-sh-from-pnut-exe
	$(BUILD_DIR)/pnut-sh-from-pnut-exe $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-from-pnut-exe-again.sh
	@if ! diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-from-pnut-exe-again.sh >/dev/null 2>&1; then \
		echo "FAILURE: Bootstrap scripts differ"; \
		exit 1; \
	fi
	@echo "Success!"
