.PHONY: help pnut-sh pnut-sh.sh pnut-sh-bootstrapped.sh pnut-exe pnut-exe.sh pnut-exe-bootstrapped bootstrap-sh bootstrap-exe install uninstall clean test-sh test-i386-linux test-x86_64-linux test-x86_64-mac

BUILD_DIR = build

# Default target shows help
.DEFAULT_GOAL := help

# Bootstrap script options that can be passed via make variables
# Examples:
#   make pnut-sh FAST=1 SAFE=1                    # Enable fast and safe mode for shell backend
#   make pnut-exe MINIMAL=1 STATS=1               # Enable minimal pnut and memory stats for exe backend
#   make pnut-exe BACKEND=i386_linux ONE_PASS=1   # Use i386_linux backend with one-pass generator
FAST ?= 0
SAFE ?= 0
MINIMAL ?= 0
ONE_PASS ?= 0
STATS ?= 0
NICE_UX ?= 0
BACKEND ?= auto

# Bootstrap flags
BOOTSTRAP_FLAGS =
ifeq ($(FAST),1)
  BOOTSTRAP_FLAGS += -DSH_SAVE_VARS_WITH_SET
endif
ifeq ($(SAFE),1)
  BOOTSTRAP_FLAGS += -DSAFE_MODE
endif
ifeq ($(MINIMAL),1)
  BOOTSTRAP_FLAGS += -DPNUT_BOOTSTRAP
endif
ifeq ($(ONE_PASS),1)
  BOOTSTRAP_FLAGS += -DONE_PASS_GENERATOR
endif
ifeq ($(STATS),1)
  BOOTSTRAP_FLAGS += -DPRINT_MEMORY_STATS
endif
ifeq ($(NICE_UX),1)
  BOOTSTRAP_FLAGS += -DNICE_UX
endif

BUILD_OPT_SH = -Dsh $(BUILD_OPT) $(BOOTSTRAP_FLAGS)

BUILD_OPT_EXE.Linux.i386 = -Dtarget_i386_linux $(BUILD_OPT) $(BOOTSTRAP_FLAGS)
BUILD_OPT_EXE.Linux.x86_64 = -Dtarget_x86_64_linux $(BUILD_OPT) $(BOOTSTRAP_FLAGS)
BUILD_OPT_EXE.Darwin.x86_64 = -Dtarget_x86_64_mac $(BUILD_OPT) $(BOOTSTRAP_FLAGS)
BUILD_OPT_EXE.Darwin.arm64 = -Dtarget_x86_64_mac $(BUILD_OPT) $(BOOTSTRAP_FLAGS) # no arm64 backend yet, x86 is emulated on ARM Macs

# Handle custom backend selection
ifeq ($(BACKEND),i386_linux)
  BUILD_OPT_EXE = $(BUILD_OPT_EXE.Linux.i386)
else ifeq ($(BACKEND),x86_64_linux)
  BUILD_OPT_EXE = $(BUILD_OPT_EXE.Linux.x86_64)
else ifeq ($(BACKEND),x86_64_mac)
  BUILD_OPT_EXE = $(BUILD_OPT_EXE.Darwin.x86_64)
else ifeq ($(BACKEND),auto)
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
else
  $(error Unknown backend: $(BACKEND). Supported backends: auto, i386_linux, x86_64_linux, x86_64_mac)
endif

pnut-sh: pnut.c sh.c sh-runtime.c ## Compile pnut shell backend
	mkdir -p $(BUILD_DIR)
	gcc $(BUILD_OPT_SH) pnut.c -o $(BUILD_DIR)/pnut-sh

pnut-sh.sh: pnut-sh ## Generate pnut shell script
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh.sh
	chmod +x $(BUILD_DIR)/pnut-sh.sh

pnut-sh-bootstrapped.sh: pnut-sh ## Bootstrap shell script (twice-compiled verification)
	$$SHELL $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-bootstrapped.sh
	diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-bootstrapped.sh

pnut-exe: pnut.c x86.c exe.c elf.c mach-o.c ## Compile pnut executable backend
	mkdir -p $(BUILD_DIR)
	gcc $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe

pnut-exe.sh: pnut-sh pnut.c x86.c exe.c elf.c mach-o.c ## Generate pnut executable as shell script
	./$(BUILD_DIR)/pnut-sh $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe.sh
	chmod +x $(BUILD_DIR)/pnut-exe.sh

pnut-exe-bootstrapped: pnut-exe ## Bootstrap executable (twice-compiled verification)
	$(BUILD_DIR)/pnut-exe $(BUILD_OPT_EXE) pnut.c > $(BUILD_DIR)/pnut-exe-bootstrapped

kit/bintools.c: ## Generate bintools.c from includes
	./utils/process-includes.sh kit/bintools/bintools-base.c > kit/bintools.c

kit/jammed.sh: kit/bintools.c ## Generate jammed shell script
	./kit/make-jammed.sh

install: pnut-sh pnut-sh.sh ## Install pnut binaries to /usr/local/bin
	sudo cp $(BUILD_DIR)/pnut-sh /usr/local/bin/pnut
	sudo cp $(BUILD_DIR)/pnut-sh.sh /usr/local/bin/pnut.sh

uninstall: ## Remove installed pnut binaries
	sudo $(RM) /usr/local/bin/pnut
	sudo $(RM) /usr/local/bin/pnut.sh

clean: ## Clean all build artifacts and test outputs
	$(RM) -r $(BUILD_DIR)
	# Recursively remove .exe files from the tests directory
	find tests -name "*.exe" -exec $(RM) {} \;
	find tests -name "*.sh" -exec $(RM) {} \;
	find tests -name "*.err" -exec $(RM) {} \;
	find tests -name "*.output" -exec $(RM) {} \;

test-sh: ## Run shell tests
	./run-tests.sh "sh"

test-i386-linux: ## Run i386 Linux tests
	./run-tests.sh "i386_linux"

test-x86_64-linux: ## Run x86_64 Linux tests
	./run-tests.sh "x86_64_linux"

test-x86_64-mac: ## Run x86_64 Mac tests
	./run-tests.sh "x86_64_mac"

pnut-artifact-x86: ## Build Docker artifact for x86 platform
	docker build -t pnut-artifact-x86 . --build-arg PNUT_SOURCE=clone --platform linux/amd64

pnut-artifact-arm: ## Build Docker artifact for ARM platform
	docker build -t pnut-artifact-arm . --build-arg PNUT_SOURCE=clone --platform linux/arm64

# Bootstrap targets with integrated options
BOOTSTRAP_SHELL ?= $(SHELL)

bootstrap-sh: pnut-sh.sh ## Full shell bootstrap process with configurable options
	@echo "Starting shell bootstrap with options: FAST=$(FAST) SAFE=$(SAFE) MINIMAL=$(MINIMAL) NICE_UX=$(NICE_UX)"
	@if [ "$(BOOTSTRAP_SHELL)" = "all" ]; then \
		set +e; \
		for shell in dash ksh bash yash mksh zsh; do \
			echo "Bootstrap with $$shell"; \
			env time $$shell $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-twice-bootstrapped.sh; \
			diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-twice-bootstrapped.sh; \
		done; \
	else \
		echo "Bootstrap with $(BOOTSTRAP_SHELL)"; \
		env time $(BOOTSTRAP_SHELL) $(BUILD_DIR)/pnut-sh.sh $(BUILD_OPT_SH) pnut.c > $(BUILD_DIR)/pnut-sh-twice-bootstrapped.sh; \
		diff $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-twice-bootstrapped.sh; \
		wc pnut.c $(BUILD_DIR)/pnut-sh.sh $(BUILD_DIR)/pnut-sh-twice-bootstrapped.sh; \
	fi

bootstrap-exe: pnut-exe ## Full executable bootstrap process with configurable options
	@echo "Starting executable bootstrap with options: BACKEND=$(BACKEND) FAST=$(FAST) SAFE=$(SAFE) MINIMAL=$(MINIMAL) ONE_PASS=$(ONE_PASS) STATS=$(STATS) NICE_UX=$(NICE_UX)"
	$(BUILD_DIR)/pnut-exe $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe-bootstrapped-1
	$(BUILD_DIR)/pnut-exe-bootstrapped-1 $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe-bootstrapped-2
	@if diff $(BUILD_DIR)/pnut-exe-bootstrapped-1 $(BUILD_DIR)/pnut-exe-bootstrapped-2 >/dev/null 2>&1; then \
		echo "SUCCESS: Bootstrap executables are identical"; \
	else \
		echo "FAILURE: Bootstrap executables differ"; \
		exit 1; \
	fi

help: ## Show this help message
	@echo "Pnut Makefile - Available targets and options:"
	@echo ""
	@echo "Targets:"
	@sed -ne '/@sed/!s/## //p' $(MAKEFILE_LIST)
	@echo ""
	@echo "Bootstrap options (set to 1 to enable):"
	@echo "  FAST=1               - Enable fast mode (-DSH_SAVE_VARS_WITH_SET)"
	@echo "  SAFE=1               - Enable safe mode (-DSAFE_MODE)"
	@echo "  MINIMAL=1            - Enable minimal pnut (-DPNUT_BOOTSTRAP)"
	@echo "  ONE_PASS=1           - Enable one-pass generator (-DONE_PASS_GENERATOR)"
	@echo "  STATS=1              - Enable memory statistics (-DPRINT_MEMORY_STATS)"
	@echo "  NICE_UX=1            - Enable nice UX features (-DNICE_UX)"
	@echo ""
	@echo "Backend selection:"
	@echo "  BACKEND=auto         - Auto-detect backend (default)"
	@echo "  BACKEND=i386_linux   - Use i386 Linux backend"
	@echo "  BACKEND=x86_64_linux - Use x86_64 Linux backend"
	@echo "  BACKEND=x86_64_mac   - Use x86_64 Mac backend"
	@echo ""
	@echo "Shell selection:"
	@echo "  BOOTSTRAP_SHELL=\$$(SHELL) - Use current shell (default)"
	@echo "  BOOTSTRAP_SHELL=all      - Test all available shells"
	@echo "  BOOTSTRAP_SHELL=bash     - Use specific shell"
	@echo ""
	@echo "Examples:"
	@echo "  make pnut-sh FAST=1 SAFE=1"
	@echo "  make bootstrap-exe BACKEND=i386_linux MINIMAL=1"
	@echo "  make bootstrap-sh BOOTSTRAP_SHELL=all FAST=1"
	@echo "  make bootstrap-sh BOOTSTRAP_SHELL=bash SAFE=1"
	@echo ""
