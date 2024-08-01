.PHONY: pnut-sh pnut.sh pnut-bootstrapped.sh install clean test

BUILD_DIR = build

BUILD_OPT_SH = -DRELEASE_PNUT_SH $(BUILD_OPT)

BUILD_OPT_EXE = -DRELEASE_PNUT_I386 $(BUILD_OPT)

pnut-sh: pnut.c sh.c sh-runtime.c
	mkdir -p $(BUILD_DIR)
	gcc $(BUILD_OPT_SH) pnut.c -o $(BUILD_DIR)/pnut-sh

pnut-exe: pnut.c x86.c exe.c elf.c
	mkdir -p $(BUILD_DIR)
	gcc $(BUILD_OPT_EXE) pnut.c -o $(BUILD_DIR)/pnut-exe

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
	$(RM) $(BUILD_DIR)/pnut-sh $(BUILD_DIR)/pnut.sh $(BUILD_DIR)/pnut-bootstrapped.sh
	$(RM) out.sh
	$(RM) tests/*.sh tests/*.err

# Test cc.sh
test:
	@echo "Running tests..."
	@for file in $(shell find tests -type f -name "*.c" | sort); do \
		filename=$$(basename $$file .c); \
		./cc.sh $$file > tests/$$filename.sh 2> tests/$$filename.err; \
		if [ $$? -eq 0 ]; then \
			chmod +x tests/$$filename.sh; \
			test_output=$$(./tests/$$filename.sh 2> tests/$$filename.err); \
			if [ $$? -eq 0 ]; then \
				if [ -f "tests/$$filename.golden" ]; then \
					diff_out=$$(echo $$test_output | diff - tests/$$filename.golden); \
					if [ $$? -eq 0 ]; then \
						echo "$$filename: ✅"; \
					else \
						echo "$$filename: ❌"; \
						echo "diff (output vs expected)"; \
						echo "$$diff_out"; \
					fi \
				else \
					$$(echo $$test_output > tests/$$filename.golden); \
					echo "$$filename: ❌ Generated golden file"; \
				fi \
			else \
				echo "$$filename: ❌ Failed to run: $$(cat tests/$$filename.err)"; \
			fi \
		else \
			echo "$$filename: ❌ Failed to compile: $$(cat tests/$$filename.err)"; \
		fi; \
		rm tests/$$filename.err; \
	done
