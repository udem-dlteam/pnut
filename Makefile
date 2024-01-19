.PHONY: watch clean test vm-tests vm-tests-ksh vm-tests-dash vm-tests-bash vm-tests-zsh vm-tests-posix vm-tests-all

watch:
	# TODO: Replace watch with an inotifywait-like loop
	# Check if file is defined
ifndef file
	$(error file is undefined. Use `make watch file=filename.c`)
endif
	touch out.sh
	chmod +x out.sh
	watch -n1 "./cc.sh $(file) > out.sh && ./out.sh"

clean:
	$(RM) out.sh
	$(RM) tests/*.sh tests/*.err
	$(RM) vm-tests/*.expected vm-tests/*.op vm-tests/*.result

test:
	@echo "Running tests..."
	@for file in $(shell find tests -type f -name "*.c"); do \
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

c4.o:
	gcc -o c4.o c4.c

c4.op: c4.o
	./c4.o -b -p c4.c > c4.op

c4-2.op: c4.op
	ksh ./c4.sh --no-exit c4.op -b -p c4.c > c4-2.op

c4-3.op: c4-2.op
	ksh ./c4.sh --no-exit c4-2.op -b -p c4.c > c4-3.op

# To specify with which shell to run the tests, use `SHELL=ksh make vm-tests`
# For each test case:
# 1. Generate bytecode with base C4
# 2. If it succeeds, rerun C4, this time running the bytecode
# 3. Run the bytecode using c4.sh
# 4. Compare output
vm-tests: c4.o c4.sh
	@echo "Running tests with $$SHELL..."
	@for file in $(shell find vm-tests -type f -name "*.c" | sort); do \
		filename=$$(basename $$file .c); \
		./c4.o -b -p $$file > vm-tests/$$filename.op 2>&1 ; \
		if [ $$? -eq 0 ]; then \
			first_line=$$(head -n 1 $$file); \
			args=$${first_line##"// args:"}; \
			./c4.o "$$file" $$args > "vm-tests/$$filename.expected" 2>&1; \
			$$SHELL ./c4.sh vm-tests/$$filename.op $$args > vm-tests/$$filename.result 2>&1; \
			diff_out=$$(diff vm-tests/$$filename.expected vm-tests/$$filename.result); \
			if [ $$? -eq 0 ]; then \
				echo "$$filename: ✅"; \
			else \
				echo "$$filename: ❌"; \
				echo "diff (output vs expected)"; \
				echo "$$diff_out"; \
			fi \
		else \
			echo "$$filename: ❌ Failed to compile: $$(cat tests/$$filename.op)"; \
		fi; \
	done

vm-tests-ksh: c4.o c4.sh
	SHELL=ksh make vm-tests

vm-tests-dash: c4.o c4.sh
	SHELL=dash make vm-tests

vm-tests-bash: c4.o c4.sh
	SHELL=bash make vm-tests

vm-tests-zsh: c4.o c4.sh
	SHELL=zsh make vm-tests

vm-tests-posix: c4.o c4.sh
	SHELL=sh make vm-tests

vm-tests-all: vm-tests-bash vm-tests-zsh vm-tests-dash vm-tests-ksh vm-tests-posix
