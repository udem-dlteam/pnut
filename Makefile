.PHONY: watch clean test test-six-cc

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
	$(RM) six-cc-tests/*.sh six-cc-tests/*.err

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

test-six-cc-all-shells:
	SHELL=ksh  make test-six-cc
	SHELL=dash make test-six-cc
	SHELL=bash make test-six-cc
	SHELL=zsh  make test-six-cc
	SHELL=yash make test-six-cc
	SHELL=mksh make test-six-cc

test-six-cc-all-options:
	python3 test-six-cc-all-options.py

test-six-cc:
ifndef QUIET
	echo "Running six-cc tests..."
endif
	@for file in $(shell find six-cc-tests -type f -name "*.c" | sort); do \
		filename=$$(basename $$file .c); \
		[ -z "$$QUIET" ] && /bin/echo -n "$$filename: "; \
		gsi six-cc.scm $$SIX_CC_OPTIONS $$file > six-cc-tests/$$filename.sh 2>&1; \
		if [ $$? -eq 0 ]; then \
			first_line=$$(head -n 1 $$file); \
			args=$${first_line#"/* args:"}; \
			args=$${args%"*/"}; \
			input=$${first_line#"/* input:"}; \
			input=$${input%"*/"}; \
			input=$${input#" "}; \
			input=$${input%" "}; \
			if [ -e "$$input" ]; then \
				$$SHELL six-cc-tests/$$filename.sh $$args < "$$input" > six-cc-tests/$$filename.result; \
			else \
				timeout 3 $$SHELL six-cc-tests/$$filename.sh $$args < c4.c > six-cc-tests/$$filename.result; \
			fi; \
			if [ $$? -eq 0 ]; then \
				if [ -f "six-cc-tests/$$filename.golden" ]; then \
					diff_out=$$(diff six-cc-tests/$$filename.golden six-cc-tests/$$filename.result); \
					if [ $$? -eq 0 ]; then \
						[ -z "$$QUIET" ] && echo "✅"; \
					else \
						[ -z "$$QUIET" ] && /bin/echo -n "$$filename: "; \
						echo "❌"; \
						echo "diff (output vs expected)"; \
						echo "$$diff_out"; \
					fi \
				else \
					cp six-cc-tests/$$filename.result six-cc-tests/$$filename.golden; \
					[ -z "$$QUIET" ] && /bin/echo -n "$$filename: "; \
					echo "❌ Generated golden file"; \
				fi \
			else \
				[ -z "$$QUIET" ] && /bin/echo -n "$$filename: "; \
				echo "❌ Failed to run: $$(cat six-cc-tests/$$filename.result)"; \
			fi; \
			rm -f six-cc-tests/$$filename.result; \
		else \
			[ -z "$$QUIET" ] && /bin/echo -n "$$filename: "; \
			echo "❌ Failed to compile. See six-cc-tests/$$filename.sh for error."; \
		fi; \
	done

c4-for-six.sh: six-cc.scm c4-for-six.c
	gsi six-cc.scm c4-for-six.c --malloc-init > c4-for-six.sh

c4_by_c4-for-six-op.golden: c4-for-six.sh c4.c
	ksh ./c4-for-six.sh -b c4.c > c4_by_c4-for-six-op.golden

# Takes ~3.3s
c4_by_c4-for-six-op.golden.ksh: c4-for-six.sh c4.c
	ksh ./c4-for-six.sh -b c4.c > c4_by_c4-for-six-op.golden.ksh

# Takes ~53s
c4_by_c4-for-six-op.golden.bash: c4-for-six.sh c4.c
	bash ./c4-for-six.sh -b c4.c > c4_by_c4-for-six-op.golden.bash

# Takes ~1min15s
c4_by_c4-for-six-op.golden.dash: c4-for-six.sh c4.c
	dash ./c4-for-six.sh -b c4.c > c4_by_c4-for-six-op.golden.dash

# Takes ~2min20s
c4_by_c4-for-six-op.golden.zsh: c4-for-six.sh c4.c
	zsh ./c4-for-six.sh -b c4.c > c4_by_c4-for-six-op.golden.zsh

# Takes ~1min
c4_by_c4-for-six-op.golden.yash: c4-for-six.sh c4.c
	yash ./c4-for-six.sh -b c4.c > c4_by_c4-for-six-op.golden.yash

# Takes ~6min30s
c4_by_c4-for-six-op.golden.mksh: c4-for-six.sh c4.c
	mksh ./c4-for-six.sh -b c4.c > c4_by_c4-for-six-op.golden.mksh

c4_by_c4-for-six-op.golden-all: c4_by_c4-for-six-op.golden.ksh c4_by_c4-for-six-op.golden.bash c4_by_c4-for-six-op.golden.dash c4_by_c4-for-six-op.golden.zsh c4_by_c4-for-six-op.golden.yash c4_by_c4-for-six-op.golden.mksh

c4-bootstrap-with-six-cc: c4-for-six.sh
	ksh c4-for-six.sh c4.c -b -p c4.c > c4_by_c4-for-six-op-bootstrap2.golden
