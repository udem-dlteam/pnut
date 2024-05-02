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
				timeout 3 $$SHELL six-cc-tests/$$filename.sh $$args > six-cc-tests/$$filename.result; \
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