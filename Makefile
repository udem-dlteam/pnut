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
	rm -f out.sh
	rm -f tests/*.sh tests/*.err
	rm -f six-cc-tests/*.sh six-cc-tests/*.err

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

test-six-cc:
	@echo "Running six-cc tests..."
	@for file in $(shell find six-cc-tests -type f -name "*.c" | sort); do \
		filename=$$(basename $$file .c); \
		gsi six-cc.scm $$file > six-cc-tests/$$filename.sh 2>&1; \
		if [ $$? -eq 0 ]; then \
			first_line=$$(head -n 1 $$file); \
			args=$${first_line#"/* args:"}; \
			args=$${args%"*/"}; \
			$$SHELL six-cc-tests/$$filename.sh $$args > six-cc-tests/$$filename.result; \
			if [ $$? -eq 0 ]; then \
				if [ -f "six-cc-tests/$$filename.golden" ]; then \
					diff_out=$$(diff six-cc-tests/$$filename.golden six-cc-tests/$$filename.result); \
					if [ $$? -eq 0 ]; then \
						echo "$$filename: ✅"; \
					else \
						echo "$$filename: ❌"; \
						echo "diff (output vs expected)"; \
						echo "$$diff_out"; \
					fi \
				else \
					cp six-cc-tests/$$filename.result six-cc-tests/$$filename.golden; \
					echo "$$filename: ❌ Generated golden file"; \
				fi \
			else \
				echo "$$filename: ❌ Failed to run: $$(cat six-cc-tests/$$filename.result)"; \
			fi; \
			rm -f six-cc-tests/$$filename.result; \
		else \
			echo "$$filename: ❌ Failed to compile. See six-cc-tests/$$filename.sh for error."; \
		fi; \
	done
