.PHONY: clean test

clean:
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
