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
	rm tests/*.sh

test:
	# Read all files in test directory
ifdef save
	echo "Saving test output..."
	for file in $(shell find tests -type f -name "*.c"); do \
		filename=$$(basename $$file .c); \
		cc_err=$$(./cc.sh $$file > tests/$$filename.sh 2>&1); \
		if [ $$? -eq 0 ]; then \
			chmod +x tests/$$filename.sh; \
			./tests/$$filename.sh > tests/$$filename.golden; \
		else \
			echo "$$filename: Failed to compile"; \
		fi \
	done
else
	echo "Running tests..."
	for file in $(shell find tests -type f -name "*.c"); do \
		filename=$$(basename $$file .c); \
		cc_err=$$(./cc.sh $$file > tests/$$filename.sh 2>&1); \
		if [ $$? -eq 0 ]; then \
			chmod +x tests/$$filename.sh; \
			test_output=$$(./tests/$$filename.sh 2>&1); \
			if [ $$? -eq 0 ]; then \
				diff_out=$$(echo $$test_output | diff - tests/$$filename.golden); \
				if [ $$? -eq 0 ]; then \
					echo "$$filename: ✅"; \
				else \
					echo "$$filename: ❌"; \
					echo $$test_output; \
				fi \
			else \
				echo "$$filename: ❌ Failed to run: $$test_output"; \
			fi \
		else \
			echo "$$filename: ❌ Failed to compile: $$cc_err"; \
		fi \
	done
endif