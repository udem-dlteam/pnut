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
		./cc.sh $$file > tests/$$filename.sh; \
		chmod +x tests/$$filename.sh; \
		./tests/$$filename.sh > tests/$$filename.golden; \
	done
else
	echo "Running tests..."
	for file in $(shell find tests -type f -name "*.c"); do \
		filename=$$(basename $$file .c); \
		./cc.sh $$file > tests/$$filename.sh; \
		chmod +x tests/$$filename.sh; \
		diff_out=$$(./tests/$$filename.sh | diff - tests/$$filename.golden); \
		if [ $$? -eq 0 ]; then \
			echo "$$filename: ✅"; \
		else \
			echo "$$filename: ❌"; \
			echo $$diff_out; \
		fi \
	done
endif