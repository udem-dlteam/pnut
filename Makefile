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
	rm tests/*.sh tests/*.err

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

