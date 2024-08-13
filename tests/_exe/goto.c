void simpleGoto() {
	putchar('A');
	goto label;
	putchar('B');
	label:
	putchar('C');
	putchar(10);
}

void complexGoto() {
	putchar('A');
	goto label;
	putchar('B');
	label:
	putchar('C');
	putchar('D');
	goto label2;
	putchar('E');
	label2:
	putchar('F');
	putchar(10);
}

void loopGoto() {
	int i = 0;
	putchar('A');
	label:
	putchar('B');
	i++;
	if (i < 3) {
		goto label;
	}
	putchar('C');
	putchar(10);
}

void loop2Goto(){
	int i = 0;

	loop:
	if (i >= 3) goto end;
	putchar('X');
	i++;
	goto loop;

	end:
	putchar('Y');
}

void breakGoto() {
	int i = 0;
	putchar('A');
	label:
	putchar('B');
	i++;
	if (i < 3) {
		goto end;
	}
	putchar('C');
	putchar(10);
	end:
	putchar('D');
	putchar(10);
}

int main() {
	simpleGoto();
	complexGoto();
	loopGoto();
	loop2Goto();
	breakGoto();
	return 0;
}