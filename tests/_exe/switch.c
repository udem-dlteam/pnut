void simpleSwitch(){
	int a = 2;

	switch (a) {
		case 1:
			putchar('A');
			break;
		case 2:
			putchar('B');
			break;
		case 3:
			putchar('C');
			break;
		default:
			putchar('D');
			break;
	}
}

void complexSwitch(){
	int a = 3;

	switch (a) {
		case 1:
			putchar('A');
			break;
		case 2:
			putchar('B');
			break;
		case 3:
			putchar('C');
			goto skip_default;
		default:
			putchar('D');
			break;
	}

	skip_default:
	putchar('E');
}

void complex2Switch(){
	int a = 0;

	start:
	switch (a) {
		case 0:
			putchar('A');
			a = 1;
			goto start;
		case 1:
			putchar('B');
			a = 2;
			goto start;
		case 2:
			putchar('C');
			goto end;
		default:
			putchar('D');
			break;
	}

	end:
	putchar('E');
}

void switchWhile(){
	int i = 0;

	while (i < 5) {
		switch (i) {
			case 0:
				putchar('A');
				break;
			case 1:
				putchar('B');
				break;
			case 2:
				putchar('C');
				break;
			case 3:
				putchar('D');
				break;
			default:
				putchar('E');
				break;
		}
		i++;
	}
}

int main() {
	simpleSwitch();
	complexSwitch();
	complex2Switch();
	switchWhile();
	return 0;
}
