#! /bin/sh
#
# Make kit/pnut-sh.sh executable

TEMP_DIR=build/kit

mkdir -p "$TEMP_DIR"

PNUT_SH_OPTIONS="-Dsh -DRT_NO_INIT_GLOBALS"

gcc -o "$TEMP_DIR/pnut-sh" $PNUT_SH_OPTIONS pnut.c
./$TEMP_DIR/pnut-sh $PNUT_SH_OPTIONS pnut.c > kit/pnut-sh.sh
