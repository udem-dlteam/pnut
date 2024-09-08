#!/bin/bash


if [ $# -eq 0 ]; then
  chroot woody /bin/bash -c "cd /pnut; bash"
else
  chroot woody /bin/bash -c "cd /pnut; $@"
fi
