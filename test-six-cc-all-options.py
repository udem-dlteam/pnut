import argparse
import subprocess

parser = argparse.ArgumentParser()

parser.add_argument('--shell', type=str, help='Which shell to run the tests with', default="ksh")
shell = parser.parse_args().shell

boolean_options = ["true", "false"]

# (option_type, key, values)
# option_type: True for options that are concatenated with a dash
#              False for options that are separated by a space
options = [ (True, "function-return-method", ["variable", "arg-loc false", "arg-loc true"])
          # Only testing with true since not initializing is incorrect semantics
          , (False, "initialize-memory-when-alloc", ["true"])
          , (False, "inline-inplace-arithmetic-ops", boolean_options)
          , (False, "prefix-local-vars", boolean_options)
          , (False, "inline-string-init", boolean_options)
          , (False, "arithmetic-conditions", boolean_options)
          , (False, "arithmetic-assignment", boolean_options)
          ]

def generate_options(opts):
  if len(opts) == 0:
    yield "";
  else:
    opt = opts[0]
    option_type = opt[0]
    key = opt[1]
    for val in opt[2]:
      for rest in generate_options(opts[1:]):
        if option_type == True:
          yield f"--{key}-{val} " + rest
        else:
          yield f"--{key} {val} " + rest

for six_cc_options in generate_options(options):
  print(f"Running tests with {shell} with options: {six_cc_options}")
  subprocess.run(f"SHELL=\"{shell}\" SIX_CC_OPTIONS=\"{six_cc_options}\" QUIET=1 make test-six-cc", shell=True)
