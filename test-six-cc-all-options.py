import argparse
import subprocess

parser = argparse.ArgumentParser()

parser.add_argument('--shell', type=str, help='Which shell to run the tests with', default="ksh")
shell = parser.parse_args().shell

boolean_options = ["true", "false"]

options = [ ["--malloc-init"] # , "--malloc-no-init"]
          , ["--free-unsets-vars", "--free-noop"]
          , ["--zero-globals", "--no-zero-globals"]
          , ["--inline-inplace-arithmetic", "--no-inline-inplace-arithmetic"]
          , ["--prefix-local-vars", "--no-prefix-local-vars"]
          , ["--init-string-inline", "--init-string-upfront"]
          , ["--callee-save"] # , "--caller-save"]
          , ["--use-shell-conditions"] # , "--use-arithmetic-conditions", ]
          , ["--use-arithmetic-assignment", "--use-regular-assignment"]
          , ["--optimize-simple-functions", "--no-optimize-simple-functions"]
          , ["--optimize-return-loc", "--no-optimize-return-loc"]
          , ["--numeric-chars", "--no-numeric-chars"]
          ]

def generate_options(opts):
  if len(opts) == 0:
    yield "";
  else:
    for opt in opts[0]:
      for rest in generate_options(opts[1:]):
        yield f"{opt} " + rest

for six_cc_options in generate_options(options):
  print(f"Running tests with {shell} with options: {six_cc_options}")
  subprocess.run(f"SHELL=\"{shell}\" SIX_CC_OPTIONS=\"{six_cc_options}\" QUIET=1 make test-six-cc", shell=True)
