import subprocess

boolean_options = ("true", "false")

for function_return_method in ("variable", "arg-loc false", "arg-loc true"):
  # Only testing with true since not initializing is incorrect semantics
  for initialize_memory_when_alloc in ("true"): # ("true", "false"):
    for inline_inplace_arithmetic_ops in boolean_options:
      for prefix_local_vars in boolean_options:
        for inline_string_init in boolean_options:
          for arithmetic_conditions in boolean_options:
            SIX_CC_OPTIONS=""
            SIX_CC_OPTIONS=SIX_CC_OPTIONS + f" --function-return-method-{function_return_method}"
            SIX_CC_OPTIONS=SIX_CC_OPTIONS + f" --initialize-memory-when-alloc {initialize_memory_when_alloc}"
            SIX_CC_OPTIONS=SIX_CC_OPTIONS + f" --inline-inplace-arithmetic-ops {inline_inplace_arithmetic_ops}"
            SIX_CC_OPTIONS=SIX_CC_OPTIONS + f" --prefix-local-vars {prefix_local_vars}"
            SIX_CC_OPTIONS=SIX_CC_OPTIONS + f" --inline-string-init {inline_string_init}"
            SIX_CC_OPTIONS=SIX_CC_OPTIONS + f" --arithmetic_conditions {arithmetic_conditions}"
            print(f"Running tests with options: {SIX_CC_OPTIONS}")
            subprocess.run(f"SIX_CC_OPTIONS=\"{SIX_CC_OPTIONS}\" QUIET=1 make test-six-cc", shell=True)
