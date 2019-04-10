import os, strformat, osproc

let list = @[
  ("case_equality", "style"),
  ("circular_argument_reference", "lint"),
  ("disjunctive_assignment_in_constructor", "lint"),
  ("boolean_symbol", "lint"),
  ("variable_name", "naming"),
  ("method_length", "metrics"),
  # ("binary_operator_parameter_name", "naming"),
  # ("class_length", "metrics"),
  # ("line_length", "metrics"),
  ("begin_block", "style")
]

for (label, directory) in list:
  echo &"{label} {directory}"
  let status = execCmd(&"./langcop {label} {directory}")
  if status == 130:
    quit(1)
