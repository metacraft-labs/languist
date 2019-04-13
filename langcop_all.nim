import os, strformat, osproc

let list = @[
  # ("case_equality", "style"),
  # ("circular_argument_reference", "lint"),
  # ("disjunctive_assignment_in_constructor", "lint"),
  # ("boolean_symbol", "lint"),
  # ("variable_name", "naming"),
  # ("method_length", "metrics"),
  # ("begin_block", "style"),
  # ("single_line_methods", "style"),
  # ("send", "style"),
  # ("end_block", "style"),#
  ("symbol_literal", "style"),
  ("if_with_semicolon", "style"),
  ("nested_ternary_operator", "style"),
  ("method_missing_super", "style"),
  ("multiline_ternary_operator", "style"),
  ("proc", "style"),
  ("when_then", "style"),
  ("inline_comment", "style"),
  ("colon_method_definition", "style"),
  ("struct_inheritance", "style"),
  ("class_and_module_camel_case", "naming"),
  ("method_name", "naming")
  # we choose ~20 of the simplest and shortest cops for beginning
  # we choose several different and maybe several big!
  # and maybe if batsov wants some for example
]

# ("symbol_literal", "style"),
# ("send", "symbol")
for (label, directory) in list:
  echo &"{label} {directory}"
  let status = execCmd(&"./langcop {label} {directory}")
  if status == 130:
    quit(1)
