#!/usr/bin/awk -f

function print_description(desc) {
  gsub(/^\s+|.|‐/, "", desc)
  gsub(/\s+/, " ", desc)
  gsub(/\\/, "\\\\", desc)
  gsub(/"/, "\\\"", desc)
  printf " \"%s\")\n", desc
  acc = ""
}

BEGIN {
  variables = 0
  conditionals = 0
  var_idx = 0
  print "("
}

/^\s+VVaarriiaabblleess/ { variables = 1; next }

/^\s+CCoonnddiittiioonnaall/ {
  print_description(acc)
  variables = 0;
  conditional = 1;
  next
}

/^\S/ { variables = conditional = 0; next }

variables && $0 ~ /^ {7}(([a-z])+)/ {
  if (var_idx++) 
    print_description(acc)
  gsub(/^\s*|./,"")
  printf "(\"%s\" ", $1
  $1 = ""

  # Extract default
  sub(/^\s+/, "")
  gsub(/[)(]/,"")
  printf "\"%s\"", $0
  next
}
# Variable descriptions
variables && var_idx {
  acc = acc $0
}

END {
  print ")"
}
