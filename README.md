
# rb2nim

Download https://gitlab.com/metacraft-labs/rubocop
install rbenv or ruby in some other way: you might need to change your commands

Download https://gitlab.com/metacraft-labs/fast-rubocop

```bash
rb2nim <filename pattern> <folder> <command>

rb2nim case_equality ~/fast-rubocop "bash ~/spec.sh"
```

where spec.sh is e.g. something like 

```bash
cd ~/rubocop
rbenv local 2.5.0

rbenv exec ruby /home/alehander42/.rbenv/versions/2.5.0/bin/rspec ~/rubocop/spec/rubocop/cop/style/case_equality_spec.rb
# your path
```

we also need to require_relative tracing.rb in the right place: this has to be automated more, but for now it's in cop.rb in your repo.

now you can see `~/fast-rubocop` and e.g. compile and run `~/fast-rubocop/nimcop.nim` with a ruby file as arg

# rubytracer

Ruby tracing

## example

```json
{"A.b":{"kind":"method","args":[{"kind":"simple","label":"Integer"}],"return_type":{"kind":"simple","label":"NilClass"}},"Love.b":{"kind":"method","args":[{"kind":"simple","label":"String"}],"return_type":{"kind":"simple","label":"NilClass"}}}
```

## How to use

## LICENSE

The MIT License (MIT)

Copyright (c) 2019 Zahary Karadjov, Alexander Ivanov
