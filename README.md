
# rb2nim

```bash
rb2nim <filename pattern> <folder> <command>

rb2nim case_equality ~/nimterop "bash ~/spec.sh"
```

where spec.sh is
```bash
cd ~/rubocop
rbenv local 2.5.0

rbenv exec ruby /home/alehander42/.rbenv/versions/2.5.0/bin/rspec ~/rubocop/spec/rubocop/cop/style/case_equality_spec.rb
```

for example

we also need to require_relative tracing.rb in the right place: this has to be automated more

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
