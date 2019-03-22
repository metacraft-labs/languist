
# rb2nim

Clone the repositories [rubocop][rubocop_url] and [fast-rubocop][fast_rubocop_url]
and place them next to each other

install rbenv or ruby in some other way: you might need to change your commands

[rubocop_url]: https://gitlab.com/metacraft-labs/rubocop
[fast_rubocop_url]: https://gitlab.com/metacraft-labs/fast-rubocop

Set in env RB2NIM_CONFIG to e.g. `rubocop.json` or `test.json`
```bash
rb2nim <filename pattern> <folder> <command>

rb2nim case_equality ~/fast-rubocop "bash ~/spec.sh"
or 
rb2nim test/..
```
But run mostly langcop for now

where spec.sh is e.g. something like 

```bash
#!/bin/bash
RUBY_VERSION=2.5.0

cd ../rubocop
rbenv local $RUBY_VERSION

rbenv exec ruby ~/.rbenv/versions/$RUBY_VERSION/bin/rspec ./spec/rubocop/cop/style/case_equality_spec.rb
```

we also need to require_relative tracing.rb in the right place: this has to be automated more, but for now it's in cop.rb in your repo.

now you can see `~/fast-rubocop` and e.g. compile and run `~/fast-rubocop/checker.nim` with a ruby file as arg

you can also use langcop

```bash
# specially for rubocop <name> <category>
./langcop block_length 'metrics rubocop'
```

# rubytracer

Ruby tracing: we will have this as a lib and as a binary

## example

```json
{"A.b":{"kind":"method","args":[{"kind":"simple","label":"Integer"}],"return_type":{"kind":"simple","label":"NilClass"}},"Love.b":{"kind":"method","args":[{"kind":"simple","label":"String"}],"return_type":{"kind":"simple","label":"NilClass"}}}
```

## LICENSE

The MIT License (MIT)

Copyright (c) 2019 Zahary Karadjov, Alexander Ivanov
