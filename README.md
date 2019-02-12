
# rubytracer

Ruby tracing

## example

```json
{"A.b":{"kind":"method","args":[{"kind":"simple","label":"Integer"}],"return_type":{"kind":"simple","label":"NilClass"}},"Love.b":{"kind":"method","args":[{"kind":"simple","label":"String"}],"return_type":{"kind":"simple","label":"NilClass"}}}
```

## How to use

python-deduckt (also launched with `python-deduckt/deduckt/main.py`) is a drop-in
replacement for the python interpreter. 

Each traced python execution will update the `python-deduckt.json` file stored in
the current directory. This file stores an annotated AST for each module imported
by your program.

```bash
python-deduckt full path test.py args
```

## Results

The format is

```python
{
    "<pythonName>": {
        <typeAnnotation>
    }
}
```

Python name

```python
"<namespaces>.<name>"
```

name

```python
"<name>" # const
"<name>" # class
"#<name>" # function
"<class>#<name>" # method
```

Variables 

```python
{
    "name": <name>,
    "type": <typeAnnotation>
}
```

A type annotation can be a class, function description or an atomic type.


```python
{
    "kind": "PyTypeFunction",
    "label": <name>,
    "args": [<variable>],
    "variables": [<variable>],
    "returnType": <typeAnnotation>
}
```

```python
{
    "kind": "PyTypeObject",
    "label": <name>,
    "fields": [<variable>]
}
```

Full doc: todo

## FAQ

* Can I generate mypy annotations with it?

It's definitely possible to generate very useful mypy annotations from the recorded data.
However that hasn't been our use case, but contributions are welcome

* Why does it analyze all the events?

In the future we might add an option to trace only a statistically significant part of them.
It's not a huge priority for our use case, as we used it for [py2nim](https://github.com/metacraft-labs/py2nim).
A porting task is not something that would be ran often and better type information is more important than speed.

* How to pronounce deduckt?

Like the first part of "deductive", or like getting rid of the duck types in your backyard. 

## LICENSE

The MIT License (MIT)

Copyright (c) 2017-2018 Zahary Karadjov, Alexander Ivanov
