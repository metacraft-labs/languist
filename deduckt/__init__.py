# import python.generator
from .generator import Generator

def instrument(filename, source):
    return Generator().gen(filename, source)
