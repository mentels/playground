#!/usr/bin/env python

import unittest

def main():
    print("Hello World!")

def return_42():
    """
    Returns 42.
    >>> return_42()
    42
    """
    return 42

if __name__ == "__main__":
    main()
    return_42() 

class TestClass(unittest.TestCase):

    def test_return42(self):
        self.assertEqual(42, return_42())


