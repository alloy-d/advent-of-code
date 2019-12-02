#!/usr/bin/env python3

from functools import reduce
from operator import add
from sys import stdin

class Node:
    def __init__(self, iterator):
        num_children = next(iterator)
        num_metadata = next(iterator)

        self.children = []
        self.metadata = []

        for i in range(num_children):
            self.children.append(Node(iterator))

        for i in range(num_metadata):
            self.metadata.append(next(iterator))

    def value(self):
        if len(self.children) == 0:
            return sum(self.metadata)

        valid_child_references = (ref for ref in self.metadata
                                  if ref <= len(self.children))
        
        return sum(self.children[i-1].value() for i in valid_child_references)

def sum_metadata(node):
    return sum(node.metadata) + reduce(add, map(sum_metadata, node.children), 0)

if __name__ == "__main__":
    stream = map(int, stdin.readline().split(" "))
    root = Node(stream)

    print(f"""\
    The sum of all the metadata is {sum_metadata(root)}.

    The value of the root node is {root.value()}.\
    """)
