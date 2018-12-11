#!/usr/bin/env python3

from itertools import cycle

players = 419
last_marble = 72164

class Marble:
    def __init__(self, value):
        self.value = value

class Circle:
    def __init__(self):
        zero = Marble(0)
        zero.clockwise = zero
        zero.anticlockwise = zero
        self.current = zero

    def move(self, distance):
        """Move by `distance` marbles, where negative values
        of distance move anticlockwise."""

        for i in range(abs(distance)):
            if distance < 0:
                self.current = self.current.anticlockwise
            else:
                self.current = self.current.clockwise
        
    def insert(self, value):
        inserted = Marble(value)

        inserted.anticlockwise = self.current.anticlockwise
        inserted.clockwise = self.current

        inserted.anticlockwise.clockwise = inserted
        inserted.clockwise.anticlockwise = inserted

        self.current = inserted

    def pop(self):
        removed = self.current
        self.current = removed.clockwise
        removed.anticlockwise.clockwise = removed.clockwise
        removed.clockwise.anticlockwise = removed.anticlockwise

        return removed.value

class Game:
    def __init__(self, players, last_marble):
        self.players = players
        self.last_marble = last_marble

        self._turns = cycle(range(players))
        self._marbles = iter(range(last_marble))

        self.circle = Circle()

        self.scores = dict()

    def _run_turn(self, player, marble):
        if marble % 23 == 0:
            score = self.scores.get(player, 0)
            self.circle.move(-7)

            removed = self.circle.pop()

            score += removed
            score += marble
            self.scores[player] = score

        else:
            self.circle.move(2)
            self.circle.insert(marble)

    def take_turn(self):
        player = next(self._turns)
        marble = next(self._marbles)

        self._run_turn(player, marble)

    def play_game(self):
        for marble in self._marbles:
            player = next(self._turns)
            self._run_turn(player, marble)

for last in [last_marble, last_marble * 100]:
    game = Game(players, last)
    game.play_game()

    print(f"The high score with a last marble of {last_marble} is {max(game.scores.values())}.")
