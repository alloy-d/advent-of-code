#!/usr/bin/env python3

from itertools import cycle

players = 419
last_marble = 72164

class Game:
    def __init__(self, players, last_marble):
        self.players = players
        self.last_marble = last_marble

        self._turns = cycle(range(players))
        self._marbles = iter(range(last_marble))

        self.circle = [next(self._marbles)]
        self.current_marble = 0

        self.scores = dict()

    def shift_current_marble(self, shift):
        """Updates the current marble by `shift`.

        Negative values of shift represent counterclockwise shifts.
        """

        index = self.current_marble + shift
        if index < 0:
            # We've shifted counterclockwise past the beginning of the list.
            self.current_marble = len(self.circle) + index
        elif index > len(self.circle):
            # We've shifted clockwise past the next open position.
            self.current_marble = index - len(self.circle)
        else:
            self.current_marble = index

    def _run_turn(self, player, marble):
        if marble % 23 == 0:
            score = self.scores.get(player, 0)
            self.shift_current_marble(-7)

            removed = self.circle.pop(self.current_marble)

            score += removed
            score += marble
            self.scores[player] = score

        else:
            self.shift_current_marble(2)
            self.circle.insert(self.current_marble, marble)

    def take_turn(self):
        player = next(self._turns)
        marble = next(self._marbles)

        self._run_turn(player, marble)

    def play_game(self):
        for marble in self._marbles:
            player = next(self._turns)
            self._run_turn(player, marble)

game = Game(players, last_marble)
game.play_game()
print(f"The high score is {max(game.scores.values())}.")
