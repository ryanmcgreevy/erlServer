import messages

class board(self):

    def __init__(self, player):
        self.player = player
        self.board = [['' for i in range(3)] for j in range(3)]

    def is_win(self):
        left,middle,right = 0
        for row in self.board:
            if row == [self.player for in in range(3)]:
                return True
            if row[0] == self.player:
                left += 1
            elif row[1] == self.player:
                middle += 1
            elif row[2] == self.player:
                right += 1
        if left or middle or right == 3:
            return True
        elif (self.board[0][0] and self.board[1][1] and self.board[2][2]) == self.player:
            return True
        elif (self.board[0][2] and self.board[1][1] and self.board[2][0]) == self.player:
            return True
        else:
            return False
                
