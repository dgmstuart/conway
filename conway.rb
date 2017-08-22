class Board
  attr_reader :living_cells
  attr_reader :width
  attr_reader :height

  def initialize(width, height, living_cells)
    @width = width
    @height = height

    @living_cells = living_cells
  end

  def to_s
    @grid = empty_grid
    @living_cells.each do |x, y|
      @grid[y][x] = true
    end

    @grid.map { |row|
      row.map { |cell| cell ? 'x' : '.' }.join
    }.join("\n") + "\n"
  end

  def tick
    Board.new(@width, @height, @living_cells)
  end

  private

  def empty_grid
    Array.new(@height).map do
      Array.new(@width, false)
    end
  end
end

class BoardIterator
  def initialize(board)
    @board = board
    @board_rules = BoardRules.new(@board.living_cells)
  end

  def next
    next_living_cells  = []

    (0..@board.height).to_a.each do |y|
      (0..@board.width).to_a.each do |x|
        cell = [x, y]

        if @board_rules.will_be_alive?(cell)
          next_living_cells << cell
        end
      end
    end

    Board.new(@board.width, @board.height, next_living_cells)
  end
end

class BoardRules
  def initialize(living_cells)
    @living_cells = living_cells
  end

  def will_be_alive?(cell)
    coordinate = Coordinate.new(cell[0], cell[1])
    number_of_living_neighbours = (coordinate.neighbours & @living_cells).count
    (alive?(cell) && comfortable?(number_of_living_neighbours)) || fertile?(number_of_living_neighbours)
  end

  private

  def alive?(cell)
    @living_cells.include?(cell)
  end

  def comfortable?(num_neighbours)
    [2, 3].include? num_neighbours
  end

  def fertile?(num_neighbours)
    num_neighbours == 3
  end
end


class Coordinate
  def initialize(x, y)
    @x = x
    @y = y
  end

  def neighbours
    [
      [@x-1, @y-1], [@x-1, @y], [@x-1, @y+1],
      [@x, @y-1], [@x, @y+1],
      [@x+1, @y-1], [@x+1, @y], [@x+1, @y+1],
    ]
  end
end

board = Board.new(10, 10 ,[[0, 0], [0, 2], [1, 1], [1, 2], [2, 1]])

10.times do
  puts board
  puts '---'
  board = BoardIterator.new(board).next
end
