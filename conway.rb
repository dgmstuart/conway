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
      @grid[x][y] = true
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
  end

  def next
    next_living_cells  = []

    (0..@board.height).to_a.each do |y|
      (0..@board.width).to_a.each do |x|
        neighbours = [
          [y-1, x-1], [y-1, x], [y-1, x+1],
          [y, x-1], [y, x+1],
          [y+1, x-1], [y+1, x], [y+1, x+1],
        ]

        living_neighbours = neighbours & @board.living_cells

        if living_neighbours.count == 3
          next_living_cells << [x, y]
        end
      end
    end

    Board.new(@board.width, @board.height, next_living_cells)
  end
end

# board =  Board.new(5,5,[[0, 0], [0, 1], [0, 2]])
# puts board
# puts '---'
# puts BoardIterator.new(board).next


