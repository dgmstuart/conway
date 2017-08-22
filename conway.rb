class Board
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
