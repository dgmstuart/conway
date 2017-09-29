require 'rspec'
require_relative 'conway'

RSpec.describe Board do
  describe 'to_s' do
    it 'prints out the alive/dead state of all the cells' do
      living_cells = [
        [1, 1],
        [1, 2],
        [2, 2],
      ]
      expect(Board.new(4, 4, living_cells).to_s).to eq <<~EOF
        ....
        .x..
        .xx.
        ....
      EOF
    end

    context 'when empty' do
      it 'prints a board showing everything dead' do
        expect(Board.new(4,4, []).to_s).to eq <<~EOF
          ....
          ....
          ....
          ....
        EOF
      end
    end
  end
end

RSpec.describe BoardIterator do
  describe 'tick' do
    it 'kills a lone cell' do
      living_cells = [[0, 0]]
      board = Board.new(3, 3, living_cells)
      expect(BoardIterator.new(board).next.living_cells).to eq []
    end

    it 'spawns life in a cell with 3 neighbours' do
      living_cells = [[0, 0], [0, 1], [0, 2]]
      board = Board.new(3, 3, living_cells)
      expect(BoardIterator.new(board).next.living_cells)
        .to include [1, 1]
    end

    it 'sustains a cell with 2 neighbours' do
      living_cells = [[0, 0], [0, 1], [1, 1]]
      board = Board.new(3, 3, living_cells)
      expect(BoardIterator.new(board).next.living_cells)
        .to include [1, 1]
    end

    it 'sustains a cell with 3 neighbours' do
      living_cells = [[0, 0], [0, 1], [0, 2], [1, 1]]
      board = Board.new(3, 3, living_cells)
      expect(BoardIterator.new(board).next.living_cells)
        .to include [1, 1]
    end

    it 'kills a cell with 4 neighbours' do
      living_cells = [[0, 0], [0, 1], [0, 2], [1, 0], [1, 1]]
      board = Board.new(3, 3, living_cells)
      expect(BoardIterator.new(board).next.living_cells)
        .to_not include [1, 1]
    end

    it 'doesnt spawn life in a cell with 4 neighbours' do
      living_cells = [[0, 0], [1, 1], [2, 0], [2, 1]]
      board = Board.new(3, 3, living_cells)
      expect(BoardIterator.new(board).next.living_cells)
        .to_not include [1, 0]
    end

    context 'when given an empty board' do
      it 'returns an empty board' do
        board = Board.new(3, 3, [])
        expect(BoardIterator.new(board).next.living_cells).to eq []
      end
    end
  end
end
