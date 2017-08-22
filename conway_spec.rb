require 'rspec'
require_relative 'conway'

RSpec.describe Board do
  describe 'to_s' do
    it 'prints out the alive/dead state of all the cells' do
      living_cells = [
        [1, 1],
        [2, 1],
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

  describe 'tick' do
    it 'kills a lone cell' do
      living_cells = [[0, 0]]
      expect(Board.new(2, 2, living_cells).tick.to_s).to eq <<~EOF
        ..
        ..
      EOF
    end

    context 'when empty' do
      it 'returns an empty board' do
        expect(Board.new(3, 3, []).tick.to_s).to eq Board.new(3, 3, []).to_s
      end
    end
  end
end
