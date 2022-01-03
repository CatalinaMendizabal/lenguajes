defmodule Point do
  defstruct x: 0, y: 0

  def new(x, y) do
    %Point{x: x, y: y}
  end

  def add(%Point{x: x1, y: y1}, %Point{x: x2, y: y2}) do
    new(x1 + x2, y1 + y2)
  end
end
