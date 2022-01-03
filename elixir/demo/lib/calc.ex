defmodule Math do

  @pi 3.14159
  def circle_area(r) do
    r * r * @pi
  end

  defmodule Calc do

      @spec add(number, number) :: number
      def add(a, b) do
        a + b
      end

      def double(a) do
       add(a, a)
      end

      def fact(0), do: 1
      def fact(n), do: n * fact(n - 1)

  end

  defmodule MyModule do
   def hello1 do
     alias Math.Calc, as: Calc
     Calc.add(1, 2)

     alias Math.Calc, as: C
     C.add(1, 2)
   end

   def hello2 do
    alias Math.Calc
    Calc.add(1, 2)
   end

   def hello3 do
    import Math.Calc, only: [add: 2]
    add(1, 2)
   end
 end
end
