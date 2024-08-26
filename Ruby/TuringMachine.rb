class TuringMachine
  def initialize(input)
    @tape = "  #{input}  ".chars # Tape with input string and extra spaces
    @head = 2 # Start at the first character of the input
    @state = "q0" # Initial state
  end

  def execute
    while @state != "accept" && @state != "reject"
      current_symbol = @tape[@head]

      case @state
      when "q0"
        if current_symbol == "a"
          @tape[@head] = "X"
          @head += 1
          @state = "q1"
        elsif current_symbol == " "
          @state = "accept"
        else
          @state = "reject"
        end

      when "q1"
        if ["a", "X"].include?(current_symbol)
          @head += 1
        elsif current_symbol == "b"
          @tape[@head] = "Y"
          @head += 1
          @state = "q2"
        else
          @state = "reject"
        end

      when "q2"
        if ["b", "Y"].include?(current_symbol)
          @head += 1
        elsif current_symbol == " "
          @state = "accept"
        else
          @state = "reject"
        end
      end
    end

    if @state == "accept"
      puts "Input is accepted by the Turing Machine."
    else
      puts "Input is rejected by the Turing Machine."
    end
  end
end

input = "aabb"
tm = TuringMachine.new(input)
tm.execute
