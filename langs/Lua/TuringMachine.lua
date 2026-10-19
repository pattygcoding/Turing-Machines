TuringMachine = {}
TuringMachine.__index = TuringMachine

function TuringMachine:new(input)
    local obj = {
        tape = "  " .. input .. "  ",
        head = 3,  -- Start at the first character of the input
        state = "q0"  -- Initial state
    }
    setmetatable(obj, TuringMachine)
    return obj
end

function TuringMachine:execute()
    while self.state ~= "accept" and self.state ~= "reject" do
        local currentSymbol = self.tape:sub(self.head, self.head)

        if self.state == "q0" then
            if currentSymbol == "a" then
                self.tape = self.tape:sub(1, self.head - 1) .. "X" .. self.tape:sub(self.head + 1)
                self.head = self.head + 1
                self.state = "q1"
            elseif currentSymbol == " " then
                self.state = "accept"
            else
                self.state = "reject"
            end

        elseif self.state == "q1" then
            if currentSymbol == "a" or currentSymbol == "X" then
                self.head = self.head + 1
            elseif currentSymbol == "b" then
                self.tape = self.tape:sub(1, self.head - 1) .. "Y" .. self.tape:sub(self.head + 1)
                self.head = self.head + 1
                self.state = "q2"
            else
                self.state = "reject"
            end

        elseif self.state == "q2" then
            if currentSymbol == "b" or currentSymbol == "Y" then
                self.head = self.head + 1
            elseif currentSymbol == " " then
                self.state = "accept"
            else
                self.state = "reject"
            end
        end
    end

    if self.state == "accept" then
        print("Input is accepted by the Turing Machine.")
    else
        print("Input is rejected by the Turing Machine.")
    end
end

local input = "aabb"
local tm = TuringMachine:new(input)
tm:execute()
