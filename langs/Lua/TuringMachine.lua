local line = io.read("*l")
if line == nil then
    line = ""
end
if line:sub(-1) == "\r" then
    line = line:sub(1, -2)
end

if line:find("[^01]") then
    print("Invalid input: expected a binary string.")
    return
end

-- The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to 0 and
-- the head moves left (carry); a 0 becomes 1 and the machine halts; a blank
-- becomes 1 (a new leading digit) and the machine halts.
local tape = {}
for index = 1, #line do
    tape[index] = line:sub(index, index)
end
local head = #tape

while true do
    local symbol = head >= 1 and tape[head] or "_"
    if symbol == "1" then
        tape[head] = "0"
        head = head - 1
    elseif symbol == "0" then
        tape[head] = "1"
        break
    else
        table.insert(tape, 1, "1")
        break
    end
end

print("Initial tape: " .. line)
print("Final tape: " .. table.concat(tape))
