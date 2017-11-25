local cpp = require("cpp")

local lines, err = cpp.parse_file(arg[1] or "/usr/include/stdio.h")
if not lines then
    io.stderr:write(tostring(err) .. "\n")
    os.exit(1)
end

local i = 1
for _, line in ipairs(lines) do
    local n = line[1]
    local text = line[2]
    while n > i do
        io.stdout:write("\n")
        i = i + 1
    end
    io.stdout:write(text.."\n")
    i = i + 1
end
