local cpp = require("cpp")

local ctx, err = cpp.parse_file(arg[1] or "/usr/include/stdio.h")
if not ctx then
    io.stderr:write(tostring(err) .. "\n")
    os.exit(1)
end

for _, line in ipairs(ctx.output) do
    print(line)
end
