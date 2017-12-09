local cpp = require("cpp")
local c99 = require("c99")
local inspect = require("inspect")
local util = require("titan-compiler.util")

c99.tracing = true

local ctx, err = cpp.parse_file(arg[1] or "/usr/include/stdio.h")
if not ctx then
    io.stderr:write("ERROR: " .. tostring(err) .. "\n")
    os.exit(1)
end

local srccode = table.concat(ctx.output, "\n").."<EOF>"

local res, err, rest = c99.language_grammar:match(srccode)
if not res then
    local line, col = util.get_line_number(srccode, (#srccode - #rest) + 1)
    print("error "..err.." at line " .. line .. ":" .. col .. ":")
    print(ctx.output[line])
    print((" "):rep(col - 1) .. "^")
    print()
    for i = math.max(line - 5, 1), math.min(line + 5, #ctx.output) do
        print(i, ctx.output[i])
    end
end

print("-----------------")
print(inspect(res))
print("-----------------")
--for i = 1, #ctx.output do
--    print(i, ctx.output[i])
--end
