local cpp = require("cpp")
local c99 = require("c99")
local inspect = require("inspect")
local util = require("titan-compiler.util")

local ctx, err = cpp.parse_file(arg[1] or "/usr/include/stdio.h")
if not ctx then
    io.stderr:write("ERROR: " .. tostring(err) .. "\n")
    os.exit(1)
end

local srccode = table.concat(ctx.output, "\n")

c99.tracing = true

local res, err, rest = c99.language_grammar:match(srccode.."<EOF>")
if not res then
    local line, col = util.get_line_number(srccode, #srccode - #rest)
    print("error "..err.." at line " .. line .. ":" .. col .. ":")
    print(ctx.output[line])
    print((" "):rep(col - 1) .. "^")
end

print(inspect(res))
print("-----------------")
print(srccode)
