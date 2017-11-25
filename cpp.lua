
local cpp = {}

local c99 = require("c99")
local inspect = require("inspect")

-- TODO default defines: `gcc -dM -E - < /dev/null`

-- Not supported:
-- * character set conversion
-- * trigraphs

local states = {
    any = {
        ['"'] = { next = "dquote" },
        ["'"] = { next = "squote" },
        ["/"] = { silent = true, next = "slash" },
    },
    dquote = {
        ['"'] = { next = "any" },
        ["\\"] = { next = "dquote_backslash" },
    },
    dquote_backslash = {
        single_char = true,
        default = { next = "dquote" },
    },
    squote = {
        ["'"] = { next = "any" },
        ["\\"] = { next = "squote_backslash" },
    },
    squote_backslash = {
        single_char = true,
        default = { print = true, next = "squote" },
    },
    slash = {
        single_char = true,
        ["/"] = { add = " ", silent = true, next = "line_comment" },
        ["*"] = { add = " ", silent = true, next = "block_comment" },
        default = { add = "/", next = "any" },
    },
    line_comment = {
        silent = true,
    },
    block_comment = {
        silent = true,
        ["*"] = { silent = true, next = "try_end_block_comment" },
        continue_line = true,
    },
    try_end_block_comment = {
        single_char = true,
        silent = true,
        ["/"] = { silent = true, next = "any" },
        otherwise = { silent = true, next = "block_comment" },
        continue_line = true,
    },
}

for _, rules in pairs(states) do
    local out = "["
    for k, _ in pairs(rules) do
        if #k == 1 then
            out = out .. k
        end
    end
    out = out .. "]"
    rules.pattern = out ~= "[]" and out
end

local function add(buf, txt)
    if not buf then
        buf = {}
    end
    table.insert(buf, txt)
    return buf
end

function cpp.initial_processing(fd)
    local backslash_buf
    local buf
    local state = "any"
    local output = {}
    local linenr = 0
    for line in fd:lines() do
        linenr = linenr + 1
        local len = #line
        if line:find("\\", len, true) then
            -- If backslash-terminated, buffer it
            backslash_buf = add(backslash_buf, line:sub(1, len - 1))
        else
            -- Merge backslash-terminated line
            if backslash_buf then
                table.insert(backslash_buf, line)
                line = table.concat(backslash_buf)
            end
            backslash_buf = nil

            len = #line
            local i = 1
            local out = ""
            -- Go through the line
            while i <= len do
                ::continue::
                -- Current state in the state machine
                local st = states[state]

                -- Look for next character matching a state transition
                local n = nil
                if st.pattern then
                    if st.single_char then
                        if line:sub(i,i):find(st.pattern) then
                            n = i
                        end
                    else
                        n = line:find(st.pattern, i)
                    end
                end

                local transition, ch
                if n then
                    ch = line:sub(n, n)
                    transition = st[ch]
                else
                    n = i
                    ch = line:sub(n, n)
                    transition = st.default
                end

                if not transition then
                    -- output the rest of the string if we should
                    if not st.skip then
                        out = i == 1 and line or line:sub(i)
                    end
                    break
                end

                -- output everything up to the transition if we should
                if n > i and not st.silent then
                    buf = add(buf, line:sub(i, n - 1))
                end

                -- Some transitions output an explicit character
                if transition.add then
                    buf = add(buf, transition.add)
                end

                if not transition.silent then
                    buf = add(buf, ch)
                end

                -- and move to the next state
                state = transition.next
                i = n + 1
            end

            -- If we ended in a non-line-terminating state
            if states[state].continue_line then
                -- buffer the output and keep going
                buf = add(buf, out)
            else
                -- otherwise, flush the buffer
                if buf then
                    table.insert(buf, out)
                    out = table.concat(buf)
                    buf = nil
                end
                -- output the string and reset the state.
                table.insert(output, { nr = linenr, line = out})
                state = "any"
            end
        end
    end
    return output
end

function cpp.tokenize(line)
    local out = c99.preprocessing_grammar:match(line)
    print(line)
    print(inspect(out))
end

function cpp.parse_file(filename)
    local fd, err = io.open(filename, "rb")
    if not fd then
        return nil, err
    end
    local linelist = cpp.initial_processing(fd)
    local state = "any"
    for _, lineitem in ipairs(linelist) do
        local linenr = lineitem.nr
        local line = lineitem.line

        cpp.tokenize(line)
    end
end

return cpp
