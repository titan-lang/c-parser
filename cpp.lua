
local cpp = {}

-- Not supported:
-- * character set conversion
-- * trigraphs

local states = {
    any = {
        ['"'] = { next = "dquote" },
        ["'"] = { next = "squote" },
        ["/"] = { skip = true, next = "slash" },
        ["\\"] = { next = "backslash" },
    },
    backslash = {
        next = "any",
    },
    dquote = {
        ['"'] = { next = "any" },
        ["\\"] = { next = "dquote_backslash" },
    },
    dquote_backslash = {
        next = "dquote",
    },
    squote = {
        ["'"] = { next = "any" },
        ["\\"] = { next = "squote_backslash" },
    },
    squote_backslash = {
        next = "squote",
    },
    slash = {
        single_char = true,
        ["/"] = { add = " ", skip = true, next = "line_comment" },
        ["*"] = { add = " ", skip = true, next = "block_comment" },
        otherwise = { add = "/", next = "any" },
    },
    line_comment = {
        skip = true,
    },
    block_comment = {
        skip = true,
        ["*"] = { skip = true, next = "try_end_block_comment" },
        continue_line = true,
    },
    try_end_block_comment = {
        single_char = true,
        skip = true,
        ["/"] = { skip = true, next = "any" },
        otherwise = { next = "block_comment" },
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

function cpp.initial_processing(filename)
    local fd, err = io.open(filename, "rb")
    if not fd then
        return nil, err
    end
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
            if not backslash_buf then
                backslash_buf = {}
            end
            table.insert(backslash_buf, line:sub(1, len - 1))
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

                -- Some states just process one character
                if st.next then
                    state = st.next
                    i = i + 1
                    goto continue
                end

                -- Look for next character matching a state transition
                local n = nil
                if st.pattern then
                    if st.single_char then
                        n = line:sub(i,i):find(st.pattern)
                        if n then
                             n = i
                        end
                    else
                        n = line:find(st.pattern, i)
                    end
                end

                -- If none,
                if not n then
                    -- Try a default transition
                    if st.otherwise then
                        state = st.otherwise.next
                        -- Some transitions output the transition character
                        if st.otherwise.add then
                            if not buf then
                                buf = {}
                            end
                            table.insert(buf, st.otherwise.add)
                        end
                        i = i + 1
                        goto continue
                    end
                    -- output the rest of the string if we should
                    if not st.skip then
                        out = i == 1 and line or line:sub(i)
                    end
                    break
                end

                -- If we have a state transition,
                if not buf then
                    buf = {}
                end
                -- output everything up to the transition if we should
                if n > i and not st.skip then
                    table.insert(buf, line:sub(i, n - 1))
                end
                i = n + 1
                local ch = line:sub(n, n)

                -- output the transition character if we should
                if not st[ch].skip then
                    table.insert(buf, ch)
                end

                -- and move to the next state
                state = st[ch].next
            end

            -- If we ended in a non-line-terminating state
            if states[state].continue_line then
                if not buf then
                    buf = {}
                end
                -- buffer the output and keep going
                table.insert(buf, out)
            else
                -- otherwise, flush the buffer
                if buf then
                    table.insert(buf, out)
                    out = table.concat(buf)
                    buf = nil
                end
                -- output the string and reset the state.
                table.insert(output, {linenr, out})
                state = "any"
            end
        end
    end
    return output
end

return cpp
