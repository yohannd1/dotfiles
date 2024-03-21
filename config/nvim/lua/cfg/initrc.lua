_G.dummy = _G.dummy or {}
local dummy = _G.dummy

local vim = _G.vim
local ucm = _G.useConfModule
local utils = ucm("utils")
_G._utils = utils

local mapKey = vim.api.nvim_set_keymap

local log_list = {}
function dummy.log(msg)
  table.insert(log_list, os.date("%Y%m%d %H:%M :: ") .. msg)
end
function dummy.showLogs()
  for _, log in ipairs(log_list) do
    print(log)
  end
end

mapKey("i", "<CR>", "v:lua.dummy.imap_enter_handle()", {expr = true, noremap = true})
mapKey("i", "<BS>", "v:lua.dummy.imap_bs_handle()", {expr = true, noremap = true})

local function hasIntegerRepr(num)
  return tostring(num):match("^%d+$") ~= nil
end

local function moveCursorHorizontal(offset)
  assert(
    hasIntegerRepr(math.abs(offset)),
    string.format(
      "Absolute of `offset` (%s) has no integer representation",
      math.abs(offset)
    )
  )

  if offset == 0 then
    return 0
  end

  local direction_str = (offset > 0) and "l" or "h"
  local direction_signal = (offset > 0) and 1 or -1

  for i = 1, math.abs(offset) do
    local ccol = vim.fn.col(".")
    vim.cmd("normal! " .. direction_str)
    if vim.fn.col(".") == ccol then
      -- Didn't move at all this round - it's the end of the line. Let's return already then.
      return i * direction_signal
    end
  end

  return offset
end

local function columnAtCharOffset(offset)
  local moved = moveCursorHorizontal(offset)
  if moved ~= offset then
    moveCursorHorizontal(-moved)
  else
    local ccol = vim.fn.col(".")
    moveCursorHorizontal(-offset)
    return ccol
  end
end

local function makeAddTxt(after_cursor)
  return function(text)
    assert(text ~= nil, "Argument `text` must not be nil")

    local line = vim.fn.getline(".")
    local start_offset = after_cursor and 0 or -1
    local divide_point = columnAtCharOffset(start_offset) or 0

    vim.fn.setline(
      ".",
      table.concat({
        vim.fn.strpart(line, 0, divide_point), -- FIXME: why is this still splitting bytes in half?
        text,
        vim.fn.strpart(line, divide_point),
      }, "")
    )
  end
end

-- local addTextAfterCursor = makeAddTxt(true)
-- local addTextBeforeCursor = makeAddTxt(false)

do
  local telescope = require("telescope")
  local conf = require('telescope.config').values
  local finders = require('telescope.finders')
  local pickers = require('telescope.pickers')
  local themes = require('telescope.themes')
  local action_state = require('telescope.actions.state')
  local actions = require("telescope.actions")

  telescope.setup {
    defaults = {
      layout_strategy = "bottom_pane",
      layout_config = {
          bottom_pane = {
            prompt_position = "bottom",
          },
      },
      mappings = {
        i = {
          ["<Esc>"] = actions.close,
          ["<C-j>"] = actions.move_selection_next,
          ["<C-k>"] = actions.move_selection_previous,
          ["<C-m>"] = actions.select_default,

          ["<ScrollWheelUp>"] = actions.move_selection_previous,
          ["<ScrollWheelDown>"] = actions.move_selection_next,
        },
        n = {}
      },
      scroll_strategy = "cycle",
    },
    pickers = {
      buffers = {
        sort_lastused = true,
        theme = "dropdown",
        previewer = false,
      },
      find_files = {
        theme = "dropdown"
      }
    },
  }

  local main_theme = themes.get_ivy()

  dummy.open_today_journal = function()
    local proc = assert(io.popen("acw-today-journal", "r"))
    local entry = proc:read("*a")
    proc:close()

    if entry == nil or entry == "" then
      print("Failed to open today's journal")
    else
      vim.cmd("e " .. vim.g.wiki_dir .. "/" .. entry:gsub("^%s*", ""):gsub("%s*$", "") .. ".wiki")
    end
  end

  -- Search and open on wiki
  dummy.open_wiki_file = function(opts, command)
    opts = opts or {}
    utils.overrideTableWith(opts, main_theme)
    command = command or {"acw-list-titles"}

    pickers.new(opts, {
      prompt_title = "Search on wiki",
      finder = finders.new_oneshot_job(command, opts),
      sorter = conf.generic_sorter(opts),
      attach_mappings = function()
        actions.select_default:replace(function(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          actions.close(prompt_bufnr)

          if selection then
            vim.cmd("e " .. vim.g.wiki_dir .. "/" .. vim.fn.split(selection[1])[1] .. ".acr")
          end
        end)

        return true
      end,
    }):find()
  end

  dummy.insert_wiki_file = function(opts)
    opts = opts or {}
    utils.overrideTableWith(opts, main_theme)

    local repr_string = opts.after_cursor and "after" or "before"

    pickers.new(opts, {
      prompt_title = "Insert wiki file: " .. repr_string,
      finder = finders.new_oneshot_job({"acw-list-titles"}, opts),
      sorter = conf.generic_sorter(opts),
      attach_mappings = function()
        actions.select_default:replace(function(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          actions.close(prompt_bufnr)

          if selection then
            local link = "@ref(" .. vim.fn.split(selection[1])[1] .. ")"
            makeAddTxt(opts.after_cursor)(link)
            vim.cmd(string.format("normal! %dl", opts.after_cursor and 2 + link:len() or 1))
          end
        end)

        return true
      end,
    }):find()
  end

  dummy.open_recent = function(opts)
    opts = opts or {}
    utils.overrideTableWith(opts, main_theme)

    pickers.new(opts, {
      prompt_title = "Open recent file",
      finder = finders.new_oneshot_job({"filehist", "list"}, opts),
      sorter = conf.generic_sorter(opts),
      attach_mappings = function()
        actions.select_default:replace(function(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          actions.close(prompt_bufnr)

          if selection then
            vim.cmd("e " .. selection[1])
          end
        end)

        return true
      end,
    }):find()
  end
end

-- TODO: inside neovim, replace the $EDITOR with a wrapper script that connects
-- to the current neovim instance, opens a buffer, and waits for the buffer to
-- unload before exiting.
--
-- Waiting for neovim to implement --remote-wait to do this. See
-- https://neovim.io/doc/user/remote.html
--
-- This might be possible without --remote-wait already, by using a BufWipeout
-- hook, but I'd need to figure out how that works and then make the wrapper
-- script wait till neovim responds back to it with a message saying the buffer
-- closed. Might not be too hard though?

-- Load configuration from other files
ucm("general")
ucm("statusline")
ucm("keybindings")
ucm("filetypes")

-- vim: sw=2 et
