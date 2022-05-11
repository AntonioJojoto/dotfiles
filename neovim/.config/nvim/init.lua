local cmd = vim.cmd
local g = vim.g
local o = vim.o
local bo = vim.bo
local wo = vim.wo

-- General Settings, in LUA
bo.expandtab = true
bo.shiftwidth = 2
bo.softtabstop = 2

-- Keybindings
-- Set using vim.api.nvim_set_keymap({mode},{keymap},{mapped to}, {options})


-- Package Manager
-- Run yay -S nvim-packer-git
require('packer').startup(function()
	use 'wbthomason/packer.nvim'
end)
