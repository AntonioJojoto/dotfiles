return {
	{
		"stevearc/conform.nvim",
		event = "BufWritePre", -- uncomment for format on save
		opts = require("configs.conform"),
	},

	-- These are some examples, uncomment them if you want to see them work!
	{
		"neovim/nvim-lspconfig",
		config = function()
			require("configs.lspconfig")
		end,
	},
	{
		"williamboman/mason.nvim",
		opts = {
			ensure_installed = {
				"ts_ls",
				"pyright",
				"lua_ls",
				"clangd",
				"eslint",
				"cssls",
				"html",
			},
		},
	},
	{
		"windwp/nvim-ts-autotag",
		ft = {
			"javascript",
			"javascriptreact",
		},
		config = function()
			require("nvim-ts-autotag").setup()
		end,
	},

	{
		"nvim-treesitter/nvim-treesitter",
		ensure_installed = {
			"vim",
			"lua",
			"vimdoc",
			"html",
			"css",
			"c",
			"python",
			"cpp",
			"bash",
			"cpp",
			"javascript",
			"typescript",
		},
	},
	"ThePrimeagen/vim-be-good",
	{
		"ThePrimeagen/harpoon",
		branch = "harpoon2",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
}
