-- Package Manager
-- Run yay -S nvim-packer-git
require('packer').startup(function()
        use 'wbthomason/packer.nvim'
        ------------------------------------------------------------------------
	--------------------------------- GUI ----------------------------------
	------------------------------------------------------------------------
	-- Status line
        use {
                'hoob3rt/lualine.nvim',
                requires = {'kyazdani42/nvim-web-devicons', opt = true}
        }
	-- use {
		-- 'famiu/feline.nvim',
		-- requires = {'kyazdani42/nvim-web-devicons', opt = true}
	-- }
	-- Bar prettifier
	-- use {'romgrk/barbar.nvim', requires = {'kyazdani42/nvim-web-devicons'}}
	-- The best theme of them all
	use {"npxbr/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
end)

