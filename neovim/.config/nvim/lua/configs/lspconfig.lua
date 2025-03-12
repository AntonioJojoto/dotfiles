-- load defaults i.e lua_lsp
require("nvchad.configs.lspconfig").defaults()

local lspconfig = require "lspconfig"

-- EXAMPLE
local servers = { "html", "cssls" , "clangd", "pyright", "ts_ls","eslint"}
local nvlsp = require "nvchad.configs.lspconfig"

-- lsps with default config
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = nvlsp.on_attach,
    on_init = nvlsp.on_init,
    capabilities = nvlsp.capabilities,
  }
end
-- configuring single server, example: typescript
-- lspconfig.ts_ls.setup {
--   on_attach = nvlsp.on_attach,
--   on_init = nvlsp.on_init,
--   capabilities = nvlsp.capabilities,
-- }
--
-- require'lspconfig'.ts_ls.setup{
--   init_options = {
--     -- plugins = {
--     --   {
--     --     name = "@vue/typescript-plugin",
--     --     location = "/usr/local/lib/node_modules/@vue/typescript-plugin",
--     --     languages = {"javascript", "typescript", "vue"},
--     --   },
--     -- },
--   },
--   filetypes = {
--     "javascript",
--     "typescript",
--     "javascriptreact"
--   }
-- }
