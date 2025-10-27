return {
    "neovim/nvim-lspconfig",
    lazy = false,
    dependencies = {
        { "ms-jpq/coq_nvim",       branch = "coq" },
        { "ms-jpq/coq.artifacts",  branch = "artifacts" },
        { "ms-jpq/coq.thirdparty", branch = "3p" },
    },
    init = function()
        vim.g.coq_settings = {
            auto_start = "shut-up",
        }
    end,
    config = function()
        vim.lsp.enable({ "nixd", "lua_ls" })
        vim.diagnostic.config({
            virtual_lines = {
                current_line = true,
            },
        })
    end,
}
