local Remap = require("tstarr.keymap")
local nnoremap = Remap.nnoremap
local inoremap = Remap.inoremap

local function config(_config)
	return vim.tbl_deep_extend("force", {
		on_attach = function()
            local opts = { buffer = true };
			nnoremap("gd", function() vim.lsp.buf.definition() end, opts)
			nnoremap("K", function() vim.lsp.buf.hover() end, opts)
			nnoremap("<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
			nnoremap("<leader>vd", function() vim.diagnostic.open_float() end, opts)
			nnoremap("[d", function() vim.diagnostic.goto_next() end, opts)
			nnoremap("]d", function() vim.diagnostic.goto_prev() end, opts)
			nnoremap("<leader>vca", function() vim.lsp.buf.code_action() end, opts)
			nnoremap("<leader>vco", function() vim.lsp.buf.code_action({
                filter = function(code_action)
                    if not code_action or not code_action.data then
                        return false
                    end

                    local data = code_action.data.id
                    return string.sub(data, #data - 1, #data) == ":0"
                end,
                apply = true
            }) end, opts)
			nnoremap("<leader>vrr", function() vim.lsp.buf.references() end, opts)
			nnoremap("<leader>vrn", function() vim.lsp.buf.rename() end, opts)
			inoremap("<C-h>", function() vim.lsp.buf.signature_help() end, opts)
		end,
	}, _config or {})
end

require("lspconfig").tsserver.setup(config())
