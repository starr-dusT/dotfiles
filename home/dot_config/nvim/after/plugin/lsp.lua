local Remap = require("tstarr.keymap")
local nnoremap = Remap.nnoremap
local inoremap = Remap.inoremap
local cmp = require('cmp')

cmp.setup({
    snippet = {
      expand = function(args)
        require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      end,
    },
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        else
          fallback()
        end
      end, { "i" }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        else
          fallback()
        end
      end, { "i" }),
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'luasnip' }, -- For luasnip users.
    }, {
        { name = 'buffer',
          option = {
            get_bufnrs = function()
              local bufs = {}
              for _, win in ipairs(vim.api.nvim_list_wins()) do
                bufs[vim.api.nvim_win_get_buf(win)] = true
              end
              return vim.tbl_keys(bufs)
            end
          }
        },
    })
})

cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' },
    { name = 'buffer' }
  }, {
    {
      name = 'cmdline',
      option = {
        ignore_cmds = { 'Man', '!' }
      }
    }
  })
})

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
