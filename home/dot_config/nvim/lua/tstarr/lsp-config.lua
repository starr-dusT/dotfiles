return {
	"neovim/nvim-lspconfig",
	lazy = false,
	dependencies = {
		{ "ms-jpq/coq_nvim", branch = "coq" },
		{ "ms-jpq/coq.artifacts", branch = "artifacts" },
		{ "ms-jpq/coq.thirdparty", branch = "3p" },
	},
	init = function()
		vim.g.coq_settings = {
			auto_start = "shut-up",
		}
	end,
	config = function()
		vim.lsp.enable("nixd")
		vim.lsp.config("nixd", {
			cmd = { "nixd" },
			settings = {
				nixd = {
					nixpkgs = {
						-- For flake.
						-- This expression will be interpreted as "nixpkgs" toplevel
						-- Nixd provides package, lib completion/information from it.
						-- Resource Usage: Entries are lazily evaluated, entire nixpkgs takes 200~300MB for just "names".
						-- Package documentation, versions, are evaluated by-need.
						expr = "import (builtins.getFlake(toString ./.)).inputs.nixpkgs { }",
					},
					formatting = {
						command = { "nixfmt" }, -- or nixfmt or nixpkgs-fmt
					},
					options = {
						nixos = {
							expr = "let flake = builtins.getFlake(toString ./.); in flake.nixosConfigurations.nz.options",
						},
						home_manager = {
							expr = 'let flake = builtins.getFlake(toString ./.); in flake.homeConfigurations."sab@mbp16".options',
						},
						darwin = {
							expr = "let flake = builtins.getFlake(toString ./.); in flake.darwinConfigurations.mbp16.options",
						},
					},
				},
			},
		})
		vim.diagnostic.config({
			virtual_lines = {
				current_line = true,
			},
		})
	end,
}
