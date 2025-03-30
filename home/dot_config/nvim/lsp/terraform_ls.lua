return {
  cmd = { 'terraform-ls', 'serve' },
  filetypes = { 'terraform', 'tf' },
  settings = {
    validate = false,
    format = { enable = true },
    completion = true,
    hover = true,
  }
}
