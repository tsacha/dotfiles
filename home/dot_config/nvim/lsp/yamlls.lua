return {
  cmd = { 'yaml-language-server', '--stdio' },
  filetypes = { 'yml', 'yaml' },
  settings = {
    validate = false,
    format = { enable = true },
    completion = true,
    hover = true,
  }
}
