rex.bind("alt+shift+h", "pane.split", { direction = "right" })
rex.bind("alt+shift+v", "pane.split", { direction = "down" })
rex.bind("alt+shift+t", "client.tab.new")
rex.bind("ctrl+shift+t", "client.tab.new")
rex.bind("alt+shift+left", "pane.focus", { direction = "left" })
rex.bind("alt+shift+right", "pane.focus", { direction = "right" })
rex.bind("alt+shift+up", "pane.focus", { direction = "up" })
rex.bind("alt+shift+down", "pane.focus", { direction = "down" })
rex.bind("alt+tab", "session.next")
rex.bind("alt+shift+tab", "session.previous")

-- client.font.increase/decrease take no step, so queue them twice to match Ghostty's :2
rex.action{
  name  = "font_up_2",
  title = "Increase Font Size by 2",
  run   = function()
    rex.client.queue("client.font.increase")
    rex.client.queue("client.font.increase")
  end,
}
rex.action{
  name  = "font_down_2",
  title = "Decrease Font Size by 2",
  run   = function()
    rex.client.queue("client.font.decrease")
    rex.client.queue("client.font.decrease")
  end,
}
rex.bind("alt+shift+r", "font_up_2")
rex.bind("alt+shift+n", "font_down_2")

rex.bind("shift+enter", function()
  rex.block.call("com.superlogical.terminal", "write", { data = "\27\r" })
  return true
end)
