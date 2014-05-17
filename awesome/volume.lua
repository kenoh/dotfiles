volume_widget = widget({ type = "textbox", name = "tb_volume", align = "right" })
volume_widget:buttons({
	button({}, 4, function () awful.util.spawn("amixer set Master 5%+") end),
	button({}, 5, function () awful.util.spawn("amixer set Master 5%-") end),
	button({}, 2, function () awful.util.spawn("aminer set Master toggle") end),
	button({}, 3, function () awful.util.spawn(terminal .. " -e alsamixer") end)
	})

function update_volume(widget)
	local fd = io.popen("amixer sget Master")
	local status = fd:read("*all")
	fd:close()
	if not status then
		return
	end

	local volume = tonumber(string.match(status, "(%d?%d?%d)%%"))
	if type(volume)=="number" then
		volume = volume/100
	else
		return
	end
	-- volume = string.format("% 3d", volume)

	status = string.match(status, "%[(o[^%]]*)%]")

	-- starting colour
	local sr, sg, sb = 0x3F, 0x3F, 0x3F
	-- ending colour
	local er, eg, eb = 0xDC, 0xDC, 0xCC

	local ir = volume * (er - sr) + sr
	local ig = volume * (eg - sg) + sg
	local ib = volume * (eb - sb) + sb
	interpol_colour = string.format("%.2x%.2x%.2x", ir, ig, ib)
	if string.find(status, "on", 1, true) then
		volume = " <span> "..tostring(volume*100).." </span>"
	else
		volume = " <span color='red'> M </span>"
	end
	widget.text = volume
end

update_volume(volume_widget)
awful.hooks.timer.register(0.1, function () update_volume(volume_widget) end)
