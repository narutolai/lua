local o1 = setmetatable({}, {__mode = "k"})
local o2 = setmetatable({}, {__mode = "k"})
local v1 = {}
local v2 = {}
local v3 = {}
 
o1[v1] = v2
o2[v2] = v3
 
local t = setmetatable({[v1] = "aaa"}, {__gc = function ( )
	print("__gc function call ...")
end})
 
v1 = nil
v2 = nil
v3 = nil
t = nil
 
collectgarbage()

 
--[[
运行结果:
__gc function call ...
------------- o1:
== o1   table: 00000000001b98d0 table: 00000000001b9a50
------------- o2:
== o2   table: 00000000001b9a50 table: 00000000001b9ad0
------------- o1:
------------- o2:
]]
