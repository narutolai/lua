local function test()
	coroutine.yield()
end

local co = coroutine.create(test)
coroutine.resume(co)
coroutine.resume(co)
