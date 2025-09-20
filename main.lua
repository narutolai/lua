local myUdata = require 'myUdata'
for k, v in pairs(myUdata) do
print(k,v)
end
local udata = myUdata.init()
print(udata)
local a = udata.getval()
print(a)