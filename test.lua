-- --所有使用到匹配的地方的代码相关
-- --battle节点应该也有调用match节点的


-- --1.game节点调用match节点的地方

-- --1.这个玩家下线了会调用一下，然后这个函数做的是检测玩家如果在房间，就会去发一下退出房间请求
-- function player:on_offline_froom()
-- 	if self.room_service == nil then
-- 		return
-- 	end

-- 	local ret = self:call_match(froom_mgr, 'room_player_quit_room_req_tos', self.userid)
-- 	if ret == true then
-- 		log.info('player quit room uesrid =', self.userid)
-- 		self:set_room_svr(nil)
-- 	end
-- end


-- --2.匹配请求里
-- function player:room_match_req_tos(msg)
-- 	--................
-- 	local ret,online,battles = self:call_match(match_center, 'match', msg, info)
-- 	--................
-- end

-- --3.指引匹配请求
-- function player:room_match_guide_req_tos()
-- 	--.............
-- end

-- --4.好友创建房间
-- function player:room_create_room_tos(msg)
-- 	local ret, rs, tcfg = self:call_match(froom_mgr, 'create', cfg, info)
-- end

-- --5.进入好友房间
-- function player:room_player_enter_req_tos(msg)
-- 	local roominfo = self:call_match(froom_mgr, 'check_room_info_by_rid', msg.roomid)
-- 	local ret, rs, tcfg = self:call_match(froom_mgr, 'enter', msg.roomid, info)
-- end


-- --6.好友房随机匹配--
-- function player:room_player_froom_match_req_tos(msg)
-- 	local ret = self:call_match(froom_mgr, 'rand_match', self.userid)
-- 	if not ret then
-- 		self:send_result('room_player_froom_match_req_toc', 'ROOM_NOT_EXIST')
-- 		return
-- 	end
-- 	self:send_pack('room_player_froom_match_req_toc', {})
-- end

-- --7.取消好友房随机匹配
-- function player:room_froom_cancel_match_req_tos(msg)
-- 	self:call_match(froom_mgr, 'cancel_rand_match', self.userid)
-- 	self:send_pack('room_froom_cancel_match_req_toc', {})
-- end


-- --8.修改房间规则
-- function player:room_change_room_rule_tos(msg)
--     if self.room_cfg and self.room_cfg.config_id == 999 then --组队房
--         forward_client_op2_ex(self, 'room_change_room_rule_tos',msg)
--         return
--     end
-- 	local err, rs, tcfg = self:call_match(froom_mgr, 'change_room_rule', self.userid, msg)
-- 	if err ~= 'SYS_SUCCESS' then
-- 		self:send_result('room_change_room_rule_toc', err)
-- 	else
-- 		self:send_pack('room_change_room_rule_toc', msg)
-- 		forward_client_msg(self, 'room_change_room_rule_toc', msg)
-- 	end
-- end


-- --9.

-- --.取消匹配函数里 
-- -- 取消匹配
-- function player:room_cancel_match_tos()
-- 	self:cancel_match()
-- 	self:send_pack('room_cancel_match_toc', {})
-- end
-- --主要是在这里调用
-- function player:cancel_match()
-- 	if self.matching then
-- 		self.matching = nil
-- 		local canceled = self:call_match(match_center, 'cancel_match', self.userid)
-- 		if canceled then
-- 			self:return_room_price()
-- 			log.info('cancel match success userid =', self.userid, 'matching =', self.matching, "room_service =", self.room_service)
-- 			return true
-- 		else
-- 			log.info('cancel match failed userid =', self.userid, 'matching =', self.matching, "room_service =", self.room_service)
-- 			return false
-- 		end
-- 	end
-- end

-- --退出房间
-- function player:room_player_quit_room_req_tos()
-- 	if self.room_service == nil then
-- 		self:send_result('room_player_quit_room_req_toc', 'ROOM_QUIT_ROOM_REQ_ROOM_NOT_EXIST')
-- 		return
-- 	end

-- 	local ret = self:call_match(froom_mgr, 'room_player_quit_room_req_tos', self.userid)
-- 	if ret == true then
-- 		log.info('player quit room uesrid=', self.userid)
-- 		self:set_room_svr(nil)
-- 	end
-- end

-- --踢出玩家
-- function player:room_kick_out_player_tos(msg)
-- 	if self.room_service == nil then
-- 		self:send_result('room_kick_out_player_toc', 'ROOM_NOT_EXIST')
-- 		return
-- 	end

-- 	local ret = self:call_match(froom_mgr, 'kick_out', self.userid,msg.userid)
-- 	if ret ~= true then
-- 		self:send_result('room_kick_out_player_toc',ret)
-- 	end
-- end

-- -- 获取每个房间的人数
-- function player:player_get_match_count_tos()
-- 	local ret = self:call_match(match_center, 'get_match_and_match_up_count')
-- 	if not ret then
-- 		self:send_result('player_get_match_count_toc', 'MATCH_COUNT_GET_MATCH_COUNT_FAIL')
-- 		return
-- 	end

-- 	local count_table = {}
-- 	for config_id, count in pairs(ret) do
-- 		table.insert(count_table, {config_id = config_id, count = count})
-- 	end

-- 	self:send_pack('player_get_match_count_toc', {count_list = count_table})
-- end

-- allgc-->{}-->{}-->{}

-- gray 
local tbl = setmetatable({},{__mode="k"})
local key =  {}
local value = {}
print(key,value)
--键值互相引用
key.a = value
value.a = key
tbl[key] = value

--外部不引用了
key = nil 
value = nil 
collectgarbage()

for k,v in pairs(tbl) do
	print(k,v)
end