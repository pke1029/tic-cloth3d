-- title:  cloth3d
-- author: pke1029
-- desc:   3d cloth simulation
-- script: lua

WIDTH = 240
HEIGHT = 136
W2 = 120
H2 = 68
PI = 3.1415
sin = math.sin
cos = math.cos
max = math.max
min = math.min
abs = math.abs
sqrt = math.sqrt
sqrt2 = sqrt(2)
ceil = math.ceil
floor = math.floor
random = math.random
sysmouse = mouse 
systime = time

mathFun = {

	between = function(x, a, b)
		return x >= a and x < b
	end,

	clamp = function(x, a, b)
		if mathFun.between(x, a, b) then return x end
		if x < a then return a end
		if x >= b then return b end
	end,

	bool2int = function(a)
		return a and 1 or 0
	end,

	mod = function(x, a)
		return x - floor(x/a)*a
	end,

	sign = function(x)
	    return x > 0 and 1 or -1
	end,

	maprange = function(x, a1, b1, a2, b2)
		return (x - a1)/(b1 - a1)*(b2 - a2) + a2
	end,

	isnan = function(x)
		return x ~= x
	end,

	round = function(x)
		return math.floor(x + 0.5)
	end,

}

stack = {

	new = function(n)
		local t = {n=n}
		for i = 1,n do
			table.insert(t, nil)
		end
		setmetatable(t, stack.mt)
		return t
	end,

	mt = {

		__index = {

			push = function(self, a)
				for i = 1,self.n-1 do
					self[i] = self[i+1]
				end
				self[self.n] = a 
			end

		}

	}

}

mouse = {

	x = 120,
	y = 68,
	dx = 0,
	dy = 0,
	md = false,
	mp = false,
	mu = false,
	right = false,
	middle = false,
	scroll = 0,

	update = function(self)
		local mx, my, md, middle, right, scrollx, scrolly = sysmouse()
		self.dx = mx - self.x
		self.dy = my - self.y
		self.x = mx
		self.y = my
		self.mp = not self.md and md
		self.mu = self.md and not md
		self.md = md
		self.right = right
		self.middle = middle
		self.scroll = scrolly
	end,

	draw = function(self)
		line(0, self.y, WIDTH, self.y, 6)
		line(self.x, 0, self.x, HEIGHT, 6)
		print('(' .. self.x .. ',' .. self.y .. ')', self.x+2, self.y+2, 6)
		local w = print(self.x, 0, -6)
		print(self.x, self.x-w, 0, 6)
		print(self.y, 0, self.y-6 , 6)
	end

}

time = {

	t = 0,
	dt = 0,
	last_t = 0,
	count = 0,
	fps = 60,

	update = function(self)
		local t = systime()
		self.dt = t - self.t
		self.t = t
		if t - self.last_t > 1000 then
			self.last_t = t
			self.fps = self.count
			self.count = 0
		else
			self.count = self.count + 1 
		end
	end,

	draw = function(self)
		print(self.fps, 228, 1, 4, true)
	end

}

vec3d = {

	new = function(x, y, z)
		local v = {x, y, z}
		setmetatable(v, vec3d.mt)
		return v
	end,

	mt = {

		__add = function (u, v)
			return vec3d.new(u[1]+v[1], u[2]+v[2], u[3]+v[3])
		end,

		__sub = function(u, v)
			return vec3d.new(u[1]-v[1], u[2]-v[2], u[3]-v[3])
		end,

		__mul = function(k, v)
			if type(k) == "table" then
				return vec3d.new(k[1]*v[1], k[2]*v[2], k[3]*v[3])
			else
				return vec3d.new(k*v[1], k*v[2], k*v[3])
			end
		end,

		__div = function(v, k)
			if type(k) == "table" then
				return vec3d.new(v[1]/k[1], v[2]/k[2], v[3]/k[3])
			else
				return vec3d.new(v[1]/k, v[2]/k, v[3]/k)
			end
		end,

		__pow = function(v, k)
			return vec3d.new(v[1]^k, v[2]^k, v[3]^k)
		end,

		__eq = function(u, v)
			return u[1] == v[1] and u[2] == v[2] and u[3] == v[3]
		end,

		__tostring = function(v)
			return "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__concat = function(s, v)
			return s .. "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__index = {

			normalise = function(self)
				local r = vec3d.norm(self)
				self[1] = self[1] / r
				self[2] = self[2] / r
				self[3] = self[3] / r
				return self
			end,

			ortho_proj = function(self, camera)
				local x = vec3d.dot(self, camera.x)
				local y = vec3d.dot(self, camera.y)
				return x, -y
			end,

			pers_proj = function(self, camera)
				local x = vec3d.dot(self, camera.x)
				local y = vec3d.dot(self, camera.y)
				local r = vec3d.dist(self, camera.o)
				x = camera.depth * x / r 
				y = camera.depth * y / r 
				return x, -y
			end,

			proj = function(self, camera)
				if camera.persective then 
					return self:pers_proj(camera)
				else 
					return self:ortho_proj(camera) 
				end
			end,

			rotate = function(self, ax, ay, az)
				local cx = cos(ax)
				local sx = sin(ax)
				local cy = cos(ay)
				local sy = sin(ay)
				local cz = cos(az)
				local sz = sin(az)
				local x1 = self[1]
				local y1 = self[2] * cx - self[3] * sx
				local z1 = self[2] * sx + self[3] * cx
				local x2 = x1 * cy + z1 * sy
				local y2 = y1
				local z2 = -x1 * sy + z1 * cy
				self[1] = x2 * cz - y2 * sz
				self[2] = x2 * sz + y2 * cz
				self[3] = z2
				return self
			end,

			qrotate = function(self, theta, u)
				u = u / vec3d.norm(u)
				local c = cos(theta/2)
				local s = sin(theta/2)
				local k = vec3d.dot(s*u, self)
				local w = vec3d.cross(s*u, self)
				local v = 2*k*s*u + (c*c-s*s)*self + 2*c*w
				self[1] = v[1]
				self[2] = v[2]
				self[3] = v[3]
				return self
			end,

			draw_highlight = function(self, x0, y0)
				local x, y = self:proj(camera)
				circb(x+x0, y+y0, 2, 4)
			end,
		}
	},

	norm = function(v)
		return sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3])
	end,

	dot = function(u, v)
		return u[1]*v[1] + u[2]*v[2] + u[3]*v[3]
	end,

	cross = function(u, v)
		return vec3d.new(u[2]*v[3]-v[2]*u[3], v[1]*u[3]-u[1]*v[3], u[1]*v[2]-v[1]*u[2])
	end,

	avg = function(u)
		return (u[1] + u[2] + u[3]) / 3
	end,

	dist = function(u, v)
		return vec3d.norm(u - v)
	end,

	mid = function(u, v)
		return (u + v) / 2
	end,

	line = function(x0, y0, u, v, camera, col)
		local x1, y1 = u:proj(camera)
		local x2, y2 = v:proj(camera)
		line(x1+x0, y1+y0, x2+x0, y2+y0, col)
	end,

	ysort = function(u, v)
		return u[2] < v[2]
	end

}

color = {

	addr = 0x03FC0,

	new = function(r, g, b)
		local c = vec3d.new(r, g, b)
		return c
	end,

	set = function(c, col)
		local addr = color.addr + 3*col
		poke(addr, c[1])
		poke(addr+1, c[2])
		poke(addr+2, c[3])
	end,

	interp = function(a, c1, c2)
		return a*c1 + (1-a)*c2
	end

}

camera = {

	o = vec3d.new(0, 0, 100),
	x = vec3d.new(1, 0, 0),
	y = vec3d.new(0, 1, 0),
	ax = -0.5,
	ay = 0.5,
	depth = 80,
	persective = true,
	r = 100,
	trackball = false,

	load = function(self)
		self.o:rotate(-0.5, 0.5, 0)
		self.x:rotate(-0.5, 0.5, 0)
		self.y:rotate(-0.5, 0.5, 0)
	end,

	update = function(self)
		
		if mouse.x < HEIGHT and cloth.operation == 1 then
			if mouse.scroll ~= 0 then
				self.r = mathFun.clamp(self.r - 4*mouse.scroll, 80, 200)
				self.o = self.r * self.o:normalise()
			end
			if mouse.md then
				-- rotate (turn table)
				self.ax = mathFun.clamp(self.ax - 0.03*mouse.dy, -PI/2, PI/2)
				self.ay = self.ay - 0.03*mouse.dx
				self.trackball = false
			elseif mouse.right then 
				-- rotate (track ball)
				local u = - mouse.dy*self.x - mouse.dx*self.y
				local theta = vec3d.norm(u) * 0.03
				if theta ~= 0 then 
					self.o:qrotate(theta, u)
					self.x:qrotate(theta, u)
					self.y:qrotate(theta, u)
				end
				self.trackball = true
			end
		end

		-- WASD control
		if key(23) then 
			self.ax = self.ax + 0.03 
			self.trackball = false 
		end
		if key(19) then 
			self.ax = self.ax - 0.03
			self.trackball = false 
		end
		if key(1) then 
			self.ay = self.ay + 0.03
			self.trackball = false 
		end
		if key(4) then 
			self.ay = self.ay - 0.03
			self.trackball = false 
		end

		if self.trackball == false then
			self.o = vec3d.new(0, 0, self.r):rotate(self.ax, self.ay, 0)
			self.x = vec3d.new(1, 0, 0):rotate(0, self.ay, 0)
			self.y = vec3d.new(0, 1, 0):rotate(self.ax, self.ay, 0)
		end

		-- reset camera
		if keyp(27) then
			self.o = vec3d.new(0, 0, 100)
			self.x = vec3d.new(1, 0, 0)
			self.y = vec3d.new(0, 1, 0)
			self.o:rotate(-0.5, 0.5, 0)
			self.x:rotate(-0.5, 0.5, 0)
			self.y:rotate(-0.5, 0.5, 0)
			self.ax = -0.5
			self.ay = 0.5
			self.r = 100
		end
		if keyp(32) then
			self.persective = not self.persective
		end
	end,

	draw = function(self)
		local x = self.o[1]
		local y = self.o[2]
		circ(x+H2, y+H2, 1, 11)
	end,

}

edge = {

	new = function(v1, v2, col)
		if col == nil then col = 12 end
		local e = {v1, v2, o=vec3d.mid(v1, v2), col=col}
		setmetatable(e, edge.mt)
		return e
	end,

	mt = {

		__index = {

			update = function(self)
				self.o = vec3d.mid(self[1], self[2])
			end,

			draw = function(self, x, y)
				local x1, y1 = self[1]:proj(camera)
				local x2, y2 = self[2]:proj(camera)
				line(x1+x, y1+y, x2+x, y2+y, self.col)
			end

		}

	},

	sort = function(e1, e2)
		local r1 = vec3d.dist(e1.o, camera.o)
    	local r2 = vec3d.dist(e2.o, camera.o)
		return r1 > r2
	end

}

light = {

	new = function(ax, az)
		if ax == nil then ax = 0 end
		if az == nil then az = 0 end
		local a = {y=vec3d.new(0,1,0), ax=ax, az=az, s=1}
		setmetatable(a, light.mt)
		return a
	end,

	mt = {

		__index = {

			update = function(self)
				-- self.o = vec3d.new(0,10,0):rotate(self.ax, 0, self.az)
				self.y = vec3d.new(0,1,0):rotate(self.ax, 0, self.az)
			end,

			brightness = function(self, n, a, b)
				if mathFun.isnan(n[1]) then return a end
				local x = self.s * vec3d.dot(n, self.y)
				x = mathFun.clamp(x, 0, 1)
				return mathFun.maprange(x, 0, 1, a, b) + 0.5
			end,

			draw = function(self)

			end,

		}
	}

}

face = {

	new = function(v1, v2, v3, v4, col)
		if col == nil then col = 12 end
		local o = (v1 + v2 + v3 + v4)/4
		local n = vec3d.cross(v4-v1, v2-v3):normalise()
		local f = {v1, v2, v3, v4, o=o, n=n, col=col}
		setmetatable(f, face.mt)
		return f
	end,

	mt = {

		__index = {

			update = function(self)
				self.o = (self[1]+self[2]+self[3]+self[4])/4
				self.n = vec3d.cross(self[4]-self[1], self[2]-self[3]):normalise()
			end,

			draw = function(self, x0, y0)
				local x1, y1 = self[1]:proj(camera)
				local x2, y2 = self[2]:proj(camera)
				local x3, y3 = self[3]:proj(camera)
				local x4, y4 = self[4]:proj(camera)
				tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, self.col+8)
				tri(x2+x0, y2+y0, x3+x0, y3+y0, x4+x0, y4+y0, self.col+8)
			end,

			-- gouraud shading
			cdraw = function(self, x0, y0)
				local x1, y1 = self[1]:proj(camera)
				local x2, y2 = self[2]:proj(camera)
				local x3, y3 = self[3]:proj(camera)
				local x4, y4 = self[4]:proj(camera)
				local vc = self.vc
				textri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, vc[1], 0, vc[2], 0, vc[3], 0)
				textri(x2+x0, y2+y0, x3+x0, y3+y0, x4+x0, y4+y0, vc[2], 0, vc[3], 0, vc[4], 0)
			end

		}

	},

	sort = function(f1, f2)
		local r1 = vec3d.dist(f1.o, camera.o)
    	local r2 = vec3d.dist(f2.o, camera.o)
		return r1 > r2
	end,

	navg = function(faces)
		local n = vec3d.new(0, 0, 0)
		for i = 1,4 do
			if faces[i] ~= nil then
				n = n + faces[i].n
			end 
		end
		return n:normalise()
	end,

}

axes = {

	x = HEIGHT-16,
	y = 15,

	edges = {edge.new(vec3d.new(0, 0, 0), vec3d.new(8, 0, 0), 2),
             edge.new(vec3d.new(0, 0, 0), vec3d.new(0, 8, 0), 6),
             edge.new(vec3d.new(0, 0, 0), vec3d.new(0, 0, 8), 9)},

	draw = function(self)
		circ(self.x, self.y, 12, 15)
		table.sort(self.edges, edge.sort)
		for i,e in ipairs(self.edges) do
			local x1, y1 = e[1]:ortho_proj(camera)
			local x2, y2 = e[2]:ortho_proj(camera)
			line(self.x+x1, self.y+y1, self.x+x2, self.y+y2, e.col)
			rect(self.x+x2-1, self.y+y2-1, 3, 3, e.col)
		end
	end

}

sphere = {

	new = function(r, o, col)
		s = {r=r, o=o, col=col}
		setmetatable(s, sphere.mt)
		return s
	end,

	mt = {

		__index = {

			update = function(self, du)
				self.o = self.o + du
			end,

			draw = function(self, x0, y0)
				local x, y, r = self:proj(camera)
				circ(x+x0, y+y0, r-1, self.col)
			end,

			draw_wire = function(self, x0, y0)
				local x, y, r = self:proj(camera)
				circb(x+x0, y+y0, r-1, self.col)
			end,

			draw_highlight = function(self, x0, y0)
				local x, y, r = self:proj(camera)
				circb(x+x0, y+y0, r-1, 4)
			end,

			ortho_proj = function(self, camera)
				local x, y = self.o:ortho_proj(camera)
				return x, y, self.r
			end,

			pers_proj = function(self, camera)
				local x, y = self.o:pers_proj(camera)
				local c = vec3d.dist(self.o, camera.o)
				local a = self.r^2 / c 
				local b = sqrt(self.r^2 - a^2)
				local r = b/(c-a) * camera.depth
				return x, y, r 
			end,

			proj = function(self, camera)
				if camera.persective then
					return self:pers_proj(camera)
				else
					return self:ortho_proj(camera)
				end
			end,

			normal = function(self, x, y)
				local x0, y0, r = self:proj(camera)
				local z0 = sqrt(r^2 - (x-x0)^2 - (y-y0)^2)
				local n0 = vec3d.new(x-x0, y-y0, z0)
				return n
			end,

			cdraw = function(self, x0, y0)
				local x, y, r = self:proj(camera)
				circ(x+x0, y+y0, r-1, self.col)
			end

		}

	},

}

mat = {

	new = function(A)
		setmetatable(A, mat.mt)
		return A
	end,

	new_const = function(c, n, m)
		local A = {}
		for i = 1,n do
			A[i] = {}
			for j = 1,m do
				A[i][j] = c
			end
		end
		setmetatable(A, mat.mt)
		return A 
	end,

	mt = {

		__add = function(A, B)
			local n, m = A:size()
			C = {}
			if getmetatable(B) == mat.mt then 
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] + B[i][j]
					end
				end
			else
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] + B
					end
				end
			end
			return mat.new(C)
		end,

		__sub = function(A, B)
			local n, m = A:size()
			C = {}
			if getmetatable(B) == mat.mt then 
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] - B[i][j]
					end
				end
			else
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] - B
					end
				end
			end
			return mat.new(C)
		end,

		__mul = function(k, A)
			local n, m = A:size()
			B = {}
			if getmetatable(k) == mat.mt then
				for i = 1,n do
					B[i] = {}
					for j = 1,m do
						B[i][j] = k[i][j] * A[i][j]
					end
				end
			else
				for i = 1,n do
					B[i] = {}
					for j = 1,m do
						B[i][j] = k * A[i][j]
					end
				end
			end
			return mat.new(B)
		end,

		__div = function(A, k)
			local n, m = A:size()
			B = {}
			for i = 1,n do
				B[i] = {}
				for j = 1,m do
					B[i][j] = A[i][j] / k
				end
			end
			return mat.new(B)
		end,

		__index = {

			size = function(self)
				local n = #self
				local m = 0
				if n ~= 0 then
					m = #self[1]
				end
				return n, m
			end,

		}
	},

}

cloth = {

	l0 = 10,
	gravity = 5,
	k1 = 10,	-- Compression
	k2 = 10,	-- Shear
	k3 = 0,	    -- Bending
	damping = 0.5,
	dt = 1/10,
	shading = 2,
	operation = 2,

	new_lattice = function(w, h, x)
		local verts = {}
		local n, m = ceil(h/cloth.l0), ceil(w/cloth.l0)
		for i = 0,n do
			verts[i+1] = {}
			for j = 0,m do
				verts[i+1][j+1] = vec3d.new(j*cloth.l0, 0, i*cloth.l0) + x
			end
		end
		local A = {verts = mat.new(verts), 
				   vold = mat.new(verts),}
		setmetatable(A, cloth.mt)
		return A
	end, 

	mt = {

		__index = {

			draw = function(self)
				if cloth.shading == 1 then
					self:draw_wire()
				elseif cloth.shading == 2 then
					self:draw_occlude()
				elseif cloth.shading == 3 then
					self:draw_flat()
				else
					self:draw_smooth()
				end
			end,

			draw_wire = function(self)
				cloth.collision:draw_wire()
				local verts = self.verts
				local n, m = verts:size()
				if self.conn == nil then
					for i = 1,n do
						for j = 1,m do
							if j ~= m then vec3d.line(H2, H2, verts[i][j], verts[i][j+1], camera, 12) end
							if i ~= n then vec3d.line(H2, H2, verts[i][j], verts[i+1][j], camera, 12) end
						end
					end
				else
					for i = 1,n do
						for j = 1,m do
							if j ~= m and self.conn.hori[i][j] == 1 then vec3d.line(H2, H2, verts[i][j], verts[i][j+1], camera, 12) end
							if i ~= n and self.conn.verti[i][j] == 1 then vec3d.line(H2, H2, verts[i][j], verts[i+1][j], camera, 12) end
						end
					end
				end
			end, 

			draw_occlude = function(self)
				local edges = {}
				local verts = self.verts
				local n, m = verts:size()
				if self.conn == nil then
					for i = 1,n do
						for j = 1,m do
							if j ~= m then table.insert(edges, edge.new(verts[i][j], verts[i][j+1])) end
							if i ~= n then table.insert(edges, edge.new(verts[i][j], verts[i+1][j])) end
						end
					end
				else
					for i = 1,n do
						for j = 1,m do
							if j ~= m and self.conn.hori[i][j] == 1 then table.insert(edges, edge.new(verts[i][j], verts[i][j+1])) end
							if i ~= n and self.conn.verti[i][j] == 1 then table.insert(edges, edge.new(verts[i][j], verts[i+1][j])) end
						end
					end
				end
				table.insert(edges, cloth.collision.sphere)
				table.sort(edges, edge.sort)
				for i,e in ipairs(edges) do
					e:draw(H2, H2)
				end
			end,

			draw_flat = function(self)
				local faces = {}
				local verts = self.verts
				local n, m = verts:size()
				if self.conn == nil then
					for i = 1,n-1 do
						for j = 1,m-1 do 
							local f = face.new(verts[i][j], verts[i][j+1], verts[i+1][j], verts[i+1][j+1])
							f.col = cloth.light:brightness(f.n, 0, 3)
							table.insert(faces, f)
						end
					end
				else
					for i = 1,n-1 do
						for j = 1,m-1 do 
							if self.conn.hori[i][j] == 1 and self.conn.verti[i][j] == 1 and self.conn.hori[i+1][j] == 1 and self.conn.verti[i][j+1] == 1 then
								local f = face.new(verts[i][j], verts[i][j+1], verts[i+1][j], verts[i+1][j+1])
								f.col = cloth.light:brightness(f.n, 0, 3)
								table.insert(faces, f)
							end
						end
					end
				end
				table.insert(faces, cloth.collision.sphere)
				table.sort(faces, face.sort)
				for i,f in ipairs(faces) do
					f:draw(H2, H2)
				end
			end,

			draw_smooth = function(self)
				local faces = {}
				local verts = self.verts
				local n, m = verts:size()
				for i = 1,n-1 do
					for j = 1,m-1 do 
						local f = face.new(verts[i][j], verts[i][j+1], verts[i+1][j], verts[i+1][j+1])
						table.insert(faces, f)
					end
				end
				-- vertex normals
				local vnormals = {}
				for i = 1,n do
					vnormals[i] = {}
					for j = 1,m do 
						local f1 = faces[(m-1)*(i-2)+j-1]
						local f2 = faces[(m-1)*(i-2)+j]
						local f3 = faces[(m-1)*(i-1)+j-1]
						local f4 = faces[(m-1)*(i-1)+j]
						vnormals[i][j] = face.navg({f1, f2, f3, f4})
					end
				end
				-- vertex color
				local vcols = {}
				for i = 1,n do
					vcols[i] = {}
					for j = 1,m do 
						vcols[i][j] = cloth.light:brightness(vnormals[i][j], 0, 3)
					end
				end
				for i = 1,n-1 do
					for j = 1,m-1 do 
						faces[(m-1)*(i-1)+j].vc = {vcols[i][j], vcols[i][j+1], vcols[i+1][j], vcols[i+1][j+1]} 
					end
				end
				-- remove based on connectivity
				if self.conn ~= nil then
					local temp = {}
					for i = 1,n-1 do
						for j = 1,m-1 do 
							if self.conn.hori[i][j] == 1 and self.conn.verti[i][j] == 1 and self.conn.hori[i+1][j] == 1 and self.conn.verti[i][j+1] == 1 then
								table.insert(temp, faces[(m-1)*(i-1)+j])
							end
						end
					end
					faces = temp
				end
				-- rasterization
				table.insert(faces, cloth.collision.sphere)
				table.sort(faces, face.sort)
				for i,f in ipairs(faces) do
					f:cdraw(H2, H2)
				end
			end,

			update = function(self)
				local verts = self.verts 
				local n, m = verts:size()
				local F = mat.new_const(vec3d.new(0,0,0), n, m)
				if self.conn == nil then
					for i = 1,n do
						for j = 1,m do
							local f
							-- compression
							if j ~= m then
								f = cloth.hook(verts[i][j], verts[i][j+1], cloth.k1, cloth.l0)
								F[i][j] = F[i][j] + f
								F[i][j+1] = F[i][j+1] - f
							end
							if i ~= n then 
								f = cloth.hook(verts[i][j], verts[i+1][j], cloth.k1, cloth.l0)
								F[i][j] = F[i][j] + f
								F[i+1][j] = F[i+1][j] - f
							end
							-- shear
							if i ~= n and j ~= m then
								f = cloth.hook(verts[i][j], verts[i+1][j+1], cloth.k2, sqrt2*cloth.l0)
								F[i][j] = F[i][j] + f
								F[i+1][j+1] = F[i+1][j+1] - f
								f = cloth.hook(verts[i+1][j], verts[i][j+1], cloth.k2, sqrt2*cloth.l0)
								F[i+1][j] = F[i+1][j] + f
								F[i][j+1] = F[i][j+1] - f
							end
							-- bending 
							if j < n-1 then
								f = cloth.hook(verts[i][j], verts[i][j+2], cloth.k3, 2*cloth.l0)
								F[i][j] = F[i][j] + f
								F[i][j+2] = F[i][j+2] - f
							end
							if i < m-1 then
								f = cloth.hook(verts[i][j], verts[i+2][j], cloth.k3, 2*cloth.l0)
								F[i][j] = F[i][j] + f
								F[i+2][j] = F[i+2][j] - f
							end
						end
					end
				else
					for i = 1,n do
						for j = 1,m do
							local f
							-- compression
							if j ~= m then
								if self.conn.hori[i][j] == 1 then
									f = cloth.hook(verts[i][j], verts[i][j+1], cloth.k1, cloth.l0)
									F[i][j] = F[i][j] + f
									F[i][j+1] = F[i][j+1] - f
								end
							end
							if i ~= n then 
								if self.conn.verti[i][j] == 1 then
									f = cloth.hook(verts[i][j], verts[i+1][j], cloth.k1, cloth.l0)
									F[i][j] = F[i][j] + f
									F[i+1][j] = F[i+1][j] - f
								end
							end
							-- shear
							if i ~= n and j ~= m then
								if self.conn.hori[i][j] == 1 and self.conn.verti[i][j] == 1 and self.conn.hori[i+1][j] == 1 and self.conn.verti[i][j+1] == 1 then
									f = cloth.hook(verts[i][j], verts[i+1][j+1], cloth.k2, sqrt2*cloth.l0)
									F[i][j] = F[i][j] + f
									F[i+1][j+1] = F[i+1][j+1] - f
									f = cloth.hook(verts[i+1][j], verts[i][j+1], cloth.k2, sqrt2*cloth.l0)
									F[i+1][j] = F[i+1][j] + f
									F[i][j+1] = F[i][j+1] - f
								end
							end
							-- bending 
							if j < n-1 then
								if self.conn.hori[i][j] == 1 and self.conn.hori[i][j+1] == 1 then
									f = cloth.hook(verts[i][j], verts[i][j+2], cloth.k3, 2*cloth.l0)
									F[i][j] = F[i][j] + f
									F[i][j+2] = F[i][j+2] - f
								end
							end
							if i < m-1 then
								if self.conn.verti[i][j] == 1 and self.conn.verti[i+1][j] == 1 then
									f = cloth.hook(verts[i][j], verts[i+2][j], cloth.k3, 2*cloth.l0)
									F[i][j] = F[i][j] + f
									F[i+2][j] = F[i+2][j] - f
								end
							end
						end
					end
				end
				F = F + cloth.gravity*vec3d.new(0,-1,0)
				F = F - cloth.damping*(verts-self.vold)/cloth.dt
				if self.pin ~= nil then 
					F = self.pin * F 
					self.vold = self.pin*self.vold + (-1*self.pin+1)*self.verts
				end
				-- verlet intergration
				F = 2*self.verts - self.vold + 0.5*cloth.dt^2*F
				self.vold = self.verts
				self.verts = F
				-- sphere collision
				for i = 1,n do
					for j = 1,m do
						local u = self.verts[i][j] - cloth.collision.sphere.o
						local r = vec3d.norm(u)
						if r < cloth.collision.sphere.r and self.pin[i][j] ~= 0 then
							local n = u / r 
							self.verts[i][j] = cloth.collision.sphere.r * n + cloth.collision.sphere.o
						end
					end
				end
				-- floor collision
				for i = 1,n do
					for j = 1,m do
						if self.verts[i][j][2] < -200 then self.verts[i][j][2] = -200 end
					end
				end
			end

		}

	},

	hook = function(v1, v2, k, l0)
		local u = v2 - v1
		local r = vec3d.norm(u)
		if r == 0 then return end
		local f = k * (r - l0)
		local F = f * u / r 
		return F 
	end,

	pin_corner = function(verts)
		local n, m = verts:size()
		local pin = mat.new_const(1, n, m)
		pin[1][1] = 0
		pin[1][m] = 0
		pin[n][1] = 0
		pin[n][m] = 0
		return pin 
	end,

	new_conn = function(verts)
		local n, m = verts:size()
		local hori = mat.new_const(1, n, m-1)
		local verti = mat.new_const(1, n-1, m)
		return {hori=hori, verti=verti}
	end,

	split_half = function(verts)
		local n, m = verts:size()
		local hori = mat.new_const(1, n, m-1)
		local verti = mat.new_const(1, n-1, m)
		for i = 1,n do
			hori[i][5] = 0
		end
		return {hori=hori, verti=verti}
	end,

	collision = {

		sphere = sphere.new(20, vec3d.new(1, -20, 2), 2),

		update = function(self)
			local dx = mouse.dx*camera.x - mouse.dy*camera.y
			self.sphere:update(dx)
		end,

		draw = function(self)
			self.sphere:draw(H2, H2)
		end,

		draw_wire = function(self)
			self.sphere:draw_wire(H2, H2)
		end,

	},

	light = light.new(0.2, 0.1)

}

grab = {

	current = nil,

	update = function(self)

		if cloth.operation ~= 2 then return end
		if mouse.x >= HEIGHT then return end

		-- controls
		if mouse.md or mouse.right then
			if self.current ~= nil then
				local u = mouse.dx * camera.x - mouse.dy * camera.y
				if self.current == cloth.collision.sphere then 
					local d = 0.0125*vec3d.dist(self.current.o, camera.o)
					self.current.o = self.current.o + d*u 
				else
					local i = self.current.i
					local j = self.current.j
					c1.verts[i][j] = c1.verts[i][j] + 0.0125*self.current.d*u 
					c1.pin[i][j] = 0
				end
			end
			return
		elseif mouse.mu then
			if self.current ~= cloth.collision.sphere and self.current ~= nil then
				local i = self.current.i
				local j = self.current.j
				c1.pin[i][j] = 1
			end
		end

		-- selection
		self.current = nil
		local x, y, r = cloth.collision.sphere:proj(camera)
		local x = mouse.x - H2 - x
		local y = mouse.y - H2 - y
		if sqrt(x^2 + y^2) < r then
			self.current = cloth.collision.sphere
			return
		end
		local n, m = c1.verts:size()
		local verts = {}
		for i = 1,n do
			for j = 1,m do
				table.insert(verts, {i=i, j=j, d=vec3d.dist(c1.verts[i][j], camera.o)})
			end
		end
		table.sort(verts, function(a, b) return a.d < b.d end)
		for i,v in ipairs(verts) do
			x, y = c1.verts[v.i][v.j]:proj(camera)
			x = mouse.x - H2 - x 
			y = mouse.y - H2 - y
			if sqrt(x^2 + y^2) < 5 then
				self.current = v
				return
			end
		end

	end,

	draw = function(self)
		if self.current ~= nil then 
			if self.current == cloth.collision.sphere then
				self.current:draw_highlight(H2, H2)
			else
				c1.verts[self.current.i][self.current.j]:draw_highlight(H2, H2)
			end 
		end
	end

}

cut = {

	trail = stack.new(8),
	falloff = {0,1,1,1,2,2,2,1},

	update = function(self)
		-- controls
		if cloth.operation ~= 3 then return end
		if mouse.md and mouse.x < HEIGHT then
			self.trail:push({mouse.x, mouse.y})
		else
			self.trail:push(nil)
		end
		-- cutting
		if self.trail[7] == nil or self.trail[8] == nil then return end
		if c1.conn == nil then c1.conn = cloth.new_conn(c1.verts) end
		local v1 = {self.trail[7][1] - H2, self.trail[7][2] - H2}
		local v2 = {self.trail[8][1] - H2, self.trail[8][2] - H2}
		local n,m = c1.verts:size()
		for i = 1,n do
			for j = 1,m-1 do
				local x1, y1 = c1.verts[i][j]:proj(camera)
				local x2, y2 = c1.verts[i][j+1]:proj(camera)
				if self.isintersect(v1, v2, {x1, y1}, {x2, y2}) then
					c1.conn.hori[i][j] = 0
				end
			end
		end
		for i = 1,n-1 do
			for j = 1,m do
				local x1, y1 = c1.verts[i][j]:proj(camera)
				local x2, y2 = c1.verts[i+1][j]:proj(camera)
				if self.isintersect(v1, v2, {x1, y1}, {x2, y2}) then
					c1.conn.verti[i][j] = 0
				end
			end
		end
	end,

	draw = function(self)
		-- cut.lstript(self.trail, 10)
		for i = 1,self.trail.n do
			if self.trail[i] ~= nil then 
				circ(self.trail[i][1], self.trail[i][2], self.falloff[i], 4)
			end
		end
		for i = 1,self.trail.n-1 do
			if self.trail[i] ~= nil and self.trail[i+1] ~= nil then
				local u1 = self.trail[i][1]
				local v1 = self.trail[i][2]
				local u2 = self.trail[i+1][1]
				local v2 = self.trail[i+1][2]
				local x1, y1, x2, y2, x3, y3, x4, y4
				if (u1 - u2) * (v1 - v2) < 0 then
					x1 = u1 - self.falloff[i]
					y1 = v1 - self.falloff[i]
					x2 = u1 + self.falloff[i]
					y2 = v1 + self.falloff[i]
					x3 = u2 - self.falloff[i+1]
					y3 = v2 - self.falloff[i+1]
					x4 = u2 + self.falloff[i+1]
					y4 = v2 + self.falloff[i+1]
				else
					x1 = u1 - self.falloff[i]
					y1 = v1 + self.falloff[i]
					x2 = u1 + self.falloff[i]
					y2 = v1 - self.falloff[i]
					x3 = u2 - self.falloff[i+1]
					y3 = v2 + self.falloff[i+1]
					x4 = u2 + self.falloff[i+1]
					y4 = v2 - self.falloff[i+1]
				end
				tri(x1, y1, x2, y2, x3, y3, 0)
				tri(x2, y2, x3, y3, x4, y4, 0)
				line(x1, y1, x3, y3, 4)
				line(x2, y2, x4, y4, 4)
			end
		end
		-- local n = self.trail.n 
		-- if self.trail[n] ~= nil then
		-- 	circ(self.trail[n][1], self.trail[n][2], self.falloff[n]+1, 11)
		-- 	circb(self.trail[n][1], self.trail[n][2], self.falloff[n]+1, 10)
		-- end
	end,

	lstript = function(trail, col)
		local n = #trail
		for i = 1,n do
			if trail[i] ~= nil and trail[i+1] ~= nil then 
				line(trail[i][1], trail[i][2], trail[i+1][1], trail[i+1][2], col)
			end
		end
	end,

	isclockwise = function(x1, y1, x2, y2, x3, y3)
		local a1 = x2 - x1
		local a2 = y2 - y1
		local b1 = x3 - x1
		local b2 = y3 - y1
		if (a1*b2 - b1*a2) < 0 then
			return -1
		else
			return 1
		end
	end,

	isintersect = function(v1, v2, v3, v4)
		local a = cut.isclockwise(v1[1], v1[2], v3[1], v3[2], v4[1], v4[2]) 
		local b = cut.isclockwise(v2[1], v2[2], v3[1], v3[2], v4[1], v4[2]) 
		local c = cut.isclockwise(v1[1], v1[2], v2[1], v2[2], v3[1], v3[2]) 
		local d = cut.isclockwise(v1[1], v1[2], v2[1], v2[2], v4[1], v4[2]) 
		if a * b == -1 and c * d == -1 then
			return true
		else
			return false
		end
	end

}


bbox = {

	new = function(x, y, w, h)
		local b = {x=x, y=y, w=w, h=h}
		setmetatable(b, bbox.mt)
		return b
	end,

	mt = {

		__index = {

			draw = function(self, col)
				if col == nil then col = 12 end
				rectb(self.x, self.y, self.w, self.h, col)
			end,

			contain = function(self, x, y)
				return mathFun.between(x, self.x, self.x+self.w+1) and mathFun.between(y, self.y, self.y+self.h+1)
			end,

			unpack = function(self)
				return self.x, self.y, self.w, self.h
			end

		}

	}

}

function reset()
	c1 = cloth.new_lattice(100, 100, vec3d.new(-50, 10, -50))
	c1.pin = cloth.pin_corner(c1.verts)
end

ui = {

	x0 = HEIGHT,
	y0 = 1,
	current = nil,
	objects = {},
	interactibles = {},
	bbox = bbox.new(HEIGHT, 0, 104, HEIGHT),
	y1 = 2,
	y2 = 77,
	y3 = 150,
	y4 = 77,
	y5 = 108,
	y6 = 116,

	load = function(self)
		ui.header.new('Physics', self.y1)
		p1 = ui.slider.new('Length', 5, 20, cloth.l0, self.y1+8)
		p2 = ui.slider.new('Gravity', 0, 10, cloth.gravity, self.y1+19)
		p3 = ui.slider.new('Compression', 1, 50, cloth.k1, self.y1+30)
		p4 = ui.slider.new('Shear', 0, 50, cloth.k2, self.y1+41)
		p5 = ui.slider.new('Bending', 0, 50, cloth.k3, self.y1+52)
		p6 = ui.slider.new('Damping', 0, 1, cloth.damping, self.y1+63)
		-- ui.header.new('Light', self.y2)
		-- l3 = ui.slider.new('Strength', 0, 2, 1, self.y2+8)
		-- l1 = ui.slider.new('Rotate x', -PI, PI, cloth.light.ax, self.y2+19)
		-- l2 = ui.slider.new('Rotate z', -PI, PI, cloth.light.az, self.y2+30)
		ui.header.new('Shading', self.y4)
		b1 = ui.button.new('Wire', 0, self.y4+8)
		b2 = ui.button.new('Occlude', 52, self.y4+8, true)
		b3 = ui.button.new('Flat', 0, self.y4+19)
		b4 = ui.button.new('Smooth', 52, self.y4+19)
		b1.click = function() cloth.shading = 1 end
		b2.click = function() cloth.shading = 2 end
		b3.click = function() cloth.shading = 3 end
		b4.click = function() cloth.shading = 4 end
		-- s1 = ui.slider.new('DOF', 60, 200, camera.depth, self.y2+8)
		-- s2 = ui.slider.new('Distance', 50, 200, vec3d.norm(camera.o), self.y2+19)
		ui.header.new('Interact', self.y5)
		q1 = ui.button2.new(4, 23, self.y6)
		q2 = ui.button2.new(5, 42, self.y6)
		q3 = ui.button2.new(3, 61, self.y6)
		q1.click = function() cloth.operation = 1 end
		q2.click = function() cloth.operation = 2 end
		q3.click = function() cloth.operation = 3 end
		r1 = ui.button3.new()
		r1.click = reset
	end,

	update = function(self)
		-- scroll wheel
		-- if mouse.scroll ~= nil and mouse.scroll ~= 0 then
		-- 	if self.bbox:contain(mouse.x, mouse.y) then
		-- 		for i,v in ipairs(ui.objects) do
		-- 			v.bbox.y = v.bbox.y + mouse.scroll*6
		-- 		end
		-- 	end
		-- end
		-- interactions
		if not mouse.md or mouse.mp then
			for i,v in ipairs(ui.interactibles) do
				if v.bbox:contain(mouse.x, mouse.y) then
					v.mo = true
					if mouse.mp then self.current = v end
				else
					v.mo = false
				end
			end
		end
		if mouse.md and self.current ~= nil then 
			self.current:update()
		else
			self.current = nil
		end
		-- update 
		cloth.l0 = p1.val
		cloth.gravity = p2.val
		cloth.k1 = p3.val
		cloth.k2 = p4.val
		cloth.k3 = p5.val
		cloth.damping = p6.val
		-- cloth.light.s = l3.val
		-- cloth.light.ax = l1.val
		-- cloth.light.az = l2.val 
		b1.val = cloth.shading == 1
		b2.val = cloth.shading == 2
		b3.val = cloth.shading == 3
		b4.val = cloth.shading == 4
		-- camera.depth = s1.val
		-- camera.o = s2.val * camera.o:normalise()
		q1:detect()
		q2:detect()
		q3:detect()
		q1.val = cloth.operation == 1
		q2.val = cloth.operation == 2
		q3.val = cloth.operation == 3
		r1:detect()
	end,

	draw = function(self)
		rectb(0, 0, HEIGHT, HEIGHT, 14)
		rect(HEIGHT, 0, WIDTH-HEIGHT, HEIGHT, 14)
		for i,v in ipairs(ui.objects) do
			v:draw()
		end
	end,

	slider = {

		w = 101,
		h = 10,

		new = function(text, a, b, val, y)
			local s = {text=text, a=a, b=b, val=val, mo=false, 
				       bbox=bbox.new(ui.x0+1, ui.y0+y, ui.slider.w, ui.slider.h)}
			setmetatable(s, ui.slider.mt)
			table.insert(ui.objects, s)
			table.insert(ui.interactibles, s)
			return s
		end,

		mt = {

			__index = {

				update = function(self)
					local x, y, w, h = self.bbox:unpack()
					local mx = mathFun.clamp(mouse.x, x+1, x+w-1)
					self.val = mathFun.maprange(mx, x+1, x+w-1, self.a, self.b)
				end,

				draw = function(self, x0, y0)
					local col = self.col 
					if col == nil then col = 15 end
					local x, y, w, h = self.bbox:unpack()
					if self.mo then self.bbox:draw(12) else self.bbox:draw(col) end 
					local ww = mathFun.maprange(self.val, self.a, self.b, 0, w-2) 
					rect(x+1, y+1, ww, h-2, col)
					pix(x, y, 14)
					pix(x+w-1, y, 14)
					pix(x, y+h-1, 14)
					pix(x+w-1, y+h-1, 14)
					if self.text ~= nil then
						print(self.text, x+3, y+2, 13)
						ui.rprint(string.format("%.2f", self.val), x+99, y+2, 13, true)
					end
				end

			}

		}

	},

	header = {

		new = function(text, y)
			local x = {text=text, bbox={y=y+ui.y0}}
			setmetatable(x, ui.header.mt)
			table.insert(ui.objects, x)
			return x
		end,

		mt = {

			__index = {

				draw = function(self)
					ui.cprint(self.text, 188, self.bbox.y, 12)
				end

			}

		}

	},

	button = {

		new = function(text, x, y, val, col)
			if val == nil then val = false end
			if col == nil then col = 15 end
			local b = {text=text, bbox=bbox.new(ui.x0+x+1, ui.y0+y, 49, 10), val=val, col=col}
			setmetatable(b, ui.button.mt)
			table.insert(ui.objects, b)
			table.insert(ui.interactibles, b)
			return b
		end,

		mt = {

			__index = {

				update = function(self)
					if mouse.mp and type(self.click) == 'function' then
						self:click()
					end
				end,

				draw = function(self, x0, y0)
					local x, y, w, h = self.bbox:unpack()
					if self.mo then self.bbox:draw(12) else self.bbox:draw(self.col) end 
					if self.val then rect(x+1, y+1, w-2, h-2, self.col) end
					pix(x, y, 14)
					pix(x+w-1, y, 14)
					pix(x, y+h-1, 14)
					pix(x+w-1, y+h-1, 14)
					print(self.text, x+3, y+2, 13)
				end

			}

		}

	},

	button2 = {

		new = function(spr, x, y, val)
			if val == nil then val = false end
			local b = {spr=spr, bbox=bbox.new(ui.x0+x+1, ui.y0+y, 16, 16), val=val, hover=false}
			setmetatable(b, ui.button2.mt)
			table.insert(ui.objects, b)
			table.insert(ui.interactibles, b)
			return b
		end,

		mt = {

			__index = {

				update = function(self)
					if mouse.mp and type(self.click) == 'function' then
						self:click()
					end
				end,

				detect = function(self)
					if self.bbox:contain(mouse.x, mouse.y) then
						self.hover = true
					else
						self.hover = false
					end
				end,

				draw = function(self, x0, y0)
					local x, y, w, h = self.bbox:unpack()
					if self.hover or self.val then
						spr(48, x, y, 0, 1, 0, 0, 2, 2)
						spr(self.spr, x+4, y+6, 0)
					else
						spr(16, x, y, 0, 1, 0, 0, 2, 2)
						spr(self.spr, x+4, y+4, 0)
					end
					-- rect(x, y, 19, 19, 13)
				end

			}

		}

	},

	button3 = {

		new = function()
			local b = {bbox=bbox.new(231, 127, 9, 9), hover=false}
			setmetatable(b, ui.button3.mt)
			table.insert(ui.objects, b)
			table.insert(ui.interactibles, b)
			return b
		end,

		mt = {

			__index = {

				update = function(self)
					if mouse.mp and type(self.click) == 'function' then
						self:click()
					end
				end,

				detect = function(self)
					if self.bbox:contain(mouse.x, mouse.y) then
						self.hover = true
					else
						self.hover = false
					end
				end,

				draw = function(self, x0, y0)
					local x, y, w, h = self.bbox:unpack()
					rect(x, y, 9, 9, 2)
					if self.hover then
						spr(2, 232, 128, 1)
					else
						spr(1, 232, 128, 0)
					end
				end

			}

		}

	},

	rprint = function(text, x, y, col, fixed)
		local w = print(text, 0, -6, 0, fixed)
		print(text, x-w, y, col, fixed)
	end,

	cprint = function(text, x, y, col, fixed)
		local w = print(text, 0, -6, 0, fixed)
		print(text, x-w/2, y, col, fixed)
	end,

}


camera:load()
ui:load()

c1 = cloth.new_lattice(100, 100, vec3d.new(-50, 10, -50))
c1.pin = cloth.pin_corner(c1.verts)

function TIC()

	--update
	time:update()
	mouse:update()
	ui:update()
	cloth.light:update()
	c1:update()
	camera:update()
	grab:update()
	cut:update()

	--draw
	cls(0)
	c1:draw()
	grab:draw()
	cut:draw()
	ui:draw()
	axes:draw()

	-- debug
	-- camera:draw()
	-- mouse:draw()
	time:draw()

end

-- <TILES>
-- 000:89abc00000000000000000000000000000000000000000000000000000000000
-- 001:000c00000c0c0c00c00c00c0c00c00c0c00c00c00c000c0000ccc00000000000
-- 002:111f11111f1f1f11f11f11f1f11f11f1f11f11f11f111f1111fff11111111111
-- 003:c00000d0cc000dd0ccc0ddd00cccdd0000cfc0000fdccf00f0f0f0f0ff000ff0
-- 004:00cccc000cddccc0ddffdcc0dffffcc00fffcccc0fffdccc00fffdc000000000
-- 005:00dccccd00cccccc00dcccccdcdcccccccdccccccccccccc0ccccccd00cccdd0
-- 016:00ffffff0feeeeeefdeeeeeefdeeeeeefdeeeeeefdeeeeeefdeeeeeefdeeeeee
-- 017:ffffff00eeeeeef0eeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeff
-- 032:fdeeeeeefdeeeeeefdeeeeeefdeeeeeefdeeeeeefdffffffffffffffffffffff
-- 033:eeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffffffffffffffffffffffffff
-- 048:0000000000000000ffffffffffffffffffeeeeeeffeeeeeeffeeeeeeffeeeeee
-- 049:0000000000000000ffffffffffffffffeeeeeeffeeeeeeffeeeeeeffeeeeeeff
-- 064:ffeeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffffffff
-- 065:eeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffeeeeeeffffffffff
-- </TILES>

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

-- <COVER>
-- 000:eed000007494648393160f00880077000012ffb0e45445353414055423e2033010000000129f40402000ff00c2000000000f0088007865c668a1c1c2ffdc5733c3754f4f4f837b461be335490b2cb3d59c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000080ff001080c1840b0a1c388031a2c58c0b1a3c780132a4c9841b2a2c008133a6cd8c1b3a7cf80234a8c1942b4294b882356a410c295a0c6930d4acc9943b6adcb91045aecd9f0d5e04f920402ec1a44b8ecc108439e0809c3b02187a4040e358835b921c6914f52c40064bba7dfaa43c22df8657065d8a053a2005b761aa357bb61e2a4f9f358e7dbb87f8a8ddbb4936959ac63ea4d5b083eaa51c275e654caf39e6ed7c09f3eed20497fafdfb96b03163c78dde455c68ff6cc8c4a323f5ac508fe641d5553f164c07bde760d56b4bd6cbc7700aecda154734adeb32ede1ed537be63edb38b27fd4cb93e43e7c1936c5e4db6b3fbea7377a69e935024ffe0e3cb8fff1f4e70014ce8e94e7fde2ddb7971f5d77a97bb9ebeb7c5f30c08e75ddd39e1830fe925ff9f770827ffd04ff5188052880e18edd866f548a11e7af91080c5816d4700018165288452800e7830e000220802a86c020070a3850da850ab8f02d721a48fdd5863ad8916f722a08626f8f1298b329812a092092856938c0a1940529a2ea81327533ec84e1e8a4968c121834e88d3a7824218542a8a022954a1926a89b124815ad7106e8ee909c5eb912e09d5a88d3e79746a892639b42b897ec7056a9a6ea994959b6279b75c7a39f9082d828664d6a0a9b12aa292ab860ab6a3af824aa9e6a09e9759a5a5e52729e8a31d7a0a2972a27aaaeaa5e97a6a6a7fedaff6b12ba1029a6015afa2ca3ed56c3e884e96927287a2598b02f9c22ea9b2a72e574d6afae6669e1a69dbe081760ba4d0ba4a14032c876ac89ca2bcd1fab325b17209d5a4b0caf714e1a92d9870139c467bf7e7b73110549560253b5ea3b5d2d9142fb68a85c312946689a7aa8f42db5fe35ecd3a02ec98566975ab9c364ade6e9c263950b198c6a76fed5c0700f1317992daefa5c6be04d13a732775d27bc7b59c62b0b92bac862bc03f051276493796a2fcc0430deaa7cd3bd49371d4a5dca4f246f14564decc3f57f370d45faa34b37e4b8c5456d1573c3b297b4ff5b2f07c532d5213c6f6a5a5b7ddbe77f756ce091b9dee9b1bdced2179537d335edb114ffd3c9ebf325c7ee8940bcdd061e9d2e997e1e36e1cd2eb809f1c36bd6f13fdb775e81d1d25f7d00ff84c69960f6b6873e0abd9b721e80b6be46cb397969634d6677496d8ca93bdffec9622091932c91f2ea133ecdeee3731c67a1bf5a99da39686fadccf844b77da15ce0993cebeb9abb6c1662e8831e9d3ae70b6f587384733e9036d9976ee4b4f0df3fd736723bbde878d9252ff85afbcb8caefcef2374ae7e5fe3472f3ba0df2f23bb8ed9f042a63da9efad7b9bfa55de18d9ab99be418bfb302ffa27893306af0d310ce0ecb82786ba063f0d7af3fb1cccf642bff14075843bbb546238724cf98051832ca16ffcf7e0c1f8703152148c48ce5752cea550ff15832cb12e07a50131858d647fe262e00db214c6edecac68e39d1ff2d3af28655f604cd3ce5574171f31f98eac7ea30223f26852c40e51918ee3702c9c3757aee9996931eb9bdfe027f2223a2fe780b3e95a13a860c042703781dad7d88257cda8f01f01950b3e91c4e549b33ac0ff8a7312e609d8d6c88dde826d548b918417d842fdfe4f8e3c09910e9251a35aa1b86d3ba4a11f88fe9d1e4aa78023b79e70599a455a10409b6cb2e521b89348f8c2152f442acce2797bcc5a50139cb4cf9d76a6c2c86a03d58f51204235596d4d5223196cace5e027895332dc53359afbc5673d325b3066035a9ce4ebc5dee23e427a93718edc666d07376c107a8d8696ff49a0563dc98c4f7ed2d27654666629e434a8bdee256e94ee1e1d145cca98047b910d87ee2729daca7e1417747ae2a74f3aebbb39d13e707b882d4c2a31d08ef35f318c40664f99c93839e9ac44a31e132d73e4a62e6f849d51c6aa4b2a7ac08e23df86249010e4776c3ed5d4fb78db19ae488516429a28c4a239462e215a2f446e05538fbcaf4615b8d8364e8135716d0e482db8865842e56c98bc592545ba7f4b1eb27a7bec4ace424ae14aa69425a3057b66558a7e438e649d5afc6a8e5942c75ba24500a51d0cad3fb9dec8c6e39f98394c6059c98a4fb2931d950d80516356b6dbcef57b9b855967650bd6caba4632bfec2a0a453b31d36e9293ba1506ffea6deab4b3ce844b93ad7bec3b08b5d0ee02539fe17618d0b53258a6f982b731eda56f9befc8e107f1b3cdd4e3c8a580de091a206a4449422f4726c9a237b9b755caa9776b5055bdc5396135fcd8beab684e86fbcd971bc3aaeae8c3de7a53bda0451de6396b2dd43d153777e2dd9c7d74845eee5be265cb1e9d45da0fcf7696b1b04d796d6d6b9ddce6ee6b4df296d4cabbedd4a6a579b4dcdadf6f7ad850035713c5c5b61f541cb065a9e6942e6dc6ac616b8edadea79db7b54a48d66cecb59a6656b6e45134f6640dce1b46b5bf4312758bfa63632b07dbaf95255dfeaa836fb202fe9b366f659525756b5af791b9ac562d375caec2b466a53a099d4839ff086acc2b2358b04c4b55a853da9cc006463fa1d378628316eac158b24b2e31679dbca6e6bc068d28692c8b9903c86043c9bec22308e96f1b78e8c407c3318984b7a1d2d96f3ff520d42481a4938b996ab62a51db91d3fa914d3813bcb6ca3871a2e429ec25e45baafbcf766e0105295a653e493d9082e5389f7dcebbf160fe7a3ef491b1155fb33e2f2382d516b58b8da2c7061465bc4eda15039dc5ea6f4757d475f319acf4d3e1fe2c24824c0777658d8de9f0992aa561473692c36cddb4ebbbb186b53113ea4ce87f1f8ca32e68e44cedc1d769681a518b7890e2c2ae63adad2f6cc61c60c50ed4d9968e61a611483ab6b8c0fbd57811c13e8636e65979ffe32f49d8addafc17c1fa411f8b125b831347db7d5c04619c9cde767eb0f5c50cd2f8cd777493f386e671e40caa696159e48d86bc05673eec5fbae947ee427b332e53939ba83d8a2f8b6498ae3a248f29b4c6ccf1e9af5649cc595d3a9bde1b39d57642c3b7ac9cda9e76757c9e5544de5c9ee5775d240bdeca5ca3d46f76b6b8f5818e6ee0bbddc6593f09cc0b2ed57b162b264e275881c3228ee3c9357122265843e1186fc33ffc9fe5ee2ba45d5403f7846e98e9356d32ee1ab09fc38e15d2ee4bfacfbe14f54ed49ed2195df82b2fcbb7462573f2b7406fcd0fc6487b3b38e0edddf5e122ebfb3a25b47bb76a624a8fe8fb677df7eb82ac38f04afcf68cffaedd597cce5dd087f844806f3bfa77d829eb4f92b8eb2efde5dceae6ff0f8176cf2ebae4ff9471efeffd2b67c240877572875a4494766a676774d1a03d08dd1b08ac1e08a03d412a6401c66921718c312184f2651c76b762b1f182b6e183b34b6b18357418196da6d910b6761a28db62813a6628a08c31ca6928db6c66f96d763b6a18238728b31fa61b6f761b13b6ea6981918a21d38e38018eb6948776b48296d48dd7f48c48158047392004758395b315582585471b7e58e97c38c588b74c4b97ee12084f5b126683b2b58368468d17b68e369d6017ed1178987611e68f68511422c01278c681b78e6678d68978e953f3288768088a23788848488588568b8d786f6098a88298a31d88e88078498f78e97188998921698035f88988888b983982a8c88f98fd48986a8e787d7c986a8e988a89a8b78b88ca8ae7ea84a8011045f141b89241a88b8a98da85a8fb8e013a5bb8cb81336889985b8701e78c78fa8d016557956c87c8e336881e1f33ac88370260d86369c43d84d8d77d173d4bc87b22670f3f01795dd8fd80e8f81958d24a343781364c8dd8145be8ce8871358921c65af16f87f8f519f8821ef4755009409509601045101000b3
-- </COVER>

