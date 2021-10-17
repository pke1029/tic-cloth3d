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
		print(self.fps, 228, 1, 10, true)
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
				circb(x+x0, y+y0, 2, 10)
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

	load = function(self)
		self.o:rotate(-0.5, 0.5, 0)
		self.x:rotate(-0.5, 0.5, 0)
		self.y:rotate(-0.5, 0.5, 0)
	end,

	update = function(self)
		
		if mouse.scroll ~= 0 then
			self.r = mathFun.clamp(self.r - 4*mouse.scroll, 80, 200)
			self.o = self.r * self.o:normalise()
		end

		if mouse.md then
			-- rotate (turn table)
			self.ax = mathFun.clamp(self.ax - 0.03*mouse.dy, -PI/2, PI/2)
			self.ay = self.ay - 0.03*mouse.dx
			self.o = vec3d.new(0, 0, self.r):rotate(self.ax, self.ay, 0)
			self.x = vec3d.new(1, 0, 0):rotate(0, self.ay, 0)
			self.y = vec3d.new(0, 1, 0):rotate(self.ax, self.ay, 0)
		elseif mouse.right then 
			-- rotate (track ball)
			local u = - mouse.dy*self.x - mouse.dx*self.y
			local theta = vec3d.norm(u) * 0.03
			if theta ~= 0 then 
				self.o:qrotate(theta, u)
				self.x:qrotate(theta, u)
				self.y:qrotate(theta, u)
			end
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

	new = function()
		local a = {y=vec3d.new(0,1,0), ax=0, az=0, s=1}
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
				tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, self.col)
				tri(x2+x0, y2+y0, x3+x0, y3+y0, x4+x0, y4+y0, self.col)
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

	edges = {edge.new(vec3d.new(0, 0, 0), vec3d.new(8, 0, 0), 1),
             edge.new(vec3d.new(0, 0, 0), vec3d.new(0, 8, 0), 2),
             edge.new(vec3d.new(0, 0, 0), vec3d.new(0, 0, 8), 3)},

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
				circb(x+x0, y+y0, r-1, 10)
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
							f.col = cloth.light:brightness(f.n, 4, 8)
							table.insert(faces, f)
						end
					end
				else
					for i = 1,n-1 do
						for j = 1,m-1 do 
							if self.conn.hori[i][j] == 1 and self.conn.verti[i][j] == 1 and self.conn.hori[i+1][j] == 1 and self.conn.verti[i][j+1] == 1 then
								local f = face.new(verts[i][j], verts[i][j+1], verts[i+1][j], verts[i+1][j+1])
								f.col = cloth.light:brightness(f.n, 4, 8)
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
						vcols[i][j] = cloth.light:brightness(vnormals[i][j], 4, 8)
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

		sphere = sphere.new(20, vec3d.new(1, -20, 2), 9),

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

	light = light.new()

}

grab = {

	current = nil,

	update = function(self)

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
		if mouse.md and mouse.x < HEIGHT then
			self.trail:push({mouse.x, mouse.y})
		else
			self.trail:push(nil)
		end
		-- cutting
		if c1.conn == nil then c1.conn = cloth.new_conn(c1.verts) end
		if self.trail[7] == nil or self.trail[8] == nil then return end
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
				circ(self.trail[i][1], self.trail[i][2], self.falloff[i], 10)
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
				tri(x1, y1, x2, y2, x3, y3, 11)
				tri(x2, y2, x3, y3, x4, y4, 11)
				line(x1, y1, x3, y3, 10)
				line(x2, y2, x4, y4, 10)
			end
		end
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

ui = {

	x0 = HEIGHT,
	y0 = 0,
	current = nil,
	objects = {},
	interactibles = {},
	bbox = bbox.new(HEIGHT, 0, 104, HEIGHT),
	y1 = 2,
	y2 = 77,
	y3 = 150,
	y4 = 119,
	y5 = 150,

	load = function(self)
		ui.header.new('Physics', self.y1)
		p1 = ui.slider.new('Length', 5, 20, cloth.l0, self.y1+8)
		p2 = ui.slider.new('Gravity', 0, 10, cloth.gravity, self.y1+19)
		p3 = ui.slider.new('Compression', 1, 50, cloth.k1, self.y1+30)
		p4 = ui.slider.new('Shear', 0, 50, cloth.k2, self.y1+41)
		p5 = ui.slider.new('Bending', 0, 50, cloth.k3, self.y1+52)
		p6 = ui.slider.new('Damping', 0, 1, cloth.damping, self.y1+63)
		ui.header.new('Light', self.y2)
		l3 = ui.slider.new('Strength', 0, 2, 1, self.y2+8)
		l1 = ui.slider.new('Rotate x', -PI, PI, cloth.light.ax, self.y2+19)
		l2 = ui.slider.new('Rotate z', -PI, PI, cloth.light.az, self.y2+30)
		ui.header.new('Shading', self.y4)
		b1 = ui.button.new('Wire', 0, self.y4+8)
		b2 = ui.button.new('Occlude', 52, self.y4+8, true)
		b3 = ui.button.new('Flat', 0, self.y4+19)
		b4 = ui.button.new('Smooth', 52, self.y4+19, false, 1)
		b1.click = function() cloth.shading = 1 end
		b2.click = function() cloth.shading = 2 end
		b3.click = function() cloth.shading = 3 end
		b4.click = function() cloth.shading = 4 end
		-- s1 = ui.slider.new('DOF', 60, 200, camera.depth, self.y2+8)
		-- s2 = ui.slider.new('Distance', 50, 200, vec3d.norm(camera.o), self.y2+19)
		-- ui.header.new('Interact', self.y5)
		-- c1 = ui.button.new('rotate', 0, self.y5+8)
		-- c2 = ui.button.new('grab', 52, self.y5+8)
		-- c3 = ui.button.new('cut', 0, self.y5+19)
		-- c4 = ui.button.new('throw', 52, self.y5+19)
	end,

	update = function(self)
		-- scroll wheel
		if mouse.scroll ~= nil and mouse.scroll ~= 0 then
			if self.bbox:contain(mouse.x, mouse.y) then
				for i,v in ipairs(ui.objects) do
					v.bbox.y = v.bbox.y + mouse.scroll*6
				end
			end
		end
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
		cloth.light.s = l3.val
		cloth.light.ax = l1.val
		cloth.light.az = l2.val 
		b1.val = cloth.shading == 1
		b2.val = cloth.shading == 2
		b3.val = cloth.shading == 3
		b4.val = cloth.shading == 4
		-- camera.depth = s1.val
		-- camera.o = s2.val * camera.o:normalise()
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
	-- cloth.collision:update()
	cloth.light:update()
	c1:update()
	if mouse.x < HEIGHT then
		-- camera:update()
		-- grab:update()
	end
	cut:update()

	--draw
	cls(11)
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
-- 000:0000456700000000000000000000000000000000000000000000000000000000
-- 001:8000000000000000000000000000000000000000000000000000000000000000
-- </TILES>

-- <PALETTE>
-- 000:1a1c2cb13e5338b76441a6f624245d3b4e815279a569a3c981ceeeb13e53ffcd751a1c2cf4f4f494b0c2566c86333c57
-- </PALETTE>

