--[[
	@Author: Anna W. Anna W. <https://devforum.roblox.com/u/ImActuallyAnna> Skylar L. <https://devforum.roblox.com/u/ScobayDu>
	@Description: RBXM File Reader
	@Date of Creation: 09. 05. 2020
	Copyright (C) 2020 Kat Digital Limited.
	 
	This program is free software: you can redistribute it and/or modify  
	it under the terms of the GNU General Public License as published by  
	the Free Software Foundation, version 3.
	
	This program is distributed in the hope that it will be useful, but 
	WITHOUT ANY WARRANTY; without even the implied warranty of 
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
	General Public License for more details.
	
	You should have received a copy of the GNU General Public License 
	along with this program. If not, see <http://www.gnu.org/licenses/>
--]]

--Module to handle binary encoding and decoding
local Binary = {}

--Efficiency is key; we might be handling huge amounts of information
local append 	= table.insert
local char 		= string.char
local byte 		= string.byte
local substr 	= string.sub
local floor 	= math.floor
local ceil		= math.ceil
local abs		= math.abs
local pow 		= math.pow
local log 		= math.log

--Binary logic
function Binary.bitwiseOr(a, b)
	return bit32.bor(a, b)
	
	--[[local out = 0
	for i = 1, 32 do
		if (a%2 == 1 or b%2 == 1) then out = out + 4294967296 end 
		a = floor(a/2)
		b = floor(b/2)
		out = out / 2
	end
	return out]]
end

function Binary.bitwiseAnd(a, b)
	return bit32.band(a, b)
	
	--[[local out = 0
	for i = 1, 32 do
		if (a%2 == 1 and b%2 == 1) then out = out + 4294967296 end 
		a = floor(a/2)
		b = floor(b/2)
		out = out / 2
	end
	return out]]
end

function Binary.bitwiseXor(a, b)
	return bit32.bxor(a, b)
	
	--[[local out = 0
	for i = 1, 32 do
		if (a%2 ~= b%2) then out = out + 4294967296 end 
		a = floor(a/2)
		b = floor(b/2)
		out = out / 2
	end
	return out]]
end

function Binary.bitwiseNot(a)
	--We don't use bit32.bnot() here because the equivalent 
	--arithmetic operation actually faster
	return 4294967295 - a
end

function Binary.byteNot(a)
	--We don't use bit32.bnot()%256 here because the equivalent 
	--arithmetic operation actually faster
	return 255 - a
end

--Shifting
function Binary.leftShift(a, n)
	return math.floor(a * math.pow(2, n)) % 4294967296
end

function Binary.rightShift(a, n)
	return math.floor(a / math.pow(2, n)) % 4294967296
end

--Integers
function Binary.encodeInt(number, nBytes)
	--Make sure we're dealing with an integer
	number = floor(number) % pow(256, nBytes)
	
	--Iterate over bytes and generate the output
	local bytesOut = {}
	for i = 0, nBytes-1 do
		bytesOut[nBytes - i] = char(number % 256)
		number = floor(number/256)
	end
	
	--Concatenate and return
	return table.concat(bytesOut)
end

function Binary.decodeInt(str, useLittleEndian)
	--Reverse the string if we're using little endian
	if (useLittleEndian) then str = str:reverse() end
	
	--Decode
	local out = byte(str, 1)
	for i = 2, #str do
		out = out*256 + byte(str, i)
	end
	
	return out
end

function Binary.decodeSignedInt(str, useLittleEndian)
	--Get basic string first
	local unsigned = Binary.decodeInt(str, useLittleEndian)
	local max_value = math.pow(256, #str)
	
	--If the sign (first bit) is 0, just return the number
	if (unsigned < max_value/2) then return unsigned end
	
	--Otherwise, get the bitwise NOT of the value and negate
	return -(max_value - unsigned)
end

--Doubles
--Define some commonly used variables here so we don't have to do this at runtime
local log2 = log(2)
local pow2to52 = pow(2,52)
local pow2to23 = pow(2,23)
local f08 = pow(2, 8)
local f16 = pow(2,16)
local f24 = pow(2,24)
local f32 = pow(2,32)
local f40 = pow(2,40)
local f48 = pow(2,48)

function Binary.encodeDouble(number)
	--IEEE double-precision floating point number
	--Specification: https://en.wikipedia.org/wiki/Double-precision_floating-point_format
	
	--Separate out the sign, exponent and fraction
	local sign 		= number < 0 and 1 or 0
	local exponent 	= ceil(log(abs(number))/log2) - 1
	local fraction	= abs(number)/pow(2,exponent) - 1
	
	--Make sure the exponent stays in range - allowed values are -1023 through 1024
	if (exponent < -1023) then 
		--We allow this case for subnormal numbers and just clamp the exponent and re-calculate the fraction
		--without the offset of 1
		exponent = -1023
		fraction = abs(number)/pow(2,exponent)
	elseif (exponent > 1024) then
		--If the exponent ever goes above this value, something went horribly wrong and we should probably stop
		error("Exponent out of range: " .. exponent)
	end
	
	--Handle special cases
	if (number == 0) then
		--Zero
		exponent = -1023
		fraction = 0
	elseif (abs(number) == math.huge) then
		--Infinity
		exponent = 1024
		fraction = 0
	elseif (number ~= number) then
		--NaN
		exponent = 1024
		fraction = (pow2to52-1)/pow2to52
	end
	
	--Prepare the values for encoding
	local expOut = exponent + 1023                              --The exponent is an 11 bit offset-binary
	local fractionOut = fraction * pow2to52                     --The fraction is 52 bit, so multiplying it by 2^52 will give us an integer
	
	
	--Combine the values into 8 bytes and return the result
	return char(
			128*sign + floor(expOut/16),                        --Byte 0: Sign and then shift exponent down by 4 bit
			(expOut%16)*16 + floor(fractionOut/f48),            --Byte 1: Shift fraction up by 4 to give most significant bits, and fraction down by 48
			floor(fractionOut/f40)%256,                         --Byte 2: Shift fraction down 40 bit
			floor(fractionOut/f32)%256,                         --Byte 3: Shift fraction down 32 bit
			floor(fractionOut/f24)%256,                         --Byte 4: Shift fraction down 24 bit
			floor(fractionOut/f16)%256,                         --Byte 5: Shift fraction down 16 bit
			floor(fractionOut/f08)%256,                         --Byte 6: Shift fraction down 8 bit
			floor(fractionOut % 256)                            --Byte 7: Last 8 bits of the fraction
		)
end

function Binary.decodeDouble(str, useLittleEndian)
	--Reverse the string if we're using little endian
	if (useLittleEndian) then str = str:reverse() end
	
	--Get bytes from the string
	local byte0 = byte(substr(str,1,1))
	local byte1 = byte(substr(str,2,2))
	local byte2 = byte(substr(str,3,3))
	local byte3 = byte(substr(str,4,4))
	local byte4 = byte(substr(str,5,5))
	local byte5 = byte(substr(str,6,6))
	local byte6 = byte(substr(str,7,7))
	local byte7 = byte(substr(str,8,8))
	
	--Separate out the values
	local sign = byte0 >= 128 and 1 or 0
	local exponent = (byte0%128)*16 + floor(byte1/16)
	local fraction = (byte1%16)*f48 
	                 + byte2*f40 + byte3*f32 + byte4*f24 
	                 + byte5*f16 + byte6*f08 + byte7
	
	--Handle special cases
	if (exponent == 2047) then
		if (fraction == 0) then return pow(-1,sign) * math.huge end
		if (fraction == pow2to52-1) then return 0/0 end
	end
	
	--Combine the values and return the result
	if (exponent == 0) then
		--Handle subnormal numbers
		return pow(-1,sign) * pow(2,exponent-1023) * (fraction/pow2to52)
	else
		--Handle normal numbers
		return pow(-1,sign) * pow(2,exponent-1023) * (fraction/pow2to52 + 1)
	end
end

--Format specification at:
--https://en.wikipedia.org/wiki/Single-precision_floating-point_format
function Binary.decodeFloat(str, useLittleEndian)
	--Reverse the string if we're using little endian
	if (useLittleEndian) then str = str:reverse() end
	
	--Get bytes from the string
	local byte0 = byte(substr(str,1,1))
	local byte1 = byte(substr(str,2,2))
	local byte2 = byte(substr(str,3,3))
	local byte3 = byte(substr(str,4,4))
	
	--Separate out the values
	local sign = byte0 >= 128 and 1 or 0
	local exponent = (byte0%128)*2 + floor(byte1/128)
	local fraction = (byte1%128)*f16 + byte2*f08 + byte3
	
	--Handle special cases
	if (exponent == 255) then
		if (fraction == 0) then return pow(-1,sign) * math.huge end
		if (fraction == pow2to23-1) then return 0/0 end
	end
	
	--Combine the values and return the result
	if (exponent == 0) then
		--Handle subnormal numbers
		return pow(-1,sign) * pow(2,exponent-127) * (fraction/pow2to23)
	else
		--Handle normal numbers
		return pow(-1,sign) * pow(2,exponent-127) * (fraction/pow2to23 + 1)
	end
end

--Stream class setup
local Stream = {}
Stream.__index = Stream

--Constructor
function Stream.new(data)
	return setmetatable({
		Data = data;
		Position = 1;
	}, Stream)
end

--Method to read a given number of bytes
function Stream:read(nBytes)
	--Return nil if we've reached the end of the stream
	if (self.Position > #self.Data) then return nil end

	--If nBytes is nil, return a single byte
	if (nBytes == nil) then nBytes = 1 end
	
	--If nBytes is math.huge, return the remaining data
	if (nBytes == math.huge) then
		local oldPos = self.Position
		self.Position = math.huge
		return self.Data:sub(oldPos)
	end
	
	--Return data and increment position
	local out = self.Data:sub(self.Position, self.Position+(nBytes-1))
	self.Position = self.Position + nBytes
	return out
end

--Method to read a number of bytes without advancing the position
function Stream:lookAhead(nBytes)
	--Return nil if we've reached the end of the stream
	if (self.Position > #self.Data) then return nil end
	
	--Return data and increment position
	local out = self.Data:sub(self.Position, self.Position+(nBytes-1))
	return out
end

--Method to read an unknown number of bytes until a pattern occurs
--If the pattern isn't matched, read until the end of the string
function Stream:readUntil(pattern)
	--Return nil if we've reached the end of the stream
	if (self.Position > #self.Data) then return nil end
	
	--Try to find the pattern
	local remaining = self.Data:sub(self.Position)
	local s, f = remaining:find(pattern)
	
	--Return the remaining data and nil if the string wasn't found
	if (s == nil) then
		local pos = self.Position
		self.Position = #self.Data + 1
		return self.Data:sub(pos), nil
	end
	
	--Otherwise, return data before the begging of the pattern, and the matched pattern
	--Position after this is the first character following the matched patterns
	local pos = self.Position
	self.Position = self.Position + f
	
	return self.Data:sub(pos, pos+(s-2)), self.Data:sub(pos+(s-1), pos+(f-1))
end

--Method to read a pattern match, and return the matched data
function Stream:readMatch(str)
	local str = self:lookAhead(#str):match("^(" .. str .. ")")
	self.Position = self.Position + #str
	return str
end

--Method to skip a given number of bytes
function Stream:skip(nBytes)
	self.Position = self.Position + nBytes
end

--Method to jump back a number of bytes
function Stream:jumpBack(nBytes)
	self.Position = math.max(self.Position - nBytes, 1)
end

--Check if the next set of characters match a string; this returns a boolean and does not
--affect the string position
function Stream:checkNext(str)
	return self:lookAhead(#str):match("^(" .. str .. ")")
end

--Method to check if we have data
function Stream:hasData()
	return self.Position <= #self.Data
end

--LZ4 library
local LZ4 = {}

--Function to decode LZ4 data
function LZ4.decode(streamOrString)
	--Make sure we have a data stream
	local data
	if (type(streamOrString) == "string") then
		data = Stream.new(streamOrString)
	else
		data = streamOrString
	end
	
	--Read LZ4 header
	local compressedLength   = Binary.decodeInt(data:read(4), true)
	local uncompressedLength = Binary.decodeInt(data:read(4), true)
	assert(Binary.decodeInt(data:read(4), true) == 0, "LZ4 E001: Invalid LZ4 header")
	
	--Create byte array for uncompressed data, and a helper function to add data to it
	local uncompressedData = table.create(uncompressedLength, -1)
	local uncompressedPosition = 0
	
	local function addUncompressed(str)
		local i, len = 1, #str
		for i = 1, len do
			assert(uncompressedPosition + i <= uncompressedLength, "LZ4 E004: Too much uncompressed data")
			uncompressedData[uncompressedPosition + i] = string.byte(str, i)
		end
		
		uncompressedPosition = uncompressedPosition + len
	end
	
	--Helper function to read 'matchLength' bytes fromt he end of the stream
	local function readMatchData(offset, matchLength)
		--Get raw string - we may have to repeat this several times
		local startPos = (uncompressedPosition - offset) + 1
		local endPos = uncompressedPosition + matchLength + 1
		
		--Append match data
		local pos = startPos
		local idx = uncompressedPosition + 1
		
		while (idx < endPos) do
			uncompressedData[idx] = uncompressedData[pos]
			idx = idx + 1
			pos = pos + 1
		end
		
		uncompressedPosition = idx - 1
	end
	
	--Generate stream from compressed data
	local data = Stream.new(data:read(compressedLength))
	
	--Repeat until we have no data left
	while true do
		--Read token byte
		local token = Binary.decodeInt(data:read(1))
		local literalLength = math.floor(token/16)
		local matchLength   = token % 16
		
		--If literal length is 15, we read additional bytes to add to the length
		--The length ends at the first byte that is not equal to 255
		if (literalLength == 15) then
			local nextByte 
			repeat
				nextByte = Binary.decodeInt(data:read(1))
				literalLength = literalLength + nextByte
			until nextByte ~= 255
		end
		
		--Read literal data
		if (literalLength > 0) then
			addUncompressed(data:read(literalLength))
		end
		
		--Read offset value, or exit if we have no match data
		if (data:hasData()) then
			local offset = Binary.decodeInt(data:read(2), true)
			
			--0 is an invalid offset
			assert(offset ~= 0, "LZ4 E006: Offset can not be 0")
			
			--If match length is 15, we read additional bytes to add to the length
			--The length ends at the first byte that is not equal to 255
			if (matchLength == 15) then
				local nextByte 
				repeat
					nextByte = Binary.decodeInt(data:read(1))
					matchLength = matchLength + nextByte
				until nextByte ~= 255
			end
			
			--Match data length is actually 4 more than is stored; 4 is the minimum length
			matchLength = matchLength + 4
			
			--Read data to append
			readMatchData(offset, matchLength)
		else
			--Exit the loop; no match data exists
			break
		end
	end
	
	--Check that the decompressed data is the correct length
	assert(uncompressedPosition == uncompressedLength, "LZ4 E005: Uncompressed length is incorrect; expected " .. uncompressedLength .. ", got " .. uncompressedPosition)
	
	--Return decompressed data
	--We have to take a slightly slower and uglier approach here because large compressed 
	--blocks (>254 values) may result in a "too many values to unpack" error
	--
	--Original code:
	--return string.char(unpack(uncompressedData, 1, uncompressedLength))
	
	local output = {}
	for i = 1, uncompressedLength, 256 do
		local append = string.char(unpack(uncompressedData, i, math.min(i + 255, uncompressedLength)))
		table.insert(output, append)
	end
	
	return table.concat(output)
end

--Shorten/Efficiency-related declarations
local byte = string.byte
local char = string.char
local insert = table.insert

--Helper function to compare two byte arrays
function compareByteArrays(array0, array1)
	--Disqualify any cases where array  lengths are not the same
	if (#array0 ~= #array1) then return false end
	
	--Compare byte arrays as strings
	local str0 = char(unpack(array0, 1, #array0))
	local str1 = char(unpack(array1, 1, #array1))
	return str0 == str1
end

--Helper function to read a string from the stream
function readString(data)
	--Read length of the string, and then the string
	local length = Binary.decodeInt(data:read(4), true)
	if (length == 0) then return "" end
	return data:read(length)
end

--Helper function to attempt to set a property on an instance
function setPropertyDirect(instance, name, value)
	instance[name] = value
end

local PROPERTY_NAME_FIX = {
	["size"]        = "Size";
	["Color3uint8"] = "Color";
}

function setProperty(instance, name, value)
	--Some values are serialized under a different name than the property - we can look these up in the table
	local name = PROPERTY_NAME_FIX[name] or name
	
	--Attempt to set the property
	local ok, errorStr = pcall(setPropertyDirect, instance, name, value)
	if (not ok) then
		--warn("Failed to set property " .. name .. " on " .. instance.ClassName .. " to " .. tostring(value) .. ": " .. errorStr)
		
		if (instance:IsA("BaseScript") and name == "Source") then
			print("Source: ")
			print(value)
		end
	end
end

--Helper function to read interleaved integers
function readInterleaved(stream, valueLength, valueCount)
	--Pre-allocate large table; fill with 0's as placeholders; passing a table to this will cause the
	--array to be filled with the same table pointer, resulting in a bug where all returned values are
	--the same
	local values = table.create(valueCount, 0)
	
	--Fill with new tables with the correct length
	for i = 1, valueCount do
		values[i] = table.create(valueLength, 0)
	end
	
	for i = 1, valueLength do
		--Read byte for each value
		for j = 1, valueCount do
			values[j][i] = stream:read(1)
		end
	end
	
	--Concatenate all the values
	for i = 1, valueCount do
		values[i] = table.concat(values[i])
	end
	
	return values
end

--Helper function to read a Roblox integer
--This undoes the transformation integers undergo before being stored to a file
function readRobloxInt(strInput, littleEndian)
	local int = Binary.decodeInt(strInput, littleEndian)
	
	if (int % 2 == 0) then
		return int / 2
	else
		return -(int+1)/2
	end
end

--Helper function to read a Roblox Float
--The sign is the LSB instead of the MSB - the rest of the float is left-shifted by 1 bit
function readRobloxFloat(strInput)
	--Convert Roblox float to IEEE754 float
	local int = Binary.decodeInt(strInput, false)
	local sign = int % 2
	local str = Binary.encodeInt(sign*0x80000000 + (int-sign)/2, 4)
	
	--Decode string to float
	return Binary.decodeFloat(str, false)
end

--NormalID list
local NORMAL_IDS = {
	[0x01] = Enum.NormalId.Front;
	[0x02] = Enum.NormalId.Bottom;
	[0x04] = Enum.NormalId.Left;
	[0x08] = Enum.NormalId.Back;
	[0x10] = Enum.NormalId.Top;
	[0x20] = Enum.NormalId.Right;
}

--Axis list
local AXIS_VALUES = {
	[0x00] = Vector3.new( 0, 0, 0);
	[0x01] = Vector3.new( 0, 0, 1);
	[0x02] = Vector3.new( 0, 1, 0);
	[0x03] = Vector3.new( 0, 1, 1);
	[0x04] = Vector3.new( 1, 0, 0);
	[0x05] = Vector3.new( 1, 0, 1);
	[0x06] = Vector3.new( 1, 1, 0);
	[0x07] = Vector3.new( 1, 1, 1);
}

--CFrame special angles
local CF000 = 0
local CF090 = math.pi/2
local CF180 = math.pi
local CF270 = -math.pi/2
local CFRAME_SPECIAL_ANGLES = {
	[0x02] = CFrame.Angles(CF000, CF000, CF000);
	[0x03] = CFrame.Angles(CF090, CF000, CF000);
	[0x05] = CFrame.Angles(CF180, CF000, CF000);
	[0x06] = CFrame.Angles(CF270, CF000, CF000);
	[0x07] = CFrame.Angles(CF180, CF000, CF270);
	[0x09] = CFrame.Angles(CF090, CF090, CF000);
	[0x0A] = CFrame.Angles(CF000, CF000, CF090);
	[0x0C] = CFrame.Angles(CF270, CF270, CF000);
	
	[0x0D] = CFrame.Angles(CF270, CF000, CF270);
	[0x0E] = CFrame.Angles(CF000, CF270, CF000);
	[0x10] = CFrame.Angles(CF090, CF000, CF090);
	[0x11] = CFrame.Angles(CF180, CF090, CF000);
	[0x14] = CFrame.Angles(CF180, CF000, CF180);
	[0x15] = CFrame.Angles(CF270, CF000, CF180);
	[0x17] = CFrame.Angles(CF000, CF000, CF180);
	[0x18] = CFrame.Angles(CF090, CF000, CF180);
	
	[0x19] = CFrame.Angles(CF000, CF000, CF270);
	[0x1B] = CFrame.Angles(CF090, CF270, CF000);
	[0x1C] = CFrame.Angles(CF180, CF000, CF090);
	[0x1E] = CFrame.Angles(CF270, CF090, CF000);
	[0x1F] = CFrame.Angles(CF090, CF000, CF270);
	[0x20] = CFrame.Angles(CF000, CF090, CF000);
	[0x22] = CFrame.Angles(CF270, CF000, CF090);
	[0x23] = CFrame.Angles(CF180, CF270, CF000);
}

--Functions to read properties to instances
local PROPERTY_FUNCTIONS
PROPERTY_FUNCTIONS = {
	--0x00 is invalid

	--String
	[0x01] = function(nInstances, stream)
		local values = table.create(nInstances, "")
		for i = 1, nInstances do 
			values[i] = readString(stream)
		end
		
		return values
	end;
	
	--Boolean
	[0x02] = function(nInstances, stream)
		local values = table.create(nInstances, false)
		for i = 1, nInstances do 
			values[i] = stream:read(1) ~= "\0"
		end
		
		return values
	end;
	
	--Int32
	[0x03] = function(nInstances, stream)
		local valuesRaw = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = readRobloxInt(valuesRaw[i])
		end
		
		return values
	end;
	
	--Float
	[0x04] = function(nInstances, stream)
		local valuesRaw = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = readRobloxFloat(valuesRaw[i])
		end
		
		return values
	end;
	
	--Double
	[0x05] = function(nInstances, stream)
		local valuesRaw = readInterleaved(stream, 8, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = Binary.decodeDouble(valuesRaw[i]:reverse())
		end
		
		return values
	end;
	
	--UDim
	[0x06] = function(nInstances, stream)
		local floatsRaw = readInterleaved(stream, 4, nInstances)
		local int32sRaw = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = UDim.new(
				readRobloxFloat(floatsRaw[i]),
				readRobloxInt  (int32sRaw[i])
			)
		end
		
		return values
	end;
	
	--UDim2
	[0x07] = function(nInstances, stream)
		local scalesX  = readInterleaved(stream, 4, nInstances)
		local scalesY  = readInterleaved(stream, 4, nInstances)
		local offsetsX = readInterleaved(stream, 4, nInstances)
		local offsetsY = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = UDim2.new(
				readRobloxFloat(scalesX[i]),
				readRobloxInt  (offsetsX[i]),
				readRobloxFloat(scalesY[i]),
				readRobloxInt  (offsetsY[i])
			)
		end
		
		return values
	end;
	
	--Ray
	[0x08] = function(nInstances, stream)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			local originX = Binary.decodeFloat(stream:read(4):reverse())
			local originY = Binary.decodeFloat(stream:read(4):reverse())
			local originZ = Binary.decodeFloat(stream:read(4):reverse())
			local dirX    = Binary.decodeFloat(stream:read(4):reverse())
			local dirY    = Binary.decodeFloat(stream:read(4):reverse())
			local dirZ    = Binary.decodeFloat(stream:read(4):reverse())
			
			values[i] = Ray.new(
				Vector3.new(originX, originY, originZ), 
				Vector3.new(dirX, dirY, dirZ)
			)
		end
		
		return values
	end;
	
	--Faces
	[0x09] = function(nInstances, stream)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			local index = Binary.decodeInt(stream:read(1))
			local surface = NORMAL_IDS[index]
			values[i] = surface
			
			assert(surface ~= nil, "RBXMReader E007: Unknown surface type: " .. index)
		end
		
		return values
	end;
	
	--Axis
	[0x0A] = function(nInstances, stream)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			local index = Binary.decodeInt(stream:read(1))
			local axis = AXIS_VALUES[index]
			values[i] = axis
			
			assert(axis ~= nil, "RBXMReader E008: Unknown axis type: " .. index)
		end
		
		return values
	end;
	
	--BrickColor
	[0x0B] = function(nInstances, stream)
		local valuesRaw = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = BrickColor.new(Binary.decodeInt(stream:read(4)))
		end
		
		return values
	end;
	
	--Color3
	[0x0C] = function(nInstances, stream)
		local valuesRawR = readInterleaved(stream, 4, nInstances)
		local valuesRawG = readInterleaved(stream, 4, nInstances)
		local valuesRawB = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = Color3.new(
				readRobloxFloat(valuesRawR[i]),
				readRobloxFloat(valuesRawG[i]),
				readRobloxFloat(valuesRawB[i])
			)
		end
		
		return values
	end;
	
	--Vector2
	[0x0D] = function(nInstances, stream)
		local valuesRawX = readInterleaved(stream, 4, nInstances)
		local valuesRawY = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = Vector2.new(
				readRobloxFloat(valuesRawX[i]),
				readRobloxFloat(valuesRawY[i])
			)
		end
		
		return values
	end;
	
	--Vector3
	[0x0E] = function(nInstances, stream)
		local valuesRawX = readInterleaved(stream, 4, nInstances)
		local valuesRawY = readInterleaved(stream, 4, nInstances)
		local valuesRawZ = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = Vector3.new(
				readRobloxFloat(valuesRawX[i]),
				readRobloxFloat(valuesRawY[i]),
				readRobloxFloat(valuesRawZ[i])
			)
		end
		
		return values
	end;
	
	--0x0F is invalid
	
	--CFrame
	[0x10] = function(nInstances, stream)
		local cframeAngles = table.create(nInstances, CFrame.new())
		local values = table.create(nInstances, CFrame.new())
		
		--Get CFrame angles
		for i = 1, nInstances do
			--Check CFrame type
			local byteValue = Binary.decodeInt(stream:read(1))
			local specialAngle = CFRAME_SPECIAL_ANGLES[byteValue]
			
			--If we have a special value, store it. Otherwise, read 9 untransformed floats to get
			--the rotation matrix
			if (specialAngle == nil) then
				local matrixValues = table.create(9, 0)
				for j = 1, 9 do
					matrixValues[j] = Binary.decodeFloat(stream:read(4), true)
				end
				
				cframeAngles[i] = CFrame.new(0, 0, 0, unpack(matrixValues, 1, 9))
			else
				cframeAngles[i] = specialAngle
			end
		end
		
		--Read position data - invoke function for Vector3s
		local positions = PROPERTY_FUNCTIONS[0x0E](nInstances, stream)
		
		--Generate final CFrames
		for i = 1, nInstances do
			values[i] = CFrame.new(positions[i]) * cframeAngles[i]
		end
		
		return values
	end;
	
	--Quaternion; unsure how this is implemented. Might require some experimentation later
	--TODO: Fix this
	[0x11] = function(nInstances, stream)
		warn("RBXMReader W001: Using quaternions")
		return PROPERTY_FUNCTIONS[0x10](nInstances, stream)
	end;
	
	--Enums - Roblox accepts EnumProperty = number so we can just return an array of numbers
	[0x12] = function(nInstances, stream)
		local valuesRaw = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		for i = 1, nInstances do
			values[i] = Binary.decodeInt(valuesRaw[i])
		end
		
		return values
	end;
	
	--Instance references
	[0x13] = function(nInstances, stream, instances)
		local valuesRaw = readInterleaved(stream, 4, nInstances)
		local values = table.create(nInstances, 0)
		
		local lastValue = 0
		for i = 1, nInstances do
			local rawValue = readRobloxInt(valuesRaw[i])
			local newValue = rawValue + lastValue
			lastValue = newValue
			values[i] = instances[newValue]
		end
		
		return values
	end;
	
	--Color3uint8
	[0x1A] = function(nInstances, stream)
		local valuesR = table.create(nInstances, 0)
		local valuesG = table.create(nInstances, 0)
		local valuesB = table.create(nInstances, 0)
		local values  = table.create(nInstances, 0)
		
		for i = 1, nInstances do valuesR[i] = Binary.decodeInt(stream:read(1)) end
		for i = 1, nInstances do valuesG[i] = Binary.decodeInt(stream:read(1)) end
		for i = 1, nInstances do valuesB[i] = Binary.decodeInt(stream:read(1)) end
		
		for i = 1, nInstances do 
			values[i] = Color3.fromRGB(valuesR[i], valuesG[i], valuesB[i])
		end
		
		return values
	end;
}

--Function to read an RBXM file
function readRBXM(binaryData)
	--Create new stream
	local data = Stream.new(binaryData)
	
	--Read & verify file header
	--First 16 bytes should be 3C 72 6F 62 6C 6F 78 21 89 FF 0D 0A 1A 0A 00 00
	local headerActualBytes = { byte(data:read(16), 1, 16) }
	local headerExpectedBytes = {
			0x3C, 0x72, 0x6F, 0x62, 
			0x6C, 0x6F, 0x78, 0x21, 
			0x89, 0xFF, 0x0D, 0x0A, 
			0x1A, 0x0A, 0x00, 0x00 
		}
	
	assert(
			compareByteArrays(headerActualBytes, headerExpectedBytes),
			"RBXMReader E001: Invalid RBXM header"
		)
	
	--Create a new model to dump instances into
	local rootModel = Instance.new("Model")
	
	--Read number of unique types
	local nUniqueTypes = Binary.decodeInt(data:read(4), true)
	local nObjects     = Binary.decodeInt(data:read(4), true)
	
	--Verify that the next 8 bytes are 0
	assert(Binary.decodeInt(data:read(4)) == 0, "RBXMReader E002: Invalid RBXM header")
	assert(Binary.decodeInt(data:read(4)) == 0, "RBXMReader E002: Invalid RBXM header")
	
	--Create lookup tables for information
	local METADATA      = {}
	local SHAREDSTRINGS = {}
	local TYPES         = {}
	local INSTANCES     = {}
	
	--Read header data
	while true do
		--Read type through look-ahead, once we get a PROP, exit
		local typeStr = data:lookAhead(4)
		if (typeStr == "PROP") then break end
		
		--Read type bytes and compressed data
		local headerType = data:read(4)
		local headerData = Stream.new(LZ4.decode(data))
		
		--Read META, SSTR and INST records
		if (headerType == "META") then
			--Read number of key/value pairs (possibly?); this *should* be 1 at the moment
			local length, j = Binary.decodeInt(headerData:read(4), true), 0
			
			for j = 1, length do
				--Read key/value pairs, and store the result
				local key   = readString(headerData)
				local value = readString(headerData)
				METADATA[key] = value
			end
		elseif (headerType == "SSTR") then
			--Read shared strings
			--TODO: Implement this; low priority currently
		elseif (headerType == "INST") then
			--Read INST thing
			local index      = Binary.decodeInt(headerData:read(4), true)
			local className  = readString(headerData)
			local isService  = headerData:read(1) ~= "\0"
			local nInstances = Binary.decodeInt(headerData:read(4), true)
			local referents  = readInterleaved(headerData, 4, nInstances)
			
			--Reading RBXM, so we should never have a service
			assert(not isService, "RBXMReader E004: File contains services")
			
			--Create the instances
			local instances, j, referent = table.create(nInstances, 0), 1, 0
			for j = 1, nInstances do
				referent = referent + readRobloxInt(referents[j])
				local instance = Instance.new(className)
				instances[j] = instance
				INSTANCES[referent] = instance
			end
			
			--Store the type reference
			TYPES[index] = {
				Index         = index;
				ClassName     = className;
				IsService     = isService;
				InstanceCount = nInstances;
				Instances     = instances;
			}
		else
			--Error because of unexpected header type
			error("RBXMReader E003: Unexpected header object type: " .. headerType)
		end
	end
	
	--Read actual body
	while true do
		--Read object type and decompress its data
		local objType = data:read(4)
		local objData = Stream.new(LZ4.decode(data))
		
		--These seem to all be PROP elements but we'll make this easily expandable for the future
		if (objType == "PROP") then
			--Handle property descriptor
			local classIndex   = Binary.decodeInt(objData:read(4), true)
			local propertyName = readString(objData)
			local propertyType = Binary.decodeInt(objData:read(1))
			local descriptor   = TYPES[classIndex]
			local nInstances   = descriptor.InstanceCount
			local instances    = descriptor.Instances
			
			--Check for property type
			local func = PROPERTY_FUNCTIONS[propertyType]
			--assert(func ~= nil, "RBXMReader E006: Invalid or unknown property type: " .. propertyType)
			
			if (func ~= nil) then
				--Get property values
				local values = func(nInstances, objData, INSTANCES)
				
				--Iterate over instances with this function
				local j
				for j = 1, nInstances do
					setProperty(instances[j], propertyName, values[j])
				end
			else
				--Print any properties we'd want to check here; debugging reasons
			end
		elseif (objType == "PRNT") then
			--Parent data
			--Read length of the list
			--May have to skip one character? We have an extra 0x00
			objData:read(1)
			local parentLength = Binary.decodeInt(objData:read(4), true)
			local referents = readInterleaved(objData, 4, parentLength)
			local parents   = readInterleaved(objData, 4, parentLength)
			
			local cReferent = 0
			local cParent   = 0
			for i = 1, parentLength do
				cReferent = cReferent + readRobloxInt(referents[i])
				cParent   = cParent   + readRobloxInt(parents[i])
				
				local instance = INSTANCES[cReferent]
				if (cParent == -1) then
					instance.Parent = rootModel
				else
					instance.Parent = INSTANCES[cParent]
				end
			end
			
			break
		else
			error("RBXMReader E005: File contains unexpected body element: " .. objType)
		end
	end
	
	--Ending bytes
	local endExpectedBytes = { 
		0x45, 0x4E, 0x44, 0x00, 
		0x00, 0x00, 0x00, 0x00, 
		0x09, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 
		0x3C, 0x2F, 0x72, 0x6F, 
		0x62, 0x6C, 0x6F, 0x78, 
		0x3E 
	}
	
	local footerBytes = { byte(data:read(25), 1, 25) }
	
	assert(
			compareByteArrays(footerBytes, endExpectedBytes),
			"RBXMReader E006: Invalid RBXM footer"
		)
	
	--Return root object
	return rootModel
end

return {
	readRBXM = readRBXM;
}
