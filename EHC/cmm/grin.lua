Frontends.GRIN      = Frontends.GRIN or { } -- initialize table
Frontends.GRIN.dir  = "../bin/10/"

function Frontends.GRIN.file(name)
  return Frontends.GRIN.dir .. '/' .. name -- does not port to windows!
end

-- Make the linker pass GRIN runtime and standard libraries
-- Ld.libs = Ld.libs .. " " .. Frontends.GRIN.file("runtime.o") 
--                   .. " " .. Frontends.GRIN.file("stdlib.a")

-- Tell the driver how to convert a .grin fle into a .cmm file
function CMD.compilertab[".grin"](file)
  local out      = CMD.outfilename(file, ".cmm") -- compiler generate output to this path
  local grinc    = Frontends.GRIN.file("grinc")
  local options  = "-v0"
  --if Frontends.GRINC.aOption then options = "..." end -- allow options
  CMD.exec(grinc .. " " .. options .. " \"" .. file .. "\"")
  return out
end

-- Allow Interpret and pretty printing the generated C-- code from a .grin file
CMD.interptab[".grin"] = CMD.compilertab[".grin"]
CMD.prettytab[".grin"] = CMD.compilertab[".grin"]
