Frontends.EH      = Frontends.EH or { } -- initialize table
Frontends.EH.dir  = "../bin/8/"

function Frontends.EH.file(name)
  return Frontends.EH.dir .. '/' .. name -- does not port to windows!
end

-- Make the linker pass EH runtime and standard libraries
-- Ld.libs = Ld.libs .. " " .. Frontends.EH.file("stdlib.eh")

-- Tell the driver how to convert a .eh fle into a .grin file
function CMD.compilertab[".eh"](file)
  local out      = CMD.outfilename(file, ".grin") -- compiler generate output to this path
  local ehc      = Frontends.GRIN.file("ehc")
  local options  = "-v1 -cgrin"
  CMD.outfilename(file, ".core") -- ehc also generates a .core file
  --if Frontends.GRINC.aOption then options = "..." end -- allow options
  CMD.exec(ehc .. " " .. options .. " \"" .. file .. "\"")
  return out
end

-- Allow Interpretting and pretty printing the generated C-- code from a .eh file
CMD.interptab[".eh"] = CMD.compilertab[".eh"]
CMD.prettytab[".eh"] = CMD.compilertab[".eh"]
