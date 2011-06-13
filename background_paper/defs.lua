function enbrace(text)
  return "{" .. text .. "}"
end

function enbracket(text)
  return "[" .. text .. "]"
end

function braceCmd(name, text)
  return "\\" .. name .. enbrace(text)
end

function bracketCmd(name, text)
  return "\\" .. name .. enbracket(text)
end

function directlua(l)
  return braceCmd("directlua", l)
end

function textit(txt)
  return braceCmd("textit", txt)
end

function newcommand(name, args, definition)
  return braceCmd("newcommand", "\\" .. name) .. enbracket(args) .. enbrace(definition)
end

function renewcommand(name, args, definition)
  return braceCmd("renewcommand", "\\" .. name) .. enbracket(args) .. enbrace(definition)
end

function makeatletter(str)
  return "\\makeatletter" .. str .. "\\makeatother"
end

function ifnextchar(char, ifcase, elsecase)
  return braceCmd("@ifnextchar", char) .. enbrace(ifcase) .. enbrace(elsecase)
end

function defAbbr(name, abbrev)
  return makeatletter(newcommand(name, 0,
    ifnextchar(".",
      textit(abbrev),
      textit(abbrev) .. "." .. "\\@\\xspace")))
end

function defAbbrs(abbrs)
  a = ""
  for cmd, abbr in pairs(abbrs) do
    a = a .. defAbbr(cmd, abbr)
  end
  return a
end

abbrevs = { ["vs"]="v.s", ["eg"]="e.g", ["ie"]="i.e", ["etc"]="etc" }
abbrevs = defAbbrs(abbrevs)

function beginEnv(envName) return braceCmd("begin", envName) end
function endEnv(envName)   return braceCmd("end",   envName) end

function putInEnv(envName, t)
  return beginEnv(envName) .. t .. endEnv(envName)
end

function center(t) return putInEnv("center", t) end

quotePrice = 0

function currencyFormat(d)
  return "£" .. quotePrice
end

tex.sprint(
  abbrevs ..
  newcommand("fancyhrule", 0, center("")) ..
  newcommand("quoteItem", 3,
    bracketCmd("item", "#1---£#2#3") ..
    directlua("quotePrice = quotePrice + #2")) ..
  newcommand("quotePrice", 0, directlua("tex.sprint(currencyFormat(quotePrice))"))
  )