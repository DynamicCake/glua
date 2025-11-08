local str = "     Lua is an embedable language --------   "
local result = str
  :match("^%s*(.-)%s*$") 
  :gsub("embedable", "embeddable")    
  :upper()                        
  :sub(1, 29)

return result
