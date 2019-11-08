--==============================================================================
-- Replaces original conversion method using COL.EXE utility:
-- @col (1 20)(31 35) < aspisys.exp >f1_board.map
--==============================================================================

--------------------------------------------------------------------------------
-- Save a text file as Windows, Linux, or Mac

function savefile(filename,lines,eol)
  eol = eol or 'Linux'                  --default platform
  assert(type(filename) == 'string','String expected for 1st arg')
  assert(type(lines) == 'table','Table expected for 2nd arg')
  assert(type(eol) == 'string','String expected for 3rd arg')
  eol = eol:lower()
  local platforms = {
    ['win'    ] = '\013\010',
    ['win32'  ] = '\013\010',
    ['win64'  ] = '\013\010',
    ['windows'] = '\013\010',
    ['mac'    ] = '\013',
    ['linux'  ] = '\010',
  }
  eol = platforms[ eol ]
  if eol == nil then
    error('Unsupported platform: '..eol)
    return
  end
  f = io.open(filename,'wb')
  f:write(table.concat(lines,eol),eol)
  f:close()
end

--------------------------------------------------------------------------------

local file,label,value = {}

for line in io.lines 'aspisys.exp' do
  label,value = line:match '(%S+)%s+set%s+(%$%x%x%x%x)'
  if label ~= nil then
    file[#file+1] = ('%-19s %s'):format(label,value)
  end
end

table.sort(file)

savefile('f1_board.map',file)
