with commandParser; Use commandParser;
with MemoryStore;
with PIN;
with Stack; use Stack;

package commandHandler is

   procedure Handle_Lock(Cmd : in Command;
                         State : in out Boolean;
                         Master_Str : in out String;
                         Master_Pin : in out PIN.PIN);

   procedure Handle_Stack(Cmd : in Command; S : in out Stack_type);

   procedure Handle_Arithmetic(Cmd : in Command; S : in out Stack_type);

   procedure Handle_Memory(Cmd : in Command; S : in out Stack_type; Mem : in out MemoryStore.Database);

end commandHandler;
