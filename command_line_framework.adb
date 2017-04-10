--/ Command_Line_Framework package body:

with Ada.Strings.Hash;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Maps.Constants; 

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Text_IO.Unbounded_IO;

use Ada.Strings.Maps.Constants; 
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Text_IO.Unbounded_IO;

package body Command_Line_Framework is

   --/ Warnings and error handling:
   
   procedure Report_Error (Message:     String  := ""; 
                           Is_Terminal: Boolean := False;
                           Is_Internal: Boolean := True) is

      Full_Message: Unbounded_String;
   begin
      if Is_Internal then
         Full_Message.Append( "*INTERNAL* ");
      end if;
      if Is_Terminal then
         Full_Message.Append( "Error");
      else
         Full_Message.Append( "Warning");
      end if;
      if Message /= "" then
         Full_Message.Append( ": " );
         Full_Message.Append( Message );
      end if;
      if Report_Event /= null then
         Report_Event( new Error_Event'(Full_Message, Terminal, Internal) );
      else
         Put( Current_Error, Full_Message );
         if Terminal then
            raise Program_Error;
         end if;
      end if;
   end;
   
   procedure Report_Error (Message:     String  := ""; 
                           Is_Terminal: Boolean := False) is
   begin
      Report_Error( Message, Is_Terminal, Is_Internal => False );
   end;
   
   procedure Report_Debug (Message: String) is
   begin
      Put( Current_Error, "[DEBUG] " & Message );
   end;
   
   --\
   --/ Option descriptor operations:

   function Short_Options return Option_Lists.List is
   
      Result: Option_Lists.List;
   begin
      for Desc of Option_Descriptors.Options loop
         if Length(Desc.Option_Name) = 1 then
            Result.Append(Desc);
         end if;
      end loop;
      return Result;
   end;
      
   function Long_Options return Option_Lists.List is
   
      Result: Option_Lists.List;
   begin
      for Desc of Option_Descriptors.Options loop
         if Length(Desc.Option_Name) > 1 then
            Result.Append(Desc);
         end if;
      end loop;
      return Result;
   end;
   
   function Normalize_Option_Name (Unnormal: String) return String is
   begin
      if Length(Unnormal) = 1 then
         return Unnormal;
      else
         return Translate( Unnormal, Lower_Case_Map ); -- long option names are case-insensitive
      end if;
   end;
      
   procedure Search_Options (Name:       in  String; 
                             Found:      out Boolean; 
                             Descriptor: out Option_Descriptor) is
                                  
      Sought_Name: constant String := Normalize_Option_Name(Name);
   begin
      if Option_Descriptors = null then
         Found := False;
         return;
      end if;
      for Desc of Option_Descriptors.Options loop
         declare
            Target_Name: constant String := Normalize_Option_Name(Desc.Name);
         begin
            if Target_Name = Sought_Name then
               Found := True;
               Descriptor := Desc;
               return;
            end if;
         end;
      end loop;
      Found := False;
   end;
   
   function Is_Legal_Option_Name (Option_Name: in String) return Boolean is
   begin
      return Length(Option_Name) > 0 and then ???;
   end;
      
   procedure Insert_Option (Option_Name:       in String;
                            Description:       in String := "";
                            Parameter_Moniker: in String := "") is
                                       
      Desc:     Option_Descriptor;
      Found:    Boolean;
      Is_Short: constant Boolean := Length(Option_Name) = 1;
   begin
      if Length(Option_Name) = 0 then
         Report_Error( "Insert_Option: Empty option name", Terminal => True );
         return;
      end if;
      if not Is_Legal_Option_Name(Option_Name) then
         Report_Error( "Insert_Option: Illegal option name", Terminal => True );
         return;
      end if;
      if Is_Short and Parameter_Moniker /= "" then
         Report_Error( "Insert_Option: '" & Option_Name & "' is a short option, so it cannot have a parameter", Terminal => True );
         return;
      end if;
      Search_Options( Option_Name, Found, Desc );
      if Found then
         Report_Error( "Insert_Option: '" & Option_Name & "' already registered", Terminal => True );
         return;
      end if;
      if Option_Descriptors = null then
         Option_Descriptors := new Option_Descriptor_Set;
      end if;
      Option_Descriptors.Options.Append
         ( new Option_Descriptor'(Option_Name       => Option_Name, 
                                  Description       => Description,
                                  Parameter_Moniker => Parameter_Moniker) );
   end;
      
   --\
   --/ Program help message:
   
   procedure Report_Program_Help (Option_Name:     in  String;
                                  Parameter_Value: in  String;
                                  Is_Terminal:     out Boolean) is
   
      Tab_Stop_1: constant Ada.Text_IO.Positive_Count := 5;
      Tab_Stop_2: constant Ada.Text_IO.Positive_Count := 20;
   begin
      Is_Terminal := True; -- if we print this message, the program should stop and do nothing else
      if Report_Event /= null then
         Report_Event( new Help_Event'(Option_Descs) );
         return;
      end if;
      Put(Program_Name);
      --if Length(Short_Options) > 0 then
         for Opt of Short_Options loop
            Put(" [-");
            Put(Opt.Letter);
            Put("]");
         end loop;
      --end if;
      --if Length(Long_Options) > 0 then
         for Opt of Long_Options loop
            Put(" [--");
            Put(Opt.Name);
            if Opt.Parameter_Moniker /= "" then
               Put("=");
               Put(Opt.Parameter_Moniker);
            end if;
            Put("]");
         end loop;
      --end if;
      if Program_Argument_Moniker /= "" then
         Put(" [");
         Put(Program_Argument_Moniker);
         Put("]...");
      end if;
      if Program_Description /= Null_Unbounded_String then
         Set_Col(Tab_Stop_2);
         Put(Program_Description);
      end if;
      New_Line;
      for Opt of Short_Options loop
         Set_Col(Tab_Stop_1);
         Put("-");
         Put(Opt.Letter);
         Set_Col(Tab_Stop_2);
         Put(Opt.Description);
         New_Line;
      end loop;
      for Opt of Long_Options loop
         Set_Col(Tab_Stop_1);
         Put("--");
         Put(Opt.Name);
         if Opt.Parameter_Moniker /= "" then
            Put("=");
            Put(Opt.Parameter_Moniker);
         end if;
         Set_Col(Tab_Stop_2);
         Put(Opt.Description);
         New_Line;
      end loop;
      if Program_Argument_Moniker /= Null_Unbounded_String 
      and Program_Argument_Description /= Null_Unbounded_String then
         Set_Col(Tab_Stop_1);
         Put(Program_Argument_Moniker);
         Set_Col(Tab_Stop_2);
         Put(Program_Argument_Description);
         New_Line;
      end if;
   end;

   --\
   --/ Standard options:
   
   Interpret_Standard_Option_Chain: Option_Interpreter := null;
   
   procedure Interpret_Standard_Option (Option_Name:      in  String;
                                        Parameter_Value:  in  String;
                                        Is_Terminal:      out Boolean) is
   begin
      if Option_Name = "h" or Option_Name = "?" or Option_Name = "help" then
         Report_Program_Help( Option_Name, Parameter_Value, Is_Terminal );
      else
         if Interpret_Standard_Option_Chain /= null then
            Interpret_Standard_Option_Chain( Option_Name, Parameter_Value, Is_Terminal );
         else
            Report_Error( "Option '" & Option_Name & "' not recognised", Is_Terminal => True );
         end if;
      end;
   end;
   
   procedure Insert_Standard_Options_Interpreter is

      Help_Desc: constant String := "outputs usage information (does nothing else)";
   begin
      if Interpret_Option = Interpret_Standard_Option'Access
      or Interpret_Standard_Option_Chain /= null then
         return; -- it has already been inserted
      end if;
      Interpret_Standard_Option_Chain := Interpret_Option; -- save current interpreter
      Interpret_Option := Interpret_Standard_Option'Access;
      Insert_Option( "h",    Help_Desc );
      Insert_Option( "?",    Help_Desc );
      Insert_Option( "help", Help_Desc );
   end;
   
   --\
   --/ Interpret the program arguments (implementation):

   procedure Interpret_Program_Arguments (Normal_Args: out Argument_List;
                                          Is_Terminal: out Boolean) is
   
      No_More_Options: Boolean := False;
      Option_Text:     Unbounded_String;
      Option_Name:     Unbounded_String;
      Option_Letter:   Character;
      Option_Param:    Unbounded_String;
      Option_Found:    Boolean;
      Long_Option:     Long_Option_Descriptor;
      Short_Option:    Short_Option_Descriptor;
      P1:              Natural;      
   begin
      Normal_Args.Clear;
      for Arg of Raw_Arguments loop
         if not No_More_Options and then Length(Arg) > 0 and then Arg(1) = '-' then -- some kind of option(s)
            if Length(Arg) = 1 then -- single '-' signals that all following arguments are to be treated as normal
               No_More_Options := True;
            else -- interpret the option(s)
               if Arg(2) = '-' then -- one long option, begins with "--"
                  Set_Unbounded_String( Option_Text, Arg(3..Length(Arg)) ); -- get all of the option text
                  P1 := Index(Option_Text,"="); -- does it have an '=' in it?
                  if P1 > 0 then -- if it does, extract option name and option parameter value
                     Unbounded_Slice( Option_Text, Option_Name, 1, P1 - 1 ); -- option name (to the left of '=')
                     Unbounded_Slice( Option_Text, Option_Param, P1 + 1, Length (Option_Text) ); -- option parameter value (to the right of '=')
                  else -- otherwise it is all just the option name
                     Option_Name  := Option_Text;
                     Option_Param := Null_Unbounded_String; -- option parameter value is ""
                  end if;
                  Report_Debug( "Option_Name = [" & To_String(Option_Name) & "]" ); -- $DEBUG
                  Search_Long_Options( To_String(Option_Name), Option_Found, Long_Option );
                  if not Option_Found then -- not found
                     Report_Error( "Long option not recognised [" & To_String (Arg) & "] - ignored" );
                  else -- found, so invoke the option's interpreter on the option name and parameter value
                     if Interpret_Option = null then
                        Is_Terminal := True;
                     else
                        Interpret_Option( To_String(Option_Name), To_String(Option_Param), Is_Terminal ); -- invoke the option interpreter
                     end if;
                     exit when Is_Terminal;
                  end if;
               else -- possibly multiple short option(s)
                  for i in 2..Length(Arg) loop -- for each option letter
                     Option_Letter := Element(Arg,i);
                     Search_Short_Options( Option_Letter, Option_Found, Short_Option ); -- find it in the short options list
                     if not Option_Found then
                        Report_Errors( "Short option not recognised [" & Option_Letter & "] - ignored" );
                     else -- found, so invoke the option's interpreter on the option letter
                     if Interpret_Option = null then
                        Is_Terminal := True;
                     else
                        Interpret_Option( String'(1 => Option_Letter), Parameter_Value => "", Is_Terminal ); -- invoke the option interpreter
                     end if;
                     exit when Is_Terminal;
                     end if;
                  end loop;
               end if;
            end if;
         else -- normal argument
            Normal_Args.Append( To_String(Arg) );
         end if;
      end loop;
   end;
   
   --\
   --/ Process the program's arguments (implementation):
   
   procedure Process_Program_Arguments is
   
      Normal_Args:        Argument_List;
      Is_Terminal: Boolean;
   begin
      Interpret_Program_Arguments( Normal_Args, Is_Terminal );
      if not Is_Terminal then
         for Arg of Normal_Args loop
            Process_Argument(Arg);
         end loop;
      end if;
   end;
   
   --\
   
begin
   --/ Initialise the raw argument list:   
   Raw_Arguments.Clear;
   for N in range 1 .. Ada.Command_Line.Argument_Count loop
      Raw_Arguments.Append( To_Unbounded_String(Ada.Command_Line.Argument(N)) );
   end loop;   
   --\   
end;

--\

