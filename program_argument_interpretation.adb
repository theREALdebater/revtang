with Ada.Strings.Hash;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Maps.Constants; 
with Ada.Strings.Unbounded;

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Text_IO;

use Ada.Strings.Maps.Constants; 
use Ada.Strings.Unbounded;

package body Program_Argument_Interpretation is
   
   function Character_Hash (Key : Character) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String'(1 => Key));
   end;

   package Short_Option_Maps is new Ada.Containers.Hashed_Maps (Character, Option_Interpreter, Character_Hash, "=");
   use Short_Option_Maps;
   
   package Long_Option_Maps is new Ada.Containers.Indefinite_Hashed_Maps (String, Option_Interpreter, Ada.Strings.Hash_Case_Insensitive, "=");
   use Long_Option_Maps;
   
   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use String_Lists;
   
   Short_Options : Short_Option_Maps.Map;
   Long_Options  : Long_Option_Maps.Map;
   Normal_Args   : String_Lists.List;
   Errors        : String_Lists.List;
   
   type String_Array is array (Positive range <>) of access String;
   
   type Dynamic_String_Array is access String_Array;
   
   Normal_Args_Array : Dynamic_String_Array := null;
   Errors_Array      : Dynamic_String_Array := null;
   
   procedure Register_Short_Option (Letter : Character; Interpreter : Option_Interpreter) is
   begin
      if Short_Options.Contains (Letter) then
         raise Program_Error;
      end if;
      Short_Options.Insert (Letter, Interpreter);
   end;
   
   procedure Register_Long_Option (Name : String; Interpreter : Option_Interpreter) is
   begin
      if Long_Options.Contains (Name) then
         raise Program_Error;
      end if;
      Long_Options.Insert (Name, Interpreter);
   end;
   
   function Copy_List_To_Array (List : in String_Lists.List) return Dynamic_String_Array is
      i : Natural := 0;
      Result : constant Dynamic_String_Array := new String_Array (1 .. Integer (List.Length));
   begin
      for S of List loop
         i := i + 1;
         Result (i) := new String'(S);
      end loop;
      return Result;
   end;
      
   procedure Interpret_Program_Arguments is
      
      No_More_Options : Boolean := False;
      Arg             : Unbounded_String;
      Option_Text     : Unbounded_String;
      Option_Name     : Unbounded_String;
      Option_Letter   : Character;
      Option_Param    : Unbounded_String;
      Option_Name_LC  : Unbounded_String;
      P1              : Natural;
      Interpreter     : Option_Interpreter;
      Long_Cursor : Long_Option_Maps.Cursor;
      Short_Cursor : Short_Option_Maps.Cursor;      
      
   begin
      
      Short_Options.Clear;
      Long_Options.Clear;
      Normal_Args.Clear;
      Errors.Clear;

      for Arg_Num in 1 .. Raw_Program_Argument_Count.all loop
         Set_Unbounded_String (Arg, Raw_Program_Argument (Arg_Num));
         if not No_More_Options and then Length (Arg) > 0 and then Element (Arg, 1) = '-' then -- some kind of option(s)
            if Length (Arg) = 1 then -- single '-' signals that all following arguments are to be treated as normal
               No_More_Options := True;
            else -- interpret the option(s)
               if Element (Arg, 2) = '-' then -- one long option, begins with "--"
                  Unbounded_Slice (Arg, Option_Text, 3, Length (Arg)); -- get all of the option text
                  P1 := Index (Option_Text, "="); -- does it have an '=' in it?
                  if P1 > 0 then -- if it does, extract option name and option parameter value
                     Unbounded_Slice (Option_Text, Option_Name, 1, P1 - 1); -- option name (to the left of '=')
                     Unbounded_Slice (Option_Text, Option_Param, P1 + 1, Length (Option_Text)); -- option parameter value (to the right of '=')
                  else -- otherwise it is all just the option name
                     Option_Name  := Option_Text;
                     Option_Param := Null_Unbounded_String; -- option parameter valueis is ""
                  end if;
                  Option_Name_LC := Translate (Option_Name, Lower_Case_Map); -- long option names are case-insensitive
                  Long_Cursor := Long_Options.Find (To_String (Option_Name_LC)); -- find the name in the long options map
                  if Long_Cursor = Long_Option_Maps.No_Element then -- not found
                     Errors.Append ("Long option not recognised [" & To_String (Arg) & "]");
                  else -- found, so invoke the option's interpreter on the option name and parameter value
                     Interpreter := Element (Long_Cursor); -- get interpreter function
                     Interpreter (Program_Context, To_String (Option_Name), To_String (Option_Param)); -- invoke it
                  end if;
               else -- possibly multiple short option(s)
                  for i in 2 .. Length (Arg) loop -- for each option letter
                     Option_Letter := Element (Arg, i);
                     Short_Cursor := Short_Options.Find (Option_Letter); -- find it in the short options map
                     if Short_Cursor = Short_Option_Maps.No_Element then -- not found
                        Errors.Append ("Short option not recognised [" & Option_Letter & "]");
                     else -- found, so invoke the option's interpreter on the option letter
                        Interpreter := Element (Short_Cursor); -- get the interpreter function
                        Interpreter (Program_Context, String'(1 => Option_Letter), Param => ""); -- invoke it
                     end if;
                  end loop;
               end if;
            end if;
         else -- normal argument
            Normal_Args.Append (To_String (Arg));
         end if;
      end loop;
      
      Normal_Args_Array := Copy_List_To_Array (Normal_Args);
      Errors_Array := Copy_List_To_Array(Errors);
      
   end;

   function Normal_Argument_Count return Natural is
   begin
      return Normal_Args_Array'Length;
   end;
      
   function Normal_Argument (N : Positive) return String is
   begin
      return Normal_Args_Array (N).all;
   end;

   function Program_Argument_Error_Count return Natural is
   begin
      return Errors_Array'Length;
   end;

   function Program_Arguments_Error (N : Positive) return String is
   begin
      return Errors_Array (N).all;
   end;

   procedure Report_Program_Argument_Errors is
      There_Is_An_Error : Boolean := False;
   begin
      for Message of Errors loop
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, Message);
         There_Is_An_Error := True;
      end loop;
      if There_Is_An_Error then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "Program terminated");
         raise Program_Argument_Error;
      end if;
   end;
   
   procedure Add_Program_Argument_Error (Message : String) is
   begin
      Errors.Append (Message);
   end;
   
end;
   
