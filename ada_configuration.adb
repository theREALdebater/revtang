--/ Command_Line_Framework package body:

with Ada.Strings;
--with Ada.Strings.Hash;
--with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Maps; 
--with Ada.Strings.Maps.Constants; 

--with Ada.Containers;
--with Ada.Containers.Hashed_Maps;
--with Ada.Containers.Indefinite_Hashed_Maps;
--with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Text_IO, Ada.Text_IO.Unbounded_IO;
--with Ada.Integer_Text_IO;
with Ada.Command_Line;

use Ada.Strings;
use Ada.Strings.Maps; 
--use Ada.Strings.Maps.Constants; 
use Ada.Text_IO, Ada.Text_IO.Unbounded_IO;
--use Ada.Integer_Text_IO;

package body Ada_Configuration is

   use Config_Maps;

   --/ String arrays and lists:
     
   package String_Lists is new Ada.Containers.Doubly_Linked_Lists(Unbounded_String);

   function To_String_Array (List: String_Lists.List) return String_Array is
   
      Result: String_Array( 1 .. Natural(List.Length) );
      N:      Natural := 0;
   begin
      for S of List loop
         N := N + 1;
         Result(N) := S;
      end loop;
      return Result;
   end;
   
   --\
   --/ Boolean config items:
   
   type Boolean_Config_Item is new Configuration_Item with
      record
         Is_True: Boolean;
      end record;
   
   function Value (Item: Boolean_Config_Item) return Boolean is 
   begin
      return Item.Is_True;
   end;
   
   function Is_True (Item: Boolean_Config_Item) return Boolean renames Value; 

   --\
   --/ Integer config items:
   
   type Integer_Config_Item is new Configuration_Item with
      record
         Value: Integer;
      end record;
   
   function Value (Item: Integer_Config_Item) return Integer is 
   begin
      return Item.Value;
   end;
   
   --\
   --/ String array config items:
   
   type String_Array_On_Heap is access String_Array;
  
   type String_Array_Config_Item is new Configuration_Item with
      record
         Ref: String_Array_On_Heap;
      end record;
   
   function Value (Item: String_Array_Config_Item) return String_Array is 
   begin
      if Item.Ref = null then
         return Null_String_Array;
      else
         return Item.Ref.all;
      end if;
   end;
   
   --\
   --/ Get_Config_Item generic function specification:
   generic
      type Item_Type(<>) is new Configuration_Item with private;
      type Value_Type(<>) is private;
      Default_Default: in Value_Type;
      with function Value (Item: Item_Type) return Value_Type is <>;
      
   function Get_Config_Item (Config:  in Configuration_Data;
                             Name:    in String;
                             Default: in Value_Type := Default_Default) return Value_Type;

   --\
   --/ Get_Config_Item generic function implementation:
   
   function Get_Config_Item (Config:  in Configuration_Data;
                             Name:    in String;
                             Default: in Value_Type := Default_Default) return Value_Type is
                         
      Item: constant Config_Maps.Cursor := Find( Config.Items, Name );
   begin
      if Item = Config_Maps.No_Element 
      or else not (Element(Item) in Item_Type'Class) then
         return Default;
      else
         return Value(Item_Type(Element(Item)));
      end if;
   end;
   
   --\
   --/ Get the various types of configuration item:
   
   function Get_Boolean_Instantiation      is new Get_Config_Item( Boolean_Config_Item,      Boolean,      False );
   function Get_Integer_Instantiation      is new Get_Config_Item( Integer_Config_Item,      Integer,      0 );
   function Get_String_Array_Instantiation is new Get_Config_Item( String_Array_Config_Item, String_Array, Null_String_Array );
   
   function Get_Boolean (Config:  in Configuration_Data;
                         Name:    in String;
                         Default: in Boolean := False) return Boolean is
   begin
      return Get_Boolean_Instantiation( Config, Name, Default );
   end;
   
   function Get_Integer (Config:  in Configuration_Data;
                         Name:    in String;
                         Default: in Integer := 0) return Integer is
   begin
      return Get_Integer_Instantiation( Config, Name, Default );
   end;
   
   function Get_String_Array (Config:  in Configuration_Data;
                              Name:    in String;
                              Default: in String_Array := Null_String_Array) return String_Array is
   begin
      return Get_String_Array_Instantiation( Config, Name, Default );
   end;
   
   --\
   --/ Was help requested?
   
   function Help_Requested (Config: Configuration_Data) return Boolean is
   begin
      return Get_Boolean( Config, Help_Requested_Config_Name );
   end;
   
   --\
   --/ Option names:
   
   Upper_Case_Letters: constant Character_Set := To_Set( Character_Range'('A', 'Z') );
   Lower_Case_Letters: constant Character_Set := To_Set( Character_Range'('a', 'z') );
   
   Legal_Name_Chars: constant Character_Set := Upper_Case_Letters or Lower_Case_Letters or To_Set('_');

   function Is_Legal_Short_Option_Name (Short_Name: in Short_Option_Character) return Boolean is
   begin
      return Short_Name /= Null_Short_Option;
   end;
   
   function All_In (Elements: in String; Set: in Character_Set) return Boolean is
   begin
      for C of Elements loop
         if not Is_In(C, Set) then return False; end if;
      end loop;
      return True;
   end;
      
   function Is_Legal_Long_Option_Name (Option_Name: in String) return Boolean is
   begin
      return Option_Name'Length > 0 and then All_In( Option_Name, Legal_Name_Chars );
   end;
      
   --\
   --/ Option descriptor operations:

   function Short_Options (Info: Help_Information) return Option_Lists.List is
   
      Result: Option_Lists.List;
   begin
      for Desc of Info.Options loop
         if Desc.Short_Name /= Null_Short_Option then
            Result.Append(Desc);
         end if;
      end loop;
      return Result;
   end;
      
   function Long_Options (Info: Help_Information) return Option_Lists.List is
   
      Result: Option_Lists.List;
   begin
      for Desc of Info.Options loop
         if Desc.Long_Name /= Null_Long_Option then
            Result.Append(Desc);
         end if;
      end loop;
      return Result;
   end;
   
   procedure Search_Long_Options (Info:       in  Help_Information;
                                  Long_Name:  in  String; 
                                  Found:      out Boolean; 
                                  Descriptor: out Option_Descriptor) is
   begin
      for Desc of Info.Options loop
         if Equal_Case_Insensitive(Long_Name, To_String(Desc.Long_Name)) then
            Found := True;
            Descriptor := Desc;
            return;
         end if;
      end loop;
      Found := False;
   end;
   
   procedure Search_Short_Options (Info:       in  Help_Information;
                                   Short_Name: in  Short_Option_Character; 
                                   Found:      out Boolean; 
                                   Descriptor: out Option_Descriptor) is
   begin
      for Desc of Info.Options loop
         if Short_Name = Desc.Short_Name then
            Found := True;
            Descriptor := Desc;
            return;
         end if;
      end loop;
      Found := False;
   end;
   
   procedure Insert_Option_Boolean (Info:               in out Help_Information;
                                    Config_Name:        in     String;
                                    Long_Name:          in     String := Null_Long_Option;
                                    Short_Name:         in     Short_Option_Character := Null_Short_Option;
                                    Description:        in     String := "") is

      Desc:     Option_Descriptor;
      Found:    Boolean;
      Is_Long:  constant Boolean := Long_Name  /= Null_Long_Option;
      Is_Short: constant Boolean := Short_Name /= Null_Short_Option;
   begin
      if not Is_Long and not Is_Short then
         raise Program_Error with "Must supply either long name or short name (or both)";
      end if;
      if Is_Long and not Is_Legal_Long_Option_Name(Long_Name) then
         raise Program_Error with "Illegal long option name";
      end if;
      if Is_Short and not Is_Legal_Short_Option_Name(Short_Name) then
         raise Program_Error with "Illegal short option character";
      end if;
      if Is_Long then
         Search_Long_Options( Info, Long_Name, Found, Desc );         
         if Found then
            raise Program_Error with "Long name '" & Long_Name & "' already exists";
         end if;
      end if;
      if Is_Short then
         Search_Short_Options( Info, Short_Name, Found, Desc );         
         if Found then
            raise Program_Error with "Short name '" & Short_Name & "' already exists";
         end if;
      end if;
      Info.Options.Append
         ( Option_Descriptor'(Long_Name          => To_Unbounded_String(Long_Name), 
                              Short_Name         => Short_Name, 
                              Description        => To_Unbounded_String(Description),
                              Parameter_Metaname => Null_Unbounded_String,
                              Parameter_Type     => Boolean_OPT,
                              Config_Name        => To_Unbounded_String(Config_Name),
                              Lower_Bound | Upper_Bound => 0) );
   end;
      
   procedure Insert_Option_Integer (Info:               in out Help_Information;
                                    Config_Name:        in     String;
                                    Long_Name:          in     String := Null_Long_Option;
                                    Short_Name:         in     Short_Option_Character := Null_Short_Option;
                                    Description:        in     String := "";
                                    Parameter_Metaname: in     String := "";
                                    Lower_Bound:        in     Integer := Integer'First;
                                    Upper_Bound:        in     Integer := Integer'Last) is

      Desc:     Option_Descriptor;
      Found:    Boolean;
      Is_Long:  constant Boolean := Long_Name  /= Null_Long_Option;
      Is_Short: constant Boolean := Short_Name /= Null_Short_Option;
   begin
      if not Is_Long and not Is_Short then
         raise Program_Error with "Must supply either long name or short name (or both)";
      end if;
      if Is_Long and not Is_Legal_Long_Option_Name(Long_Name) then
         raise Program_Error with "Illegal long option name";
      end if;
      if Is_Short and not Is_Legal_Short_Option_Name(Short_Name) then
         raise Program_Error with "Illegal short option character";
      end if;
      if not Is_Long and Parameter_Metaname /= "" then
         raise Program_Error with "'" & Short_Name & "' is a short option only, so it cannot have a parameter";
      end if;
      if Is_Long then
         Search_Long_Options( Info, Long_Name, Found, Desc );         
         if Found then
            raise Program_Error with "Long name '" & Long_Name & "' already exists";
         end if;
      end if;
      if Is_Short then
         Search_Short_Options( Info, Short_Name, Found, Desc );         
         if Found then
            raise Program_Error with "Short name '" & Short_Name & "' already exists";
         end if;
      end if;
      Info.Options.Append
         ( Option_Descriptor'(Long_Name          => To_Unbounded_String(Long_Name), 
                              Short_Name         => Short_Name, 
                              Description        => To_Unbounded_String(Description),
                              Parameter_Metaname => To_Unbounded_String(Parameter_Metaname),
                              Parameter_Type     => Integer_OPT,
                              Config_Name        => To_Unbounded_String(Config_Name),
                              Lower_Bound        => Lower_Bound,
                              Upper_Bound        => Upper_Bound) );
   end;
      
   --\
   --/ Program help information:
   
   procedure Set_Program_Info (Info:                 in out Help_Information;
                               Program_Description:  in     String;
                               Argument_Metaname:    in     String;
                               Argument_Description: in     String;
                               Program_Name:         in String := "") is
   begin
      Set_Unbounded_String( Info.Program_Description,  Program_Description );
      Set_Unbounded_String( Info.Argument_Metaname,    Argument_Metaname );
      Set_Unbounded_String( Info.Argument_Description, Argument_Description );
      if Program_Name = "" then
         Set_Unbounded_String( Info.Program_Name, Ada.Command_Line.Command_Name );
      else
         Set_Unbounded_String( Info.Program_Name, Program_Name );
      end if;
   end;
   
   --\
   --/ Standard options:
   
   procedure Insert_Standard_Options (Info: in out Help_Information) is

      Help_Desc: constant String := "outputs program usage information (does nothing else)";
   begin
      Insert_Option_Boolean( Info, Help_Requested_Config_Name, "help", 'h', Help_Desc );
      Insert_Option_Boolean( Info, Help_Requested_Config_Name, "info", '?', Help_Desc );
   end;
   
   --\
   -- / Configuration data:
   
   procedure Include_Boolean_Config_Item (Config: in out Configuration_Data;
                                          Name:   in    String;
                                          Value:  in    Boolean) is
   begin
      Config.Items.Include( Name, Boolean_Config_Item'( To_Unbounded_String(Name), Value ) );
   end;

   procedure Include_Integer_Config_Item (Config: in out Configuration_Data;
                                          Name:   in    String;
                                          Value:  in    Integer) is
   begin
      Config.Items.Include( Name, Integer_Config_Item'( To_Unbounded_String(Name), Value ) );
   end;   
   
   procedure Include_String_Array_Config_Item (Config: in out Configuration_Data;
                                               Name:   in    String;
                                               Value:  in    String_Array) is
   begin
      Config.Items.Include( Name, String_Array_Config_Item'( To_Unbounded_String(Name), new String_Array'(Value) ) );
   end;   
   
   --\
   --/ Interpret the program arguments (implementation):
   
   procedure Configure_Option (Config:      in out Configuration_Data;
                               Option_Desc: in     Option_Descriptor;
                               Param_Value: in     String := "") is
   begin
      case Option_Desc.Parameter_Type is
         when Boolean_OPT =>
            -- TODO: Parameter could be 'true' or 'false'?
            Include_Boolean_Config_Item( Config, To_String(Option_Desc.Config_Name), True );
         when Integer_OPT =>
            -- TODO: Check the parameter first? YES!!!
            Include_Integer_Config_Item( Config, To_String(Option_Desc.Config_Name), Integer'Value(Param_Value) );
         when String_Array_OPT =>
            raise Program_Error with "Not supported";
            -- TODO: Support it!
      end case;   
   end;
   
   procedure Configure_Normal_Arguments (Config: in out Configuration_Data;
                                         Args:   in     String_Lists.List) is
   begin
      Include_String_Array_Config_Item( Config, Program_Arguments_Config_Name, To_String_Array(Args) );
   end;

   procedure Interpret_Program_Arguments (Config:          in out Configuration_Data;
                                          Raw_Args:        in     String_Lists.List) is
   
      No_More_Options: Boolean := False;
      Option_Text:     Unbounded_String;
      Option_Name:     Unbounded_String;
      Option_Letter:   Character;
      Option_Param:    Unbounded_String;
      Option_Found:    Boolean;
      Option_Desc:     Option_Descriptor;
      P1:              Natural;
      Normal_Args:     String_Lists.List;
   begin
      --Normal_Args.Clear;
      for Arg of Raw_Args loop
         if not No_More_Options and then Length(Arg) > 0 and then Element(Arg,1) = '-' then -- some kind of option(s)
            if Length(Arg) = 1 then -- single '-' signals that all following arguments are to be treated as normal
               No_More_Options := True;
            else -- interpret the option(s)
               if Element(Arg,2) = '-' then -- one long option, begins with "--"
                  Set_Unbounded_String( Option_Text, Slice(Arg,3,Length(Arg)) ); -- get all of the option text
                  P1 := Index( Option_Text, "=" ); -- does it have an '=' in it?
                  if P1 > 0 then -- if it does, extract option name and option parameter value
                     Unbounded_Slice( Option_Text, Option_Name, 1, P1 - 1 ); -- option name (to the left of '=')
                     Unbounded_Slice( Option_Text, Option_Param, P1 + 1, Length (Option_Text) ); -- option parameter value (to the right of '=')
                  else -- otherwise it is all just the option name
                     Option_Name  := Option_Text;
                     Option_Param := Null_Unbounded_String; -- option parameter value is ""
                  end if;
                  --Report_Debug( "Option_Name = [" & To_String(Option_Name) & "]" ); -- $DEBUG
                  Search_Long_Options( Config.Help, To_String(Option_Name), Option_Found, Option_Desc );
                  if not Option_Found then -- not found
                     raise Program_Error with "Long option not recognised [" & To_String(Arg) & "]";
                  else -- found
                     Configure_Option( Config, Option_Desc, To_String(Option_Param) );
                  end if;
               else -- possibly multiple short option(s)
                  for i in 2..Length(Arg) loop -- for each option letter
                     Option_Letter := Element(Arg,i);
                     Search_Short_Options( Config.Help, Option_Letter, Option_Found, Option_Desc ); -- find it in the short options list
                     if not Option_Found then
                        raise Program_Error with "Short option not recognised [" & Option_Letter & "]";
                     else -- found
                        Configure_Option( Config, Option_Desc );
                     end if;
                  end loop;
               end if;
            end if;
         else -- normal argument
            Normal_Args.Append(Arg);
         end if;
      end loop;
      Configure_Normal_Arguments( Config, Normal_Args );
   end;
   
   --\
   --/ Program help message:
   
   procedure Report_Program_Help (Info: in Help_Information) is
   
      Tab_Stop_1: constant Ada.Text_IO.Positive_Count := 5;
      Tab_Stop_2: constant Ada.Text_IO.Positive_Count := 20;
   begin
      Put(Info.Program_Name);
      --if Length(Short_Options) > 0 then
         for Opt of Short_Options(Info) loop
            Put(" [-");
            Put(Opt.Short_Name);
            Put("]");
         end loop;
      --end if;
      --if Length(Long_Options) > 0 then
         for Opt of Long_Options(Info) loop
            Put(" [--");
            Put(Opt.Long_Name);
            if Opt.Parameter_Metaname /= "" then
               Put("=");
               Put(Opt.Parameter_Metaname);
            end if;
            Put("]");
         end loop;
      --end if;
      if Info.Argument_Metaname /= "" then
         Put(" [");
         Put(Info.Argument_Metaname);
         Put("]...");
      end if;
      if Info.Program_Description /= Null_Unbounded_String then
         Set_Col(Tab_Stop_2);
         Put(Info.Program_Description);
      end if;
      New_Line;
      for Opt of Short_Options(Info) loop
         Set_Col(Tab_Stop_1);
         Put("-");
         Put(Opt.Short_Name);
         Set_Col(Tab_Stop_2);
         Put(Opt.Description);
         New_Line;
      end loop;
      for Opt of Long_Options(Info) loop
         Set_Col(Tab_Stop_1);
         Put("--");
         Put(Opt.Long_Name);
         if Opt.Parameter_Metaname /= "" then
            Put("=");
            Put(Opt.Parameter_Metaname);
         end if;
         Set_Col(Tab_Stop_2);
         Put(Opt.Description);
         New_Line;
      end loop;
      if Info.Argument_Metaname /= Null_Unbounded_String 
      and Info.Argument_Description /= Null_Unbounded_String then
         Set_Col(Tab_Stop_1);
         Put(Info.Argument_Metaname);
         Set_Col(Tab_Stop_2);
         Put(Info.Argument_Description);
         New_Line;
      end if;
   end;

   --\
   --/ Initialise the raw argument list:   
   
   function Default_Raw_Arguments return String_Lists.List is
   
      Result: String_Lists.List;
   begin
      for N in Positive range 1 .. Ada.Command_Line.Argument_Count loop
         Result.Append( To_Unbounded_String(Ada.Command_Line.Argument(N)) );
      end loop;
      return Result;
   end;

   --\   
   --/ Process the program's arguments (implementation):
   
   procedure Set_Help_Information (Config: in out Configuration_Data;
                                   Info:   in     Help_Information) is
   begin
      Config.Help := Info;
   end;
   
   procedure Load_Configuration (Config: in out Configuration_Data) is
   begin
      Interpret_Program_Arguments( Config, Default_Raw_Arguments );
      if Help_Requested(Config) then
         Report_Program_Help(Config.Help);
      end if;
   end;
   
   --\
end;

--\

