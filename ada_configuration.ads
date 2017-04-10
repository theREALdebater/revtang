--/ Command_Line_Framework package specification:

--| This package provides facilities to enable a program (main) procedure to interpret options
--| supplied as specially formatted arguments on the command line.

with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;

use Ada.Strings.Unbounded;

package Ada_Configuration is

   --/ Option descriptors:

   type Help_Information is private;
   
   subtype Short_Option_Character is Character range ' '..'z';
   
   Null_Short_Option: constant Short_Option_Character := ' '; -- indicates no short option
   Null_Long_Option:  constant String := ""; -- indicates no long option

   procedure Insert_Option_Boolean (Info:               in out Help_Information;
                                    Config_Name:        in     String;
                                    Long_Name:          in     String := Null_Long_Option;
                                    Short_Name:         in     Short_Option_Character := Null_Short_Option;
                                    Description:        in     String := "");

   procedure Insert_Option_Integer (Info:               in out Help_Information;
                                    Config_Name:        in     String;
                                    Long_Name:          in     String := Null_Long_Option;
                                    Short_Name:         in     Short_Option_Character := Null_Short_Option;
                                    Description:        in     String := "";
                                    Parameter_Metaname: in     String := "";
                                    Lower_Bound:        in     Integer := Integer'First;
                                    Upper_Bound:        in     Integer := Integer'Last);

   procedure Insert_Standard_Options (Info: in out Help_Information);
                                 
   --\
   --/ Program specifications:
   
   procedure Set_Program_Info (Info:                 in out Help_Information;
                               Program_Description:  in String;  -- e.g. "Bloogles the waffleparnes accurately"
                               Argument_Metaname:    in String;  -- e.g. "file"
                               Argument_Description: in String;
                               Program_Name:         in String := ""); -- e.g. "waffleparne descriptor file to be bloogled"
   
   --| These variables should be set as appropriate before calling `Load_Configuration`, so that the standard
   --| help options function fully. The program's name is set to a default, and the others are optional. These variables are 
   --| irrelevant if the standard help options are not used. 
      
   --\
   --/ String lists:
   
   type String_Array is array (Positive range <>) of Unbounded_String;
   
   Null_String_Array: constant String_Array := String_Array'( 1..0 => Null_Unbounded_String ); -- strange Ada formulation for an empty array
   
   --\
   --/ :
   
   type Configuration_Data is limited private;
   
   procedure Set_Help_Information (Config: in out Configuration_Data;
                                   Info:   in     Help_Information);
   
   procedure Load_Configuration (Config: in out Configuration_Data);

   --| ... interpret or process the arguments (and options) passed into the program upon its invocation ...
   
   --\
   --/ Fetch configuration items:
   
   function Get_Boolean (Config:  in Configuration_Data;
                         Name:    in String;
                         Default: in Boolean := False) return Boolean;
   
   function Get_Integer (Config:  in Configuration_Data;
                         Name:    in String;
                         Default: in Integer := 0) return Integer;
   
   function Get_String_Array (Config:  in Configuration_Data;
                              Name:    in String;
                              Default: in String_Array := Null_String_Array) return String_Array;
   
   function Help_Requested (Config: Configuration_Data) return Boolean;   
   
   --\
   --/ Configuration names:
   
   Help_Requested_Config_Name:         constant String := "Help";
   Program_Arguments_Config_Name:      constant String := "Command_Line:Arguments";
   Program_Options_Config_Name_Prefix: constant String := "Command_Line:Options:";
   
   --\
   
private

   --/ Option descriptors:
   
   type Option_Parameter_Type is (Boolean_OPT, Integer_OPT, String_Array_OPT); -- there will be more
   
   type Option_Descriptor is
      record
         Long_Name:          Unbounded_String;
         Short_Name:         Short_Option_Character;
         Description:        Unbounded_String;
         Parameter_Metaname: Unbounded_String;
         Parameter_Type:     Option_Parameter_Type;
         Config_Name:        Unbounded_String;
         Lower_Bound, Upper_Bound: Integer; -- apply to Integer_OPT only
      end record;
   
   package Option_Lists is new Ada.Containers.Doubly_Linked_Lists(Option_Descriptor);
      
   type Help_Information is 
      record
         Program_Name:         Unbounded_String; -- g.e. "bloogify"
         Program_Description:  Unbounded_String; -- e.g. "Bloogles the waffleparnes and reports on their frugality"
         Argument_Metaname:    Unbounded_String; -- e.g. "file"
         Argument_Description: Unbounded_String; -- e.g. "file path of waffleparne to be processed"
         Options:              Option_Lists.List;
      end record;

   --\
   --/ Configuration data:
   
   type Configuration_Item is abstract tagged 
      record
         Name: Unbounded_String;
      end record;
   
   package Config_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      ( Key_Type        => String,
        Element_Type    => Configuration_Item'Class,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive );
   
   type Configuration_Data is limited
      record
         Items: Config_Maps.Map;
         Help:  Help_Information;
         -- there will be more
      end record;
   
   --\
      
end;

--\