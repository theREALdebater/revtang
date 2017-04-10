--/ Command_Line_Framework package specification:

--| This package provides facilities to enable a program (main) procedure to interpret options
--| supplied as specially formatted arguments on the command line.

with Ada.Command_Line;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package Command_Line_Framework is

   --/ Error reporting:
   
   procedure Report_Error (Message:     String  := ""; 
                           Is_Terminal: Boolean := False);

   --| To report errors (and warnings) to the user, the application should call the `Report_Error` procedure, instead of 
   --| directly using `Ada.Text_IO` and related packages, 
   --| because this will allow alternative implementations to be easily put in place, and the errors can be caught
   --| by a test harness (see below).   
   
   --\
   --/ Program specifications:
   
   Program_Name:                 Unbounded_String := To_Unbounded_String(Ada.Command_Line.Program_Name);
   Program_Description:          Unbounded_String := Null_Unbounded_String; -- e.g. "Bloogles the waffleparnes accurately"
   Program_Argument_Moniker:     Unbounded_String := Null_Unbounded_String; -- e.g. "file"
   Program_Argument_Description: Unbounded_String := Null_Unbounded_String; -- e.g. "waffleparne descriptor file to be bloogled"
   
   --| These variables should be set as appropriate before calling `Interpret_Program_Arguments`, so that the standard
   --| help options function fully. `Program_Name` is set to a default, and the others are optional. These variables are 
   --| irrelevant if the standard help options are not used. 
      
   --\
   --/ Option descriptors:

   type Option_Descriptor_Set is private;
   
   type Option_Interpreter is access procedure (Option_Name:      in  String; -- long option if name more than length 1
                                                Parameter_Value:  in  String;
                                                Is_Terminal:      out Boolean);

   type Argument_Processor is access procedure (Arg_Value:   in  String;
                                                Is_Terminal: out Boolean);

   --| An ~option interpreter~ etc...
   
   Option_Descriptors: access Option_Descriptor_Set := null;
   
   Interpret_Option: Option_Interpreter := null;   
   Process_Argument: Argument_Processor := null;
   
   procedure Insert_Option (Option_Name:       in String; -- long option if name more than length 1
                            Description:       in String := "";
                            Parameter_Moniker: in String := "");

   procedure Insert_Standard_Options;
                                 
   --\
   --/ Argument lists:
   
   package Argument_Lists is new Ada.Containers.Doubly_Linked_Lists(Unbounded_String);
      
   subtype Argument_List is Argument_Lists.List;
   
   --\
   --/ Interpret or process the arguments (and options) passed into the program upon its invocation:
   
   procedure Process_Program_Arguments;

   procedure Interpret_Program_Arguments (Normal_Args:        out Argument_List;
                                          Is_Terminal: out Boolean);
   
   --\
   --/ Changing the raw arguments:

   --| By default the arguments passed on the command line are used as the raw arguments to interpret (for options),
   --| but these can be substituted by a different sequence of arguments by setting the `Raw_Arguments` list
   --| (before calling `Interpret_Program_Arguments` or `Process_Program_Arguments`). This facility is most likely to be useful for testing. 

   Raw_Arguments: Argument_List;
   
   --\
   --/ Automated testing and debugging facilities:
   
   type Harness_Event is abstract tagged null record;

   type Event_Handler is access procedure (Event : in Harness_Event);

   Report_Event: Event_Handler := null;
   
   --| A harness of any kind (e.g. a test harness) should set `Report_Event` to an event handler procedure, so that instead of 
   --| actually performing console I/O, the framework reports interactions to the harness in the form of events.
   
   type Error_Event is new Harness_Event with
      record
         Message:  Unbounded_String := Null_Unbounded_String;
         Terminal: Boolean := False;
         Internal: Boolean := False;
      end record;
   
   type Help_Event is new Harness_Event with null record;
   
   --\

private

   --/ Option descriptors:

   type Option_Descriptor is
      record
         Option_Name:       Unbounded_String; -- option is long if name length > 1
         Description:       Unbounded_String;
         Parameter_Moniker: Unbounded_String;
      end record;
   
   package Option_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists(Option_Descriptor);
      
   type Option_Descriptor_Set is 
      record
         Options: Option_Lists.List;
      end record;

   --\
      
end;

--\