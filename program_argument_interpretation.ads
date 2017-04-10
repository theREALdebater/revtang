--/ Program_Argument_Interpretation package specification:

--| This package provides facilities to enable a program (main) procedure to interpret options
--| supplied as specially formatted arguments on the command line.

with Ada.Command_Line;

generic
   type Context_Type is private;

package Program_Argument_Interpretation is

   --/ Program context:

   Program_Context : Context_Type;

   --| The type `Context_Type` (a generic parameter) will contain the state (information)
   --| that results from interpreting any valid options supplied. The global variable
   --| `Program_Context` contains the context for the processing of program arguments.

   --\
   --/ Options interpreters:

   type Option_Interpreter is access procedure (Context : in out Context_Type;
                                                Name    : in     String;
                                                Param   : in     String);

   --| An ~option interpreter~

   procedure Register_Short_Option (Letter      : Character;
                                    Interpreter : Option_Interpreter);

   procedure Register_Long_Option (Name        : String;
                                   Interpreter : Option_Interpreter);

   --\
   --/ Interpret the raw arguments:

   procedure Interpret_Program_Arguments;

   procedure Add_Program_Argument_Error (Message : String);

   --\
   --/ Retrieve the normal arguments and any errors:

   function Normal_Argument_Count return Natural;

   function Normal_Argument (N : Positive) return String;

   function Program_Argument_Error_Count return Natural;

   function Program_Arguments_Error (N : Positive) return String;

   procedure Report_Program_Argument_Errors;

   Program_Argument_Error : exception;

   --\
   --/ Obtaining the raw arguments:

   --| The functions `Raw_Program_Argument_Count` and `Raw_Program_Argument` are used in the body
   --| of this package to obtain the raw program argument values (before they have been processed to
   --| extract options). These functions are by default set to the equivalent functions provided by the
   --| (Ada standard) `Ada.Command_Line` functions. They could be overridden for the purposes of testing,
   --| for example.

   type String_Function is access function return String;

   type String_Array_Count_Function is access function return Natural;

   type String_Array_Element_Function is access function (N : Positive) return String;

   Raw_Program_Argument_Count : String_Array_Count_Function := Ada.Command_Line.Argument_Count'Access;

   Raw_Program_Argument   : String_Array_Element_Function := Ada.Command_Line.Argument'Access;

   Effective_Program_Name : String_Function := Ada.Command_Line.Command_Name'Access;

   --\

end;

--\

