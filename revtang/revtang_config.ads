with Ada_Configuration;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package revtang_Config is

   --/ Check-only flag:

   Nongenerate_Mode : Boolean := False;

   --\
   --/ Verbosity mode:
   
   type Program_Verbosity is (Low_Verbosity, Normal_Verbosity, High_Verbosity, Debugging_Verbosity);
   
   Verbosity: Program_Verbosity := Debugging_Verbosity; -- will be Normal_Verbosity when we are out of alpha testing
   
   --\
   --/ Source file types:
   
   type Source_File_Type is (Ada_Family, C_Family); -- currently we are only supporting Ada_Family
   
   --| The Ada family of languages includes essentially all those languages which have line comments that begin with 
   --| a `--`, so it includes Ada and SQL plus a few others. Currently we are only supporting `Ada_Family`.
   
   type File_Pattern_Set is array (Positive range <>) of Unbounded_String;
   
   function Source_File_Patterns (File_Type: in Source_File_Type) return File_Pattern_Set is
   begin
      case File_Type is
         when Ada_Family =>
            return new File_Pattern_Set'("*.ada", "*.ads", "*.adb");
         when others =>
            raise Program_Error with "Other file types not supported yet";
      end case;
   end;
   
   --\
   --/ Standard prefixes for Ada/RevTang:

   Prose_Comment_Prefix  : constant Wide_String := "--|";
   Fragment_Start_Prefix : constant Wide_String := "--/";
   Fragment_End_Prefix   : constant Wide_String := "--\";

   Fragment_Tag_Prefix : constant Wide_String := "#[";
   Fragment_Tag_Suffix : constant Wide_String := "]";
   Fragment_Id_Prefix  : constant Wide_String := "frag-";

   --\
   --/ Ada source text files to be processed:

   Source_File_Root_Names : access Ada_Configuration.String_Array;
   Help_Requested         : Boolean;

   --\
   --/ Eceptions specific to RevTang:
   
   Fragment_Number_Error: exception;
   
   --\

end;
