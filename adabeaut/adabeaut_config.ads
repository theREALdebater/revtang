with Ada_Configuration;

package adabeaut_Config 
   with Elaborate_Body is

   --/ Limits on line lengths and margins:

   Max_Line_Width:   constant := 1000; -- the longest line we will allow
   Min_Right_Margin: constant := 20; -- the shortest right margin we will allow
   
   subtype Right_Margin_Bounds is Natural range Min_Right_Margin .. Max_Line_Width;
   
   Default_Right_Margin: constant Right_Margin_Bounds := 95;
   
   Right_Margin: Right_Margin_Bounds := Default_Right_Margin;
   
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
   
   Source_File_Names: access Ada_Configuration.String_Array;
   Help_Requested:    Boolean;
   
   --\

end;