with Ada.Text_IO;
with Ada.Exceptions;

package body adabeaut_Config is

   use Ada_Configuration;

   Info:   Help_Information;
   Config: Configuration_Data;
   
   Right_Margin_Config_Name: constant String := "Right_Margin";

begin
   Insert_Option_Integer
      ( Info               => Info,
        Config_Name        => Right_Margin_Config_Name,
        Long_Name          => "margin", 
        Short_Name         => 'm',
        Description        => "Sets the right margin to n", 
        Parameter_Metaname => "n",
        Lower_Bound        => Right_Margin_Bounds'First, 
        Upper_Bound        => Right_Margin_Bounds'Last );

   Insert_Standard_Options( Info );

   Set_Program_Info
      ( Info                 => Info,
        Program_Description  => "Reformats comments neatly in an Ada program and inserts missing fragment identifiers", 
        Argument_Metaname    => "file",
        Argument_Description => "Name or path of Ada source text file to process" );

   Set_Help_Information( Config, Info );
        
   Load_Configuration( Config );
   
   Right_Margin := Get_Integer( Config, Program_Options_Config_Name_Prefix & Right_Margin_Config_Name, Right_Margin );
   
   declare
      Names: constant String_Array := Get_String_Array( Config, Program_Arguments_Config_Name, Null_String_Array ); -- normal arguments only
   begin
      Source_File_Names := new String_Array( 1 .. Names'Length );
      Source_File_Names.all := Names;
   end;
   
   Help_Requested := Ada_Configuration.Help_Requested(Config);

exception
   when E: others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Information (E));
end;
