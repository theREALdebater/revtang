pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2015 (20150428-49)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_adabeaut" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#54c5cdf8#;
   pragma Export (C, u00001, "adabeautB");
   u00002 : constant Version_32 := 16#fbff4c67#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#f72f352b#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00005, "ada__charactersS");
   u00006 : constant Version_32 := 16#e753e265#;
   pragma Export (C, u00006, "ada__characters__conversionsB");
   u00007 : constant Version_32 := 16#761d31b0#;
   pragma Export (C, u00007, "ada__characters__conversionsS");
   u00008 : constant Version_32 := 16#b19b6653#;
   pragma Export (C, u00008, "system__secondary_stackB");
   u00009 : constant Version_32 := 16#5faf4353#;
   pragma Export (C, u00009, "system__secondary_stackS");
   u00010 : constant Version_32 := 16#f4ce8c3a#;
   pragma Export (C, u00010, "systemS");
   u00011 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00011, "system__parametersB");
   u00012 : constant Version_32 := 16#8ae48145#;
   pragma Export (C, u00012, "system__parametersS");
   u00013 : constant Version_32 := 16#a207fefe#;
   pragma Export (C, u00013, "system__soft_linksB");
   u00014 : constant Version_32 := 16#af945ded#;
   pragma Export (C, u00014, "system__soft_linksS");
   u00015 : constant Version_32 := 16#b612ca65#;
   pragma Export (C, u00015, "ada__exceptionsB");
   u00016 : constant Version_32 := 16#1d190453#;
   pragma Export (C, u00016, "ada__exceptionsS");
   u00017 : constant Version_32 := 16#a46739c0#;
   pragma Export (C, u00017, "ada__exceptions__last_chance_handlerB");
   u00018 : constant Version_32 := 16#3aac8c92#;
   pragma Export (C, u00018, "ada__exceptions__last_chance_handlerS");
   u00019 : constant Version_32 := 16#393398c1#;
   pragma Export (C, u00019, "system__exception_tableB");
   u00020 : constant Version_32 := 16#5ad7ea2f#;
   pragma Export (C, u00020, "system__exception_tableS");
   u00021 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00021, "system__exceptionsB");
   u00022 : constant Version_32 := 16#9cade1cc#;
   pragma Export (C, u00022, "system__exceptionsS");
   u00023 : constant Version_32 := 16#37d758f1#;
   pragma Export (C, u00023, "system__exceptions__machineS");
   u00024 : constant Version_32 := 16#b895431d#;
   pragma Export (C, u00024, "system__exceptions_debugB");
   u00025 : constant Version_32 := 16#472c9584#;
   pragma Export (C, u00025, "system__exceptions_debugS");
   u00026 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00026, "system__img_intB");
   u00027 : constant Version_32 := 16#f6156cf8#;
   pragma Export (C, u00027, "system__img_intS");
   u00028 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00028, "system__storage_elementsB");
   u00029 : constant Version_32 := 16#d90dc63e#;
   pragma Export (C, u00029, "system__storage_elementsS");
   u00030 : constant Version_32 := 16#b98c3e16#;
   pragma Export (C, u00030, "system__tracebackB");
   u00031 : constant Version_32 := 16#6af355e1#;
   pragma Export (C, u00031, "system__tracebackS");
   u00032 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00032, "system__traceback_entriesB");
   u00033 : constant Version_32 := 16#f4957a4a#;
   pragma Export (C, u00033, "system__traceback_entriesS");
   u00034 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00034, "system__wch_conB");
   u00035 : constant Version_32 := 16#efb3aee8#;
   pragma Export (C, u00035, "system__wch_conS");
   u00036 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00036, "system__wch_stwB");
   u00037 : constant Version_32 := 16#c2a282e9#;
   pragma Export (C, u00037, "system__wch_stwS");
   u00038 : constant Version_32 := 16#92b797cb#;
   pragma Export (C, u00038, "system__wch_cnvB");
   u00039 : constant Version_32 := 16#e004141b#;
   pragma Export (C, u00039, "system__wch_cnvS");
   u00040 : constant Version_32 := 16#6033a23f#;
   pragma Export (C, u00040, "interfacesS");
   u00041 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00041, "system__wch_jisB");
   u00042 : constant Version_32 := 16#60740d3a#;
   pragma Export (C, u00042, "system__wch_jisS");
   u00043 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00043, "system__stack_checkingB");
   u00044 : constant Version_32 := 16#7a71e7d2#;
   pragma Export (C, u00044, "system__stack_checkingS");
   u00045 : constant Version_32 := 16#6ea605be#;
   pragma Export (C, u00045, "ada__directoriesB");
   u00046 : constant Version_32 := 16#c9f0eb84#;
   pragma Export (C, u00046, "ada__directoriesS");
   u00047 : constant Version_32 := 16#649a98f6#;
   pragma Export (C, u00047, "ada__calendarB");
   u00048 : constant Version_32 := 16#e67a5d0a#;
   pragma Export (C, u00048, "ada__calendarS");
   u00049 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00049, "interfaces__cB");
   u00050 : constant Version_32 := 16#4a38bedb#;
   pragma Export (C, u00050, "interfaces__cS");
   u00051 : constant Version_32 := 16#f4bb3578#;
   pragma Export (C, u00051, "system__os_primitivesB");
   u00052 : constant Version_32 := 16#441f0013#;
   pragma Export (C, u00052, "system__os_primitivesS");
   u00053 : constant Version_32 := 16#0881bbf8#;
   pragma Export (C, u00053, "system__task_lockB");
   u00054 : constant Version_32 := 16#9544bb54#;
   pragma Export (C, u00054, "system__task_lockS");
   u00055 : constant Version_32 := 16#1716ff24#;
   pragma Export (C, u00055, "system__win32S");
   u00056 : constant Version_32 := 16#1a9147da#;
   pragma Export (C, u00056, "system__win32__extS");
   u00057 : constant Version_32 := 16#7bf85949#;
   pragma Export (C, u00057, "ada__calendar__formattingB");
   u00058 : constant Version_32 := 16#937437b5#;
   pragma Export (C, u00058, "ada__calendar__formattingS");
   u00059 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00059, "ada__calendar__time_zonesB");
   u00060 : constant Version_32 := 16#991bad49#;
   pragma Export (C, u00060, "ada__calendar__time_zonesS");
   u00061 : constant Version_32 := 16#7ebd8839#;
   pragma Export (C, u00061, "system__val_intB");
   u00062 : constant Version_32 := 16#bc6ba605#;
   pragma Export (C, u00062, "system__val_intS");
   u00063 : constant Version_32 := 16#699628fa#;
   pragma Export (C, u00063, "system__unsigned_typesS");
   u00064 : constant Version_32 := 16#b44f9ae7#;
   pragma Export (C, u00064, "system__val_unsB");
   u00065 : constant Version_32 := 16#793ec5c1#;
   pragma Export (C, u00065, "system__val_unsS");
   u00066 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00066, "system__val_utilB");
   u00067 : constant Version_32 := 16#586e3ac4#;
   pragma Export (C, u00067, "system__val_utilS");
   u00068 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00068, "system__case_utilB");
   u00069 : constant Version_32 := 16#d0c7e5ed#;
   pragma Export (C, u00069, "system__case_utilS");
   u00070 : constant Version_32 := 16#faa9a7b2#;
   pragma Export (C, u00070, "system__val_realB");
   u00071 : constant Version_32 := 16#0ae7fb2b#;
   pragma Export (C, u00071, "system__val_realS");
   u00072 : constant Version_32 := 16#6c05c057#;
   pragma Export (C, u00072, "system__exn_llfB");
   u00073 : constant Version_32 := 16#48b037e6#;
   pragma Export (C, u00073, "system__exn_llfS");
   u00074 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00074, "system__float_controlB");
   u00075 : constant Version_32 := 16#1432cf06#;
   pragma Export (C, u00075, "system__float_controlS");
   u00076 : constant Version_32 := 16#a4beea4d#;
   pragma Export (C, u00076, "system__powten_tableS");
   u00077 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00077, "ada__characters__handlingB");
   u00078 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00078, "ada__characters__handlingS");
   u00079 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00079, "ada__characters__latin_1S");
   u00080 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00080, "ada__stringsS");
   u00081 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00081, "ada__strings__mapsB");
   u00082 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00082, "ada__strings__mapsS");
   u00083 : constant Version_32 := 16#41937159#;
   pragma Export (C, u00083, "system__bit_opsB");
   u00084 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00084, "system__bit_opsS");
   u00085 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00085, "ada__strings__maps__constantsS");
   u00086 : constant Version_32 := 16#1b6c1696#;
   pragma Export (C, u00086, "ada__directories__validityB");
   u00087 : constant Version_32 := 16#d34bdf62#;
   pragma Export (C, u00087, "ada__directories__validityS");
   u00088 : constant Version_32 := 16#e5480ede#;
   pragma Export (C, u00088, "ada__strings__fixedB");
   u00089 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00089, "ada__strings__fixedS");
   u00090 : constant Version_32 := 16#d22169ac#;
   pragma Export (C, u00090, "ada__strings__searchB");
   u00091 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00091, "ada__strings__searchS");
   u00092 : constant Version_32 := 16#f78329ae#;
   pragma Export (C, u00092, "ada__strings__unboundedB");
   u00093 : constant Version_32 := 16#e303cf90#;
   pragma Export (C, u00093, "ada__strings__unboundedS");
   u00094 : constant Version_32 := 16#12c8cd7d#;
   pragma Export (C, u00094, "ada__tagsB");
   u00095 : constant Version_32 := 16#ce72c228#;
   pragma Export (C, u00095, "ada__tagsS");
   u00096 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00096, "system__htableB");
   u00097 : constant Version_32 := 16#700c3fd0#;
   pragma Export (C, u00097, "system__htableS");
   u00098 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00098, "system__string_hashB");
   u00099 : constant Version_32 := 16#d25254ae#;
   pragma Export (C, u00099, "system__string_hashS");
   u00100 : constant Version_32 := 16#5b9edcc4#;
   pragma Export (C, u00100, "system__compare_array_unsigned_8B");
   u00101 : constant Version_32 := 16#5dcdfdb7#;
   pragma Export (C, u00101, "system__compare_array_unsigned_8S");
   u00102 : constant Version_32 := 16#5f72f755#;
   pragma Export (C, u00102, "system__address_operationsB");
   u00103 : constant Version_32 := 16#e7c23209#;
   pragma Export (C, u00103, "system__address_operationsS");
   u00104 : constant Version_32 := 16#6a859064#;
   pragma Export (C, u00104, "system__storage_pools__subpoolsB");
   u00105 : constant Version_32 := 16#e3b008dc#;
   pragma Export (C, u00105, "system__storage_pools__subpoolsS");
   u00106 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00106, "system__address_imageB");
   u00107 : constant Version_32 := 16#55221100#;
   pragma Export (C, u00107, "system__address_imageS");
   u00108 : constant Version_32 := 16#b5b2aca1#;
   pragma Export (C, u00108, "system__finalization_mastersB");
   u00109 : constant Version_32 := 16#80d8a57a#;
   pragma Export (C, u00109, "system__finalization_mastersS");
   u00110 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00110, "system__img_boolB");
   u00111 : constant Version_32 := 16#0117fdd1#;
   pragma Export (C, u00111, "system__img_boolS");
   u00112 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00112, "system__ioB");
   u00113 : constant Version_32 := 16#6a8c7b75#;
   pragma Export (C, u00113, "system__ioS");
   u00114 : constant Version_32 := 16#b7ab275c#;
   pragma Export (C, u00114, "ada__finalizationB");
   u00115 : constant Version_32 := 16#19f764ca#;
   pragma Export (C, u00115, "ada__finalizationS");
   u00116 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00116, "ada__streamsB");
   u00117 : constant Version_32 := 16#2e6701ab#;
   pragma Export (C, u00117, "ada__streamsS");
   u00118 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00118, "ada__io_exceptionsS");
   u00119 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00119, "system__finalization_rootB");
   u00120 : constant Version_32 := 16#bb3cffaa#;
   pragma Export (C, u00120, "system__finalization_rootS");
   u00121 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00121, "system__storage_poolsB");
   u00122 : constant Version_32 := 16#01950bbe#;
   pragma Export (C, u00122, "system__storage_poolsS");
   u00123 : constant Version_32 := 16#63f11652#;
   pragma Export (C, u00123, "system__storage_pools__subpools__finalizationB");
   u00124 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00124, "system__storage_pools__subpools__finalizationS");
   u00125 : constant Version_32 := 16#e5ac57f8#;
   pragma Export (C, u00125, "system__atomic_countersB");
   u00126 : constant Version_32 := 16#39b218f0#;
   pragma Export (C, u00126, "system__atomic_countersS");
   u00127 : constant Version_32 := 16#fb75f7f4#;
   pragma Export (C, u00127, "system__machine_codeS");
   u00128 : constant Version_32 := 16#f4e1c091#;
   pragma Export (C, u00128, "system__stream_attributesB");
   u00129 : constant Version_32 := 16#221dd20d#;
   pragma Export (C, u00129, "system__stream_attributesS");
   u00130 : constant Version_32 := 16#845f5a34#;
   pragma Export (C, u00130, "system__crtlS");
   u00131 : constant Version_32 := 16#55efb7f8#;
   pragma Export (C, u00131, "system__file_attributesS");
   u00132 : constant Version_32 := 16#2e610ef3#;
   pragma Export (C, u00132, "system__os_constantsS");
   u00133 : constant Version_32 := 16#431faf3c#;
   pragma Export (C, u00133, "system__file_ioB");
   u00134 : constant Version_32 := 16#53bf6d5f#;
   pragma Export (C, u00134, "system__file_ioS");
   u00135 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00135, "interfaces__c_streamsB");
   u00136 : constant Version_32 := 16#8bb5f2c0#;
   pragma Export (C, u00136, "interfaces__c_streamsS");
   u00137 : constant Version_32 := 16#ee0f26dd#;
   pragma Export (C, u00137, "system__os_libB");
   u00138 : constant Version_32 := 16#d7b69782#;
   pragma Export (C, u00138, "system__os_libS");
   u00139 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00139, "system__stringsB");
   u00140 : constant Version_32 := 16#8a719d5c#;
   pragma Export (C, u00140, "system__stringsS");
   u00141 : constant Version_32 := 16#09511692#;
   pragma Export (C, u00141, "system__file_control_blockS");
   u00142 : constant Version_32 := 16#933fac2f#;
   pragma Export (C, u00142, "system__regexpB");
   u00143 : constant Version_32 := 16#014a32df#;
   pragma Export (C, u00143, "system__regexpS");
   u00144 : constant Version_32 := 16#bc438ed0#;
   pragma Export (C, u00144, "ada__strings__wide_fixedB");
   u00145 : constant Version_32 := 16#412537cd#;
   pragma Export (C, u00145, "ada__strings__wide_fixedS");
   u00146 : constant Version_32 := 16#5ac153f8#;
   pragma Export (C, u00146, "ada__strings__wide_mapsB");
   u00147 : constant Version_32 := 16#f0f30b79#;
   pragma Export (C, u00147, "ada__strings__wide_mapsS");
   u00148 : constant Version_32 := 16#f677cb46#;
   pragma Export (C, u00148, "ada__strings__wide_searchB");
   u00149 : constant Version_32 := 16#1748eeac#;
   pragma Export (C, u00149, "ada__strings__wide_searchS");
   u00150 : constant Version_32 := 16#28f088c2#;
   pragma Export (C, u00150, "ada__text_ioB");
   u00151 : constant Version_32 := 16#1a9b0017#;
   pragma Export (C, u00151, "ada__text_ioS");
   u00152 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00152, "ada__text_io__integer_auxB");
   u00153 : constant Version_32 := 16#b9793d30#;
   pragma Export (C, u00153, "ada__text_io__integer_auxS");
   u00154 : constant Version_32 := 16#181dc502#;
   pragma Export (C, u00154, "ada__text_io__generic_auxB");
   u00155 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00155, "ada__text_io__generic_auxS");
   u00156 : constant Version_32 := 16#18d57884#;
   pragma Export (C, u00156, "system__img_biuB");
   u00157 : constant Version_32 := 16#afb4a0b7#;
   pragma Export (C, u00157, "system__img_biuS");
   u00158 : constant Version_32 := 16#e7d8734f#;
   pragma Export (C, u00158, "system__img_llbB");
   u00159 : constant Version_32 := 16#ee73b049#;
   pragma Export (C, u00159, "system__img_llbS");
   u00160 : constant Version_32 := 16#9777733a#;
   pragma Export (C, u00160, "system__img_lliB");
   u00161 : constant Version_32 := 16#e581d9eb#;
   pragma Export (C, u00161, "system__img_lliS");
   u00162 : constant Version_32 := 16#0e8808d4#;
   pragma Export (C, u00162, "system__img_llwB");
   u00163 : constant Version_32 := 16#471f93df#;
   pragma Export (C, u00163, "system__img_llwS");
   u00164 : constant Version_32 := 16#428b07f8#;
   pragma Export (C, u00164, "system__img_wiuB");
   u00165 : constant Version_32 := 16#c1f52725#;
   pragma Export (C, u00165, "system__img_wiuS");
   u00166 : constant Version_32 := 16#b3aa7b17#;
   pragma Export (C, u00166, "system__val_lliB");
   u00167 : constant Version_32 := 16#6eea6a9a#;
   pragma Export (C, u00167, "system__val_lliS");
   u00168 : constant Version_32 := 16#06052bd0#;
   pragma Export (C, u00168, "system__val_lluB");
   u00169 : constant Version_32 := 16#13647f88#;
   pragma Export (C, u00169, "system__val_lluS");
   u00170 : constant Version_32 := 16#2c09f1c1#;
   pragma Export (C, u00170, "ada__wide_text_ioB");
   u00171 : constant Version_32 := 16#92d6b5bc#;
   pragma Export (C, u00171, "ada__wide_text_ioS");
   u00172 : constant Version_32 := 16#0d364ce2#;
   pragma Export (C, u00172, "ada__wide_text_io__integer_auxB");
   u00173 : constant Version_32 := 16#065836b1#;
   pragma Export (C, u00173, "ada__wide_text_io__integer_auxS");
   u00174 : constant Version_32 := 16#01e5b1e2#;
   pragma Export (C, u00174, "ada__wide_text_io__generic_auxB");
   u00175 : constant Version_32 := 16#3ef64803#;
   pragma Export (C, u00175, "ada__wide_text_io__generic_auxS");
   u00176 : constant Version_32 := 16#2af6d23c#;
   pragma Export (C, u00176, "adabeaut_configB");
   u00177 : constant Version_32 := 16#3e483939#;
   pragma Export (C, u00177, "adabeaut_configS");
   u00178 : constant Version_32 := 16#e34550ca#;
   pragma Export (C, u00178, "system__pool_globalB");
   u00179 : constant Version_32 := 16#c88d2d16#;
   pragma Export (C, u00179, "system__pool_globalS");
   u00180 : constant Version_32 := 16#2bce1226#;
   pragma Export (C, u00180, "system__memoryB");
   u00181 : constant Version_32 := 16#adb3ea0e#;
   pragma Export (C, u00181, "system__memoryS");
   u00182 : constant Version_32 := 16#d4c99303#;
   pragma Export (C, u00182, "ada_configurationB");
   u00183 : constant Version_32 := 16#20a2d085#;
   pragma Export (C, u00183, "ada_configurationS");
   u00184 : constant Version_32 := 16#72fd7b17#;
   pragma Export (C, u00184, "ada__command_lineB");
   u00185 : constant Version_32 := 16#d59e21a4#;
   pragma Export (C, u00185, "ada__command_lineS");
   u00186 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00186, "ada__containersS");
   u00187 : constant Version_32 := 16#54b8a3b7#;
   pragma Export (C, u00187, "ada__text_io__unbounded_ioS");
   u00188 : constant Version_32 := 16#97a2d3b4#;
   pragma Export (C, u00188, "ada__strings__unbounded__text_ioB");
   u00189 : constant Version_32 := 16#2124c8bb#;
   pragma Export (C, u00189, "ada__strings__unbounded__text_ioS");
   u00190 : constant Version_32 := 16#1767a79e#;
   pragma Export (C, u00190, "system__assertionsB");
   u00191 : constant Version_32 := 16#3943a0ae#;
   pragma Export (C, u00191, "system__assertionsS");
   u00192 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00192, "system__concat_3B");
   u00193 : constant Version_32 := 16#ffbed09f#;
   pragma Export (C, u00193, "system__concat_3S");
   u00194 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00194, "system__concat_2B");
   u00195 : constant Version_32 := 16#f66e5bea#;
   pragma Export (C, u00195, "system__concat_2S");
   u00196 : constant Version_32 := 16#06cb2950#;
   pragma Export (C, u00196, "system__strings__stream_opsB");
   u00197 : constant Version_32 := 16#55d4bd57#;
   pragma Export (C, u00197, "system__strings__stream_opsS");
   u00198 : constant Version_32 := 16#a71b0af5#;
   pragma Export (C, u00198, "ada__streams__stream_ioB");
   u00199 : constant Version_32 := 16#31fc8e02#;
   pragma Export (C, u00199, "ada__streams__stream_ioS");
   u00200 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00200, "system__communicationB");
   u00201 : constant Version_32 := 16#edaed9e8#;
   pragma Export (C, u00201, "system__communicationS");
   u00202 : constant Version_32 := 16#654e2c4c#;
   pragma Export (C, u00202, "ada__containers__hash_tablesS");
   u00203 : constant Version_32 := 16#c24eaf4d#;
   pragma Export (C, u00203, "ada__containers__prime_numbersB");
   u00204 : constant Version_32 := 16#6d3af8ed#;
   pragma Export (C, u00204, "ada__containers__prime_numbersS");
   u00205 : constant Version_32 := 16#22d17b05#;
   pragma Export (C, u00205, "ada__strings__equal_case_insensitiveB");
   u00206 : constant Version_32 := 16#a7ec4680#;
   pragma Export (C, u00206, "ada__strings__equal_case_insensitiveS");
   u00207 : constant Version_32 := 16#eee9c0c6#;
   pragma Export (C, u00207, "ada__strings__hash_case_insensitiveB");
   u00208 : constant Version_32 := 16#f9e6d5c1#;
   pragma Export (C, u00208, "ada__strings__hash_case_insensitiveS");
   u00209 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00209, "system__concat_4B");
   u00210 : constant Version_32 := 16#8aaaa71a#;
   pragma Export (C, u00210, "system__concat_4S");
   u00211 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00211, "system__concat_5B");
   u00212 : constant Version_32 := 16#7390cf14#;
   pragma Export (C, u00212, "system__concat_5S");
   u00213 : constant Version_32 := 16#c621f396#;
   pragma Export (C, u00213, "system__wch_wtsB");
   u00214 : constant Version_32 := 16#5cda815a#;
   pragma Export (C, u00214, "system__wch_wtsS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.conversions%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  ada.command_line%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.atomic_counters%b
   --  system.os_primitives%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.task_lock%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.task_lock%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.val_int%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.val_int%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  system.wch_wts%s
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.containers.hash_tables%s
   --  ada.containers.prime_numbers%s
   --  ada.containers.prime_numbers%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.equal_case_insensitive%s
   --  ada.strings.equal_case_insensitive%b
   --  ada.strings.hash_case_insensitive%s
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.communication%s
   --  system.communication%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.file_control_block%s
   --  ada.streams.stream_io%s
   --  system.file_io%s
   --  ada.streams.stream_io%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.os_constants%s
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.win32%s
   --  system.win32.ext%s
   --  system.os_primitives%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  ada.wide_text_io%s
   --  ada.wide_text_io.generic_aux%s
   --  ada.wide_text_io.generic_aux%b
   --  ada.wide_text_io.integer_aux%s
   --  ada.wide_text_io.integer_aux%b
   --  system.assertions%s
   --  system.assertions%b
   --  system.file_attributes%s
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.secondary_stack%s
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  system.file_io%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.fixed%b
   --  ada.strings.maps%b
   --  ada.strings.hash_case_insensitive%b
   --  system.wch_wts%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  ada.command_line%b
   --  ada.characters.handling%b
   --  ada.characters.conversions%b
   --  system.secondary_stack%b
   --  ada.calendar.formatting%b
   --  system.address_image%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  ada.strings.wide_maps%s
   --  ada.strings.wide_maps%b
   --  ada.strings.wide_fixed%s
   --  ada.strings.wide_search%s
   --  ada.strings.wide_search%b
   --  ada.strings.wide_fixed%b
   --  ada.directories%s
   --  ada.directories.validity%s
   --  ada.directories.validity%b
   --  system.regexp%s
   --  system.regexp%b
   --  ada.directories%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.wide_text_io%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.strings.unbounded.text_io%s
   --  ada.strings.unbounded.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.text_io.unbounded_io%s
   --  ada_configuration%s
   --  ada_configuration%b
   --  adabeaut_config%s
   --  adabeaut_config%b
   --  adabeaut%b
   --  END ELABORATION ORDER


end ada_main;
