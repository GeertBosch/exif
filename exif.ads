with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;            use Interfaces;
with System;

package Exif is

   File_Bit_Order : System.Bit_Order := System.High_Order_First;
   --  This should be set correctly before processing data.
   --  The current setting determines how File_XX types are interpreted.

   Always_Show_Type : Boolean := True;
   --  Forces typing of all displayed data

   type Storage is new Stream_Element_Array;
   type Storage_Access is access all Storage;
   subtype Storage_Offset is Stream_Element_Offset;

   subtype File_8  is Stream_Element;
   subtype File_16 is Storage (1 .. 2);
   subtype File_32 is Storage (1 .. 4);
   subtype File_64 is Storage (1 .. 8);

   type IFD_Datatype is
     (Byte, Ascii, Short, Long, Rational, SByte, Undefined,
      SShort, SLong, SRational, Floating, Double);

   type Fraction is record
      Numerator   : Unsigned_32;
      Denominator : Unsigned_32;
   end record;

   type SFraction is record
      Numerator   : Integer_32;
      Denominator : Integer_32;
   end record;

   type Text is access constant String;

   No_Text : constant Text := null;

   function Value (Item : File_8)  return Unsigned_8;
   function Value (Item : File_16) return Unsigned_16;
   function Value (Item : File_16) return Integer_16;
   function Value (Item : File_32) return Unsigned_32;
   function Value (Item : File_32) return Integer_32;
   function Value (Item : File_64) return Fraction;
   function Value (Item : File_64) return SFraction;

   subtype IFD_Entry is Storage (0 .. 11);

   type IFD_Kind is
     (Unknown_IFD,
      --  The following four share most of their tags
      TIFF_IFD, Primary_Image_IFD, Thumbnail_IFD, Exif_IFD,
      Interoperability_IFD, GPS_IFD,
      Canon_IFD, Casio_IFD, Fujifilm_IFD, Kodak_IFD, Nikon_IFD,
      Nikon_Old_IFD, Olympus_IFD,
      --  The following are generated directories for non-TIFF IFD's
      Flashpix_IFD, Photoshop_IFD, Canon_RaW_IFD);

   function Image (Item : Fraction) return String;
   function Image (Item : SFraction) return String;

   procedure Write (Stream : Stream_IO.Stream_Access; Data : Integer_32);
   procedure Write (Stream : Stream_IO.Stream_Access; Data : Unsigned_32);
   procedure Write (Stream : Stream_IO.Stream_Access; Data : Integer_16);
   procedure Write (Stream : Stream_IO.Stream_Access; Data : Unsigned_16);
   procedure Write (Stream : Stream_IO.Stream_Access; Data : Fraction);
   procedure Write (Stream : Stream_IO.Stream_Access; Data : SFraction);
   procedure Write (Stream : Stream_IO.Stream_Access; Datatype : IFD_Datatype;
                    Data     : Storage);

   function Storage_Size (Datatype : IFD_Datatype) return Storage_Offset;
   --  Return size in bytes for one value with type datatype

   function To_Datatype (Item : File_16) return IFD_Datatype;

   type Interoperability_Tag is
     (Interoperability_Index, Interoperability_Version,
      Related_Image_File_Format,
      Related_Image_Width,
      Related_Image_Length);

   type Exif_Tag is
     (New_Subfile_Type,
      Subfile_Type,
      Image_Width,
      Image_Length,
      Bits_Per_Sample,
      Compression,
      Photometric_Interpretation,
      Thresholding,
      Cell_Width,
      Cell_Length,
      Fill_Order,
      Document_Name,
      Image_Description,
      Make,
      Model,
      Strip_Offsets,
      Orientation,
      Samples_Per_Pixel,
      Rows_Per_Strip,
      Strip_Byte_Counts,
      Min_Sample_Value,
      Max_Sample_Value,
      XResolution,
      YResolution,
      Planar_Configuration,
      Page_Name,
      XPosition,
      YPosition,
      Free_Offsets,
      Free_Byte_Counts,
      Gray_Response_Unit,
      Gray_Response_Curve,
      T4_Options,
      T6_Options,
      Resolution_Unit,
      Page_Number,
      Transfer_Function,
      Software,
      Date_Time,
      Artist,
      Host_Computer,
      Predictor,
      White_Point,
      Primary_Chromaticities,
      Color_Map,
      Halftone_Hints,
      Tile_Width,
      Tile_Length,
      Tile_Offsets,
      Tile_Byte_Counts,
      Sub_IFDs,
      Ink_Set,
      Ink_Names,
      Number_Of_Inks,
      Dot_Range,
      Target_Printer,
      Extra_Samples,
      Sample_Format,
      SMin_Sample_Value,
      SMax_Sample_Value,
      Transfer_Range,
      Clip_Path,
      X_Clip_Path_Units,
      Y_Clip_Path_Units,
      Indexed,
      JPEG_Tables,
      OPI_Proxy,
      JPEG_Proc,
      JPEG_Interchange_Format,
      JPEG_Interchange_Format_Length,
      JPEG_Restart_Interval,
      JPEG_Lossless_Predictors,
      JPEG_Point_Transforms,
      JPEG_Q_Tables,
      JPEG_DC_Tables,
      JPEG_AC_Tables,
      YCb_Cr_Coefficients,
      YCb_Cr_Sub_Sampling,
      YCb_Cr_Positioning,
      Reference_Black_White,
      Image_ID,
      CFA_Repeat_Pattern_Dim,
      CFA_Pattern_Old,
      Battery_Level,
      Kodak_Camera_Info_Offset,
      Copyright,
      Exposure_Time,
      FNumber,
      IPTC_NAA,
      IT8_Raster_Padding,
      IT8_Color_Table,
      PS_Image_Resources,
      Exif_Offset,
      Internation_Color_Profile,
      Exposure_Program,
      Spectral_Sensitivity,
      GPS_Info,
      ISO_Speed_Ratings,
      OECF,
      Interlace,
      Time_Zone_Offset,
      Self_Timer_Mode,
      Exif_Version,
      Date_Time_Original,
      Date_Time_Digitized,
      Components_Configuration,
      Compressed_Bits_Per_Pixel,
      Shutter_Speed_Value,
      Aperture_Value,
      Brightness_Value,
      Exposure_Bias_Value,
      Max_Aperture_Value,
      Subject_Distance,
      Metering_Mode,
      Light_Source,
      Flash,
      Focal_Length,
      TIFF_EP_Flash_Energy,
      TIFF_EP_Spatial_Frequency_Response,
      TIFF_EP_Noise,
      TIFF_EP_Image_Number,
      TIFF_EP_Security_Classification,
      TIFF_EP_Image_History,
      TIFF_EP_Subject_Location,
      TIFF_EP_Exposure_Index,
      TIFF_EP_Standard_ID,
      Maker_Note,
      User_Comment,
      Sub_Sec_Time,
      Sub_Sec_Time_Original,
      Sub_Sec_Time_Digitized,
      Image_Source_Data,
      Flash_Pix_Version,
      Color_Space,
      Exif_Image_Width,
      Exif_Image_Length,
      Related_Sound_File,
      Interoperability_Offset,
      Flash_Energy,
      Spatial_Frequency_Response,
      Focal_Plane_XResolution,
      Focal_Plane_YResolution,
      Focal_Plane_Resolution_Unit,
      Subject_Location,
      Exposure_Index,
      Sensing_Method,
      File_Source,
      Scene_Type,
      CFA_Pattern,
      Custom_Rendered,
      Exposure_Mode,
      White_Balance,
      Digital_Zoom_Ratio,
      Focal_Length_In_35mm_Film,
      Scene_Capture_Type,
      Gain_Control,
      Contrast,
      Saturation,
      Sharpness,
      Device_Settings_Description,
      Subject_Distance_Range,
      Image_Unique_ID,
      Gamma,
      Annotations);

   type GPS_Tag is
     (GPS_Tag_Version,
      North_or_South_Latitude,
      Latitude,
      East_or_West_Longitude,
      Longitude,
      Altitude_reference,
      Altitude,
      GPS_time,
      GPS_satellites_used,
      GPS_receiver_status,
      GPS_measurement_mode,
      Measurement_precision,
      Speed_unit,
      Speed_of_GPS_receiver,
      Reference_for_direction_of_movement,
      Direction_of_movement,
      Reference_for_direction_of_image,
      Direction_of_image,
      Geodetic_survey_data_used,
      Reference_for_latitude_of_destination,
      Latitude_of_destination,
      Reference_for_longitude_of_destination,
      Longitude_of_destination,
      Reference_for_bearing_of_destination,
      Bearing_of_destination,
      Reference_for_distance_to_destination,
      Distance_to_destination);

   type Canon_Tag is
     (Settings_1,
      Settings_4,
      Image_Type,
      Firmware_Version,
      Image_Number,
      Owner_Name,
      Camera_Serial_Number,
      Custom_Functions);

   type Casio_Tag is
     (Recording_Mode,
      Quality,
      Focusing_Mode,
      Flash_Mode,
      Flash_Intensity,
      Object_Distance,
      White_Balance,
      Digital_Zoom,
      Sharpness,
      Contrast,
      Saturation,
      CCD_Sensitivity);

   type Canon_Raw_Tag is
     (JPEG_Image);

   type Flashpix_Tag is
     (JPEG_Image);

   type Fujifilm_Tag is
     (Fujifilm_Tag_Version,
      Quality,
      Sharpness,
      White_Balance,
      Color,
      Tone,
      Flash_Mode,
      Flash_Strength,
      Macro,
      Focus_Mode,
      Slow_Sync,
      Picture_Mode,
      Continuous_Or_Bracket,
      Blur_Warning,
      Focus_Warning,
      Exposure_Warning);

   type Kodak_Tag is
     (Film_Product_Code,
      Image_Source,
      Intended_Print_Area,
      Camera_Owner_Id,
      Camera_Serial_Number,
      Group_Caption,
      Dealer_Id,
      Filmstrip_Id,
      Bag_Number,
      Scan_Frame_Sequence_Number,
      Film_Category,
      Film_Generation_Code,
      Scanner_Software,
      Film_Size,
      Image_Rotation_Status,
      Roll_Guide,
      Metadata_Version,
      Edit_Tag_Array,
      Burst_Time_Lapse_Sequence,
      Digital_Resize_Factor,
      Burst_Time_Lapse_Sequence2,
      Cartridge_Hand_Of_Load,
      Native_Focal_Plane_XResolution,
      Native_Focal_Plane_YResolution,
      Special_Effects,
      Borders,
      Native_Focal_Plane_Resolution_Unit);

   type Nikon_Old_Tag is
     (Nikon_Tag_Version,
      Quality,
      Color_Mode,
      Image_Adjustment,
      CCD_Sensitivity,
      White_Balance,
      Focus,
      Digital_Zoom,
      Converter);

   type Nikon_Tag is
     (Nikon_Tag_Version,
      ISO_Setting,
      Color_Mode,
      Quality,
      White_Balance,
      Image_Sharpening,
      Focus_Mode,
      Flash_Setting,
      ISO_Selection,
      Image_Adjustment,
      Adapter,
      Lens,
      Manual_Focus_Distance,
      Digital_Zoom,
      Auto_Focus_Position);

   type Olympus_Tag is
     (Special_Mode,
      Quality,
      Macro,
      Digital_Zoom,
      Firmware_Version,
      Picture_Info,
      Camera_ID);

   type Photoshop_Tag is
     (Print_Info,
      Resolution,
      Alpha_Channel_Names,
      Alpha_Channel_Settings,
      Print_Flags,
      Monochrome_Halftone_Settings,
      Color_Halftone_Settings,
      Monochrome_Transfer_Settings,
      Color_Transfer_Settings,
      Layer_State,
      Layer_Groups,
      Caption,
      JPEG_Quality,
      Guides,
      Copyright_Flag,
      New_Windows_Thumbnail,
      FX_Global_Lighting_Angle,
      ICC_Profile,
      ICC_Untagged_Flag,
      Layer_ID_Generator_Base,
      Alpha_Channel_Unicode_Names,
      FX_Global_Altitude,
      Slices,
      Alpha_Channel_Identifiers,
      URL_Overrides,
      Version_Compatibility_Info,
      EXIF,
      EXIF2,
      Japanese_Print_Flags
      );

   type IFD_Tag (Kind : IFD_Kind := TIFF_IFD) is record
      case Kind is
         when Unknown_IFD =>
            Value : Unsigned_16;

         when Interoperability_IFD =>
            Interoperability : Interoperability_Tag;

         when TIFF_IFD | Primary_Image_IFD | Exif_IFD | Thumbnail_IFD =>
            TIFF  : Exif_Tag;

         when GPS_IFD =>
            GPS   : GPS_Tag;

         when Canon_IFD =>
            Canon : Canon_Tag;

         when Canon_Raw_IFD =>
            Canon_Raw : Canon_Raw_Tag;

         when Casio_IFD =>
            Casio : Casio_Tag;

         when Flashpix_IFD =>
            Flashpix : Flashpix_Tag;

         when Fujifilm_IFD =>
            Fujifilm : Fujifilm_Tag;

         when Kodak_IFD =>
            Kodak : Kodak_Tag;

         when Nikon_IFD =>
            Nikon : Nikon_Tag;

         when Nikon_Old_IFD =>
            Nikon_Old : Nikon_Old_Tag;

         when Olympus_IFD =>
            Olympus : Olympus_Tag;

         when Photoshop_IFD =>
            Photoshop : Photoshop_Tag;

      end case;
   end record;

   function To_IFD_Tag (Kind : IFD_Kind; Value : Unsigned_16) return IFD_Tag;
   function Value (Item : IFD_Tag) return Unsigned_16;

   function Describe
     (Tag      : IFD_Tag;
      Datatype : IFD_Datatype;
      Value    : Unsigned_32)
      return Text;

   procedure Write (Stream : Stream_IO.Stream_Access; Tag : IFD_Tag);

private

   type Value_Description is record
      Value       : Unsigned_32;
      Description : Text;
   end record;

   type Value_Description_Table is
     array (Positive range <>) of Value_Description;

   type Value_Description_Table_Access is
     access constant Value_Description_Table;

   type Tag_Description is record
      Tag    : IFD_Tag;
      Values : Value_Description_Table_Access;
   end record;

   type Tag_Description_Table is
     array (Positive range <>) of Tag_Description;


   for Interoperability_Tag'Size use 16;

   for Interoperability_Tag use
     (Interoperability_Index    => 16#1#,
      Interoperability_Version  => 16#2#,
      Related_Image_File_Format => 16#1000#,
      Related_Image_Width       => 16#1001#,
      Related_Image_Length      => 16#1002#);

   for GPS_Tag'Size use 16;

   for GPS_Tag use
     (GPS_Tag_Version                        => 16#0#, -- BYTE 4
      North_or_South_Latitude                => 16#1#, -- ASCII 2
      Latitude                               => 16#2#, -- RATIONAL 3
      East_or_West_Longitude                 => 16#3#, -- ASCII 2
      Longitude                              => 16#4#, -- RATIONAL 3
      Altitude_reference                     => 16#5#, -- BYTE 1
      Altitude                               => 16#6#, -- RATIONAL 1
      GPS_time                               => 16#7#, -- RATIONAL 3
      GPS_satellites_used                    => 16#8#, -- ASCII Any
      GPS_receiver_status                    => 16#9#, -- ASCII 2
      GPS_measurement_mode                   => 16#A#, -- ASCII 2
      Measurement_precision                  => 16#B#, -- RATIONAL 1
      Speed_unit                             => 16#C#, -- ASCII 2
      Speed_of_GPS_receiver                  => 16#D#, -- RATIONAL 1
      Reference_for_direction_of_movement    => 16#E#, -- ASCII 2
      Direction_of_movement                  => 16#F#, -- RATIONAL 1
      Reference_for_direction_of_image       => 16#10#, -- ASCII 2
      Direction_of_image                     => 16#11#, -- RATIONAL 1
      Geodetic_survey_data_used              => 16#12#, -- ASCII Any
      Reference_for_latitude_of_destination  => 16#13#, -- ASCII 2
      Latitude_of_destination                => 16#14#, -- RATIONAL 3
      Reference_for_longitude_of_destination => 16#15#, -- ASCII 2
      Longitude_of_destination               => 16#16#, -- RATIONAL 3
      Reference_for_bearing_of_destination   => 16#17#, -- ASCII 2
      Bearing_of_destination                 => 16#18#, -- RATIONAL 1
      Reference_for_distance_to_destination  => 16#19#, -- ASCII 2
      Distance_to_destination                => 16#1A#); -- RATIONAL 1

   for Exif_Tag use
     (New_Subfile_Type          => 16#FE#,
      Subfile_Type              => 16#FF#,
      Image_Width                => 16#100#,
      Image_Length               => 16#101#,
      Bits_Per_Sample            => 16#102#,
      Compression                => 16#103#,
      Photometric_Interpretation => 16#106#,
      Thresholding               => 16#107#,
      Cell_Width                 => 16#108#,
      Cell_Length                => 16#109#,
      Fill_Order                 => 16#10A#,
      Document_Name              => 16#10D#,
      Image_Description          => 16#10E#,
      Make                       => 16#10F#,
      Model                      => 16#110#,
      Strip_Offsets              => 16#111#,
      Orientation                => 16#112#,
      Samples_Per_Pixel          => 16#115#,
      Rows_Per_Strip             => 16#116#,
      Strip_Byte_Counts          => 16#117#,
      Min_Sample_Value           => 16#118#,
      Max_Sample_Value           => 16#119#,
      XResolution                => 16#11A#,
      YResolution                => 16#11B#,
      Planar_Configuration       => 16#11C#,
      Page_Name                  => 16#11D#,
      XPosition                  => 16#11E#,
      YPosition                  => 16#11F#,
      Free_Offsets               => 16#120#,
      Free_Byte_Counts           => 16#121#,
      Gray_Response_Unit         => 16#122#,
      Gray_Response_Curve        => 16#123#,
      T4_Options                 => 16#124#,
      T6_Options                 => 16#125#,
      Resolution_Unit            => 16#128#,
      Page_Number                => 16#129#,
      Transfer_Function          => 16#12D#,
      Software                   => 16#131#,
      Date_Time                  => 16#132#,
      Artist                     => 16#13B#,
      Host_Computer              => 16#13C#,
      Predictor                  => 16#13D#,
      White_Point                => 16#13E#,
      Primary_Chromaticities     => 16#13F#,
      Color_Map                  => 16#140#,
      Halftone_Hints             => 16#141#,
      Tile_Width                 => 16#142#,
      Tile_Length                => 16#143#,
      Tile_Offsets               => 16#144#,
      Tile_Byte_Counts           => 16#145#,
      Sub_IFDs                   => 16#14A#, -- Used by PhotoShop 6, PageMaker
      Ink_Set                    => 16#14C#,
      Ink_Names                  => 16#14D#,
      Number_Of_Inks             => 16#14E#,
      Dot_Range                  => 16#150#,
      Target_Printer             => 16#151#,
      Extra_Samples              => 16#152#,
      Sample_Format              => 16#153#,
      SMin_Sample_Value          => 16#154#,
      SMax_Sample_Value          => 16#155#,
      Transfer_Range             => 16#156#,
      Clip_Path                  => 16#157#, -- See Pagemaker TIFF Tech notes
      X_Clip_Path_Units          => 16#158#,
      Y_Clip_Path_Units          => 16#159#,
      Indexed                    => 16#15A#, -- Last PM specific tag
      JPEG_Tables                => 16#15B#,
      OPI_Proxy                  => 16#15F#, -- Adobe OPI 2.0 tag
      JPEG_Proc                  => 16#200#,
      JPEG_Interchange_Format    => 16#201#,
      JPEG_Interchange_Format_Length => 16#202#,
      JPEG_Restart_Interval      => 16#203#,
      JPEG_Lossless_Predictors   => 16#205#,
      JPEG_Point_Transforms      => 16#206#,
      JPEG_Q_Tables              => 16#207#,
      JPEG_DC_Tables             => 16#208#,
      JPEG_AC_Tables             => 16#209#,
      YCb_Cr_Coefficients        => 16#211#,
      YCb_Cr_Sub_Sampling        => 16#212#,
      YCb_Cr_Positioning         => 16#213#,
      Reference_Black_White      => 16#214#,
      Image_ID                   => 16#800D#, -- Adobe OPI 2.0 tag
      CFA_Repeat_Pattern_Dim     => 16#828D#,
      CFA_Pattern_Old            => 16#828E#,
      Battery_Level              => 16#828F#,
      Kodak_Camera_Info_Offset   => 16#8290#,
      --  16#8292# occurs in an HP C912 generated TIFF file
      Copyright                  => 16#8298#,
      Exposure_Time              => 16#829A#,
      FNumber                    => 16#829D#,
      IPTC_NAA                   => 16#83BB#,  -- Used by PhotoShop 6.0
      IT8_Raster_Padding         => 16#84E3#,  -- (34019) Used by Pagemaker
      IT8_Color_Table            => 16#84E6#,  -- (34022) Used by Pagemaker
      PS_Image_Resources         => 16#8649#,  -- Used by PhotoShop 6.0
      Exif_Offset                => 16#8769#,
      Internation_Color_Profile  => 16#8773#,
      Exposure_Program           => 16#8822#,
      Spectral_Sensitivity       => 16#8824#,
      GPS_Info                   => 16#8825#,
      ISO_Speed_Ratings          => 16#8827#,
      OECF                       => 16#8828#,
      Interlace                  => 16#8829#,
      Time_Zone_Offset           => 16#882A#,
      Self_Timer_Mode            => 16#882B#,
      Exif_Version               => 16#9000#,
      Date_Time_Original         => 16#9003#,
      Date_Time_Digitized        => 16#9004#,
      Components_Configuration   => 16#9101#,
      Compressed_Bits_Per_Pixel  => 16#9102#,
      Shutter_Speed_Value        => 16#9201#,
      Aperture_Value             => 16#9202#,
      Brightness_Value           => 16#9203#,
      Exposure_Bias_Value        => 16#9204#,
      Max_Aperture_Value         => 16#9205#,
      Subject_Distance           => 16#9206#,
      Metering_Mode              => 16#9207#,
      Light_Source               => 16#9208#,
      Flash                      => 16#9209#,
      Focal_Length               => 16#920A#,
      TIFF_EP_Flash_Energy               => 16#920B#,
      TIFF_EP_Spatial_Frequency_Response => 16#920C#,
      TIFF_EP_Noise                      => 16#920D#,
      TIFF_EP_Image_Number               => 16#9211#,
      TIFF_EP_Security_Classification    => 16#9212#,
      TIFF_EP_Image_History              => 16#9213#,
      TIFF_EP_Subject_Location           => 16#9214#,
      TIFF_EP_Exposure_Index             => 16#9215#,
      TIFF_EP_Standard_ID                => 16#9216#,
      Maker_Note                 => 16#927C#,
      User_Comment               => 16#9286#,
      Sub_Sec_Time               => 16#9290#,
      Sub_Sec_Time_Original      => 16#9291#,
      Sub_Sec_Time_Digitized     => 16#9292#,

      Image_Source_Data          => 16#935C#,  -- PhotoShop 6.0 Extension

      Flash_Pix_Version          => 16#A000#,
      Color_Space                => 16#A001#,
      Exif_Image_Width           => 16#A002#,
      Exif_Image_Length          => 16#A003#,
      Related_Sound_File         => 16#A004#,
      Interoperability_Offset    => 16#A005#,
      Flash_Energy               => 16#A20B#,  -- 16#0x92# in TIFF/EP
      Spatial_Frequency_Response => 16#A20C#,  -- 16#0x92#    -  -
      Focal_Plane_XResolution    => 16#A20E#,  -- 16#0x92#    -  -
      Focal_Plane_YResolution    => 16#A20F#,  -- 16#0x92#    -  -
      Focal_Plane_Resolution_Unit => 16#A210#, -- 16#0x92#    -  -
      Subject_Location           => 16#A214#,  -- 16#0x92#    -  -
      Exposure_Index             => 16#A215#,  -- 16#0x92#    -  -
      Sensing_Method             => 16#A217#,  -- 16#0x92#    -  -
      File_Source                => 16#A300#,
      Scene_Type                 => 16#A301#,
      CFA_Pattern                => 16#A302#,
      Custom_Rendered            => 16#A401#,
      Exposure_Mode              => 16#A402#,
      White_Balance              => 16#A403#,
      Digital_Zoom_Ratio         => 16#A404#,
      Focal_Length_In_35mm_Film  => 16#A405#,
      Scene_Capture_Type         => 16#A406#,
      Gain_Control               => 16#A407#,
      Contrast                   => 16#A408#,
      Saturation                 => 16#A409#,
      Sharpness                  => 16#A40A#,
      Device_Settings_Description => 16#A40B#,
      Subject_Distance_Range     => 16#A40C#,
      Image_Unique_ID            => 16#A420#,
      Gamma                      => 16#A500#,
      Annotations                => 16#C44F#); --  PhotoShop 6.0 Extension

   for Canon_Tag'Size use 16;
   for Canon_Tag use
     (Settings_1             => 16#1#,
      Settings_4             => 16#4#,
      Image_Type             => 16#6#,
      Firmware_Version       => 16#7#,
      Image_Number           => 16#8#,
      Owner_Name             => 16#9#,
      Camera_Serial_Number   => 16#C#,
      Custom_Functions       => 16#F#);

   for Canon_Raw_Tag'Size use 16;

   for Casio_Tag'Size use 16;
   for Casio_Tag use
     (Recording_Mode => 16#1#,
      Quality        => 16#2#,
      Focusing_Mode  => 16#3#,
      Flash_Mode     => 16#4#,
      Flash_Intensity => 16#5#,
      Object_Distance => 16#6#,
      White_Balance     => 16#7#,
      Digital_Zoom      => 16#a#,
      Sharpness         => 16#b#,
      Contrast          => 16#c#,
      Saturation        => 16#d#,
      CCD_Sensitivity   => 16#14#);

   for Flashpix_Tag'Size use 16;

   for Fujifilm_Tag'Size use 16;
   for Fujifilm_Tag use
     (Fujifilm_Tag_Version => 16#0000#, --  0130
      Quality              => 16#1000#, --  BASIC, NORMAL, FINE
      Sharpness            => 16#1001#, --  1,2:soft, 3:normal, 4,5: hard
      White_Balance        => 16#1002#, --  0:auto, 256:Daylight, 512:Cloudy,
                                        --  768:DaylightColor-fluorescence,
                                        --  769:DaywhiteColor-fluorescence,
                                        --  770:White-fluorescence,
                                        --  1024:Incandenscense,
                                        --  3840:Custom white balance
      Color                => 16#1003#, --  0:normal, 256:High, 512:Low
      Tone                 => 16#1004#, --  0:normal, 256:High, 512:Low
      Flash_Mode           => 16#1010#, --  0:auto, 1:on, 2:off, 3:red-eye
      Flash_Strength       => 16#1011#, --  Unit is APEX (EV)
      Macro                => 16#1020#, --  0:off, 1:on
      Focus_Mode           => 16#1021#, --  0:auto focus, 1:manual focus
      Slow_Sync            => 16#1030#, --  0:off, 1:on
      Picture_Mode         => 16#1031#, --  0:auto, 1:portrait scene,
                                        --  2:landscape scene, 4:sports scene,
                                        --  5:night scene, 6:program AE,
                                        --  256:aperture prior AE,
                                        --  512:shutter prior AE,
                                        --  768:manual exposure
      Continuous_Or_Bracket => 16#1100#, -- 0:off, 1:on
      Blur_Warning         => 16#1300#,  -- 0:off, 1:on
      Focus_Warning        => 16#1301#,  -- 0:auto focus good, 1: out of focus
      Exposure_Warning     => 16#1302#); -- 0:exposure good, 1:over exposure

   for Kodak_Tag'Size use 16;
   for Kodak_Tag use
     (Film_Product_Code          => 16#C350#,  -- 50000
      Image_Source               => 16#C351#,  -- 50001
      Intended_Print_Area        => 16#C352#,  -- 50002
      Camera_Owner_Id            => 16#C353#,  -- 50003
      Camera_Serial_Number       => 16#C354#,  -- 50004
      Group_Caption              => 16#C355#,  -- 50005
      Dealer_Id                  => 16#C356#,  -- 50006
      Filmstrip_Id               => 16#C357#,  -- 50007, also seen Order_Id?
      Bag_Number                 => 16#C358#,  -- 50008
      Scan_Frame_Sequence_Number => 16#C359#,  -- 50009
      Film_Category              => 16#C35A#,  -- 50010
      Film_Generation_Code       => 16#C35B#,  -- 50011
      Scanner_Software           => 16#C35C#,  -- 50012
      Film_Size                  => 16#C35D#,  -- 50013
      Image_Rotation_Status      => 16#C363#,  -- 50019
      Roll_Guide                 => 16#C364#,  -- 50020
      Metadata_Version           => 16#C365#,  -- 50021
      Edit_Tag_Array             => 16#C366#,  -- 50022
      Burst_Time_Lapse_Sequence  => 16#C367#,  -- 50023, also magnification?
      Digital_Resize_Factor          => 16#C368#,  -- 50024, if done in camera
      Burst_Time_Lapse_Sequence2     => 16#C369#,  -- 50025, or 50023?
      Cartridge_Hand_Of_Load         => 16#C36A#,  -- 50026
      Native_Focal_Plane_XResolution => 16#C36C#,  -- 50028
      Native_Focal_Plane_YResolution => 16#C36D#,  -- 50029
      Special_Effects                => 16#C36E#,  -- 50030
      Borders                        => 16#C36F#,  -- 50031
      Native_Focal_Plane_Resolution_Unit => 16#C37A#); -- 50029

   for Nikon_Tag'Size use 16;
   for Nikon_Tag use
     (Nikon_Tag_Version => 16#1#,
      ISO_Setting      => 16#2#,
      Color_Mode       => 16#3#, -- COLOR, B&W
      Quality          => 16#4#, -- NORMAL, FINE, BASIC
      White_Balance    => 16#5#, -- AUTO, WHITE PRESET, ...
      Image_Sharpening => 16#6#, -- AUTO, HIGH, ...
      Focus_Mode       => 16#7#, -- AF-S, AF-C
      Flash_Setting    => 16#8#, -- NORMAL, RED-EYE, ...
      ISO_Selection    => 16#f#, -- MANUAL, AUTO
      Image_Adjustment => 16#80#, -- AUTO, NORMAL, CONTRAST(+), ...
      Adapter          => 16#82#, -- OFF, FISHEYE 2, WIDE ADAPTER, ...
      Lens             => 16#84#, -- For example: 28.0 105.0 3.5 4.5
      Manual_Focus_Distance => 16#85#,  -- Distance in Meters when focus is MF
      Digital_Zoom          => 16#86#,  -- 1.0 if no digital zoom
      Auto_Focus_Position   => 16#88#); -- (0,0,0,0) => Center,
                                        -- (0,1,0,0) => Top,
                                        -- (0,2,0,0) => Bottom,
                                        -- (0,3,0,0) => Left,
                                        -- (0,4,0,0) => Right

   for Nikon_Old_Tag'Size use 16;
   for Nikon_Old_Tag use
     (Nikon_Tag_Version  => 16#2#,
      Quality            => 16#3#,
      Color_Mode         => 16#4#,
      Image_Adjustment   => 16#5#,
      CCD_Sensitivity    => 16#6#,
      White_Balance      => 16#7#,
      Focus              => 16#8#,
      Digital_Zoom       => 16#A#,
      Converter          => 16#B#);

   for Olympus_Tag'Size use 16;
   for Olympus_Tag use
     (Special_Mode     => 16#0200#,
      Quality          => 16#0201#,
      Macro            => 16#0202#,
      Digital_Zoom     => 16#0204#,
      Firmware_Version => 16#0207#,
      Picture_Info     => 16#0208#,
      Camera_ID        => 16#0209#);

   for Photoshop_Tag'Size use 16;
   for Photoshop_Tag use
     (Print_Info                 =>  1001,
      Resolution                 =>  1005,
      Alpha_Channel_Names        =>  1006,
      Alpha_Channel_Settings     =>  1007,
      Print_Flags                =>  1011,
      Monochrome_Halftone_Settings => 1012,
      Color_Halftone_Settings    =>  1013,
      Monochrome_Transfer_Settings => 1015,
      Color_Transfer_Settings    =>  1016,
      Layer_State                =>  1024,
      Layer_Groups               =>  1026,
      Caption                    =>  1028, -- IPTC-NAA
      JPEG_Quality               =>  1030,
      Guides                     =>  1032,
      Copyright_Flag             =>  1034,
      New_Windows_Thumbnail      =>  1036,
      FX_Global_Lighting_Angle   =>  1037,
      ICC_Profile                =>  1039,
      ICC_Untagged_Flag          =>  1041,
      Layer_ID_Generator_Base    =>  1044,
      Alpha_Channel_Unicode_Names => 1045,
      FX_Global_Altitude         =>  1049,
      Slices                     =>  1050,
      Alpha_Channel_Identifiers  =>  1053,
      URL_Overrides              =>  1054,
      Version_Compatibility_Info =>  1057,
      EXIF                       =>  1058,
      EXIF2                      =>  1059,
      Japanese_Print_Flags       => 10000
      );

   for IFD_Datatype use
      (Byte      =>  1,  --  8-bit unsigned integer
       Ascii     =>  2,  --  8-bit byte that contains a 7-bit ASCII code;
                         --  the last byte shall be NUL (binary zero)
       Short     =>  3,  --  16-bit (2-byte) unsigned integer
       Long      =>  4,  --  32-bit (4-byte) unsigned integer
       Rational  =>  5,  --  Two Long's: the first represents the numerator
                         --  of a fraction; the second, the denominator
       SByte     =>  6,  --  An 8-bit signed (twos-complement) integer
       Undefined =>  7,  --  An 8-bit byte that may contain anything,
                         --  depending on the definition of the field
       SShort    =>  8,  --  A 16-bit (2-byte) signed (twos-complement) integer
       SLong     =>  9,  --  A 32-bit (4-byte) signed (twos-complement) integer
       SRational => 10,  --  Two SLong's: the first represents the numerator
                         --  of the fraction, the second the denominator
       Floating  => 11,  --  Single precision (4-byte) IEEE format
       Double    => 12); --  Double precision (8-byte) IEEE format

   for IFD_Datatype'Size use 16;

end Exif;

