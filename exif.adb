
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Checked_Conversion;

package body Exif is
   use type System.Bit_Order;

   Max_Values : constant := 64;

   Tag_Descriptions : constant Tag_Description_Table :=
     (((TIFF_IFD, Compression), new Value_Description_Table'
       ((0, new String'("uncompressed")),
        (1, new String'("TIFF uncompressed")),
        (2, new String'("CCITT Group 3")),
        (3, new String'("CCITT T.4")),
        (4, new String'("CCITT T.6")),
        (5, new String'("LZW")),
        (6, new String'("JPEG")),
        (32773, new String'("TIFF PackBits")),
        (32895, new String'("IT8 uncompressed")),
        (32896, new String'("IT8 linework")))),
      ((TIFF_IFD, Extra_Samples), new Value_Description_Table'
       ((0, new String'("Unspecified data")),
        (1, new String'("Associated alpha data")),
        (2, new String'("Unassociated alpha data")))),
      ((TIFF_IFD, Fill_Order), new Value_Description_Table'
       ((1, new String'("Lower column value in higher order bit")),
        (2, new String'("Lower column value in lower order bit")))),
      ((TIFF_IFD, Gray_Response_Unit), new Value_Description_Table'
       ((1, new String'("Tenths of a unit")),
        (2, new String'("Hundredths of a unit")),
        (3, new String'("Thousands of a unit")),
        (4, new String'("Tenthousands of a unit")),
        (5, new String'("Hundredthousands of a unit")))),
      ((TIFF_IFD, New_Subfile_Type), new Value_Description_Table'
       ((1, new String'("Reduced resolution version of other image in " &
                        "document")),
        (2, new String'("Single page of multipage document")),
        (3, new String'("Single reduced resolution page of " &
                        "multipage document")),
        (4, new String'("Transparency mask for other image in document")),
        (5, new String'("Transparency mask for reduced resolution version " &
                        "of other image in document")),
        (6, new String'("Transparency mask for single page of multipage " &
                        "document")),
        (7, new String'("Transparency mask for single reduced resolution " &
                        "page of multipage document" )))),
      ((TIFF_IFD, Orientation), new Value_Description_Table'
       ((1, new String'("No rotation")),
        (2, new String'("Mirrored horizontally")),
        (3, new String'("Mirrored vertically")),
        (4, new String'("180 degrees rotated")),
        (5, new String'("Mirrored horizontally, rotated CCW")),
        (6, new String'("Rotated 90 degrees CW")),
        (7, new String'("Mirrored horizontally, rotated CW")),
        (8, new String'("Rotated 90 degrees CCW")))),
      ((TIFF_IFD, Photometric_Interpretation), new Value_Description_Table'
       ((0, new String'("White is zero")),
        (1, new String'("Black is zero")),
        (2, new String'("RGB")),
        (3, new String'("RGB Palette")),
        (4, new String'("Transparency mask")),
        (5, new String'("CMYK")),
        (6, new String'("YCbCr")),
        (8, new String'("CIE L*a*b*")), -- Normal 1976 CIE L*a*b encoding
        (9, new String'("ICCLab")))),   -- See Adobe PhotoShop TIFF tech notes
      ((TIFF_IFD, Planar_Configuration), new Value_Description_Table'
       ((1, new String'("Chunky format")),
        (2, new String'("Planar format")))),
      ((TIFF_IFD, Predictor), new Value_Description_Table'
       ((1, new String'("No prediction used")),
        (2, new String'("Horizontal differencing")))),
      ((TIFF_IFD, Resolution_Unit), new Value_Description_Table'
       ((2, new String'("Inches")),
        (3, new String'("Centimeters")))),
      ((TIFF_IFD, Ink_Set), new Value_Description_Table'
       ((1, new String'("CMYK")),
        (2, new String'("Multi-ink/hifi")))),
      ((TIFF_IFD, Indexed), new Value_Description_Table'
       ((0, new String'("Not indexed")),
        (1, new String'("Indexed")))),
      ((TIFF_IFD, OPI_Proxy), new Value_Description_Table'
       ((0, new String'("No higher-resolution version exists")),
        (1, new String'("Low-resolution proxy of high-resolution image")))),
      ((TIFF_IFD, Subfile_Type), new Value_Description_Table'
       ((1, new String'("Full-resolution image data")),
        (2, new String'("Reduced-resolution image data")),
        (3, new String'("Single page of a multipage document")))),
      ((TIFF_IFD, Thresholding), new Value_Description_Table'
       ((1, new String'("No dithering or halftoning has been applied")),
        (2, new String'("Ordered dither or halftoning has been applied")),
        (3, new String'("Randomized dither or halftoning has been applied")))),
      ((TIFF_IFD, Ycb_Cr_Positioning), new Value_Description_Table'
       ((1, new String'("Centered")),
        (2, new String'("Co-sited")))),

      ((Exif_IFD, Color_Space), new Value_Description_Table'
       ((1, new String'("sRGB")),
        (16#FFFF#, new String'("Uncalibrated")))),
      ((Exif_IFD, Exposure_Program), new Value_Description_Table'
       ((0, new String'("Not defined")),
        (1, new String'("Manual")),
        (2, new String'("Normal program")),
        (3, new String'("Aperture priority")),
        (4, new String'("Shutter priority")),
        (5, new String'("Creative program")),
        (6, new String'("Action program")),
        (7, new String'("Portrait program")),
        (8, new String'("Landscape program")))),
      ((Exif_IFD, File_Source), new Value_Description_Table'
       ((0, new String'("Other")),
        (1, new String'("Scanner, transparency")),
        (2, new String'("Scanner, reflexive")),
        (3, new String'("DSC")))),
      ((Exif_IFD, Flash), new Value_Description_Table'
       ((16#0000#, new String'("Flash not fired")),
        (16#0001#, new String'("Flash fired")),
        (16#0005#, new String'("Flash fired, strobe return not detected")),
        (16#0007#, new String'("Flash fired, strobe return detected")),
        (16#0009#, new String'("Flash fired, compulsory flash mode")),
        (16#000D#, new String'("Flash fired, compulsory flash mode, "
                             & "return light not detected")),
        (16#000F#, new String'("Flash fired, compulsory flash mode, "
                             & "return light detected")),
        (16#0010#, new String'("Flash did not fire, compulsory flash mode")),
        (16#0018#, new String'("Flash did not fire, auto mode")),
        (16#0019#, new String'("Flash fired, auto mode")),
        (16#001D#, new String'("Flash fired, auto mode, "
                             & "return light not detected")),
        (16#001F#, new String'("Flash fired, auto mode, "
                             & "return light detected")),
        (16#0020#, new String'("No flash function")),
        (16#0041#, new String'("Flash fired, red-eye reduction mode")),
        (16#0045#, new String'("Flash fired, red-eye reduction mode, "
                             & "return light not detected")),
        (16#0047#, new String'("Flash fired, red-eye reduction mode, "
                             & "return light detected")),
        (16#0049#, new String'("Flash fired, compulsory flash mode, "
                             & "red-eye reduction mode")),
        (16#004D#, new String'("Flash fired, compulsory flash mode, "
                             & "red-eye reduction mode, "
                             & "return light not detected")),
        (16#004F#, new String'("Flash fired, compulsory flash mode, "
                             & "red-eye reduction mode, "
                             & "return light detected")),
        (16#0059#, new String'("Flash fired, auto mode, "
                             & "red-eye reduction mode, ")),
        (16#005D#, new String'("Flash fired, auto mode, "
                             & "red-eye reduction mode, "
                             & "return light not detected")),
        (16#005F#, new String'("Flash fired, auto mode, "
                             & "red-eye reduction mode, "
                             & "return light detected")))),
      ((Exif_IFD, Focal_Plane_Resolution_Unit), new Value_Description_Table'
       ((2, new String'("Inches")),
        (3, new String'("Centimeters")))),
      ((Exif_IFD, Light_Source), new Value_Description_Table'
       ((0, new String'("unknown")),
        (1, new String'("Daylight")),
        (2, new String'("Fluorescent")),
        (3, new String'("Tungsten")),
        (4, new String'("Flash")),
        (9, new String'("Fine weather")),
        (10, new String'("Cloudy weather")),
        (11, new String'("Shade")),
        (12, new String'("Daylight fluorescent (D 5700 - 7100K)")),
        (13, new String'("Day white fluorescent (N 4600 - 5400K)")),
        (14, new String'("Cool white fluorescent (W 3900 -4500K)")),
        (15, new String'("White fluorescent (WW 3200 - 3700K)")),
        (17, new String'("Standard light A")),
        (18, new String'("Standard light B")),
        (19, new String'("Standard light C")),
        (20, new String'("D55")),
        (21, new String'("D65")),
        (22, new String'("D75")),
        (23, new String'("D50")),
        (24, new String'("ISO studio tungsten")),
        (255, new String'("other light source")))),
      ((Exif_IFD, Metering_Mode), new Value_Description_Table'
       ((1, new String'("Average")),
        (2, new String'("Center-weighted average")),
        (3, new String'("Spot")),
        (4, new String'("Multi-spot")),
        (5, new String'("Pattern")),
        (6, new String'("Partial")),
        (255, new String'("other")))),
      ((Exif_IFD, Scene_Type), new Value_Description_Table'
       (1 => (1, new String'("Directly photographed image")))),
      ((Exif_IFD, Sensing_Method), new Value_Description_Table'
       ((1, new String'("not defined")),
        (2, new String'("One-chip color area sensor")),
        (3, new String'("Two-chip color area sensor")),
        (4, new String'("Three-chip color area sensor")),
        (5, new String'("Color sequential area sensor")),
        (7, new String'("Trilinear sensor")),
        (8, new String'("Color sequential linear sensor")))),
      ((Exif_IFD, Custom_Rendered), new Value_Description_Table'
       ((0, new String'("Normal process")),
        (1, new String'("Custom process")))),
      ((Exif_IFD, Exposure_Mode), new Value_Description_Table'
       ((0, new String'("Auto exposure")),
        (1, new String'("Manual exposure")),
        (2, new String'("Auto bracket")))),
      ((Exif_IFD, White_Balance), new Value_Description_Table'
       ((0, new String'("Auto white balance")),
        (1, new String'("Manual white balance")))),
      ((Exif_IFD, Scene_Capture_Type), new Value_Description_Table'
       ((0, new String'("Standard")),
        (1, new String'("Landscape")),
        (2, new String'("Portrait")),
        (3, new String'("Night scene")))),
      ((Exif_IFD, Gain_Control), new Value_Description_Table'
       ((0, new String'("None")),
        (1, new String'("Low gain up")),
        (2, new String'("High gain up")),
        (3, new String'("Low gain down")),
        (4, new String'("High gain down")))),
      ((Exif_IFD, Contrast), new Value_Description_Table'
       ((0, new String'("Normal")),
        (1, new String'("Soft")),
        (2, new String'("Hard")))),
      ((Exif_IFD, Saturation), new Value_Description_Table'
       ((0, new String'("Normal")),
        (1, new String'("Low saturation")),
        (2, new String'("High saturation")))),
      ((Exif_IFD, Sharpness), new Value_Description_Table'
       ((0, new String'("Normal")),
        (1, new String'("Soft")),
        (2, new String'("Hard")))),
      ((Exif_IFD, Subject_Distance_Range), new Value_Description_Table'
       ((0, new String'("unknown")),
        (1, new String'("Macro")),
        (2, new String'("Close view")),
        (3, new String'("Distant view")))),
      ((Kodak_IFD, Film_Category), new Value_Description_Table'
       ((0, new String'("Unidentified")),
        (1, new String'("B&W negative")),
        (2, new String'("Color negative")),
        (3, new String'("B&W reversal")),
        (4, new String'("Color reversal")))),

      ((Kodak_IFD, Film_Size), new Value_Description_Table'
       ((0, new String'("35 mm")),
        (1, new String'("APS")))),

      ((Kodak_IFD, Image_Rotation_Status), new Value_Description_Table'
       ((0, new String'("not rotated")),
        (1, new String'("rotated based on user input")),
        (2, new String'("rotated based on customer direction")),
        (3, new String'("rotated based on algorithm input")),
        (255, new String'("rotated for unknown reason")))),

      ((Kodak_IFD, Image_Source), new Value_Description_Table'
       ((0, new String'("Unidentified")),
        (1, new String'("Film scanner")),
        (2, new String'("Reflection print scanner")),
        (3, new String'("Digital camera")),
        (4, new String'("Still from video")),
        (5, new String'("Computer graphics")))),

      ((Kodak_IFD, Intended_Print_Area), new Value_Description_Table'
       ((0, new String'("Complete image")),
        (1, new String'("APS 16:9 (HDTV) aspect ratio")),
        (2, new String'("APS 3:1 (Panoramic) aspect ratio")),
        (3, new String'("APS 3:2 (Classic) aspect ratio")))),

      ((Nikon_IFD, Auto_Focus_Position), new Value_Description_Table'
       ((16#00_00_00_00#, new String'("Center")),
        (16#00_01_00_00#, new String'("Top")),
        (16#00_02_00_00#, new String'("Bottom")),
        (16#00_03_00_00#, new String'("Left")),
        (16#00_04_00_00#, new String'("Right")))),

      ((Nikon_Old_IFD, Quality), new Value_Description_Table'
       ((1, new String'("VGA Basic")),
        (2, new String'("VGA Normal")),
        (3, new String'("VGA Fine")),
        (4, new String'("SXGA Basic")),
        (5, new String'("SXGA Normal")),
        (6, new String'("SXGA Fine")))),
      ((Nikon_Old_IFD, Color_Mode), new Value_Description_Table'
       ((1, new String'("Color")),
        (2, new String'("Monochrome")))),
      ((Nikon_Old_IFD, Image_Adjustment), new Value_Description_Table'
       ((0, new String'("Normal")),
        (1, new String'("Bright+")),
        (2, new String'("Bright-")),
        (3, new String'("Contrast+")),
        (4, new String'("Contrast-")))),
      ((Nikon_Old_IFD, CCD_Sensitivity), new Value_Description_Table'
       ((0, new String'("ISO 80")),
        (2, new String'("ISO 160")),
        (4, new String'("ISO 320")),
        (5, new String'("ISO 100")))),
      ((Nikon_Old_IFD, White_Balance), new Value_Description_Table'
       ((0, new String'("Auto")),
        (1, new String'("Preset")),
        (2, new String'("Daylight")),
        (3, new String'("Incandescense")),
        (4, new String'("Fluorescense")),
        (5, new String'("Cloudy")),
        (6, new String'("SpeedLight")))),
      ((Nikon_Old_IFD, Converter), new Value_Description_Table'
       ((0, new String'("None")),
        (1, new String'("Fisheye"))))
      );

   function To_Tag is new Checked_Conversion (Unsigned_16, Exif_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, GPS_Tag);
   function To_Tag is new Checked_Conversion (Unsigned_16,
                                                         Interoperability_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Canon_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Canon_Raw_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Casio_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Flashpix_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Fujifilm_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Kodak_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Nikon_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Nikon_Old_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Olympus_Tag);
   function To_Tag  is new Checked_Conversion (Unsigned_16, Photoshop_Tag);

   function Convert is new Checked_Conversion (Unsigned_16, IFD_Datatype);

   --------------
   -- Describe --
   --------------

   function Describe
     (Tag      : IFD_Tag;
      Datatype : IFD_Datatype;
      Value    : Unsigned_32)
      return Text
   is
   begin
      for T in Tag_Descriptions'Range loop
         if Tag_Descriptions (T).Tag = Tag then
            declare
               Description : constant Tag_Description := Tag_Descriptions (T);
            begin
               for V in Description.Values'Range loop
                  if Description.Values (V).Value = Value then
                     return Description.Values (V).Description;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      case Tag.Kind is
         when Primary_Image_IFD | Thumbnail_IFD | Exif_IFD =>
            return Describe
             ((Kind => TIFF_IFD, TIFF => Tag.TIFF), Datatype,Value);

         when others => return null;
      end case;
   end Describe;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (Datatype : IFD_Datatype) return Storage_Offset is
   begin
      case Datatype is
         when Byte | SByte | Ascii | Undefined => return 1;
         when Short | SShort                   => return 2;
         when Long | SLong | Floating          => return 4;
         when Rational | SRational | Double    => return 8;
      end case;
   end Storage_Size;

   -----------------
   -- To_Datatype --
   -----------------

   function To_Datatype (Item : File_16) return IFD_Datatype is
   begin
      return Convert (Value (Item));
   end To_Datatype;

   -----------
   -- Image --
   -----------

   function Image (Item : Fraction) return String is
      Result : String := Item.Numerator'Img & Item.Denominator'Img;
   begin
      if Item.Numerator = 0 or Item.Denominator = 1 then
         if Item.Denominator > 1 then
            return " 0.0";
         else
            return Item.Numerator'Img & ".0";
         end if;
      end if;

      if Item.Numerator rem 10 = 0 and Item.Denominator rem 10 = 0 then
         return Image (Fraction'(Item.Numerator / 10, Item.Denominator / 10));
      end if;

      if Item.Denominator = 10 and Item.Numerator /= 1 then
         for N in Result'First + 2 .. Result'Last loop
            if Result (N) = ' ' then
               Result (N) := Result (N - 1);
               Result (N - 1) := '.';
               return Result (1 .. N);
            end if;
         end loop;

      elsif Item.Denominator = 100 and Item.Numerator /= 1 then
         for N in Result'First + 3 .. Result'Last loop
            if Result (N) = ' ' then
               Result (N)     := Result (N - 1);
               Result (N - 1) := Result (N - 2);
               Result (N - 2) := '.';
               return Result (1 .. N);
            end if;
         end loop;

      elsif Item.Denominator = 1000 and Item.Numerator /= 1 then
         for N in Result'First + 4 .. Result'Last loop
            if Result (N) = ' ' then
               Result (N - 2 .. N) := Result (N - 3 .. N - 1);
               Result (N - 3) := '.';
               return Result (1 .. N);
            end if;
         end loop;
      end if;

      if Item.Numerator > Item.Denominator then
         declare
            Whole : String   := Unsigned_32'Image
                                   (Item.Numerator / Item.Denominator);
            Rest  : Fraction := (Item.Numerator rem Item.Denominator,
                                 Item.Denominator);
         begin
            if Rest.Numerator = 0 then
               return Whole & ".0";

            else
               return Whole & Image (Rest);
            end if;
         end;
      end if;

      if Item.Denominator > 0 then
         for N in reverse Result'Range loop
            if Result (N) = ' ' then
               Result (N) := '/';
               return Result;
            end if;
         end loop;

      else
         return "Infinity";
      end if;

      raise Constraint_Error;
    end Image;

    function Image (Item : SFraction) return String is
       Result : constant String := Image (Fraction'
          (Unsigned_32 (abs Item.Numerator),
          Unsigned_32 (abs Item.Denominator)));
    begin
       if Item.Numerator < 0 xor Item.Denominator < 0 then
          return " -" & Result (2 .. Result'Length);

       else
          return Result;
       end if;
    end Image;

    ----------------
    -- To_IFD_Tag --
    ----------------

   function To_IFD_Tag (Kind : IFD_Kind; Value : Unsigned_16) return IFD_Tag is
      Result : IFD_Tag (Kind);
   begin
      case Kind is
         when Unknown_IFD =>
            Result.Value := Value;

         when Interoperability_IFD =>
            Result.Interoperability := To_Tag (Value);

         when TIFF_IFD | Primary_Image_IFD | Thumbnail_IFD | Exif_IFD =>
            Result.TIFF := To_Tag (Value);

         when GPS_IFD =>
            Result.GPS := To_Tag (Value);

         when Canon_IFD =>
            Result.Canon := To_Tag (Value);

         when Canon_Raw_IFD =>
            Result.Canon_Raw := To_Tag (Value);

         when Casio_IFD =>
            Result.Casio := To_Tag (Value);

         when Fujifilm_IFD =>
            Result.Fujifilm := To_Tag (Value);

         when Kodak_IFD =>
            Result.Kodak := To_Tag (Value);

         when Nikon_IFD =>
            Result.Nikon := To_Tag (Value);

         when Nikon_Old_IFD =>
            Result.Nikon_Old := To_Tag (Value);

         when Olympus_IFD =>
            Result.Olympus := To_Tag (Value);

         when Photoshop_IFD =>
            Result.Photoshop := To_Tag (Value);

         when others =>
            return (Unknown_IFD, Value);

      end case;
      return Result;

   exception
      when Constraint_Error => return (Unknown_IFD, Value);
   end To_IFD_Tag;

   -----------
   -- Write --
   -----------

   procedure Write (Stream : Stream_IO.Stream_Access;
                    Data : Unsigned_32; Tag : IFD_Tag) is
   begin
      for T in Tag_Descriptions'Range loop
         if Tag_Descriptions (T).Tag = Tag then
            declare
               Description : Tag_Description := Tag_Descriptions (T);
            begin
               for V in Description.Values'Range loop
                  if Description.Values (V).Value = Data then
                     Character'Write (Stream, ' ');
                     String'Write (Stream,
                        Description.Values (V).Description.all);
                     return;
                  end if;
               end loop;
            end;
         end if;
      end loop;
      String'Write (Stream, Data'Img);
   end Write;

   procedure Write (Stream : Stream_Access; Data : Integer_32) is
   begin
      Character'Write (Stream, ' ');
      String'Write (Stream, Data'Img);
   end Write;

   procedure Write (Stream : Stream_Access; Data : Integer_16) is
   begin
      Character'Write (Stream, ' ');
      Write (Stream, Integer_32 (Data));
   end Write;

   procedure Write (Stream : Stream_Access; Data : Unsigned_16) is
   begin
      Write (Stream, Unsigned_32 (Data));
   end Write;

   procedure Write (Stream : Stream_Access; Data : Unsigned_32) is
   begin
      String'Write (Stream, Data'Img);
   end Write;

   procedure Write (Stream : Stream_Access; Data : Fraction) is
   begin
      String'Write (Stream, Image (Data));
   end Write;

   procedure Write (Stream : Stream_Access; Data : SFraction) is
   begin
      String'Write (Stream, Image (Data));
   end Write;

   procedure Write (Stream : Stream_Access; Data : Unsigned_16; Tag : IFD_Tag) is
   begin
      Write (Stream, Unsigned_32 (Data), Tag);
   end Write;

   procedure Write (Stream : Stream_Access; Tag : IFD_Tag) is
      Kind     : constant IFD_Kind := Tag.Kind;
      Kind_Img : String renames IFD_Kind'Image (Kind);
   begin
      String'Write (Stream, Kind_Img (1 .. Kind_Img'Length - 4));
      Character'Write (Stream, '.');
      case Kind is
         when Unknown_IFD =>
            declare
               Img : constant String := Tag.Value'Img;
            begin
               String'Write (Stream, Img (2 .. Img'Length));
            end;

         when GPS_IFD =>
            String'Write (Stream, Tag.GPS'Img);

         when Interoperability_IFD =>
            String'Write (Stream, Tag.Interoperability'Img);

         when TIFF_IFD | Primary_Image_IFD | Thumbnail_IFD | Exif_IFD =>
            String'Write (Stream, Tag.Tiff'Img);

         when Canon_IFD =>
            String'Write (Stream, Tag.Canon'Img);

         when Canon_Raw_IFD =>
            String'Write (Stream, Tag.Canon_Raw'Img);

         when Casio_IFD =>
            String'Write (Stream, Tag.Casio'Img);

         when Flashpix_IFD =>
            String'Write (Stream, Tag.Flashpix'Img);

         when Fujifilm_IFD =>
            String'Write (Stream, Tag.Fujifilm'Img);

         when Kodak_IFD =>
            String'Write (Stream, Tag.Kodak'Img);

         when Nikon_IFD =>
            String'Write (Stream, Tag.Nikon'Img);

         when Nikon_Old_IFD =>
            String'Write (Stream, Tag.Nikon_Old'Img);

         when Olympus_IFD =>
            String'Write (Stream, Tag.Olympus'Img);

         when Photoshop_IFD =>
            String'Write (Stream, Tag.Photoshop'Img);
      end case;
   end Write;

   procedure Write (Stream : Stream_IO.Stream_Access;
                    Datatype : IFD_Datatype;
                    Data     : Storage)
   is
      Datasize : constant Storage_Offset := Storage_Size (Datatype);
      Values   : constant Storage_Offset := Data'Length / Datasize;
      Pr_Vals  : Storage_OFfset := Storage_Offset'Min (Values, Max_Values);
      Ptr      : Storage_Offset := Data'First;
   begin
      if Always_Show_Type then
         String'Write (Stream, Datatype'Img);
      end if;

      case Datatype is
         when Ascii =>
            String'Write (Stream, " """);
            for C in 1 .. Pr_Vals loop
               exit when Data (Ptr) = 0;
               Character'Write (Stream, Character'Val (Data (Ptr)));
               Ptr := Ptr + Datasize;
            end loop;
            String'Write (Stream, """");

         when SShort =>
            for C in 1 .. Pr_Vals loop
               Write (Stream, Integer_16'(Value (Data (Ptr .. Ptr + Datasize - 1))));
               Ptr := Ptr + Datasize;
            end loop;

         when Short =>
            for C in 1 .. Pr_Vals loop
               Write (Stream, Unsigned_16'(Value (Data (Ptr .. Ptr + Datasize - 1))));
               Ptr := Ptr + Datasize;
            end loop;

         when SLong =>
            for C in 1 .. Pr_Vals loop
               Write (Stream, Integer_32'(Value (Data (Ptr .. Ptr + Datasize - 1))));
               Ptr := Ptr + Datasize;
            end loop;

         when Long =>
            for C in 1 .. Pr_Vals loop
               Write (Stream, Unsigned_32'(Value (Data (Ptr .. Ptr + Datasize - 1))));
               Ptr := Ptr + Datasize;
            end loop;

         when Rational =>
            for N in 1 .. Pr_Vals loop
               Write (Stream, Fraction'(Value (Data (Ptr .. Ptr + Datasize - 1))));
               Ptr := Ptr + Datasize;
            end loop;

        when SRational =>
            for N in 1 .. Pr_Vals loop
               Write (Stream, SFraction'(Value (Data (Ptr .. Ptr + Datasize - 1))));
               Ptr := Ptr + Datasize;
            end loop;

         when Undefined | Byte =>
            if not Always_Show_Type then
               --  If datatype not shown, show it anyway for these types
               Character'Write (Stream, ' ');
               String'Write (Stream, Datatype'Img);
            end if;

            for C in 1 .. Pr_Vals loop
               String'Write (Stream, Data (Ptr)'Img);
               declare
                  Ch : constant Character := Character'Val (Data (Ptr));
               begin
                  if Is_Graphic (Ch) then
                     String'Write (Stream, " '" & Ch & "'");
                  end if;
               end;
               Ptr := Ptr + 1;
            end loop;


         when others =>
            Pr_Vals := Values;
            String'Write (Stream, Datatype'Img);
            String'Write (Stream, " * ");
            String'Write (Stream, Values'Img);

      end case;

      if Values > Pr_Vals then
         String'Write (Stream, " ...");
      end if;
   end Write;

   -----------
   -- Value --
   -----------

   function Value (Item : File_8) return Unsigned_8 is
   begin
      return Unsigned_8 (Item);
   end Value;

   function Value (Item : File_16) return Unsigned_16 is
      Offset : constant Storage_Offset := Item'First;
   begin
      if File_Bit_Order = System.High_Order_First then
         return Stream_Element'Pos (Item (Offset)) * 2**8
            + Stream_Element'Pos (Item (Offset + 1));
      else
         return Stream_Element'Pos (Item (Offset + 1)) * 2**8
            + Stream_Element'Pos (Item (Offset));
      end if;
   end Value;

   function Value (Item : File_16) return Integer_16 is
      U : constant Unsigned_16 := Value (Item);
   begin
      if U > 16#8000# then
         return -Integer_16(not U) - 1;
      end if;

      return Integer_16 (U);
   end Value;

   function Value (Item : File_32) return Unsigned_32 is
      Offset : constant Storage_Offset := Item'First;
   begin
      if File_Bit_Order = System.High_Order_First then
         return Stream_Element'Pos (Item (Offset + 0)) * 2**24
              + Stream_Element'Pos (Item (Offset + 1)) * 2**16
              + Stream_Element'Pos (Item (Offset + 2)) * 2**8
              + Stream_Element'Pos (Item (Offset + 3));
      else
         return Stream_Element'Pos (Item (Offset + 3)) * 2**24
              + Stream_Element'Pos (Item (Offset + 2)) * 2**16
              + Stream_Element'Pos (Item (Offset + 1)) * 2**8
              + Stream_Element'Pos (Item (Offset + 0));
      end if;
   end Value;

   function Value (Item : File_32) return Integer_32 is
      U : constant Unsigned_32 := Value (Item);
   begin
      if U > 16#8000_0000# then
         return -Integer_32(not U) - 1;
      end if;

      return Integer_32 (U);
   end Value;

   function Value (Item : File_64) return Fraction is
      Offset : constant Storage_Offset := Item'First;
   begin
      return Fraction'(Numerator => Value (Item (Offset + 0 .. Offset + 3)),
                     Denominator => Value (Item (Offset + 4 .. Offset + 7)));
   end Value;

   function Value (Item : File_64) return SFraction is
      Offset : constant Storage_Offset := Item'First;
   begin
      return SFraction'(Numerator => Value (Item (Offset + 0 .. Offset + 3)),
                      Denominator => Value (Item (Offset + 4 .. Offset + 7)));
   end Value;

   function Value (Item : IFD_Tag) return Unsigned_16 is
   begin
      case Item.Kind is
         when Unknown_IFD =>
            return Item.Value;

         when Interoperability_IFD =>
            return Interoperability_Tag'Enum_Rep (Item.Interoperability);

         when TIFF_IFD | Primary_Image_IFD | Thumbnail_IFD | Exif_IFD =>
            return Exif_Tag'Enum_Rep(Item.TIFF);

         when GPS_IFD =>
            return GPS_Tag'Enum_Rep (Item.GPS);

         when Canon_IFD =>
            return Canon_Tag'Enum_Rep (Item.Canon);

         when Canon_Raw_IFD =>
            return Canon_Raw_Tag'Enum_Rep (Item.Canon_Raw);

         when Casio_IFD =>
            return Casio_Tag'Enum_Rep (Item.Casio);

         when Flashpix_IFD =>
            return Flashpix_Tag'Enum_Rep (Item.Flashpix);

         when Fujifilm_IFD =>
            return Fujifilm_Tag'Enum_Rep (Item.Fujifilm);

         when Kodak_IFD =>
            return Kodak_Tag'Enum_Rep (Item.Kodak);

         when Nikon_IFD =>
            return Nikon_Tag'Enum_Rep (Item.Nikon);

         when Nikon_Old_IFD =>
            return Nikon_Old_Tag'Enum_Rep (Item.Nikon_Old);

         when Olympus_IFD =>
            return Olympus_Tag'Enum_Rep (Item.Olympus);

         when Photoshop_IFD =>
            return Photoshop_Tag'Enum_Rep (Item.Photoshop);
      end case;
   end Value;

end Exif;
