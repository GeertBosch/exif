package Exif.Store is

   type Image_Id is range 0 .. 9_999_999;

   No_Image   : constant Image_Id;

   function New_Image (Name : String) return Image_Id;

   generic
      with procedure Action (Image : Image_Id);
   procedure Iterate;

   function File_Name (Image : Image_Id) return String;

   function Name (Image : Image_Id) return String;

   function Get_Data (Image : Image_Id) return Storage_Access;

   function Get_Thumbnail (Image : Image_ID) return Storage_Access;

   function Valid (Image : Image_Id) return Boolean;

   type Info_Key is record
      Image : Image_Id;
      Tag   : IFD_Tag;
   end record;

   type Info_Element
     (Bit_Order : System.Bit_Order := System.High_Order_First) is
   record
      Datatype : IFD_Datatype   := Undefined;
      Data     : Storage_Access := null;
   end record;

   No_Info : constant Info_Element
              := (System.High_Order_First, Undefined, null);

   procedure Set (K : Info_Key; E : Info_Element);
   procedure Set (K : Info_Key; Value : Unsigned_32);
   --  Associates a piece of information with a given image and tag.
   --  Overrides any previously associated info.


   procedure Set_Data (Image : Image_ID; Data : Storage_Access);
   --  Associates a data segment with the image

   procedure Reset;
   --  Removes and frees all info in the store

   function Get (K : Info_Key) return Info_Element;
   --  Returns the Element associated with a key or No_Element if the
   --  given key has not associated element

   function Get (K : Info_Key) return Text;
   --  Returns the text associated with a key or No_Element if the
   --  given key has no associated text.

   procedure Remove (K : Info_Key);
   --  Removes the latest inserted info element associated with the
   --  given key if any, does nothing if none.

   function Get_First return Info_Element;
   --  Returns No_Element if the store is empty, otherwise returns one
   --  non specified element. There is no guarantee that 2 calls to this
   --  function will return the same element.

   function Get_Next return Info_Element;
   --  Returns a non-specified element that has not been returned by the
   --  same function since the last call to Get_First or No_Element if
   --  there is no such element. If there is no call to 'Set' in between
   --  Get_Next calls, all the elements of the store will be traversed.

   function Value (Info : Info_Element) return Unsigned_32;

private

   No_Image : constant Image_Id := 0;
end Exif.Store;
