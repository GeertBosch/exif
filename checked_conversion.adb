with Ada.Unchecked_Conversion;
function Checked_Conversion (Item : Source) return Target is
   function Convert is new Ada.Unchecked_Conversion (Source, Target);

   Result : constant Target := Convert (Item);
begin
   if Source'Size /= Target'Size or else not Result'Valid then
      raise Constraint_Error;
   end if;

   return Result;

end Checked_Conversion;
