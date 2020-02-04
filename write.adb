with Ada.Containers.Functional_Maps;

procedure Write with SPARK_Mode is

   Error_State : Integer := 0;

   ADA_EINTR : constant Integer := 4;

   subtype Str is String
     with Predicate => Str'First = 1;

   function My_Eq (A, B : Str) return Boolean is
     (A'Last = B'Last and then
        (for all I in A'Range => A (I) = B (I)));

   package M is new Ada.Containers.Functional_Maps
     (Key_Type => Natural,
      Element_Type => Str,
      Equivalent_Keys => "=",
      "=" => My_Eq,
      Enable_Handling_Of_Equivalence => True);

   use M;

   function Get_Errno return Integer is (Error_State) with Global => Error_State;

   Contents : M.Map with Ghost;

   procedure My_Write (Fd : Natural; S : String; Has_Written : out Integer)
   with Global => (In_Out => (Error_State, Contents)),
     Post =>
       (case Has_Written is
          when -1                =>
            Contents = Contents'Old
              and then (if Get_Errno = ADA_EINTR then Has_Key (Contents, Fd)),
          when 0                 =>
            Contents = Contents'Old
              and then Has_Key (Contents, Fd),
          when 1 .. Integer'Last =>
            Has_Written <= S'Length
              and then
            Has_Key (Contents, Fd)
              and then
            M.Same_Keys (Contents, Contents'Old)
              and then
            Get (Contents, Fd)
            = Get (Contents'Old, Fd) & S (S'First .. S'First + Has_Written - 1)
        and then
            M.Elements_Equal_Except (Contents,
                                     Contents'Old,
                                     Fd),
          when others            => False);

   pragma Import (C, My_Write, "mywrite");

   procedure Safe_Write (Fd : Natural; S : String; Has_Written : out Integer)
     with Post =>
       (case Has_Written is
          when -1                =>
            Contents = Contents'Old and then Get_Errno /= ADA_EINTR,
          when 0                 =>
            Contents = Contents'Old
              and then Has_Key (Contents, Fd),
          when 1 .. Integer'Last =>
            Has_Written <= S'Length
              and then
            Has_Key (Contents, Fd)
              and then
            M.Same_Keys (Contents, Contents'Old)
              and then
            Get (Contents, Fd)
            = Get (Contents'Old, Fd) & S (S'First .. S'First + Has_Written - 1)
        and then
            M.Elements_Equal_Except (Contents,
                                     Contents'Old,
                                     Fd),
          when others            => False);


   procedure Safe_Write (Fd : Natural; S : String; Has_Written : out Integer) is
      Contents_Start : M.Map := Contents with Ghost;
   begin
      loop
         pragma Loop_Invariant (Contents = Contents_Start);
         My_Write (Fd, S, Has_Written);
         exit when (Has_Written < 0 and then Get_Errno /= ADA_EINTR)
                      or else
                    Has_Written >= 0;
      end loop;
   end Safe_Write;
begin
   null;

end Write;


