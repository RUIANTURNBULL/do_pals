-- Name: Ian Turnbull
-- Date: March 23, 2017
-- Course: ITEC 320 Principles of Procedural Programming
-- Section: 01
-- Assignment: Do_Pals
--
-- Purpose: a program that reads a series of strings, one string per line of input, and
-- prints Each String IN Exactly One OF The Three Different Groups Given Below. The
-- strings Are To Be Tested AND Pritned WITH ALL Non-Letters Removed AND IN Alphabetical
-- order within each group. The groups are as follow:
--    Palindrome as is.
--    Palindrome when converted to upper case.
--    Not a palindrome.
-- each group should be labeled and printed.
-- when checking the palindrome status, any character that is not one of the 52 upper
-- and lower case characters should be ignored.
-- When sorting your strings, you should use the built-in definition of less than (ie <)
-- for type Character. Sorting should be done in dictionary order.
--
-- Help received: I used material for sorting from www.adapower.com
PRAGMA Ada_2012;
WITH Ada.Text_IO;
USE Ada.Text_IO;
WITH Ada.Characters.Handling;
USE Ada.Characters.Handling;

PROCEDURE Do_Pals IS

   TYPE Palindrome IS --distinguish b/w palindromes

         (IsPal,
          NotPal,
          UppPal,
          Unk);

   TYPE Line IS --contains a line from file

      RECORD
         StrLine : String (1 .. 80);
         Len     : Natural;
         Pal     : Palindrome;
      END RECORD;

   TYPE StrArray IS ARRAY (Natural RANGE <>) OF Line;
   TYPE PalArray IS ARRAY (Natural RANGE <>) OF Line;
   TYPE NotArray IS ARRAY (Natural RANGE <>) OF Line;
   TYPE UppArray IS ARRAY (Natural RANGE <>) OF Line;

   ----------------------------------------------------------------------
   -- Swap: swaps two different records of Line
   ----------------------------------------------------------------------
   PROCEDURE Swap (
         X,
         Y : IN OUT Line) IS
      Temp : Line;
   BEGIN
      Temp:= X;
      X:= Y;
      Y:= Temp;
   END Swap;

   ----------------------------------------------------------------------
   -- Add2UppArr: receives an array containing all Lines and returns a
   -- new array of Line that only contains uppercase palindromes
   ----------------------------------------------------------------------
   FUNCTION Add2UppArr (
         LA : StrArray;
         TL : Natural)
     RETURN UppArray IS
      UppArr      : UppArray (1 .. 100);
      LineArr     : StrArray (1 .. 100);
      Total_Lines,
      Pos,
      Min         : Natural;
   BEGIN
      LineArr:= LA;
      Total_Lines:= TL;
      Pos:= 0;

      FOR I IN 1 .. Total_Lines LOOP --adds lines to array
         IF LineArr(I).Pal = UppPal THEN
            Pos:= Pos + 1;
            UppArr(Pos):= LineArr(I);
         END IF;
      END LOOP;

      FOR I IN 1 .. Pos-1 LOOP --sorts array by "<"
         Min := I;
         FOR J IN I+1 .. Pos LOOP
            IF UppArr(J).StrLine < UppArr(Min).StrLine THEN
               Min := J;
            END IF;
         END LOOP;
         IF Min /= I THEN
            Swap(UppArr(I),UppArr(Min)); --swap if not a duplicate
         END IF;
      END LOOP;

      RETURN UppArr;

   END Add2UppArr;

   ----------------------------------------------------------------------
   -- Add2NotArr: receives an array containing all Lines and returns a
   -- new array of Line that are not palindromes
   ----------------------------------------------------------------------
   FUNCTION Add2NotArr (
         LA : StrArray;
         TL : Natural)
     RETURN NotArray IS
      NotArr      : NotArray (1 .. 100);
      LineArr     : StrArray (1 .. 100);
      Total_Lines,
      Pos,
      Min         : Natural;
   BEGIN
      LineArr:= LA;
      Total_Lines:= TL;
      Pos:= 0;

      FOR I IN 1 .. Total_Lines LOOP --adds lines to array
         IF LineArr(I).Pal = NotPal THEN
            Pos:= Pos + 1;
            NotArr(Pos):= LineArr(I);
         END IF;
      END LOOP;

      FOR I IN 1 .. Pos-1 LOOP --sorts array by "<"
         Min := I;
         FOR J IN I+1 .. Pos LOOP
            IF NotArr(J).StrLine < NotArr(Min).StrLine THEN
               Min := J;
            END IF;
         END LOOP;
         IF Min /= I THEN
            Swap(NotArr(I),NotArr(Min)); --swap if not a duplicate
         END IF;
      END LOOP;

      RETURN NotArr;

   END Add2NotArr;

   ----------------------------------------------------------------------
   -- Add2PalArr: receives an array containing all Lines and returns a
   -- new array of Line that only contains palindromes
   ----------------------------------------------------------------------
   FUNCTION Add2PalArr (
         LA : StrArray;
         TL : Natural)
     RETURN PalArray IS
      PalArr      : PalArray (1 .. 100);
      LineArr     : StrArray (1 .. 100);
      Total_Lines,
      Pos,
      Min         : Natural;
   BEGIN
      LineArr:= LA;
      Total_Lines:= TL;
      Pos:= 0;

      FOR I IN 1 .. Total_Lines LOOP --adds lines to array
         IF LineArr(I).Pal = IsPal THEN
            Pos:= Pos + 1;
            PalArr(Pos):= LineArr(I);
         END IF;
      END LOOP;

      FOR I IN 1 .. Pos-1 LOOP --sorts array by "<"
         Min := I;
         FOR J IN I+1 .. Pos LOOP
            IF PalArr(Min).Len = 0 THEN --if the string is empty then it equals
               Min:= J;                    --the min in array (only for palindromes)
            ELSIF PalArr(J).StrLine < PalArr(Min).StrLine THEN
               Min := J;
            END IF;
         END LOOP;
         IF Min /= I THEN
            Swap(PalArr(I),PalArr(Min)); --swap if not a duplicate
         END IF;
      END LOOP;

      RETURN PalArr;

   END Add2PalArr;

   ----------------------------------------------------------------------
   -- IsPal: receives an array containing all Lines and determines if
   -- the Line is a palindrome
   ----------------------------------------------------------------------
   PROCEDURE IsPal (
         LineArray   : IN OUT StrArray;
         Total_Lines :        Natural;
         Pal_Amt,
         NPal_Amt,
         UPal_Amt    : IN OUT Natural) IS
      Reg,
      Rev   : String (1 .. 80);
      Pos,
      Len,
      CkPal,
      CkUpp : Natural;
   BEGIN
      Pos:= 0; --for reversing the string
      CkPal:= 0; --counts characters that dont match

      FOR I IN 1 .. Total_Lines LOOP
         Reg:= LineArray(I).StrLine;
         Len:= LineArray(I).Len;

         FOR J IN REVERSE 1 .. Len LOOP --reverse the string
            Pos:= Pos +1;
            Rev(Pos):= Reg(J);
         END LOOP;

         CkUpp:= Len;

         FOR P IN 1 .. Len LOOP          --compares forward and reverse strings
            IF Reg(P) /= Rev(P) THEN
               CkPal:= CkPal + 1;
            END IF;
         END LOOP;

         FOR Q IN 1 .. Len LOOP -- compares forward and reverse uppercase stings
            IF To_Upper(Reg(Q)) = To_Upper(Rev(Q)) THEN
               CkUpp:= CkUpp - 1;
            END IF;
         END LOOP;

         IF CkPal = 0 THEN             --if CkPal = 0 then a palindrome
            LineArray(I).Pal:= IsPal;
            Pal_Amt:= Pal_Amt + 1;
         ELSIF CkUpp = 0 THEN          --if CkUpp = 0 then a uppercase palindrome
            LineArray(I).Pal:= UppPal;
            UPal_Amt:= UPal_Amt + 1;
         ELSE                          --if CkPal /= 0 then not a palindrome
            LineArray(I).Pal:= NotPal;
            NPal_Amt:= NPal_Amt + 1;
         END IF;

         CkPal:= 0;
         Pos:= 0;

      END LOOP;

   END IsPal;

   ----------------------------------------------------------------------
   -- RemoveOther: receives an array containing all Lines and removes
   -- any character that is not a letter
   ----------------------------------------------------------------------
   PROCEDURE RemoveOther (
         LineArray : IN OUT StrArray;
         Count     :        Natural) IS
      L     : Line;
      Pos,
      Len   : Natural;
      Temp,
      Temp2 : String (1 .. 80);
   BEGIN
      Pos:= 0;

      FOR I IN 1 .. Count LOOP
         Temp:= LineArray(I).StrLine;
         Len:= LineArray(I).Len;

         FOR J IN 1 .. Len LOOP --only adds letters back into the array
            IF Temp(J) IN 'a' .. 'z' | 'A' .. 'Z' THEN
               Pos:= Pos + 1; --new string length
               Temp2(Pos):= Temp(J);
            END IF;
         END LOOP;

         L.Len:= Pos;
         L.StrLine:= Temp2;
         LineArray(I):= L; --replaces current Line with new Line w/ only letters or empty
         Pos:= 0;

      END LOOP;

   END RemoveOther;

   ----------------------------------------------------------------------
   -- Control: receives an array containing all Lines and is responsible for
   -- controling the program and prints results
   ----------------------------------------------------------------------
   PROCEDURE Control (
         LA : StrArray;
         C  : Natural) IS
      PalArr    : PalArray (1 .. 100);
      NotArr    : NotArray (1 .. 100);
      UppArr    : UppArray (1 .. 100);
      LineArray : StrArray (1 .. 100);
      Count,
      Pal_Amt,
      NPal_Amt,
      UPal_Amt  : Natural;
   BEGIN
      LineArray:= LA;
      Count:= C;
      Pal_Amt:= 0;
      NPal_Amt:= 0;
      UPal_Amt:= 0;

      RemoveOther(LineArray, Count); --RemoveOther
      IsPal(LineArray, Count, Pal_Amt, NPal_Amt, UPal_Amt); --IsPal
      PalArr:= Add2PalArr(LineArray, Count); --PalArr
      UppArr:= Add2UppArr(LineArray, Count); --UppArr
      NotArr:= Add2NotArr(LineArray, Count); --NotArr

      Put_Line("Palindrome as is:"); --prints results

      FOR I IN 1 .. Pal_Amt LOOP
         Set_Col(4);
         Put("""");
         Put(PalArr(I).StrLine(1 .. PalArr(I).Len));
         Put("""");
         New_Line;
      END LOOP;

      New_Line;
      Put_Line("Palindrome when converted to upper case:");

      FOR I IN 1 .. UPal_Amt LOOP
         Set_Col(4);
         Put("""");
         Put(UppArr(I).StrLine(1 .. UppArr(I).Len));
         Put("""");
         New_Line;
      END LOOP;

      New_Line;
      Put_Line("Not a palindrome:");

      FOR I IN 1 .. NPal_Amt LOOP
         Set_Col(4);
         Put("""");
         Put(NotArr(I).StrLine(1 .. NotArr(I).Len));
         Put("""");
         New_Line;
      END LOOP;

   END Control;

   ----------------------------------------------------------------------
   -- Open: opens file and gets each line and inserts into an array of Line
   ----------------------------------------------------------------------
   PROCEDURE Open IS
      L         : Line;
      LineArray : StrArray (1 .. 100);
      File      : File_Type;
      Count     : Natural;
   BEGIN
      Count:= 0;

      Open(
         File => File,
         Mode => In_File,
         Name => "do_pals_lines.txt");

      WHILE(NOT End_Of_File(File)) LOOP --loops till end of file
         IF Count > 100 THEN
            RAISE Constraint_Error;
         END IF;
         Count:= Count + 1;
         Get_Line(File,L.StrLine,L.Len); --get_line
         IF L.Len > 60 THEN
            RAISE Data_Error;
         END IF;
         L.Pal:= Unk;
         LineArray(Count):= L; --new line is added to array
      END LOOP;

      Control(LineArray, Count); --Control

      Close (File);
   END Open;

   ----------------------------------------------------------------------

BEGIN
   Open; --Open
EXCEPTION
   WHEN Data_Error => --when a line is >60
      Put_Line("Line too long");
   WHEN Constraint_Error => --when amount of lines >100
      Put_Line("Too much input");

END Do_Pals;
