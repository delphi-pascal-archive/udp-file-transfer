unit UnitVerif;

interface

uses Windows,SysUtils;

type
  TMD5Buffer  = array[0..3]  of Cardinal;  //size: 16  (4*4)
  TMD5DblWord = array[0..1]  of Cardinal;
  TMD5Cvn     = array[0..15] of Cardinal;
  TMD5Data    = array[0..15] of Byte;     // size 16  (1*16)

  TMD5Context = record
    Buf   : TMD5Buffer;
    Bytes : TMD5DblWord;
    Cvn   : TMD5Cvn;
  end;

// !i!i!i!i!i!i!i!i! Pour les fichier !i!i!i!i!i!i!i!i!i!i!//

// Convertis une chaine de caracteres representant un MD5 vers une donn?e MD5
function MD5StrToMD5Data(const S : String) : TMD5Data;
// Verifie la validit?e d'une chaine representant un MD5
function MD5StrCheck(const S : string) : boolean;
// Renvois une donn?e MD5 d'un fichier
function MD5DataFromFile(const FileName: string): TMD5Data;
// Compare deux donn?es MD5 grace a CompareMem
function MD5MemEqual(const A, B : TMD5Data) : boolean;
procedure MD5CoreInitialize(var ctx: TMD5Context);
procedure MD5CoreUpdate(var ctx: TMD5Context; const buffer; len: Cardinal);
function  MD5CoreFinalize(var ctx: TMD5Context): TMD5Data;
procedure MD5CoreTransform(var buf: TMD5Buffer; const cvn: TMD5Cvn);

// !i!i!i!i!i!i!i!i!i! Pour du texte i!!i!i!i!i!i!i!i!i!i!i!//

// Renvois la representation du MD5 d'une chaine de caracteres
function MD5(const S : string) : string; overload;
// Renvois la representation du MD5 d'un buffer quelquonque
function MD5(const Buffer; const Len: integer): string; overload;

// Convertis une donn?e MD5 vers une chaine de caracteres
function MD5DataToStr(const Data : TMD5Data) : string;
// Renvois une donn?e MD5 d'un buffer quelquonque
function MD5DataFromBuffer(const Buffer; const Len: integer) : TMD5Data;


const
  FileNameList = 'list.lfc';
  
  MD5_INITBUF : TMD5Buffer  = (
     $67452301,$EFCDAB89,$98BADCFE,$10325476
  );

  MD5_INITDWD : TMD5DblWord = (
     $00000000,$00000000
  );

  MD5_TRSF    : array[0..63] of cardinal = (
     $D76AA478,$E8C7B756,$242070DB,$C1BDCEEE,$F57C0FAF,$4787C62A,$A8304613,$FD469501,
     $698098D8,$8B44F7AF,$FFFF5BB1,$895CD7BE,$6B901122,$FD987193,$A679438E,$49B40821,
     $F61E2562,$C040B340,$265E5A51,$E9B6C7AA,$D62F105D,$02441453,$D8A1E681,$E7D3FBC8,
     $21E1CDE6,$C33707D6,$F4D50D87,$455A14ED,$A9E3E905,$FCEFA3F8,$676F02D9,$8D2A4C8A,
     $FFFA3942,$8771F681,$6D9D6122,$FDE5380C,$A4BEEA44,$4BDECFA9,$F6BB4B60,$BEBFBC70,
     $289B7EC6,$EAA127FA,$D4EF3085,$04881D05,$D9D4D039,$e6DB99E5,$1fA27CF8,$C4AC5665,
     $F4292244,$432AFF97,$AB9423A7,$FC93A039,$655B59C3,$8F0CCC92,$FFEFF47D,$85845DD1,
     $6FA87E4F,$FE2CE6E0,$A3014314,$4E0811A1,$F7537E82,$BD3AF235,$2AD7D2BB,$EB86D391
  );
implementation


function MD5StrToMD5Data(const S : String) : TMD5Data;
var
  N,SP : integer;
begin
  if (Length(s) <> 32) or (not MD5StrCheck(S)) then
     exit;
  for N := 0 to 15 do begin
      SP := (N shl 1)+1;
      Result[N] := byte(StrToInt('$'+S[SP]+S[SP+1]));
  end;
end;

function MD5StrCheck(const S : string) : boolean;
var N,L : integer;
begin
  N      := 1;
  L      := Length(S)+1;
  result := L = 33;
  while Result and (N < L) do begin
     Result := S[N] in ['0'..'9','a'..'f'];
     inc(N);
  end;
end;

function MD5DataFromFile(const FileName: string): TMD5Data;
var
	FileHandle,
	MapHandle   : THandle;
	ViewPointer : pointer;
	Context     : TMD5Context;
begin
  if not FileExists(FileName) then
     exit;

	MD5CoreInitialize(Context);

	FileHandle := CreateFile(pChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
		            nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);

	if FileHandle <> INVALID_HANDLE_VALUE then
     try
		   MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
		   if MapHandle <> 0 then
          try
			      ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
			      if ViewPointer <> nil then
               try
				         MD5CoreUpdate(Context, ViewPointer^, GetFileSize(FileHandle, nil));
               finally
				         UnmapViewOfFile(ViewPointer);
               end;
          finally
			      CloseHandle(MapHandle);
          end;
     finally
		   CloseHandle(FileHandle);
     end;

  result := TMD5Data(MD5CoreFinalize(Context));
end;

procedure MD5CoreInitialize(var ctx: TMD5Context);
begin
  with ctx do begin
    Buf   := MD5_INITBUF;
    Bytes := MD5_INITDWD;
  end;
end;

procedure MD5CoreUpdate(var ctx: TMD5Context; const buffer; len: Cardinal);
var
  cnt : cardinal;
  pb  : ^Byte;
begin
  pb := @buffer;

  cnt := ctx.bytes[0];
  Inc(ctx.bytes[0], len);
  if ctx.bytes[0] < cnt then
    Inc(ctx.bytes[1]);

  cnt := 64 - (cnt and $3f);
  if cnt > len then begin
     Move(pb^, Pointer(Cardinal(@ctx.Cvn) + 64 - cnt)^, len);
     Exit;
  end;

  Move(pb^, Pointer(Cardinal(@ctx.Cvn) + 64 - cnt)^, cnt);
  MD5CoreTransform(ctx.buf, ctx.Cvn);
  Inc(pb, cnt);
  Dec(len, cnt);

  while len >= 64 do begin
    Move(pb^, ctx.Cvn, 64);
    MD5CoreTransform(ctx.buf, ctx.Cvn);
    Inc(pb, 64);
    Dec(len, 64);
  end;

  Move(pb^, ctx.Cvn, len);
end;

function MD5CoreFinalize(var ctx: TMD5Context): TMD5Data;
var
  cnt : Integer;
  pb  : ^Byte;
begin
  cnt := ctx.bytes[0] and $3f;
  pb  := @ctx.Cvn;
  Inc(pb, cnt);

  pb^ := $80;
  Inc(pb);

  cnt := 56 - 1 - cnt;

  if cnt < 0 then begin
    FillChar(pb^, cnt + 8, 0);
    MD5CoreTransform(ctx.buf, ctx.Cvn);
    pb  := @ctx.Cvn;
    cnt := 56;
  end;
  FillChar(pb^, cnt, 0);

  ctx.Cvn[14] := ctx.bytes[0] shl 3;
  ctx.Cvn[15] := (ctx.bytes[1] shl 3) or (ctx.bytes[0] shr 29);
  MD5CoreTransform(ctx.buf, ctx.Cvn);

  Move(ctx.buf, Result, 16);
  FillChar(ctx, SizeOf(ctx), 0);
end;

procedure MD5CoreTransform(var buf: TMD5Buffer; const cvn : TMD5Cvn);
var
  a, b, c, d: cardinal;
begin
  a := buf[0];  b := buf[1];  c := buf[2];  d := buf[3];

  Inc(a, cvn[0]  + MD5_TRSF[0]  + (d xor (b and (c xor d))));
  a := ((a shl 7)  or (a shr 25)) + b;
  Inc(d, cvn[1]  + MD5_TRSF[1]  + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr 20)) + a;
  Inc(c, cvn[2]  + MD5_TRSF[2]  + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr 15)) + d;
  Inc(b, cvn[3]  + MD5_TRSF[3]  + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr 10)) + c;
  Inc(a, cvn[4]  + MD5_TRSF[4]  + (d xor (b and (c xor d))));
  a := ((a shl 7)  or (a shr 25)) + b;
  Inc(d, cvn[5]  + MD5_TRSF[5]  + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr 20)) + a;
  Inc(c, cvn[6]  + MD5_TRSF[6]  + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr 15)) + d;
  Inc(b, cvn[7]  + MD5_TRSF[7]  + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr 10)) + c;
  Inc(a, cvn[8]  + MD5_TRSF[8]  + (d xor (b and (c xor d))));
  a := ((a shl 7)  or (a shr 25)) + b;
  Inc(d, cvn[9]  + MD5_TRSF[9]  + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr 20)) + a;
  Inc(c, cvn[10] + MD5_TRSF[10] + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr 15)) + d;
  Inc(b, cvn[11] + MD5_TRSF[11] + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr 10)) + c;
  Inc(a, cvn[12] + MD5_TRSF[12] + (d xor (b and (c xor d))));
  a := ((a shl 7)  or (a shr 25)) + b;
  Inc(d, cvn[13] + MD5_TRSF[13] + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr 20)) + a;
  Inc(c, cvn[14] + MD5_TRSF[14] + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr 15)) + d;
  Inc(b, cvn[15] + MD5_TRSF[15] + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr 10)) + c;

  Inc(a, cvn[1]  + MD5_TRSF[16] + (c xor (d and (b xor c))));
  a := ((a shl 5)  or (a shr 27)) + b;
  Inc(d, cvn[6]  + MD5_TRSF[17] + (b xor (c and (a xor b))));
  d := ((d shl 9)  or (d shr 23)) + a;
  Inc(c, cvn[11] + MD5_TRSF[18] + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr 18)) + d;
  Inc(b, cvn[0]  + MD5_TRSF[19] + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr 12)) + c;
  Inc(a, cvn[5]  + MD5_TRSF[20] + (c xor (d and (b xor c))));
  a := ((a shl 5)  or (a shr 27)) + b;
  Inc(d, cvn[10] + MD5_TRSF[21] + (b xor (c and (a xor b))));
  d := ((d shl 9)  or (d shr 23)) + a;
  Inc(c, cvn[15] + MD5_TRSF[22] + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr 18)) + d;
  Inc(b, cvn[4]  + MD5_TRSF[23] + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr 12)) + c;
  Inc(a, cvn[9]  + MD5_TRSF[24] + (c xor (d and (b xor c))));
  a := ((a shl 5)  or (a shr 27)) + b;
  Inc(d, cvn[14] + MD5_TRSF[25] + (b xor (c and (a xor b))));
  d := ((d shl 9)  or (d shr 23)) + a;
  Inc(c, cvn[3]  + MD5_TRSF[26] + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr 18)) + d;
  Inc(b, cvn[8]  + MD5_TRSF[27] + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr 12)) + c;
  Inc(a, cvn[13] + MD5_TRSF[28] + (c xor (d and (b xor c))));
  a := ((a shl 5)  or (a shr 27)) + b;
  Inc(d, cvn[2]  + MD5_TRSF[29] + (b xor (c and (a xor b))));
  d := ((d shl 9)  or (d shr 23)) + a;
  Inc(c, cvn[7]  + MD5_TRSF[30] + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr 18)) + d;
  Inc(b, cvn[12] + MD5_TRSF[31] + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr 12)) + c;

  Inc(a, cvn[5]  + MD5_TRSF[32] + (b xor c xor d));
  a := ((a shl 4)  or (a shr 28)) + b;
  Inc(d, cvn[8]  + MD5_TRSF[33] + (a xor b xor c));
  d := ((d shl 11) or (d shr 21)) + a;
  Inc(c, cvn[11] + MD5_TRSF[34] + (d xor a xor b));
  c := ((c shl 16) or (c shr 16)) + d;
  Inc(b, cvn[14] + MD5_TRSF[35] + (c xor d xor a));
  b := ((b shl 23) or (b shr 9))  + c;
  Inc(a, cvn[1]  + MD5_TRSF[36] + (b xor c xor d));
  a := ((a shl 4)  or (a shr 28)) + b;
  Inc(d, cvn[4]  + MD5_TRSF[37] + (a xor b xor c));
  d := ((d shl 11) or (d shr 21)) + a;
  Inc(c, cvn[7]  + MD5_TRSF[38] + (d xor a xor b));
  c := ((c shl 16) or (c shr 16)) + d;
  Inc(b, cvn[10] + MD5_TRSF[39] + (c xor d xor a));
  b := ((b shl 23) or (b shr 9))  + c;
  Inc(a, cvn[13] + MD5_TRSF[40] + (b xor c xor d));
  a := ((a shl 4)  or (a shr 28)) + b;
  Inc(d, cvn[0]  + MD5_TRSF[41] + (a xor b xor c));
  d := ((d shl 11) or (d shr 21)) + a;
  Inc(c, cvn[3]  + MD5_TRSF[42] + (d xor a xor b));
  c := ((c shl 16) or (c shr 16)) + d;
  Inc(b, cvn[6]  + MD5_TRSF[43] + (c xor d xor a));
  b := ((b shl 23) or (b shr 9))  + c;
  Inc(a, cvn[9]  + MD5_TRSF[44] + (b xor c xor d));
  a := ((a shl 4)  or (a shr 28)) + b;
  Inc(d, cvn[12] + MD5_TRSF[45] + (a xor b xor c));
  d := ((d shl 11) or (d shr 21)) + a;
  Inc(c, cvn[15] + MD5_TRSF[46] + (d xor a xor b));
  c := ((c shl 16) or (c shr 16)) + d;
  Inc(b, cvn[2]  + MD5_TRSF[47] + (c xor d xor a));
  b := ((b shl 23) or (b shr 9))  + c;

  Inc(a, cvn[0]  + MD5_TRSF[48] + (c xor (b or (not d))));
  a := ((a shl 6)  or (a shr 26)) + b;
  Inc(d, cvn[7]  + MD5_TRSF[49] + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr 22)) + a;
  Inc(c, cvn[14] + MD5_TRSF[50] + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr 17)) + d;
  Inc(b, cvn[5]  + MD5_TRSF[51] + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr 11)) + c;
  Inc(a, cvn[12] + MD5_TRSF[52] + (c xor (b or (not d))));
  a := ((a shl 6)  or (a shr 26)) + b;
  Inc(d, cvn[3]  + MD5_TRSF[53] + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr 22)) + a;
  Inc(c, cvn[10] + MD5_TRSF[54] + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr 17)) + d;
  Inc(b, cvn[1]  + MD5_TRSF[55] + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr 11)) + c;
  Inc(a, cvn[8]  + MD5_TRSF[56] + (c xor (b or (not d))));
  a := ((a shl 6)  or (a shr 26)) + b;
  Inc(d, cvn[15] + MD5_TRSF[57] + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr 22)) + a;
  Inc(c, cvn[6]  + MD5_TRSF[58] + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr 17)) + d;
  Inc(b, cvn[13] + MD5_TRSF[59] + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr 11)) + c;
  Inc(a, cvn[4]  + MD5_TRSF[60] + (c xor (b or (not d))));
  a := ((a shl 6)  or (a shr 26)) + b;
  Inc(d, cvn[11] + MD5_TRSF[61] + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr 22)) + a;
  Inc(c, cvn[2]  + MD5_TRSF[62] + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr 17)) + d;
  Inc(b, cvn[9]  + MD5_TRSF[63] + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr 11)) + c;

  Inc(buf[0], a);  Inc(buf[1], b);  Inc(buf[2], c);  Inc(buf[3], d);
end;

function MD5MemEqual(const A, B : TMD5Data) : boolean;
begin
  result := CompareMem(@A,@B,SizeOf(TMD5Data));
end;

function MD5(const Buffer; const Len: integer): string;
begin
  result := MD5DataToStr( MD5DataFromBuffer(buffer,len) );
end;

function MD5(const S : string) : string;
begin
  result := MD5DataToStr( MD5DataFromBuffer(PChar(S)^, Length(S)) );
end;

function MD5DataToStr(const Data : TMD5Data) : string;
var
  P: PChar;
  I: Integer;
const
  Digits: array[0..15] of Char = '0123456789abcdef';
begin
  SetLength(result, 32);
  P := PChar(result);
  for I := 0 to 15 do begin
    P[0] := Digits[Data[I] shr 4];
    P[1] := Digits[Data[I] and $F];
    Inc(P,2);
  end;
end;

function MD5DataFromBuffer(const Buffer; const Len: integer): TMD5Data;
var
  Context: TMD5Context;
begin
  MD5CoreInitialize(Context);
  MD5CoreUpdate(Context, Buffer, Len);
  Result := TMD5Data(MD5CoreFinalize(Context));
end;

end.
