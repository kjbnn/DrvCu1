unit commCu01;

{$mode objfpc}{$H+}

interface

uses
  Classes, synaser, Graphics,
  mMain;

type

  TProcessProc = function(IsWrite: boolean): boolean of object;

  { TComPort }
  TComPort = class(TThread)
  protected
    ser: TBlockSerial;
    procedure Execute; override;

  private
    strLog: string;
    procedure LocalLog;
    procedure ThreadLog(s: string);
    procedure DrawRead;
    procedure DrawWrite;

  public
    ProcessProc: TProcessProc;
    Serial, Baud: string;
    Owner: pointer;
  end;

function CRC_EVS(Buffer: TBuf; Number: byte): byte;

implementation

uses
  SysUtils, Forms;

procedure TComPort.LocalLog;
begin
  Log(strLog);
end;

procedure TComPort.ThreadLog(s: string);
begin
  strLog := s;
  Synchronize(@LocalLog);
end;

function ArrayToStr(Ar: array of byte; Count: byte): string;
var
  i: byte;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + IntToHex(Ar[i - 1], 2);
end;

procedure TComPort.DrawWrite;
begin
  with TLine(Owner) do
    try
      if GraphicScannerHandle > 0 then
        DrawGraphicScanner(w, wCount, clRed);
      if ExistDebugKey('Port') then
        ThreadLog('W> ' + ArrayToStr(w, wCount));
    except
      Log('WThread.DrawWrite');
    end;
end;

procedure TComPort.DrawRead;
begin
  with TLine(Owner) do
    try
      if GraphicScannerHandle > 0 then
        DrawGraphicScanner(r, rCount, clLime);
      if ExistDebugKey('Port') then
        ThreadLog('R> ' + ArrayToStr(r, rCount));
    except
      Log('RThread.DrawReceive');
    end;
end;

procedure TComPort.Execute;
var
  waiting: integer;
  TotalWaiting: word;
  crc: byte;
  i: byte;

begin

  try
    ser := TBlockSerial.Create;
    ser.RaiseExcept := True;
    ser.LinuxLock := False;
    ser.Connect(Serial);
    ser.Config(StrToInt(Baud), 8, 'N', 0, False, False);
    strLog := Format('Последовательный порт %s открыт',
      [Serial]);
    ThreadLog(strLog);

    with Tline(Owner) do
      while (not Terminated) do
      begin
        {отправка}
        sleep(10);
        FillChar(w, 255, 0);
        if not ProcessProc(True) then
          break;
        ArrayInsert(w, wCount, wCount, CRC_EVS(w, wCount));
        ArrayInsert(w, wCount, 0, StartFlag);
        ArrayInsert(w, wCount, wCount, EndFlag);
        i := 1;
        while i < (wCount - 1) do
        begin
          if w[i] in [StartFlag, EndFlag, EscFlag] then
          begin
            ArrayInsert(w, wCount, i, EscFlag);
            Inc(i);
          end;
          Inc(i);
        end;
        ser.SendBuffer(@w, wCount);
        Synchronize(@DrawWrite);

        {прием}
        FillChar(r, 255, 0);
        rCount := 0;
        TotalWaiting := 0;
        waiting := 0;
        sleep(10);
        repeat
          waiting := ser.WaitingData;
          if (rCount > 0) and (waiting = 0) then
            break;
          //ThreadLog( Format('waiting=%d', [waiting]) );
          ser.RecvBuffer(@r[rCount], waiting);
          rCount := rCount + waiting;
          Inc(TotalWaiting);
          sleep(10);
        until ((rCount > 0) and (waiting = 0)) or (TotalWaiting >= 200 {300});
        //ThreadLog( Format('TotalWaiting=%d', [TotalWaiting]) );
        Synchronize(@DrawRead);

        CurDev.Connected := False;
        if (rCount < 5) or (r[0] <> StartFlag) or (r[rCount - 1] <> EndFlag) then
        begin
          CurDev.Op:= DOP_EVENTLOGGET3;
          continue;
        end;

        // удаление флагов
        i := 1;
        while i < (wCount - 1) do
        begin
          if w[i] in [StartFlag, EndFlag, EscFlag] then
            ArrayDelete(w, wCount, i);
          Inc(i);
        end;
        ArrayDelete(w, wCount, 0);
        ArrayDelete(w, wCount, wCount - 1);
        // удаление флагов
        i := 1;
        while i < (rCount - 1) do
        begin
          if r[i] in [StartFlag, EndFlag, EscFlag] then
            ArrayDelete(r, rCount, i);
          Inc(i);
        end;
        ArrayDelete(r, rCount, 0);
        ArrayDelete(r, rCount, rCount - 1);
        crc := CRC_EVS(r, rCount - 1);
        if r[rCount - 1] = crc then
        begin
          CurDev.Connected := True;
          ProcessProc(False);
        end;

      end;

  finally
    Terminate;
    strLog := Format('Последовательный порт %s закрыт',
      [Serial]);
    if ser.LastError <> 0 then
      strLog := strLog + Format(', ошибка %d', [ser.LastError]);
    ThreadLog(strLog);
    try
      ser.Free;
    finally
      aMain.Close;
    end;
  end;

end;

function CRC_EVS(Buffer: TBuf; Number: byte): byte;
const
  abCSTbl: array [0..255] of byte = (
    $00, $1D, $3A, $27, $74, $69, $4E, $53, $E8, $F5, $D2, $CF, $9C, $81, $A6, $BB,
    $CD, $D0, $F7, $EA, $B9, $A4, $83, $9E, $25, $38, $1F, $02, $51, $4C, $6B, $76,
    $87, $9A, $BD, $A0, $F3, $EE, $C9, $D4, $6F, $72, $55, $48, $1B, $06, $21, $3C,
    $4A, $57, $70, $6D, $3E, $23, $04, $19, $A2, $BF, $98, $85, $D6, $CB, $EC, $F1,
    $13, $0E, $29, $34, $67, $7A, $5D, $40, $FB, $E6, $C1, $DC, $8F, $92, $B5, $A8,
    $DE, $C3, $E4, $F9, $AA, $B7, $90, $8D, $36, $2B, $0C, $11, $42, $5F, $78, $65,
    $94, $89, $AE, $B3, $E0, $FD, $DA, $C7, $7C, $61, $46, $5B, $08, $15, $32, $2F,
    $59, $44, $63, $7E, $2D, $30, $17, $0A, $B1, $AC, $8B, $96, $C5, $D8, $FF, $E2,
    $26, $3B, $1C, $01, $52, $4F, $68, $75, $CE, $D3, $F4, $E9, $BA, $A7, $80, $9D,
    $EB, $F6, $D1, $CC, $9F, $82, $A5, $B8, $03, $1E, $39, $24, $77, $6A, $4D, $50,
    $A1, $BC, $9B, $86, $D5, $C8, $EF, $F2, $49, $54, $73, $6E, $3D, $20, $07, $1A,
    $6C, $71, $56, $4B, $18, $05, $22, $3F, $84, $99, $BE, $A3, $F0, $ED, $CA, $D7,
    $35, $28, $0F, $12, $41, $5C, $7B, $66, $DD, $C0, $E7, $FA, $A9, $B4, $93, $8E,
    $F8, $E5, $C2, $DF, $8C, $91, $B6, $AB, $10, $0D, $2A, $37, $64, $79, $5E, $43,
    $B2, $AF, $88, $95, $C6, $DB, $FC, $E1, $5A, $47, $60, $7D, $2E, $33, $14, $09,
    $7F, $62, $45, $58, $0B, $16, $31, $2C, $97, $8A, $AD, $B0, $E3, $FE, $D9, $C4);
var
  i: byte;
begin
  Result := 0;
  for i := 1 to Number do
    Result := abCSTbl[Result xor Buffer[i - 1]];

end;



end.
