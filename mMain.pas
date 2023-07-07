unit mMain;

interface

uses
  SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  ExtCtrls, ComCtrls, Menus, ValEdit, FileUtil,
  laz2_DOM,
  laz2_XMLRead,
  FileInfo,
  cksbmes, cForFormKsb;

const
  MAX_LOG_SIZE = 3e10;
  PORT_BUF_SIZE = 256;
  OFFSET_SEARCHCMD = 12;

  CO_NOLINK = 0;
  CO_NOLINK_FAULT = 2;
  CO_NOLINK_CORESTART = 4;
  CO_NOLINK_STOP = 6;
  CO_NOLINK_UNKNOWN = $FF;
  CO_DISCONNECTED = 20;

  TYPEDEVICE_CU01 = 1;
  TYPEDEVICE_CELL = 2;
  TYPEDEVICE_USER = 3;

  MSG_BASE_ROSTEK = 14000;
  MSG_CU01_AVAILABLE = MSG_BASE_ROSTEK + 501;
  MSG_CU01_NOT_AVAILABLE = MSG_BASE_ROSTEK + 502;
  MSG_CU01_STATE = MSG_BASE_ROSTEK + 503;
  MSG_CELL_STATE = MSG_BASE_ROSTEK + 504;
  MSG_CMD_GET_STATES = MSG_BASE_ROSTEK + 505;
  MSG_CMD_CELL_OPEN = MSG_BASE_ROSTEK + 506;
  MSG_CMD_CELL_CLOSE = MSG_BASE_ROSTEK + 507;

  MSG_CMD_ADD_USER = MSG_BASE_ROSTEK + 508;
  MSG_CMD_ADD_USER_WITH_RIGHTS = MSG_BASE_ROSTEK + 509;
  MSG_ADDED_USER = MSG_BASE_ROSTEK + 510;
  MSG_NOT_ADDED_USER = MSG_BASE_ROSTEK + 511;
  MSG_CMD_SET_USERRIGHTS = MSG_BASE_ROSTEK + 512;
  MSG_CMD_SET_USERRIGHTS_WITH_SEARCH = MSG_BASE_ROSTEK + 513;
  MSG_ADDED_USERRIGHTS = MSG_BASE_ROSTEK + 514;
  MSG_NOT_ADDED_USERRIGHTS = MSG_BASE_ROSTEK + 515;
  MSG_CMD_DELETE_USER = MSG_BASE_ROSTEK + 516;
  MSG_CMD_DELETE_USER_WITH_SEARCH = MSG_BASE_ROSTEK + 517;
  MSG_DELETED_USER = MSG_BASE_ROSTEK + 518;
  MSG_NOT_DELETED_USER = MSG_BASE_ROSTEK + 519;
  MSG_CMD_DELETE_ALL_USERS = MSG_BASE_ROSTEK + 520;

  StartFlag = $81;
  EndFlag = $82;
  EscFlag = $83;
  ReplyOK = $FF;

type

  TRule = record
    Func: string [16];
    Arg: array [0..255] of word;
    TextRule: string [255];
  end;

  TArrayShift = (SHLEFT, SHRIGHT);

  TDevOp = (
    DOP_NOP,
    DOP_GET_NAME,
    DOP_SET_SOUND,
    DOP_SET_TIME,
    DOP_GET_TIME,
    DOP_EVENTLOGSEEK,

    DOP_BOXGETSTATE,
    DOP_BOXOPEN,
    DOP_BOXCLOSE,
    DOP_CARDSETMODE,
    DOP_GETFIRMWAREVERSION,
    DOP_GETCONFIG2,
    DOP_CLEARALLDB,
    DOP_EVENTLOGGET2,
    DOP_BOXGETSTATE2,

    DOP_BOXOPEN2,
    DOP_ADDUSER,
    DOP_USERRIGHTS,
    DOP_USERSEARCH,
    DOP_DELUSER,
    DOP_GETUSER,
    DOP_EVENTLOGGET3
    );

  ParamKind = (PARAM_WORD, PARAM_TIME);

  TBuf = array [0..PORT_BUF_SIZE - 1] of byte;
  PTBuf = ^TBuf;

  TAct = (ComPortList, DeviceList, ShleifList, RelayList, ReaderList);

  TOption = record
    LogForm,
    LogFile: boolean;
    FileMask: string;
    Debug: string;
  end;

  TObjectType = (UNKNOWN, DEVCU, CELL, USER);

  TCu1Obj = class
  public
    Kind: TObjectType;
    Number, Bigdevice, Smalldevice: word;
    ParentObj: pointer;
    ChildsObj: TList;
    constructor Create;
    destructor Destroy; override;
    function FindChild(bKind: TObjectType; Num: word): word;
    function FindWithIdChild(bKind: TObjectType; Id: word): word;
  end;

  TDev = class;

  { TLine }
  TLine = class(TCu1Obj)
  private
    Serial: TObject;
    Cmds: TThreadList;
    procedure Read;
    function NextDev: TDev;
    function Process(IsWrite: boolean): boolean;
    procedure AddCmd(Op: TDevOp; ArSize: byte; Ar: TBytes);
    procedure GetCmd(var Op: TDevOp; var ArSize: byte; var Ar: TBytes);
    procedure SetOp(Prm: byte = 0);
  public
    w, r: TBuf;
    rCount, wCount: byte;
    CurDev: TDev;
    constructor Create;
    destructor Destroy; override;
  end;

  TCmdRec = record
    Op: TDevOp;
    ParamSize: byte;
    Params: TBytes;
  end;

  { TDev }
  TDev = class(TCu1Obj)
    FLinkBit: byte;
    FNoAnswer: byte;
    Op: TDevOp;
    TempIndex: word;
    CmdParamsSize: byte;
    CmdParams: Tbytes;
    CurMes, LastMes: longword;
    SecNumber: byte;
    lRule: TList;
    function GetConnect: boolean;
    procedure SetConnect(Value: boolean);
    function GetLinkBit: byte;
    function FindRule(s: string): pointer;
    function FindArmRule(s: string): pointer;
    function StrToRule(s: string; var rule: TRule): boolean;
    procedure AddRule(rule: TRule);
    procedure DelRule(p: pointer);






  public
    constructor Create;
    destructor Destroy; override;
    property LinkBit: byte read GetLinkBit;
  published
    property Connected: boolean read GetConnect write SetConnect;
  end;


  { TaMain }
  TaMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    N5: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    StatusBar1: TStatusBar;
    Memo1: TMemo;
    N1: TMenuItem;
    N2: TMenuItem;
    Indicator: TShape;
    FormTimer: TTimer;
    TimerVisible: TTimer;
    ValueListEditor1: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure FormTimerTimer(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure ReadParam;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormConstrainedResize(Sender: TObject;
      var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
    procedure N3Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure IndicatorMouseMove(Sender: TObject);
    procedure TimerVisibleTimer(Sender: TObject);
  private
    XML: TXMLDocument;
    procedure ReadConfiguration;
    procedure ReadConfigNode(Node: TDOMNode; pParent: Pointer);
    procedure ReadUsers(Node: TDOMNode);
    procedure SetIndicator;
  public
  end;

function ExistDebugKey(sub: string): boolean;
function ArrayToStr(Ar: array of byte; Count: byte): string;
procedure WriteLog(str: string);
procedure Log(str: string);
function GetVersion(FileName: string): string;
function FindDev(Number: word): TDev;
function FindDevWithId(Id: word): TDev;
procedure Consider(mes: KSBMES; str: string);
procedure Send(mes: KSBMES); overload;
procedure Send(mes: KSBMES; str: PChar); overload;
procedure ArrayShift(var a: TBuf; StartPos, EndPos: byte; Kind: TArrayShift);
procedure ArrayInsert(var a: TBuf; var ArraySize: byte; StartPos, Value: byte);
procedure ArrayDelete(var a: TBuf; var ArraySize: byte; StartPos: byte);


var
  aMain: TaMain;
  Option: TOption;
  GraphicScannerHandle: THandle;
  InitGraphicScanner: procedure(Left, Top, Col, Row: word); stdcall;
  ViewGraphicScanner: procedure(Value: boolean); stdcall;
  DrawGraphicScanner: procedure(m: array of byte; Num: word; Color: dword); stdcall;
  Lines: TList;
  Devs: TList;
  Event: array [0..511] of string;
  CU01State: array [0..1] of string = ('Нет связи !!!!', 'На связи');
  ModuleNetDevice, ModuleBigDevice: byte;



implementation

uses
  cmesdriver_api,
  DateUtils,
  commCu01,
  cIniKey,
  constants,
  typinfo;

{$R *.lfm}


procedure TaMain.FormCreate(Sender: TObject);
var
  mes: KSBMES;

const
  IndicatorSize = 15;

var
  i: byte;

begin
  AppKsbInit(self);
  TimerVisible.Enabled := True;

  try
    KsbAppType := GetKey('NUMBER', APPLICATION_DRV_CU01);
    ReadParam;
    Log('Старт модуля (вер. ' + GetVersion(Application.ExeName) + ')');
    StatusBar1.Panels.Items[0].Text := ' Старт: ' + DateTimeToStr(Now);
    StatusBar1.Panels.Items[2].Text :=
      Format(' Net=%d Big=%d, ', [ModuleNetDevice, ModuleBigDevice]) +
      GetKey('COMMENT', 'Комментарий...');

    ReadConfiguration;

    if not ExistDebugKey('noport') then
      for i := 1 to Lines.Count do
        (TLine(Lines.Items[i - 1]).Serial as TComPort).Start
    else
      MessageDlg('Внимание',
        'Модуль загружен в режиме без связи с СУ01!',
        mtWarning, [mbOK], 0, mbOK);

    FormTimer.Enabled := True;

  except
    On E: Exception do
    begin
      Log(Format('Процесс завершен с ошибкой #%s', [E.Message]));
      Close;
    end;
  end;

  Indicator.Parent := StatusBar1;
  Indicator.BorderSpacing.Left := StatusBar1.Panels[0].Width + 4;
  Indicator.BorderSpacing.Top := ((StatusBar1.Height - IndicatorSize) div 2);
  {$ifdef MSWINDOWS}
  Indicator.BorderSpacing.Top := Indicator.BorderSpacing.Top + 1;
  {$endif}
  Indicator.Shape := stCircle;
  Indicator.Visible := True;
  Indicator.Width := IndicatorSize;
  Indicator.Height := IndicatorSize;
  Indicator.Pen.Color := clGray;
  Indicator.Brush.Color := clRed;
  Indicator.ShowHint := True;

  // vvv МУ
  ValueListEditor1.Cells[2, 1] := '1';
  ValueListEditor1.Cells[2, 2] := '1';
  ValueListEditor1.Cells[2, 3] := '1';
  ValueListEditor1.Cells[2, 4] := '1';
  ValueListEditor1.Cells[2, 5] := '14000';
  // ^^^
end;

procedure TaMain.ReadParam;
var
  i: integer;
begin
  with Option do
  begin
    ModuleNetDevice := getkey('ModuleNetDevice', 1);
    ModuleBigDevice := getkey('ModuleBigDevice', 1);
    FileMask := GetKey('FileMask', '');
    if FileMask = '' then
    begin
      FileMask := ExtractFileName(ParamStr(0));
      i := Pos('.exe', FileMask);
      if i > 0 then
        SetLength(FileMask, Length(FileMask) - 4);
    end;
    LogForm := StrToInt(getkey('LogForm', '1')) = 1;
    LogFile := StrToInt(getkey('LogFile', '1')) = 1;
    Debug := getkey('Debug', '');
  end;
  Left := StrToInt(getkey('POS_LEFT', '0'));
  Top := StrToInt(getkey('POS_TOP', '0'));
  Width := StrToInt(getkey('POS_WIDTH', '400'));
  Height := StrToInt(getkey('POS_HEIGHT', '450'));
end;

procedure TaMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: byte;
begin
  for i := 1 to Lines.Count do
    if TLine(Lines.Items[i - 1]).Serial <> nil then
    begin
      //(TLine(Lines.Items[i - 1]).Serial as TComPort).Suspend;
      (TLine(Lines.Items[i - 1]).Serial as TComPort).Terminate;
    end;
  CloseAction := caFree;
  setkey('POS_LEFT', left);
  setkey('POS_TOP', top);
  setkey('POS_WIDTH', Width);
  setkey('POS_HEIGHT', Height);
  Log('Останов модуля');
  inherited;
end;

procedure TaMain.FormConstrainedResize(Sender: TObject;
  var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
begin
  MinWidth := 300;
  MinHeight := 150;
  MaxWidth := MaxWidth;
  MaxHeight := MaxHeight;
end;

procedure TaMain.ReadConfiguration;
var
  str: string;
begin
  with  Option do
  begin
    str := ExtractFilePath(Application.ExeName);
    Log('Чтение файла ' + FileMask + '.xml...');
    ReadXMLFile(Xml, str + FileMask + '.xml');
    ReadConfigNode(Xml.FindNode('EvsConfig'), nil);
    Log('Файл ' + FileMask + '.xml прочитан');
  end;
end;

procedure TaMain.ReadConfigNode(Node: TDOMNode; pParent: Pointer);
var
  XmlDocNode: TDOMNode;
  i: word;
  pObj: pointer;
  s: string;
  rule: TRule;

begin
  if Node = nil then
    exit;
  for i := 1 to Node.ChildNodes.Count do
  begin
    pObj := nil;
    XmlDocNode := Node.ChildNodes.Item[i - 1];

    if XmlDocNode.NodeName = 'Line' then
    begin
      s := Format('%s Link=%s Baud=%s', [XmlDocNode.NodeName,
        TDOMElement(XmlDocNode).GetAttribute('Link'),
        TDOMElement(XmlDocNode).GetAttribute('Baud')]);
      Log(s);
      s := TDOMElement(XmlDocNode).GetAttribute('Link');
      if Pos('Com', s) > 0 then
      begin
        pObj := TLine.Create;
        with TLine(pObj) do
        begin
          ParentObj := pParent;
          Serial := TComPort.Create(True);
          with (Serial as TComPort) do
          begin
            Serial := TDOMElement(XmlDocNode).GetAttribute('Link');
            Baud := TDOMElement(XmlDocNode).GetAttribute('Baud');
            Owner := pObj;
            ProcessProc := Process;
          end;
        end;
        Lines.Add(pObj);
      end;
    end;

    if XmlDocNode.NodeName = 'Su01' then
    begin
      s := Format('%s Id=%s Address=%s', [XmlDocNode.NodeName,
        TDOMElement(XmlDocNode).GetAttribute('Id'),
        TDOMElement(XmlDocNode).GetAttribute('Address')]);
      Log(s);
      pObj := TDev.Create;
      with TDev(pObj) do
      begin
        lRule := TList.Create;
        Bigdevice := TDOMElement(XmlDocNode).GetAttribute('Id').ToInteger;
        Number := TDOMElement(XmlDocNode).GetAttribute('Address').ToInteger;
        ParentObj := pParent;
        TCu1Obj(ParentObj).ChildsObj.Add(pObj);
        Devs.Add(pObj);
      end;
    end;

    if XmlDocNode.NodeName = 'Rule' then
    begin
      s := Format('%s Text=%s ', [XmlDocNode.NodeName,
        TDOMElement(XmlDocNode).GetAttribute('Text')]);
      Log(s);

      TDev(pParent).StrToRule(TDOMElement(XmlDocNode).GetAttribute('Text'), rule);
      TDev(pParent).AddRule(rule);

    end;

    ReadConfigNode(XmlDocNode, pObj);
  end;
end;

{
Перебираются все Dev
}
procedure TaMain.ReadUsers(Node: TDOMNode);
var
  XmlDocNode: TDOMNode;
  i: word;
  pObj: pointer;
  s: ansistring;
  j, k: word;
  Ar: TBytes;
  ArSize: byte;
  Id: word;
  DevCells: array [1..10] of dword;

begin
  if Node = nil then
    exit;

  for i := 1 to Node.ChildNodes.Count do
  begin
    pObj := nil;
    XmlDocNode := Node.ChildNodes.Item[i - 1];

    if XmlDocNode.NodeName = 'Line' then
    begin
      s := Format('%s Link=%s', [XmlDocNode.NodeName,
        TDOMElement(XmlDocNode).GetAttribute('Link')]);
      Log(s);
    end;

    if XmlDocNode.NodeName = 'Su01' then
    begin
      s := Format('%s Id=%s Address=%s', [XmlDocNode.NodeName,
        TDOMElement(XmlDocNode).GetAttribute('Id'),
        TDOMElement(XmlDocNode).GetAttribute('Address')]);
      Log(s);
    end;

    if XmlDocNode.NodeName = 'User' then
    begin
      s := Format('%s Id=%s Number=%s PesonalCode=%s Card=%s Pin=%s  Cells=%s ',
        [XmlDocNode.NodeName, TDOMElement(XmlDocNode).GetAttribute('Id'),
        TDOMElement(XmlDocNode).GetAttribute('HwId'),
        TDOMElement(XmlDocNode).GetAttribute('PesonalCode'),
        TDOMElement(XmlDocNode).GetAttribute('Card'),
        TDOMElement(XmlDocNode).GetAttribute('Pin'),
        TDOMElement(XmlDocNode).GetAttribute('Cells')]);
      Log(s);

      //находим Line и добавляем в нее команду DOP_ADDUSER
      s := ' ' + ' ' + ' ' + ' ' + TDOMElement(XmlDocNode).GetAttribute('Card') +
        ' ' + TDOMElement(XmlDocNode).GetAttribute('PesonalCode') + ' ' + ' ';
      ArSize := 0;
      for j := 1 to length(s) do
      begin
        Inc(ArSize);
        setLength(Ar, ArSize);
        if s[j] <> ' ' then
          Ar[j - 1] := Ord(s[j])
        else
          Ar[j - 1] := 0;
      end;
      Inc(ArSize);
      setLength(Ar, ArSize);
      Ar[ArSize - 1] := TDOMElement(XmlDocNode).GetAttribute('Type').ToInteger;
      id := TDOMElement(Node).GetAttribute('Id').ToInteger;
      pObj := FindDevWithId(id);
      pObj := TCu1Obj(pObj).ParentObj;
      TLine(pObj).AddCmd(DOP_ADDUSER, ArSize, Ar);

      //находим Line и добавляем в нее команду DOP_USERRIGHTS
      FillChar(DevCells, sizeof(DevCells), 0);
      s := TDOMElement(XmlDocNode).GetAttribute('Cells');
      j := 1;
      while j < length(s) do
      begin
        k := Pos('U', s);
        if k = 5 then
          DevCells[StrToInt(Copy(s, 1, 2))] :=
            DevCells[StrToInt(Copy(s, 1, 2))] or (1 shl (StrToInt(Copy(s, 3, 2)) - 1));
        k := Pos(' ', s);
        if (k > 0) then
          Delete(s, 1, k);
      end;

      ArSize := 0;
      for j := 1 to 10 do
        if DevCells[j] > 0 then
        begin
          ArSize := ArSize + 5;
          setLength(Ar, ArSize);
          Ar[ArSize - 5] := j;
          move(DevCells[j], Ar[ArSize - 4], 4);

          for k := 1 to 32 do
            if (DevCells[j] and (1 shl (k - 1))) > 0 then
            begin
              ArSize := ArSize + 1;
              setLength(Ar, ArSize);
              Ar[ArSize - 1] := 1;
            end;
        end;
      id := TDOMElement(Node).GetAttribute('Id').ToInteger;
      pObj := FindDevWithId(id);
      pObj := TCu1Obj(pObj).ParentObj;
      TLine(pObj).AddCmd(DOP_USERRIGHTS, ArSize, Ar);
    end;

    ReadUsers(XmlDocNode);
  end;

end;

(* ------------------------------------- *)
(*        O B J,  L I N E,  D E V        *)
(* ------------------------------------- *)

{ TCu1Obj }
constructor TCu1Obj.Create;
begin
  ChildsObj := TList.Create;
  kind := UNKNOWN;
end;

destructor TCu1Obj.Destroy;
begin
  while ChildsObj.Count > 0 do
  begin
    TCu1Obj(ChildsObj.Items[0]).Free;
    ChildsObj.Delete(0);
  end;
  inherited;
end;

function TCu1Obj.FindChild(bKind: TObjectType; Num: word): word;
var
  i: word;
  Obj: TCu1Obj;
begin
  Result := $FFFF;
  for i := 1 to ChildsObj.Count do
  begin
    Obj := ChildsObj.Items[i - 1];
    if (Obj.Kind = bKind) and (Obj.Number = Num) then
    begin
      Result := i - 1;
      Break;
    end
    else
      continue;
  end;
end;

function TCu1Obj.FindWithIdChild(bKind: TObjectType; Id: word): word;
var
  i: word;
  Obj: TCu1Obj;
begin
  Result := $FFFF;
  for i := 1 to ChildsObj.Count do
  begin
    Obj := ChildsObj.Items[i - 1];
    if (Obj.Kind = bKind) and (Obj.Smalldevice = Id) then
    begin
      Result := i - 1;
      Break;
    end
    else
      continue;
  end;
end;

{ TDev }
constructor TDev.Create;
begin
  inherited;
  FLinkBit := $00;
  FNoAnswer := CO_DISCONNECTED;
  SecNumber := 0;
  CurMes := 0;
  LastMes := 0;
end;

destructor TDev.Destroy;
begin
  inherited;
end;

function TDev.GetConnect: boolean;
begin
  Result := False;
  if FNoAnswer < CO_DISCONNECTED then
    Result := True;
end;

procedure TDev.SetConnect(Value: boolean);
var
  mes: KSBMES;
  s: string;

begin
  if Value then
  begin
    if (FNoAnswer >= CO_DISCONNECTED) then
    begin
      Init(mes);
      mes.SysDevice := SYSTEM_EVS;
      mes.TypeDevice := TYPEDEVICE_CU01;
      mes.NetDevice := ModuleNetDevice;
      mes.BigDevice := Bigdevice;
      mes.Code := MSG_CU01_AVAILABLE;
      Send(mes);
      s := Format('СУ01 #%d СОБЫТИЕ >> %s', [Number, CU01State[1]]);
      Log(s);
    end;
    FNoAnswer := 0;

    if FLinkBit = 0 then
      FLinkBit := $80
    else
      FLinkBit := $00;
  end

  else if (FNoAnswer < CO_DISCONNECTED) then
  begin
    Inc(FNoAnswer);

    if (FNoAnswer = CO_DISCONNECTED) then
    begin
      Init(mes);
      mes.SysDevice := SYSTEM_EVS;
      mes.TypeDevice := TYPEDEVICE_CU01;
      mes.NetDevice := ModuleNetDevice;
      mes.BigDevice := Bigdevice;
      mes.Code := MSG_CU01_NOT_AVAILABLE;
      Send(mes);
      s := Format('СУ01 #%d СОБЫТИЕ >> %s', [Number, CU01State[0]]);
      Log(s);
    end;

  end;

end;

function TDev.GetLinkBit: byte;
begin
  Result := FLinkBit;
end;

procedure TLine.SetOp(Prm: byte);
var
  CmdOp: TDevOp;

begin
  with CurDev do
    case Op of
      DOP_NOP: Op := DOP_GET_NAME;
      DOP_GET_NAME: Op := DOP_SET_TIME;
      DOP_SET_TIME: Op := DOP_GET_TIME;
      DOP_GET_TIME: Op := DOP_GETFIRMWAREVERSION;
      DOP_GETFIRMWAREVERSION: Op := DOP_GETCONFIG2;
      DOP_GETCONFIG2:
        if SecNumber > 0 then
        begin
          Op := DOP_BOXGETSTATE2;
          TempIndex := 1;
        end;
      DOP_BOXGETSTATE2:
        if TempIndex <= SecNumber then
          Inc(TempIndex)
        else
          Op := DOP_EVENTLOGGET3;

      DOP_EVENTLOGGET3:
      begin
        if (CurMes = LastMes) and (CurMes <> 0) then
          GetCmd(CmdOp, CmdParamsSize, CmdParams);
        if CmdOp <> DOP_NOP then
        begin
          TempIndex := $FFFF;
          Op := CmdOp;
        end;
      end;

      DOP_USERSEARCH:
        if Prm = 1 then
          case CmdParams[7] of
            2: Op := DOP_USERRIGHTS;
            3: Op := DOP_DELUSER;
          end
        else
          Op := DOP_EVENTLOGGET3;


      else
        Op := DOP_EVENTLOGGET3;

    end;
end;


{ TLine }
constructor TLine.Create;
begin
  inherited Create;
  Cmds := TThreadList.Create;
end;

destructor TLine.Destroy;
begin
  Cmds.Free;
  inherited;
end;

procedure TLine.AddCmd(Op: TDevOp; ArSize: byte; Ar: TBytes);
var
  internalList: TList;
  pCmdRec: ^TCmdRec;
begin
  new(pCmdRec);
  pCmdRec^.Op := Op;
  pCmdRec^.ParamSize := ArSize;
  pCmdRec^.Params := Ar;
  internalList := Cmds.LockList;
  internalList.Add(pCmdRec);
  Cmds.UnlockList;
end;

procedure TLine.GetCmd(var Op: TDevOp; var ArSize: byte; var Ar: TBytes);
var
  internalList: TList;
  pCmdRec: ^TCmdRec;
begin
  Op := DOP_NOP;
  internalList := Cmds.LockList;
  if internalList.Count > 0 then
  begin
    PCmdRec := internalList.First;
    Op := PCmdRec^.Op;
    ArSize := pCmdRec^.ParamSize;
    Ar := pCmdRec^.Params;

    pCmdRec^.Params := nil;
    internalList.Remove(PCmdRec);
    Dispose(pCmdRec);
  end;
  Cmds.UnlockList;
end;

function TLine.Process(IsWrite: boolean): boolean;
var
  s: string;
  i: word;
  dw: dword;
  mes: KSBMes;
  pRule: ^TRule;

begin
  Result := False;

  if ExistDebugKey('Tic') then
    if CurDev <> nil then
    begin
      if IsWrite then
        s := Format('tic >> %s СУ01 #%d %s %s (%d)',
          [TComPort(Serial).Serial, CurDev.Number, 'W',
          GetEnumName(TypeInfo(TDevOp), Ord(CurDev.Op)), Ord(CurDev.Op)])
      else
        s := Format('tic >> %s СУ01 #%d %s %s (%d)',
          [TComPort(Serial).Serial, CurDev.Number, 'R',
          GetEnumName(TypeInfo(TDevOp), Ord(CurDev.Op)), Ord(CurDev.Op)]);
      log(s);
    end;

  if IsWrite then
  begin
    CurDev := NextDev;
    if CurDev = nil then
    begin
      Log(Format('Для %s не заданы СУ01',
        [TComPort(self.Serial).Serial]));
      exit;
    end;
  end;

  try
    with CurDev do
      case Op of

        DOP_NOP:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $00;
            wCount := 2;
          end
          else
          begin
            if r[1] = ReplyOK then
              Log(Format('СУ01 #%d СУ01 на связи', [Number]));
            TempIndex := 0;
            SetOp;
          end;

        DOP_GET_NAME:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $01;
            wCount := 2;
          end
          else
          begin
            for i := 2 to rCount - 2 do
              s := s + Chr(r[i - 1]);
            Log(s);
            SetOp;
          end;

        DOP_GET_TIME:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $06;
            wCount := 2;
          end
          else
          begin
            s := Format(
              'СУ01 #%d Текущие дата и время %d/%d/%d %d:%d:%d ',
              [Number, 1900 + r[1], r[2], r[3], r[4], r[5], r[6]]);
            Log(s);
            SetOp;
          end;

        DOP_SET_TIME:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $07;
            w[2] := YearOf(now) - 1900;
            w[3] := MonthOf(now);
            w[4] := DayOf(now);
            w[5] := HourOf(now);
            w[6] := MinuteOf(now);
            w[7] := SecondOf(now);
            wCount := 8;
          end
          else
          begin
            if r[1] = ReplyOK then
            begin
              Log(Format('СУ01 #%d Установлено время', [Number]));
            end
            else
            begin
              Log(Format('СУ01 #%d Ошибка установки времени',
                [Number]));
            end;
            SetOp;
          end;

        DOP_BOXOPEN:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $14;
            w[2] := $01;//Cell number
            wCount := 3;
          end
          else
          begin
            if r[1] = ReplyOK then
            begin
              Log(Format('СУ01 #%d Открыта ячейка #%d', [Number, w[2]]));
            end
            else
            begin
              Log(Format(
                'СУ01 #%d Ошибка открытия ячейки #%d',
                [Number, w[2]]));
            end;
            SetOp;
          end;

        DOP_BOXCLOSE:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $15;
            w[2] := $01; //Cell number
            wCount := 3;
          end
          else
          begin
            if r[1] = ReplyOK then
            begin
              Log(Format('СУ01 #%d Закрыта ячейка #%d', [Number, w[2]]));
            end
            else
            begin
              Log(Format(
                'СУ01 #%d Ошибка закрытия ячейки #%d',
                [Number, w[2]]));
            end;
            SetOp;
          end;

        DOP_SET_SOUND:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $21;
            w[2] := $04;
            //Sound (1-сирена, 2-внимание, 3-пенал в чуж яч, 4-пенал вставлен без карты, 5-ч)
            wCount := 1;
          end
          else
          begin
            if r[1] = ReplyOK then
            begin
              Log(Format('СУ01 #%d Звук #%d активирован',
                [Number, w[1]]));
            end
            else
            begin
              Log(Format(
                'СУ01 #%d Активировация звук #%d завершилось с ошибкой',
                [Number, w[1]]));
            end;

            SetOp;
          end;

        DOP_GETFIRMWAREVERSION:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $27;
            wCount := 2;
          end
          else
          begin
            s := Format(
              'Версия прошивки СУ01 #%d - %d.%d.%d.%d Дата ',
              [Number, r[1], r[2], r[3], r[4]]);
            for i := 1 to 11 do
              s := s + Chr(r[i + 4]);
            Log(s);
            SetOp;
          end;

        DOP_CLEARALLDB:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $33;
            w[2] := $01;
            //Флаги побитно, 0-карты, 1-расписания, 2-предупр. яч.
            w[3] := w[2];//Повтор флагов
            wCount := 4;
          end
          else
          begin
            if r[1] = ReplyOK then
              Log(Format('СУ01 #%d Все пользователи удалены',
                [Number]))
            else
              Log(Format(
                'СУ01 #%d Ошибка удаления пользователей', [Number]));
            SetOp;
          end;

        DOP_GETCONFIG2:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $34;
            w[2] := $00; //General info
            wCount := 3;
          end
          else
          begin
            Log(Format('СУ01 #%d Секция #%d типа. Режим работы:',
              [Number, r[1]]));
            if (r[2] and (1 shl 0)) > 0 then
              Log('^ идентификация картой');
            if (r[2] and (1 shl 1)) > 0 then
              Log('^ идентификация личный номер');
            if (r[2] and (1 shl 2)) > 0 then
              Log('^ идентификация отпечатком пальца');
            if (r[2] and (1 shl 7)) > 0 then
              Log('^ при идентификация подтверждение ПИН-кодом');
            s := Format('^ версия прошивки %d.%d.%d.%d',
              [r[4], r[5], r[6], r[7]]);
            Log(s);
            s := Format('^ cерийный номер %d.%d.%d.%d',
              [r[8], r[9], r[10], r[11]]);
            Log(s);
            SecNumber := r[12];
            s := Format('^ количество секций #%d', [SecNumber]);
            Log(s);
            SetOp;
          end;

        DOP_BOXGETSTATE2:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $36;
            w[2] := TempIndex; //Secton
            wCount := 3;
            Log(Format('СУ01 #%d Запрос состояния секции #%d',
              [Number, w[2]]));
          end
          else
          begin
            case r[1] of
              0: s := 'Нет секции с таким номером';
              1: s := 'Секция СК24';
              2: s := 'Секция СД18';
              3: s := 'Секция СК32';
              4: s := 'Секция СУ12';
            end;
            Log(s);
            if r[1] > 0 then
            begin
              for i := 2 to rCount - 2 do
              begin
                s := Format('Ячейка %d ', [i - 1]);
                case r[i] and $03 of
                  0: s := s + ' Нет пенала';
                  1: s := s + ' Чужой пенал';
                  2: s := s + ' Свой пенал';
                end;
                if (r[i] and $04) > 0 then
                  s := s + ' Норма';
                if (r[i] and $10) > 0 then
                  s := s + ' Ошибка в ячейке (чужой пенал...)';
                if (r[i] and $20) > 0 then
                  s := s +
                    ' Предупреждение, пенала в ячейке не было';
                Log(s);
              end;
            end;
            SetOp;
          end;

        DOP_BOXOPEN2:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $37;
            w[2] := $01; //Secton
            w[3] := $01; //Cell
            wCount := 4;
          end
          else
          begin
            if r[1] = ReplyOK then
            begin
              Log(Format('СУ01 #%d Открыта ячейка #%d в секции %d',
                [Number, w[3], w[2]]));
            end;
            SetOp;
          end;

        DOP_ADDUSER:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $41;
            w[2] := 0;
            w[3] := 0;
            w[4] := 0;
            w[5] := 0;
            w[6] := $00; // add
            move(CmdParams[0], w[7], CmdParamsSize);
            wCount := 7 + CmdParamsSize;
          end
          else
          begin
            s := '';
            for i := 1 to 6 do
              if CmdParams[i - 1] <> 0 then
                s := s + Chr(CmdParams[i - 1])
              else
                s := s + ' ';
            Trim(s);
            if rCount = 6 then
              Log(Format(
                'СУ01 #%d Пользователь #%s (%d) добавлен ',
                [Number, s, $100000 * r[4] + $10000 * r[3] + $100 * r[2] + r[1]]))
            else
              Log(Format(
                'СУ01 #%d Ошибка добавления пользователя #%s (%d)',
                [Number, s, $100000 * w[5] + $10000 * w[4] + $100 * w[3] + w[2]]));
            SetOp;
          end;

        DOP_USERSEARCH:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $42;
            w[2] := $00;
            w[3] := $00;
            w[4] := $00;
            w[5] := $00;
            w[6] := $06;
            move(CmdParams[0], w[7], 7);
            wCount := 7 + 7;
          end
          else
          begin
            if (r[1] <> $ff) and (r[2] <> $ff) and (r[3] <> $ff) and (r[4] <> $ff) then
            begin
              s := '';
              for i := 1 to 6 do
                if CmdParams[i - 1] <> 0 then
                  s := s + Chr(CmdParams[i - 1]);
              Log(Format('СУ01 #%d Пользователь %s (%d) найден ',
                [Number, s, $100000 * r[4] + $10000 * r[3] + $100 * r[2] + r[1]]));
              for i := 1 to 4 do
                CmdParams[i + 7] := r[i];
              SetOp(1);
            end
            else
            begin
              s := '';
              for i := 1 to 6 do
                if CmdParams[i - 1] <> 0 then
                  s := s + Chr(CmdParams[i - 1]);
              Log(Format('СУ01 #%d Пользователь %s не найден',
                [Number, s]));
              SetOp;
            end;
          end;

        DOP_USERRIGHTS:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $41;
            move(CmdParams[8], w[2], 4); //HwId
            w[6] := $04; // Rights
            move(CmdParams[OFFSET_SEARCHCMD], w[7], CmdParamsSize - OFFSET_SEARCHCMD);
            wCount := 7 + CmdParamsSize - OFFSET_SEARCHCMD;
          end
          else
          begin
            s := '';
            for i := 1 to 6 do
              s := s + Chr(CmdParams[i - 1]);
            if (w[2] = r[1]) and (w[3] = r[2]) and (w[4] = r[3]) and (w[5] = r[4]) then
              Log(Format(
                'СУ01 #%d Права пользователя %s (%d) установлены',
                [Number, s, $100000 * r[4] + $10000 * r[3] + $100 * r[2] + r[1]]))
            else
              Log(Format(
                'СУ01 #%d Ошибка установки прав пользователя %s (%d)',
                [Number, s, $100000 * w[5] + $10000 * w[4] + $100 * w[3] + w[2]]));
            SetOp;
          end;

        DOP_DELUSER:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $41;
            move(CmdParams[8], w[2], 4); //HwId
            w[6] := $05; //del
            wCount := 7;
          end
          else
          begin
            s := '';
            for i := 1 to 6 do
              s := s + Chr(CmdParams[i - 1]);
            if (w[2] = r[1]) and (w[3] = r[2]) and (w[4] = r[3]) and (w[5] = r[4]) then
            begin
              Log(Format('СУ01 #%d Пользователь #%s (%d) удален',
                [Number, s, $100000 * w[5] + $10000 * w[4] + $100 * w[3] + w[2]]));
            end
            else
              Log(Format(
                'СУ01 #%d Ошибка удаления пользователя #%s (%d)',
                [
                Number, s, $100000 * w[5] + $10000 * w[4] + $100 * w[3] + w[2]]));
            SetOp;
          end;

        DOP_GETUSER:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $42;
            move(TempIndex, w[2], 4);
            w[6] := 00; // Param kind (1-4,6,7)
            wCount := 7;
          end
          else
          begin
            if rCount > 3 then
              if (r[1] <> 0) or (r[2] <> 0) or (r[3] <> 0) or (r[4] <> 0) then
              begin
                case w[6] of
                  0: // id
                  begin
                    move(r[1], dw, 4);
                    s := Format(
                      'СУ01 #%d Пользователь %d параметры: ',
                      [Number, dw]);
                    for i := 5 to rCount do
                      s := s + IntToHex(r[i], 2);
                    Log(s);
                  end;
                end;
                move(r[1], TempIndex, 4);
                Inc(TempIndex);
              end

              else
                SetOp;
          end;

        DOP_EVENTLOGGET3:
          if IsWrite then
          begin
            w[0] := Number or LinkBit;
            w[1] := $45;
            wCount := 2;
          end
          else
          begin
            CurMes := $100000 * r[1] + $10000 * r[2] + $100 * r[3] + r[4];
            if LastMes <> CurMes then
            begin
              LastMes := CurMes;
              dw := 0;
              for i := 5 to 8 do
                dw := (dw shl 8) + r[i];
              s := Format('СУ01 #%d Событие #%d', [Number, CurMes]);

              s := s + Format(' Время %d/%d/%d %d:%d:%d',
                [2000 + (dw shr 26) and $3f, (dw shr 22) and
                $0f, (dw shr 17) and $1f, (dw shr 12) and $1f,
                (dw shr 6) and $3f, (dw shr 0) and $3f]);

              if (256 * r[9] + r[10]) <= 511 then
                s := s + Format(' %s (#%d)', [Event[256 * r[9] + r[10]],
                  256 * r[9] + r[10]])
              else
                s := s + Format('#%d', [256 * r[9] + r[10]]);

              case 256 * r[9] + r[10] of
                6..16,
                18..27,
                31..36,
                42, 43,
                46..49:
                  s := s + Format(' пользователь %d',
                    [$100000 * r[14] + $10000 * r[13] + $100 * r[12] + r[11]]);

              end;

              case 256 * r[9] + r[10] of
                9..11,
                18..21,
                23, 24,
                31..36,
                46, 47:
                  if r[15] > 0 then
                  begin
                    dw := 0;
                    if (r[15] and $10) = 0 then
                    begin
                      for i := 1 to (r[15] and $0F) do
                        dw := (dw shl 8) + r[15 + i];
                      s := s + Format(' карта %d/%d (%x)',
                        [dw shr 16, dw and $FFFF, dw]);
                    end
                    else

                    begin
                      for i := 1 to (r[15] and $0F) do
                        if (i mod 2) > 0 then
                          dw := (dw * 10) + (r[16 + ((i - 1) div 2)] shr 4)
                        else
                          dw := (dw * 10) + (r[16 + ((i - 1) div 2)] and $0F);
                      s := s + Format(' персональный код %d', [dw]);
                    end;

                  end;

              end;

              case 256 * r[9] + r[10] of
                6..8,
                12..16,
                22, 25, 26, 28, 32,
                42..45, 49:
                  s := s + Format(' секция #%d ячейка #%d',
                    [r[23], r[24]]);
              end;

              Log(s);

              case 256 * r[9] + r[10] of

                6: //Вставили пенал
                begin
                  s := Format('arm,%d,%d,', [r[23], r[24]]);
                  Log('Поиск правил: ' + s);
                  for i := 1 to lRule.Count do
                  begin
                    pRule := lRule.Items[i - 1];
                    if Pos(s, pRule^.TextRule) > 0 then
                    begin
                      Log('Правило найдено...');
                      Init(mes);
                      mes.SysDevice := pRule^.Arg[3];
                      mes.TypeDevice := pRule^.Arg[4];
                      mes.NetDevice := pRule^.Arg[5];
                      mes.BigDevice := pRule^.Arg[6];
                      mes.SmallDevice := pRule^.Arg[7];
                      mes.Code := pRule^.Arg[8];
                      Send(mes);
                    end;
                  end;
                end;

                8: //Вынули пенал аналогично
                begin
                  s := Format('arm,%d,%d,', [r[23], r[24]]);
                  Log('Поиск правил: ' + s);
                  for i := 1 to lRule.Count do
                  begin
                    pRule := lRule.Items[i - 1];
                    if Pos(s, pRule^.TextRule) > 0 then
                    begin
                      Log('Правило найдено...');
                      Init(mes);
                      mes.SysDevice := pRule^.Arg[3];
                      mes.TypeDevice := pRule^.Arg[4];
                      mes.NetDevice := pRule^.Arg[5];
                      mes.BigDevice := pRule^.Arg[6];
                      mes.SmallDevice := pRule^.Arg[7];
                      mes.Code := pRule^.Arg[9];
                      Send(mes);
                    end;
                  end;
                end;

              end;

            end;
            SetOp;
          end;

      end; //case Op

    Result := True;

  except
    if IsWrite then
      Log(Format(
        'Порт %s, ошибка формирования посылки к СУ01...',
        [
        TComPort(Self.Serial).Serial]))
    else
      Log(Format('Порт %s, ошибка разбора приема...',
        [TComPort(Self.Serial).Serial]))
  end;

end;

procedure TLine.Read;
begin
  with CurDev, aMain do
  begin
    case Op of
      DOP_NOP: ;//GetOp: ; // заменить на read_event
    end;// Op

  end;// with

end;

function TLine.NextDev: TDev;
var
  Index: word;

begin
  Result := nil;

  case ChildsObj.Count of

    0: ; // Ситуация: Нет

    1: // Ситуация: Один
      CurDev := ChildsObj.Items[0];

    else
      if CurDev = nil then // Ситуация: Не было
        CurDev := ChildsObj.First
      else if CurDev = ChildsObj.Last then // Ситуация: Последний
        CurDev := ChildsObj.First
      else
      begin
        Index := ChildsObj.IndexOf(CurDev);
        CurDev := ChildsObj.Items[Index + 1];
      end;

  end;// case

  Result := CurDev;
end;

(* --------------- *)
(*      KSBMES     *)
(* --------------- *)
procedure Send(mes: KSBMES; str: PChar);
var
  s: string;

begin
  mes.Proga := KsbAppType;
  mes.NumDevice := mes.SmallDevice;
  WriteNet(mes, str);

  s := Format('SEND: %s Code=%d Sys=%d Type=%d Net=%d Big=%d Small=%d ' +
    'Mode=%d Part=%d Lev=%d Us=%d Num=%d Card=%d Mon=%d Cam=%d Prog=%d NumDev=%d',
    [DateTimeToStr(mes.SendTime), mes.Code, mes.SysDevice, mes.TypeDevice,
    mes.NetDevice, mes.BigDevice, mes.SmallDevice, mes.Mode, mes.Partion,
    mes.Level, mes.User, mes.Num, mes.NumCard, mes.Monitor, mes.Camera,
    mes.Proga, mes.NumDevice]);
  if mes.Size > 0 then
    s := s + Format(' str(%d)=%s', [mes.Size, Bin2Simbol(str, mes.Size)]);
  Log(s);
end;

procedure Send(mes: KSBMES);
begin
  Send(mes, '');
end;

procedure Consider(mes: KSBMES; str: string);
var
  s, s1: string;
  Dev: TDev;
  i: byte;
  l: TBytes;
  catch: boolean;

begin
  catch := False;

  if (mes.Proga <> KsbAppType) and (mes.NetDevice = ModuleNetDevice) and
    (mes.BigDevice = ModuleBigDevice) then
    case mes.Code of
      CHECK_LIVE_PROGRAM,
      KILL_PROGRAM,
      EXIT_PROGRAM,
      MSG_BASE_ROSTEK..(MSG_BASE_ROSTEK + 999):
        catch := True;
    end;

  case mes.Code of
    R8_COMMAND_ZONE_ARM,
    R8_COMMAND_ZONE_DISARM,
    R8_ZONE_ARMED,
    R8_ZONE_DISARMED:
      catch := True;
  end;

  if not catch then
    exit;

  SetLength(l, mes.Size);
  if mes.Size > 0 then
    Simbol2Bin(str, @l[0], mes.Size);

  case mes.Code of
    CHECK_LIVE_PROGRAM,
    KILL_PROGRAM,
    EXIT_PROGRAM: ;

    MSG_CMD_GET_STATES:
    begin
      Log('Запрос состояний');
      for i := 1 to Devs.Count do
      begin
        Dev := Devs.Items[i - 1];
        Init(mes);
        mes.SysDevice := SYSTEM_EVS;
        mes.TypeDevice := TYPEDEVICE_CU01;
        mes.NetDevice := ModuleNetDevice;
        mes.BigDevice := Dev.Bigdevice;
        if Dev.Connected then
          mes.Level := MSG_CU01_AVAILABLE
        else
          mes.Level := MSG_CU01_NOT_AVAILABLE;
        mes.Code := MSG_CU01_STATE;
        Send(mes);
        Log(
          Format('СУ01 #%d Состояние >> %s',
          [Dev.Number, CU01State[Ord(Dev.Connected)]])
          );
      end;
    end;

    MSG_CMD_ADD_USER:
    begin
      Dev := FindDevWithId(mes.BigDevice);
      if Dev <> nil then
      begin
        SetLength(l, 10);
        s := IntToHex((mes.Facility shl 16) + mes.NumCard, 6);
        s := LowerCase(s);
        for i := 1 to 6 do
          l[i - 1] := Ord(s[i]);
        l[6] := 0;
        l[7] := 0;
        l[8] := 0;
        l[9] := mes.Level;
        mes.Size := Length(l);
        Tline(Dev.ParentObj).AddCmd(DOP_ADDUSER, mes.Size, l);
        s := Format('СУ01 #%d Добавить пользователя #%s',
          [Dev.Number, s]);
      end;
    end;

    MSG_CMD_SET_USERRIGHTS_WITH_SEARCH:
    begin
      Dev := FindDevWithId(mes.BigDevice);
      if Dev <> nil then
      begin
        SetLength(l, mes.Size);
        s := IntToHex((mes.Facility shl 16) + mes.NumCard, 6);
        s := LowerCase(s);
        for i := 1 to 6 do //card
          l[i - 1] := Ord(s[i]);
        l[6] := 0; //end
        l[7] := 2; //nextOp 1-add, 2-rigths, 3-del
        Tline(Dev.ParentObj).AddCmd(DOP_USERSEARCH, mes.Size, l);
        s := Format(
          'СУ01 #%d Установить права пользователя #%s',
          [Dev.Number, s]);
      end;
    end;

    MSG_CMD_DELETE_USER_WITH_SEARCH:
    begin
      Dev := FindDevWithId(mes.BigDevice);
      if Dev <> nil then
      begin
        SetLength(l, 8 + 4);
        s := IntToHex((mes.Facility shl 16) + mes.NumCard, 6);
        s := LowerCase(s);
        for i := 1 to 6 do //card
          l[i - 1] := Ord(s[i]);
        l[6] := 0; //end
        l[7] := 3; //nextOp 1-add, 2-rigths, 3-del
        Tline(Dev.ParentObj).AddCmd(DOP_USERSEARCH, mes.Size, l);
        s := Format('СУ01 #%d Удалить  пользователя #%s',
          [Dev.Number, s]);
      end;
    end;

    MSG_CMD_DELETE_ALL_USERS:
    begin
      Dev := FindDevWithId(mes.BigDevice);
      if Dev <> nil then
      begin
        Tline(Dev.ParentObj).AddCmd(DOP_CLEARALLDB, mes.Size, l);
        s := Format('СУ01 #%d Удалить всех пользователей',
          [Dev.Number]);
      end;
    end;
  end;

  s1 := Format('Code=%d Sys=%d Type=%d Net=%d Big=%d Small=%d ' +
    'Mode=%d Part=%d Lev=%d Us=%d Card=%d Mon=%d Cam=%d Prog=%d NumDev=%d',
    [mes.Code, mes.SysDevice, mes.TypeDevice, mes.NetDevice, mes.BigDevice,
    mes.SmallDevice, mes.Mode, mes.Partion, mes.Level, mes.User,
    mes.NumCard, mes.Monitor, mes.Camera, mes.Proga, mes.NumDevice]);
  if mes.Size > 0 then
    s1 := s1 + Format(' str(%d)=%s', [mes.Size, str]);
  Log('READ: ' + s1);

  case mes.Code of
    MSG_BASE_ROSTEK..MSG_BASE_ROSTEK + 999:
      Log('READ: Получена команда. ' + s);
  end;

end;

procedure TaMain.N1Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TaMain.N3Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TaMain.IndicatorMouseMove(Sender: TObject);
var
  i, total: word;
begin
  total := 0;
  for i := 1 to Devs.Count do
    if TDev(Devs.Items[i - 1]).GetConnect then
      Inc(total);

  TControl(Sender).Hint :=
    Format('На связи #%d из #%d приборов СУ01',
    [total, Devs.Count]);
end;

procedure TaMain.SetIndicator;
var
  i, total: word;
begin
  total := 0;
  for i := 1 to Devs.Count do
    if TDev(Devs.Items[i - 1]).Connected then
      Inc(total);

  with Indicator.Brush do
    if total = 0 then
      Color := clRed
    else if total < Devs.Count then
      Color := clYellow
    else
      Color := clLime;
end;

procedure TaMain.FormTimerTimer(Sender: TObject);
begin
  SetIndicator;
end;

procedure TaMain.MenuItem1Click(Sender: TObject);
begin
  ShowMessage('Драйвер СУ01,'#13#10'версия ' +
    GetVersion(Application.ExeName));
end;

{MSG_CMD_ADD_USER}
procedure TaMain.MenuItem4Click(Sender: TObject);
var
  mes: KSBMES;
  l: array [0..127] of byte;
  v: dword;

begin
  init(mes);
  mes.TypeDevice := StrToInt(ValueListEditor1.Cells[1, 1]);
  mes.Netdevice := StrToInt(ValueListEditor1.Cells[1, 2]);
  mes.Bigdevice := StrToInt(ValueListEditor1.Cells[1, 3]);
  mes.Smalldevice := StrToInt(ValueListEditor1.Cells[1, 4]);
  ValueListEditor1.Cells[2, 5] := IntToStr(MSG_CMD_ADD_USER);
  mes.Code := StrToInt(ValueListEditor1.Cells[1, 5]);
  // Card
  try
    v := StrToInt('$' + ValueListEditor1.Cells[1, 9]);
    mes.Facility := v shr 16;
    mes.NumCard := v and $ffff;
    mes.Level := StrToInt('$' + ValueListEditor1.Cells[1, 10]);
    mes.Proga:= 0;
    Consider(mes, Bin2Simbol(PChar(@l[0]), mes.Size));
  finally
  end;
end;

{MSG_CMD_SET_USERRIGHTS_WITH_SEARCH}
procedure TaMain.MenuItem5Click(Sender: TObject);
var
  mes: KSBMES;
  l: array [0..127] of byte;
  v: dword;
  i, j, k, m, loff, len: word;
  s: ansistring;

begin
  init(mes);
  mes.TypeDevice := StrToInt(ValueListEditor1.Cells[1, 1]);
  mes.Netdevice := StrToInt(ValueListEditor1.Cells[1, 2]);
  mes.Bigdevice := StrToInt(ValueListEditor1.Cells[1, 3]);
  mes.Smalldevice := StrToInt(ValueListEditor1.Cells[1, 4]);
  ValueListEditor1.Cells[2, 5] := IntToStr(MSG_CMD_SET_USERRIGHTS_WITH_SEARCH);
  mes.Code := StrToInt(ValueListEditor1.Cells[1, 5]);
  try
    //Card
    v := StrToInt('$' + ValueListEditor1.Cells[1, 9]);
    mes.Facility := v shr 16;
    mes.NumCard := v and $ffff;
    mes.Level := StrToInt('$' + ValueListEditor1.Cells[1, 10]);
    //Rigths
    FillChar(l, sizeof(l), 0);
    loff := OFFSET_SEARCHCMD;
    s := ValueListEditor1.Cells[1, 11];
    repeat
      i := Pos(' ', s);
      Delete(s, i, 1);
    until i = 0;

    len := length(s) div (2 * 5);
    for i := 1 to len do
    begin
      for j := 1 to 5 do
        l[loff + 5 * (i - 1) + (j - 1)] :=
          StrToInt('$' + Copy(s, 2 * 5 * (i - 1) + 1 + 2 * (j - 1), 2));
      m := 0;
      for j := 1 to 4 do
        for k := 0 to 7 do
          if (l[5 * (i - 1) + j + loff] and (1 shl k)) > 0 then
            Inc(m);
      for j := 1 to m do
        l[loff + 5 * i + (j - 1)] := 1;
      loff := loff + m;
    end;
    l[loff + 5 * len] := 255;
    mes.Size := loff + 5 * len + 1;

    mes.Proga:= 0;
    Consider(mes, Bin2Simbol(PChar(@l[0]), mes.Size));
  finally
  end;

end;

procedure TaMain.MenuItem6Click(Sender: TObject);
var
  mes: KSBMES;
begin
  init(mes);
  mes.SysDevice := 1;
  mes.TypeDevice := 2;
  mes.NetDevice := 3;
  mes.BigDevice := 4;
  mes.SmallDevice := 5;
  mes.Code := 14777;
  Send(mes);
end;

{ MSG_CMD_DELETE_USER_WITH_SEARCH }
procedure TaMain.MenuItem12Click(Sender: TObject);
var
  mes: KSBMES;
  l: array [0..127] of byte;
  v: dword;

begin
  init(mes);
  mes.TypeDevice := StrToInt(ValueListEditor1.Cells[1, 1]);
  mes.Netdevice := StrToInt(ValueListEditor1.Cells[1, 2]);
  mes.Bigdevice := StrToInt(ValueListEditor1.Cells[1, 3]);
  mes.Smalldevice := StrToInt(ValueListEditor1.Cells[1, 4]);
  ValueListEditor1.Cells[2, 5] := IntToStr(MSG_CMD_DELETE_USER_WITH_SEARCH);
  mes.Code := StrToInt(ValueListEditor1.Cells[1, 5]);
  //Card
  v := StrToInt('$' + ValueListEditor1.Cells[1, 9]);
  mes.Facility := v shr 16;
  mes.NumCard := v and $ffff;
  mes.Level := StrToInt('$' + ValueListEditor1.Cells[1, 10]);

  mes.Proga:= 0;
  Consider(mes, Bin2Simbol(PChar(@l[0]), mes.Size));
end;

{ MSG_CMD_DELETE_ALL_USERS }
procedure TaMain.MenuItem10Click(Sender: TObject);
var
  mes: KSBMES;
begin
  init(mes);
  mes.TypeDevice := StrToInt(ValueListEditor1.Cells[1, 1]);
  mes.Netdevice := StrToInt(ValueListEditor1.Cells[1, 2]);
  mes.Bigdevice := StrToInt(ValueListEditor1.Cells[1, 3]);
  mes.Smalldevice := StrToInt(ValueListEditor1.Cells[1, 4]);
  ValueListEditor1.Cells[2, 5] := IntToStr(MSG_CMD_DELETE_ALL_USERS);
  mes.Code := StrToInt(ValueListEditor1.Cells[1, 5]);

  mes.Proga:= 0;
  Consider(mes, '');
end;

{ Загрузка пользователей из файла }
procedure TaMain.MenuItem3Click(Sender: TObject);
begin
  //ReadUsers(Xml.FindNode('EvsConfig'));
end;


procedure TaMain.TimerVisibleTimer(Sender: TObject);
begin
  AppKsbTimer();
end;


function ExistDebugKey(sub: string): boolean;
var
  p1: word;

begin
  Result := False;

  with Option do
  begin
    p1 := pos(sub, Debug);
    if p1 = 0 then
      exit;
    if p1 > 1 then
      if not (Debug[p1 - 1] in [' ', ';', ',', '.']) then
        exit;
    if (p1 + Length(sub) - 1) < Length(Debug) then
      if not (Debug[p1 + Length(sub)] in [' ', ';', ',', '.']) then
        exit;
  end;

  Result := True;
end;

function ArrayToStr(Ar: array of byte; Count: byte): string;
var
  i: byte;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + IntToHex(Ar[i - 1], 2);
end;

procedure WriteLog(str: string);
var
  tf: TextFile;
  FileName, OldFileName, CurDir, OldDir, s: string;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: word;
  fSize: integer;

begin
  DecodeDateTime(now, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  CurDir := ExtractFilePath(ParamStr(0));
  with Option do
  begin
    FileName := FileMask + '.log';
    fSize := FileSize(CurDir + FileName);
    if fSize > MAX_LOG_SIZE then
    begin
      s := '_' + Format('%u%.2u%.2u_%.2u%.2u_%.2u%.3u',
        [AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond]);
      OldFileName := Option.FileMask + s + '.log';

      OldDir := CurDir + 'DrvCu1Files\';
      if not DirectoryExists(OldDir) then
        CreateDir(OldDir);

      if not RenameFile(CurDir + FileName, OldDir + OldFileName) then
        str := str + #13#10' Ошибка переименования файла (' +
          CurDir + FileName + '): ' + IntToStr(GetLastOSError);
    end;
  end;

  AssignFile(tf, CurDir + FileName);
  try
    if FileExists(CurDir + FileName) then
      Append(tf)
    else
      rewrite(tf);
    Writeln(tf, DateTimeToStr(Now) + ' ' + str);
    Flush(tf);
  finally
    CloseFile(tf);
  end;

end;

procedure Log(str: string);
begin
  if Option.LogFile then
    WriteLog(str);

  if Option.LogForm then
    with aMain.Memo1.Lines do
    begin
      Add(DateTimeToStr(Now) + ' ' + str);

      while Count > 1000 do
      begin
        BeginUpdate;
        Delete(0);
        EndUpdate;
      end;
    end;

end;

function GetVersion(FileName: string): string;
var
  Version: TFileVersionInfo;
begin
  Version := TFileVersionInfo.Create(nil);
  Version.fileName := FileName;
  Version.ReadFileInfo;
  Result := Version.VersionStrings.Values['FileVersion'];
  {
  For i:= 1 to Version.VersionStrings.Count do
      amain.memo1.Lines.add('Version -> ' + Version.VersionStrings[i-1]);
  }
  Version.Free;
end;

function FindDev(Number: word): TDev;
var
  i: word;
  Dev: TDev;
begin
  Result := nil;
  for i := 1 to Devs.Count do
  begin
    Dev := Devs.Items[i - 1];
    if Dev.Number = Number then
    begin
      Result := Dev;
      break;
    end;
  end;
end;

function FindDevWithId(Id: word): TDev;
var
  i: word;
  Dev: TDev;
begin
  Result := nil;
  for i := 1 to Devs.Count do
  begin
    Dev := Devs.Items[i - 1];
    if Dev.Bigdevice = Id then
    begin
      Result := Dev;
      break;
    end;
  end;
end;

procedure ArrayShift(var a: TBuf; StartPos, EndPos: byte; Kind: TArrayShift);
var
  i: word;

begin
  if Kind = SHLEFT then
    for i := StartPos to EndPos do
      if i = EndPos then
        a[i] := 0
      else
        a[i] := a[i + 1]
  else
    for i := EndPos downto StartPos do
      if i = StartPos then
      begin
        a[i + 1] := a[i];
        a[i] := 0;
      end
      else
        a[i + 1] := a[i];
end;

procedure ArrayInsert(var a: TBuf; var ArraySize: byte; StartPos, Value: byte);
begin
  ArrayShift(a, StartPos, ArraySize, SHRIGHT);
  a[StartPos] := Value;
  Inc(ArraySize);
end;

procedure ArrayDelete(var a: TBuf; var ArraySize: byte; StartPos: byte);
begin
  ArrayShift(a, StartPos, ArraySize, SHLEFT);
  if ArraySize > 0 then
    Dec(ArraySize);
end;

procedure _AppKsbConsider(strmes: string);
var
  mes: KSBMES;
  tail: string;
begin
  Unpack(strmes, mes, tail);
  Consider(mes, tail);
end;

function TDev.FindRule(s: string): pointer;
var
  pRule: ^TRule;
  i: word;
begin
  Result := nil;
  if lRule.Count > 0 then
    for i := 0 to lRule.Count - 1 do
    begin
      pRule := lRule.Items[i];
      if pRule^.TextRule = s then
      begin
        Result := pRule;
        break;
      end;
    end;
end;

function TDev.FindArmRule(s: string): pointer;
var
  pRule: ^TRule;
  i: word;
begin
  Result := nil;
  for i := 1 to lRule.Count do
  begin
    pRule := lRule.Items[i - 1];
    if Pos(s, pRule^.TextRule) > 0 then
    begin
      Result := pRule;
      break;
    end;
  end;
end;

function TDev.StrToRule(s: string; var rule: TRule): boolean;
var
  i, ps: word;
  s1: string;
  code: integer;

begin
  Result := False;

  i := 1;
  s1 := '';
  while (i <= length(s)) do
  begin
    if s[i] in ['a'..'z', 'A'..'Z', '0'..'9', ','] then
      s1 := s1 + s[i];
    Inc(i);
  end;

  FillChar(rule, sizeof(rule), 0);
  ps := Pos(',', s1);
  if (ps > 255) then
    exit;

  if (ps = 0) then
    rule.Func := s1
  else
  begin
    rule.Func := Copy(s1, 1, ps - 1);
    if rule.Func = '' then
      exit;
    Delete(s1, 1, ps);

    i := 1;
    repeat
      ps := Pos(',', s1);
      code := 0;

      if (ps > 1) then
        Val(Copy(s1, 1, ps - 1), rule.Arg[i], code)
      else
        Val(s1, rule.Arg[i], code);
      if (code <> 0) then
        exit;

      Delete(s1, 1, ps);
      rule.Arg[0] := i;
      Inc(i);
    until (ps = 0) or (i > 255) or (s1 = '');

  end;
  rule.TextRule := s;
  Result := True;
end;

procedure TDev.AddRule(rule: TRule);
var
  pRule: ^TRule;
begin
  new(pRule);
  move(rule, pRule^, sizeof(rule));
  lRule.Add(pRule);
end;

procedure TDev.DelRule(p: pointer);
var
  pRule: ^TRule;
begin
  pRule := p;
  if pRule <> nil then
  begin
    lRule.Remove(pRule);
    Dispose(pRule);
  end;
end;


initialization
  Pointer_AppKsbConsider := @_AppKsbConsider;
  Lines := TList.Create;
  Devs := TList.Create;
  Event[0] := 'Больше нет событий';
  Event[1] := 'Пропала сеть';
  Event[2] := 'Появилась сеть';
  Event[3] := 'Аккумулятор разрядился';
  Event[4] := 'Датчик стены (в модели СК-24 – отсутствует)';
  Event[5] := 'Админ. выкл. сирену';
  Event[6] := 'Вставили пенал';
  Event[7] := 'Вынули пенал без разреш.';
  Event[8] := 'Вынули пенал';
  Event[9] := 'Начало идентификации карты (лич.ном.)';
  Event[10] := 'Время действия карты (лич.ном.) истекло';
  Event[11] := 'Настало время предупреждения';
  Event[12] := 'Вставили чужой пенал';
  Event[13] := 'Вставили свой без разрешения';
  Event[14] := 'Вынули чужой пенал';
  Event[15] := 'Вынули свой, ошибочно сданный';
  Event[16] := 'Вставили в блокированную ячейку';
  Event[17] := 'Включили питание';
  Event[18] := 'Набран номер на клавиатуре';
  Event[19] := 'Введен неверный pin-code';
  Event[20] :=
    'Предоставлено разрешение по карте (лич.ном.)';
  Event[21] := 'Карта (лич.ном.) блокирована';
  Event[22] := 'Нарушено расписание для ячейки';
  Event[23] :=
    'Изменен уровень доступа для карты с клавиатуры';
  Event[24] :=
    'Изменен список ячеек для указанной карты с клавиатуры';
  Event[25] :=
    'Изменена маска расписаний для ячейки указанной карты с клавиатуры';
  Event[26] :=
    'Изменено время предупреждения для ячейки с клавиатуры';
  Event[27] := 'Изменено расписание с клавиатуры';
  Event[28] := 'Таймаут ячейки';
  Event[29] := 'Прервалась связь с секцией хранения';
  Event[30] := 'Установлена связь с секцией хранения';
  Event[31] := 'Подтверждение доступа картой';
  Event[32] := 'Нет доступа к ячейке';
  Event[33] := 'Изменения параметров пользователя';
  Event[34] := 'Изменения личного номера пользователя';
  Event[35] := 'Изменения карты пользователя';
  Event[36] := 'Изменения ПИН-кода пользователя';
  Event[37] := 'Установлено соединение по RS-485';
  Event[38] := 'Разорвано соединение по RS-485';
  Event[39] := 'Установлено соединение по Ethernet';
  Event[40] := 'Разорвано соединение по Ethernet';
  Event[41] := 'В цепи аккумулятора неисправность';
  Event[42] := 'Открыта дверца ячейки';
  Event[43] := 'Закрыта дверца ячейки';
  Event[44] := 'Открыта дверца ячейки без разрешения';
  Event[45] := 'Закрыта дверца ячейки без разрешения';
  Event[46] := 'Предъявлена неизвестная карта';
  Event[47] := 'Введен неизвестный личный номер';
  Event[48] := 'Неизвестный отпечаток пальца';
  Event[49] := 'Выбрана ячейка';

end.
